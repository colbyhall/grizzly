use {
	crate::{
		ast::*,
		lexer::{Lexer, LexerError, Token, TokenKind},
	},
	root::iter::Peekable,
};

#[derive(Debug)]
pub enum ParseError {
	LexerError(LexerError),
	UnexpectedToken {
		expected: Vec<TokenKind>,
		found: Option<Token>,
	},
}

impl ParseError {
	fn unexpected_token(expected: &[TokenKind], found: Option<Token>) -> ParseError {
		ParseError::UnexpectedToken {
			expected: expected.to_vec(),
			found,
		}
	}
}

pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	pub fn parse(src: &'a str) -> Result<Ast, ParseError> {
		let mut parser = Parser {
			lexer: Lexer::new(src).peekable(),
		};

		let mut statements = Vec::new();
		while parser.peek()?.is_some() {
			statements.push(parser.statement()?);
		}

		Ok(Ast {
			src,
			body: Body { statements },
		})
	}

	/// Parses the next token but does not step the lexer iterator forward
	#[inline]
	fn peek(&mut self) -> Result<Option<Token>, ParseError> {
		match self.lexer.peek() {
			Some(result) => {
				let token = result.map_err(ParseError::LexerError)?;
				Ok(Some(token))
			}
			None => Ok(None),
		}
	}

	/// Parses the next token and steps the lexer iterator forward
	#[inline]
	fn next(&mut self) -> Result<Option<Token>, ParseError> {
		match self.lexer.next() {
			Some(result) => {
				let token = result.map_err(ParseError::LexerError)?;
				Ok(Some(token))
			}
			None => Ok(None),
		}
	}

	/// Peeks the next token and if it is 'kind' consume the token and return it.
	fn accept(&mut self, kind: TokenKind) -> Result<Option<Token>, ParseError> {
		if let Some(token) = self.peek()? {
			if token.kind == kind {
				self.next()?;
				return Ok(Some(token));
			}
		}

		Ok(None)
	}

	/// Accepts the next token and if the result is none throw an error
	fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
		if let Some(token) = self.accept(kind)? {
			Ok(token)
		} else {
			Err(ParseError::unexpected_token(&[kind], self.next()?))
		}
	}

	fn body(&mut self) -> Result<Body, ParseError> {
		self.expect(TokenKind::LCurly)?;

		let mut statements = Vec::with_capacity(1024);
		loop {
			let statement = self.statement()?;
			statements.push(statement);

			if self.accept(TokenKind::RCurly)?.is_some() {
				break;
			}
		}

		Ok(Body { statements })
	}

	fn statement(&mut self) -> Result<Statement, ParseError> {
		let allowed = &[TokenKind::Let];
		let result = if let Some(peeked) = self.peek()? {
			match peeked.kind {
				TokenKind::Let => Statement::Decleration(self.decleration()?),
				_ => return Err(ParseError::unexpected_token(allowed, Some(peeked))),
			}
		} else {
			return Err(ParseError::unexpected_token(allowed, None));
		};
		self.expect(TokenKind::Semicolon)?;
		Ok(result)
	}

	fn decleration(&mut self) -> Result<Decleration, ParseError> {
		self.expect(TokenKind::Let)?;
		let mutable = self.accept(TokenKind::Mut)?.is_some();
		let name = self.expect(TokenKind::Identifier)?;
		self.expect(TokenKind::Equal)?;
		let value = self.expression()?;

		Ok(Decleration {
			name: name.location,
			value,
			mutable,
		})
	}

	fn expression(&mut self) -> Result<Expression, ParseError> {
		let allowed = &[
			TokenKind::LParen,
			TokenKind::Identifier,
			TokenKind::Int,
			TokenKind::Float,
			TokenKind::Char,
			TokenKind::String,
		];
		if let Some(token) = self.next()? {
			match token.kind {
				TokenKind::Identifier
				| TokenKind::LParen
				| TokenKind::Int
				| TokenKind::Float
				| TokenKind::Char
				| TokenKind::String => {
					let lhs = match token.kind {
						TokenKind::LParen => {
							let result = self.expression()?;
							self.expect(TokenKind::RParen)?;
							Expression::Parenthesis {
								expression: Box::new(result),
							}
						}
						TokenKind::Identifier => {
							if self.accept(TokenKind::LParen)?.is_some() {
								let mut args = Vec::new();
								loop {
									if let Some(peeked) = self.peek()? {
										if peeked.kind == TokenKind::RParen {
											self.next()?;
											break;
										}
										args.push(self.expression()?);
										self.accept(TokenKind::Comma)?;
									}
								}
								Expression::Function {
									name: token.location,
									args,
								}
							} else {
								Expression::Identifier {
									name: token.location,
								}
							}
						}
						TokenKind::Int => Expression::Constant {
							kind: Constant::Int,
							value: token.location,
						},
						TokenKind::Float => Expression::Constant {
							kind: Constant::Float,
							value: token.location,
						},
						TokenKind::String => Expression::Constant {
							kind: Constant::String,
							value: token.location,
						},
						_ => unreachable!(),
					};

					if let Some(peeked) = self.peek()? {
						if peeked.kind != TokenKind::Semicolon {
							let operator = match peeked.kind {
								TokenKind::Plus => Operator::Add,
								TokenKind::Minus => Operator::Subtract,
								TokenKind::Star => Operator::Multiply,
								TokenKind::Slash => Operator::Divide,
								_ => {
									return Ok(lhs);
								}
							};
							self.next()?;

							let operand = self.expression()?;
							return Ok(Expression::Operation {
								left: Box::new(lhs),
								operator,
								right: Box::new(operand),
							});
						}
					}

					Ok(lhs)
				}
				_ => Err(ParseError::unexpected_token(allowed, Some(token))),
			}
		} else {
			Err(ParseError::unexpected_token(allowed, None))
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn parse_decleration() {
		let script = "let mut foo = (45 - x(42 + 123, y)) / 120 + y;";
		let ast = Parser::parse(script).unwrap();
		println!("{:?}", ast);
		assert_eq!(ast.body.statements.len(), 1);
	}
}
