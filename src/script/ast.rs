use {
	crate::lexer::{
		Lexer,
		LexerError,
		Location,
		Token,
		TokenKind,
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

struct Parser<'a> {
	src: &'a str,
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	/// Parses the next token but does not step the lexer forward
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
			let statement = self.stmt()?;
			statements.push(statement);

			if self.accept(TokenKind::RCurly)?.is_some() {
				break;
			}
		}

		Ok(Body { stmt: statements })
	}

	fn stmt(&mut self) -> Result<Stmt, ParseError> {
		let allowed = &[TokenKind::Let];
		let result = if let Some(peeked) = self.peek()? {
			match peeked.kind {
				TokenKind::Let => Stmt::Decleration(self.decl()?),
				TokenKind::Return => {
					self.expect(TokenKind::Return)?;
					Stmt::Return(self.expr(0)?)
				}
				TokenKind::If | TokenKind::While => {
					let result = Stmt::Expression(self.expr(0)?);
					return Ok(result);
				}
				_ => Stmt::Expression(self.expr(0)?),
			}
		} else {
			return Err(ParseError::unexpected_token(allowed, None));
		};
		self.expect(TokenKind::Semicolon)?;
		Ok(result)
	}

	fn decl(&mut self) -> Result<Decl, ParseError> {
		self.expect(TokenKind::Let)?;
		let mutable = self.accept(TokenKind::Mut)?.is_some();
		let name = self.expect(TokenKind::Identifier)?;
		let mut ty = None;
		if self.accept(TokenKind::Colon)?.is_some() {
			ty = Some(self.expect(TokenKind::Identifier)?.location);
		}
		self.expect(TokenKind::Equal)?;
		let value = self.expr(0)?;

		Ok(Decl {
			name: name.location,
			value,
			ty,
			mutable,
		})
	}

	fn expr(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
		let allowed = &[
			TokenKind::LParen,
			TokenKind::Identifier,
			TokenKind::Int,
			TokenKind::Float,
			TokenKind::String,
			TokenKind::True,
			TokenKind::False,
			TokenKind::If,
		];

		if self.peek()?.is_none() {
			return Err(ParseError::unexpected_token(allowed, None));
		}
		let token = self.next()?.unwrap();
		match token.kind {
			TokenKind::Identifier
			| TokenKind::LParen
			| TokenKind::Int
			| TokenKind::Float
			| TokenKind::True
			| TokenKind::False
			| TokenKind::String => {
				let mut lhs = match token.kind {
					TokenKind::Identifier => {
						if self.accept(TokenKind::LParen)?.is_some() {
							let mut args = Vec::new();
							loop {
								if let Some(peeked) = self.peek()? {
									if peeked.kind == TokenKind::RParen {
										self.next()?;
										break;
									}
									args.push(self.expr(0)?);
									self.accept(TokenKind::Comma)?;
								}
							}
							Expr::Call {
								name: token.location,
								args,
							}
						} else {
							Expr::Ident {
								name: token.location,
							}
						}
					}
					TokenKind::Int => Expr::Const(Const::Int(
						token.location.slice_str(self.src).parse().unwrap(),
					)),
					TokenKind::Float => Expr::Const(Const::Float(
						token.location.slice_str(self.src).parse().unwrap(),
					)),
					TokenKind::True => Expr::Const(Const::Bool(true)),
					TokenKind::False => Expr::Const(Const::Bool(false)),
					TokenKind::String => Expr::Const(Const::String(token.location)),
					TokenKind::LParen => {
						let lhs = self.expr(0)?;
						self.expect(TokenKind::RParen)?;
						lhs
					}
					_ => unreachable!(),
				};

				while let Some(peeked) = self.peek()? {
					if peeked.kind == TokenKind::Equal {
						self.next()?;
						let value = self.expr(0)?;
						lhs = Expr::Asmt(Asmt {
							variable: token.location,
							value: Box::new(value),
						});
						break;
					}

					let operator = match peeked.kind {
						TokenKind::Plus => Op::Binary(BinaryOp::Add),
						TokenKind::Minus => Op::Binary(BinaryOp::Sub),
						TokenKind::Star => Op::Binary(BinaryOp::Mul),
						TokenKind::Slash => Op::Binary(BinaryOp::Div),
						TokenKind::EqualTo => Op::Binary(BinaryOp::EqualTo),
						TokenKind::NotEqualTo => Op::Binary(BinaryOp::NotEqualTo),
						TokenKind::LessThan => Op::Binary(BinaryOp::LessThan),
						TokenKind::LessThanOrEqual => Op::Binary(BinaryOp::LessThanOrEqual),
						TokenKind::GreaterThan => Op::Binary(BinaryOp::GreaterThan),
						TokenKind::GreaterThanOrEqual => Op::Binary(BinaryOp::GreaterThanOrEqual),
						TokenKind::And => Op::Binary(BinaryOp::LogicalAnd),
						TokenKind::Or => Op::Binary(BinaryOp::LogicalOr),
						_ => {
							break;
						}
					};

					let (l_bp, r_bp) = match operator {
						Op::Binary(operator) => match operator {
							BinaryOp::Add | BinaryOp::Sub => (3, 4),
							BinaryOp::Mul | BinaryOp::Div => (5, 6),
							_ => (1, 2),
						},
						_ => unimplemented!(),
					};
					if l_bp < min_bp {
						break;
					}

					self.next()?;
					let rhs = self.expr(r_bp)?;

					lhs = Expr::Op {
						op: operator,
						input: vec![lhs, rhs],
					};
				}

				Ok(lhs)
			}
			TokenKind::Fn => {
				self.expect(TokenKind::LParen)?;
				let mut args = Vec::new();
				while let Some(arg) = self.accept(TokenKind::Identifier)? {
					args.push(arg.location);
					if self.accept(TokenKind::Comma)?.is_none() {
						break;
					}
				}
				self.expect(TokenKind::RParen)?;
				let body = self.body()?;

				Ok(Expr::FnDecl { args, body })
			}
			TokenKind::If => {
				let mut branches = Vec::new();

				let cond = self.expr(0)?;
				let body = self.body()?;
				let if_branch = Branch {
					condition: cond,
					body,
				};
				branches.push(if_branch);

				let mut else_body = None;
				while let Some(peeked) = self.peek()? {
					if peeked.kind == TokenKind::Else {
						self.next()?;
						if self.accept(TokenKind::If)?.is_some() {
							let condition = self.expr(0)?;
							let body = self.body()?;
							branches.push(Branch { condition, body });
						} else {
							else_body = Some(self.body()?);
							break;
						}
					} else {
						break;
					}
				}

				Ok(Expr::Branch {
					branches,
					else_body,
				})
			}
			TokenKind::While => {
				let cond = self.expr(0)?;
				let body = self.body()?;
				Ok(Expr::While {
					cond: Box::new(cond),
					body,
				})
			}
			_ => Err(ParseError::unexpected_token(allowed, Some(token))),
		}
	}
}

#[derive(Debug)]
pub struct Ast<'a> {
	pub src: &'a str,
	pub body: Body,
}

impl<'a> Ast<'a> {
	pub fn new(src: &'a str) -> Result<Self, ParseError> {
		let mut parser = Parser {
			src,
			lexer: Lexer::new(src).peekable(),
		};

		let mut statements = Vec::new();
		while parser.peek()?.is_some() {
			statements.push(parser.stmt()?);
		}

		Ok(Ast {
			src,
			body: Body { stmt: statements },
		})
	}
}

#[derive(Debug, Clone)]
pub struct Body {
	pub stmt: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
	Decleration(Decl),
	Return(Expr),
	Expression(Expr),
}

#[derive(Debug, Clone)]
pub struct Decl {
	pub name: Location,
	pub ty: Option<Location>,
	pub value: Expr,
	pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Asmt {
	pub variable: Location,
	pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Branch {
	pub condition: Expr,
	pub body: Body,
}

#[derive(Debug, Clone, Copy)]
pub enum BitOp {
	And,
	Or,
	Xor,
	Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
	Add,
	Sub,
	Mul,
	Div,

	EqualTo,
	NotEqualTo,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,

	LogicalAnd,
	LogicalOr,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
	Not,
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
	Binary(BinaryOp),
	Unary(UnaryOp),
}

#[derive(Debug, Clone)]
pub enum Const {
	Bool(bool),
	Int(i64),
	Float(f64),
	String(Location),
}

#[derive(Debug, Clone)]
pub enum Expr {
	Branch {
		branches: Vec<Branch>,
		else_body: Option<Body>,
	},
	Op {
		op: Op,
		input: Vec<Expr>,
	},
	Const(Const),
	Ident {
		name: Location,
	},
	Call {
		name: Location,
		args: Vec<Expr>,
	},
	FnDecl {
		args: Vec<Location>,
		body: Body,
	},
	While {
		cond: Box<Expr>,
		body: Body,
	},
	Asmt(Asmt),
}
