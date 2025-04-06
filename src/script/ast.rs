use {
	crate::lexer::{Lexer, LexerError, Location, Token, TokenKind},
	root::{collections::HashMap, iter::Peekable, rc::Rc},
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
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
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
		let value = self.expression(0)?;

		Ok(Decleration {
			name: name.location,
			value,
			mutable,
		})
	}

	fn expression(&mut self, min_bp: u8) -> Result<Expression, ParseError> {
		let allowed = &[
			TokenKind::LParen,
			TokenKind::Identifier,
			TokenKind::Int,
			TokenKind::Float,
			TokenKind::Char,
			TokenKind::String,
		];

		if self.peek()?.is_none() {
			return Err(ParseError::unexpected_token(allowed, None));
		}

		let lhs = self.next()?.unwrap();
		match lhs.kind {
			TokenKind::Identifier
			| TokenKind::LParen
			| TokenKind::Int
			| TokenKind::Float
			| TokenKind::Char
			| TokenKind::String => {
				let mut lhs = match lhs.kind {
					TokenKind::Identifier => {
						if self.accept(TokenKind::LParen)?.is_some() {
							let mut args = Vec::new();
							loop {
								if let Some(peeked) = self.peek()? {
									if peeked.kind == TokenKind::RParen {
										self.next()?;
										break;
									}
									args.push(self.expression(0)?);
									self.accept(TokenKind::Comma)?;
								}
							}
							Expression::Function {
								name: lhs.location,
								args,
							}
						} else {
							Expression::Identifier { name: lhs.location }
						}
					}
					TokenKind::Int => Expression::Constant {
						kind: Constant::Int,
						value: lhs.location,
					},
					TokenKind::Float => Expression::Constant {
						kind: Constant::Float,
						value: lhs.location,
					},
					TokenKind::String => Expression::Constant {
						kind: Constant::String,
						value: lhs.location,
					},
					_ => unreachable!(),
				};

				while let Some(peeked) = self.peek()? {
					let operator = match peeked.kind {
						TokenKind::Plus => Operator::Add,
						TokenKind::Minus => Operator::Subtract,
						TokenKind::Star => Operator::Multiply,
						TokenKind::Slash => Operator::Divide,
						_ => {
							break;
						}
					};

					let (l_bp, r_bp) = match operator {
						Operator::Add | Operator::Subtract => (1, 2),
						Operator::Multiply | Operator::Divide => (3, 4),
					};
					if l_bp < min_bp {
						break;
					}

					self.next()?;
					let rhs = self.expression(r_bp)?;

					lhs = Expression::Operation {
						operator,
						input: Box::new((lhs, rhs)),
					};
				}

				Ok(lhs)
			}
			_ => Err(ParseError::unexpected_token(allowed, Some(lhs))),
		}
	}
}

#[derive(Debug)]
pub struct Ast<'a> {
	pub src: &'a str,
	pub body: Body,
}

impl<'a> Ast<'a> {
	pub fn new(src: &'a str) -> Result<Ast, ParseError> {
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
}

#[derive(Debug, Clone)]
pub struct Body {
	pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
	Decleration(Decleration),
}

#[derive(Debug, Clone)]
pub struct Decleration {
	pub name: Location,
	pub value: Expression,
	pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Assignment {
	pub identifier: Location,
	pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Branch {
	pub condition: Expression,
	pub body: Body,
}

#[derive(Debug, Clone)]
pub enum Operator {
	Add,
	Subtract,
	Multiply,
	Divide,
}

#[derive(Debug)]
pub struct OperationRHS {
	pub operator: Operator,
	pub operand: Expression,
}

#[derive(Debug, Clone)]
pub enum Constant {
	Int,
	Float,
	String,
}

#[derive(Debug, Clone)]
pub enum Expression {
	Branch {
		if_branch: Box<Branch>,
		else_if_branches: Vec<Branch>,
		else_body: Option<Body>,
	},
	Operation {
		operator: Operator,
		input: Box<(Expression, Expression)>,
	},
	Constant {
		kind: Constant,
		value: Location,
	},
	Identifier {
		name: Location,
	},
	Function {
		name: Location,
		args: Vec<Expression>,
	},
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub members: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
	Null,
	Int(i64),
	Float(f64),
	String(String),
	Struct(Rc<Struct>),
}

#[derive(Default, Debug, Clone)]
pub struct VM {
	variables: HashMap<String, Value>,
}

impl VM {
	pub fn new() -> VM {
		VM {
			variables: HashMap::new(),
		}
	}

	pub fn set_variable(&mut self, name: impl Into<String>, value: Value) {
		self.variables.insert(name.into(), value);
	}

	pub fn get_variable(&self, name: &str) -> Option<Value> {
		self.variables.get(name).cloned()
	}

	pub fn execute(&mut self, ast: &Ast) -> Result<(), String> {
		for statement in &ast.body.statements {
			match statement {
				Statement::Decleration(decl) => {
					let value = self.evaluate_expression(ast, &decl.value)?;
					self.variables
						.insert(decl.name.slice_str(&ast.src).to_string(), value);
				}
			}
		}
		Ok(())
	}

	pub fn evaluate_expression(&self, ast: &Ast, expression: &Expression) -> Result<Value, String> {
		match expression {
			Expression::Constant { kind, value } => {
				let value_str = value.slice_str(ast.src);
				match kind {
					Constant::Int => value_str
						.parse::<i64>()
						.map(Value::Int)
						.map_err(|_| "Invalid integer".to_string()),
					Constant::Float => value_str
						.parse::<f64>()
						.map(Value::Float)
						.map_err(|_| "Invalid float".to_string()),
					Constant::String => Ok(Value::String(value_str.to_string())),
				}
			}
			Expression::Identifier { name } => self
				.variables
				.get(name.slice_str(ast.src))
				.cloned()
				.ok_or_else(|| format!("Undefined variable: {:?}", name)),
			Expression::Operation { operator, input } => {
				let left_value = self.evaluate_expression(ast, &input.0)?;
				let right_value = self.evaluate_expression(ast, &input.1)?;
				match operator {
					Operator::Add => match (left_value, right_value) {
						(Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
						(Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
						(Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
						(Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
						_ => Err("Type mismatch in addition".to_string()),
					},
					Operator::Subtract => match (left_value, right_value) {
						(Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
						(Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
						(Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
						(Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
						_ => Err("Type mismatch in addition".to_string()),
					},
					Operator::Multiply => match (left_value, right_value) {
						(Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
						(Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
						(Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
						(Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
						_ => Err("Type mismatch in addition".to_string()),
					},
					Operator::Divide => match (left_value, right_value) {
						(Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
						(Value::Float(l), Value::Int(r)) => Ok(Value::Float(l / r as f64)),
						(Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 / r)),
						(Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
						_ => Err("Type mismatch in addition".to_string()),
					},
				}
			}
			_ => Err("Unsupported expression type".to_string()),
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn vm() {
		let script = "let mut foo = 45.0 + 17 - 12 * 1000;";
		let ast = Ast::new(script).unwrap();

		let mut vm = VM::new();
		vm.execute(&ast).unwrap();

		println!("foo: {:?}", vm.get_variable("foo"));
	}
}
