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
				TokenKind::Return => {
					self.expect(TokenKind::Return)?;
					Statement::Return(self.expression(0)?)
				}
				_ => Statement::Expression(self.expression(0)?),
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
							Expression::FunctionCall {
								name: lhs.location,
								args,
							}
						} else {
							Expression::Identifier { name: lhs.location }
						}
					}
					TokenKind::Int => Expression::Constant(Constant::Int(
						lhs.location.slice_str(self.src).parse().unwrap(),
					)),
					TokenKind::Float => Expression::Constant(Constant::Float(
						lhs.location.slice_str(self.src).parse().unwrap(),
					)),
					TokenKind::String => Expression::Constant(Constant::String(lhs.location)),
					TokenKind::LParen => {
						let lhs = self.expression(0)?;
						self.expect(TokenKind::RParen)?;
						lhs
					}
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

				Ok(Expression::FunctionDecleration { args, body })
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
			src,
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
	Return(Expression),
	Expression(Expression),
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

#[derive(Debug, Clone)]
pub enum Constant {
	Int(i64),
	Float(f64),
	String(Location),
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
	Constant(Constant),
	Identifier {
		name: Location,
	},
	FunctionCall {
		name: Location,
		args: Vec<Expression>,
	},
	FunctionDecleration {
		args: Vec<Location>,
		body: Body,
	},
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub members: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Function {
	Script { args: Vec<String>, body: Body },
	Native(fn(&[Value]) -> Result<Value, String>),
}

#[derive(Debug, Clone)]
pub enum Value {
	Null,
	Int(i64),
	Float(f64),
	String(String),
	Struct(Rc<Struct>),
	Function(Function),
}

#[derive(Default, Debug, Clone)]
struct Scope {
	variables: HashMap<String, Value>,
}

#[derive(Default, Debug, Clone)]
pub struct VM {
	stack: Vec<Scope>,
}

impl VM {
	pub fn new() -> VM {
		VM {
			stack: vec![Scope {
				variables: HashMap::new(),
			}],
		}
	}

	pub fn get_variable(&self, name: &str) -> Option<Value> {
		for scope in self.stack.iter().rev() {
			if let Some(value) = scope.variables.get(name) {
				return Some(value.clone());
			}
		}
		None
	}

	pub fn set_variable(&mut self, name: impl Into<String>, value: Value) {
		if let Some(scope) = self.stack.last_mut() {
			scope.variables.insert(name.into(), value);
		}
	}

	pub fn execute(&mut self, ast: &Ast) -> Result<Option<Value>, String> {
		self.execute_body(ast, &ast.body)
	}

	fn execute_body(&mut self, ast: &Ast, body: &Body) -> Result<Option<Value>, String> {
		for statement in &body.statements {
			match statement {
				Statement::Decleration(decl) => {
					let value = self.execute_expression(ast, &decl.value)?;
					self.stack
						.last_mut()
						.unwrap()
						.variables
						.insert(decl.name.slice_str(ast.src).to_string(), value);
				}
				Statement::Return(expression) => {
					let value = self.execute_expression(ast, expression)?;
					return Ok(Some(value));
				}
				Statement::Expression(expression) => {
					self.execute_expression(ast, expression)?;
				}
			}
		}
		Ok(None)
	}

	fn execute_expression(&mut self, ast: &Ast, expression: &Expression) -> Result<Value, String> {
		match expression {
			Expression::Constant(value) => match value {
				Constant::Int(x) => Ok(Value::Int(*x)),
				Constant::Float(x) => Ok(Value::Float(*x)),
				Constant::String(location) => {
					Ok(Value::String(location.slice_str(ast.src).to_string()))
				}
			},
			Expression::Identifier { name } => self
				.get_variable(name.slice_str(ast.src))
				.ok_or_else(|| format!("Undefined variable: {:?}", name)),
			Expression::Operation { operator, input } => {
				let left_value = self.execute_expression(ast, &input.0)?;
				let right_value = self.execute_expression(ast, &input.1)?;
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
			Expression::FunctionDecleration { args, body } => {
				let args = args
					.iter()
					.map(|arg| arg.slice_str(ast.src).to_string())
					.collect::<Vec<_>>();
				Ok(Value::Function(Function::Script {
					args,
					body: body.clone(),
				}))
			}
			Expression::FunctionCall { name, args } => {
				let possible_function = self
					.get_variable(name.slice_str(ast.src))
					.ok_or_else(|| format!("Undefined function: {:?}", name))?;
				match possible_function {
					Value::Function(func) => match func {
						Function::Script {
							args: arg_names,
							body,
						} => {
							let mut arg_values = Vec::new();
							for arg in args {
								let value = self.execute_expression(ast, arg)?;
								arg_values.push(value);
							}
							if arg_values.len() != arg_names.len() {
								return Err(format!(
									"Function {:?} expected {} arguments, got {}",
									name,
									arg_names.len(),
									arg_values.len()
								));
							}

							let mut new_scope = Scope {
								variables: HashMap::new(),
							};
							for (arg_name, arg_value) in arg_names.iter().zip(arg_values) {
								new_scope.variables.insert(arg_name.clone(), arg_value);
							}
							self.stack.push(new_scope);
							Ok(self.execute_body(ast, &body)?.unwrap_or(Value::Null))
						}
						Function::Native(f) => (f)(&args
							.iter()
							.map(|arg| self.execute_expression(ast, arg))
							.collect::<Result<Vec<_>, _>>()?),
					},
					_ => Err(format!("Expected function, found {:?}", possible_function)),
				}
			}
			_ => Err("Unsupported expression type".to_string()),
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	fn print(args: &[Value]) -> Result<Value, String> {
		for arg in args {
			match arg {
				Value::String(s) => print!("{}", s),
				Value::Int(i) => print!("{}", i),
				Value::Float(f) => print!("{}", f),
				_ => return Err("Unsupported type".to_string()),
			}
		}
		Ok(Value::Null)
	}
	fn println(args: &[Value]) -> Result<Value, String> {
		for arg in args {
			match arg {
				Value::String(s) => print!("{}", s),
				Value::Int(i) => print!("{}", i),
				Value::Float(f) => print!("{}", f),
				_ => return Err("Unsupported type".to_string()),
			}
		}
		println!();
		Ok(Value::Null)
	}

	fn print_location(src: &str, location: Location) {
		let mut byte_offset = 0;
		let mut line = 0;
		let mut column = 0;
		for c in src.chars() {
			if c == '\n' {
				line += 1;
				column = 0;
			} else {
				column += 1;
			}

			if byte_offset == location.start() {
				break;
			}

			byte_offset += c.len_utf8();
		}

		let lines = src.lines().collect::<Vec<_>>();
		let min_line = (line - 1).max(0);
		let max_line = (line + 1).min(lines.len() - 1);
		for i in min_line..=max_line {
			println!("{:>3}: {}", i + 1, lines[i]);
			if i == line {
				let column = column + 4;
				for j in 0..=column {
					if j == column {
						println!("^");
					} else {
						print!(" ");
					}
				}
			}
		}
	}

	#[test]
	fn vm() {
		let script = r#"
            let foo = (45.0 + 17 - 12) * 1000;
            let bar = fn(x, y) {
                return x + y;
            };
            let baz = bar(foo, 100);
            println("Hello World Foo Buz Ba   ", baz, " ", foo);
        "#;
		let ast = match Ast::new(script) {
			Ok(ast) => ast,
			Err(err) => {
				match err {
					ParseError::LexerError(err) => match err {
						LexerError::UnexpectedChar(index, c) => {
							println!("Error: unexpected char {:?}", c);
							print_location(script, Location::new(index, index + 1));
						}
					},
					ParseError::UnexpectedToken { expected, found } => {
						println!("Error: expected {:?}, found {:?}", expected, found);
						if let Some(found) = found {
							print_location(script, found.location);
						}
					}
				}
				return;
			}
		};

		let mut vm = VM::new();
		vm.set_variable(
			"print".to_string(),
			Value::Function(Function::Native(print)),
		);
		vm.set_variable(
			"println".to_string(),
			Value::Function(Function::Native(println)),
		);
		vm.execute(&ast).unwrap();
		println!("baz: {:?}", vm.get_variable("baz"));
	}
}
