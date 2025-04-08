use {
	crate::{
		ast::{self, Ast, BinaryOp, ParseError, UnaryOp},
		lexer::{LexerError, Location},
	},
	root::{collections::HashMap, fmt, io::Write},
};

#[derive(Debug, Clone)]
pub enum Type {
	Bool,
	Int,
	Float,
	String,
}

pub enum Constant {
	Bool(bool),
	Int(i64),
	Float(f64),
	String(String),
}

impl fmt::Debug for Constant {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Constant::Bool(v) => write!(f, "{}", v),
			Constant::Int(v) => write!(f, "{}", v),
			Constant::Float(v) => write!(f, "{}", v),
			Constant::String(v) => write!(f, "{:?}", v),
		}
	}
}

impl Constant {
	pub fn type_decleration(&self) -> usize {
		match self {
			Constant::Bool(_) => Generator::BOOL_TYPE_DECL,
			Constant::Int(_) => Generator::INT_TYPE_DECL,
			Constant::Float(_) => Generator::FLOAT_TYPE_DECL,
			Constant::String(_) => Generator::STRING_TYPE_DECL,
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum OpArg {
	Variable(usize),
	Constant(usize),
}

impl OpArg {
	pub fn to_expression(self) -> Expression {
		match self {
			OpArg::Variable(x) => Expression::Variable { target: x },
			OpArg::Constant(x) => Expression::Constant { value: x },
		}
	}
}

#[derive(Debug, Clone)]
pub enum Op {
	Binary {
		op: BinaryOp,
		lhs: OpArg,
		rhs: OpArg,
	},
	Unary(UnaryOp, OpArg),
}

#[derive(Debug, Clone)]
pub struct PhiArg {
	prev_block: usize,
	value: usize,
}

#[derive(Debug, Clone)]
pub enum Expression {
	Op(Op),

	Variable { target: usize },
	Constant { value: usize },
	Phi(Vec<PhiArg>),

	Type(Type),
}

impl Expression {
	pub fn as_op_arg(&self) -> Option<OpArg> {
		match self {
			Expression::Variable { target } => Some(OpArg::Variable(*target)),
			Expression::Constant { value } => Some(OpArg::Constant(*value)),
			_ => None,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Decleration {
		mutable: bool,
		ty: usize,
		value: Expression,
	},
	Jump {
		target: usize,
	},
	JumpIf {
		condition: usize,
		if_true: usize,
		if_false: usize,
	},
	Block,
}

#[derive(Debug)]
pub enum GeneratorError {
	CanNotImplicitCast { from: usize, to: usize },
	UnknownDecleration(String),
	DeclerationImmutable(String),
}

type Result<T> = root::result::Result<T, GeneratorError>;

#[derive(Debug, Default)]
pub struct Generator {
	symbols: HashMap<String, usize>,
	statements: Vec<Statement>,
	constants: Vec<Constant>,
}

impl Generator {
	pub fn new() -> Self {
		let mut result = Self {
			symbols: HashMap::new(),
			statements: Vec::new(),
			constants: Vec::new(),
		};

		result.register_primitive("Bool", Type::Bool);
		result.register_primitive("Int", Type::Int);
		result.register_primitive("Float", Type::Float);
		result.register_primitive("String", Type::String);

		result
	}

	const BOOL_TYPE_DECL: usize = 0;
	const INT_TYPE_DECL: usize = 1;
	const FLOAT_TYPE_DECL: usize = 2;
	const STRING_TYPE_DECL: usize = 3;

	fn register_primitive(&mut self, name: impl Into<String>, ty: Type) {
		let name = name.into();
		let index = self.statements.len();
		self.symbols.insert(name, index);
		self.statements.push(Statement::Decleration {
			mutable: false,
			ty: index,
			value: Expression::Type(ty),
		});
	}

	pub fn generate(&mut self, ast: &Ast) -> Result<()> {
		self.generate_body(ast, &ast.body)?;
		Ok(())
	}

	/// Given an expression, return an index to the result type decleration
	fn infer_type(&mut self, expression: &Expression) -> Result<usize> {
		match expression {
			Expression::Op(op) => match op {
				Op::Binary { op, lhs, rhs } => {
					let convert_to_bool = matches!(
						op,
						BinaryOp::EqualTo
							| BinaryOp::NotEqualTo
							| BinaryOp::LessThan | BinaryOp::LessThanOrEqual
							| BinaryOp::GreaterThan
							| BinaryOp::GreaterThanOrEqual
							| BinaryOp::LogicalOr | BinaryOp::LogicalAnd
					);

					let lhs = match lhs {
						OpArg::Constant(index) => self.constants[*index].type_decleration(),
						OpArg::Variable(index) => match &self.statements[*index] {
							Statement::Decleration {
								mutable: _,
								ty,
								value: _,
							} => *ty,
							_ => unimplemented!(),
						},
					};
					let rhs = match rhs {
						OpArg::Constant(index) => self.constants[*index].type_decleration(),
						OpArg::Variable(index) => match &self.statements[*index] {
							Statement::Decleration {
								mutable: _,
								ty,
								value: _,
							} => *ty,
							_ => unimplemented!(),
						},
					};

					if convert_to_bool {
						Ok(Self::BOOL_TYPE_DECL)
					} else {
						match (lhs, rhs) {
							(Self::FLOAT_TYPE_DECL, Self::INT_TYPE_DECL)
							| (Self::INT_TYPE_DECL, Self::FLOAT_TYPE_DECL) => Ok(Self::FLOAT_TYPE_DECL),
							_ => Ok(self.implicit_cast(rhs, lhs)?),
						}
					}
				}
				Op::Unary(_, value) => match value {
					OpArg::Constant(index) => Ok(self.constants[*index].type_decleration()),
					OpArg::Variable(index) => match &self.statements[*index] {
						Statement::Decleration {
							mutable: _,
							ty,
							value: _,
						} => Ok(*ty),
						_ => unimplemented!(),
					},
				},
			},
			Expression::Constant { value } => Ok(self.constants[*value].type_decleration()),
			Expression::Variable { target } => match &self.statements[*target] {
				Statement::Decleration {
					mutable: _,
					ty,
					value: _,
				} => Ok(*ty),
				_ => unreachable!(),
			},
			_ => unimplemented!(),
		}
	}

	/// Determine a type can implicit cast to another, given the types decleration index. Return the result type decleration index
	fn implicit_cast(&self, from: usize, to: usize) -> Result<usize> {
		// If we're the same type decl index than we must be able to cast to ourselves
		if from == to {
			return Ok(to);
		}

		let from_index = from;
		let to_index = to;

		// Acquire the type info from the type decleration
		let (from, to) = (
			match &self.statements[from] {
				Statement::Decleration {
					mutable: _,
					ty: _,
					value,
				} => match value {
					Expression::Type(ty) => ty,
					_ => unreachable!(),
				},
				_ => unimplemented!(),
			},
			match &self.statements[to] {
				Statement::Decleration {
					mutable: _,
					ty: _,
					value,
				} => match value {
					Expression::Type(ty) => ty,
					_ => unreachable!(),
				},
				_ => unimplemented!(),
			},
		);

		// Check if the types can implicit cast
		let can_cast = matches!(
			(from, to),
			(Type::Int, Type::Float)
				| (Type::Float, Type::Int)
				| (Type::Int, Type::Int)
				| (Type::Float, Type::Float)
				| (Type::String, Type::String)
		);

		// If so return the to_index
		if can_cast {
			Ok(to_index)
		}
		// If not then throw an error
		else {
			Err(GeneratorError::CanNotImplicitCast {
				from: from_index,
				to: to_index,
			})
		}
	}

	fn generate_body(&mut self, ast: &Ast, body: &ast::Body) -> Result<usize> {
		let block = self.statements.len();
		self.statements.push(Statement::Block);

		for statement in &body.statements {
			if let Some(statement) = self.generate_statement(ast, statement)? {
				self.statements.push(statement);
			}
		}

		Ok(block)
	}

	fn generate_statement(
		&mut self,
		ast: &Ast,
		statement: &ast::Statement,
	) -> Result<Option<Statement>> {
		match statement {
			ast::Statement::Decleration(decl) => {
				// Generate the expression IR
				let value = self.generate_expression(ast, &decl.value, 0)?;

				// Add this statement into the symbol lookup table. This effectively does SSA as we
				// will update the variable index to the new variable decl.
				let name = decl.name.slice_str(ast.src).to_string();
				let index = self.statements.len();
				self.symbols.insert(name, index);

				// Determine what the type of this expression should be
				let ty = self.infer_type(&value)?;

				// If the decleration provides type info ensure that the expression can implicit
				// cast to it
				let ty = if let Some(decl_ty) = decl.ty {
					let name = decl_ty.slice_str(ast.src);
					let decl_ty = *self
						.symbols
						.get(name)
						.ok_or(GeneratorError::UnknownDecleration(name.to_string()))?;
					self.implicit_cast(ty, decl_ty)?
				}
				// Otherwise use the inferred type
				else {
					ty
				};

				Ok(Some(Statement::Decleration {
					mutable: decl.mutable,
					ty,
					value,
				}))
			}
			ast::Statement::Expression(expr) => {
				self.generate_expression(ast, expr, 0)?;
				Ok(None)
			}
			_ => unimplemented!(),
		}
	}

	fn generate_expression(
		&mut self,
		ast: &Ast,
		expression: &ast::Expression,
		depth: u8,
	) -> Result<Expression> {
		match expression {
			ast::Expression::Assignment(assignment) => {
				let name = assignment.variable.slice_str(ast.src);
				match self.symbols.get(name) {
					Some(s) => {
						if let Statement::Decleration { mutable, ty, value } =
							self.statements[*s].clone()
						{
							// Determine if original decleration was marked as mutable
							if !mutable {
								return Err(GeneratorError::DeclerationImmutable(name.to_string()));
							}

							// Generate expression before pushing new SSA decleration
							let new_value =
								self.generate_expression(ast, &assignment.value, depth)?;

							// Determine what the type of this expression should be
							let inferred_ty = self.infer_type(&value)?;
							let ty = self.implicit_cast(inferred_ty, ty)?;

							// Update the symbols table with the newest decleration value and push
							// the new decleration
							let index = self.statements.len();
							self.symbols.insert(name.to_string(), index);
							self.statements.push(Statement::Decleration {
								mutable,
								ty,
								value: new_value,
							});

							Ok(Expression::Variable { target: index })
						} else {
							unreachable!();
						}
					}
					None => Err(GeneratorError::UnknownDecleration(name.to_string())),
				}
			}
			ast::Expression::Constant(value) => {
				// Convert the ast::Constant (which has no heap allocations) to the memory owning
				// ir::Constant
				let value = match value {
					ast::Constant::Bool(value) => Constant::Bool(*value),
					ast::Constant::Int(value) => Constant::Int(*value),
					ast::Constant::Float(value) => Constant::Float(*value),
					ast::Constant::String(value) => {
						Constant::String(value.slice_str(ast.src).to_string())
					}
				};

				// Add the constant to the list of constants
				let index = self.constants.len();
				self.constants.push(value);

				Ok(Expression::Constant { value: index })
			}
			ast::Expression::Identifier { name } => {
				// Query the identifier in the symbols table and throw an error if it does not
				// exist.
				let name = name.slice_str(ast.src);
				let target = *self
					.symbols
					.get(name)
					.ok_or(GeneratorError::UnknownDecleration(name.to_string()))?;
				Ok(Expression::Variable { target })
			}
			ast::Expression::Operation {
				operator: op,
				input,
			} => match op {
				ast::Op::Binary(op) => {
					let lhs = self.generate_expression(ast, &input[0], depth + 1)?;
					let rhs = self.generate_expression(ast, &input[1], depth + 1)?;

					let result = Expression::Op(Op::Binary {
						op: *op,
						lhs: lhs.as_op_arg().unwrap(),
						rhs: rhs.as_op_arg().unwrap(),
					});

					if depth == 0 {
						Ok(result)
					} else {
						let ty = self.infer_type(&result)?;

						let index = self.statements.len();
						self.statements.push(Statement::Decleration {
							mutable: false,
							ty,
							value: result,
						});
						Ok(Expression::Variable { target: index })
					}
				}
				_ => unimplemented!(),
			},
			ast::Expression::Branch {
				if_branch,
				else_if_branches,
				else_body,
			} => {
				self.generate_branch(ast, if_branch)?;

				for branch in else_if_branches.iter() {
					self.generate_branch(ast, branch)?;
				}

				if let Some(body) = else_body {
					self.generate_body(ast, body)?;
				}

				todo!()
			}
			_ => unimplemented!(),
		}
	}

	fn generate_branch(&mut self, ast: &Ast, branch: &ast::Branch) -> Result<()> {
		let condition = self.generate_expression(ast, &branch.condition, 0)?;
		let ty = self.infer_type(&condition)?;
		let ty = self.implicit_cast(ty, Self::BOOL_TYPE_DECL)?;

		let index = self.statements.len();
		self.statements.push(Statement::Decleration {
			mutable: false,
			ty,
			value: condition,
		});
		let condition = index;

		self.statements.push(Statement::JumpIf {
			condition,
			if_true: 0,
			if_false: 0,
		});
		self.generate_body(ast, &branch.body)?;

		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;

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
		#[allow(clippy::needless_range_loop)]
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

	fn print_expression(
		generator: &Generator,
		index_to_symbol: &HashMap<usize, String>,
		expression: &Expression,
	) {
		match expression {
			Expression::NoOp => {
				print!("noop");
			}
			Expression::Variable { target } => {
				if let Some(name) = index_to_symbol.get(target) {
					print!("{}", name);
				} else {
					print!("%{}", *target);
				}
			}
			Expression::Constant { value } => {
				print!("{:?}", generator.constants[*value]);
			}
			Expression::Type(ty) => {
				print!("{:?}", ty);
			}
			Expression::Op(op) => match op {
				Op::Binary { op, lhs, rhs } => {
					let op = match op {
						BinaryOp::Add => "add",
						BinaryOp::Sub => "sub",
						BinaryOp::Mul => "mul",
						BinaryOp::Div => "div",

						BinaryOp::EqualTo => "eq",
						BinaryOp::NotEqualTo => "neq",
						BinaryOp::LessThan => "lt",
						BinaryOp::LessThanOrEqual => "lte",
						BinaryOp::GreaterThan => "gt",
						BinaryOp::GreaterThanOrEqual => "gte",

						BinaryOp::LogicalAnd => "and",
						BinaryOp::LogicalOr => "or",
					};
					print!("{:^3} ", op);
					print_expression(generator, index_to_symbol, &lhs.to_expression());
					print!(", ");
					print_expression(generator, index_to_symbol, &rhs.to_expression());
				}
				Op::Unary(_, _) => {
					unimplemented!()
				}
			},
		}
	}

	#[test]
	fn generate() {
		let script = r#"
            let mut i: Float = 45 + (123 + 123) * 42.0;
            i = 420;
            let foo = false || (true && i > 200);
            if i > 420 {
                i = 100;
            } else {
                i = 0;
            }
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
						LexerError::UnexpectedEOF => {
							println!("Error: unexpected EOF");
							print_location(script, Location::new(script.len(), script.len()));
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

		let mut generator = Generator::new();
		generator.generate(&ast).unwrap();
		let index_to_symbol: HashMap<_, _> = generator
			.symbols
			.iter()
			.map(|(k, v)| (*v, k.clone()))
			.collect();

		println!();
		for (index, statement) in generator.statements.iter().enumerate() {
			match statement {
				Statement::Decleration {
					mutable: _,
					ty,
					value,
				} => {
					if *ty == index {
						continue;
					}

					if let Some(name) = index_to_symbol.get(&index) {
						print!("{:>16}", name);
					} else {
						print!("{:>16}", format!("%{}", index));
					}

					print!(": ");
					if let Some(name) = index_to_symbol.get(ty) {
						print!("{:<5} = ", name);
					} else {
						print!("%{:<5} = ", index);
					}

					print_expression(&generator, &index_to_symbol, value);
					println!(";");
				}
				Statement::Jump { target } => {
					print!("{:>16}  ", "br");

					if let Some(name) = index_to_symbol.get(target) {
						print!("{}", name);
					} else {
						print!("%{}", target);
					}
				}
				Statement::JumpIf {
					condition,
					if_true,
					if_false,
				} => {
					print!("{:>16}  ", "br");

					if let Some(name) = index_to_symbol.get(condition) {
						print!("{}", name);
					} else {
						print!("%{}", condition);
					}
					println!(", %{}, %{}", if_true, if_false);
				}
				_ => println!("{:?}", statement),
			}
		}
		println!();
	}
}
