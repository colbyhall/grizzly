use {
	crate::ast::{
		self,
		Ast,
		BinaryOp,
		UnaryOp,
	},
	root::{
		collections::HashMap,
		fmt,
	},
};

#[derive(Debug, Clone)]
pub enum Type {
	Bool,
	Int,
	Float,
	String,
}

pub enum Const {
	Bool(bool),
	Int(i64),
	Float(f64),
	String(String),
}

impl fmt::Debug for Const {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Const::Bool(v) => write!(f, "{}", v),
			Const::Int(v) => write!(f, "{}", v),
			Const::Float(v) => write!(f, "{}", v),
			Const::String(v) => write!(f, "{:?}", v),
		}
	}
}

impl Const {
	pub fn type_decl(&self) -> DeclId {
		match self {
			Const::Bool(_) => BOOL_DECL,
			Const::Int(_) => INT_DECL,
			Const::Float(_) => FLOAT_DECL,
			Const::String(_) => STRING_TYPE_DECL,
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Arg {
	Decl(DeclId),
	Const(ConstId),
}

impl Arg {
	pub fn as_expr(self) -> Expr {
		match self {
			Arg::Decl(x) => Expr::Decl(x),
			Arg::Const(x) => Expr::Const(x),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Op {
	Binary { op: BinaryOp, lhs: Arg, rhs: Arg },
	Unary(UnaryOp, Arg),
}

#[derive(Debug, Clone)]
pub enum Expr {
	Op(Op),
	Decl(DeclId),
	Const(ConstId),
	Type(Type),
}

impl Expr {
	pub fn assume_arg(&self) -> Arg {
		match self {
			Expr::Decl(d) => Arg::Decl(*d),
			Expr::Const(c) => Arg::Const(*c),
			_ => unreachable!(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Decl {
	mutable: bool,
	type_decl: DeclId,
	value: Expr,
}

macro_rules! define_id {
	($name:ident) => {
		#[derive(Debug, Clone, Copy, PartialEq, Eq)]
		pub struct $name(usize);

		impl root::fmt::Display for $name {
			fn fmt(&self, f: &mut root::fmt::Formatter<'_>) -> root::fmt::Result {
				write!(f, "%{}", self.0)
			}
		}
	};
}

define_id!(DeclId);
define_id!(ConstId);
define_id!(BlockId);

#[derive(Debug, Clone)]
pub struct JumpIf {
	cond: DeclId,
	true_block: BlockId,
	false_block: BlockId,
}

#[derive(Debug, Clone)]
pub enum Ir {
	Decl(Decl),
	Jump(BlockId),
	JumpIf(JumpIf),
	Block,
}

#[derive(Debug)]
pub enum Error {
	CanNotCast { from: Type, to: Type },
	UnknownDecl(String),
	DeclImmutable(String),
}

type Result<T> = root::result::Result<T, Error>;

define_id!(ScopeId);

#[derive(Debug, Clone)]
pub struct Scope {
	parent: Option<ScopeId>,
	start: BlockId,
	locals: HashMap<String, DeclId>,
	used: Vec<DeclId>,
}

#[derive(Debug, Default)]
pub struct Generator {
	scopes: Vec<Scope>,
	stack: Vec<ScopeId>,
	ir: Vec<Ir>,
	constants: Vec<Const>,
}

const BOOL_DECL: DeclId = DeclId(1);
const INT_DECL: DeclId = DeclId(2);
const FLOAT_DECL: DeclId = DeclId(3);
const STRING_TYPE_DECL: DeclId = DeclId(4);

impl Generator {
	pub fn new() -> Self {
		let mut result = Self {
			scopes: Vec::new(),
			stack: Vec::new(),
			ir: Vec::new(),
			constants: Vec::new(),
		};

		let block = BlockId(result.push_ir(Ir::Block));

		let scope = ScopeId(result.scopes.len());
		result.scopes.push(Scope {
			parent: None,
			start: block,
			locals: HashMap::new(),
			used: Vec::new(),
		});
		result.stack.push(scope);

		result.register_primitive("Bool", Type::Bool);
		result.register_primitive("Int", Type::Int);
		result.register_primitive("Float", Type::Float);
		result.register_primitive("String", Type::String);

		result
	}

	pub fn generate(&mut self, ast: &Ast) -> Result<()> {
		self.eval_body(ast, &ast.body)?;
		Ok(())
	}
}

impl Generator {
	fn top_scope(&self) -> ScopeId {
		self.stack.last().copied().unwrap()
	}

	fn scope(&self, scope: ScopeId) -> &Scope {
		&self.scopes[scope.0]
	}

	fn scope_mut(&mut self, scope: ScopeId) -> &mut Scope {
		&mut self.scopes[scope.0]
	}

	fn push_ir(&mut self, ir: Ir) -> usize {
		let index = self.ir.len();
		self.ir.push(ir);
		index
	}

	fn push_decl(&mut self, decl: Decl) -> DeclId {
		let decl = DeclId(self.push_ir(Ir::Decl(decl)));

		let scope = self.top_scope();
		let scope = self.scope_mut(scope);
		scope.used.push(decl);

		decl
	}

	fn push_local(&mut self, name: &str, decl: Decl) -> DeclId {
		let top_scope = self.top_scope();
		let decl = self.push_decl(decl);

		if let Some((prev, prev_scope)) = self.query_local(top_scope, name) {
			let top_scope = self.scope_mut(top_scope);
			top_scope.used.push(prev);

			let scope = self.scope_mut(prev_scope);
			let slot = scope.locals.get_mut(name).unwrap();
			*slot = decl;
		} else {
			let top_scope = self.scope_mut(top_scope);

			if let Some(slot) = top_scope.locals.get_mut(name) {
				*slot = decl;
			} else {
				top_scope.locals.insert(name.to_string(), decl);
			}
		}
		decl
	}

	fn decl(&self, decl: DeclId) -> &Decl {
		match &self.ir[decl.0] {
			Ir::Decl(d) => d,
			_ => unreachable!(),
		}
	}

	fn query_local(&self, start: ScopeId, name: &str) -> Option<(DeclId, ScopeId)> {
		let mut id = start;
		loop {
			let scope = self.scope(id);

			if let Some(decl) = scope.locals.get(name) {
				return Some((*decl, id));
			}

			match scope.parent {
				Some(p) => {
					id = p;
				}
				None => break,
			}
		}

		None
	}

	fn constant(&self, c: ConstId) -> &Const {
		&self.constants[c.0]
	}

	fn register_primitive(&mut self, name: &str, ty: Type) {
		let index = self.ir.len();
		self.push_local(
			name,
			Decl {
				mutable: false,
				type_decl: DeclId(index),
				value: Expr::Type(ty),
			},
		);
	}

	/// Given an expression, return an index to the result type decleration
	fn infer_type(&mut self, expression: &Expr) -> Result<DeclId> {
		match expression {
			Expr::Op(op) => match op {
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
						Arg::Const(index) => self.constant(*index).type_decl(),
						Arg::Decl(index) => self.decl(*index).type_decl,
					};
					let rhs = match rhs {
						Arg::Const(index) => self.constant(*index).type_decl(),
						Arg::Decl(index) => self.decl(*index).type_decl,
					};

					if convert_to_bool {
						Ok(BOOL_DECL)
					} else {
						match (lhs, rhs) {
							(FLOAT_DECL, INT_DECL) | (INT_DECL, FLOAT_DECL) => Ok(FLOAT_DECL),
							_ => Ok(self.implicit_cast(rhs, lhs)?),
						}
					}
				}
				Op::Unary(_, value) => match value {
					Arg::Const(index) => Ok(self.constant(*index).type_decl()),
					Arg::Decl(index) => Ok(self.decl(*index).type_decl),
				},
			},
			Expr::Const(value) => Ok(self.constant(*value).type_decl()),
			Expr::Decl(target) => Ok(self.decl(*target).type_decl),
			_ => unimplemented!(),
		}
	}

	/// Determine a type can implicit cast to another, given the types decleration index. Return the result type decleration index
	fn implicit_cast(&self, from: DeclId, to: DeclId) -> Result<DeclId> {
		// If we're the same type decl index than we must be able to cast to ourselves
		if from == to {
			return Ok(to);
		}

		let to_index = to;

		// Acquire the type info from the type decleration
		let (from, to) = (
			match &self.decl(from).value {
				Expr::Type(ty) => ty,
				_ => unreachable!(),
			},
			match &self.decl(to).value {
				Expr::Type(ty) => ty,
				_ => unreachable!(),
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
			Err(Error::CanNotCast {
				from: from.clone(),
				to: to.clone(),
			})
		}
	}

	fn eval_body(&mut self, ast: &Ast, body: &ast::Body) -> Result<BlockId> {
		let block = BlockId(self.push_ir(Ir::Block));

		let scope = ScopeId(self.scopes.len());
		self.scopes.push(Scope {
			parent: Some(self.top_scope()),
			start: block,
			locals: HashMap::new(),
			used: Vec::new(),
		});

		self.stack.push(scope);
		for stmt in &body.stmt {
			self.eval_stmt(ast, stmt)?;
		}
		self.stack.pop();

		Ok(block)
	}

	fn eval_stmt(&mut self, ast: &Ast, statement: &ast::Stmt) -> Result<()> {
		match statement {
			ast::Stmt::Decleration(decl) => {
				// Generate the expression IR
				let value = self.eval_expr(ast, &decl.value, 0)?.unwrap();

				// Add this statement into the symbol lookup table. This effectively does SSA as we
				// will update the variable index to the new variable decl.
				let name = decl.name.slice_str(ast.src);

				// Determine what the type of this expression should be
				let ty = self.infer_type(&value)?;

				// If the decleration provides type info ensure that the expression can implicit
				// cast to it
				let ty = if let Some(decl_ty) = decl.ty {
					let name = decl_ty.slice_str(ast.src);
					let decl_ty = self
						.query_local(self.top_scope(), name)
						.ok_or(Error::UnknownDecl(name.to_string()))?
						.0;
					self.implicit_cast(ty, decl_ty)?
				}
				// Otherwise use the inferred type
				else {
					ty
				};

				self.push_local(
					name,
					Decl {
						mutable: decl.mutable,
						type_decl: ty,
						value,
					},
				);

				Ok(())
			}
			ast::Stmt::Expression(expr) => {
				self.eval_expr(ast, expr, 0)?;
				Ok(())
			}
			_ => unimplemented!(),
		}
	}

	fn eval_expr(&mut self, ast: &Ast, expression: &ast::Expr, depth: u8) -> Result<Option<Expr>> {
		match expression {
			ast::Expr::Asmt(assignment) => {
				// Query the local from the stack
				let name = assignment.variable.slice_str(ast.src);
				let local = self
					.query_local(self.top_scope(), name)
					.ok_or_else(|| Error::UnknownDecl(name.to_string()))?
					.0;
				let local = self.decl(local).clone();

				// Determine if original decleration was marked as mutable
				if !local.mutable {
					return Err(Error::DeclImmutable(name.to_string()));
				}

				// Generate expression before pushing new SSA decleration
				let new_value = self.eval_expr(ast, &assignment.value, depth)?.unwrap();

				// Ensure that the expression results in a type this local can use
				let inferred = self.infer_type(&local.value)?;
				let type_decl = self.implicit_cast(inferred, local.type_decl)?;

				// Update the symbols table with the newest decleration value and push
				// the new decleration
				let index = self.push_local(
					name,
					Decl {
						mutable: true,
						type_decl,
						value: new_value,
					},
				);

				Ok(Some(Expr::Decl(index)))
			}
			ast::Expr::Const(value) => {
				// Convert the ast::Constant (which has no heap allocations) to the memory owning
				// ir::Constant
				let value = match value {
					ast::Const::Bool(value) => Const::Bool(*value),
					ast::Const::Int(value) => Const::Int(*value),
					ast::Const::Float(value) => Const::Float(*value),
					ast::Const::String(value) => {
						Const::String(value.slice_str(ast.src).to_string())
					}
				};

				// Add the constant to the list of constants
				let index = self.constants.len();
				self.constants.push(value);

				Ok(Some(Expr::Const(ConstId(index))))
			}
			ast::Expr::Ident { name } => {
				let name = name.slice_str(ast.src);
				let decl = self
					.query_local(self.top_scope(), name)
					.ok_or(Error::UnknownDecl(name.to_string()))?
					.0;

				// Keep track that this identifier was used in the current scope
				let scope = self.top_scope();
				let scope = self.scope_mut(scope);
				scope.used.push(decl);

				Ok(Some(Expr::Decl(decl)))
			}
			ast::Expr::Op { op, input } => match op {
				ast::Op::Binary(op) => {
					let lhs = self.eval_expr(ast, &input[0], depth + 1)?.unwrap();
					let rhs = self.eval_expr(ast, &input[1], depth + 1)?.unwrap();

					let result = Expr::Op(Op::Binary {
						op: *op,
						lhs: lhs.assume_arg(),
						rhs: rhs.assume_arg(),
					});

					if depth == 0 {
						Ok(Some(result))
					} else {
						let type_decl = self.infer_type(&result)?;

						let index = self.push_decl(Decl {
							mutable: false,
							type_decl,
							value: result,
						});
						Ok(Some(Expr::Decl(index)))
					}
				}
				_ => unimplemented!(),
			},
			ast::Expr::Branch {
				if_branch,
				else_if_branches,
				else_body,
			} => {
				self.eval_branch(ast, if_branch)?;

				for branch in else_if_branches.iter() {
					self.eval_branch(ast, branch)?;
				}

				if let Some(body) = else_body {
					self.eval_body(ast, body)?;
				}

				Ok(None)
			}
			_ => unimplemented!(),
		}
	}

	fn eval_branch(&mut self, ast: &Ast, branch: &ast::Branch) -> Result<()> {
		let condition = self.eval_expr(ast, &branch.condition, 0)?.unwrap();
		let ty = self.infer_type(&condition)?;
		let ty = self.implicit_cast(ty, BOOL_DECL)?;

		let condition = self.push_decl(Decl {
			mutable: false,
			type_decl: ty,
			value: condition,
		});

		self.push_ir(Ir::JumpIf(JumpIf {
			cond: condition,
			true_block: BlockId(0),
			false_block: BlockId(0),
		}));
		self.eval_body(ast, &branch.body)?;

		Ok(())
	}
}

#[cfg(test)]
mod test {
	use {
		super::*,
		crate::{
			ast::ParseError,
			lexer::{
				LexerError,
				Location,
			},
		},
	};

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

	fn print_expr(generator: &Generator, expression: &Expr) {
		match expression {
			Expr::Decl(target) => {
				print!("{}", *target);
			}
			Expr::Const(value) => {
				print!("const {:?}", generator.constant(*value));
			}
			Expr::Type(ty) => {
				print!("{:?}", ty);
			}
			Expr::Op(op) => match op {
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
					print!("{:>3} ", op);
					print_expr(generator, &lhs.as_expr());
					print!(", ");
					print_expr(generator, &rhs.as_expr());
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
		println!("{:#?}", generator);

		println!();
		for (id, ir) in generator.ir.iter().enumerate() {
			match ir {
				Ir::Decl(decl) => {
					let id = DeclId(id);
					let id = format!("{}", id);
					print!("{:>5}: {:>5} = ", id, decl.type_decl);
					print_expr(&generator, &decl.value);
					println!(";");
				}
				Ir::Jump(target) => {
					println!("{:>5}  {}", "br", target);
				}
				Ir::JumpIf(jump_if) => {
					println!(
						"{:>5}  {}, {}, {}",
						"br", jump_if.cond, jump_if.true_block, jump_if.false_block
					);
				}
				Ir::Block => {
					println!();
					println!("block({})", BlockId(id));
				}
			}
		}
		println!();
	}
}
