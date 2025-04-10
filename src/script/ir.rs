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

#[derive(Clone)]
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
			Const::Float(v) => write!(f, "{}f64", v),
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
pub struct Phi(Vec<(BlockId, DeclId)>);

#[derive(Debug, Clone)]
pub enum Expr {
	Op(Op),
	Decl(DeclId),
	Const(ConstId),
	Type(Type),
	Phi(Phi),
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
		#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
define_id!(JumpId);
define_id!(JumpIfId);

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
	Block(ScopeId),
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
	locals: HashMap<String, DeclId>,
	mutated: HashMap<String, DeclId>,
}

#[derive(Debug, Default)]
pub struct Generator {
	scopes: Vec<Scope>,
	stack: Vec<ScopeId>,
	current_block: Option<BlockId>,

	constants: Vec<Const>,
	ir: Vec<Ir>,
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
			current_block: None,
		};

		let scope = ScopeId(result.scopes.len());
		result.push_block(scope);

		result.scopes.push(Scope {
			parent: None,
			locals: HashMap::new(),
			mutated: HashMap::new(),
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
		DeclId(self.push_ir(Ir::Decl(decl)))
	}

	fn push_local(&mut self, name: &str, decl: Decl) -> DeclId {
		let decl = self.push_decl(decl);

		let scope = self.top_scope();
		let scope = self.scope_mut(scope);

		if let Some(slot) = scope.locals.get_mut(name) {
			*slot = decl;
		} else {
			scope.locals.insert(name.to_string(), decl);
		}
		decl
	}

	fn decl(&self, decl: DeclId) -> &Decl {
		match &self.ir[decl.0] {
			Ir::Decl(d) => d,
			_ => unreachable!(),
		}
	}

	fn query_local(&self, start: ScopeId, name: &str) -> Option<DeclId> {
		let mut id = start;
		loop {
			let scope = self.scope(id);

			if let Some(decl) = scope.locals.get(name) {
				return Some(*decl);
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

	fn jump_mut(&mut self, id: JumpId) -> &mut BlockId {
		match &mut self.ir[id.0] {
			Ir::Jump(d) => d,
			_ => unreachable!(),
		}
	}

	fn jump_if_mut(&mut self, id: JumpIfId) -> &mut JumpIf {
		match &mut self.ir[id.0] {
			Ir::JumpIf(d) => d,
			_ => unreachable!(),
		}
	}

	fn push_block(&mut self, scope: ScopeId) -> BlockId {
		let id = BlockId(self.push_ir(Ir::Block(scope)));
		self.current_block = Some(id);
		id
	}

	fn block_scope(&self, id: BlockId) -> ScopeId {
		match &self.ir[id.0] {
			Ir::Block(d) => *d,
			_ => unreachable!(),
		}
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
			Expr::Phi(phi) => {
				let decl = phi.0[0].1;
				Ok(self.decl(decl).type_decl)
			}
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
		let scope = ScopeId(self.scopes.len());
		let block = self.push_block(scope);

		self.scopes.push(Scope {
			parent: Some(self.top_scope()),
			locals: HashMap::new(),
			mutated: HashMap::new(),
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
				let inferred = self.infer_type(&value)?;

				// If the decleration provides type info ensure that the expression can implicit
				// cast to it
				let ty = if let Some(decl_ty) = decl.ty {
					let name = decl_ty.slice_str(ast.src);
					let decl_ty = self
						.query_local(self.top_scope(), name)
						.ok_or(Error::UnknownDecl(name.to_string()))?;
					self.implicit_cast(inferred, decl_ty)?
				}
				// Otherwise use the inferred type
				else {
					inferred
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
					.ok_or_else(|| Error::UnknownDecl(name.to_string()))?;
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

				let scope = self.top_scope();
				let scope = self.scope_mut(scope);
				scope.mutated.insert(name.to_string(), index);

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
					.ok_or(Error::UnknownDecl(name.to_string()))?;

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
				branches,
				else_body,
			} => {
				// There should always be atleast 1 branch as the parser cant parse a naked else
				// statement
				assert!(!branches.is_empty());

				// The hard part about branching is needing to...
				// a. Go back and manipulate Ir after its been pushed. This means you need to do
				// some book keeping about what Ir you need to go back to
				// b. For phi resolution you need to keep track of what declerations have been
				// mutated in branch scopes and what block those mutations occurred in

				// Retain the current block incase there in no else block and phi resolution
				// requires original blocks decl
				let original = self.current_block.unwrap();
				let mut phi_original = vec![original];

				// Keep track of:
				// - All jump IR that needs to be patched with the soon to be created merge block
				// - Block id's to all children branch scope for later phi solving
				// - The last jump-if statement as we will need to patch its false_block to be else
				// body or merge block
				let mut phi_candidates = Vec::new();
				let mut merge_jumps = Vec::with_capacity(branches.len());
				let mut last_jump_if = None;
				for (index, branch) in branches.iter().enumerate() {
					// Evaluate the branch and then append its jump location that will later be
					// patched when the merge block is created.
					let (jump_if, merge_jump) = self.eval_branch(ast, branch)?;
					merge_jumps.push(merge_jump);

					phi_candidates.push(self.current_block.unwrap());

					// For any branch that isnt the last before the else body, create a block in
					// the current scope so we can evaluate the next branch condition and branch
					if index < branches.len() - 1 {
						let false_block = self.push_block(self.top_scope());
						self.jump_if_mut(jump_if).false_block = false_block;

						// TODO: This is only required for the last if statement that if false goes
						// to merge block
						phi_original.push(false_block);
					} else {
						last_jump_if = Some(jump_if);
					}
				}
				let last_jump_if = last_jump_if.unwrap();

				// If there is an else body append a jump to merge block and then patch the last
				// jump if's false_block to be this else block
				if let Some(body) = else_body {
					// Eval the else body and append the block to the last jump if
					let else_block = self.eval_body(ast, body)?;
					self.jump_if_mut(last_jump_if).false_block = else_block;

					// Else body could have mutated locals so its a merge candidate
					phi_candidates.push(self.current_block.unwrap());

					// Append a merge jump that will later be patched with the yet to be created
					// merge block
					let merge_jump = JumpId(self.push_ir(Ir::Jump(BlockId(0))));
					merge_jumps.push(merge_jump);
				}

				// Create the merge block and then patch previous branch IR
				let merge_block = self.push_block(self.top_scope());
				for jump in merge_jumps.iter() {
					*self.jump_mut(*jump) = merge_block;
				}

				// Patch merge into false_block of last jump if if no else body is provided
				if else_body.is_none() {
					self.jump_if_mut(last_jump_if).false_block = merge_block;
				}

				// We need to first figure out what local decls have been mutated. Since we're doing SSA
				// there is no actual mutation and we just declare a new variables when we want it
				// to change. For each mutated local we need to then keep track of what block and
				// decl is the result for our mutation
				let mut resolve: HashMap<String, Vec<(BlockId, DeclId)>> = HashMap::new();

				// For every phi candidate determine what variables mutated in that scope.
				for phi in phi_candidates.iter() {
					// Scope is how we track what has been "mutated"
					let scope = self.block_scope(*phi);
					let scope = self.scope(scope);

					// Multiple blocks can point to the same scope so there may be duplications. We handle that
					// by using a HashMap of local variables and list of what blocks they were changed
					// in along with the decl that is the new value.
					for (mutated, decl) in scope.mutated.iter() {
						let map = if let Some(entry) = resolve.get_mut(mutated) {
							entry
						} else {
							resolve.entry(mutated.to_string()).or_default()
						};
						map.push((*phi, *decl));
					}
				}

				// If we have no else body then we need to append the original value to the phi
				// expression
				if else_body.is_none() {
					for block in phi_original.iter() {
						for (name, map) in resolve.iter_mut() {
							let scope = self.top_scope();
							let scope = self.scope(scope);
							let decl = scope.locals.get(name).cloned().unwrap();
							map.push((*block, decl));
						}
					}
				}

				// Now we finally get to output the actual phi operator. This is how we handle
				// different branches mutating variables in SSA. Phi takes in the block and the
				// decl and it essentially means if we come from this block use this value
				for (name, map) in resolve.iter() {
					let params = map.iter().map(|(k, v)| (*k, *v)).collect();
					let new_value = Expr::Phi(Phi(params));

					// All the types have been ensured to have been valid when they were assigned
					// in children scopes. We can kind of relax here.
					let inferred = self.infer_type(&new_value)?;

					// Push the new assignment using the phi expression
					let index = self.push_local(
						name,
						Decl {
							mutable: true,
							type_decl: inferred,
							value: new_value,
						},
					);

					// Record that this variable has been mutated on this scope
					let scope = self.top_scope();
					let scope = self.scope_mut(scope);
					scope.mutated.insert(name.to_string(), index);
				}

				Ok(None)
			}
			_ => unimplemented!(),
		}
	}

	fn eval_branch(&mut self, ast: &Ast, branch: &ast::Branch) -> Result<(JumpIfId, JumpId)> {
		let condition = self.eval_expr(ast, &branch.condition, 0)?.unwrap();
		let inferred = self.infer_type(&condition)?;
		let bool_decl = self.implicit_cast(inferred, BOOL_DECL)?;

		let condition = self.push_decl(Decl {
			mutable: false,
			type_decl: bool_decl,
			value: condition,
		});

		let jump_if = JumpIfId(self.push_ir(Ir::JumpIf(JumpIf {
			cond: condition,
			true_block: BlockId(0),
			false_block: BlockId(0),
		})));

		let true_block = self.eval_body(ast, &branch.body)?;
		match &mut self.ir[jump_if.0] {
			Ir::JumpIf(jump_if) => {
				jump_if.true_block = true_block;
			}
			_ => unreachable!(),
		}
		let merge = JumpId(self.push_ir(Ir::Jump(BlockId(0))));

		Ok((jump_if, merge))
	}
}

#[derive(Debug)]
pub struct Interpreter {
	ir: Vec<Ir>,
	consts: Vec<Const>,
	pc: usize,
	last_block: BlockId,
	current_block: BlockId,
	decls: HashMap<DeclId, Const>,
}

impl Interpreter {
	pub fn run(ir: Vec<Ir>, consts: Vec<Const>) -> Self {
		let mut interp = Self {
			ir,
			consts,
			pc: 0,
			last_block: BlockId(0),
			current_block: BlockId(0),
			decls: HashMap::new(),
		};
		'outer: loop {
			match &interp.ir[interp.pc] {
				Ir::Block(_) => {
					interp.last_block = interp.current_block;
					interp.current_block = BlockId(interp.pc);
				}
				Ir::Decl(decl) => {
					// TODO: Ensure that value is casted to the correct type
					let value = match &decl.value {
						Expr::Decl(d) => interp.decl(*d),
						Expr::Const(d) => interp.consts(*d),
						Expr::Phi(p) => {
							let mut r = None;
							for (block, decl) in p.0.iter() {
								if interp.last_block == *block {
									r = Some(interp.decl(*decl));
								}
							}
							r.unwrap()
						}
						Expr::Type(_) => {
							interp.pc += 1;
							continue 'outer;
						}
						Expr::Op(op) => match op {
							Op::Binary { op, lhs, rhs } => {
								let lhs = interp.arg(*lhs);
								let rhs = interp.arg(*rhs);

								match op {
									BinaryOp::Add => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Int(x + y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Float(x + (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Float((*x as f64) + y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Float(x + y),
										_ => unreachable!(),
									},
									BinaryOp::Sub => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Int(x - y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Float(x - (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Float((*x as f64) - y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Float(x - y),
										_ => unreachable!(),
									},
									BinaryOp::Mul => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Int(x * y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Float(x * (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Float((*x as f64) * y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Float(x * y),
										_ => unreachable!(),
									},
									BinaryOp::Div => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Int(x / y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Float(x / (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Float((*x as f64) / y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Float(x / y),
										_ => unreachable!(),
									},
									BinaryOp::EqualTo => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x == y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x == (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) == *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x == y),
										(Const::Bool(x), Const::Bool(y)) => Const::Bool(x == y),
										(Const::String(x), Const::String(y)) => Const::Bool(x == y),
										_ => unreachable!(),
									},
									BinaryOp::NotEqualTo => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x != y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x != (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) != *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x != y),
										(Const::Bool(x), Const::Bool(y)) => Const::Bool(x != y),
										(Const::String(x), Const::String(y)) => Const::Bool(x != y),
										_ => unreachable!(),
									},
									BinaryOp::LessThan => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x < y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x < (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) < *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x < y),
										_ => unreachable!(),
									},
									BinaryOp::LessThanOrEqual => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x <= y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x <= (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) <= *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x <= y),
										_ => unreachable!(),
									},
									BinaryOp::GreaterThan => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x > y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x > (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) > *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x > y),
										_ => unreachable!(),
									},
									BinaryOp::GreaterThanOrEqual => match (&lhs, &rhs) {
										(Const::Int(x), Const::Int(y)) => Const::Bool(x >= y),
										(Const::Float(x), Const::Int(y)) => {
											Const::Bool(*x >= (*y as f64))
										}
										(Const::Int(x), Const::Float(y)) => {
											Const::Bool((*x as f64) >= *y)
										}
										(Const::Float(x), Const::Float(y)) => Const::Bool(x >= y),
										_ => unreachable!(),
									},
									_ => unimplemented!(),
								}
							}
							_ => unimplemented!(),
						},
					};

					interp.decls.insert(DeclId(interp.pc), value);
				}
				Ir::Jump(jump) => {
					interp.pc = jump.0;
					continue 'outer;
				}
				Ir::JumpIf(jump_if) => match interp.decl(jump_if.cond) {
					Const::Bool(x) => {
						if x {
							interp.pc = jump_if.true_block.0;
							continue 'outer;
						} else {
							interp.pc = jump_if.false_block.0;
							continue 'outer;
						}
					}
					_ => unreachable!(),
				},
			}
			interp.pc += 1;
			if interp.pc >= interp.ir.len() {
				break;
			}
		}

		interp
	}

	fn arg(&self, arg: Arg) -> Const {
		match arg {
			Arg::Decl(x) => self.decl(x),
			Arg::Const(x) => self.consts(x),
		}
	}

	fn decl(&self, id: DeclId) -> Const {
		self.decls.get(&id).cloned().unwrap()
	}

	fn consts(&self, id: ConstId) -> Const {
		self.consts[id.0].clone()
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
			Expr::Phi(phi) => {
				print!("phi ");
				for (i, p) in phi.0.iter().enumerate() {
					print!("[block({}), {}]", p.0, p.1);
					if i < phi.0.len() - 1 {
						print!(", ");
					}
				}
			}
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
            let mut i: Float = 45 + (123 + 123) * 42.5;

            if i > 420 {
                if i == 0 {
                    i = i - 100;
                }
                else {
                    i = 0;
                }
            } else if i < 120 {
                i = 123;
            } 

            i = i + 100;
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
				Ir::Block(_) => {
					println!();
					println!("block({})", BlockId(id));
				}
			}
		}
		println!();

		let Generator {
			scopes: _,
			stack: _,
			current_block: _,
			constants,
			ir,
		} = generator;
		let interp = Interpreter::run(ir, constants);
		println!("{:#?}", interp.decls);
	}
}
