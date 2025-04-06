use {
	crate::{
		lexer::Location,
		parser::{ParseError, Parser},
	},
	root::{collections::HashMap, rc::Rc},
};

#[derive(Debug)]
pub struct Ast<'a> {
	pub src: &'a str,
	pub body: Body,
}

impl<'a> Ast<'a> {
	pub fn new(src: &'a str) -> Result<Ast, ParseError> {
		Parser::parse(src)
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
	LogicalAnd,
	LogicalOr,
	LogicalNot,
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
		left: Box<Expression>,
		operator: Operator,
		right: Box<Expression>,
	},
	Constant {
		kind: Constant,
		value: Location,
	},
	Identifier {
		name: Location,
	},
	Parenthesis {
		expression: Box<Expression>,
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
			Expression::Operation {
				left,
				operator,
				right,
			} => {
				let left_value = self.evaluate_expression(ast, left)?;
				let right_value = self.evaluate_expression(ast, right)?;
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

					_ => Err("Unsupported operator".to_string()),
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
		let script = "let mut foo = 45.0 + 17 - 12;";
		let ast = Ast::new(script).unwrap();

		let mut vm = VM::new();
		vm.execute(&ast).unwrap();

		println!("foo: {:?}", vm.get_variable("foo"));
	}
}
