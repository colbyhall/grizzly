pub mod lexer;
pub mod parser;

const TEST_SCRIPT: &str = r#"
let x = 42;
let y = x + 1234;
let z = x + y;

print(z);
"#;
