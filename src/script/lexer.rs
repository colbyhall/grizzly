use root::{iter::Iterator, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
	Identifier,

	Int,
	Float,
	String,
	True,
	False,

	Let,
	Mut,
	If,
	Else,
	While,
	For,
	Fn,
	Return,

	LParen,
	RParen,
	LBracket,
	RBracket,
	LCurly,
	RCurly,
	Comma,
	Not,
	Equal,
	EqualTo,
	NotEqualTo,

	And,
	Or,

	Plus,
	Minus,
	Colon,
	Semicolon,
	Star,
	Slash,

	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
}

impl TokenKind {
	pub fn is_keyword(self) -> bool {
		matches!(
			self,
			TokenKind::Let
				| TokenKind::Mut
				| TokenKind::If
				| TokenKind::Else
				| TokenKind::While
				| TokenKind::For
				| TokenKind::Fn
				| TokenKind::True
				| TokenKind::False
		)
	}

	pub fn is_operator(self) -> bool {
		matches!(
			self,
			TokenKind::Plus
				| TokenKind::Minus
				| TokenKind::Star
				| TokenKind::Slash
				| TokenKind::Not
				| TokenKind::LessThan
				| TokenKind::LessThanOrEqual
				| TokenKind::GreaterThan
				| TokenKind::GreaterThanOrEqual
		)
	}
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
	start: u32,
	end: u32,
}

impl Location {
	pub fn new(start: usize, end: usize) -> Self {
		Self {
			start: start as _,
			end: end as _,
		}
	}

	#[inline]
	pub fn start(self) -> usize {
		self.start as _
	}

	#[inline]
	pub fn end(self) -> usize {
		self.end as _
	}

	#[inline]
	pub fn slice_str(self, s: &str) -> &str {
		&s[self.start()..self.end()]
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
	pub kind: TokenKind,
	pub location: Location,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
	original: &'a str,
	chars: Chars<'a>,
	byte_offset: usize,
}

impl<'a> Lexer<'a> {
	pub fn new(input: &'a str) -> Self {
		Lexer {
			original: input,
			chars: input.chars(),
			byte_offset: 0,
		}
	}

	pub fn consume_while<F>(&mut self, mut predicate: F) -> &'a str
	where
		F: FnMut(char) -> bool,
	{
		let start = self.byte_offset;
		for c in self.chars.clone() {
			if !predicate(c) {
				break;
			}
			self.chars.next();
			self.byte_offset += c.len_utf8();
		}
		&self.original[start..self.byte_offset]
	}
}

#[derive(Clone, Copy, Debug)]
pub enum LexerError {
	UnexpectedChar(usize, char),
	UnexpectedEOF,
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Result<Token, LexerError>;

	fn next(&mut self) -> Option<Self::Item> {
		let peeked = self.chars.clone();
		for c in peeked {
			let start = self.byte_offset;
			return Some(match c {
				'0'..='9' => {
					let mut found_decimal = false;
					self.consume_while(|c| {
						if !found_decimal && c == '.' {
							found_decimal = true;
							return true;
						}

						c.is_ascii_digit()
					});
					let end = self.byte_offset;

					if found_decimal {
						Ok(Token {
							kind: TokenKind::Float,
							location: Location::new(start, end),
						})
					} else {
						Ok(Token {
							kind: TokenKind::Int,
							location: Location::new(start, end),
						})
					}
				}
				'a'..='z' | 'A'..='Z' | '_' => {
					let ident = self.consume_while(|c| c.is_alphanumeric() || c == '_');
					let end = self.byte_offset;

					let kind = match ident {
						"let" => TokenKind::Let,
						"mut" => TokenKind::Mut,
						"if" => TokenKind::If,
						"else" => TokenKind::Else,
						"while" => TokenKind::While,
						"for" => TokenKind::For,
						"fn" => TokenKind::Fn,
						"return" => TokenKind::Return,
						"true" => TokenKind::True,
						"false" => TokenKind::False,
						_ => TokenKind::Identifier,
					};

					Ok(Token {
						kind,
						location: Location::new(start, end),
					})
				}
				'"' => {
					self.byte_offset += c.len_utf8();
					self.chars.next();

					let start = self.byte_offset;
					self.consume_while(|c| c != '"');
					let end = self.byte_offset;

					self.byte_offset += c.len_utf8();
					self.chars.next();

					Ok(Token {
						kind: TokenKind::String,
						location: Location::new(start, end),
					})
				}
				'+' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Plus,
						location: Location::new(start, end),
					})
				}
				'-' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Minus,
						location: Location::new(start, end),
					})
				}
				'*' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Star,
						location: Location::new(start, end),
					})
				}
				'/' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Slash,
						location: Location::new(start, end),
					})
				}
				'(' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::LParen,
						location: Location::new(start, end),
					})
				}
				')' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::RParen,
						location: Location::new(start, end),
					})
				}
				'{' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::LCurly,
						location: Location::new(start, end),
					})
				}
				'}' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::RCurly,
						location: Location::new(start, end),
					})
				}
				'[' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::LBracket,
						location: Location::new(start, end),
					})
				}
				']' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::RBracket,
						location: Location::new(start, end),
					})
				}
				',' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Comma,
						location: Location::new(start, end),
					})
				}
				':' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Colon,
						location: Location::new(start, end),
					})
				}
				';' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();
					Ok(Token {
						kind: TokenKind::Semicolon,
						location: Location::new(start, end),
					})
				}
				'=' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();

					let mut peeked = self.chars.clone();
					if peeked.next() == Some('=') {
						self.byte_offset += 1;
						let end = self.byte_offset;
						self.chars.next();
						Ok(Token {
							kind: TokenKind::EqualTo,
							location: Location::new(start, end),
						})
					} else {
						Ok(Token {
							kind: TokenKind::Equal,
							location: Location::new(start, end),
						})
					}
				}
				'!' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();

					let mut peeked = self.chars.clone();
					if peeked.next() == Some('=') {
						self.byte_offset += 1;
						let end = self.byte_offset;
						self.chars.next();
						Ok(Token {
							kind: TokenKind::NotEqualTo,
							location: Location::new(start, end),
						})
					} else {
						Ok(Token {
							kind: TokenKind::Not,
							location: Location::new(start, end),
						})
					}
				}
				'<' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();

					let mut peeked = self.chars.clone();
					if peeked.next() == Some('=') {
						self.byte_offset += 1;
						let end = self.byte_offset;
						self.chars.next();
						Ok(Token {
							kind: TokenKind::LessThanOrEqual,
							location: Location::new(start, end),
						})
					} else {
						Ok(Token {
							kind: TokenKind::LessThan,
							location: Location::new(start, end),
						})
					}
				}
				'>' => {
					self.byte_offset += c.len_utf8();
					let end = self.byte_offset;

					self.chars.next();

					let mut peeked = self.chars.clone();
					if peeked.next() == Some('=') {
						self.byte_offset += 1;
						let end = self.byte_offset;
						self.chars.next();
						Ok(Token {
							kind: TokenKind::GreaterThanOrEqual,
							location: Location::new(start, end),
						})
					} else {
						Ok(Token {
							kind: TokenKind::GreaterThan,
							location: Location::new(start, end),
						})
					}
				}
				'|' => {
					self.byte_offset += c.len_utf8();
					self.chars.next();

					let mut peeked = self.chars.clone();
					match peeked.next() {
						Some(next) => {
							if next == '|' {
								self.byte_offset += 1;
								let end = self.byte_offset;
								self.chars.next();
								Ok(Token {
									kind: TokenKind::Or,
									location: Location::new(start, end),
								})
							} else {
								Err(LexerError::UnexpectedChar(self.byte_offset, c))
							}
						}
						_ => Err(LexerError::UnexpectedEOF),
					}
				}
				'&' => {
					self.byte_offset += c.len_utf8();
					self.chars.next();

					let mut peeked = self.chars.clone();
					match peeked.next() {
						Some(next) => {
							if next == '&' {
								self.byte_offset += 1;
								let end = self.byte_offset;
								self.chars.next();
								Ok(Token {
									kind: TokenKind::Or,
									location: Location::new(start, end),
								})
							} else {
								Err(LexerError::UnexpectedChar(self.byte_offset, c))
							}
						}
						_ => Err(LexerError::UnexpectedEOF),
					}
				}
				' ' | '\t' | '\n' => {
					self.consume_while(|c| c.is_whitespace());
					continue;
				}
				_ => Err(LexerError::UnexpectedChar(self.byte_offset, c)),
			});
		}
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn suite() {
		let mut lexer = Lexer::new("let mut bar = a + (b - 42) + baz();").map(|x| x.unwrap());
		assert_eq!(
			lexer.next(),
			Some(Token {
				kind: TokenKind::Let,
				location: Location { start: 0, end: 3 },
			})
		);
		assert_eq!(
			lexer.next(),
			Some(Token {
				kind: TokenKind::Mut,
				location: Location { start: 3, end: 7 },
			})
		);
	}
}
