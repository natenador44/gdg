use anyhow::{Context, Result, anyhow};
use std::str::Chars;

use crate::tokens::Token;

pub struct Parser {
    original_text: String,
    pos: usize,
}

pub struct Next<'a> {
    parser: &'a mut Parser,
    token: Token,
    consume_len: usize,
}

impl<'a> Next<'a> {
    pub fn peek(&self) -> &Token {
        &self.token
    }

    pub fn consume(self) -> Token {
        self.parser.eat(self.consume_len);
        self.token
    }
}

impl Parser {
    pub fn new(original_text: String) -> Self {
        Self {
            original_text,
            pos: 0,
        }
    }

    pub fn expect_n<F>(&mut self, n: usize, expecting: F, desc: &str) -> Result<()>
    where
        F: Fn(&Token) -> bool,
    {
        for _ in 0..n {
            let next = self
                .next()?
                .ok_or(anyhow!("expected {n} '{desc}', got EOI"))?;

            if !expecting(next.peek()) {
                return Err(
                    anyhow!("expected {n} '{desc}', got {:?} instead", next.peek())
                        .context(self.err_context()),
                );
            }

            next.consume();
        }

        Ok(())
    }

    pub fn eat(&mut self, amt: usize) {
        self.pos += amt;
    }

    fn err_context(&self) -> String {
        let context = &self.original_text
            [self.pos.saturating_sub(25)..(self.pos + 25).min(self.original_text.len())];
        format!("Position: {}, context: '{}'", self.pos, context)
    }

    pub fn expect<F>(&mut self, expecting: F) -> Result<Token>
    where
        F: Fn(&Token) -> bool,
    {
        let next = self.next()?.ok_or(anyhow!("expected token, got EOI"))?;
        if expecting(next.peek()) {
            Ok(next.consume())
        } else {
            Err(anyhow!("unexpected token: {:?}", next.peek()).context(self.err_context()))
        }
    }

    pub fn expect_map<F, T>(&mut self, expect_map: F, desc: &str) -> Result<T>
    where
        F: Fn(&Token) -> Option<T>,
    {
        let next = self.next()?.ok_or(anyhow!("expected '{desc}', got EOI"))?;
        match expect_map(next.peek()) {
            Some(v) => {
                next.consume();
                Ok(v)
            }
            None => Err(anyhow!("expected '{desc}', got {:?} instead", next.peek())
                .context(self.err_context())),
        }
    }

    pub fn is_consumed(&self) -> bool {
        self.pos >= self.original_text.len()
    }

    pub fn next<'a>(&'a mut self) -> Result<Option<Next<'a>>> {
        if self.is_consumed() {
            Ok(None)
        } else {
            let remaining = &self.original_text[self.pos..];
            let mut chars = remaining.chars();

            let Some(next) = chars.next() else {
                return Ok(None);
            };

            let mut space_count = 0;

            let token = match next {
                ' ' => {
                    let mut indentation_count = 1;
                    loop {
                        if indentation_count == 4 {
                            break Some(Token::Indentation);
                        }
                        // four spaces == Indentation
                        let Some(next) = chars.next() else {
                            return Ok(None);
                        };

                        match next {
                            ' ' => {
                                indentation_count += 1;
                            }
                            other => {
                                space_count = indentation_count;
                                break identify_non_indentation_token(other, &mut chars)
                                    .context(self.err_context())?;
                            }
                        }
                    }
                }
                other => {
                    identify_non_indentation_token(other, &mut chars).context(self.err_context())?
                }
            };

            let Some(token) = token else {
                return Ok(None);
            };

            Ok(Some(Next {
                parser: self,
                consume_len: space_count + token.len(),
                token,
            }))
        }
    }

    pub fn remaining(&self) -> &str {
        if self.is_consumed() {
            ""
        } else {
            &self.original_text[self.pos..]
        }
    }
}

fn parse_ident(c: char, chars: &mut Chars) -> String {
    let mut ident = String::from(c);

    while let Some(c) = chars.next() {
        if c.is_alphanumeric() || c == '_' || c == '-' {
            ident.push(c);
        } else {
            break;
        }
    }

    ident
}

// fn skip_whitespace(chars: &mut Chars) -> Option<(usize, char)> {
//     let mut whitespace_count = 0;
//     while let Some(c) = chars.next() {
//         if !c.is_whitespace() || c == '\n' || c == 't' {
//             return Some((whitespace_count, c));
//         }
//         whitespace_count += 1;
//     }
//     None
// }

fn identify_non_indentation_token(next: char, chars: &mut Chars) -> Result<Option<Token>> {
    let token = match next {
        '+' => Token::Plus,
        '-' => {
            let Some(next) = chars.next() else {
                return Ok(None);
            };
            match next {
                '>' => Token::Arrow,
                _ => Token::HorizontalPipe,
            }
        }
        '|' => Token::VerticalPipe,
        '.' => Token::Period,
        ':' => Token::Colon,
        '(' => Token::ParenOpen,
        ')' => Token::ParenClose,
        '*' => Token::Asterisk,
        '>' => Token::RightCaret,
        '\'' => Token::SingleQuote,
        '\\' => Token::Backslash,
        ',' => Token::Comma,
        '\n' => Token::NewLine,
        ';' => Token::SemiColon,
        c if c.is_alphanumeric() || c == '_' => {
            let ident = parse_ident(c, chars);
            if ident == "c" {
                Token::C
            } else if ident == "n" {
                Token::N
            } else {
                Token::Ident(ident)
            }
        }
        other => return Err(anyhow!("unknown character '{other}")),
    };

    Ok(Some(token))
}
