use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Indentation,
    /// +
    Plus,
    /// -
    HorizontalPipe,
    /// |
    VerticalPipe,
    /// .
    Period,
    /// :
    Colon,
    /// ,
    Comma,
    /// >
    RightCaret,
    /// ->
    /// version conflict resolution, by gradle. The version on the right is what gradle selected
    Arrow,
    /// (
    ParenOpen,
    /// )
    ParenClose,
    /// [
    SquareOpen,
    /// ]
    SquareClose,
    /// '
    SingleQuote,
    /// \
    Backslash,
    /// *
    /// This transitive dependency subtree has already been listed
    Asterisk,
    /// c
    /// dependency contraint... not sure what that means right now
    C,
    /// n
    /// could not be resolved by gradle
    N,
    Ident(String),
    NewLine,
    SemiColon,
}

impl Token {
    pub fn len(&self) -> usize {
        match self {
            Token::Plus => '+'.len_utf8(),
            Token::HorizontalPipe => '-'.len_utf8(),
            Token::VerticalPipe => '|'.len_utf8(),
            Token::Period => '.'.len_utf8(),
            Token::Colon => ':'.len_utf8(),
            Token::RightCaret => '>'.len_utf8(),
            Token::Arrow => "->".len(),
            Token::ParenOpen => '('.len_utf8(),
            Token::ParenClose => ')'.len_utf8(),
            Token::Asterisk => '*'.len_utf8(),
            Token::C => 'c'.len_utf8(),
            Token::N => 'n'.len_utf8(),
            Token::Ident(i) => i.len(),
            Token::NewLine => '\n'.len_utf8(),
            Token::SingleQuote => '\''.len_utf8(),
            Token::Backslash => '\\'.len_utf8(),
            Token::Comma => ','.len_utf8(),
            Token::SemiColon => ';'.len_utf8(),
            Token::Indentation => "    ".len(),
            Token::SquareOpen => '['.len_utf8(),
            Token::SquareClose => ']'.len_utf8(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Plus => write!(f, "+"),
            Token::HorizontalPipe => write!(f, "-"),
            Token::VerticalPipe => write!(f, "|"),
            Token::Period => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Arrow => write!(f, "->"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Asterisk => write!(f, "*"),
            Token::C => write!(f, "c"),
            Token::N => write!(f, "n"),
            Token::Ident(i) => write!(f, "{i}"),
            Token::NewLine => write!(f, "\n"),
            Token::RightCaret => write!(f, ">"),
            Token::SingleQuote => write!(f, "'"),
            Token::Backslash => write!(f, "\\"),
            Token::Comma => write!(f, ","),
            Token::SemiColon => write!(f, ";"),
            Token::Indentation => write!(f, "    "),
            Token::SquareOpen => write!(f, "["),
            Token::SquareClose => write!(f, "]"),
        }
    }
}
