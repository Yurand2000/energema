use super::*;

impl<'a> std::fmt::Display for Stream<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.iter_elements();
        if let Some(first) = iter.next() {
            writeln!(f, "Line {}:{}", first.get_line(), first.get_column())?;
            first.display_token(f)?;
        }

        for item in iter {
            item.display_token(f)?;
        }

        Ok(())
    }
}

impl LocatedToken {
    fn display_token(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_token())
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(value) => write!(f, " {}", value),
            Token::Keyword(value) => write!(f, " {}", value),
            Token::Symbol(value) => write!(f, "{}", value),
            Token::UnitLiteral => write!(f, " {}", "unit"),
            Token::BoolLiteral(value) => write!(f, " {}", value),
            Token::I32Literal(value) => write!(f, " {}", value),
            Token::RuneLiteral(value) => write!(f, "\'{}\'", value),
            Token::StringLiteral(value) => write!(f, "\"{}\"", value),
            Token::Eof => Ok(()),
        }
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::While => write!(f, "while"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Let => write!(f, "let"),
            Keyword::Perform => write!(f, "perform"),
            Keyword::With => write!(f, "with"),
            Keyword::Handler => write!(f, "handler"),
            Keyword::Effect => write!(f, "effect"),
            Keyword::Return => write!(f, "return"),
        }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::OpenParenthesis => write!(f, "( "),
            Symbol::CloseParentesis => write!(f, " )"),
            Symbol::OpenBracket => write!(f, "[ "),
            Symbol::CloseBracket => write!(f, " ]"),
            Symbol::OpenBrace => write!(f, "{{ "),
            Symbol::CloseBrace => write!(f, " }}"),
            Symbol::Comma => write!(f, ", "),
            Symbol::Colon => write!(f, ": "),
            Symbol::Semicolon => write!(f, "; "),
            Symbol::Tilde => write!(f, "~"),
            Symbol::Arrow => write!(f, " ->"),
            Symbol::DoubleArrow => write!(f, " =>"),
            Symbol::NegatedSet => write!(f, "^"),
            Symbol::Exclamation => write!(f, "!"),
            Symbol::Plus => write!(f, "+"),
            Symbol::Minus => write!(f, "-"),
            Symbol::Times => write!(f, "*"),
            Symbol::ForwardSlash => write!(f, "/"),
            Symbol::Modulo => write!(f, "%"),
            Symbol::LogicalAnd => write!(f, " && "),
            Symbol::LogicalOr => write!(f, " || "),
            Symbol::LogicalXor => write!(f, "^^"),
            Symbol::Pipe => write!(f, "|"),
            Symbol::Equal => write!(f, "="),
            Symbol::DoubleEqual => write!(f, "=="),
            Symbol::NotEqual => write!(f, "!="),
            Symbol::GreaterThan => write!(f, ">"),
            Symbol::GreaterOrEqual => write!(f, ">="),
            Symbol::SmallerThan => write!(f, "<"),
            Symbol::SmallerOrEqual => write!(f, "<="),
        }
    }
}