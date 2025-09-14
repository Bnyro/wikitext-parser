use crate::error::ParserErrorKind;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Display;

static HTML_TAGS: &[&str] = &[
    "nowiki",
    "math",
    "code",
    "syntaxhighlight",
    "pre",
    "gallery",
];

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Text(Cow<'a, str>),
    Equals,
    OpenBracket,
    CloseBracket,
    DoubleOpenBrace,
    DoubleCloseBrace,
    DoubleOpenBracket,
    DoubleCloseBracket,
    OpenComment,
    CloseComment,
    OpenBraceWithBar,
    CloseBraceWithBar,
    BarWithDash,
    BarWithPlus,
    Exclamation,
    DoubleExclamation,
    HtmlTagOpen(Cow<'a, str>, Cow<'a, str>),
    HtmlTagClose(Cow<'a, str>),
    VerticalBar,
    DoubleVerticalBar,
    Apostrophe,
    Colon,
    Semicolon,
    Star,
    Sharp,
    Newline,
    Eof,
}

/// A position in a text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TextPosition {
    /// One-based line number.
    pub line: usize,
    /// One-based column number.
    pub column: usize,
}

impl TextPosition {
    /// Create a new text position at the given `line` and `column`.
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Default for TextPosition {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

#[derive(Clone, Debug)]
pub struct PositionAwareStrIterator<'input> {
    input: &'input str,
    position: TextPosition,
}

impl<'input> PositionAwareStrIterator<'input> {
    pub fn new<'input_argument: 'input>(input: &'input_argument str) -> Self {
        Self {
            input,
            position: Default::default(),
        }
    }

    pub fn remaining_input(&self) -> &'input str {
        self.input
    }

    pub fn advance_until(&mut self, limit: usize) {
        let mut cumulative_advancement = 0;
        while cumulative_advancement < limit {
            cumulative_advancement += self.advance_one();
        }
        assert_eq!(cumulative_advancement, limit);
    }

    pub fn advance_one(&mut self) -> usize {
        assert!(!self.input.is_empty());
        if self.input.starts_with('\n') {
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.position.column += 1;
        }

        if let Some((offset, _)) = self.input.char_indices().nth(1) {
            self.input = &self.input[offset..];
            offset
        } else {
            let offset = self.input.len();
            self.input = &self.input[offset..];
            offset
        }
    }

    /// Returns `true` if the tokenizer has not yet been advanced.
    pub fn is_at_start(&self) -> bool {
        self.position == Default::default()
    }
}

#[derive(Debug)]
pub struct Tokenizer<'input> {
    input: PositionAwareStrIterator<'input>,
}

impl<'input> Tokenizer<'input> {
    pub fn new<'input_argument: 'input>(input: &'input_argument str) -> Self {
        Self {
            input: PositionAwareStrIterator::new(input),
        }
    }

    #[allow(unused)]
    pub fn tokenize_all(&mut self) -> Vec<Token<'input>> {
        let mut tokens = Vec::new();
        while tokens.last() != Some(&Token::Eof) {
            tokens.push(self.next());
        }
        tokens
    }

    pub fn next<'token, 'this>(&'this mut self) -> Token<'token>
    where
        'input: 'token + 'this,
    {
        let mut text_token_content = String::new();
        loop {
            let input = self.input.remaining_input();

            if input.is_empty() {
                if !text_token_content.is_empty() {
                    return Token::Text(text_token_content.into());
                }

                return Token::Eof;
            } else if let Some((token, length)) = self.special_token(input) {
                if !text_token_content.is_empty() {
                    return Token::Text(text_token_content.into());
                } else {
                    self.input.advance_until(length);
                    return token;
                }
            } else if let Some(c) = input.chars().nth(0) {
                self.input.advance_one();
                text_token_content.push(c);
            }
        }
    }

    /// Try to match a special token that's not raw text.
    ///
    /// Returns the token and its textual length, if found.
    pub fn special_token<'token, 'this>(
        &'this self,
        input: &'token str,
    ) -> Option<(Token<'token>, usize)>
    where
        'input: 'token + 'this,
    {
        if input.starts_with(r"{{") {
            Some((Token::DoubleOpenBrace, 2))
        } else if input.starts_with(r"}}") {
            Some((Token::DoubleCloseBrace, 2))
        } else if input.starts_with("[[") {
            Some((Token::DoubleOpenBracket, 2))
        } else if input.starts_with("]]") {
            Some((Token::DoubleCloseBracket, 2))
        } else if input.starts_with(r"[") {
            Some((Token::OpenBracket, 1))
        } else if input.starts_with(r"]") {
            Some((Token::CloseBracket, 1))
        } else if input.starts_with("{|") {
            Some((Token::OpenBraceWithBar, 2))
        } else if input.starts_with("|}") && !input.starts_with("|}}") {
            Some((Token::CloseBraceWithBar, 2))
        } else if input.starts_with("|-") && !input.starts_with("|-|") {
            Some((Token::BarWithDash, 2))
        } else if input.starts_with("|+") && !input.starts_with("|+|") {
            Some((Token::BarWithPlus, 2))
        } else if input.starts_with("<!--") {
            Some((Token::OpenComment, 4))
        } else if input.starts_with("-->") {
            Some((Token::CloseComment, 3))
        } else if input.starts_with("</") {
            for html_tag in HTML_TAGS {
                let full_end_tag = format!("</{html_tag}>");
                if input.starts_with(&full_end_tag) {
                    return Some((
                        Token::HtmlTagClose(html_tag.to_string().into()),
                        full_end_tag.len(),
                    ));
                }
            }

            None
        } else if input.starts_with("<") {
            for html_tag in HTML_TAGS {
                let full_start_tag = format!("<{html_tag}");
                if !input.starts_with(&full_start_tag) {
                    continue;
                }

                if let Some(end_index) = input.find(">") {
                    let attrs = &input[full_start_tag.len()..end_index];

                    // attributes must start with a blank, e.g. <code lang="c">
                    // otherwise, it's likely not the html element we're looking for
                    if !(attrs.is_empty() || attrs.starts_with(" ")) {
                        return None;
                    }

                    return Some((
                        Token::HtmlTagOpen(html_tag.to_string().into(), attrs.into()),
                        end_index + 1,
                    ));
                };

                break;
            }

            None
        } else if input.starts_with('=') {
            Some((Token::Equals, 1))
        } else if input.starts_with("!!") {
            Some((Token::DoubleExclamation, 2))
        } else if input.starts_with('!') {
            Some((Token::Exclamation, 1))
        } else if input.starts_with("||") {
            Some((Token::DoubleVerticalBar, 2))
        } else if input.starts_with('|') {
            Some((Token::VerticalBar, 1))
        } else if input.starts_with('\'') {
            Some((Token::Apostrophe, 1))
        } else if input.starts_with('\n') {
            Some((Token::Newline, 1))
        } else if input.starts_with(':') {
            Some((Token::Colon, 1))
        } else if input.starts_with(';') {
            Some((Token::Semicolon, 1))
        } else if input.starts_with('*') {
            Some((Token::Star, 1))
        } else if input.starts_with('#') {
            Some((Token::Sharp, 1))
        } else {
            None
        }
    }

    /// Returns `true` if the tokenizer has not yet been advanced.
    #[allow(unused)]
    pub fn is_at_start(&self) -> bool {
        self.input.is_at_start()
    }
}

#[derive(Debug)]
pub struct MultipeekTokenizer<'tokenizer> {
    tokenizer: Tokenizer<'tokenizer>,
    peek: VecDeque<(Token<'tokenizer>, TextPosition)>,
    next_was_called: bool,
}

impl<'tokenizer> MultipeekTokenizer<'tokenizer> {
    pub fn new(tokenizer: Tokenizer<'tokenizer>) -> Self {
        Self {
            tokenizer,
            peek: VecDeque::new(),
            next_was_called: false,
        }
    }

    pub fn next<'token>(&mut self) -> (Token<'token>, TextPosition)
    where
        'tokenizer: 'token,
    {
        self.next_was_called = true;
        if let Some((token, text_position)) = self.peek.pop_front() {
            (token, text_position)
        } else {
            let text_position = self.tokenizer.input.position;
            (self.tokenizer.next(), text_position)
        }
    }

    pub fn peek(&mut self, distance: usize) -> &(Token<'_>, TextPosition) {
        while self.peek.len() < distance + 1 {
            let text_position = self.tokenizer.input.position;
            self.peek.push_back((self.tokenizer.next(), text_position));
        }
        &self.peek[distance]
    }

    /// Peeks a position inside the current peek buffer.
    /// If the position and no position after it was not yet peeked, returns `None`.
    /// This is useful because it does not require a mutable reference to self.
    pub fn repeek(&self, distance: usize) -> Option<&(Token<'_>, TextPosition)> {
        self.peek.get(distance)
    }

    pub fn expect(&mut self, token: &Token) -> crate::error::Result<()> {
        let (next, text_position) = self.next();
        if &next == token {
            Ok(())
        } else {
            Err(ParserErrorKind::UnexpectedToken {
                expected: token.to_string(),
                actual: next.to_string(),
            }
            .into_parser_error(text_position))
        }
    }

    /// Returns `true` if the tokenizer has not yet been advanced.
    #[allow(unused)]
    pub fn is_at_start(&self) -> bool {
        !self.next_was_called
    }
}

impl Display for Token<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", self.to_str())
    }
}

impl<'token> Token<'token> {
    pub fn to_str(&self) -> Cow<'token, str> {
        match self {
            Token::Text(text) => text.to_string().into(),
            Token::Equals => "=".into(),
            Token::DoubleOpenBrace => "{{".into(),
            Token::DoubleCloseBrace => "}}".into(),
            Token::DoubleOpenBracket => "[[".into(),
            Token::DoubleCloseBracket => "]]".into(),
            Token::OpenBraceWithBar => "{|".into(),
            Token::CloseBraceWithBar => "|}".into(),
            Token::BarWithDash => "|-".into(),
            Token::BarWithPlus => "|+".into(),
            Token::Exclamation => "!".into(),
            Token::DoubleExclamation => "!!".into(),
            Token::HtmlTagOpen(tag, attrs) => format!("<{tag} {attrs}>").into(),
            Token::HtmlTagClose(tag) => format!("<{tag}/>").into(),
            Token::VerticalBar => "|".into(),
            Token::DoubleVerticalBar => "||".into(),
            Token::Apostrophe => "'".into(),
            Token::Newline => "\n".into(),
            Token::Colon => ":".into(),
            Token::Semicolon => ";".into(),
            Token::Star => "*".into(),
            Token::Sharp => "#".into(),
            Token::Eof => "<EOF>".into(),
            Token::OpenBracket => "[".into(),
            Token::CloseBracket => "]".into(),
            Token::OpenComment => "<!--".into(),
            Token::CloseComment => "-->".into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Token, Tokenizer};

    #[test]
    fn simple() {
        let input = "{{==a=  v}} }} } edf } } [ {";
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize_all();
        assert_eq!(
            tokens.as_slice(),
            [
                Token::DoubleOpenBrace,
                Token::Equals,
                Token::Equals,
                Token::Text("a".into()),
                Token::Equals,
                Token::Text("  v".into()),
                Token::DoubleCloseBrace,
                Token::Text(" ".into()),
                Token::DoubleCloseBrace,
                Token::Text(" } edf } } ".into()),
                Token::OpenBracket,
                Token::Text(" {".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn tokenize_math() {
        let input = "Hello <math display=block>\\a_3 * 7 + a^4</math> World";
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize_all();
        assert_eq!(
            tokens.as_slice(),
            [
                Token::Text("Hello ".into()),
                Token::HtmlTagOpen("math".into(), " display=block".into()),
                Token::Text("\\a_3 ".into()),
                Token::Star,
                Token::Text(" 7 + a^4".into()),
                Token::HtmlTagClose("math".into()),
                Token::Text(" World".into()),
                Token::Eof
            ]
        );
    }
}
