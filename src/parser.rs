use crate::error::ParserErrorKind;
use crate::level_stack::LevelStack;
use crate::tokenizer::{MultipeekTokenizer, Token, Tokenizer};
use crate::wikitext::{
    Attribute, Headline, Line, TableCell, Text, TextFormatting, TextPiece, Wikitext,
};
use crate::ParserError;
use lazy_static::lazy_static;
use log::debug;
use regex::Regex;
use std::collections::HashMap;
use std::mem;

pub const MAX_SECTION_DEPTH: usize = 6;

#[cfg(not(test))]
static DO_PARSER_DEBUG_PRINTS: bool = false;
#[cfg(test)]
static DO_PARSER_DEBUG_PRINTS: bool = true;

lazy_static! {
    static ref HTML_ATTR_REGEX: Regex = Regex::new(r#"(\w+)=(".*"|.*)"#).unwrap();
}

/// Parse textual wikitext into a semantic representation.
pub fn parse_wikitext(
    wikitext: &str,
    headline: String,
    mut error_consumer: impl FnMut(ParserError),
) -> Wikitext {
    let mut level_stack = LevelStack::new(headline);
    let mut tokenizer = MultipeekTokenizer::new(Tokenizer::new(wikitext));

    loop {
        tokenizer.peek(1);
        if DO_PARSER_DEBUG_PRINTS {
            println!(
                "parse_wikitext tokens: {:?} {:?}",
                tokenizer.repeek(0),
                tokenizer.repeek(1),
            );
        }

        if tokenizer.repeek(0).unwrap().0 == Token::Newline
            && tokenizer.repeek(1).unwrap().0 == Token::Newline
        {
            level_stack.new_paragraph();
            tokenizer.next();
            continue;
        }

        let (token, pos) = tokenizer.peek(0);

        if token == &Token::Equals {
            if let Some(headline) = parse_potential_headline(&mut tokenizer, &mut error_consumer) {
                level_stack.append_headline(headline);
                continue;
            }
        } else if token == &Token::Eof {
            break;
        }

        level_stack.append_line(parse_line(&mut tokenizer, &mut error_consumer));
    }

    Wikitext {
        root_section: level_stack.into_root_section(),
    }
}

fn parse_line(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Line {
    debug_assert_eq!(parse_potential_headline(tokenizer, error_consumer), None);

    let mut list_prefix = String::new();

    // parse list_prefix
    while let token @ (Token::Colon | Token::Semicolon | Token::Star | Token::Sharp) =
        &tokenizer.peek(0).0
    {
        list_prefix.push_str(&token.to_str());
        tokenizer.next();
    }

    // parse remaining text
    if tokenizer.peek(0).0 == Token::OpenBraceWithBar {
        parse_table(tokenizer, error_consumer)
    } else if !list_prefix.is_empty() {
        let mut text_formatting = TextFormatting::Normal;
        let text = parse_text_until(
            tokenizer,
            error_consumer,
            Text::new(),
            &mut text_formatting,
            &|token: &Token<'_>| {
                matches!(token, Token::Newline | Token::Eof | Token::OpenBraceWithBar)
            },
        );
        let (_, text_position) = tokenizer.next();
        if text_formatting != TextFormatting::Normal {
            debug!("Line contains unclosed text formatting expression at {text_position:?}");
        }
        Line::List { list_prefix, text }
    } else {
        let mut text_formatting = TextFormatting::Normal;
        let text = parse_text_until(
            tokenizer,
            error_consumer,
            Text::new(),
            &mut text_formatting,
            &|token| matches!(token, Token::Newline | Token::Eof | Token::OpenBraceWithBar),
        );
        let (_, text_position) = tokenizer.next();
        if text_formatting != TextFormatting::Normal {
            debug!("Line contains unclosed text formatting expression at {text_position:?}");
        }
        Line::Normal { text }
    }
}

#[derive(Debug, Clone, Copy)]
enum TableRowType {
    Header,
    Content,
}

fn collect_table_cell(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Option<TableCell> {
    let mut cell = TableCell::default();

    loop {
        let text = parse_text_until(
            tokenizer,
            error_consumer,
            Text::new(),
            &mut TextFormatting::Normal,
            &|token: &Token<'_>| {
                matches!(
                    token,
                    Token::VerticalBar
                        | Token::Exclamation
                        | Token::DoubleVerticalBar
                        | Token::DoubleExclamation
                        | Token::CloseBraceWithBar
                        | Token::BarWithDash
                )
            },
        );

        if tokenizer.peek(0).0 == Token::VerticalBar {
            if let Some(html_attr) = HTML_ATTR_REGEX.captures(text.to_string().trim()) {
                tokenizer.next();

                if let (Some(key), Some(value)) = (html_attr.get(1), html_attr.get(2)) {
                    let Ok(value) = value.as_str().replace('"', "").parse::<i32>() else {
                        continue;
                    };
                    if key.as_str().eq_ignore_ascii_case("rowspan") {
                        cell.rowspan = value;
                    } else if key.as_str().eq_ignore_ascii_case("colspan") {
                        cell.colspan = value;
                    }
                }

                continue;
            }
        }

        for piece in text.pieces {
            cell.text.pieces.push(piece);
        }
        break;
    }

    cell.text.trim_self();
    Some(cell)
}

fn parse_table(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Line {
    if DO_PARSER_DEBUG_PRINTS {
        println!("parse_table token: {:?}", tokenizer.peek(0));
    }

    // remove {| from start
    tokenizer.next();

    let mut header_rows = vec![];
    let mut content_rows = vec![];

    let mut current_type = TableRowType::Content;
    let mut current_row: Vec<TableCell> = vec![];

    loop {
        let (current_token, current_pos) = tokenizer.next();

        match current_token {
            Token::BarWithPlus => {
                // seek to end of comment/caption
                while tokenizer.peek(0).0 != Token::BarWithDash {
                    tokenizer.next();
                }
            }
            Token::BarWithDash => {
                if !current_row.is_empty() {
                    match current_type {
                        TableRowType::Header => {
                            header_rows.push(current_row);
                        }
                        TableRowType::Content => {
                            content_rows.push(current_row);
                        }
                    }
                }

                current_type = TableRowType::Content;
                current_row = vec![];
            }
            Token::Exclamation | Token::DoubleExclamation => {
                current_type = TableRowType::Header;

                if let Some(cell) = collect_table_cell(tokenizer, error_consumer) {
                    current_row.push(cell);
                }
            }
            Token::VerticalBar | Token::DoubleVerticalBar => {
                current_type = TableRowType::Content;

                if let Some(cell) = collect_table_cell(tokenizer, error_consumer) {
                    current_row.push(cell);
                }
            }
            Token::CloseBraceWithBar => {
                if !current_row.is_empty() {
                    match current_type {
                        TableRowType::Header => {
                            header_rows.push(current_row);
                        }
                        TableRowType::Content => {
                            content_rows.push(current_row);
                        }
                    }
                }

                // end of table
                break;
            }
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedCloseBraceWithBar.into_parser_error(current_pos),
                );
            }
            _token => {}
        }
    }

    Line::Table {
        header_rows,
        content_rows,
    }
}

fn get_html_attrs(attr_text: &str) -> HashMap<String, String> {
    let mut attrs = HashMap::new();
    for html_attr in HTML_ATTR_REGEX.captures_iter(attr_text) {
        if let (Some(key), Some(value)) = (html_attr.get(1), html_attr.get(2)) {
            attrs.insert(
                key.as_str().to_string(),
                // remove surrounding '"' for captures like key="value"
                value.as_str().trim_matches('"').to_string(),
            );
        }
    }

    attrs
}

fn parse_text_until(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    mut prefix: Text,
    text_formatting: &mut TextFormatting,
    terminator: &impl Fn(&Token<'_>) -> bool,
) -> Text {
    loop {
        if DO_PARSER_DEBUG_PRINTS {
            println!("parse_text_until token: {:?}", tokenizer.peek(0));
        }
        let (token, text_position) = tokenizer.peek(0);
        if terminator(token) {
            break;
        }

        match token {
            token @ (Token::Text(_)
            | Token::Equals
            | Token::Colon
            | Token::Semicolon
            | Token::Star
            | Token::Sharp
            | Token::Newline
            | Token::OpenBraceWithBar
            | Token::CloseBraceWithBar
            | Token::BarWithDash
            | Token::BarWithPlus
            | Token::Exclamation
            | Token::DoubleExclamation
            | Token::VerticalBar
            | Token::DoubleVerticalBar) => {
                prefix.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::DoubleOpenBrace => prefix.pieces.push(parse_double_brace_expression(
                tokenizer,
                error_consumer,
                text_formatting,
            )),
            Token::DoubleOpenBracket => {
                prefix = parse_internal_link(tokenizer, error_consumer, prefix, text_formatting);
            }
            Token::OpenBracket => {
                prefix = parse_external_link(tokenizer, error_consumer, prefix, text_formatting);
            }
            Token::HtmlTagOpen(tag, attrs) => {
                let tag = tag.to_string();
                let attrs = get_html_attrs(attrs);

                let mut text = parse_raw_block(
                    tokenizer,
                    error_consumer,
                    Text::new(),
                    text_formatting,
                    &Token::HtmlTagClose(tag.clone().into()),
                );

                match tag.as_str() {
                    "nowiki" | "pre" => {
                        prefix.extend_with_text(text);
                    }
                    "math" => {
                        text.trim_self();

                        let block = attrs.get("display") == Some(&"block".to_string());
                        let text_piece = TextPiece::Math {
                            block,
                            text: text.to_string(),
                        };
                        prefix.pieces.push(text_piece);
                    }
                    "code" | "syntaxhighlight" => {
                        text.trim_self();

                        let language = attrs.get("language").or_else(|| attrs.get("lang")).cloned();
                        let text_piece = TextPiece::Code {
                            language,
                            text: text.to_string(),
                        };
                        prefix.pieces.push(text_piece);
                    }
                    _ => unreachable!("html tag not implemented by parser"),
                }
            }
            Token::DoubleCloseBrace => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleCloseBrace.into_parser_error(*text_position),
                );
                prefix.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::DoubleCloseBracket => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleCloseBracket.into_parser_error(*text_position),
                );
                prefix.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::CloseBracket => {
                error_consumer(
                    ParserErrorKind::UnmatchedCloseBracket.into_parser_error(*text_position),
                );
                prefix.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::HtmlTagClose(_tag) => {
                error_consumer(
                    ParserErrorKind::UnmatchedHtmlBlockClose.into_parser_error(*text_position),
                );
                prefix.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::Apostrophe => {
                tokenizer.peek(4);
                let apostrophe_prefix_length = (0..5)
                    .take_while(|i| tokenizer.peek(*i).0 == Token::Apostrophe)
                    .count();
                if apostrophe_prefix_length == 1 {
                    prefix.extend_with_formatted_text(*text_formatting, "'");
                    tokenizer.next();
                } else {
                    let apostrophe_prefix_length = if apostrophe_prefix_length == 4 {
                        3
                    } else {
                        apostrophe_prefix_length
                    };
                    *text_formatting = text_formatting.next_formatting(apostrophe_prefix_length);
                    for _ in 0..apostrophe_prefix_length {
                        tokenizer.next();
                    }
                }
            }
            Token::Eof => {
                error_consumer(ParserErrorKind::UnexpectedEof.into_parser_error(*text_position));
                break;
            }
        }
    }

    prefix
}

fn parse_raw_block(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    mut text: Text,
    text_formatting: &TextFormatting,
    end_token: &Token,
) -> Text {
    tokenizer.next();

    loop {
        if DO_PARSER_DEBUG_PRINTS {
            println!("parse_nowiki token: {:?}", tokenizer.peek(0));
        }
        let (token, text_position) = tokenizer.peek(0);

        if token == end_token {
            tokenizer.next();
            break;
        }

        match token {
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedHtmlBlockOpen.into_parser_error(*text_position),
                );
                break;
            }
            token => {
                text.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
        }
    }

    text
}

fn parse_potential_headline(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Option<Headline> {
    if DO_PARSER_DEBUG_PRINTS {
        tokenizer.peek(2 * MAX_SECTION_DEPTH + 2);
        println!(
            "parse_potential_headline initial tokens: {:?}",
            (0..2 * MAX_SECTION_DEPTH + 3)
                .map(|i| tokenizer.repeek(i))
                .collect::<Vec<_>>()
        );
    }

    let text_position = tokenizer.peek(0).1;
    let prefix_length = (0..MAX_SECTION_DEPTH)
        .take_while(|i| tokenizer.peek(*i).0 == Token::Equals)
        .count();
    if prefix_length == 0 {
        return None;
    }

    let mut label = String::new();
    let mut text_limit = prefix_length;
    loop {
        let (token, _) = tokenizer.peek(text_limit);
        if DO_PARSER_DEBUG_PRINTS {
            println!("parse_potential_headline label token: {:?}", token);
        }

        match token {
            Token::Newline | Token::Eof | Token::Equals => break,
            token => {
                label.push_str(&token.to_str());
            }
        }

        text_limit += 1;
    }

    tokenizer.peek(text_limit + prefix_length + 1);
    let suffix_length = ((text_limit)..=(text_limit + prefix_length + 1))
        .take_while(|i| tokenizer.repeek(*i).unwrap().0 == Token::Equals)
        .count();

    if prefix_length == suffix_length {
        let whitespace_after_headline =
            match &tokenizer.repeek(text_limit + suffix_length).unwrap().0 {
                Token::Text(text) => {
                    debug_assert!(text.chars().all(|c| c != '\n'));
                    if text.chars().all(|c| c.is_ascii_whitespace()) {
                        if matches!(
                            tokenizer.repeek(text_limit + suffix_length + 1).unwrap().0,
                            Token::Newline | Token::Eof
                        ) {
                            Some(2)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Token::Newline | Token::Eof => Some(1),
                _ => None,
            };

        if let Some(whitespace_after_headline) = whitespace_after_headline {
            let label = label.trim().to_string();
            for _ in 0..text_limit + suffix_length + whitespace_after_headline {
                tokenizer.next();
            }

            if prefix_length == 1 {
                error_consumer(
                    ParserErrorKind::SecondRootSection {
                        label: label.clone(),
                    }
                    .into_parser_error(text_position),
                );
            }

            Some(Headline {
                label,
                level: prefix_length.try_into().unwrap(),
            })
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_double_brace_expression(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    text_formatting: &mut TextFormatting,
) -> TextPiece {
    tokenizer.expect(&Token::DoubleOpenBrace).unwrap();
    if DO_PARSER_DEBUG_PRINTS {
        println!(
            "parse_double_brace_expression initial token: {:?}",
            tokenizer.peek(0)
        );
    }
    let tag = parse_tag(tokenizer, error_consumer);
    let mut attributes = Vec::new();

    // parse attributes
    loop {
        if DO_PARSER_DEBUG_PRINTS {
            println!(
                "parse_double_brace_expression token: {:?}",
                tokenizer.peek(0)
            );
        }
        let (token, text_position) = tokenizer.peek(0);
        match token {
            Token::VerticalBar => {
                attributes.push(parse_attribute(tokenizer, error_consumer, text_formatting))
            }
            Token::DoubleCloseBrace => {
                tokenizer.next();
                break;
            }
            token @ (Token::Text(_)
            | Token::Equals
            | Token::DoubleOpenBrace
            | Token::DoubleOpenBracket
            | Token::DoubleCloseBracket
            | Token::OpenBracket
            | Token::CloseBracket
            | Token::HtmlTagOpen(_, _)
            | Token::HtmlTagClose(_)
            | Token::Apostrophe
            | Token::Newline
            | Token::Colon
            | Token::Semicolon
            | Token::Star
            | Token::OpenBraceWithBar
            | Token::CloseBraceWithBar
            | Token::DoubleVerticalBar
            | Token::BarWithDash
            | Token::BarWithPlus
            | Token::Exclamation
            | Token::DoubleExclamation
            | Token::Sharp) => {
                error_consumer(
                    ParserErrorKind::UnexpectedToken {
                        expected: "| or }}".to_string(),
                        actual: token.to_string(),
                    }
                    .into_parser_error(*text_position),
                );
                tokenizer.next();
            }
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBrace.into_parser_error(*text_position),
                );
                break;
            }
        }
    }

    TextPiece::DoubleBraceExpression { tag, attributes }
}

fn parse_tag(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Text {
    if DO_PARSER_DEBUG_PRINTS {
        println!("parse_tag initial token: {:?}", tokenizer.peek(0));
    }
    let text_position = tokenizer.peek(0).1;
    let mut text_formatting = TextFormatting::Normal;
    let mut tag = Text::new();

    loop {
        tag = parse_text_until(
            tokenizer,
            error_consumer,
            tag,
            &mut text_formatting,
            &|token: &Token<'_>| {
                matches!(
                    token,
                    Token::DoubleCloseBrace
                        | Token::VerticalBar
                        | Token::DoubleOpenBracket
                        | Token::Eof
                )
            },
        );
        let (token, text_position) = tokenizer.peek(0);
        match token {
            Token::DoubleCloseBrace | Token::VerticalBar => break,
            token @ Token::DoubleOpenBracket => {
                error_consumer(
                    ParserErrorKind::UnexpectedTokenInTag {
                        token: token.to_string(),
                    }
                    .into_parser_error(*text_position),
                );
                tag.extend_with_formatted_text(text_formatting, &token.to_str());
                tokenizer.next();
            }
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBrace.into_parser_error(*text_position),
                );
                break;
            }
            token => unreachable!("Not a stop token above: {token:?}"),
        }
    }

    if text_formatting != TextFormatting::Normal {
        error_consumer(
            ParserErrorKind::UnclosedTextFormatting {
                formatting: text_formatting,
            }
            .into_parser_error(text_position),
        );
    }

    tag.trim_self();
    tag
}

fn parse_attribute(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    text_formatting: &mut TextFormatting,
) -> Attribute {
    tokenizer.expect(&Token::VerticalBar).unwrap();
    let mut name = Some(String::new());
    let mut value = Text::new();

    // parse name
    loop {
        if DO_PARSER_DEBUG_PRINTS {
            println!("parse_attribute name token: {:?}", tokenizer.peek(0));
        }
        let (token, text_position) = tokenizer.peek(0);
        match token {
            Token::Text(text) => {
                name.as_mut().unwrap().push_str(text);
                tokenizer.next();
            }
            Token::Newline => {
                name.as_mut().unwrap().push('\n');
                tokenizer.next();
            }
            Token::Equals => {
                tokenizer.next();
                break;
            }
            Token::DoubleOpenBrace
            | Token::DoubleOpenBracket
            | Token::HtmlTagOpen(_, _)
            | Token::HtmlTagClose(_)
            | Token::DoubleCloseBrace
            | Token::OpenBracket
            | Token::CloseBracket
            | Token::VerticalBar
            | Token::DoubleVerticalBar
            | Token::Apostrophe
            | Token::Colon
            | Token::Semicolon
            | Token::OpenBraceWithBar
            | Token::CloseBraceWithBar
            | Token::BarWithDash
            | Token::BarWithPlus
            | Token::Exclamation
            | Token::DoubleExclamation
            | Token::Star
            | Token::Sharp => {
                value.pieces.push(TextPiece::Text {
                    text: name.take().unwrap(),
                    formatting: *text_formatting,
                });
                break;
            }
            token @ Token::DoubleCloseBracket => {
                error_consumer(
                    ParserErrorKind::UnexpectedTokenInParameter {
                        token: token.to_string(),
                    }
                    .into_parser_error(*text_position),
                );
                name.as_mut().unwrap().push_str(&token.to_str());
                tokenizer.next();
            }
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBrace.into_parser_error(*text_position),
                );
                break;
            }
        }
    }

    // parse value
    let mut value = parse_text_until(
        tokenizer,
        error_consumer,
        value,
        text_formatting,
        &|token: &Token<'_>| matches!(token, Token::VerticalBar | Token::DoubleCloseBrace),
    );

    // whitespace is stripped from named attribute names and values, but not from unnamed attributes
    if let Some(name) = &mut name {
        *name = name.trim().to_string();
        value.trim_self();
    }

    Attribute { name, value }
}

fn parse_internal_link(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    mut text: Text,
    text_formatting: &mut TextFormatting,
) -> Text {
    tokenizer.expect(&Token::DoubleOpenBracket).unwrap();
    let surrounding_depth = if tokenizer.peek(0).0 == Token::DoubleOpenBracket {
        tokenizer.next();
        1
    } else {
        0
    };
    let mut target = Text::new();
    let mut options = Vec::new();
    let mut label = None;

    // parse target
    target = parse_text_until(
        tokenizer,
        error_consumer,
        target,
        text_formatting,
        &|token: &Token<'_>| {
            matches!(
                token,
                Token::DoubleCloseBracket
                    | Token::VerticalBar
                    | Token::DoubleCloseBrace
                    | Token::DoubleOpenBracket
                    | Token::OpenBracket
                    | Token::OpenBraceWithBar
                    | Token::Newline
                    | Token::Eof
            )
        },
    );
    if DO_PARSER_DEBUG_PRINTS {
        println!("parse_link target token: {:?}", tokenizer.peek(0));
    }
    let (token, text_position) = tokenizer.peek(0);
    match token {
        token @ (Token::Text(_)
        | Token::Colon
        | Token::Sharp
        | Token::Semicolon
        | Token::Star
        | Token::Apostrophe
        | Token::Equals
        | Token::DoubleOpenBrace
        | Token::OpenBraceWithBar
        | Token::CloseBraceWithBar
        | Token::CloseBracket
        | Token::HtmlTagOpen(_, _)
        | Token::HtmlTagClose(_)) => {
            unreachable!("Not a stop token above: {token:?}");
        }
        Token::DoubleCloseBracket
        | Token::BarWithDash
        | Token::BarWithPlus
        | Token::Exclamation
        | Token::DoubleExclamation => {
            tokenizer.next();
        }
        Token::VerticalBar | Token::DoubleVerticalBar => {
            tokenizer.next();
            label = Some(Text::new());
        }
        token @ (Token::Newline | Token::Eof) => {
            error_consumer(
                ParserErrorKind::UnmatchedDoubleOpenBracket.into_parser_error(*text_position),
            );
            if token != &Token::Eof {
                text.extend_with_formatted_text(*text_formatting, &token.to_str());
            }
            tokenizer.next();
        }
        token @ (Token::DoubleCloseBrace | Token::DoubleOpenBracket | Token::OpenBracket) => {
            error_consumer(
                ParserErrorKind::UnexpectedTokenInLink {
                    token: token.to_string(),
                }
                .into_parser_error(*text_position),
            );
            text.extend_with_formatted_text(*text_formatting, &token.to_str());
            tokenizer.next();
        }
    }

    // parse options and label
    let label = label.map(|mut label| {
        let mut link_finished = false;

        // parse options
        loop {
            if DO_PARSER_DEBUG_PRINTS {
                println!("parse_link options token: {:?}", tokenizer.peek(0));
            }
            let (token, text_position) = tokenizer.peek(0);
            match token {
                token @ (Token::Equals | Token::Text(_)) => {
                    label.extend_with_formatted_text(*text_formatting, &token.to_str());
                    tokenizer.next();
                }
                Token::VerticalBar | Token::DoubleVerticalBar => {
                    let mut new_label = Text::new();
                    mem::swap(&mut label, &mut new_label);
                    if new_label.pieces.is_empty() {
                        options.push(Default::default());
                    } else {
                        options.push(new_label);
                    }
                    tokenizer.next();
                }
                Token::DoubleCloseBracket => {
                    tokenizer.next();
                    link_finished = true;
                    break;
                }
                Token::Apostrophe => {
                    label = parse_text_until(
                        tokenizer,
                        error_consumer,
                        label,
                        text_formatting,
                        &|token| !matches!(token, Token::Apostrophe),
                    );
                }
                Token::DoubleOpenBrace
                | Token::DoubleOpenBracket
                | Token::OpenBracket
                | Token::HtmlTagOpen(_, _)
                | Token::HtmlTagClose(_)
                | Token::Colon
                | Token::Semicolon
                | Token::Star
                | Token::Sharp
                | Token::OpenBraceWithBar
                | Token::BarWithDash
                | Token::BarWithPlus
                | Token::Exclamation
                | Token::DoubleExclamation
                | Token::Newline => {
                    break;
                }
                token @ (Token::DoubleCloseBrace
                | Token::CloseBracket
                | Token::CloseBraceWithBar) => {
                    error_consumer(
                        ParserErrorKind::UnexpectedTokenInLinkLabel {
                            token: token.to_string(),
                        }
                        .into_parser_error(*text_position),
                    );
                    label.extend_with_formatted_text(*text_formatting, &token.to_str());
                    tokenizer.next();
                }
                Token::Eof => {
                    error_consumer(
                        ParserErrorKind::UnmatchedDoubleOpenBracket
                            .into_parser_error(*text_position),
                    );
                    break;
                }
            }
        }

        if !link_finished {
            // parse label
            loop {
                label = parse_text_until(
                    tokenizer,
                    error_consumer,
                    label,
                    text_formatting,
                    &|token: &Token<'_>| {
                        matches!(
                            token,
                            Token::DoubleCloseBracket
                                | Token::VerticalBar
                                | Token::Newline
                                | Token::Eof
                        )
                    },
                );

                let (token, text_position) = tokenizer.peek(0);
                match token {
                    Token::DoubleCloseBracket => {
                        tokenizer.next();
                        break;
                    }
                    token @ Token::VerticalBar => {
                        error_consumer(
                            ParserErrorKind::UnexpectedTokenInLinkLabel {
                                token: token.to_string(),
                            }
                            .into_parser_error(*text_position),
                        );
                        label.extend_with_formatted_text(*text_formatting, &token.to_str());
                        tokenizer.next();
                    }
                    Token::Newline | Token::Eof => {
                        error_consumer(
                            ParserErrorKind::UnmatchedDoubleOpenBracket
                                .into_parser_error(*text_position),
                        );
                        tokenizer.next();
                        break;
                    }
                    token => unreachable!("Not a stop token above: {token:?}"),
                }
            }

            label
        } else {
            label
        }
    });

    // update text
    for _ in 0..surrounding_depth {
        text.extend_with_formatted_text(*text_formatting, "[[");
    }
    text.pieces.push(TextPiece::InternalLink {
        target,
        options,
        label,
    });
    for _ in 0..surrounding_depth {
        let (token, text_position) = tokenizer.peek(0);
        match token {
            token @ Token::DoubleCloseBracket => {
                text.extend_with_formatted_text(*text_formatting, &token.to_str());
                tokenizer.next();
            }
            _ => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBracket.into_parser_error(*text_position),
                );
            }
        }
    }

    text
}

fn parse_external_link(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    mut text: Text,
    text_formatting: &mut TextFormatting,
) -> Text {
    tokenizer.expect(&Token::OpenBracket).unwrap();

    let link_with_title = parse_text_until(
        tokenizer,
        error_consumer,
        Text::new(),
        text_formatting,
        &|token: &Token<'_>| matches!(token, Token::Eof | Token::CloseBracket),
    );
    let (end_token, text_position) = tokenizer.next();
    match end_token {
        Token::Eof => {
            error_consumer(ParserErrorKind::UnmatchedOpenBracket.into_parser_error(text_position));
        }
        Token::CloseBracket => {}
        _ => unreachable!(),
    }

    let mut is_in_link = true;
    let mut link = Text::new();
    let mut label = Text::new();
    for piece in link_with_title.pieces {
        // anything remaining is the link label
        if !is_in_link {
            label.pieces.push(piece);
            continue;
        }

        // find the full url, e.g. the first part of [https://example.com label]
        if let Some((url_part, text_part)) = piece.to_string().split_once(" ") {
            is_in_link = false;

            link.pieces.push(TextPiece::Text {
                formatting: *text_formatting,
                text: url_part.to_string(),
            });

            label.pieces.push(TextPiece::Text {
                formatting: *text_formatting,
                text: text_part.to_string(),
            });
        } else {
            link.pieces.push(piece);
        }
    }

    text.pieces.push(TextPiece::ExternalLink {
        target: link,
        label: if !label.is_empty() { Some(label) } else { None },
    });

    text
}

#[test]
fn test_css_attr_regex() {
    assert!(HTML_ATTR_REGEX.is_match(r#"align="center""#))
}
