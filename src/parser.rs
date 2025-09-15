use crate::error::ParserErrorKind;
use crate::level_stack::LevelStack;
use crate::tokenizer::{MultipeekTokenizer, Token, Tokenizer};
use crate::wikitext::{
    Attribute, GalleryEntry, Headline, Line, ListHead, ListItem, ListType, TableCell, Text,
    TextFormatting, TextPiece, Wikitext,
};
use crate::ParserError;
use lazy_static::lazy_static;
use log::debug;
use regex::Regex;
use std::collections::HashMap;

pub const MAX_SECTION_DEPTH: usize = 6;

#[cfg(not(test))]
static DO_PARSER_DEBUG_PRINTS: bool = false;
#[cfg(test)]
static DO_PARSER_DEBUG_PRINTS: bool = true;

lazy_static! {
    static ref HTML_ATTR_REGEX: Regex = Regex::new(r#"([\w-]+)=(".*"|.*)"#).unwrap();
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

        let (token, _pos) = tokenizer.peek(0);

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

    // parse remaining text
    let token = tokenizer.peek(0).0.clone();
    if let Token::HtmlTagOpen(tag, attrs) = token {
        let attrs = attrs.to_string();
        if tag == "gallery" {
            return parse_gallery(tokenizer, error_consumer, &attrs);
        }
    }

    if tokenizer.peek(0).0 == Token::OpenBraceWithBar {
        parse_table(tokenizer, error_consumer)
    } else if matches!(
        tokenizer.peek(0).0,
        Token::Colon | Token::Semicolon | Token::Star | Token::Sharp
    ) {
        parse_list(tokenizer, error_consumer)
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

fn parse_table_cell(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Option<TableCell> {
    let mut cell = TableCell::default();

    'outer: loop {
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
                        | Token::Eof
                )
            },
        );

        // if a html attr is found: continue with the outer loop, e.g. rowspan=2|content
        // if no html attr is found: skip the closure and pus
        if tokenizer.peek(0).0 == Token::VerticalBar {
            'parse_html_attr: {
                let text = text.to_string();
                let Some(html_attr) = HTML_ATTR_REGEX.captures(text.trim()) else {
                    break 'parse_html_attr;
                };
                let (Some(key), Some(value)) = (html_attr.get(1), html_attr.get(2)) else {
                    break 'parse_html_attr;
                };

                // html attrs have to be at the start of the table expression, otherwise it's likely
                // real content containing a html tag (which we don't want to match here)
                if key.start() > 2 {
                    break 'parse_html_attr;
                }

                // parse rowspan and colspan attributes to set the cell's size
                if let Ok(value) = value.as_str().replace('"', "").parse::<i32>() {
                    if key.as_str().eq_ignore_ascii_case("rowspan") {
                        cell.rowspan = value;
                    } else if key.as_str().eq_ignore_ascii_case("colspan") {
                        cell.colspan = value;
                    }
                };

                // current token was html attribute -> continue with next token
                tokenizer.next();
                continue 'outer;
            }
        }

        // no html attribute -> current iteration contains table content
        for piece in text.pieces {
            cell.text.pieces.push(piece);
        }
        cell.text.trim_self();
        return Some(cell);
    }
}

fn parse_list(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
) -> Line {
    let mut items = vec![];
    loop {
        let mut list_prefix = vec![];
        // parse list_prefix
        while let Token::Colon | Token::Semicolon | Token::Star | Token::Sharp =
            &tokenizer.peek(0).0
        {
            let token = tokenizer.next().0;
            list_prefix.push(token);
        }
        if list_prefix.is_empty() {
            break;
        }

        let is_definition_list = list_prefix[list_prefix.len() - 1] == Token::Semicolon;
        let mut text_formatting = TextFormatting::Normal;
        let mut text = parse_text_until(
            tokenizer,
            error_consumer,
            Text::new(),
            &mut text_formatting,
            &|token: &Token<'_>| {
                // only seek until ':' in definitions, e.g. "; term : description"
                matches!(token, Token::Newline | Token::Eof | Token::OpenBraceWithBar)
                    || (is_definition_list && token == &Token::Colon)
            },
        );

        // continue parsing the list item's description
        if is_definition_list && tokenizer.peek(0).0 == Token::Colon {
            // backup the definition term early to be able to overwrite it with the description
            items.push((list_prefix.clone(), text));

            // replace ';' with ':' to indicate the start of a definition description
            list_prefix.pop();
            list_prefix.push(Token::Colon);
            tokenizer.next();

            text = parse_text_until(
                tokenizer,
                error_consumer,
                Text::new(),
                &mut text_formatting,
                &|token: &Token<'_>| {
                    matches!(token, Token::Newline | Token::Eof | Token::OpenBraceWithBar)
                },
            );
        }

        let (_, text_position) = tokenizer.next();
        if text_formatting != TextFormatting::Normal {
            debug!("Line contains unclosed text formatting expression at {text_position:?}");
        }

        items.push((list_prefix, text))
    }

    let items = items
        .iter()
        .map(|(prefix, text)| (prefix.iter().collect::<Vec<_>>(), text))
        .collect::<Vec<_>>();
    let list = build_list_structure(&items);
    Line::List { list }
}

/// Build a tree structure from nested lists, e.g.
///
/// ```wikitext
/// ; Mixed definition lists
/// ; item 1 : definition
/// :; sub-item 1 plus term
/// :: two colons plus definition
/// :; sub-item 2 : colon plus definition
/// ; item 2
/// : back to the main list
/// ```
///
/// see <https://www.mediawiki.org/wiki/Help:Lists>
fn build_list_items(items: &[(Vec<&Token>, &Text)]) -> Vec<ListItem> {
    let mut items = items.to_vec();
    let mut list_items = vec![];

    while !items.is_empty() {
        let (sublist_start_prefix, text) = items[0].clone();
        if sublist_start_prefix.is_empty() {
            list_items.push(ListItem::Text((*text).clone()));
            items.remove(0);
            continue;
        }

        let mut sublist_items = vec![];
        let is_definition = sublist_start_prefix[0] == &Token::Semicolon;
        while !items.is_empty() {
            let (cur_prefix, next_text) = &items[0];
            if cur_prefix.is_empty() {
                break;
            }

            if sublist_items.is_empty() {
                if is_definition && !matches!(cur_prefix[0], Token::Semicolon | Token::Colon) {
                    break;
                }

                if !is_definition && cur_prefix[0] != sublist_start_prefix[0] {
                    break;
                }
            }

            sublist_items.push((cur_prefix.to_vec(), (*next_text)));
            items.remove(0);
        }
        list_items.push(ListItem::List(build_list_structure(&sublist_items)));
    }

    list_items
}

fn build_list_structure(items: &[(Vec<&Token>, &Text)]) -> ListHead {
    if let Some((prefix, _text)) = items.first() {
        let root_prefix = &prefix[0];

        match root_prefix {
            Token::Star => ListHead {
                list_type: ListType::Unordered,
                items: build_list_items(
                    &items
                        .iter()
                        .map(|(prefix, text)| (prefix[1..].to_vec(), *text))
                        .collect::<Vec<_>>(),
                ),
            },
            Token::Sharp => ListHead {
                list_type: ListType::Ordered,
                items: build_list_items(
                    &items
                        .iter()
                        .map(|(prefix, text)| (prefix[1..].to_vec(), *text))
                        .collect::<Vec<_>>(),
                ),
            },
            Token::Semicolon => {
                let mut definitions = vec![];
                let mut definition = Text::new();
                let mut definition_values: Vec<(Vec<&Token>, &Text)> = vec![];

                // ensure that the previous definition list is always closed
                // by adding an empty one to the end (that'll be skipped)
                let mut items = items.to_vec();
                let empty_text = &Text::new();
                items.push(((vec![&Token::Semicolon]), empty_text));

                for item @ (prefix, text) in &items {
                    let root_prefix = &prefix[0];

                    if root_prefix == &&Token::Semicolon {
                        if !definition_values.is_empty() {
                            definitions.push(ListHead {
                                list_type: ListType::Definition(definition),
                                items: build_list_items(
                                    &definition_values
                                        .iter()
                                        .map(|(prefix, text)| (prefix[1..].to_vec(), *text))
                                        .collect::<Vec<_>>(),
                                ),
                            });
                        }

                        definition = (*text).clone();
                        definition_values = vec![];
                    } else {
                        definition_values.push((*item).clone());
                    }
                }

                ListHead {
                    list_type: ListType::ContainerList,
                    items: definitions
                        .iter()
                        .map(|def| ListItem::List(def.clone()))
                        .collect(),
                }
            }
            Token::Colon => ListHead {
                list_type: ListType::ContainerList,
                items: build_list_items(
                    &items
                        .iter()
                        .map(|(prefix, text)| (prefix[1..].to_vec(), *text))
                        .collect::<Vec<_>>(),
                ),
            },
            _ => unreachable!(),
        }
    } else {
        ListHead {
            list_type: ListType::Unordered,
            items: vec![],
        }
    }
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

                if let Some(cell) = parse_table_cell(tokenizer, error_consumer) {
                    current_row.push(cell);
                }
            }
            Token::VerticalBar | Token::DoubleVerticalBar => {
                current_type = TableRowType::Content;

                if let Some(cell) = parse_table_cell(tokenizer, error_consumer) {
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
            | Token::CloseComment
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
            Token::OpenComment => {
                // seek to the end of the comment
                while tokenizer.next().0 != Token::CloseComment {
                    continue;
                }
            }
            Token::HtmlTagOpen(tag, attrs) => {
                let tag = tag.to_string();
                let attrs = get_html_attrs(attrs);

                tokenizer.next();
                // raw blocks don't use the text formatting that was declared outside
                let mut text = parse_raw_block(
                    tokenizer,
                    error_consumer,
                    Text::new(),
                    &TextFormatting::Normal,
                    &|token| token == &Token::HtmlTagClose(tag.clone().into()),
                );
                tokenizer.next();

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
                    "gallery" => {
                        // only supported at line level
                        continue;
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
    terminator: &impl Fn(&Token<'_>) -> bool,
) -> Text {
    loop {
        if DO_PARSER_DEBUG_PRINTS {
            println!("parse_raw_block token: {:?}", tokenizer.peek(0));
        }
        let (token, text_position) = tokenizer.peek(0);

        if terminator(token) {
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

    if prefix_length != suffix_length {
        return None;
    }

    // seek until there's a new line (i.e. end of the heading's line)
    // comments after the headline are allowed, only other tokens aren't
    let mut is_in_comment = false;
    let mut whitespace_length = 0;
    loop {
        match &tokenizer
            .peek(text_limit + suffix_length + whitespace_length)
            .0
        {
            Token::Newline | Token::Eof => {
                break;
            }
            Token::OpenComment => {
                is_in_comment = true;
            }
            Token::CloseComment => {
                is_in_comment = false;
            }
            Token::Text(text) => {
                if !is_in_comment && text.chars().any(|c| !c.is_ascii_whitespace()) {
                    return None;
                };
            }
            _ => {
                if !is_in_comment {
                    return None;
                }
            }
        }

        whitespace_length += 1;
    }
    for _ in 0..text_limit + suffix_length + whitespace_length {
        tokenizer.next();
    }

    let label = label.trim().to_string();
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
            Token::VerticalBar => attributes.push(parse_attribute(
                tokenizer,
                error_consumer,
                text_formatting,
                &|token: &Token<'_>| matches!(token, Token::DoubleCloseBrace),
            )),
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
            | Token::OpenComment
            | Token::CloseComment
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
    terminator: &impl Fn(&Token<'_>) -> bool,
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
        if terminator(token) {
            value.pieces.push(TextPiece::Text {
                text: name.take().unwrap(),
                formatting: *text_formatting,
            });
            break;
        }
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
            | Token::OpenComment
            | Token::CloseComment
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
        &|token: &Token<'_>| matches!(token, Token::VerticalBar) || terminator(token),
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

    if DO_PARSER_DEBUG_PRINTS {
        println!("parse_link target token: {:?}", tokenizer.peek(0));
    }

    // parse target link or path
    let mut target = parse_raw_block(
        tokenizer,
        error_consumer,
        Text::new(),
        &TextFormatting::Normal,
        &|token: &Token<'_>| {
            matches!(
                token,
                Token::DoubleCloseBracket | Token::VerticalBar | Token::Newline | Token::Eof
            )
        },
    );
    target.trim_self();

    let (token, text_position) = tokenizer.peek(0);
    if let Token::Newline | Token::Eof = token {
        error_consumer(
            ParserErrorKind::UnmatchedDoubleOpenBracket.into_parser_error(*text_position),
        );
    }

    let mut options: Vec<Text> = vec![];
    let mut label: Option<Text> = None;

    while tokenizer.peek(0).0 == Token::VerticalBar {
        tokenizer.next();

        let option = parse_text_until(
            tokenizer,
            error_consumer,
            Text::new(),
            text_formatting,
            &|token| {
                matches!(
                    token,
                    Token::DoubleCloseBracket
                        | Token::DoubleCloseBrace
                        | Token::CloseBracket
                        | Token::VerticalBar
                        | Token::Eof
                )
            },
        );

        let (token, text_position) = tokenizer.peek(0);
        match token {
            Token::Eof => {
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBracket.into_parser_error(*text_position),
                );
                label = Some(option);
            }
            Token::VerticalBar => {
                options.push(option);
            }
            Token::DoubleCloseBrace | Token::CloseBracket => {
                error_consumer(
                    ParserErrorKind::UnexpectedTokenInLinkLabel {
                        token: token.to_string(),
                    }
                    .into_parser_error(*text_position),
                );
                error_consumer(
                    ParserErrorKind::UnmatchedDoubleOpenBracket.into_parser_error(*text_position),
                );
            }
            Token::DoubleCloseBracket => {
                label = Some(option);
                break;
            }
            _ => unreachable!(),
        }
    }
    if tokenizer.peek(0).0 == Token::DoubleCloseBracket {
        tokenizer.next();
    }

    // update text
    for _ in 0..surrounding_depth {
        text.extend_with_formatted_text(*text_formatting, "[[");
    }
    text.pieces.push(TextPiece::InternalLink {
        target: target.to_string(),
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

    if DO_PARSER_DEBUG_PRINTS {
        println!("parse_external_link token: {:?}", tokenizer.peek(0));
    }

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
        // only consider normal text pieces, otherwise we would also split nested expressions
        // e.g. [{{foo|bar baz}}] would otherwise falsely be split to {{foo|bar and baz}}
        if let TextPiece::Text {
            formatting: _,
            text,
        } = &piece
        {
            if let Some((url_part, text_part)) = text.split_once(" ") {
                is_in_link = false;

                link.pieces.push(TextPiece::Text {
                    formatting: *text_formatting,
                    text: url_part.to_string(),
                });

                label.pieces.push(TextPiece::Text {
                    formatting: *text_formatting,
                    text: text_part.to_string(),
                });
                continue;
            }
        }

        link.pieces.push(piece);
    }

    text.pieces.push(TextPiece::ExternalLink {
        target: link,
        label: if !label.is_empty() { Some(label) } else { None },
    });

    text
}

fn parse_gallery(
    tokenizer: &mut MultipeekTokenizer,
    error_consumer: &mut impl FnMut(ParserError),
    attr_text: &str,
) -> Line {
    debug_assert!(matches!(tokenizer.next().0, Token::HtmlTagOpen(_, _)));
    let attrs = get_html_attrs(attr_text);
    let mut images = vec![];

    let end_tag = Token::HtmlTagClose("gallery".into());
    while tokenizer.peek(0).0 != end_tag {
        // seek until end of whitespace
        loop {
            match &tokenizer.peek(0).0 {
                Token::Text(text) => {
                    if text.chars().all(|c| c.is_whitespace()) {
                        tokenizer.next();
                    } else {
                        break;
                    }
                }
                Token::Newline => {
                    tokenizer.next();
                }
                _ => {
                    break;
                }
            }
        }

        // parse target link or path
        let mut target = parse_raw_block(
            tokenizer,
            error_consumer,
            Text::new(),
            &TextFormatting::Normal,
            &|token: &Token<'_>| {
                matches!(token, Token::VerticalBar | Token::Newline | Token::Eof)
                    || token == &end_tag
            },
        );
        target.trim_self();

        let mut attributes = vec![];
        while tokenizer.peek(0).0 == Token::VerticalBar {
            let attr = parse_attribute(
                tokenizer,
                error_consumer,
                &mut TextFormatting::Normal,
                &|token| matches!(token, Token::Newline) || token == &end_tag,
            );
            attributes.push(attr);
        }

        let (token, text_position) = tokenizer.peek(0);
        if token == &Token::Eof {
            error_consumer(
                ParserErrorKind::UnmatchedHtmlBlockOpen.into_parser_error(*text_position),
            );
            break;
        }

        if !target.is_empty() || !attributes.is_empty() {
            images.push(GalleryEntry {
                target: target.to_string(),
                attributes,
            })
        }
    }
    tokenizer.next();

    Line::Gallery {
        attributes: attrs,
        images,
    }
}

#[test]
fn test_css_attr_regex() {
    assert!(HTML_ATTR_REGEX.is_match(r#"align-items="center""#))
}
