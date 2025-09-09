#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Display, Formatter};

/// The root of a wikitext document.
#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Wikitext {
    /// The root of the section tree of the page.
    pub root_section: Section,
}

impl Wikitext {
    /// Print the headlines of the text.
    pub fn print_headlines(&self) {
        self.root_section.print_headlines();
    }

    /// List the headlines of the text.
    pub fn list_headlines(&self) -> Vec<Headline> {
        let mut result = Vec::new();
        self.root_section.list_headlines(&mut result);
        result
    }

    /// List the double brace expressions of the text.
    pub fn list_double_brace_expressions(&self) -> Vec<TextPiece> {
        let mut result = Vec::new();
        self.root_section.list_double_brace_expressions(&mut result);
        result
    }

    /// List the plain parts of the text.
    pub fn list_plain_text(&self) -> Vec<TextPiece> {
        let mut result = Vec::new();
        self.root_section.list_plain_text(&mut result);
        result
    }
}

/// A section of wikitext.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Section {
    /// The headline of the section.
    pub headline: Headline,
    /// The paragraphs of the section.
    pub paragraphs: Vec<Paragraph>,
    /// The subsections of the section.
    pub subsections: Vec<Section>,
}

impl Section {
    /// Print the headlines of the text.
    pub fn print_headlines(&self) {
        println!(
            "{0} {1} {0}",
            "=".repeat(self.headline.level.into()),
            self.headline.label
        );
        for subsection in &self.subsections {
            subsection.print_headlines();
        }
    }

    /// List the headlines of the text.
    pub fn list_headlines(&self, result: &mut Vec<Headline>) {
        result.push(self.headline.clone());
        for subsection in &self.subsections {
            subsection.list_headlines(result);
        }
    }

    /// Iterate over all text pieces in the wikitext.
    pub fn iter_text_pieces(&self) -> impl Iterator<Item = &'_ TextPiece> {
        self.paragraphs
            .iter()
            .flat_map(|paragraph| paragraph.lines.iter())
            .flat_map(|line| line.iter_text_pieces())
    }

    /// List the double brace expressions of the text.
    pub fn list_double_brace_expressions(&self, result: &mut Vec<TextPiece>) {
        for text_piece in self.iter_text_pieces() {
            if matches!(text_piece, TextPiece::DoubleBraceExpression { .. }) {
                result.push(text_piece.clone());
            }
        }
        for subsection in &self.subsections {
            subsection.list_double_brace_expressions(result);
        }
    }

    /// List the plain parts of the text.
    pub fn list_plain_text(&self, result: &mut Vec<TextPiece>) {
        for text_piece in self.iter_text_pieces() {
            if matches!(text_piece, TextPiece::Text { .. }) {
                result.push(text_piece.clone());
            }
        }
        for subsection in &self.subsections {
            subsection.list_plain_text(result);
        }
    }
}

/// A headline of a section of wikitext.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Headline {
    /// The label of the headline.
    pub label: String,
    /// The level of the headline.
    pub level: u8,
}

impl Headline {
    /// Create a new headline with the given label and level.
    pub fn new(label: impl Into<String>, level: u8) -> Self {
        Self {
            label: label.into(),
            level,
        }
    }
}

/// A paragraph of a section.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Paragraph {
    /// The lines of the paragraph.
    pub lines: Vec<Line>,
}

/// A cell in a table
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct TableCell {
    /// The amount of columns this cell fills.
    pub colspan: i32,
    /// The amount of rows this cell fills.
    pub rowspan: i32,
    /// The text content of the cell.
    pub text: Text,
}

impl Default for TableCell {
    fn default() -> Self {
        Self {
            rowspan: 1,
            colspan: 1,
            text: Text::new(),
        }
    }
}

/// The type of a list.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ListType {
    /// Numbered
    Ordered,
    /// Items are in no specific order.
    Unordered,
    /// List containing multiple other lists. Shouldn't be extra-formatted.
    ContainerList,
    /// The first list item is the label/title, the following items are the definition.
    Definition(Text),
}

/// The entry point into a list.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ListHead {
    pub list_type: ListType,
    pub items: Vec<ListItem>,
}

/// The type of a list item.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ListItem {
    /// Normal text content.
    Text(Text),
    /// A nested list.
    List(ListHead),
}

impl ListItem {
    pub fn iter_text_pieces(&self) -> impl Iterator<Item = &'_ TextPiece> {
        match self {
            ListItem::Text(text) => text.pieces.iter().collect::<Vec<_>>(),
            ListItem::List(list_head) => list_head
                .items
                .iter()
                .flat_map(|item| item.iter_text_pieces())
                .collect::<Vec<_>>(),
        }
        .into_iter()
    }
}

/// A line of a paragraph.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Line {
    /// A normal text line.
    Normal {
        /// The text of the line.
        text: Text,
    },
    /// A list containing multiple list items.
    List {
        /// The entrypoint to the list.
        list: ListHead,
    },
    /// A table element.
    Table {
        /// The header rows of the table.
        header_rows: Vec<Vec<TableCell>>,
        /// The content rows of the table.
        content_rows: Vec<Vec<TableCell>>,
    },
}

impl Line {
    /// Returns true if the line would be ignored by the wikitext renderer.
    pub fn is_empty(&self) -> bool {
        match self {
            Line::Normal { text } => text.is_empty(),
            Line::List { .. } => false,
            Line::Table { .. } => false,
        }
    }

    /// Iterates over the text pieces of the line's contents.
    pub fn iter_text_pieces(&self) -> impl Iterator<Item = &'_ TextPiece> {
        match self {
            Line::Normal { text } => text.pieces.iter().collect::<Vec<_>>(),
            Line::List { list } => list
                .items
                .iter()
                .flat_map(|item| item.iter_text_pieces())
                .collect::<Vec<_>>(),
            Line::Table {
                header_rows,
                content_rows,
            } => {
                let header_pieces = header_rows
                    .iter()
                    .flat_map(|row| row.iter().flat_map(|r| r.text.pieces.iter()));
                let content_pieces = content_rows
                    .iter()
                    .flat_map(|row| row.iter().flat_map(|r| r.text.pieces.iter()));

                header_pieces.chain(content_pieces).collect()
            }
        }
        .into_iter()
    }
}

/// Some text, either a line or an argument to an expression.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Text {
    /// The pieces of the text.
    pub pieces: Vec<TextPiece>,
}

impl Text {
    /// Create a new empty text.
    pub fn new() -> Self {
        Default::default()
    }

    /// Returns `true` if this `Text` contains no pieces.
    pub fn is_empty(&self) -> bool {
        self.pieces.is_empty()
    }

    /// Extend the current last text piece with the given [`Text`](TextPiece::Text)
    pub fn extend_with_text(&mut self, text: Text) {
        for piece in text.pieces {
            self.pieces.push(piece);
        }
    }

    /// Extend the current last text piece with the given string,
    /// or append a new text piece created from the given string if there is no text piece
    /// or the last text piece is not of variant [`Text`](TextPiece::Text) or has different formatting.
    pub fn extend_with_formatted_text(&mut self, text_formatting: TextFormatting, text: &str) {
        if let Some(TextPiece::Text {
            formatting: last_formatting,
            text: last,
        }) = self.pieces.last_mut()
        {
            if text_formatting == *last_formatting {
                last.push_str(text);
                return;
            }
        }

        self.pieces.push(TextPiece::Text {
            formatting: text_formatting,
            text: text.to_string(),
        });
    }

    /// Trim whitespace from the beginning and the end of the text.
    pub fn trim_self(&mut self) {
        self.trim_self_start();
        self.trim_self_end();
    }

    /// Trim whitespace from the beginning of the text.
    pub fn trim_self_start(&mut self) {
        let mut offset = 0;
        while offset < self.pieces.len() {
            match &mut self.pieces[offset] {
                TextPiece::Text { text, .. } => {
                    *text = text.trim_start().to_string();
                    if !text.is_empty() {
                        break;
                    }
                }
                TextPiece::DoubleBraceExpression { .. }
                | TextPiece::InternalLink { .. }
                | TextPiece::ExternalLink { .. }
                | TextPiece::Math { .. }
                | TextPiece::Code { .. } => break,
            }
            offset += 1;
        }
        self.pieces.drain(..offset);
    }

    /// Trim whitespace from the end of the text.
    pub fn trim_self_end(&mut self) {
        let mut limit = self.pieces.len();
        while limit > 0 {
            match &mut self.pieces[limit - 1] {
                TextPiece::Text { text, .. } => {
                    *text = text.trim_end().to_string();
                    if !text.is_empty() {
                        break;
                    }
                }
                TextPiece::DoubleBraceExpression { .. }
                | TextPiece::InternalLink { .. }
                | TextPiece::ExternalLink { .. }
                | TextPiece::Math { .. }
                | TextPiece::Code { .. } => break,
            }
            limit -= 1;
        }
        self.pieces.drain(limit..);
    }
}

/// A piece of text of a section of wikitext.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum TextPiece {
    /// Plain text to be rendered as is, with the given formatting.
    Text {
        /// The formatting applied to the text.
        formatting: TextFormatting,
        /// The text.
        text: String,
    },
    /// A double brace expression.
    DoubleBraceExpression {
        /// The tag of the expression.
        tag: Text,
        /// The attributes of the expression.
        attributes: Vec<Attribute>,
    },
    /// An internal link.
    InternalLink {
        /// The link target.
        target: Text,
        /// The link options.
        options: Vec<Text>,
        /// The label of the link.
        label: Option<Text>,
    },
    /// An external link.
    ExternalLink {
        /// The link target.
        target: Text,
        /// The label of the link.
        label: Option<Text>,
    },
    /// A raw LaTeX math string.
    Math {
        /// Whether the math content is inline or a full line (block).
        block: bool,
        /// The raw LaTeX string.
        text: String,
    },
    /// A raw code string.
    Code {
        /// The name of the programming language of the code block.
        language: Option<String>,
        /// The raw code block.
        text: String,
    },
}

/// An attribute of e.g. a double brace expression.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Attribute {
    /// The name of the attribute.
    pub name: Option<String>,
    /// The value of the attribute.
    pub value: Text,
}

/// Format of formatted text.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[allow(missing_docs)]
pub enum TextFormatting {
    Normal,
    Italic,
    Bold,
    ItalicBold,
}

impl TextFormatting {
    /// Returns the new formatting to use after encountering an apostrophe run of length `apostrophe_length`.
    pub fn next_formatting(&self, apostrophe_length: usize) -> Self {
        match (self, apostrophe_length) {
            (TextFormatting::Normal, 2) => TextFormatting::Italic,
            (TextFormatting::Normal, 3) => TextFormatting::Bold,
            (TextFormatting::Normal, 5) => TextFormatting::ItalicBold,
            (TextFormatting::Italic, 2) => TextFormatting::Normal,
            (TextFormatting::Italic, 3) => TextFormatting::ItalicBold,
            (TextFormatting::Italic, 5) => TextFormatting::Bold,
            (TextFormatting::Bold, 2) => TextFormatting::ItalicBold,
            (TextFormatting::Bold, 3) => TextFormatting::Normal,
            (TextFormatting::Bold, 5) => TextFormatting::Italic,
            (TextFormatting::ItalicBold, 2) => TextFormatting::Bold,
            (TextFormatting::ItalicBold, 3) => TextFormatting::Italic,
            (TextFormatting::ItalicBold, 5) => TextFormatting::Normal,
            (_, apostrophe_length) => unreachable!("Unused apostrophe length: {apostrophe_length}"),
        }
    }
}

impl PartialOrd for TextFormatting {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (TextFormatting::Normal, TextFormatting::Normal) => Some(Ordering::Equal),
            (TextFormatting::Normal, TextFormatting::Italic) => Some(Ordering::Less),
            (TextFormatting::Normal, TextFormatting::Bold) => Some(Ordering::Less),
            (TextFormatting::Normal, TextFormatting::ItalicBold) => Some(Ordering::Less),
            (TextFormatting::Italic, TextFormatting::Normal) => Some(Ordering::Greater),
            (TextFormatting::Italic, TextFormatting::Italic) => Some(Ordering::Equal),
            (TextFormatting::Italic, TextFormatting::Bold) => None,
            (TextFormatting::Italic, TextFormatting::ItalicBold) => Some(Ordering::Less),
            (TextFormatting::Bold, TextFormatting::Normal) => Some(Ordering::Greater),
            (TextFormatting::Bold, TextFormatting::Italic) => None,
            (TextFormatting::Bold, TextFormatting::Bold) => Some(Ordering::Equal),
            (TextFormatting::Bold, TextFormatting::ItalicBold) => Some(Ordering::Less),
            (TextFormatting::ItalicBold, TextFormatting::Normal) => Some(Ordering::Greater),
            (TextFormatting::ItalicBold, TextFormatting::Italic) => Some(Ordering::Greater),
            (TextFormatting::ItalicBold, TextFormatting::Bold) => Some(Ordering::Greater),
            (TextFormatting::ItalicBold, TextFormatting::ItalicBold) => Some(Ordering::Equal),
        }
    }
}

impl Display for Text {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        for text_piece in &self.pieces {
            write!(fmt, "{text_piece}")?;
        }
        Ok(())
    }
}

impl Display for TextPiece {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TextPiece::Text { text, formatting } => {
                write!(fmt, "{}", formatting)?;
                write!(fmt, "{text}")?;
                write!(fmt, "{}", formatting)
            }
            TextPiece::DoubleBraceExpression {
                tag,
                attributes: parameters,
            } => {
                write!(fmt, "{{{{{tag}")?;

                for parameter in parameters {
                    write!(fmt, "|{parameter}")?;
                }

                write!(fmt, "}}}}")
            }
            TextPiece::InternalLink {
                target: url,
                options,
                label,
            } => {
                write!(fmt, "[[{url}")?;
                for option in options {
                    write!(fmt, "|{option}")?;
                }
                if let Some(label) = label {
                    write!(fmt, "|{label}")?;
                }
                write!(fmt, "]]")
            }
            TextPiece::ExternalLink { target: url, label } => {
                write!(fmt, "[{url}")?;
                if let Some(label) = label {
                    write!(fmt, " {label}")?;
                }
                write!(fmt, "]")
            }
            TextPiece::Math { block: _, text } => {
                write!(fmt, "<math>{text}</math>")
            }
            TextPiece::Code { language, text } => {
                write!(fmt, "<code lang=\"{:?}\">{text}</code>", language)
            }
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(fmt, "{name}=")?;
        }

        write!(fmt, "{}", self.value)
    }
}

impl Display for TextFormatting {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TextFormatting::Normal => Ok(()),
            TextFormatting::Italic => write!(fmt, "''"),
            TextFormatting::Bold => write!(fmt, "'''"),
            TextFormatting::ItalicBold => write!(fmt, "'''''"),
        }
    }
}
