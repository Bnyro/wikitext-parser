use std::collections::HashMap;

use crate::wikitext::{GalleryEntry, Headline, Line, ListHead, ListItem, ListType, Paragraph};
use crate::{
    parse_wikitext, Attribute, ParserErrorKind, Section, TableCell, Text, TextFormatting,
    TextPiece, TextPosition, Wikitext,
};

mod full_pages;

#[test]
fn test_wiktionary_free_substrings() {
    let input_json_strings = [
        r#""[[File:Free Beer.jpg|thumb|A sign advertising '''free''' beer (obtainable without payment). It is a joke: every day the sign is read, the free beer will be available \"tomorrow\".]]""#,
        r#""{{a}}ȝ""#,
        r#""[[s:Twelve O'Clock|Twelve O'Clock]]""#,
    ];
    for input_json_string in input_json_strings {
        let input: String = serde_json::from_str(input_json_string).unwrap();
        println!("{input:?}");
        let mut errors = Vec::new();
        parse_wikitext(
            &input,
            "free".to_string(),
            &mut Box::new(|error| errors.push(error)),
        );
        assert!(errors.is_empty());
    }
}

#[test]
fn test_wiktionary_nested_formatting() {
    let input = r"{{quote-book|en|year=1988|author=Andrew Radford|title=Transformational grammar: a first course|location=Cambridge, UK|publisher=Cambridge University Press|page=339|chapter=7|passage=But what other kind(s) of syntactic information should be included in Lexical Entries? Traditional '''dictionaries''' such as Hornby's (1974) ''Oxford Advanced Learner's '''Dictionary''' of Current English'' include not only ''categorial'' information in their entries, but also information about the range of ''Complements'' which a given item permits (this information is represented by the use of a number/letter code).}}";
    let mut errors = Vec::new();
    parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
}

#[test]
fn test_wiktionary_nested_formatting_with_combined_open_or_close() {
    let input = r"0''a'''b'''''c'''d''e'''''f'''''g''h'''i'''''j'''k''l'''''m'''''";
    let mut errors = Vec::new();
    assert_eq!(
        parse_wikitext(
            input,
            Default::default(),
            &mut Box::new(|error| errors.push(error))
        )
        .root_section,
        Section {
            headline: Headline::new("", 1),
            paragraphs: vec![Paragraph {
                lines: vec![Line::Normal {
                    text: Text {
                        pieces: vec![
                            TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "0".to_string()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Italic,
                                text: "a".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::ItalicBold,
                                text: "b".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "c".to_string()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Bold,
                                text: "d".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::ItalicBold,
                                text: "e".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "f".to_string()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::ItalicBold,
                                text: "g".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Bold,
                                text: "h".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "i".to_string()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::ItalicBold,
                                text: "j".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Italic,
                                text: "k".into()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "l".to_string()
                            },
                            TextPiece::Text {
                                formatting: TextFormatting::ItalicBold,
                                text: "m".into()
                            },
                        ]
                    }
                }],
            }],
            subsections: Default::default(),
        }
    );
    assert!(errors.is_empty());
}

#[test]
fn test_complex_internal_links() {
    let input = r#"[[File:Free Beer.jpg|thumb|A sign advertising '''free''' beer (obtainable without payment). It is a joke: every day the sign is read, the free beer will be available &quot;tomorrow&quot; ([[ergo]] never).]]
[[File:Buy one, get one free ^ - geograph.org.uk - 153952.jpg|thumb|A &quot;buy one get one '''free'''&quot; sign at a flower stand (obtainable without additional payment).]]
[[File:Berkeley Farms Fat-Free Half &amp; Half.jpg|thumb|This food product ([[half and half]]) is labelled &quot;fat '''free'''&quot;, meaning it contains no detectable fat.]]"#;
    let mut errors = Vec::new();
    parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
}

#[test]
fn test_section_headers() {
    let input = "<ref name=\"ISample\"/><ref name=\"COttoni\"/>";
    let mut errors = Vec::new();
    parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
}

#[test]
fn test_headlines() {
    let input = "==abc===c==b==g== a \n ==c==";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        "title".to_string(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
    let headlines = parsed.list_headlines();
    assert_eq!(headlines, vec![Headline::new("title", 1),]);

    let input = "==abc== \n =c= \n==b==g== a \n=====c=====";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        "title".to_string(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
    let headlines = parsed.list_headlines();
    assert_eq!(
        headlines,
        vec![
            Headline::new("title", 1),
            Headline::new("abc", 2),
            Headline::new("c", 5),
        ]
    );

    let input = "== History ==\n=== 2006–2009: Early years  ===";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        "title".to_string(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
    let headlines = parsed.list_headlines();
    assert_eq!(
        headlines,
        vec![
            Headline::new("title", 1),
            Headline::new("History", 2),
            Headline::new("2006–2009: Early years", 3)
        ]
    );

    let input = "== first ==<!-- comment --> \n=== second === <!-- comment --> invalid chars";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        "title".to_string(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
    let headlines = parsed.list_headlines();
    assert_eq!(
        headlines,
        vec![Headline::new("title", 1), Headline::new("first", 2),]
    );
}

#[test]
fn test_equals_in_tag() {
    let input = "{{fake==|English}}\n{{fake===|Noun}}\n{{fake====|Synonyms}}";
    let mut errors = Vec::new();
    parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert!(errors.is_empty());
}

#[test]
fn test_complex_double_brace_expression() {
    let input_json = "\"{{der3|vi\\n|{{vi-l|y tá|醫佐|[[nurse]]}}\\n|{{vi-l|Đông y|東醫|[[traditional]] [[East Asian]] medicine]]}}\\n|{{vi-l|y học|醫學|[[medicine]]}}\\n|{{vi-l|Tây y|西醫|[[modern]] [[medicine]]}}\\n|{{vi-l|pháp y|法醫|[[forensic]] [[science]]}}\\n|{{vi-l|y khoa|醫科|[[medicine]]}}\\n|{{vi-l|y sĩ|醫士|([[junior]]) [[physician]]}}\\n|{{vi-l|y tế|醫濟|[[health care]]}}\\n|{{vi-l|nan y|難醫|(of [[disease]]) [[difficult]] to [[cure]]}}\\n|{{vi-l|lương y|良醫|([[literary]]) a [[good]] [[physician]]}}\\n|{{vi-l|y sinh|醫生|[[physician]]}}\\n|{{vi-l|y dược|醫藥|[[medicine]] and [[pharmacy]]}}\\n|{{vi-l|y viện|醫院|([[literary]]) [[hospital]]}}\\n|{{vi-l|lương y như từ mẫu|良醫如慈母|([[literary]]) a [[good]] [[physician]] is [[like]] a good [[mother]]}}\\n|{{vi-l|y đạo|醫道|([[literary]]) [[art]] of [[healing]]}}\\n|{{vi-l|y lệnh|醫令|[[doctor]]'s [[instructions]]}}\\n}}\"";
    let input: String = serde_json::from_str(input_json).unwrap();
    let mut errors = Vec::new();
    parse_wikitext(
        &input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        errors,
        vec![
            ParserErrorKind::UnmatchedDoubleCloseBracket.into_parser_error(TextPosition {
                line: 3,
                column: 58
            })
        ]
    );
}

#[test]
fn test_multiple_root_sections() {
    let input_json = "\"=a=\\nsome text\\n=b=\\nsome more text\\n=c=\"";
    let input: String = serde_json::from_str(input_json).unwrap();
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        &input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );

    assert_eq!(
        errors,
        vec![
            ParserErrorKind::SecondRootSection {
                label: "a".to_string()
            }
            .into_parser_error(TextPosition { line: 1, column: 1 }),
            ParserErrorKind::SecondRootSection {
                label: "b".to_string()
            }
            .into_parser_error(TextPosition { line: 3, column: 1 }),
            ParserErrorKind::SecondRootSection {
                label: "c".to_string()
            }
            .into_parser_error(TextPosition { line: 5, column: 1 }),
        ]
    );

    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline::new("", 1),
                paragraphs: Vec::new(),
                subsections: Vec::new()
            },
        }
    );
}

#[test]
fn test_simple_table() {
    let input = r#"{| class="wikitable" style="margin:auto"
        |+ Caption text
        |-
        ! Header 1 !! Header 2
        |-
        |align="center"|Orange || Apple
        |-
        |colspan=2|Bread
        |Pie
        |}"#;

    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );

    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Table {
                        header_rows: vec![vec![
                            TableCell {
                                rowspan: 1,
                                colspan: 1,
                                text: Text {
                                    pieces: vec![TextPiece::Text {
                                        formatting: TextFormatting::Normal,
                                        text: "Header 1".to_string()
                                    }]
                                }
                            },
                            TableCell {
                                rowspan: 1,
                                colspan: 1,
                                text: Text {
                                    pieces: vec![TextPiece::Text {
                                        formatting: TextFormatting::Normal,
                                        text: "Header 2".to_string()
                                    }]
                                }
                            }
                        ]],
                        content_rows: vec![
                            vec![
                                TableCell {
                                    rowspan: 1,
                                    colspan: 1,
                                    text: Text {
                                        pieces: vec![TextPiece::Text {
                                            formatting: TextFormatting::Normal,
                                            text: "Orange".to_string()
                                        }]
                                    }
                                },
                                TableCell {
                                    rowspan: 1,
                                    colspan: 1,
                                    text: Text {
                                        pieces: vec![TextPiece::Text {
                                            formatting: TextFormatting::Normal,
                                            text: "Apple".to_string()
                                        }]
                                    }
                                }
                            ],
                            vec![
                                TableCell {
                                    rowspan: 1,
                                    colspan: 2,
                                    text: Text {
                                        pieces: vec![TextPiece::Text {
                                            formatting: TextFormatting::Normal,
                                            text: "Bread".to_string()
                                        }]
                                    }
                                },
                                TableCell {
                                    rowspan: 1,
                                    colspan: 1,
                                    text: Text {
                                        pieces: vec![TextPiece::Text {
                                            formatting: TextFormatting::Normal,
                                            text: "Pie".to_string()
                                        }]
                                    }
                                }
                            ],
                        ]
                    }]
                }],
                subsections: vec![]
            }
        }
    );
}

#[test]
fn test_complex_table_header() {
    let input = r#"{|class="wikitable sortable"
|-
!rowspan=2|Year !! rowspan=2|Total number of<br>billionaires !! rowspan=2|Combined wealth of<br>known billionaires !! colspan=5|Number of billionaires 
! colspan="2" |World's wealthiest<br>individual
|-
!   [[List of Americans by net worth|U.S.]] !! [[List of Chinese by net worth|Chinese]] !! [[List of Indian people by net worth|Indian]]  
![[Germany|German]]!! [[List of Russians by net worth|Russian]]
! Name!!Net worth
|}
    "#;

    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    match &parsed.root_section.paragraphs[0].lines[0] {
        Line::Table {
            header_rows,
            content_rows: _,
        } => {
            let first_cell = header_rows.first().unwrap().first().unwrap();
            assert_eq!(first_cell.rowspan, 2);
            assert_eq!(first_cell.colspan, 1);
            assert_eq!(first_cell.text.to_string(), "Year");
        }
        _ => panic!(
            "expected table type, got {:?}",
            parsed.root_section.paragraphs[0].lines[0]
        ),
    }
}

#[test]
fn test_parse_math() {
    let input = r#"See the following equation: <math display="block">2+3</math>. We see that"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![
                                TextPiece::Text {
                                    formatting: TextFormatting::Normal,
                                    text: "See the following equation: ".to_string()
                                },
                                TextPiece::Math {
                                    block: true,
                                    text: "2+3".to_string(),
                                },
                                TextPiece::Text {
                                    formatting: TextFormatting::Normal,
                                    text: ". We see that".to_string()
                                },
                            ],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
}

#[test]
fn test_parse_code() {
    let input = r#"<syntaxhighlight lang="c">int a = 2+3;</syntaxhighlight>"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![TextPiece::Code {
                                language: Some("c".to_string()),
                                text: "int a = 2+3;".to_string(),
                            },],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
}

#[test]
fn test_external_link() {
    let input = r"[https://example.com normal | ''italic'']";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![TextPiece::ExternalLink {
                                target: Text {
                                    pieces: vec![TextPiece::Text {
                                        formatting: TextFormatting::Normal,
                                        text: "https://example.com".into()
                                    }]
                                },

                                label: Some(Text {
                                    pieces: vec![
                                        TextPiece::Text {
                                            formatting: TextFormatting::Normal,
                                            text: "normal | ".to_string()
                                        },
                                        TextPiece::Text {
                                            formatting: TextFormatting::Italic,
                                            text: "italic".to_string()
                                        }
                                    ]
                                }),
                            },],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
}

#[test]
fn test_parse_comment() {
    let input = r#"before <!-- comment --> after"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![TextPiece::Text {
                                formatting: TextFormatting::Normal,
                                text: "before  after".to_string()
                            },],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
}

#[test]
fn test_parse_list() {
    let input = "#;Title 1\n#:Item A\n#;Title 2\n#:Item B";
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::List {
                        list: ListHead {
                            list_type: ListType::Ordered,
                            items: vec![ListItem::List(ListHead {
                                list_type: ListType::ContainerList,
                                items: vec![
                                    ListItem::List(ListHead {
                                        list_type: ListType::Definition(Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Title 1".to_string()
                                            }]
                                        }),
                                        items: vec![ListItem::Text(Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Item A".to_string()
                                            }]
                                        })]
                                    }),
                                    ListItem::List(ListHead {
                                        list_type: ListType::Definition(Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Title 2".to_string()
                                            }]
                                        }),
                                        items: vec![ListItem::Text(Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Item B".to_string()
                                            }]
                                        })]
                                    })
                                ]
                            })]
                        }
                    }]
                }],
                subsections: vec![]
            }
        }
    );
}

#[test]
fn test_parse_formatted_html_block() {
    let input = r#"'''<code>file</code>'''"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );

    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![TextPiece::Code {
                                language: None,
                                text: "file".to_string(),
                            },],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
    assert!(errors.is_empty());
}

#[test]
fn test_parse_nested_internal_links() {
    let input = r#"[[File:Europe germanic-languages 2.PNG|thumb|

    '''[[Anglic languages]]'''
    {{legend|#FCA503|English}}
    {{legend|#FD7B24|[[Scots language|Scots]]}}
    '''[[Anglo-Frisian languages]]'''<br/> Anglic and
    {{legend|#E9D803|[[Frisian languages|Frisian]] ([[West Frisian language|West]], [[North Frisian language|North]], [[Saterland Frisian language|Saterland]])}}
    '''[[North Sea Germanic languages]]''' Anglo-Frisian and
    {{legend|#80FF00|[[Low German|Low German/Low Saxon]]}}
    '''[[West Germanic languages]]'''<br/> North Sea Germanic and
    {{legend|#F0F702|[[Dutch language|Dutch]]; in Africa: [[Afrikaans]]}}
    ...... German ([[High German|High]]):
    {{legend|#00FF00|[[Central German|Central]]; in [[Luxembourg|Lux.]]: [[Luxembourgish]]}}
    {{legend|#008000|[[Upper German|Upper]]}}
    ...... [[Yiddish]]]]"#;
    let mut errors = Vec::new();
    let _parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    dbg!(&errors);
    assert!(errors.is_empty());
}

#[test]
fn test_parse_gallery() {
    let input = r#"<gallery attrs="foo">
A b c.jpg|Label 1|opt1=a|opt2=b
 D e f.jpg|Label 2|opt1=a| opt2=b

</gallery>"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    dbg!(&parsed);
    dbg!(&errors);

    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Gallery {
                        attributes: HashMap::from([("attrs".to_string(), "foo".to_string())],),
                        images: vec![
                            GalleryEntry {
                                target: "A b c.jpg".to_string(),
                                attributes: vec![
                                    Attribute {
                                        name: None,
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Label 1".to_string(),
                                            },],
                                        },
                                    },
                                    Attribute {
                                        name: Some("opt1".to_string(),),
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "a".to_string(),
                                            },],
                                        },
                                    },
                                    Attribute {
                                        name: Some("opt2".to_string(),),
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "b".to_string(),
                                            },],
                                        },
                                    },
                                ],
                            },
                            GalleryEntry {
                                target: "D e f.jpg".to_string(),
                                attributes: vec![
                                    Attribute {
                                        name: None,
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "Label 2".to_string(),
                                            },],
                                        },
                                    },
                                    Attribute {
                                        name: Some("opt1".to_string(),),
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "a".to_string(),
                                            },],
                                        },
                                    },
                                    Attribute {
                                        name: Some("opt2".to_string(),),
                                        value: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "b".to_string(),
                                            },],
                                        },
                                    },
                                ],
                            },
                        ],
                    },],
                },],
                subsections: vec![],
            },
        }
    );
    assert!(errors.is_empty());
}

#[test]
fn test_parse_template_in_external_link() {
    let input = r#"[{{IPA|ˈdɑn.əld dʒɑn tɹɐmp}}]"#;
    let mut errors = Vec::new();
    let parsed = parse_wikitext(
        input,
        Default::default(),
        &mut Box::new(|error| errors.push(error)),
    );
    dbg!(&errors);
    dbg!(&parsed);

    assert!(errors.is_empty());
    assert_eq!(
        parsed,
        Wikitext {
            root_section: Section {
                headline: Headline {
                    label: "".to_string(),
                    level: 1,
                },
                paragraphs: vec![Paragraph {
                    lines: vec![Line::Normal {
                        text: Text {
                            pieces: vec![TextPiece::ExternalLink {
                                target: Text {
                                    pieces: vec![TextPiece::DoubleBraceExpression {
                                        tag: Text {
                                            pieces: vec![TextPiece::Text {
                                                formatting: TextFormatting::Normal,
                                                text: "IPA".to_string(),
                                            }],
                                        },
                                        attributes: vec![Attribute {
                                            name: None,
                                            value: Text {
                                                pieces: vec![TextPiece::Text {
                                                    formatting: TextFormatting::Normal,
                                                    text: "ˈdɑn.əld dʒɑn tɹɐmp".to_string(),
                                                },],
                                            },
                                        },],
                                    },],
                                },
                                label: None,
                            },],
                        },
                    },],
                },],
                subsections: vec![],
            },
        }
    );
}
