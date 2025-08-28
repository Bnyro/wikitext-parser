# Approximate parser for wikitext

This is a fork of [ISibboI's wikitext parser](https://github.com/ISibboI/wikitext-parser).

Parse wikitext into an approximate representation, parsing some features of wikitext correctly, while being mostly incorrect for erroneous wikitext.

## Usage
Add the following to your `Cargo.toml`:
```toml
wikitext-parser = { git = "https://github.com/Bnyro/wikitext-parser" }
```

You can then parse wikitext into a semantical representation using the following code
```rust
let wikitext = "== Hello World =="; // the raw wikitext string
let title = String::from("My test document"); // the document's title
let parsed = wikitext_parser::parse_wikitext(wikitext, title, |err| {
    // handle parsing errors
});

// list all headlines
parsed.print_headlines();
```

For an example project using it, see [wikiviewer on Codeberg](https://codeberg.org/bnyro/wikiviewer).
