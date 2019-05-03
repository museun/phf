# phf

This crate creates a perfect hash function for an enum, providing a single method: `T::lookup(&str) -> Option<T>`

This method will return either the exact match of the variant, or None

`std::fmt::Display` is also implemented for convenient printing of the string representation of the variant

## Examples:
### derive
Derive a perfect hash on a enum, uses the variant names, as lowercase in the PHF:
```rust
use phf::PerfectHash;
#[derive(PerfectHash, Debug, PartialEq)]
enum Keyword {
    Fn,
    When,
    If,
    Then,
    Else,
    Where,
    Select,
    Return
}

assert_eq!(Keyword::lookup("when"), Some(Keyword::When));
assert_eq!(Keyword::lookup("asdf"), None);
```

### mapping
Use a macro to create an enum for things that can't be trivially converted via their name:
```rust
use phf::{PerfectHash, phf_mapping};
// This takes in an enum name to generate, then the variants and what they should map to:
phf_mapping! { Sigil =>
    Pipe = "<|",
    FDiv = "/.",
    IDIv = "//",
};

assert_eq!(Sigil::lookup("<|"), Some(Sigil::Pipe));
```
