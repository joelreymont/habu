# Unicode data provenance

This directory pins the final Unicode Character Database for Unicode 16.0.0.
The class generator reads only `UnicodeData.txt`, `PropList.txt` and
`BidiBrackets.txt`; the script generator reads only `Scripts.txt` and
`PropertyValueAliases.txt`. The `ReadMe.txt` file identifies them as final
Version 16.0.0 data, and `LICENSE.txt` records the Unicode Data Files and
Software License.

| File | Source | SHA-256 |
|---|---|---|
| `ReadMe.txt` | `https://www.unicode.org/Public/16.0.0/ucd/ReadMe.txt` | `14cafa23788d3a20dd21d6b0cdcb8d6dab520781fcd9ad9392f3b88ea607e633` |
| `UnicodeData.txt` | `https://www.unicode.org/Public/16.0.0/ucd/UnicodeData.txt` | `ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f` |
| `PropList.txt` | `https://www.unicode.org/Public/16.0.0/ucd/PropList.txt` | `53d614508e2a0b2305a8aa21cd60d993de9326cdf65993660dfcce4503548583` |
| `BidiBrackets.txt` | `https://www.unicode.org/Public/16.0.0/ucd/BidiBrackets.txt` | `b8f32554c6f658821fb0ee742d21c5b1f2086b9bf13071fed04894b022f93d67` |
| `Scripts.txt` | `https://www.unicode.org/Public/16.0.0/ucd/Scripts.txt` | `9e88f0a677df47311106340be8ede2ecdacd9c1c931831218d2be6d5508e0039` |
| `PropertyValueAliases.txt` | `https://www.unicode.org/Public/16.0.0/ucd/PropertyValueAliases.txt` | `440fd3e5460b9bfe31da67b6f923992e1989d31fe2ed91e091c4b8f8e2620bf9` |
| `LICENSE.txt` | `https://www.unicode.org/license.txt` | `e7a93b009565cfce55919a381437ac4db883e9da2126fa28b91d12732bc53d96` |

Each generator verifies its input digests before parsing and writes its
table atomically. Each independent verifier checks every scalar against the
source properties and compares the table with a fresh byte-identical
regeneration.

## Why this version matches the GPT-2 reference

The pinned tokenizer reference is the Linux AArch64 wheel for `tiktoken`
0.13.0, SHA-256
`3f277ebea5edd7b8bf03c6f9431e1d67d517530115572b2dc1d465326e8f88c7`.
Its tagged `Cargo.toml` pins `fancy-regex` 0.17.0. The wheel embeds
`regex-syntax` 0.8.10; the published crate archive has SHA-256
`dc897dd8d9e8bd1ed8cdad82b5966c3e0ecae09fb1907d58efaa013543185d0a`.
That crate's generated `unicode_tables/age.rs` states that it was produced
from `ucd-16.0.0`, and its script table contains the scripts added in Unicode
16.0 while containing no Unicode 17 age.

The GPT-2 constructor in the same wheel uses `r50k_pat_str`, whose classes are
`\p{L}`, `\p{N}`, and `\s`. Native `tiktoken` matching therefore uses the
Unicode 16.0.0 general-category and White_Space truth encoded by that regex
engine. Pinning a newer host Unicode database would silently change token
boundaries and would not match the reference.

Reference sources:

- `https://github.com/openai/tiktoken/blob/0.13.0/Cargo.toml`
- `https://github.com/openai/tiktoken/blob/0.13.0/tiktoken_ext/openai_public.py`
- `https://static.crates.io/crates/regex-syntax/regex-syntax-0.8.10.crate`
- `https://www.unicode.org/versions/Unicode16.0.0/`
