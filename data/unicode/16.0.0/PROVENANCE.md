# Unicode class data provenance

This directory pins the final Unicode Character Database for Unicode 16.0.0.
The generator reads only the two authoritative property files below. The
`ReadMe.txt` file identifies them as final Version 16.0.0 data, and
`LICENSE.txt` records the Unicode Data Files and Software License.

| File | Source | SHA-256 |
|---|---|---|
| `ReadMe.txt` | `https://www.unicode.org/Public/16.0.0/ucd/ReadMe.txt` | `14cafa23788d3a20dd21d6b0cdcb8d6dab520781fcd9ad9392f3b88ea607e633` |
| `UnicodeData.txt` | `https://www.unicode.org/Public/16.0.0/ucd/UnicodeData.txt` | `ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f` |
| `PropList.txt` | `https://www.unicode.org/Public/16.0.0/ucd/PropList.txt` | `53d614508e2a0b2305a8aa21cd60d993de9326cdf65993660dfcce4503548583` |
| `LICENSE.txt` | `https://www.unicode.org/license.txt` | `e7a93b009565cfce55919a381437ac4db883e9da2126fa28b91d12732bc53d96` |

The generated runtime artifact `lib/unicode/class-data.f` has SHA-256
`d40a9c36cca56d5b0757e552e0aa0a4920eea1593f8e8967a7abb89d3a8289d8`.
It is 16,615 bytes and contains 677 canonical letter ranges, 144 number
ranges, and 10 whitespace ranges.
The same value is stored alone in `class-data.sha256`, outside the generated
source so the lock is not self-referential. The generator verifies both input
digests before parsing, writes the table and lock atomically one file at a
time, and the independent verifier requires the lock, the actual output hash,
and a fresh byte-identical regeneration to agree.

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
