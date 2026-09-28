---
title: Measure repeated DATA encoding
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T19:36:05.916615+02:00\""
---

Current qualified paired engine contains629864 physical DATA-value bytes plus47736 grouped bitmap bytes; current codec independently ULEB-encodes every present64bitcell. User asks repeated-pattern optimization and up to1MB more size reduction. Investigate repeated byte/value sequences and compact lossless final-image representation, preserving every cell/bitmap/relocation and mutable runtime behavior. First source design of complete producer/consumer/version/temporary-memory paths and cheapest checked-Habu exact-byte census, then measure actual full stream/framing economics before implementation. Prefer a small established encoding over new general frameworks; no source/code executable compression, DATA pruning, guessed roots or inflated runtime state claims. Compact seed tables are an independent active feature; avoid double counting. Lead owns tracking and scope; no new production work without measured positive basis.

The exact checked census completed against source
`6aefe4ba622ece80dac6eba19e79bd060826fb84`, engine SHA-256
`5907200e52b43a55a24e27814dca4d82600a3d234923c3cad0e2dddc5763526c`.
Independent LZ4 blocks over canonical ULEB values save 164,228 physical bytes:
629,864 value bytes become 465,636 including 88 framing and two padding bytes.
The unchanged bitmap makes final DATA 513,372 versus 677,600 bytes. Expanding
the same cells to LE64 before compression saves only 45,172 bytes against the
original ULEB charge; repeat packets save 84 and modular delta blocks 6,944.
Compressing the bitmap separately saves 37,040 bytes; that is not yet a
combined representation or production proposal.

All bytes, 248,854 present values, 743,259 cell coordinates/presence bits,
actual wrappers and unchanged relocation rows were checked. All 84 codec
children exited zero. The declared-cell exclusion is structural: 447
WINDOW-tagged rows are absent; seven fixed engine cells are intentionally
present and restored separately. Accepted probe05 exited zero in 1.94 seconds.
Evidence: `~/.cache/tmp/habu-data-codec-census-completion-20260928-01.md`
and its retained checked probe, exact streams, hashes and replay instructions.

One bounded follow-up compares LZ4 HC level 9 with level 1 on the same ten
ULEB blocks and one bitmap block, using the same decoder format. No codec
sweep is authorized. Production Habu encoder/decoder size, startup cost and
native memory behavior remain unmeasured; no engine saving is claimed yet.
