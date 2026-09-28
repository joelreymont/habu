---
title: Measure repeated DATA encoding
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T19:36:05.916615+02:00\""
---

Current qualified paired engine contains629864 physical DATA-value bytes plus47736 grouped bitmap bytes; current codec independently ULEB-encodes every present64bitcell. User asks repeated-pattern optimization and up to1MB more size reduction. Investigate repeated byte/value sequences and compact lossless final-image representation, preserving every cell/bitmap/relocation and mutable runtime behavior. First source design of complete producer/consumer/version/temporary-memory paths and cheapest checked-Habu exact-byte census, then measure actual full stream/framing economics before implementation. Prefer a small established encoding over new general frameworks; no source/code executable compression, DATA pruning, guessed roots or inflated runtime state claims. Compact seed tables are an independent active feature; avoid double counting. Lead owns tracking and scope; no new production work without measured positive basis.
