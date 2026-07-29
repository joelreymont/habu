---
title: Write the canonicalization stage into the design
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T01:39:18.799158+02:00"
---

Full context: gap 4 from agent ircanon 2026-07-30. docs/compiler-ir-design.md section 6.6 names table order and reference renumbering but no canonicalization stage or authority split; the implemented split (IR-CANON owns canonical ordinals plus the renumbered cell stream; the encoder owns framing, widths, versions, digest) is documented only in src/compiler/ir/canon.f's header. Add the one-paragraph stage description to section 6.6 with the authority split and the rejected alternative (re-materializing a canonical module is impossible because NEW-BUILDER interns the dialect name first, so a re-materialized symbol table is never sorted). The encoder lane (habu-encode-compiler-ir-545ee6d1) inherits this; also record there that eight canonical tables per context is the committed ceiling the encoder must live within.
