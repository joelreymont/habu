---
title: Structure object linker tables
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:30:50.840669+02:00"
---

lib/object-link.f:29-63 defines 35 parallel columns and :106-244 adds exactly 35 pointer accessors for package, type, import/export, definition, relocation, and object rows. DEF+ accepts five indistinguishable n/string-span fields; REL+ stores kind/symbol spans, patch, and target=-1 across six columns. Same-width off/len/effect/address/patch swaps certify, EXP-IDX/DEF-IDX expose -1, relocation kind remains a string until APPLY-RELOC, and a throw during repeated SYM+ calls can consume arena bytes before the row count commits. Define STRUCTURE rows for each semantic table with named typed span/address/offset fields and store them in LAYOUT-BUFFER. Define relocation-kind ENUM with one explicit object-wire codec and a payload ENUM relocation-state (unresolved | resolved(target)); return option<export-id>/option<definition-id> from lookups. Preflight row and symbol-arena capacity and commit each row plus arena cursor transactionally; unresolved relocation cannot reach APPLY-RELOC by type. Preserve object format, symbol/effect matching, relocation order, merged text/data bytes, package visibility, and errors. Add checker negatives for adjacent and cross-row swaps, unknown relocation wire kinds, unresolved apply, canary/full-capacity/injected-throw atomicity, and exact link/relocation byte goldens. Measure source lines, pointer helpers removed, JIT/DATA/CODELEN, table bytes, and link throughput before/after. Files: lib/object-link.f and focused object/link tests. Verify object/link/AOT/build/fixpoint suites, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: in-memory linker row representation and atomic append only.
