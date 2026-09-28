---
title: Table opcode names
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T20:41:01.318748+02:00\\\"\""
closed-at: "2026-09-29T09:18:17.991622+02:00"
close-reason: "Replace six immutable opcode-name dispatch ladders with shared typed tables. Independent review accepted; native HIR/A64IR, selection, session and twice-restored image checks pass. Five generations/names are identical at 2444023 bytes, saving 16512 signed bytes. Full registry ran492:491 passed; sole external Gforth cache failure passes unchanged with installed runtime libraries. Duplicate full run cancelled as redundant; Maki excluded by user. No DATA/CODE compression. Evidence ~/.cache/tmp/habu-opcode-names-completion-20260929-01.md."
---

Measured HIR/A64IR OP-NAME, RULE and RENDERER dispatch ladders occupy 11188 shipped code bytes before replacement costs. Replace repeated immutable name selection with small owner tables keyed by existing stable ORD, preserving enum typing, exact strings, interning ownership, invalid-ordinal refusals, source reconstruction and all schema behavior. No TAG substitution or runtime concatenation buffer. Keep schema finishing extraction separate. Use existing real schema, compiler session and capture paths; add only a genuine E2E gap before production code. Count tables, initialization, accessors, helper code, relocation/metadata, padding and signatures. Accept only measured positive complete product economics, independent Astra review and native qualification. Design and attribution: ~/.cache/tmp/habu-repeat-source-design-20260928-01.md and habu-generator-census-completion-20260928-01.md. Lead owns tracking and integration; Sol owns isolated implementation in .jj-ws/opcode-names.
