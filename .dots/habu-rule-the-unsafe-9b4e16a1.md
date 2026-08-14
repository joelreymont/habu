---
title: Rule the unsafe-set membership of generating definers
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T08:57:46.172852+02:00"
---

Found by the does-conv lane: layout-buffer, typed-buffer and cast: are in checker.f's UNSAFE-TOK? set BECAUSE they evaluate generated source; the converted definers (BEGIN-STRUCTURE, +FIELD, PTR-FIELD:, CFIELD:, and the coming BUFFER:/BUFFER-E/task trio) now do the same and are NOT listed. Arguably pre-existing (they always parsed live input via create) but it should be a decision: either they join the set with the reason stated, or the set's criterion is written down and they measurably fall outside it. Files: src/core/checker.f. Depends: the a65e56e5 conversions landing.
