---
title: Package gate-pool and shorten the why-threw tails
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T10:53:07.302464+02:00"
---

Residual from the fold-handle landing: test/gate-pool.f is 1155 lines / 262 unpackaged global definitions with ~19 consumers, and its unpackaged state is now the only reason tools/why-threw.f keeps its name-repeating tails (WHY-THREW:WHY-THREW-DUMP, WHY-THREW:WHY-THREW - shortening them edits a gate-pool body and pulls the packaging in). Do the cascade with the proven recipe (EXPORT blocks from measured references, using-imports, no renames of consumer-visible spellings beyond the two why-threw tails, drivers after ;package), then respell the two why-threw entries to short non-redundant tails and fix the callers. Gates: both diff lints 0, full battery, engine byte-identical x2. Files: test/gate-pool.f, tools/why-threw.f, consumers found by the sweep. Depends: none.
