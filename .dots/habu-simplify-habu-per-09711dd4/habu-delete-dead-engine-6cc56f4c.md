---
title: Delete dead engine words
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.800206+03:00"
---

Problem: the variable RDP (src/core/util.f:56) has no caller in src, tools, test, zen, tender or maki (rg on master). src/core/roles.f VRDEF-FOLD-C and VRDEF-STR=CI (:184-195) duplicate src/core/util.f:47-54 CORE-FOLD-C and CORE-STR=CI, which load first. Words in habu1.f and habu2.f are left out: they are machinery being replaced (docs/design.md "Machinery being replaced").

Acceptance: each word above deleted, or kept with the caller found, after rg shows no caller in src, tools, test, zen, tender or maki; roles.f uses util.f's two words; hb's behaviour unchanged apart from the removed words.

Files: src/core/util.f, src/core/roles.f.

Verify: the candidate engine built as docs/bootstrap.md "Refresh `bin/hb`" says (`bin/hb --load tools/hb-build.f -- --engine -o $OUT/hb`), `test/run.f` on it, and the convergence check `tools/chain-run-build.f` (docs/bootstrap.md "Convergence check"), since the engine's bytes change.

Depends: none.

Ownership: unassigned. Claim: unassigned.
