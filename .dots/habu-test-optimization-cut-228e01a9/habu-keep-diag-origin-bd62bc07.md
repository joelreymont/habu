---
title: Keep diag-origin markers off definer names
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:24:37.424769+02:00"
---

Problem: tools/diag-origin-core.f:323-329 (DIAG-ORIGIN-RUN) emits an origin marker before every `:` it sees, including a `:` that is the name operand of a definer, so `create :` then `: 42 ( -- ) ;` ($HOME/.cache/tmp/kestrel-r4-lexrec/c3d/w2.f) loads rc 0 but check.f dies rc 70 "hb: interpret stack underdepth: DIAG-ORIGIN!": the marker splits create from its name. The set of definers is open (CREATE, VARIABLE, CONSTANT, user definers), so a fixed list is not the rule. Found by the r4-lexrec c3 worker (brief 129). Acceptance: w2.f checks with the same verdict as it loads, with a located diagnostic if it refuses, never an interpret-stack underdepth; a real `:` definition after any definer still gets its marker; failing case first in the suite that owns diag-origin; decide the rule from what the engine does when it reads a definer's name (the lexrec c3 raw-operand marks may be the model). Base: after lexrec c3 (dot 1bed26df). Files: tools/diag-origin-core.f and its test.
