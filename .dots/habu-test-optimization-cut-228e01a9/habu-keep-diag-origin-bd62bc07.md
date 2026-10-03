---
title: Keep diag-origin markers off definer names
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:24:37.424769+02:00"
---

Problem: tools/diag-origin-core.f:323-329 (DIAG-ORIGIN-RUN) emits an origin marker before every `:` it sees, including a `:` that is the name operand of a definer, so `create :` then `: F ( -- n ) 7 ;` then `F .` ($HOME/.cache/tmp/kestrel-r4-rev208/probes/n9.f) loads rc 0 and prints 7 but check.f dies rc 70 "hb: interpret stack underdepth: DIAG-ORIGIN!": the marker splits create from its name. (The first input found, c3d/w2.f with `: 42`, no longer reaches diag-origin after lexrec c3: the lint refuses `: 42` first. A reserved definer name such as `char` is lint-refused too, so only a non-reserved name such as `:` reaches this.) The set of definers is open (CREATE, VARIABLE, CONSTANT, user definers), so a fixed list is not the rule. Found by the r4-lexrec c3 worker (brief 129); input from review 208. Acceptance: n9.f checks rc 0 as it loads, never an interpret-stack underdepth; a real `:` definition after any definer still gets its marker; failing case first in the suite that owns diag-origin; decide the rule from what the engine does when it reads a definer's name (the lexrec c3 raw-operand marks may be the model). Base: after lexrec c3 (dot 1bed26df). Files: tools/diag-origin-core.f and its test.
