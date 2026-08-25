---
title: "variable I shadows nothing: loop-index primitive wins in bodies"
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T21:56:57.345494+02:00"
---

Found by bake-chain-18's census probe (2026-08-16), unchecked code only: 'variable I' at top level defines a word, but inside a colon body I still compiles the LOOP-INDEX primitive, so 'I @' dereferences the loop index register and SIGSEGVs (ldr x9,[x9], x9=0x10000, exit 134) with no diagnostic. The checker rejects I outside a loop, so checked code is safe - the hazard is unchecked/tool code and the REPL. Decide the fix at the root: either the definer refuses single-letter names that collide with loop primitives (I, J - a named refusal at definition time), or body compilation prefers a user definition over the loop primitive when one exists and no loop is open. Reproducer is two lines; keep it as the regression either way.
