---
title: "Engine: arity-guard deref-first top-level underflow"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T10:09:21.935949+02:00"
---

Follow-up to habu-engine-top-level-d3e79eda. The LMAIN depth-floor guard (src/habu/habu2.f EM-COMMENT + EM-INTERPRET-UNDERFLOW) converts ALL multi-token top-level underflows into a clean E-UNDERFLOW diagnostic (caught before the next token runs), covering the observed cad-4 consume-then-deref crash. Residual: a single deref/execute primitive (@ ! c@ c! type count execute) as the LITERAL FIRST token on an empty stack still faults inside the primitive (SIGSEGV -> crash handler exit 134) before the post-token guard can observe XDS<S0, e.g. 'bin/hb' loading a file whose first top-level token is '@'. Fully closing this needs a PRE-execution arity check at EM-INTERPRET-FIND: consult the found word's minimum input depth and throw E-UNDERFLOW before BLR if depth<min-in. Runtime dict records carry no arity today; the checker's certified sigs do (checker-owned, off-limits to the interpreter-underflow work). Options: (1) bake a per-primitive min-in table indexable by xt at the interpret boundary, or (2) expose a runtime-readable arity field on dict records. Add a negative regression: a --load file whose only line is '@' must exit with E-UNDERFLOW (not 134). Owner: interpreter loop + primitive model; coordinate with TFAM on any dict-record field change.
