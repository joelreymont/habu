---
title: Warn when a typed local shadows a resolvable word
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T13:36:41.995760+02:00"
---

Found during json-write packaging: a locals group {: key:ptr ... :} silently shadowed the package word KEY (lookup is case-insensitive), so a bare KEY in the body resolved to the local pointer instead of the emitter; the checker caught only the downstream stack mismatch, not the shadowing itself, and the failure read as a confusing arity error at an unrelated word. Add a checker-side diagnostic: when a typed local's folded name equals a word resolvable at that point (open-package private/public, used publics once USING lands, or global), emit a named warning or reject outright - decide which after measuring how many existing locals groups trip it (a census first; if the tree is nearly clean, reject; if noisy, warn and dot the cleanup). Cover with positive fixtures (local shadowing a package word, a global, and - once USING lands - a used public) and prove the diagnostic names both the local and the shadowed word. The json-write case (local key vs public KEY, fixed by renaming to kp) is the reproducer to encode.

SHARPENED by the ij-locals landing (41b13bdd, 2026-08-13): the chain now
compiles a local named i/j INSIDE a loop (local-first, the engine's own
rule), so the trap this dot describes - a declaration silently shadowing
a resolvable word, most sharply the loop index - is live in BOTH
compilers. The lint this dot proposes is now the only guard.
