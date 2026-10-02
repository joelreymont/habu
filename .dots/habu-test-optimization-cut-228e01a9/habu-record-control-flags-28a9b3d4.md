---
title: Record control flags for a word that shadows a binding
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:27:12.321053+02:00"
---

Problem (r4-dupthrow lane, ba5adbed, dot b25a209e): (1) src/core/checker.f CHECK's record step (~:16927, 'NMA @ NMU @ CTL-WORD ... <>') compares the new definition's control flags with the name's active binding, not the symbol being recorded, so a word whose name matches an existing binding (case-insensitive) with equal flags gets no control entry of its own. Measured: ': DIE ( ptr u8 n -- ) 70 die ;' in a package is not treated as never returning, so 's" x" DIE 0' certifies ($HOME/.cache/tmp/kestrel-r4-dupthrow/q/d1.f rc 0; non-shadowing d2.f rc 70 E-DEAD-CODE); throw flags and intact masks the same. Fixing it makes at least src/habu/aot-file.f:274 TARGET-ID ('DIE 0') dead code: census every site the fix newly refuses and fix each. (2) SHADOW-ARITY-CK (asked in CHECKER-PUBLISH-PARSED ~:10659) throws E-SHADOWED-ARITY after the control entry is appended; its comment 'before anything is written' is false; hoisting it needs the interned symbol, i.e. (1). Related: dot 7b3f85de (KEEP-ARITY bare-name lookup borrows a same-named global's record) — same class, say whether one change covers both. Acceptance: each new symbol gets the control entry of its own flags; every refusal in the record step precedes every write (comment true); cases for a shadowing never-returning word and a shadowed-arity refusal followed by a later definer of that public, seen failing first; baked: rebuild, g1 == g2, two-generation build. Base: after the master merge and ba5adbed land.
