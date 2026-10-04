---
title: Retire E-SHADOWED-ARITY
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:05:34.543778+03:00"
---

Found by lane 521 (dot 7b3f85de): src/core/checker.f SHADOW-ARITY-CK refuses E-SHADOWED-ARITY a package or private definition whose arity differs from a same-named word, only because src/compiler/native/compiler.f KEEP-ARITY read the effect record by bare name and could borrow the twin's arity. Once 7b3f85de lands (KEEP-ARITY, WORK, NO-RETURN? and RETRACT read the definition's own record through its checker symbol), the refusal guards nothing and refuses programs that compile correctly. Acceptance: a shadowing definition with a different arity that the checker certifies compiles and runs at tier 0 and tier 1 (seen refused E-SHADOWED-ARITY first through the real load path); SHADOW-ARITY-CK, the code and its lib/errors.f row are removed, test/shadowed-arity-test.f becomes the positive case or is deleted if test/compiler/native-own-record.f already covers it; docs (forth.md rules learned by refusal, the card) drop the rule; error-code-lint rc 0. Baked: rebuild, g1 == g2 with .names, two-generation build. Base: after 7b3f85de lands.
