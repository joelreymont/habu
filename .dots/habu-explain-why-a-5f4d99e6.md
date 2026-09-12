---
title: Explain why a private variable named DEPTH fails to certify under a binding row
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T17:48:25.869441+03:00"
---

Problem: a package-private variable DEPTH fails to certify in a body whose input row holds a CBIND:binding (': ZC ( CBIND:binding -- CBIND:binding ) DEPTH @ 0<> if 1 throw then ;' gives 'habu: in zc: at DEPTH'), while the same body reading a variable named OTHER certifies, and DEPTH @ certifies with ( -- ), ( n -- n ) or ( IR-CTX:ctx -- ... ) rows; found by the session-compile lane on 2026-09-12 (reproducer at the lane's scratchpad depth2.f, described in its report). The name collides with the engine's depth primitive; the worrying direction is the accepted cases, which may be resolving to the primitive and typechecking against it. Acceptance: the resolution rule for a package-private word that shadows a primitive is stated (docs/forth.md says a package word shadows a global or primitive with no root qualifier) and enforced the same way for every input row; a regression for the shadowing case with a nominal input row; the reproducer either certifies or is refused for a stated reason. Files: src/core/checker.f, test/. Verify: the regression on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.
