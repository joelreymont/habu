---
title: Unify enum and structure token safety
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:43.380019+02:00"
---

Problem: checker asymmetry (cutover final review finding F9). The token enum is in UNSAFE-TOK? (src/core/checker.f line 6278) and in UNSAFE-SET-SEAL (line 6378); the token structure is in neither, so a colon body that compiles STRUCTURE certifies while one that compiles ENUM is refused. Post-cutover both are identical checked global declaration keywords, so the asymmetry is unprincipled. Required result: unify the treatment - probably both refused, since a declaration keyword inside a colon body is a form confusion - with the decision recorded at the definition site. Acceptance: minimal checked fixtures prove a colon body containing ENUM and one containing STRUCTURE get the same checker verdict; a negative regression pins the refused form; removing either token row fails a focused production checker fixture. Files: src/core/checker.f and the focused checker fixture. Verify: production checker suites and self-certification fixpoint. Depends: none. Ownership: the unsafe-token rows for the two declaration keywords only. Claim: unassigned.
