---
title: "Fix the comments C4's review found"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.661085+01:00"
closed-at: "2026-11-16T04:07:36.115695+01:00"
---

Problem: comments name the old refusal vocabulary (rejection.f:53-54, check-hook.f:12-16, SIG-EXPECTED, VREC-WHY, CHECKER-DEFRECORD, 'Typed memory refuses', REJECTION:CLOSER! header, USIG-ADD-AS, check-all-errors-core.f, check-verify-child*.f, t0.f), point at other files (DICT-CELL@, LBUF-NAME-OK?), duplicate (conditional.f header), cite missing words (decl-diag-capture.f:1-5, structure-decl-suite.f:260-261 CA-DIAG-*), say 'packet' (generated-declaration.f, sumtype.f) or carry history (verify-source.f:198-200 E-SOURCE-READ's 'check.f's packets'); docs/forth.md:2058-2064 quotes the deleted hook: line. Acceptance: each says only what the code cannot, in reject wording. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
