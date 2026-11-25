---
title: Nest binders past 32 in one effect
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:13:03.768597+01:00"
---

Problem: src/core/checker.f BINDER-DEPTH-MAX (32, :6848) sizes fixed tables (BIND-DOMAINS :6852, src/core/render.f RBIND :245, src/core/check-walk.f GUEST-CAP :3388), so a signature nesting more binders is rejected (checker.f :7582, :11314) and a sig-less word whose inferred effect nests more is left unrecorded with W-EFFECT-NOT-RECORDED (render.f RBIND-FULL, REC-REJECT-WHY, REC-REJECT-HELP); test/engine-suite.f RSF33; test/diag-position-test.f TEST-NOT-RECORDED and tools/lsp-test-lib.f WIDENS trigger W-EFFECT-NOT-RECORDED with 33 binders once dot habu-name-every-type-307586d5 lands, so they need a trigger this change keeps. The cap is Habu's own table. Acceptance: binders nest to any depth (the tables grow); RSF33's word records and its caller certifies; the BINDER-DEPTH-MAX reasons go. Depends: none. Claim: unassigned.
