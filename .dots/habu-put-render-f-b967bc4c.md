---
title: Put render.f in a package
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:58:57.993492+02:00"
blocks:
  - habu-c2e-refuse-unusable-0ff44b58
---

Problem: src/core/render.f has no package (card section 1: every module is a package): its words (RQM, RSBUF, RPOS, REND-EFFECT, REC-SIG, the DIAG and JSON writers) are globals named with an R or REND prefix, and checker.f reaches it only through defers (RECXT). New words in it follow the file's convention until it moves. Acceptance: render.f's words live in one package named for its subject, private unless another package calls them; callers outside it qualify or import; no behaviour change; the self-build fixpoint holds. Files: src/core/render.f and its callers (rg the public words). Verify: candidate build; chain fixpoint; bin/hb --load test/run.f. Depends: C2e (the facts writers settle which render words other packages call). Ownership: render.f. Claim: unassigned.
