---
title: Find why an ENUM constructor named x counts as taken
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:34:29.166486+02:00"
---

Problem (fold of carl-depth, test/checker-soundness-suite.f WIDE-COUNT): 'package CHECKER-SOUNDNESS public ENUM csdone x y ;ENUM' is refused 'bad enum declaration csdone: name is reserved or already taken at x' (uncaught 7110, rc 70), while 'ENUM csdone solo pair ;ENUM' declares. The spellings are settled (re-review of carl-depth, probes ~/.cache/tmp/rv-depth2/p/ b1 b2 h1 h2): a bare csdone resolves inside its own package (docs/forth.md 543-544), and CHECKER--SOUNDNESS-CSDONE:SOLO is the language's rule for a package family's generated words (docs/forth.md 646-649). Acceptance: a hunt report: why x counts as taken (which table or word claims it, path:line), whether that refusal is the language's rule (then forth.md and the card say it) or a defect (its cause and a fix brief). Files: read-only hunt. Verify: the two ENUM declarations above, each inside 'package CHECKER-SOUNDNESS' with 'public'. Depends: none. Ownership: none. Claim: unassigned.
