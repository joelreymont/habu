---
title: "Skip a dead parsing word's text in the checker"
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T10:31:27.666829+02:00"
---

Problem: src/core/checker.f's dead region (LIVE-TOKEN?, DEAD-CLOSE?) scans the text of a dead parsing word (s" ." [char] ' ['] postpone is) as tokens, so a closer inside that text ends the region. A checked body `: G4 ( -- [ -- n ] ) [: 1 exit s" a ;] b" drop ;] ;` (probe /tmp/jerry-rv-t0fe/p/g4k.f, review of lane t0fe 2026-10-31) is refused as "undefined word 'b\"'" instead of at `s"` after 'exit'. Accept or reject is right; the message names the wrong token. T0 refuses the TRUSTED: body at `s"` after lane t0fe (it refuses the first dead row whatever the checker's region does). Acceptance: the checked body is refused at `s"` after 'exit'; a dead parsing word consumes its text as the live path does. Files: src/core/checker.f DEAD-TOK / the dispatch at ~22417, test/load-reject-diag-test.f. Verify: row load-reject-diag; g4k.f. Not a T0 release prerequisite. Ownership: unassigned. Claim: unassigned.
