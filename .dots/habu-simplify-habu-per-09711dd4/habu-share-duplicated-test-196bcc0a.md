---
title: Share duplicated test fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.203872+03:00"
---

Problem: 145 groups of test fixture definitions are copied verbatim (about 68.6K chars, 36.6K of them in test/compiler). HB$ is defined 26 times while engine-candidate.f:53 PATH$ already exists; the c2 e2e helpers are copied 5 times; a PCAP helper is copied into at least 20 files. This is against docs/gate.md:235. Acceptance: HB$ copies use ENGINE-CANDIDATE PATH$; the c2 e2e helpers and the PCAP helper each live in one test library; other groups outside test/compiler shared where at least three files copy them; test/compiler left alone (tier 1). Files: test/*.f users and new shared test libraries. Verify: the rows of the touched tests. Depends: the unrun-tests dot (some copies are in files it deletes). Ownership: test files touched. Claim: unassigned.
