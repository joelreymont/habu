---
title: "Link an alias of one of hb's words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T00:11:15.663236+01:00"
blocks:
  - habu-build-a-program-29edfe20
---

Problem: src/habu/linker.f SAME-START (:1631) looks for an alias's source only from FIRST-REC, so an alias whose code starts at one of hb's words answers -1; ID-REF+ then writes (HB-FILE, -1, 0), which ID-REF-CK marks BAD (before 29edfe20's fix it read REC-IDS at -2). Whether a program reaches it is not known. Acceptance: a program file that makes an alias of one of hb's words either builds and runs the same warm and cold, or is rejected at the alias with a placed rejection; a test shows it. Depends: habu-build-a-program-29edfe20. Claim: unassigned.
