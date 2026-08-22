---
title: twelve native die sites print only the offending token
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.228185+02:00"
---

Problem: C-DIE-DOES 2444, C-SEAL-PACKAGE-FAIL 2945, C-SIG-BAD 2571, C-DEFER-DIE-TOKEN 3380, J-QUOT 2600, J-SEMIQUOT 2614, J-DOES 2877, C-STORE-NAME fail 2714, C-QUALIFY-FAIL 2719, C-LOCAL-REF 4454, C-PACKAGE-FAIL 6193, C-POSTPONE 3670 print one token and an rc in the shared 70-77 family; the mirror adds bare exit_group with no bytes at forth.fs:3098,3336,3353,3592,3607,3622,3625,3650,3658,3666. This is what made the recovery failure 'pick-reason at CODE-REASON rc 70' un-diagnosable. Acceptance: the DEFER-DIAG shape (label + token + newline) at every site in both engines; a test per site asserting the label. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/. Verify: the tests. Depends: none. Ownership: diagnostics. Claim: unassigned.
