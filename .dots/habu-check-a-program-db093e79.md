---
title: Check a program past the source root it sets
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:23:54.616065+01:00"
---

Problem: tools/check.f --verify-only stops with W-CHECK-STOPPED at a top-level SOURCE-ROOT:ROOT! (tools/source-discovery.f SD-ROOT-MOVE?, :1094-1099), and checks nothing after it in the file or any file that loads it. Zen's src/server/server.f sets its root at line 5 (SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME SOURCE-ROOT:ROOT!) and Tender's tender.f at line 4 (ZEN-ROOT:ROOT$ SOURCE-ROOT:ROOT!), so a verify-only check of either program verifies four or five lines and none of its requires (seen on master 8afd1287, arm64 and x86-64 alike). Acceptance: verify-only of both programs checks every file they load, as a load reaches them, with a case in the check's tests that fails before. Depends: none. Claim: unassigned.
