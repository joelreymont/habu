---
title: Check that each SPARQL fix removes its mistake
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T16:51:25.317051+01:00"
closed-at: "2026-11-27T18:42:11.827517+01:00"
---

Problem: src/lib/sparql/syntax-test.f FOLLOW passes any chain of fixes that parses within 32, so a fix that leaves its mistake in place, or that the follower applies only in part, still passes; 40 of the 123 chains that parse take 2 to 10 fixes. Acceptance: each case's chain takes one fix per mistake its query holds, and the test fails when a fix leaves its mistake. Depends: none.
