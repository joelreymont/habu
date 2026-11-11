---
title: "x86-64 ndict! and cp! lack the profiler's sync and reclaim"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:01:32.403460+02:00"
---

Problem: x86-64's ndict! lowering (src/habu/kernel-x64.f OCC-COUNT :3196-3225) does not replay deferred profiler samples before the records go; arm64's calls LPROFTRIM (habu1.f:5949, prof.f EMIT-PROFTRIM), tested by test/prof-rewind-child.f. x86-64's cp! (DEF-CODE-RECLAIM-BODY :3999-4003) has no twin of arm64's LPROFRECLAIM (habu1.f:3914) or of the index prune. Both predate the seed-ndict deletion (its review, finding 5). Acceptance: both twins on x86-64 with kernel-prof cases; one more case, lost with REWIND-CASE: a word on a reused ordinal whose package row also reused a trimmed ordinal is named with the new package's qualifier (finding 4). Files: src/habu/kernel-x64.f, src/habu/prof-x64.f, test/x86-64-kernel-prof.f. Verify: test/x86-64-kernel-prof.f run on an x86-64 peer. Depends: the x86-64 step (habu 3ed6f45d). Ownership: those files. Claim: unassigned.
