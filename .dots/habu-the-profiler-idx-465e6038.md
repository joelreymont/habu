---
title: The profiler index keeps a branch for seed-retained spans nothing writes
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:01:32.391751+02:00"
---

Problem: after seed-ndict! went (arm64 in 1d97c3f2, x86-64 in habu-the-x86-64-d5bbcb09) nothing writes RID-OWNER -1 with a nonempty stored span: SNAP writes owner = ordinal with span 0, BUILD,'s live branch owners >= 0, the trim owner -1 with span 0. So src/habu/prof-x64.f BUILD,'s branch for it (:395-400, labels live and entry, comment :382-384 'a seed-retired row uses its stored span') and arm64's twin in src/habu/prof.f C-PROF-INDEX-BUILD (retained, :941, :950-953, comments :929-931, :1286-1287) are dead (seed-ndict review, finding 3). Acceptance: both branches and their comments gone; x86-64 BUILD, tests the owner's sign and goes to next; RID-START/RID-END stores stay (arm64 C-PROF-INDEX-PRUNE and EMIT-PROFRECLAIM read them). Files: src/habu/prof-x64.f, src/habu/prof.f. Verify: engine gate hb1 = hb2, test/prof-index.f, test/x86-64-kernel-prof.f run on an x86-64 peer. Depends: the x86-64 step (habu 3ed6f45d). Ownership: those files. Claim: unassigned.
