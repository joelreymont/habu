---
title: "Inline T0's tag check for a plain ENUM fetch"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T06:43:24.578219+02:00"
---

Problem: src/habu/t0.f:1170-1179 VALIDATE emits a call to NFETCH-CHECK:TAGS before every enum fetch; for a plain ENUM that walks a one-row, no-guard descriptor (src/compiler/native/fetch-check.f:101, :74, :62, :22) to prove 0 <= tag < limit. carl measured it at 11-12% exclusive, 20% inclusive, of a warm KERNEL-BUILD (two 1 ms profiles, ~520 samples each: .jj-ws.noindex/carl-linkcost/.out/kprof1.txt, kprof2.txt); linker.f alone has 49 uses of GOOD?. Acceptance: for a descriptor of one row with no guards VALIDATE emits an inline range compare branching to BAD-TAG; a bad tag still throws the same code; gen 1 = gen 2 byte for byte; the warm-link profile no longer shows NFETCH-CHECK:TAGS for plain enums. Files: src/habu/t0.f. Verify: warm link profile before and after on one host; chain build converges. Depends: lands after the timestamp lanes (habu-decide-what-a-ed28f3a4), which come first. Ownership: src/habu/t0.f. Claim: unassigned.
