---
title: Name the ceiling in the data-space refusal
status: active
priority: 2
issue-type: task
created-at: "2026-09-23T10:05:20.891656+03:00"
---

Problem: DP-CHECK (src/habu/habu1.f:1768) routes an out-of-range DP to LDPBAD (src/habu/habu2.f:589), whose line hb: data space out of range names neither the cap (DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE = $2000000 in src/os/linux/layout.f:10), the DP at the refusal nor the definition being compiled, against the capacity-refusal standard habu2.f states above LDPBADMSG (one line naming what filled up, the ceiling and the count). Tender build --server at 246b3e49 hit it after about 29 MB of application data and needed a ten-minute gate run to read (~/.cache/tender/habu-gaps/data-space-bound/report.md). Acceptance: the refusal prints one line naming the cap in bytes, the DP (here - data-base) at the refusal and the definition (LDIAGU/LDIAGDEF are there); rc 76 and the catchable evaluate path unchanged; the mirror bootstrap/cg/forth.fs carries the same shape or states why not; a regression row drives a child engine into the refusal with a bounded allot and pins the numbers (the fixture of dot 4e5c3c2b, the bare-exit predecessor, is the shape). Files: src/habu/habu2.f, src/habu/habu1.f, bootstrap/cg/forth.fs, the fixture. Verify: the fixture on the rebuilt engine, then the chain. Depends: none. Ownership: src/habu/habu1.f src/habu/habu2.f. Claim: agent=hazel workspace=.jj-ws/hazel-dp-cap.
