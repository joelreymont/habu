---
title: Bring the stripped tenderd under 500 KB
status: active
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.082560+03:00"
---

Problem: Joel's target (2026-09-22) for Tender's stripped bin/tenderd is under 500 KB; today it is 2,031,808 B = code 1,783,176 / names 0 / data 129,431 / padding 53,817 / other 65,384 (hb-build's own size line, engine 6dc6cd7b, log ~/.cache/tender/verification/stripped-lane/build-server-retry.log; Tender's stripped bin/tender is 1,507,520 B) for about 117K reachable lines of Tender source - about 12 bytes of AArch64 per line. Joel's reading, unproved either way: the compiler emits type checks into machine code, leaves bounds checks in an optimized build, and the closure walker does not tree-shake at word granularity; the three children are the probes that settle each. Acceptance: Tender's python3 scripts/habu.py build --server (from ~/Work/Tender, once habu-refuse-a-stripped-92290c75 makes the image serve) produces a stripped bin/tenderd under 500 KB that passes test/server/tenderd-binary-test.f, hb-build's size line recorded here. Verification: that build command and its size line, the binary test. Ownership: alder (compiler and linker); aspen re-measures on request. Claim: agent=alder workspace=.jj-ws/alder-size-probes (measurement first; no compiler change without a reduced failure).
