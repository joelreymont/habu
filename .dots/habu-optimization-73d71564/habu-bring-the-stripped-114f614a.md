---
title: Bring the stripped tenderd under 500 KB
status: active
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.082560+03:00"
---

Current result: Tender feb55f7d defaults tenderd to stripped on Habu 806f0654 /
engine 274f9bea. An independent private build measures 2,228,416 bytes, of which
2,037,592 are code (91.4%), 143,113 DATA and zero names/dictionary. Aspen's
binary DB/HTTP and MemoryDenyWriteExecute migrate/serve checks pass at this pin.
The 500 KB target is not met. The 16.8 MB compressed server discussed later was
a REPL snapshot of older Tender source, not this deployed image shape;
snapshot compression is held. Child 5b7d02bb now includes a broader DATA
reachability reduction: 64 KiB of wholly unused initialized storage doubles an
empty stripped file while code size is unchanged. Prioritize generated-code
quality and removal of unreachable DATA, not compression of that storage.

Problem: Joel's target (2026-09-22) for Tender's stripped bin/tenderd is under 500 KB; today it is 2,031,808 B = code 1,783,176 / names 0 / data 129,431 / padding 53,817 / other 65,384 (hb-build's own size line, engine 6dc6cd7b, log ~/.cache/tender/verification/stripped-lane/build-server-retry.log; Tender's stripped bin/tender is 1,507,520 B) for about 117K reachable lines of Tender source - about 12 bytes of AArch64 per line. Joel's reading, unproved either way: the compiler emits type checks into machine code, leaves bounds checks in an optimized build, and the closure walker does not tree-shake at word granularity; the three children are the probes that settle each. Acceptance: Tender's python3 scripts/habu.py build --server (from ~/Work/Tender, once habu-refuse-a-stripped-92290c75 makes the image serve) produces a stripped bin/tenderd under 500 KB that passes test/server/tenderd-binary-test.f, hb-build's size line recorded here. Verification: that build command and its size line, the binary test. Ownership: alder (compiler and linker); aspen re-measures on request. Claim: agent=alder workspace=.jj-ws/alder-size-probes (measurement first; no compiler change without a reduced failure).
