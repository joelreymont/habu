---
title: Remove bounds checks from an optimized build
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.087679+03:00"
---

Problem: Joel (2026-09-22): 'bounds checking should be removed during optimization, unless we are in debug mode' - unproved either way. Probe: a stripped program with a SPAN:U8! loop over a span, objdump -d, the compare-and-branch pairs per access counted; then the same under whatever release or no-check build mode exists - if none exists, that absence is the finding. Acceptance: the per-access check count recorded with the engine sha for both modes, and either a release mode that drops the checks or the reason one cannot exist yet; Tender's bin/tenderd re-measured by hb-build's size line. Verification: the images and disassembly under ~/.cache/tender/habu-gaps/bounds-checks-release/, then Tender's python3 scripts/habu.py build --server. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-size-probes.
