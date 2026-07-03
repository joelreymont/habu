---
title: Re-measure + set Linux AOT-REPL size baseline (zed phase)
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T15:38:27.987202+02:00"
---

test/gate-build-size.f GB-SIZE-BASELINE-LINUX=90304 is STALE after the AOT-REPL M2 seed changes (DATA+CODE literal reloc passes, EM-AOT-RELOC-DATA/CODE, and the real-REPL AOT blob). The macOS baseline was re-measured and raised (99319->115831 fixture stage, will change again after real-REPL wiring). Linux cannot be built/measured on the macOS dev host. On a Linux build: run the owning gate slice (candidate build+validate via test/gate-engine-lib.f -> test/gate-build-size.f), read the measured bin/hb size, and set GB-SIZE-BASELINE-LINUX to the exact measured value in the same commit. The ratchet is exact-match (grew=fail, shrank=STALE-BASELINE), so it must equal the measured Linux artifact byte count.
