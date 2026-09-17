---
title: Merge captured DATA runs to cut the row overhead
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:39:26.144753+03:00"
---

Problem: tools/engine-size.f on the line-head engine (5112000 bytes, 2026-09-17) reports aot/data-run-rows 682320 bytes (13.3 percent): 85290 runs at 8 bytes a row. The heap owner alone is 58035 runs for 1241138 bytes (image cost 1705418) and SYMS-BOOT is 26670 runs for 113678 bytes (image cost 327038), so the rows cost more than a quarter of what they describe. A row costs 8 bytes, so two runs separated by fewer than 8 zero bytes cost more as two rows than as one run carrying the gap. Acceptance: the capture merges adjacent runs whose gap is below a named threshold chosen by measurement (rows plus bytes minimised), the loader zero-fills nothing new because the merged run carries the gap bytes, the payload version and src/habu/aot-file.f stay consistent, the engine shrinks by the measured amount (expect several hundred KB), byte fixpoint converges, test/run.f passes, and tools/bootstrap.sh check stays green (if the seed reads payloads the change is two-stage). Files: src/habu/aot-capture.f (run capture), src/habu/aot-file.f, the payload loader in src/habu/habu2.f, tools/engine-size.f. Verify: engine-size before and after; fixpoint; test/run.f; bootstrap check. Depends: none. Ownership: AOT capture. Claim: unassigned.
