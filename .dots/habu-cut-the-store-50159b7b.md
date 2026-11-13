---
title: "Cut the store guard's cost under JSON reads"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:33:39.161838+02:00"
---

Problem: measured by the JSON:VALUE= hunt (profile of 46,782 samples, installed hb from master c128ff07, tier not determined): every '!' calls the store guard (PROT-SPAN), 7.4% of samples, under every JSON read; src/habu/habu1.f BSTORE (:2129-2131) emits the guard call (PROT-GUARD:CALL) before each store. The stores a JSON read makes are mostly to computed addresses (the reader's cursor fields), which habu-t0-skip-the-4da2e9e2 (jerry's: skip the guard for a constant address outside every protected band) does not cover. Which tier compiled the profiled code is not known, and how a guarded store to a computed address gets cheaper (an inline band check, or a proof the address is outside every band) is a design nobody has made. Acceptance: the tier that compiled the profiled stores is named, and a design for the guard on computed addresses is decided, scoped against 4da2e9e2 (what it leaves to that dot, what it adds). Files: none until the design; then that tier's store emission. Verify: the hunt's JSON read profiled before and after, the PROT-SPAN share measured. Depends: none. Ownership: unassigned. Claim: unassigned.
