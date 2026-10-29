---
title: T0 performance steps
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T20:55:09.934159+02:00"
---

After the JIT is gone. Rules (Joel 2026-10-29): one technique per change; measured before and after on the same corpus with instruction counts (/tmp/jerry-meas/im.sh: interleaved rounds, medians, output hashes); the bar written in the brief before work starts; outputs identical; each lands alone; no step without a number. Order: (1) S2B-2: fetch facts read live, NFETCH:CAPTURE off T0's path (load time, drops a tier 1 dependency); (2) re-profile load and take the biggest item; (3) run time only where a profile of Etch or the suite shows a hot spot (T0 already carries the JIT's register techniques); (4) floats only if measured; (5) x86-64 and Wasm after arm64 is done.
