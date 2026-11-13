---
title: T0 performance steps
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T20:55:09.934159+02:00"
---

Problem: which load or run cost of T0 (tier 0, the compiler that replaces the JIT) is largest is not known; no step after the first has a target. Joel's order: (1) fetch facts read live, NFETCH:CAPTURE off T0's path (habu-read-fetch-facts-61426492); (2) re-profile load and take the biggest item; (3) run time only where a profile of Etch or the suite shows a hot spot (T0 already carries the JIT's register techniques); (4) floats only if measured (habu-keep-floats-in-c277c8a5); (5) x86-64 and Wasm after arm64 is done. This dot is steps 2 and 3 on arm64, a hunt. Joel's rules for every step: one technique per change; measured before and after on the same corpus with instruction counts (interleaved rounds, medians, output hashes); the bar written in the brief before work starts; outputs identical; each lands alone; no step without a number.
Acceptance: a profile of a load and one of an Etch run or the suite, on arm64 after step 1, report the biggest load item and any run-time hot spot at file:line, each filed as its own dot with its bar.
Files: none changed; the dots filed under habu-run-one-tier-705c2c00.
Verify: the two profiles and the filed dots.
Depends: habu-read-fetch-facts-61426492, habu-delete-the-jit-d9fdaf67.
Ownership: unassigned. Claim: unassigned.
