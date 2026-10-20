---
title: Make captured build facts location independent
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T16:53:17.758740+03:00"
closed-at: "2026-10-20T18:19:57.904173+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Source-equivalent core builds4384 and11d0 have identical captured code/maps but unequal DATA: SOURCE-PRODUCER checker digest includes checkout paths, and2265 CHECKER-REG record-link ADDR fixed-value fields persist live donor-window addresses. Same-source cold/warm and five generations are equal; cross-tree equality is not.

Freeze the checker role from the existing ordered relative-path compiler stream, preserving domains, framing, bytes, options and freeze order. Keep CURRENT$ and its actual paths and roots unchanged. Canonical shipped record-link rows retain FIXED-KIND and scalar values, while the unused ADDR fixed value is zero; live address reads still use their owning dictionary record. Preserve live publication, transfers and capture cleanup.

Keep this task active until the optional native-build-edit-e2e source-copy case proves exact engine and map equality from separate cold caches and both products run their typed DATA contract. Qualify the existing restored map-seed address read/write and refusal checks, native suite and five-generation convergence on the matching rebuilt engine, with independent Astra review. Do not normalize comparison bytes or add restore machinery.

Implementation ad50d243 has independent Astra source review clearance. Expanded map fixture, driver loads, input/root/path-shadow checks pass. Reviewed0a9e35df corrects obsolete whole-file recompilation expectations while retaining every runnable output and freshness check; its unchanged E2E passes on the3868 donor. Cross-root results follow; final matching native and convergence remain required.

Two actual cold source-copy builds complete and compare exactly: executable SHA25683eeb04c79585ce77539770d324e48c76e6de568c324114d2c30f5cea0493e3c, map ea9e58ef02ec575b6095baa35bdc6ee99506b52815e76cda18f9ee1052187186; both cmp exit0. The test command exits1 only at its two existing typed-output checks, which expected spaces although dot prints LF. Both saved products independently exit0 with exact typed=41 LF42 LF42 LF LF. Reviewed3afa3de1 corrects that literal without weakening values or any other check; Habu decodes its16 expected bytes and both output comparisons pass. No cold builds repeated just for formatting. Combined final native/convergence qualification remains required.
