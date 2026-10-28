---
title: Make REPL twin builds identical again
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T18:34:09.534428+02:00"
---

Problem: at master 7ae17f60 with the stage engine (aeb74d3b), tools/hb-build-repl-twin-test.f fails asserts 4, 5 (tier 1) and 20, 21 (tier 0): HBT-TWIN-SAME (:169-175) finds the --repl images A (warm cache), B (fresh cache) and D (other output name) differ, first byte at ~9.89M or ~14.17M. Last fixed by closed dot habu-fix-the-repl-897178b0 (2026-10-23); test history since: c4054574, 436bbf42, 25e30329, affdbf58, 1dc1f348. The full suite's only red row on the key lane, and the same at its master base. Acceptance: root cause named (which cell or section differs and which writer puts a process- or cache-dependent value there), fixed at that writer, hb-build-repl-twin passes at both tiers; no test change unless the test is wrong. Files: by root cause. Verify: bin/hb --load tools/hb-build-repl-twin-test.f (HABU_RETAIN_TWIN=1 keeps the images), the other hb-build-repl-* rows. Depends: none. Ownership: worker in .jj-ws.noindex/carl-twin. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-twin.
