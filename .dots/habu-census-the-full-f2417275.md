---
title: "Census the full suite's cost"
status: active
priority: 1
issue-type: task
created-at: "2026-11-05T16:53:59.899946+02:00"
---

Joel 2026-11-05: 'why are you running the full suite? why does it take 30 minutes? have you profiled it? is there duplication? are all tests optimally written and load-bearing? do they all reuse a single hb binary?' Measured: last full suite 23:28 wall, 5737 s user CPU; per-test sum 9739 s; slowest 15 tests 61%; native-build-recompile 1403 s is the wall time alone; per-definition build-cache tests (native-file-*, native-build-recompile, *-cache) 18 tests, 3162 s (32%). Brief /tmp/jerry-suite-census/brief.md: per slow test its engine and image builds, children and binaries, cases, machinery-being-replaced and other-machine flags; duplicates across and within tests. Files: /tmp/jerry-suite-census/ only. Verify: the worker's report. Depends: none. Ownership: none (read-only). Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-s1b (read-only).
