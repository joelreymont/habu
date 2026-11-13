---
title: test/ files end their waits by a clock, group 4
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.105502+01:00"
closed-at: "2026-11-13T21:29:13.955982+01:00"
---

Problem: test/protection-span.f, require-cap-test.f, seal.f, source-root-exe-test.f, stdlib-standalone-load.f, tmp-path-test.f, underdepth-gate.f, wasm/numeric.f, whole-source-read-test.f and xt-effect-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wT4.
