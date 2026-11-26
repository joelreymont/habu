---
title: Find where x86-64 loses CPU on load-heavy work
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T13:59:32.662953+01:00"
closed-at: "2026-11-26T14:23:37.032096+01:00"
---

Problem: loading and checking source takes 15-27% more CPU on x86-64 (pulsar) than on arm64 (spark) at master 6c07c237: SAP's test/config-test.f 16.88 s vs 13.25 s user+sys, test/serve-test.f 6.44 vs 5.67 s, although pulsar runs C faster than spark and Habu's small benchmarks run within about 5%. Acceptance: the words whose x86-64 sample share exceeds arm64's on one load workload are named, each with what tier 0 emits there (path:line) and its estimated share of the excess, and each cause has its own fix dot. Depends: none. Claim: unassigned.
