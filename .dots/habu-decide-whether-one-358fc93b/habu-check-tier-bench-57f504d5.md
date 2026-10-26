---
title: Check tier-bench results outside the timing
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T11:28:18.206816+02:00"
closed-at: "2026-10-26T11:57:12.367037+02:00"
close-reason: Check tier benchmark results across tiers
---

tools/tier-bench.f writes SINK but never reads it (tier-bench.f:126,186-220): a miscompiled kernel times as fast. Assert each kernel's result after the timed region so measurements of the tier 0 steps are trustworthy. Experiment order (Joel 2026-10-26): fixes first.

Lane: jerry, worker in .jj-ws.noindex/jerry-bench; handoff ~/.cache/tmp/jerry-bench-handoff.md. Design: tier-bench prints an R line per benchmark (SINK after the timed window, checked equal across rounds); test/compile-floor-gate.f requires tier 0 and tier 1 R values to agree. Claim: jerry.

Done: tier-bench records each result after its timed window, dies if a round differs, prints R lines; fold/move results are position-weighted sums of the whole output, search returns where a tail-unique needle matched; test/compile-floor-gate.f requires tier 0 and tier 1 R values to agree and shows the child stderr. Medians unchanged.
