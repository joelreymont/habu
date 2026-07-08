---
title: "error-code-lint: range-aware reservation enforcement"
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T21:17:57.887531+02:00"
---

tools/error-code-lint.f enforces one owner per claimed negative E- code, but E-*-FIRST/E-*-LAST range sentinels (lib/errors.f blocks) are only excluded from claims, not modeled as reservations: a foreign file minting a code INSIDE another subsystem's declared FIRST..LAST range is not flagged until the owning block mints that exact member. Extend the core to parse FIRST/LAST pairs as [first,last] reservations and flag any claim inside a foreign reservation (same allowances as today). Add fixtures to tools/error-code-lint-test.f: foreign claim inside a reserved range flagged; the owning block's own members pass.
