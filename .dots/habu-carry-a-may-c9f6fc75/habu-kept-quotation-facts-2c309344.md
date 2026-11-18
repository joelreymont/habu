---
title: Kept quotation facts are checked one way through a pointer or a join
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:08:06.292323+01:00"
closed-at: "2026-11-18T09:22:11.117911+01:00"
---

Problem: src/core/checker.f QUOT-PAIR compares a kept pair's facts covariantly only; a pointee, store slot or join input pair (both role D bits set) takes the union, so ^[ -- renders ] passed a [ -- ] cell's address keeps a renders word unstated, and a join's verdict depends on branch order. Acceptance: those pairs require equal statable facts; review triggers A and B rejected in test/xt-effect-test.f. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9f1.
