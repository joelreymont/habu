---
title: "TRUSTED: effects accept fact words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:08:06.314498+01:00"
closed-at: "2026-11-18T09:22:11.148965+01:00"
---

Problem: src/core/checker.f TRUST-DECL-ROW lets every row's outputs end in fact words, so 'TRUSTED: TX ( -- renders ) ;' loads; only a defer's effect may state facts. Acceptance: TRUSTED: rejects fact words live and under --verify-only, defers keep them; tested in test/xt-effect-test.f. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9f3.
