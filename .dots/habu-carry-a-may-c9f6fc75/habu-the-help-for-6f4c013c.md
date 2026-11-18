---
title: The help for a quotation compile, keeps cannot be followed
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:08:06.326393+01:00"
closed-at: "2026-11-18T09:22:11.133876+01:00"
---

Problem: src/core/render.f FACT-HELP tells "['] V compile," with V parsing to state parses in the type that keeps it, which is compile,'s own primitive row. Acceptance: the help names both fixes, stating the fact in the type that keeps it or keeping a word without it; tested in test/xt-effect-test.f. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9f6.
