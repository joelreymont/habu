---
title: Charge a kept quotation at every keeping
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:24:26.542601+01:00"
closed-at: "2026-11-18T11:24:41.940391+01:00"
---

Problem: src/core/checker.f QUOT-PAIR writes the facts a kept quotation is charged with into it only on a failure (:4856), so in `[: RUN ;] dup SV ! XN !` (SV `[ [ -- renders ] -- renders ]`, XN `[ [ -- renders ] -- ]`) the first keep ties the literal's input to a kept slot and the second keep no longer charges it; alone, `[: RUN ;] XN !` is rejected. Acceptance: the pair is rejected at `XN !`. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nC.
