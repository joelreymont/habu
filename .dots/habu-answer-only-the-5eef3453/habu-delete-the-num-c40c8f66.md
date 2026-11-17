---
title: Delete the NUM words no program calls
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T16:20:46.182328+01:00"
closed-at: "2026-11-17T16:55:52.576953+01:00"
---

Problem: most public words of package NUM (src/lib/num-types.f, src/lib/num-arithmetic.f) have no caller outside tests in Habu, Zen, Tender, Maki or Loom, and the roles cell-count, cell-off and alignment have none either. Acceptance: those words and roles are gone with their tests; every word a program calls stays. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-numA.
