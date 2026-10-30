---
title: Say why tier 1 refuses an unchecked defer or does>
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T23:10:21.192152+02:00"
---

Measured on the public hb (master) and the T0 stack candidate alike, at 1 set-tier under 0 set-check: `defer UC-D` exits 76 printing only `UC-D at <path>:3`; `: M create , does> @ ;` exits 76 printing only `does> at <path>:3`. The line names no reason and no fix. Probes: /tmp/jerry-lead-tmp/sidef/d.f, m.f. Found by lane nfrtest2. Acceptance: each refusal names its rule (the missing signature) as the other definer refusals do; a rejected-program test per case.
