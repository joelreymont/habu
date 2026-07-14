---
title: "V2 typestate: report render demotion"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T16:17:36.151072+02:00\""
---

Implement sub-dot 5 of the R7 typestate addendum: MODEL-CAD-V2-PLAN.md:1690-1701 (design at 1280-1643). Report demoted to a RENDER of typed evidence; raw REPORT:GATE! write path retired from the public surface. Review folding (BLOCKING): TS-OLD-GATE outcome is PINNED to retire (verdict 1 = word absent/uncheckable, plan:1614-1616); the sub-dot's retype alternative is rejected because it contradicts render-only reports. Acceptance: TS-OLD-GATE regression pin verdict 1; canonical report bytes unchanged for green paths (golden). Verify: typestate-test suite, maki/test.f + report goldens, typed-local-diff-lint. Depends: habu-v2-typestate-promotion-2266b236. Ownership: maki/report.f render surface (disjoint from sub-dot 6 files). Claim: unassigned.

Claim: agent=tsreport workspace=.jj-ws/fable-tsreport
