---
title: README points at archived plans and stale facts
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.022197+02:00"
---

Problem: README.md:269-271 and 356-359 present docs/archive/model-cad.md and cad-plan.md as the design while MODEL-CAD-V2-PLAN.md:9-12 says both were archived 2026-07-18 and README never names the four root plans; :52-53 "165 KB macOS, 144 KB Linux" conflates two artifacts: the unseeded fixpoint engine (Linux measured 147,648 B on 2026-08-23; the macOS row claims 165,367 B) and the delivered bin/hb, which bakes the AOT REPL chain and is 2,076,864 B on this host - both numbers belong in an honest sentence; :349 'bench/' does not exist; :109-131 sm_87/Orin while the Orin was retired 2026-07-19 (docs/eval-triton.md:405) and GB10 sm_121a is the device (docs/bootstrap.md:26-32); :321-323 advertises the broken recovery. docs/positioning.md:3-4 and docs/macho.md:443 repeat the archived-plan and bench/ claims. Acceptance: one Plans list naming the live plan and the archived ones; numbers taken from gate-build-size.f or dropped; bench/ removed; GB10 stated; a stop banner on the recovery until the chain is green. Files: README.md, docs/positioning.md, docs/macho.md. Verify: every path/command in README exists (script the check). Depends: none. Ownership: docs. Claim: unassigned.
