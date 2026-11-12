---
title: Delete the effect readers nothing calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:27:55.189900+02:00"
---

Problem: CHECKER-OWNER:DIN-CON, DOUT-CON and STACK-STABLE? (src/compiler/native/checker-owner.f) have no caller, and so neither have the rows EFFECT-DIN-CON-OFF, EFFECT-DOUT-CON-OFF and EFFECT-STACK-STABLE-OFF (src/core/checker-owner-abi.f, src/habu/layout.f DECL-EFFECT-DIN-CON-OFF, DECL-EFFECT-DOUT-CON-OFF, DECL-EFFECT-STACK-STABLE-OFF) nor src/core/checker.f EFFECT-DIN-CON, EFFECT-DOUT-CON and EFFECT-STACK-STABLE? with their installs. Acceptance: each is deleted with what only it reaches; the engine builds twice and the suite passes. Depends: none. Claim: unassigned.
