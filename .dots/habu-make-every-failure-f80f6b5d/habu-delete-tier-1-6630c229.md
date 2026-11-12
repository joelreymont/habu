---
title: "Delete tier 1's target check that cannot fire"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.154252+02:00"
---

Problem: src/compiler/native/compiler.f:29 'HB-TARGET-KNOWN? 0= [if] E-CTGT-ABI throw [then]' never fires, since HB-TARGET-KNOWN? is true on every seam (src/os/*/target.f), and E-CTGT-ABI (src/core/error.f:303, 'the ABI is not one the architecture can run') does not describe it. Acceptance: the line and its comment at :26-27 are gone and compiler.f still loads on every seam it loads on today; the chain build reaches its fixpoint and the suite passes. Depends: none. Claim: unassigned.
