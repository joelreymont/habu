---
title: "Delete the constructor gate's re-checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.335944+01:00"
closed-at: "2026-11-16T04:07:36.370460+01:00"
---

Problem: src/core/generated-declaration.f:399-402 E-CTOR-ARM, :421 GENERATED? and :426-428 GEN-REQUIRE re-check at run time, three times per declaration, what the front ends already rejected. Acceptance: they and GENERATED-DECL-CTOR:PART-PREPARE are gone, each shown unreachable from source. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
