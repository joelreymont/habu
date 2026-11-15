---
title: "Name the fix in the engine's shadow rejection"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T17:48:43.772315+01:00"
closed-at: "2026-11-16T04:07:35.337872+01:00"
---

Problem: the engine's rejection of a bare name that a global and a used package both export (src/habu/interpreter.f SHADOWED: 'bare word a global and a used package both export: NAME') names no fix, while the checker's E-USING-SHADOW-GLOBAL for the same case does. Acceptance: the engine's message gives the place, the source line and the fix the checker's gives, as a test in test/load-reject-diag-test.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
