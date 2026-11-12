---
title: Add an exact floating remainder to FMATH
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:50:48.063814+02:00"
---

Problem (Maki, bob 2026-11-12): Maki's revolute mate reduces a turn to one revolution with its own exact loop (maki .jj-ws.noindex/bob-turn src/assembly.f:2291-2306, TOP-STEP and MOD-360), and Maki puts generic maths in Habu. Acceptance: a public FMATH word ( r r -- r ) giving x - n*y exactly, n = trunc(x/y), the result carrying x's sign; a nonfinite input or a zero y is E-DOMAIN, as every FMATH word but the IEEE wrappers refuses one (fmath.f:22-29); fmath-test.f covers exact cases near multiples, huge quotients, subnormals and signs; on master and installed. Name: Joel's. Depends: none. Claim: unassigned.
