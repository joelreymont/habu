---
title: Lower native return stack
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-26T22:59:20.040248+02:00\\\"\""
closed-at: "2026-08-13T18:44:47.532089+02:00"
close-reason: "The erase design landed in full across 9f24f274 (EFFECT-RET-NEUTRAL?), fd25dc9d (R1 refusal consumer in dict.f + PRIM axiom) and 8db52e9e (join threading + differential suite): >r/r>/r@ are compile-time transfers, zero instructions, RSP-CELL never touched by chain code; checker return row proven the sole and sufficient authority; census unmodeled 40 to 24, compiled 3284 to 3297."
blocks:
  - habu-lower-native-typed-01db198b
---

Full context: design Wave 4 adds >R, R>, and R@ as explicit typed return-stack state, not hidden machine stack behavior. Acceptance: depth/type/ownership across branches and calls validates; underflow, leak, wrong role, and exception-unwind mutations reject.

Claim: agent=rstack workspace=.jj-ws/habu-rstack-design
