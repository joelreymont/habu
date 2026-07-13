---
title: "Eval: real generation-token count + collective/attention tasks"
status: open
priority: 3
issue-type: task
created-at: "\"2026-06-27T12:37:56.243001+02:00\""
---

tokens-to-green (maki/eval-repair.f) is a whitespace source-token proxy; wire a model-token count. Autograder now covers SAXPY + softmax (maki/eval-device.f, eval-device-sm.f); extend to collective/2D/attention authoring tasks as those kernels land on device.

## Parked 2026-07-13 (session limit, BLOCK on review)
Worker (liveeval) committed 4 commits (tip c1744541) in .jj-ws/fable-liveeval:
EVAL:GEN-TOK-EST model-token estimate wired through the matrix; 3 off-device
authoring tasks (sumnorm/gemm/attention); a live 15/15 pass@1 round. Destruction
review verdict BLOCK: the autograders grade SIX semantically-wrong kernels
GREEN(2) (in/out swap, out=in/sum^2, dead-store no-op, double MM-K-LOOP, Q/K
swap, O-into-V) because the structural gates test only instruction
presence/absence; commit/doc claims overstate what they catch. Fix round was
dispatched then died at the session limit with nothing committed. DO NOT MERGE
as-is. Resume: pin the 6 wrong-but-green shapes as acknowledged regressions,
scope the overclaims (eval-emit-test.f:4, docs/eval-triton.md:753/724), harden
EE-EMIT to distinguish infra failure from candidate rejection. Claim released.
