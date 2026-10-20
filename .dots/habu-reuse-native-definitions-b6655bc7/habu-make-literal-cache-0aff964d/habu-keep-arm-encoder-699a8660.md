---
title: Keep ARM encoder operands local
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:28:25.839358+03:00"
closed-at: "2026-10-20T18:19:57.984237+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Both split/merge native-file-auto segment images differ solely in completed A64ASM operands plus separately tracked BGLUE-I. Warm versus fresh ARM-BASE F8000400/F8400400,RD0/30,RN19/31,IMM8/16 represent last STRPOST/LDRPOST encoder calls; all relevant private reads initialize operands within the call. Remove the eight ARM operand scratch globals and helper-only stores using invocation locals, retaining exact public operand/effect contracts, validation order/codes and encoded bits. Preserve existing local RRR/RRI/runtime reader boundaries. Existing segment equality is the regression; source encoder success/refusal tests, matching native byte equality/fullsuite/convergence and independent review. No per-instruction scrub, registry or changed opcode.
