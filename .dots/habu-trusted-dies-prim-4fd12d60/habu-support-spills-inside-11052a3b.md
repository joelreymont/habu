---
title: Support spills inside quotation function bodies
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:50:23.291688+03:00\""
---

Owner: /root/check_api (Astra), workspace .jj-ws/cedar-quotation-frames. Rowan KEEP review BB 20260911-133339.417-rowan-3be9 found this additional correctness blocker.

Reproducers: `: QB1 ( n n -- n ) [: >r RV:CALLEE r> + ;] execute ;` and `[: 0 ?do RV:CALLEE loop ;] execute` fail E-A64RA-FRAME (-8324) for nonconstant saved values. Old FRAME-ONCE-CK rejects spill slots after function 0; spill-plan rows also use function-local block numbers and can cross sibling functions.

Frozen candidate 5137130d12c0c1c452cda4d9d756b580089c1ba9 on cfcf1baf changes regalloc.f, spill.f, native-quot.f, native-regalloc.f. Rows use module block ordinals; each function invocation has its own frame setup and tokens. Shared module slot offsets are safe because each invocation reserves its frame separately. Exact-source cold build 22.562 s, SHA256 f04cdc9e2ed4581844cb485bb04e7b997afde9b62aa645fdcaa0e1e8d305d21c.

Validation: original and nested runtime frame reducers return 11/3/7/22. native-quot 6.660 s, native-regalloc 19.251 s, native-rstack 6.411 s, order-exit 0.506 s, loop-frame-order 5.973 s, internal-call 7.993 s all pass. Acceptance remaining: independent actual-diff Astra review, integrated matched binary and regression checks. Separate exploratory lexically nested quotation rejects -8651 before allocation; record separately and do not fold into this ownership.
