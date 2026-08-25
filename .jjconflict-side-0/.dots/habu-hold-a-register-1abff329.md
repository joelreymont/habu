---
title: "Hold a register-convention emission's second function to its own places"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T13:05:45.628149+02:00"
---

select.f FUN-PLACES! and regalloc-verify.f FUN-PLACES derive per-function place lists only under the data-stack convention; under a register convention the contract's own lists stand for every function, so a non-empty register place list over a multi-function module would measure the second function's boundary against the first's places. Unreachable today: abi.f is the only production contract writer and declares the data-stack convention for every routine, and every register-convention fixture passes SEQ-NONE (measured, quot core lane 2026-08-11). Needs a per-function register place declaration, or a refusal, BEFORE anything emits a register-convention module of more than one function - which the post-cut register-convention lane (da01bd62) will do, so order this ahead of it. Files: src/compiler/native/{select,regalloc-verify}.f. Depends: none (blocks da01bd62's multi-function case).
