---
title: Preserve computed real values across calls
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T16:50:23.007484+03:00\\\""
closed-at: "2026-09-11T17:21:55.726564+03:00"
close-reason: Reviewed b817c42f integrated as92ef13f0; combined c0bd71d4 explicit-tier1 native-rstack, native-quot and native-regalloc pass, including computed real direct/local/indirect regressions. Full compiler selfbuild tracked separately.
---

Owner: cedar (/root), workspace .jj-ws/cedar-real-call on cfcf1baf. Rowan KEEP review BB 20260911-133339.417-rowan-3be9 found this additional correctness blocker.

Reproducer: with RV:CALLEE ( n -- n ), `: PR3 ( r n -- n r ) swap 2.0 f* >r RV:CALLEE r> ;` fails E-NELAB-TYPE (-8580). Computed REAL values on the hidden return stack are checked by NO-REAL-CK even though KEEP candidate 931a543c removed hidden values from call operands. Input-real-only coverage did not expose this.

Responsible files: src/compiler/native/elaborate.f and test/compiler/native-rstack.f. Inspect the sole CALL-OPERANDS+ guard and preserve typed hidden SSA through calls; do not weaken checks on actual call operands. Acceptance: matched rebuilt binary passes direct and indirect calls with computed real values, preserves output ordering and exact values, and passes native-rstack plus relevant real/call neighbors. Independent Astra review before integration. Status: root implementation starting; no fix claimed.


Update 2026-09-11 13:59 UTC: Frozen fix b817c42f, independently cleared by compiler_xhigh_review and integrated as 92ef13f0. NO-REAL-CK now checks only actual VN call operands; hidden REAL SSA keeps FPR clobber and typed spill/reload checks. Old cfc binary fails the new regression at -8580; rebuilt ed383f57f4bfba1f3567bffd76ae4789fbe648d89753c33feb9208184201a863 passes native-rstack (6.377 s), native-tail (6.078 s), native-exec (7.032 s). Direct, local and indirect computed values checked. Combined compiler selfbuild/full validation still pending.
