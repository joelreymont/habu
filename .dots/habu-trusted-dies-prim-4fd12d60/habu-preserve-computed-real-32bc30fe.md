---
title: Preserve computed real values across calls
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:50:23.007484+03:00\""
---

Owner: cedar (/root), workspace .jj-ws/cedar-real-call on cfcf1baf. Rowan KEEP review BB 20260911-133339.417-rowan-3be9 found this additional correctness blocker.

Reproducer: with RV:CALLEE ( n -- n ), `: PR3 ( r n -- n r ) swap 2.0 f* >r RV:CALLEE r> ;` fails E-NELAB-TYPE (-8580). Computed REAL values on the hidden return stack are checked by NO-REAL-CK even though KEEP candidate 931a543c removed hidden values from call operands. Input-real-only coverage did not expose this.

Responsible files: src/compiler/native/elaborate.f and test/compiler/native-rstack.f. Inspect the sole CALL-OPERANDS+ guard and preserve typed hidden SSA through calls; do not weaken checks on actual call operands. Acceptance: matched rebuilt binary passes direct and indirect calls with computed real values, preserves output ordering and exact values, and passes native-rstack plus relevant real/call neighbors. Independent Astra review before integration. Status: root implementation starting; no fix claimed.
