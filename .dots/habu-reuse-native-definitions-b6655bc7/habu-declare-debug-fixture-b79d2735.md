---
title: Declare debug fixture token storage
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T14:15:18.089884+03:00"
closed-at: "2026-10-19T14:31:36.965939+03:00"
close-reason: Typed fixture cells and exact private token-to-numeric-code-address casts preserve the original atomic-cas target, refusal -9300, complete table/code comparison and native resume output. Matching native test/gate-debug.f passedexit0; independent Astra reviewclear. No production boundary change or weakened assertion.
---

Matching21b4c68d existing test/gate-debug.f passes all profiler cases then fails unsupported-breakpoint fixture before its assertions: top-row constant typed effect mismatch. GDB-BREAKPOINT-REFUSAL renders ticked VALID and atomic-cas into raw constants, contrary to the adopted typed storage rule. Use declared token storage preserving the exact original atomic-cas target and breakpoint refusal/table/code artifact assertions; do not wrap the target or weaken refusal. Inspect all generated fixture token/address stores for the same downstream path. Existing failing gate is the pre-code regression; narrow native rerun and independent review required.
