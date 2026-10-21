---
title: Define sum types while checking is off
status: open
priority: 2
issue-type: task
created-at: "2026-10-21T16:38:36.063160+03:00"
---

A sum-type declaration loaded after `0 set-check` dies 76 "checker: incomplete generated constructor plan" (src/core/sumtype.f TDECL-GEN-EVAL-BODY :1905 CTOR-PEND-REQUIRE-DONE): the preflight consumes the plan through CHECK!, but the generated constructor definitions are evaluated with the hook suspended, so the evaluator never consumes it. Reproducer: a file containing `0 set-check` then `require src/lib/process.f` (rc 76 on the installed engine and on master). Decide at the responsible layer: either the generated declaration text runs under the suspended hook's checker (constructors keep their effects), or a declaration under a suspended hook is refused by name; never an internal 76. Acceptance: the reproducer's outcome is the chosen one through the real load path, with an E2E row.
