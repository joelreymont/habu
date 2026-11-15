---
title: Share one source builder among the tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:14:01.622868+02:00"
---

Problem: eight test files (test/signature-nesting.f, test/diag-json-escape.f, test/debugger-resume.f, test/gate-dictionary-lib.f, test/interpreter-number.f, test/gate-debug-lib.f, test/gate-common-lib.f, test/memory-fault.f) each define their own buffer and words that build source text for TEST-EVAL (src/lib/test/eval.f). Acceptance: one builder beside TEST-EVAL, and every such test uses it. Depends: none. Claim: unassigned.
