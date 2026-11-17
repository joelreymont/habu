---
title: Name each exit status once
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:07:40.700037+01:00"
---

Problem: a status a process exits with is thrown under a name per package: 70 as CHECK-RC (src/core/check-hook.f), CHECKER-REJECT-RC (src/core/rejection.f), INCLUDE-EVAL-RC $46 (src/core/include.f), WRONG-RC (tools/code-bench.f); 76 as FULL-RC (src/core/checker.f), E-INTERNAL (src/lib/argv.f), E-LINT-INTERN-CAP (tools/lint/intern.f) and a bare 76 at src/habu/debug.f:115; 77 as DICT-FULL-RC (src/core/rejection.f); 64 and 74 as USAGE-RC and IO-RC in several tools; 72 as ASM-EXIT-RC and ICODE-EXIT-RC. Acceptance: each status the engine and tools exit with has one name, in package EXIT-STATUS (Joel's name, 2026-11-17) in src/core/exit-status.f, the boot list's first file, which every thrower uses; no file mints its own. Depends: none. Claim: unassigned.
