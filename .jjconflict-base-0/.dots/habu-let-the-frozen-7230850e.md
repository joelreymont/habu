---
title: Let the frozen declaration door fold its keys
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:54:02.165034+02:00"
---

hir-word.f SYM-CK (the frozen-module declaration door) is handed symbol ROWS without the byte pool, so it cannot fold; a row declared there under a non-canonical spelling is unreachable by any query (fail-closed, pinned by STATED-KEY-CASE, merged ce6488f7). Only tests use that door today. Fix: give those three declarers access to the pool so they key through the same fold as the builder side - one rule, no second door. Acceptance: a frozen-door row declared in capitals is reachable; STATED-KEY-CASE inverts to a positive. Files: src/compiler/native/hir-word.f. Depends: none.
