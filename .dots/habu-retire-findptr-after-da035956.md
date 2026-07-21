---
title: Retire FINDPTR after BL reflow
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T06:27:08.996004+02:00"
---

Loose end from the direct-BL landing (1e9a3926): FINDPTR in src/habu/aot-closure.f is now exercised ONLY by the AOT registry security unit test - closure reflow uses the new FINDADDR-PTR (exact code-entry match). Retire FINDPTR and rewrite that unit test against the surviving surface, as its own change. src/ change: CODELEN rows same-commit.
