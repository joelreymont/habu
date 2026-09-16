---
title: Extend the repair contract to runtime failures
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:54.716300+03:00"
---

Problem: docs/repair-diagnostics.md covers checker diagnostics only; a throw, die, stack-bounds trap or foreign-call failure at load or run time prints free text and an exit code, so a repair loop cannot act on it. Acceptance: every engine and runtime failure path emits one newline-terminated JSON diagnostic on stderr with the same schema fields (code, repair_class, verdict, word, token or site, file, line, column, suggestion) before exiting or throwing to the REPL; the human-readable line is derived from it, not separate; tools/gate-json-assert.f diag-contract is extended to runtime fixtures; docs/repair-diagnostics.md documents the runtime verdict. Files: src/habu/habu2.f fd2 emission, src/core/render.f, lib/errors.f, tools/gate-json-assert.f, test/gate-diagnostics.f, docs/repair-diagnostics.md. Verify: fixtures for throw, die, stack bounds and FFI failure; test/run.f green. Depends: habu-compose-every-fd-ffa78ad1. Ownership: engine lane. Claim: unassigned.
