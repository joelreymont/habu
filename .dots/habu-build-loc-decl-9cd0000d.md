---
title: Build LOC-DECL-ROW from the expanded row on refusal
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.848297+02:00"
---

Problem: on its refusal path LOC-DECL-ROW (src/core/checker.f) is built from the unexpanded 'pre' row (stuart, hbind fix, 2026-11-07). Conservative: an untyped local over such a cell is counted unknown, never given a wrong width; a later diagnostic may name the wrong width. Acceptance: the row is built from the expanded terms, or the comment states why the unexpanded row is right; a refused program's message checked. Files: src/core/checker.f. Verify: test/wide-typed-local-probe.f. Depends: none. Ownership: carl. Claim: unassigned.
