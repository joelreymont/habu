---
title: Say why a type variable is one cell in a mismatch
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:31:13.069619+01:00"
---

Problem: a one-cell rejection names the callee's variable by its order of appearance (src/core/render.f VAR-NAME), not as the signature writes it: test/program-diagnostic-one-cell.f's `: AP ( [ -- t ] -- [ -- t ] ) ;` given `[ -- pr ]` prints "`a` in `ap`'s effect is one cell" and expected `[ -- a ]`. Acceptance: the record names the signature's own variable (`t`); a program-diagnostics test fails first. Depends: none. Claim: unassigned.
