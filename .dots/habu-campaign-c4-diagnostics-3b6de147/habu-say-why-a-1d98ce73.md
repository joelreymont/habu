---
title: Say why a type variable is one cell in a mismatch
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T13:31:13.069619+01:00"
---

Problem: for a correct rejection, such as a word `( [ -- t ] -- [ -- t ] )` given `[ -- wide ]`, the record prints the callee's variable by its order of appearance (`[ -- a ]`, src/core/render.f VAR-NAME) and says neither that t is one cell because it stands in a quotation row nor that wide is two cells. Acceptance: the record names the signature's own variable, the position that fixes its width and the width found; a program-diagnostics test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wid.
