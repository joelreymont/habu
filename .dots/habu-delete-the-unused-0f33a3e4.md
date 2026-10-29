---
title: Delete the unused error codes in errors.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.521020+02:00"
---

Problem: no code under src, tools or test uses src/lib/errors.f E-CID-* (-6700..-6708), E-CIE-FORM, E-CRL-* (-6880..-6883) or E-FFI-CALLBACK-FULL (-3506, kept 'for numeric compatibility'); Habu keeps no legacy. Acceptance: the codes and their render rows are gone and nothing under src, tools, test or docs names them. Files: src/lib/errors.f, src/core/render.f if it lists them. Verify: rg -n 'E-CID-|E-CIE-FORM|E-CRL-|E-FFI-CALLBACK-FULL' src tools test docs prints nothing; bin/hb --load tools/check.f -- src/lib/errors.f rc 0. Depends: none. Ownership: src/lib/errors.f. Claim: unassigned.
