---
title: DERIVE eq dies at a fixed 4 KiB render buffer
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.835785+02:00"
---

Problem: src/core/sumtype.f TDGEN-CAP :1106 ($4000) and :1355 '76 die' when TDGEN-EQ-DIAG (:1762) renders V*V arms (~25-35 bytes per inner arm + ~45 per outer): enough single-payload variants exceed it and the process exits mid-declaration, bypassing TDECL-RUN's rollback (:185). Every other plan buffer in the file grows (TDPLAN-ENSURE :1164). Acceptance: TDGEN-BUF grows through ARENA-BYTES-GROW like TDPLAN, or throws E-TDECL-CAP so the declaration rolls back with a diagnostic; a 16-variant DERIVE eq fixture passes. Files: src/core/sumtype.f. Verify: the fixture under bin/hb. Depends: none. Ownership: declaration generator. Claim: unassigned.

`ENUM shape 0 DERIVE eq` with enough single-field `n` payload variants dies with `sumtype: generated constructor too long`, exit 76 (TDGEN-C,), and the declaration does not roll back.
