---
title: DERIVE eq dies at a fixed 4 KiB render buffer
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.835785+02:00"
---

Problem: src/core/sumtype.f:1064 TDGEN-CAP $1000 and :1329-1330 '76 die' when TDGEN-EQ-DIAG (1793-1810) renders V*V arms (~25-35 bytes per inner arm + ~45 per outer): about 12 single-payload variants with 6-byte names exceed it and the process exits mid-declaration, bypassing TDECL-RUN's rollback (138-146). Every other plan buffer in the file grows (TDPLAN-ENSURE 1157). Acceptance: TDGEN-BUF grows through ARENA-BYTES-GROW like TDPLAN, or throws E-TDECL-CAP so the declaration rolls back with a diagnostic; a 16-variant DERIVE eq fixture passes. Files: src/core/sumtype.f. Verify: the fixture under bin/hb. Depends: none. Ownership: declaration generator. Claim: unassigned.
