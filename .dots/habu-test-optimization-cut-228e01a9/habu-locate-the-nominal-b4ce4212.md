---
title: "Locate the nominal pass's missing-terminator stops"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:53:22.194495+03:00"
---

Problem: checking a subject that opens PRODUCT or VALUE-RECORD and never closes it is refused by the nominal pass (tools/check-core.f CHK-PROD-REGISTER, CHK-VREC-REGISTER) with an unlocated line, 'check.f: missing ;PRODUCT' or 'check.f: missing END-VALUE-RECORD', and no JSON record even under --json-errors; a SUMTYPE missing ;SUMTYPE gives two records (the declaration packet, then a statement-throw record). Measured by lane 553 (lexloc part B, $HOME/.cache/tmp/kestrel-jerry-lexlocb/before/summary.txt). Fix: each refusal is one located E-TDECL-SYNTAX record at the opener, in every mode, through the statement-throw record dot 44279554 part B adds. Acceptance: file and stdin cases for PRODUCT, VALUE-RECORD and SUMTYPE, seen failing first, one record each under default, --json-errors and --all-errors. After: 44279554 part B.
