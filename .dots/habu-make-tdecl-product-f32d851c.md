---
title: Make TDECL-PRODUCT refuse an unclosed body through TDECL-DEFPRODUCT-NOEND
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.026336+02:00"
---

Problem: src/core/sumtype.f TDECL-PRODUCT builds its own 'missing ;PRODUCT' packet while the pre-pass (verify-source.f RECORD-PRODUCT) goes through the new public TDECL-DEFPRODUCT-NOEND / CHECKER-DEFPRODUCT-NOEND (lane carl-pp1, change tsykpwvu): two paths own one refusal, as SUMTYPE has one (TDECL-DEFSUM-NOEND serves both). Acceptance: --load and the pre-pass refuse an unclosed PRODUCT through the one entry, same code, reason and place; the duplicate packet code in TDECL-PRODUCT is gone. Files: src/core/sumtype.f. Verify: an unclosed PRODUCT under --load and under tools/check.f --verify-only --json-errors give the same packet (probe ~/.cache/tmp/rv-ppopen/p/p3-two.f). Depends: carl-pp1 landed. Ownership: sumtype.f. Claim: unassigned.
