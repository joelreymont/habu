---
title: Make TDECL-PRODUCT refuse an unclosed body through TDECL-DEFPRODUCT-NOEND
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.026336+02:00"
---

Problem: src/core/sumtype.f TDECL-PRODUCT (:2471) refuses an unclosed body by repeating TDECL-DEFPRODUCT-NOEND's steps (:2466: TDECL-CTX!, then TDECL-RUN of TDECL-PRODUCT-NOEND-BODY) instead of calling it, while TDECL-SUMTYPE (:2434) refuses through TDECL-DEFSUM-NOEND, the entry the pre-pass also uses: two paths own one refusal. Probe: ~/.cache/tmp/rv-ppopen/p/p3-two.f. Acceptance: --load and the pre-pass refuse an unclosed PRODUCT through the one entry, same code, reason and place; TDECL-PRODUCT keeps no copy of that entry's steps. Depends: none. Claim: unassigned.
