---
title: "Make the checker's global internals private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.889423+01:00"
---

Problem: checker words and cells checked code has no use for are global or public: on the installed hb `' NAME drop` runs, rc 0, for LOAN-FIELD-FAM C2-READ-FAM C2-MUT-FAM C2-INIT-FAM C2-RECORDS-FAM C2-STOW-FAM CHECKER-WRITE-WINDOW! WITH-TICK-ORDER CHECKER-VERIFY-FILE CHECKER-ROWS-END TRUST-DECL TRUST-RAW NOMINAL-PROVIDER CHECKER-VERIFY-PKG-START CHECKER-VERIFY-PKG-DONE EXPORT-RESOLVE EXPORT-EFF-INST EXPORT-RECORD EXPORT-META-COPY EXPORT-CARRIES? DL-TAKE-TAIL CHECKER-EXPORT-ROW CHECKER-EXPORT CHECKER-EXPORT-DECL (src/core/checker.f), TYPE-DECL:TDECL-FAM-REG :TDECL-TXN-XT :TDECL-TXN-ARMED :TDECL-NAME-PREFLIGHT-XT :TDECL-CAPACITY-PREFLIGHT-XT :TDPLAN-N (src/core/sumtype.f), SCHEMA-REG:SCH-CAP-V :SCH-ROOT-CAP-V :SCH-N :SCH-ROOT-N :SCH-RBF-DEPTH (src/core/type-schema.f), and the public TFAM cells TF-STR-U TF-PK-N TFAM-N TFX-HI SUMV-N SVX-HI VNX-HI PF-N PF-COMMIT-N PF-TX-CAP-V PF-TX-P PF-TX-DEPTH PF-TX-SERIAL LAY-N TF-RBF-DEPTH, which take a store: `99 TFAM:PF-COMMIT-N !` runs, rc 0, at top level and in a checked colon body. A checked body can also rebind a global checker defer and so change what the checker does: `: P ( -- ) [: drop 0 ;] is TFAM-ARITY-XT ;` (src/core/checker.f:1635 `defer TFAM-ARITY-XT`) certifies and loads, rc 0. Acceptance: each is E-UNDEFINED outside its package on the product, and no checker defer can be rebound from checked code outside its package. Depends: none. Claim: unassigned.
