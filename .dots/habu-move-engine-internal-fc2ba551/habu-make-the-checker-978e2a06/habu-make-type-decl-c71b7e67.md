---
title: "Make TYPE-DECL's SD-ONE-NAME and TDECL-CONVERTERS private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T17:00:09.405071+01:00"
---

Problem: TYPE-DECL:SD-ONE-NAME (src/core/structure-decl.f:1217) and TYPE-DECL:TDECL-CONVERTERS (src/core/sumtype.f) are public; their only caller, src/lib/type/deftype.f MINT-OVER, is a boot file (src/habu/boot-files.f:131) that already takes TYPE-DECL's private SD-WRAP by EXPORT. ' TYPE-DECL:SD-ONE-NAME drop passes tools/check.f -- --verify-only, rc 0. Acceptance: both E-UNDEFINED outside TYPE-DECL; deftype.f takes them by EXPORT. Depends: none. Claim: unassigned.
