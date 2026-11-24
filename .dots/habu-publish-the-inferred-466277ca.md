---
title: "Publish the inferred effect's name as spelled"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:29:26.555903+01:00"
---

Problem: src/core/checker.f CHECKER-USIG-CERT-CURRENT (about :17811), the inferred-effect publication, stores 'na nu NMOFF @ TADDR nu CHECKER-REC-NAME-AT!', the folded name, so 'hb --check --index' prints "decl_name":"xx" for a word spelled XX on the no-visit path (CHECK-BUF-BAD); after the spelling fix the 'na nu' inputs of CHECKER-USIG-CERT-PARSED and CHECKER-USIG-CERT-ADD-PENDING go unused (callers tools/check-walk.f :1459, :1584, :1596 pass 'NMA @ NMU @'). Found by carl-ixspell's worker. Acceptance: --index on a source that spells a word in mixed case prints the spelling on every path; no unused input remains. Depends: carl's spelling batch on master. Claim: unassigned.
