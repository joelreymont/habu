---
title: "Publish the inferred effect's name as spelled"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:29:26.555903+01:00"
---

Problem: src/core/checker.f CHECKER-USIG-CERT-CURRENT (:17816), the inferred-effect publication, stores 'na nu NMOFF @ TADDR nu CHECKER-REC-NAME-AT!', the folded name, so 'hb --check --index' prints "decl_name":"xx" for a word spelled XX on the no-visit path (CHECK-BUF-BAD); after the spelling fix the 'na nu' inputs of CHECKER-USIG-CERT-PARSED and CHECKER-USIG-CERT-ADD-PENDING go unused (callers src/core/check-walk.f :1459, :1584, :1596 pass 'NMA @ NMU @'), and every caller of USIG-CERT-ADMIT (checker.f :17448) passes the name twice (checker.f :17457, :17512, :24441 'na nu na nu'; check-walk.f :616 'at atu at atu'). Acceptance: --index on a source that spells a word in mixed case prints the spelling on every path; no unused or repeated input remains. Depends: none. Claim: unassigned.
