---
title: TFAM 13 repair-packet ADT variant/tag field
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T14:59:02.249233+02:00"
---

Scoped chain for the remaining ADT fields on the E-MISMATCH repair packet. DONE this slice: family [render-only] — render.f DIAG-FAMILY walks DEXP/DACT for the first LAYOUT-PARAM, emits "family":"<TFAM-NAME$>" (expected side precedence, else actual; absent for pure-scalar mismatches). NEXT (this dot): variant/tag — needs the checker to CAPTURE the specific sum-variant/tag involved at the mismatch point (SGBAD-ADT capture) plus SV-* rollback safety for that capture (mirrors SV-DEXP/SV-DACT in TRIAL-SAVE/TRIAL-REST), then render emits "variant"/"tag". This is the foundation slice; without variant capture render only has the family, not which arm. THEN: payload-pos (which payload slot mismatched) → arity (declared vs actual family arity). Each is a separate red-first fixture: an ADT mismatch on a specific variant → packet carries the variant/tag/payload/arity. render.f:599 ROW-FAM/DIAG-FAMILY is the extension point; checker.f LAYOUT-PARAM?/PARAM>FAM (checker.f:410/950) resolve family; the variant/tag needs a NEW checker capture at CHECKER-STEP (checker.f:1408) alongside DEXP/DACT.
