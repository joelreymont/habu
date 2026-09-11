---
title: Track retained JIT code correctly before saving executables
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.854352+03:00\""
---

Owner: Rowan tier lane .jj-ws/rowan-tier. Extra Astra independent review of64e07df8 +ccd6aad7087e found blockers; superseded60e01e90 must not be integrated. Engine source habu2.f; save guard pending. Current per-dictionary-record bitmap is insufficient to certify all reachable code as optimized.

Strong public-API false-zero reproducer: `defer RV-D ( -- n ) : RV-INSTALL ( -- ) [: 7 ;] is RV-D ; RV-INSTALL s" RV-INSTALL" HIDE-DEFS-FROM tier0-count@ . cr RV-D . cr` prints0,7: hidden installer leaves a live JIT quotation in earlier deferred storage. AOT callers similarly retain direct calls to hidden/reused JIT records; aliases and DOES clauses can also preserve JIT code while count returns0. Alias reducer and direct-call reducer are in BB20260911-130409.139-cedar-d125 and20260911-130440.104-cedar-c5a3.

False-positive publisher bug: `variable ND ndict@ ND ! : JA ( -- ) ; ND @ ndict! package RP ;package tier0-count@ . cr` prints1, expected0. C-STORE-DEF-NAME clearing misses DOES-REC:RECORD and explicit/implicit package publishers. EXPORT clears instead of inheriting source provenance. Ordinary cold count0 and warm count620 persistence pass; they do not establish executable compliance.

Acceptance: enforce user's every-executable-optimized invariant against retained callable JIT code, including hiding, aliases, DOES and stored quotations/direct calls. Pure warm recapture of already optimized code/data works. Saving new retained JIT code must refuse clearly until actual optimizing conversion exists. Do not invent source replay with repeated top-level effects or a new snapshot format to hide this gap. Add focused positive/negative actual APP-IMAGE:SAVE child tests. Reviewer pinned binary SHA75abbe2bd7ca0d04fb35e4d0dee91bfa1188bdf27714fd8d4c209b200546318a.

Rowan accepted the record-count contract failure and is implementing code spans, not yet reviewed. Caveat sent on BB20260911-131255.544-cedar-2eec: CP lowering alone does not overwrite bytes; preserve provenance until actual overwrite, filter queries by current CP, and test unchanged CP lower/re-expose. Add clear span capacity behavior and document count semantics. Do not claim the replacement accepted before independent negative controls.
