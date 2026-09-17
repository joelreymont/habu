---
title: Respect contravariance in quotation input rows
status: closed
priority: 1
issue-type: task
created-at: "2026-09-14T13:31:34.143512+03:00"
closed-at: "2026-09-16T14:34:51.508083+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Residue: quotation input rows must be contravariant so a callback cannot narrow where a direct call is refused"
---

Audit C2 independently reproduces on current /tmp/cedar-compiler-correctness SHA46921f52: a [: TAKE-U8 ;] callback satisfies [ i64 -- ], while direct narrowing rejects. Repro: TRUSTED: TAKE-U8 ( u8 -- ) drop ; TRUSTED: MK-I64 ( -- i64 ) 300 ; : CALLQ ( [ i64 -- ] -- ) MK-I64 swap execute ; : PV ( -- ) [: TAKE-U8 ;] CALLQ ; PV. checker.f U-TYPE quotation din/rin pairs around2089 retain outer covariance. Fix input variance, preserve output covariance, test both rows and nested quotations through current owner path/two tiers. Read-only confirmed before Herdr restart; no patch.
