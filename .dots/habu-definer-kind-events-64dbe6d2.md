---
title: Definer-kind events in the discovery producer
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:15:00.353328+02:00"
---

Capability gap flagged while closing habu-tfam-5-preverify-23fac8cb (scout, 2026-07-04): the ordered event log (src/core/include.f) records only INCLUDED/REQUIRED/PROVIDED loader events, so within-file support replay in tools/check-all-errors-core.f (CA-COLLECT-SUPPORT/CA-SUPPORT-BEFORE :540-558,:885-891) orders definers by byte ranges, not events. Adding definer-kind events (package/public/private/end-package, deftype/deflinear/value-record/typefamily/sumtype, TRUST/immediate/EXPORT) to tools/source-discovery.f would make every consumer share one event stream. NO CONSUMER NEEDS THIS TODAY — byte order is sound (definers precede uses textually). Take up only if a consumer appears (e.g. partial-file replay, incremental checking); otherwise defer at campaign end with this rationale.
