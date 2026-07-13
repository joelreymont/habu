---
title: "checker: TDECL payload grammar rejects package-qualified family names"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T10:35:08.768604+02:00\""
---

Found by the MP-SLOT probe (2026-07-13, dot habu-maki-typed-no-c4836a39): 'SUMTYPE mp-slot 0 / VARIANT some MIR:input-slot' rejects with "bad sumtype declaration 'mp-slot': unknown payload type at 'MIR:input-slot'" - the TDECL payload grammar does not admit package-qualified family names, while the SAME qualified spelling works in signatures, LAYOUT-BUFFER type position (probe ctrl2: MAKI:dtype), and type arguments (option<MIR:input-slot> certifies). Inconsistent name-resolution surface: a family usable everywhere else cannot be a VARIANT/FIELD payload unless declared in-package. Fix: route TDECL payload-type lookup through the same qualified resolver the signature parser uses (TFAM-RESOLVE / TFQ path), negative for a bogus qualifier, positive for a cross-package payload + rollback/replay coverage (verify-source RECORD-* must re-parse the qualified payload identically). Type-system lane; small-medium.
