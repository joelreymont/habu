---
title: Publish seed pointer definer occurrences
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:58:20.032110+03:00"
---

Current check-only Gforth recovery cannot produce hb-stdin: minimal emitted 7-dot Mach-O exits67 before user code. LLDB localizes bootstrap/cg/forth.fs BDEFSELECT to DEF-OCC:E-SELECT -7232 for freshly declared BUF-P in src/core/lower-cert-base.f after PERSISTED-PTR-VARIABLE. Requested dictionaryordinal0x169e, NDICT0x169f, occurrence slotq[0x169f]=0 while prior slot populated. Reproduces on149 and150 owner source, so not caused by newest owner field. Compare native publication and seed mirror; establish occurrence at the responsible dictionary publication boundary, preserve stale/invalid occurrence refusal and code/data layout. Existing bootstrap-engine-stack and full check-only recovery already fail on this path; run these real emitted artifacts after repair, required independent Astra review before landing. No guard bypass, fabricated occurrence, private consumer engine or unrelated MCU work. Rootowns integration/qualification; read-only localization evidence retained in carl-core-bootstrap150.kNzGXW.
