---
title: Split call-crossed values around the loop
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.149470+02:00"
---

CALL-PRESSURE road, from the spill lane's measurement (2026-08-05, bookmark spill): a value live across a CALLLESS loop already spills today (probe: 7 live across a callless loop compiles rc 0; the same 7 across a call in the loop refuses E-A64RA-SPILL -8508) — the refusal comes from MB-KEEP-BLOCK KEEPing every operand of the call site's data-stack saves in the middle block, NOT from loop residency. Fix: split the live range so the value is dead across the loop — store in the entry block, reload in the exit block — a placement ORDER-CK already permits; no middle-block frame redesign (that stays with habu-spill-from-a-4145325c and is NOT needed here). Also: correct tools/codegen-compare-new4.f's stale 'one refusal, two roads' paragraph, and promote the lane's probe (/private/tmp/claude-501/spill-probe-final.f) into tools/ as the regression instrument. Acceptance: CALL-PRESSURE compiles, answers match the engine bit-for-bit, the A64RAV validator checks the split slots, no previously-supported row changes bytes, both-gaps reported, corpus-4 baseline re-pinned deliberately.
