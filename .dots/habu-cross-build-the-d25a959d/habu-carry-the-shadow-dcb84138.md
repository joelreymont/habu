---
title: Carry the shadow emission in the capture
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.715680+03:00"
closed-at: "2026-10-01T10:27:28.647570+03:00"
close-reason: "test/aot-shadow-capture.f (SUITE aot-shadow-capture) passes on spark and the ThinkPad, READ refusing forged span, site and code-cell rows and MERGE refusing a shadow by name; all 32 SUITE aot* rows rc 0 on spark; native-build of this tree is e11cab554496d74f (byte-identical to base)."
---

Problem: the capture carries only the primary emission; the x86 writer needs the shadow per record.
Acceptance: the X2a reader over `NSHADOW`'s rows produces a shadow section (per-record x86 span, sites, does offsets; `aot-decl.f`/`aot-file.f` VERSION bump); xt-cell rows map through record index; the ARM64 product unchanged when no shadow; the capture part of the `docs/x86-64.md` dual-emission section.
Files: `src/habu/aot-decl.f`, `src/habu/aot-capture.f`, `src/habu/aot-file.f`, `test/aot-*`, `docs/x86-64.md`.
Verify: spark: the `test/aot-*` capture suites; rebuild; chain gen2==gen3; gate.
Depends: habu-emit-a-second-8a260f5a (X1), habu-read-recorded-sites-d4949953 (X2a). Serialise with X2a and I7 on `aot-capture.f`/`aot-closure.f`.
Route: Alder (shared: src/habu/aot-decl.f, src/habu/aot-capture.f, src/habu/aot-file.f, test/aot-*).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, X1 landed): NSHADOW (`src/compiler/native/shadow.f`) holds one map row per published record (`RECORDS`, `RECORD@`, `EMISSION@`, `ENTRY@`) and per emission its bytes, function offsets, call and address sites (`EMISSIONS`, `SIZE`, `BYTES`, `RET-BYTES`, `FUNCTIONS`, `FUNCTION-OFFSET@`, `CALL-SITE@`/`CALL-KIND@`/`CALL-TARGET@`, `ADDR-SITE@`/`ADDR-SITE-KIND@`); a `does>` companion enters at the clause function's offset. `NCOMP:CAPTURE-PREPARE` closes the shadow, so this leaf reads the map before it. Looking a record up is a linear scan today; add an index from record to row if this leaf needs one.
