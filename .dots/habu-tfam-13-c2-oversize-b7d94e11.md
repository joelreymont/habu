---
title: "TFAM 13 C2: oversized declaration bodies must emit the declaration packet"
status: open
priority: 3
issue-type: task
created-at: "2026-07-10T12:00:00.000000+02:00"
---

Destruction-review finding C2 from dot habu-tfaam-13-adt-5d3288f0 (S2 audit,
2026-07-10). Oversized declaration bodies die RAW without the §24 declaration
packet: src/core/sumtype.f:669 TDECL-C, ("sumtype: declaration too long" 70 die,
TDECL-CAP $1000); src/habu/verify-source.f:154 BODY-APPEND ("verify-source:
check body too long" 74 die); and the check-core CHK-EXP/CHK-VREC buffer paths.
The S2 slice fixed the missing name/arity/;SUMTYPE pre-checks; the oversize path
is a separate parity gap. Fix: route each too-long die through CHECKER-DEFSUM-NOEND
/ TDECL-DIAG (reason "declaration too long") so native/verify-source/check-core
emit the same E-BAD-DECLARATION packet. Needs family-name plumbing to the deep
BODY-APPEND/TDECL-C, sites (the name is known before the body overflows).
Red-first fixture: an oversized TYPEFAMILY/SUMTYPE body asserts the packet on all
three paths (currently RED raw-die). Engine change -> byte-fixpoint x2.
