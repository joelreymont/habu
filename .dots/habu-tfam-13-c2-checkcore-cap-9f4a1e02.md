---
title: "TFAM 13 C2 follow-up: check-core >$100000 declaration body via packet"
status: open
priority: 4
issue-type: task
created-at: "2026-07-10T14:00:00.000000+02:00"
---

C2 (habu-tfam-13-c2-oversize) unified the effective declaration-body limit to
TDECL-CAP ($1000): CHECKER-DEFSUM/DEFENUM/DEFPRODUCT-BODY now reject an over-cap
body (TDECL-REQUIRE-FIT) with the E-BAD-DECLARATION packet, and the collection
buffers cap instead of raw-dying (native TDECL-C, sets TDECL-OVERSIZE;
verify-source BODY-APPEND caps). This fixes every realistic body: a >$1000 body
is caught by the length check on all three paths (native / verify-source /
check-core) because verify-source (BODYBUF-CAP 8000) and check-core
(CHK-SRC-CAP $100000) hand the full (or capped) body to CHECKER-DEFSUM, whose
length check fires at $1000.

Remaining edge (out of C2's clean scope): a declaration body larger than
check-core's CHK-SRC-CAP ($100000 = 1 MB) still throws E-FS-CAPACITY from
tools/check-core.f CHK-VREC-ROOM (652) rather than the E-BAD-DECLARATION packet.
CHK-VREC-ROOM is SHARED with the value-record collector (CHK-VALUE-RECORD path),
which has no length check, so capping CHK-VREC-ROOM blindly would silently
truncate an over-cap value record. A clean fix needs a declaration-block-specific
cap (or a length check in the value-record handler too) so a >$100000 declaration
body also reports the declaration packet. This is a 1 MB+ absurd body (no real
ADT), so low priority; parity for realistic bodies already holds.
