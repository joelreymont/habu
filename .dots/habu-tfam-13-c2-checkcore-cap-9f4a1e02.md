---
title: "TFAM 13 C2 follow-up: check-core >$100000 declaration body via packet"
status: closed
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

## CLOSED — premise corrected + clean fail-closed fix (tools-only, 2026-07-11)

PREMISE CORRECTION: CHK-VREC-ROOM (shared with the value-record collector) is
UNREACHABLE for declaration bodies — a collected body is a space-joined token
stream that is never longer than the source region it came from, and every
source ingest path already caps at the same CHK-SRC-CAP, so `body <= source <=
cap` holds and the ROOM guard can never fire on the block-declaration path. It
stays untouched as the defensive invariant (and the value-record path keeps its
honest E-FS-CAPACITY, as this dot warned).

The REAL failure was worse than described: a >$100000 source (1.7 MB fixture,
one giant SUMTYPE) died with an UNCAUGHT E-FS-CAPACITY (-2106) from the read
layer — raw exit 67, no diagnostic text at all — on both the file and stdin
paths, because CHK-MATERIALIZE runs outside any catch in CHECK-MAIN. There is
no declaration packet to emit for such a file: the source is rejected before a
single token is read, so E-BAD-DECLARATION parity does not apply; the honest
report is a clean input-capacity diagnostic.

FIX (tools/check-core.f): CHK-SOURCE-TOO-BIG (`check.f: source exceeds
capacity` + CHK-E-NOINPUT via CHK-FAIL, matching the `no such source` pattern);
CHK-MATERIALIZE now catches the dispatch and maps E-FS-CAPACITY to it (other
codes rethrow); CHK-MATERIALIZE-BUF-AS's explicit oversize throw uses it too.
Proof: 1.7 MB fixture -> exit 66 + the message on file/stdin/json paths (was
uncaught 67); missing-file still 66 `no such source`; normal stdin source still
exit 0. Red-first regression CKT-TEST-OVERCAP-SOURCE (check-test-lib.f) pins
CHK-E-NOINPUT + the message via CHK-MATERIALIZE-BUF-AS. ENGINE SIDE: none
needed — TDECL-C capping landed with C2; no src/core change (derive-worker
constraint honored).
