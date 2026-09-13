---
title: Grow captured relocation sites within the code blob bound
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T20:48:33+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
---

The first complete all-tier1 target source load reached capture on source
f91cacb4, then refused74 after323.052s: `aot-capture: too many DATA sites`.
The identified5b969df8 bootstrap input had completed paired compiler loading;
no product was emitted or accepted. Evidence: native-bootstrap-B1.log.

Cause: aot-decl owned one static16,384-row u32 buffer shared by DATA and CODE
relocations, sized from an old measured ratio. Replace it with existing transient
DYNAMIC-BUFFER ownership. The format maximum is AOT-BLOB-CAP/4: the two sweeps
partition aligned relocation starts and deduplicated defer cells cannot add a
second row for a start. Append reserves before count publication. Artifact
READ/IMPORT/MERGE share validated section reservation and preserve packed rows;
the section budget counts their shared maximum once. No fixed engine slot moves.

Own aot-decl/capture, aot-file reservation/room checks, the one owned-import
reservation call, icode's derived-budget agreement, and focused site fixtures.
Reject negative/overflowing counts and combined table overflow before allocation
or copy. Preserve the existing transient registry lifetime. Verify more than
16,384 rows through actual appenders, released-storage file/owned transfer,
overlapping merge tail movement, exact bound reservation and named refusals.

The same reader seam fixes reviewed D05: ?SPAN previously accepted negative
DATA spans when runs/address rows were empty. File and owned inputs must reject
-1/min/cap+1 while restoring0/cap exactly. Root reviews and composes this slice
with the typed writer and provenance patch before the next complete bootstrap.

Focused checks pass on seed5b969df8: the exact registered PROBE-DATA-SITES
helper passes; the positive fixture passes at tier0 and tier1 with20,001 rows
of each kind, exact restored contents, released-source storage, merge-tail
movement and preservation after reserving the format maximum. Shared overflow
refuses75 at both tiers; negative/max+1/overflowing reservation and late DATA
append refuse74. D05's five scalar values pass both file/owned matrices with
the required outcomes. icode-fixup-test passes the updated budget agreement.
No full bootstrap, restored product or complete compiler gate is claimed here.

Independent review found the adjacent MERGE scalar path could hide source -1
under an8-byte host span and accept merged span7. SCALARS@ now validates the
incoming span before storing it or computing a merged extent. Real file MERGE
refuses that counterexample75 and accepts source0 with exact merged span8;
both controls are registered beside the file/owned scalar matrix.
