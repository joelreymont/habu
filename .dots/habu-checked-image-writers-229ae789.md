---
title: Checked image writers
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.896981+02:00"
---

Rewrite src/os/{linux/elf.f,linux/sign.f,macos/macho.f,macos/sign2.f} under records + ptr-arith capability so BF-APPEND-CHECK-OFF (build-fixpoint.f:536) and BF-APPEND-IMAGE-TRUSTS (:555-560, 5 generated TRUST rows) are deleted - the image writers become ordinary checked source in stage2. Also converts aot-lib.f's open 0-set-check region (~237 ln ARM64 relocation core) into named TRUSTED: words at minimum (raw region -> named+tested boundaries). Effort M (~4d). Depends: ptr-arith + dict-record capabilities.

## Build-fixpoint half landed (2026-07-07, from head 671c15a7)

Finding: the image writers ALREADY check as written. Blocking certify
(BF-CERTIFY-GENERATED) has been statically verifying every definition in
elf.f/sign.f/macho.f/sign2.f for as long as it has been green - VERIFY:
SOURCE-BUF ignores the generated set-check window - so the window and the
five synthetic TRUST rows protected nothing. Deleted BF-APPEND-CHECK-OFF,
BF-APPEND-FRESH-CHECK-HOOK, BF-APPEND-IMAGE-TRUSTS (+ BF-APPEND-TRUST/
BF-APPEND-SQUOTE helpers) at both emit sites (stage2 + snap), and the
mirrored injection in tools/bootstrap.sh emit_src. The live stage compile
now checks the writers with the real hook: refresh converged (fixpoint OK)
first try. BFT pins the new shape (stage2 contains NO bare 0 set-check
line and none of the retired TRUST rows; image region has no window).
TRUSTED.md generated-trust exemption updated (set now empty).

No records/ptr-arith capability was needed: the writers' effects come from
their own checked definitions (IMG-M*/M-* from already-checked
image-bytes.f, phase tokens from roles.f, in-file SNAP-EXTRA-* TRUSTs).

REMAINING (this dot stays open): the aot-lib.f half - convert its
file-top 0 set-check region (~237 ln ARM64 relocation core) into named
TRUSTED: words at minimum; probe first whether most defs check as-is the
same way the writers did (hb-build maker source under certify).

## aot half, probe results (2026-07-07, from head ccbe0bd1)

Split outcome:
- src/habu/aot-closure.f window RETIRED - the file now compiles CHECKED in
  the live hb-build maker. Needed: two prim-axiom TRUST rows (JSON-DIAGS
  `-- ptr a`, CHECK! `ptr u8 n -- n`; the checker registry does not publish
  its own words, same class as verify-source CHECK-BODY), typed effects on
  FINDADDR ( n -- ptr a ) / FINDMAIN ( -- ptr a ) with the not-found return
  minted through xref.f XREF-NULL instead of a raw 0 (HIDX-MEM-NULL class),
  and SCAN-REC's found test via XREF-FOUND? instead of `dup IF` on a ptr.
  Behavior identical (null is the same 0 bits); stripped AOT binary proven
  end-to-end (7 SQ -> 49).
- src/habu/aot-lib.f window STAYS: genuine typed gaps in the relocation
  core. Fixed on the way (kept, valid under the window too): the
  EMIT-DATA-REGION-MAP LIT64, sites now reuse habu2.f's EM-DATA-VA>N
  boundary instead of raw DATA-VA.

aot-lib gap inventory (next blocker verbatim, then suspects in file order):
- `habu: in map-in-blob: at 'REC-END' expected: ptr a ptr n actual:
  ptr a ptr ptr a` - MAP-IN-BLOB {: r:ptr t:ptr :} bare-ptr locals vs
  REC-END ( ptr a -- ptr u8 ); the record/blob pointer roles need explicit
  pointee types through the whole MAP-IN-BLOB/OLD>NEW/MAP-TARGET chain.
- REC record reads (r @ / r 8 + @) want the typed dictionary-record
  capability (habu-typed-dictionary-record-c67adddb spec) rather than
  ad-hoc per-site unification.
- -1 sentinels mixed with offsets in REC-NEWOFF/MAP-IN-BLOB/OLD>NEW are
  n-typed and fine, but the found/not-found flow through `dup -1 <> IF
  EXIT THEN drop` needs re-verification once pointers are typed.
- The ADR/ADRP/B/BCOND/CBZ/TBZ immediate patchers and EMIT-* entry code
  operate on raw instruction words (n) over CODE/blob byte pointers -
  expect a small named TRUSTED: set (per this dot's original "named
  boundaries at minimum") for the patch32-class stores.
Estimated remainder: the ~180 lines from RAW-LEN to AOT-LINK.
