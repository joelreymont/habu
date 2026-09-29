\ record-launder-probe.f - what a mixed memory record costs today.
\ Run: bin/hb --load test/record-launder-probe.f
\
\ A MIXED RECORD is a contiguous region holding at least one POINTER field and
\ at least one SCALAR field under one base. The language has no declared form
\ for one (BEGIN-STRUCTURE publishes a size constant; STRUCTURE/PRODUCT are
\ value families), so every mixed record in the tree casts one of the two
\ halves, and each cast is a launder the raw-storage rule cannot close:
\
\   cast A  `<off> ptr-field` over a base whose pointee the checker does not
\           know to be a pointer - the row is ( ptr a n -- ptr ptr b ) with `b`
\           FREE, so it manufactures a typed pointer out of a base that is only
\           a parameter. This is the row `PTR-FIELD:` itself generates.
\   cast B  `byte-view cell-view` over a DECLARED pointer cell - both are
\           type-level renames, so the pair reads the pointer's own cell, or a
\           count behind it, as a scalar cell, and a store puts an integer where
\           the rule prescribed an address.
\
\ The open repros below show both casts. The gate checks direct refusals and
\ legitimate field reads without requiring the open holes to certify.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

\ ---- the three storage forms a mixed record has today ------------------------

\ 1. raw: the whole record is `create`d dictionary bytes (tools/lint/text.f
\    LINT-SLAB, lib/vector.f's header, every BEGIN-STRUCTURE layout).
create RLP-RAW 3 cells allot

\ 2. declared head plus allotted counts: the form the raw-storage rule
\    prescribes (src/core/cell-effects.f STATE, every DYNAMIC-BUFFER).
PTR-VARIABLE RLP-HEAD 0 , 0 ,

\ 3. a declared pointer table: pointers only, no scalar field, so it needs no
\    cast at all - the control that says the cast is the RECORD's fault.
2 PTR-U8-TABLE RLP-TAB

package RECORD-LAUNDER-PROBE
private

\ =============================================================================
\ Open repros: these are source examples, not gate assertions. Cast A makes a
\ pointer through a free pointee; cast B writes an integer into a pointer cell.
\ The checker must refuse these before the gate can assert a refusal.
\ =============================================================================
\ L1/L2: habu-refuse-ptr-field-331a9731
\ : RLP-FIELD-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;
\ : RLP-FIELD-N ( ptr n -- ptr ptr u8 ) 0 ptr-field ;
\ : L1 ( n -- u8 ) RLP-RAW ! RLP-RAW RLP-FIELD-A @ c@ ;
\ : L2 ( n -- u8 ) RLP-RAW ! RLP-RAW RLP-FIELD-N @ c@ ;
\ L3/L4: habu-refuse-a-scalar-030be3ad
\ : L3 ( n -- ) RLP-HEAD BYTE-VIEW CELL-VIEW ! ;
\ : L4 ( n -- u8 ) RLP-HEAD BYTE-VIEW CELL-VIEW ! RLP-HEAD @ c@ ;

\ =============================================================================
\ 1. The direct forms are fenced.
\ =============================================================================
: SECTION-FENCED ( -- )
   s" the direct forms of both puns are refused" T-LABEL
   \ the value door and the field door over a named raw base
   s" C1 ( n -- n ) RLP-RAW ! RLP-RAW @ @"    CHECK-QUIET-CANDIDATE! 0 T=
   s" C2 ( n -- n ) RLP-RAW ! RLP-RAW 0 ptr-field @ @"
                                              CHECK-QUIET-CANDIDATE! 0 T=
   \ and the cast without `ptr-field` to legitimize the specialization
   s" C3 ( ptr a -- ptr ptr u8 ) "             CHECK-QUIET-CANDIDATE! 0 T= ;

\ =============================================================================
\ 2. What must keep certifying. Each row is a mixed record's field read; the
\    design turns each into a typed accessor the record declaration generates,
\    so the cast disappears rather than the capability.
\ =============================================================================
: SECTION-KEEP ( -- )
   s" the legitimate field reads the design has to preserve" T-LABEL
   \ a count cell behind a declared head: DYNAMIC-BUFFER's generated accessor,
   \ src/core/dynamic-storage.f CTL, src/core/cell-effects.f INSTALL.
   \ Becomes a scalar field accessor of the declared record.
   s" K1 ( -- n ) RLP-HEAD CELL + BYTE-VIEW CELL-VIEW @"
                                              CHECK-QUIET-CANDIDATE! -1 T=
   s" K2 ( n -- ) RLP-HEAD CELL + BYTE-VIEW CELL-VIEW !"
                                              CHECK-QUIET-CANDIDATE! -1 T=
   \ the pointer field of a declared cell: the form the rule prescribes.
   \ Becomes a pointer field accessor of the declared record.
   s" K3 ( n -- ptr u8 ) cells RLP-TAB + 0 ptr-field @"
                                              CHECK-QUIET-CANDIDATE! -1 T=
   s" K4 ( -- ptr u8 ) RLP-HEAD @"            CHECK-QUIET-CANDIDATE! -1 T=
   \ and the reason a declared cell alone is not a record: its pointee is open,
   \ so the SAME cell takes two unrelated pointer types. A declared field pins
   \ the pointee at the declaration, which is what closes these two.
   s" K5 ( ptr a -- ) RLP-HEAD 0 ptr-field !" CHECK-QUIET-CANDIDATE! -1 T=
   s" K6 ( ptr u8 -- ) RLP-HEAD 0 ptr-field !"
                                              CHECK-QUIET-CANDIDATE! -1 T= ;

public

: RUN ( -- )
   T-RESET
   SECTION-FENCED
   SECTION-KEEP
   T-REPORT ;

RUN

;package
