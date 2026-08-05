\ effect-test.f - focused boundary, truth-table, rejection-matrix, and
\ static-rejection suite for the finite CAD effect vocabulary + row algebra
\ (dot habu-define-finite-cad-0bdf52ad). Run:
\   bin/hb --load src/cad/effect-test.f
\
\ Direct-loaded gate home following the lib/nominal + lib/cad-num-types precedent:
\ CAD-EFFECT has no production consumer yet (the checker/registry/fusion owners are
\ later dots), so this file is NOT in test/gate-stdlib-cases.f and no gate slice
\ schedules it. It reopens package NOM to forge / fabricate the
\ boundary inputs the public API never mints, and package CAD-EFFECT to reach the
\ private wire classifier for direct binding-field assertions.

require src/cad/effect.f
require lib/test.f
require test/checker-assert.f

create CET-BA 40 allot          \ canonical key A
create CET-BB 40 allot          \ canonical key B
create CET-EA 8192 allot        \ encode buffer A
create CET-EB 8192 allot        \ encode buffer B
variable CET-LA  variable CET-LB

package CAD-EFFECT
private

\ ---- fixture rows --------------------------------------------------------------
: CET-WB ( -- NOM:row )                     \ weight + bias, both parameter-read
   NEW PARAM-READ OPERAND 0 EMIT PARAM-READ OPERAND 1 EMIT FREEZE ;
: CET-WB-REV ( -- NOM:row )                 \ same content, reversed insertion order
   NEW PARAM-READ OPERAND 1 EMIT PARAM-READ OPERAND 0 EMIT FREEZE ;
: CET-SA ( -- NOM:row )                     \ state-write + atomic sharing operand slot 0
   NEW STATE-WRITE OPERAND 0 EMIT ATOMIC OPERAND 0 EMIT FREEZE ;
: CET-SW3 ( -- NOM:row )                    \ single state-write at attribute slot 3
   NEW STATE-WRITE ATTRIBUTE 3 EMIT FREEZE ;
: CET-SW4 ( -- NOM:row )                    \ differs only in slot index
   NEW STATE-WRITE ATTRIBUTE 4 EMIT FREEZE ;
: CET-SC ( -- NOM:row )                     \ single random at capability slot 0
   NEW RANDOM CAPABILITY 0 EMIT FREEZE ;

\ ---- boundary inputs the public API never constructs ---------------------------
: CET-STALE ( -- NOM:row )                  \ a live handle then RESET => a stale record index
   CET-SW3 RESET ;
: CET-FOREIGN ( -- NOM:row )                \ a valid NOM row that is not a valid effect row
   NOM:NEW NOM:ROOT 42 NOM:CONS 0 NOM:BIND NOM:ADD NOM:FREEZE ;

\ ---- atom enumeration (test-side code -> effect-atom) --------------------------
: CET-ATOM ( n -- effect-atom )
   case
      0 of PURE-ATOM endof         1 of PARAM-READ endof
      2 of STATE-WRITE endof       3 of RANDOM endof
      4 of HOST-IO endof           5 of DEVICE-LAUNCH endof
      6 of ATOMIC endof            7 of COLLECTIVE endof
      8 of ALLOCATION endof        9 of PUBLICATION endof
      PUBLICATION swap
   endcase ;

\ ============================================================================
\ Atom-level truth tables: every atom pinned on every unary table.
\ ----------------------------------------------------------------------------
: CET-DUP ( -- )
   PURE-ATOM DUP-OK? TTRUE         PARAM-READ DUP-OK? TTRUE
   STATE-WRITE DUP-OK? TFALSE      RANDOM DUP-OK? TFALSE
   HOST-IO DUP-OK? TFALSE          DEVICE-LAUNCH DUP-OK? TFALSE
   ATOMIC DUP-OK? TFALSE           COLLECTIVE DUP-OK? TFALSE
   ALLOCATION DUP-OK? TFALSE       PUBLICATION DUP-OK? TFALSE ;
: CET-CACHE ( -- )
   PURE-ATOM CACHEABLE? TTRUE      PARAM-READ CACHEABLE? TTRUE
   STATE-WRITE CACHEABLE? TFALSE   RANDOM CACHEABLE? TFALSE
   HOST-IO CACHEABLE? TFALSE       DEVICE-LAUNCH CACHEABLE? TFALSE
   ATOMIC CACHEABLE? TFALSE        COLLECTIVE CACHEABLE? TFALSE
   ALLOCATION CACHEABLE? TFALSE    PUBLICATION CACHEABLE? TFALSE ;
: CET-BARRIER ( -- )
   PURE-ATOM BARRIER? TFALSE       PARAM-READ BARRIER? TFALSE
   STATE-WRITE BARRIER? TTRUE      RANDOM BARRIER? TTRUE
   HOST-IO BARRIER? TTRUE          DEVICE-LAUNCH BARRIER? TTRUE
   ATOMIC BARRIER? TTRUE           COLLECTIVE BARRIER? TTRUE
   ALLOCATION BARRIER? TTRUE       PUBLICATION BARRIER? TTRUE ;

\ ---- commute table: full 10x10 matrix vs the reference policy ------------------
: CET-REF-COMMUTE? ( n n -- bool ) {: a:n b:n :}   \ pure-with-any, or two parameter-reads
   a 0= if 0 0= exit then
   b 0= if 0 0= exit then
   a 1 = b 1 = and ;

variable CET-CMT-A
: CET-CMT-CELL ( n -- ) {: b:n :}                   \ table matches the symmetric reference for (a,b)
   CET-CMT-A @ b CET-REF-COMMUTE? if
      CET-CMT-A @ CET-ATOM b CET-ATOM COMMUTE? TTRUE
      b CET-ATOM CET-CMT-A @ CET-ATOM COMMUTE? TTRUE
   else
      CET-CMT-A @ CET-ATOM b CET-ATOM COMMUTE? TFALSE
      b CET-ATOM CET-CMT-A @ CET-ATOM COMMUTE? TFALSE
   then ;
: CET-CMT-ROW ( n -- )
   CET-CMT-A !
   0 begin dup 10 < while dup CET-CMT-CELL 1+ repeat drop ;
: CET-COMMUTE ( -- )
   0 begin dup 10 < while dup CET-CMT-ROW 1+ repeat drop ;

\ ============================================================================
\ Row construction: distinctness and sharing.
\ ----------------------------------------------------------------------------
: CET-BUILD ( -- )
   PURE SIZE 0 T=                                   \ PURE is the empty row
   CET-WB SIZE 2 T=                                 \ weight and bias both bind parameter-read
   CET-SA SIZE 2 T=                                 \ state-write + atomic share a site/slot
   CET-WB CET-WB-REV EQUAL? TTRUE ;                 \ insertion order is not identity

\ ---- canonical equality / serialization ignore handle + allocation order -------
: CET-BYTES= ( ptr u8 n ptr u8 -- bool ) {: pa:ptr n:n pb:ptr :}
   0 begin dup n < while
      dup pa + c@ over pb + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
: CET-CANON ( -- )
   CET-WB CET-EA 8192 ENCODE CET-LA !
   CET-WB-REV CET-EB 8192 ENCODE CET-LB !
   CET-LA @ CET-LB @ T=                             \ equal length regardless of build order
   CET-EA CET-LA @ CET-EB CET-BYTES= TTRUE          \ byte-identical wire
   CET-WB CET-BA KEY  CET-WB-REV CET-BB KEY
   CET-BA 32 CET-BB CET-BYTES= TTRUE                \ byte-identical 32-byte content key
   CET-WB CET-EA 8192 ENCODE  CET-EA swap DECODE  CET-WB EQUAL? TTRUE ;   \ fresh-decode replay

\ ============================================================================
\ REMAP: preserves slot kind/index, prefixes capture-free, deterministic.
\ ----------------------------------------------------------------------------
: CET-REMAP ( -- )
   CET-SW3 7 REMAP SIZE 1 T=                        \ count preserved
   CET-SW3 7 REMAP  CET-SW3 7 REMAP  EQUAL? TTRUE   \ deterministic
   CET-SW3 7 REMAP  CET-SW3 8 REMAP  EQUAL? TFALSE  \ different site => distinct
   CET-SW3 7 REMAP  CET-SW4 7 REMAP  EQUAL? TFALSE  \ index preserved through remap
   CET-SW3 7 REMAP VALIDATE SIZE 1 T= ;             \ remapped row is still a valid effect row

\ ---- direct kind/index preservation via the private classifier ----------------
: CET-REMAP-FIELDS ( -- )
   CET-SW3 CE-SCAN-ROW drop
   CE-B-ATOM @ STATE-WRITE ATOM>CODE T=
   CE-B-KIND @ ATTRIBUTE KIND>CODE T=
   CE-B-INDEX @ 3 T=
   CET-SW3 9 REMAP CE-SCAN-ROW drop                 \ after remap the leaf slot is untouched
   CE-B-ATOM @ STATE-WRITE ATOM>CODE T=
   CE-B-KIND @ ATTRIBUTE KIND>CODE T=
   CE-B-INDEX @ 3 T= ;

\ ============================================================================
\ UNION: associative, commutative, idempotent, deterministic after remap.
\ ----------------------------------------------------------------------------
: CET-UNION ( -- )
   CET-WB CET-SA UNION SIZE 4 T=                    \ disjoint merge
   CET-WB CET-WB UNION CET-WB EQUAL? TTRUE          \ idempotent
   CET-WB CET-SA UNION  CET-SA CET-WB UNION  EQUAL? TTRUE                 \ commutative
   CET-WB CET-SA UNION CET-SC UNION
   CET-WB  CET-SA CET-SC UNION  UNION  EQUAL? TTRUE                       \ associative
   CET-SW3 7 REMAP  CET-SW4 7 REMAP  UNION
   CET-SW4 7 REMAP  CET-SW3 7 REMAP  UNION  EQUAL? TTRUE ;                \ commutative after remap

\ ---- two callees that both bind local slot zero stay distinct once remapped ----
: CET-DISTINCT ( -- )
   CET-SW3 CET-SW3 EQUAL? TTRUE                     \ identical local rows
   CET-SW3 1 REMAP  CET-SW3 2 REMAP  EQUAL? TFALSE  \ into different sites: distinct
   CET-SW3 1 REMAP  CET-SW3 1 REMAP  UNION SIZE 1 T= ;   \ into the same site: idempotent

\ ============================================================================
\ Row-level classification folds the truth tables over a row's atoms.
\ ----------------------------------------------------------------------------
: CET-CLASSIFY ( -- )
   PURE ROW-DUP-OK? TTRUE      PURE ROW-CACHEABLE? TTRUE   PURE ROW-BARRIER? TFALSE
   CET-WB ROW-DUP-OK? TTRUE    CET-WB ROW-CACHEABLE? TTRUE CET-WB ROW-BARRIER? TFALSE
   CET-SA ROW-DUP-OK? TFALSE   CET-SA ROW-CACHEABLE? TFALSE CET-SA ROW-BARRIER? TTRUE
   CET-WB CET-WB ROWS-COMMUTE? TTRUE                \ two parameter-read rows commute
   PURE CET-SA ROWS-COMMUTE? TTRUE                  \ pure commutes with anything
   CET-WB CET-SA ROWS-COMMUTE? TFALSE              \ parameter-read vs state-write do not
   CET-SA CET-SC ROWS-COMMUTE? TFALSE ;            \ two effectful rows do not

\ ============================================================================
\ Rejection matrix: each rejects transactionally with a typed diagnostic.
\ ----------------------------------------------------------------------------
: CET-DUP-INSERT ( -- NOM:row )                 \ same binding EMITted twice
   NEW STATE-WRITE OPERAND 0 EMIT STATE-WRITE OPERAND 0 EMIT FREEZE ;
: CET-EMIT-PURE ( -- NOM:row )                  \ the pure atom is not a binding
   NEW PURE-ATOM OPERAND 0 EMIT FREEZE ;
: CET-EMIT-NEGIDX ( -- NOM:row )
   NEW STATE-WRITE OPERAND -1 EMIT FREEZE ;
: CET-EMIT-BIGIDX ( -- NOM:row )            \ index above the 56-bit tagged-segment range
   NEW STATE-WRITE OPERAND $0100000000000000 EMIT FREEZE ;
: CET-BUDGET ( -- NOM:row )
   2 NOM:BUDGET!
   NEW STATE-WRITE OPERAND 0 EMIT STATE-WRITE OPERAND 1 EMIT STATE-WRITE OPERAND 2 EMIT FREEZE ;
: CET-DBLOPEN ( -- n )   NEW [: NEW ROLLBACK ;] catch swap ROLLBACK ;
: CET-FORGE-COUNT ( -- )                           \ corrupt the protocol count to overflow the buffer
   CET-SW3 CET-EA 8192 ENCODE CET-LA !
   $7F CET-EA 0 + c! ;

: CET-REJECT ( -- )
   [: CET-STALE SIZE drop ;] E-NOM-HANDLE TTHROWSQ                  \ forged / stale handle
   [: CET-FOREIGN VALIDATE SIZE drop ;] E-CADEFF-MALFORMED TTHROWSQ \ foreign / noncanonical row
   [: CET-EA 5 DECODE SIZE drop ;] E-NOM-WIRE TTHROWSQ              \ truncated wire
   CET-FORGE-COUNT
   [: CET-EA CET-LA @ DECODE SIZE drop ;] E-NOM-WIRE TTHROWSQ       \ protocol-count overflow
   [: CET-BUDGET SIZE drop ;] E-NOM-BUDGET TTHROWSQ                 \ allocator / resource failure
   $100000 NOM:BUDGET!
   [: CET-DUP-INSERT SIZE drop ;] E-CADEFF-DUPLICATE TTHROWSQ       \ direct duplicate insertion
   [: CET-EMIT-PURE SIZE drop ;] E-CADEFF-ATOM TTHROWSQ             \ the pure atom is not bindable
   [: CET-EMIT-NEGIDX SIZE drop ;] E-CADEFF-INDEX TTHROWSQ          \ negative slot index
   [: CET-EMIT-BIGIDX SIZE drop ;] E-CADEFF-INDEX TTHROWSQ          \ out-of-range slot index
   [: CET-SW3 -1 REMAP SIZE drop ;] E-CADEFF-SITE TTHROWSQ         \ negative call-site ordinal
   [: CET-SW3 $0100000000000000 REMAP SIZE drop ;] E-CADEFF-SITE TTHROWSQ  \ out-of-range call-site ordinal
   CET-DBLOPEN E-NOM-OPEN T= ;                                      \ incomplete builder: second open

: CET-ROLLBACK ( -- )                              \ ROLLBACK publishes nothing; a fresh build still works
   NEW STATE-WRITE OPERAND 0 EMIT ROLLBACK
   PURE SIZE 0 T=
   CET-WB SIZE 2 T= ;

\ ============================================================================
\ Static rejection: the checked boundary is fail-closed on raw n / cross-type.
\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
\ ----------------------------------------------------------------------------
: CET-STATIC ( -- )
   s" CET-OK-DUP ( CAD-EFFECT:effect-atom -- bool ) CAD-EFFECT:DUP-OK?"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CET-OK-ROW ( NOM:row -- n ) CAD-EFFECT:SIZE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CET-BAD-RAW-ATOM ( n -- bool ) CAD-EFFECT:DUP-OK?"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CET-BAD-KIND-ATOM ( CAD-EFFECT:slot-kind -- bool ) CAD-EFFECT:DUP-OK?"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CET-BAD-RAW-ROW ( n -- n ) CAD-EFFECT:SIZE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CET-BAD-ATOM-ROW ( CAD-EFFECT:effect-atom -- n ) CAD-EFFECT:SIZE"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: CET-ALL ( -- )
   T-RESET
   RESET
   CET-DUP CET-CACHE CET-BARRIER CET-COMMUTE
   CET-BUILD CET-CANON
   CET-REMAP CET-REMAP-FIELDS
   CET-UNION CET-DISTINCT
   CET-CLASSIFY
   CET-REJECT CET-ROLLBACK
   CET-STATIC
   T-REPORT ;

CET-ALL
;package
