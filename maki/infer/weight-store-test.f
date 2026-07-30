\ weight-store-test.f - focused coverage for the weight-store residency value
\ (maki/infer/weight-store.f). Run standalone:
\ bin/hb --load maki/infer/weight-store-test.f
\
\ Fixed aligned, unaligned, boundary, and high-bit u32 expectations cover both
\ residency arms through their production constructors.
\
\ Reads return NONE for a far valid slot, numeric overflow, slot crossing, or
\ arm crossing. The far slot would make unchecked table-row arithmetic overflow.
\ The slot-crossing read starts inside a four-byte slot but ends in distinct
\ bytes of its eight-byte backing. Forked children release each arm's backing
\ page first, proving every refusal returns without reading it.
\
\ WHICH REFUSALS COST A BLOCK, AND WHY THAT SPLIT IS THE POINT. The three SLOT!
\ refusals run their population step as a stack-preserving quotation under `catch`,
\ so the builder survives the refusal and the leg gives it back through
\ BUILDER-DISPOSE - they cost nothing now, and the residue after them is 0. The two
\ SEAL refusals cannot be written that way, because a `catch` over SEAL would have to
\ hold a builder on one arm and a table on the other, so each still strands one
\ builder. Value-level read refusals preserve the store and add no stranded owners.
\
\ The static half feeds bad definitions to the checker itself: store, table,
\ builder, and buffer linearity (no dup / drop / store / reuse), double DISPOSE,
\ arm confusion in both constructor directions, nominal SLOT!/read roles, and
\ dropped or raw cleanup results. ACCEPTED controls prove the harness resolves
\ this package; UNRESOLVED probes prove the representation leaves stay behind
\ the seal, and a forked child proves the seal itself refuses new definitions.
\
\ The sealed-table disposal test covers the case where no store is created: a caller
\ seals a table and stops, so the table is the only thing it
\ owns. TABLE-DISPOSE frees it and WSTORE:LIVE returns to where it started. The
\ shape this word retires is worth naming, because it is what the checkpoint
\ load would otherwise have had to do to free a table:
\
\   detach the census, unwrap `moved`, fabricate a mapped store, dispose it, and
\   release the census
\
\ That type-checks, and it is a hack: it reaches a private free path by consuming
\ the census's file mapping through DETACH-MAPPING - an atomic, TERMINAL transfer
\ the caller did not want to make - and by fabricating a store that never
\ described any residency. It is recorded here as a rejected alternative so nobody
\ reintroduces it; a table's own exit is the honest fix.
\
\ The builder and buffer legs are the same argument for the module's other two owners.
\ A builder a caller will not seal - because a SLOT! refused, or because it gave up on
\ the load - and a filled buffer whose store was never built are both reachable
\ states this package minted, and neither had any exit: the checker refuses a bare
\ `drop` on either, correctly, and the block behind each is package-private. There was
\ not even a hack available for the builder, since the fabricate-a-store shape above
\ needs a SEALED table. Those legs mint each owner alone, dispose it, and check the
\ counter returns to where it started; the refusal legs further down are where a
\ builder that was actually refused gets given back.

require lib/test.f
require test/checker-assert.f
require lib/string.f
require lib/fs.f
require lib/cad-num-arithmetic.f
require lib/memory.f
require lib/adt/option.f
require lib/adt/result.f
require lib/test/outcome.f
require lib/test/subject.f
require maki/infer/safetensors.f
require maki/infer/weight-store.f

package WSTORE-TEST

34 constant DQ                                  \ "
96 constant BT                                  \ ` placeholder for " in s" literals
$7FFFFFFFFFFFFFFF constant WT-MAX-N

-7799 constant E-WST-FIX                        \ fixture invariant broke (never expected)

512 constant IMG-CAP
create IMG IMG-CAP allot   variable LEN-I
create SUBJ-OUT $400 allot
create SUBJ-ERR $400 allot

\ ---- fixture geometry: two tensors, data bytes are their own index ----------
: J-SYNTH ( -- ptr u8 n )
   s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`BF16`,`shape`:[4],`data_offsets`:[16,24]}}" ;

24 constant DATA-N
16 constant NB-A       8 constant NB-B
0 constant BEG-A       16 constant BEG-B
4 constant TEST-U32-BYTES
NB-A TEST-U32-BYTES - constant LAST-A-U32
LAST-A-U32 1+ constant FIRST-BAD-A-U32
4 constant HIGH-REL
BEG-B HIGH-REL + constant HIGH-DATA
$55 constant HIGH-B0
$66 constant HIGH-B1
$77 constant HIGH-B2
$88 constant HIGH-B3
8 constant SMALL-ARM-BYTES
4 constant SLOT-BYTES
2 constant SLOT-CROSS-OFF
SLOT-CROSS-OFF TEST-U32-BYTES + constant SLOT-CROSS-END
$A5 constant CROSS-B0
$5A constant CROSS-B1
2 constant ARM-TAIL-BYTES
SMALL-ARM-BYTES ARM-TAIL-BYTES - constant ARM-CROSS-OFF

\ ---- the table block TABLE-DISPOSE gives back --------------------------------
\ Mirrors the module's documented table layout: two header cells - slot count and
\ populated count - then three cells per slot (offset, extent, set flag). Derived
\ from those named parts rather than written as one magic byte count, so a layout
\ change reds this leg instead of quietly agreeing with it.
3 constant WT-ROW-CELLS
2 constant WT-TBL-HDR-CELLS                     \ slot count + populated count
WT-ROW-CELLS cells constant WT-ROW-BYTES
WT-MAX-N WT-ROW-BYTES / 1+ constant FAR-SLOT

: TBL-BYTES ( n -- n )                          \ block bytes for a table of n slots
   WT-ROW-BYTES * WT-TBL-HDR-CELLS cells + ;

: SYNTH-PATH ( -- ptr u8 n )  s" /tmp/hb-wst-synth.safetensors" ;

: CLEANUP ( -- )  SYNTH-PATH FS-PATHZ unlink drop ;

\ ---- image builder (the safetensors-test BUILD shape) ------------------------
: SYNTH-BYTE ( n -- n ) {: i:n :}
   i HIGH-DATA     = if HIGH-B0 exit then
   i HIGH-DATA 1 + = if HIGH-B1 exit then
   i HIGH-DATA 2 + = if HIGH-B2 exit then
   i HIGH-DATA 3 + = if HIGH-B3 exit then
   i $FF and ;

: BUILD ( ptr u8 n ptr u8 n n -- n )
   {: da:ptr dcap:n ja:ptr ju:n dcount:n :}
   8 ju + dcount + dcap > if E-STR-CAPACITY throw then
   ju 0 ?do
      ja i + c@ dup BT = if drop DQ then
      da 8 i + + c!
   loop
   8 0 ?do  ju i 8 * rshift $FF and  da i +  c!  loop
   dcount 0 ?do  i SYNTH-BYTE  da 8 ju + i + +  c!  loop
   8 ju + dcount + ;

: BUILD-IMG ( -- )
   IMG IMG-CAP J-SYNTH DATA-N BUILD LEN-I ! ;

: BUILD-SYNTH ( -- )
   BUILD-IMG
   SYNTH-PATH IMG LEN-I @ WRITE-ALL ;

\ ---- option / result assertions (the safetensors-test shapes) ----------------
: MISSING ( -- )
   s" required option was NONE" T-LABEL
   0 0= 0= TTRUE ;

: OPT= ( option<n> n -- ) {: want:n :}
   MATCH option
      none OF MISSING ENDOF
      some OF want T= ENDOF
   ;MATCH ;

: OPT-NONE ( option<n> -- )
   MATCH option
      none OF 0 0= TTRUE ENDOF
      some OF drop 0 0= 0= TTRUE ENDOF
   ;MATCH ;

: OPT-VAL ( option<n> -- n )
   MATCH option
      none OF MISSING -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: CLEANUP-ERR ( n -- )
   drop
   s" cleanup result was err, not ok" T-LABEL
   0 0= 0= TTRUE ;

: RES-OK= ( result<n,n> n -- ) {: want:n :}
   MATCH result
      ok  OF want T= ENDOF
      err OF CLEANUP-ERR ENDOF
   ;MATCH ;

: RES-DROP ( result<n,n> -- )                   \ refusal legs never reach their dispose
   MATCH result
      ok  OF drop ENDOF
      err OF drop ENDOF
   ;MATCH ;

: ID-OF ( SAFET:census ptr u8 n -- SAFET:census n )
   SAFET:FIND OPT-VAL ;

: TAKE-MOVED ( SAFET:map-take -- SAFET:mapping )
   MATCH SAFET:map-take
      moved OF ENDOF
      empty OF E-WST-FIX throw ENDOF
   ;MATCH ;

\ ---- checker-candidate verdict assertions -------------------------------------
: REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- validated-role makers: the fixture's constants are all nonnegative -------
: FIX-BOFF ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-WST-FIX throw ENDOF
      zero OF E-WST-FIX throw ENDOF             overflow OF E-WST-FIX throw ENDOF
      underflow OF E-WST-FIX throw ENDOF        bad-alignment OF E-WST-FIX throw ENDOF
      misaligned OF E-WST-FIX throw ENDOF
   ;MATCH ;

: FIX-BLEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-WST-FIX throw ENDOF
      zero OF E-WST-FIX throw ENDOF             overflow OF E-WST-FIX throw ENDOF
      underflow OF E-WST-FIX throw ENDOF        bad-alignment OF E-WST-FIX throw ENDOF
      misaligned OF E-WST-FIX throw ENDOF
   ;MATCH ;

: FIX-IDX ( CAD-NUM:numeric-result<CAD-NUM:index> -- CAD-NUM:index )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-WST-FIX throw ENDOF
      zero OF E-WST-FIX throw ENDOF             overflow OF E-WST-FIX throw ENDOF
      underflow OF E-WST-FIX throw ENDOF        bad-alignment OF E-WST-FIX throw ENDOF
      misaligned OF E-WST-FIX throw ENDOF
   ;MATCH ;

: >BOFF ( n -- CAD-NUM:byte-off )   CAD-NUM:BYTE-OFF FIX-BOFF ;
: >BLEN ( n -- CAD-NUM:byte-len )   CAD-NUM:BYTE-LEN FIX-BLEN ;
: >IDX  ( n -- CAD-NUM:index )      CAD-NUM:INDEX FIX-IDX ;

\ ---- store builders -------------------------------------------------------------
: MK-ABUF ( SAFET:census -- SAFET:census WSTORE:buffer )   \ both tensors at their data-section offsets
   DATA-N MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   s" a" ID-OF {: ia:n :}
   s" b" ID-OF {: ib:n :}
   ia base BEG-A BYTE+ NB-A SAFET:COPY-DATA? NB-A OPT=
   ib base BEG-B BYTE+ NB-B SAFET:COPY-DATA? NB-B OPT=
   base DATA-N MEM:BYTES-ALLOC-LEN WSTORE:BUFFER ;

: MK-MTBL ( SAFET:census -- SAFET:census WSTORE:table )    \ rows at the MAP-OFFSET? frame
   s" a" ID-OF {: ia:n :}
   s" b" ID-OF {: ib:n :}
   ia SAFET:MAP-OFFSET? OPT-VAL {: moa:n :}
   ib SAFET:MAP-OFFSET? OPT-VAL {: mob:n :}
   2 WSTORE:TABLE-NEW
   0 >IDX moa >BOFF NB-A >BLEN WSTORE:SLOT!
   1 >IDX mob >BOFF NB-B >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: MK-ATBL ( -- WSTORE:table )                   \ rows at the buffer frame
   2 WSTORE:TABLE-NEW
   0 >IDX BEG-A >BOFF NB-A >BLEN WSTORE:SLOT!
   1 >IDX BEG-B >BOFF NB-B >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: READ= ( WSTORE:store n n n -- WSTORE:store ) {: slot:n off:n want:n :}
   slot >IDX off >BOFF WSTORE:U32-LE@? want OPT= ;

: READ-NONE ( WSTORE:store n n -- WSTORE:store ) {: slot:n off:n :}
   slot >IDX off >BOFF WSTORE:U32-LE@? OPT-NONE ;

$03020100 constant LE-0123
$04030201 constant LE-1234
$0F0E0D0C constant LE-1215
$13121110 constant LE-1619
$88776655 constant LE-HIGH

\ ---- fixed reads and slot boundary through both residency arms -------------------
: T-EQUALITY ( -- )
   s" both arms read fixed little-endian values through U32-LE@?" T-LABEL
   BUILD-SYNTH
   SAFET:OPEN SYNTH-PATH SAFET:MAP-FILE SAFET:PARSE SAFET:DETACH   \ ( c )
   SAFET-MAP:LIVE 1 T=
   MK-ABUF                                      \ ( c abuf ) copied while the census owns the bytes
   swap MK-MTBL                                 \ ( abuf c mtbl )
   swap SAFET:DETACH-MAPPING TAKE-MOVED         \ ( abuf mtbl c m )
   swap SAFET:RELEASE                           \ ( abuf mtbl m )
   swap                                         \ ( abuf m mtbl )
   WSTORE-STORE:MAPPED                          \ ( abuf mstore )
   swap MK-ATBL                                 \ ( mstore abuf atbl )
   WSTORE-STORE:ALLOCATED                       \ ( mstore astore )
   WSTORE:LIVE 3 T=                             \ two tables + one buffer record live
   s" allocated reads are aligned, unaligned, slot-relative, and bounded" T-LABEL
   0 0 LE-0123 READ=
   0 1 LE-1234 READ=
   0 LAST-A-U32 LE-1215 READ=
   0 FIRST-BAD-A-U32 READ-NONE
   1 0 LE-1619 READ=
   1 HIGH-REL LE-HIGH READ=
   swap                                         \ ( astore mstore )
   s" mapped reads have the same fixed expectations and boundary" T-LABEL
   0 0 LE-0123 READ=
   0 1 LE-1234 READ=
   0 LAST-A-U32 LE-1215 READ=
   0 FIRST-BAD-A-U32 READ-NONE
   1 0 LE-1619 READ=
   1 HIGH-REL LE-HIGH READ=
   s" mapped DISPOSE gives the mapping back to the kernel" T-LABEL
   WSTORE:DISPOSE LEN-I @ RES-OK=               \ ( astore )
   SAFET-MAP:LIVE 0 T=
   s" allocated DISPOSE releases the buffer" T-LABEL
   WSTORE:DISPOSE DATA-N RES-OK=
   WSTORE:LIVE 0 T=
   SAFET:LIVE-OWNERS 0 T=
   CLEANUP ;

\ ---- a sealed table with no arm owner disposes on its own ---------------------------
\ Entered at zero live blocks (T-EQUALITY leaves it there), so the counter proves
\ the whole life of the table: one block after SEAL, none after TABLE-DISPOSE. No
\ census and no mapping take part, which is the point - this is the state a caller
\ that stops before creating a store is left holding, and the test would be impossible to
\ write at all if the table's only exit ran through a store.
: T-TABLE-DISPOSE ( -- )
   s" a sealed table that never became a store disposes on its own" T-LABEL
   WSTORE:LIVE 0 T=
   MK-ATBL                                      \ ( tbl ) sealed, sole owner of its block
   WSTORE:LIVE 1 T=
   WSTORE:TABLE-DISPOSE 2 TBL-BYTES RES-OK=     \ ok carries the block it gave back
   WSTORE:LIVE 0 T=
   SAFET-MAP:LIVE 0 T=                          \ nothing was ever mapped for this leg
   SAFET:LIVE-OWNERS 0 T= ;

\ ---- the other two owners dispose on their own too -----------------------------------
\ Same argument and same counter discipline as the leg above, for the two owners that
\ had no exit before: a builder whose population was refused, and a filled buffer
\ whose store never got built. Each leg is entered at zero live blocks and returns
\ there, so the counter covers the whole life of the owner. What ok carries is checked
\ against the named layout parts, not a magic number: the block a builder gives back
\ is the same size a sealed table's is (SEAL retypes one allocation, it does not copy
\ it), and a buffer gives back the bytes it owned, not its two-cell record.
: MK-BUF8 ( -- WSTORE:buffer )                  \ eight owned bytes, never made a store
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES WSTORE:BUFFER ;

: MK-BUF8N ( -- WSTORE:buffer )                 \ the same, allocated and owned in one step
   8 MEM:BYTES-ALLOC-LEN WSTORE:BUFFER-NEW ;

: T-OWNER-EXITS ( -- )
   s" an unsealed builder disposes on its own" T-LABEL
   WSTORE:LIVE 0 T=
   2 WSTORE:TABLE-NEW
   WSTORE:LIVE 1 T=
   WSTORE:BUILDER-DISPOSE 2 TBL-BYTES RES-OK=
   WSTORE:LIVE 0 T=
   s" a half-populated builder gives back the same block" T-LABEL
   2 WSTORE:TABLE-NEW
   0 >IDX 0 >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT!
   WSTORE:LIVE 1 T=
   WSTORE:BUILDER-DISPOSE 2 TBL-BYTES RES-OK=
   WSTORE:LIVE 0 T=
   s" a filled buffer that never became a store disposes on its own" T-LABEL
   MK-BUF8
   WSTORE:LIVE 1 T=
   WSTORE:BUFFER-DISPOSE 8 RES-OK=              \ the bytes it owned, not its record
   WSTORE:LIVE 0 T=
   s" BUFFER-NEW allocates and owns in one step, and disposes the same way" T-LABEL
   MK-BUF8N
   WSTORE:LIVE 1 T=
   WSTORE:BUFFER-DISPOSE 8 RES-OK=
   WSTORE:LIVE 0 T=
   SAFET-MAP:LIVE 0 T=                          \ nothing was ever mapped for this leg
   SAFET:LIVE-OWNERS 0 T= ;

\ ---- refusal legs: table construction ----------------------------------------------
: TBL-BURN ( WSTORE:table -- )                  \ consume a table through a real store
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES WSTORE:BUFFER
   swap WSTORE-STORE:ALLOCATED
   WSTORE:DISPOSE RES-DROP ;

: TN-ZERO ( -- )   0 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;
: TN-HUGE ( -- )   $10001 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;   \ MAX-SLOTS + 1

\ ---- the three SLOT! refusals, with the builder given back ---------------------------
\ SLOT! is stack-preserving ( tbuilder -- tbuilder ), so the population step runs as a
\ quotation under `catch` and a refusal leaves the builder exactly where it was - the
\ SAFET:LOAD / GPT2LOAD:PREPARE discipline. Each test then hands the block back through
\ BUILDER-DISPOSE and rethrows the code TTHROWSQ is asserting, so it proves two things
\ at once: the named refusal still fires, and it no longer costs a block. Before
\ BUILDER-DISPOSE existed none of this could be written at all and each leg leaked one
\ builder; the residue below counted six such leaks.
: BUILDER-BACK ( WSTORE:tbuilder n n -- )       \ the code catch reported, the slot count
   {: code:n nslots:n :}
   WSTORE:BUILDER-DISPOSE nslots TBL-BYTES RES-OK=
   code 0= if E-WST-FIX throw then              \ the step was supposed to refuse
   code throw ;

: SET-RANGE ( WSTORE:tbuilder -- WSTORE:tbuilder )   \ slot 2 of a two-slot table
   2 >IDX 0 >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT! ;

: SET-DUP ( WSTORE:tbuilder -- WSTORE:tbuilder )     \ slot 0 written twice
   0 >IDX 0 >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT!
   0 >IDX TEST-U32-BYTES >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT! ;

: SET-OVER ( WSTORE:tbuilder -- WSTORE:tbuilder )    \ row end overflows a cell
   0 >IDX WT-MAX-N >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT! ;

: SL-RANGE ( -- )   2 WSTORE:TABLE-NEW [: SET-RANGE ;] catch 2 BUILDER-BACK ;
: SL-DUP   ( -- )   1 WSTORE:TABLE-NEW [: SET-DUP ;]   catch 1 BUILDER-BACK ;
: SL-OVER  ( -- )   1 WSTORE:TABLE-NEW [: SET-OVER ;]  catch 1 BUILDER-BACK ;

\ ---- the two SEAL refusals, which still strand ---------------------------------------
\ These cannot be written the way the three above are. SEAL is ( tbuilder -- table ), so
\ a `catch` over it would have to hold a BUILDER on the refusal arm and a TABLE on the
\ success arm, and no stack effect can say that today. The missing piece is the
\ linear-scope combinator (habu-checker-linear-scope-6218899c) - a scope that disposes
\ the owner it holds when its body throws - and until it lands each of these legs
\ strands its builder. That is the whole of the residue T-TABLE-ERRORS asserts.
: SEAL-UNSET ( -- )
   2 WSTORE:TABLE-NEW
   0 >IDX 0 >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SEAL-EMPTY ( -- )
   1 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;

: T-TABLE-ERRORS ( -- )
   s" every table-construction refusal throws its named code" T-LABEL
   [: TN-ZERO ;]    WSTORE:E-SLOTS  TTHROWSQ
   [: TN-HUGE ;]    WSTORE:E-SLOTS  TTHROWSQ
   WSTORE:LIVE 0 T=                             \ refused before any allocation
   s" a refused SLOT! gives its builder back instead of stranding it" T-LABEL
   [: SL-RANGE ;]   WSTORE:E-SLOT   TTHROWSQ
   [: SL-DUP ;]     WSTORE:E-SET    TTHROWSQ
   [: SL-OVER ;]    WSTORE:E-EXTENT TTHROWSQ
   WSTORE:LIVE 0 T=                             \ all three disposed; nothing left over
   s" a refused SEAL still strands its builder (the linear-scope gap)" T-LABEL
   [: SEAL-UNSET ;] WSTORE:E-UNSET  TTHROWSQ
   [: SEAL-EMPTY ;] WSTORE:E-UNSET  TTHROWSQ
   WSTORE:LIVE 2 T= ;                           \ exactly the two SEAL strands

\ ---- value-level read refusals -------------------------------------------------------
: SMALL-BYTE ( n -- n ) {: off:n :}
   off SLOT-BYTES     = if CROSS-B0 exit then
   off SLOT-BYTES 1 + = if CROSS-B1 exit then
   off $10 + ;

: SMALL-FILL ( ptr u8 -- ) {: base:ptr :}
   SMALL-ARM-BYTES 0 ?do i SMALL-BYTE base i BYTE+ c! loop ;

: CROSS-GUARD ( ptr u8 -- ) {: base:ptr :}
   base SLOT-BYTES BYTE+ c@ CROSS-B0 <> if E-WST-FIX throw then
   base SLOT-BYTES 1 + BYTE+ c@ CROSS-B1 <> if E-WST-FIX throw then ;

: SMALL-TBL ( -- WSTORE:table )
   1 WSTORE:TABLE-NEW
   0 >IDX 0 >BOFF SLOT-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: MK-ASTORE ( -- WSTORE:store )                 \ four-byte slot in eight populated bytes
   SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   base SMALL-FILL
   base CROSS-GUARD
   base SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN WSTORE:BUFFER
   SMALL-TBL
   WSTORE-STORE:ALLOCATED ;

257 constant WIDE-SLOTS                         \ crosses the low byte of the slot ordinal
WIDE-SLOTS 1- constant WIDE-LAST
$123 constant WIDE-OFF                          \ crosses the low byte of the stored row offset
$400 constant WIDE-BYTES
$11 constant BYTE-RAMP
1 constant WIDE-FIRST
5 constant BASE-FIRST
$44332211 constant WIDE-VALUE
$88776655 constant BASE-VALUE

: PATTERN! ( ptr u8 n n -- ) {: base:ptr off:n first:n :}
   TEST-U32-BYTES 0 ?do
      i first + BYTE-RAMP * base off i + >BOFF CAD-NUM:BYTE+ c!
   loop ;

: WIDE-DATA! ( ptr u8 -- )
   dup 0 BASE-FIRST PATTERN!
   WIDE-OFF WIDE-FIRST PATTERN! ;

: WIDE-ROW-OFF ( n -- n )
   WIDE-LAST = if WIDE-OFF else 0 then ;

: MK-WIDE-STORE ( -- WSTORE:store )
   WIDE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   base WIDE-DATA!
   base WIDE-BYTES MEM:BYTES-ALLOC-LEN WSTORE:BUFFER
   WIDE-SLOTS WSTORE:TABLE-NEW
   WIDE-SLOTS 0 ?do
      i >IDX i WIDE-ROW-OFF >BOFF TEST-U32-BYTES >BLEN WSTORE:SLOT!
   loop
   WSTORE:SEAL
   WSTORE-STORE:ALLOCATED ;

: T-WIDE-ROWS ( -- )
   s" native table cells preserve high bytes in counts, indexes, and offsets" T-LABEL
   MK-WIDE-STORE
   0 0 BASE-VALUE READ=
   WIDE-LAST 0 WIDE-VALUE READ=
   WSTORE:DISPOSE WIDE-BYTES RES-OK=
   WSTORE:LIVE 0 T= ;

: MK-EXTSTORE ( -- WSTORE:store )               \ row reaches past the allocated arm
   SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES WSTORE:BUFFER
   1 WSTORE:TABLE-NEW
   0 >IDX ARM-CROSS-OFF >BOFF SMALL-ARM-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:ALLOCATED ;

: MK-MAP-EXTSTORE ( -- WSTORE:store )           \ row reaches past the mapped arm
   IMG LEN-I @ SAFET:LOAD-SPAN
   SAFET:DETACH-MAPPING TAKE-MOVED
   swap SAFET:RELEASE
   1 WSTORE:TABLE-NEW
   0 >IDX LEN-I @ ARM-TAIL-BYTES - >BOFF SMALL-ARM-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:MAPPED ;

: T-ACCESS ( -- )
   s" bad slot, overflow, and slot crossing return NONE with the store" T-LABEL
   SLOT-CROSS-END SLOT-BYTES > TTRUE
   SLOT-CROSS-END SMALL-ARM-BYTES <= TTRUE
   MK-ASTORE
   FAR-SLOT 0 READ-NONE
   0 WT-MAX-N READ-NONE
   0 SLOT-CROSS-OFF READ-NONE
   WSTORE:DISPOSE SMALL-ARM-BYTES RES-OK=
   s" allocated and mapped arm crossings return NONE" T-LABEL
   MK-EXTSTORE 0 0 READ-NONE WSTORE:DISPOSE SMALL-ARM-BYTES RES-OK=
   BUILD-IMG
   MK-MAP-EXTSTORE 0 0 READ-NONE WSTORE:DISPOSE 0 RES-OK=
   WSTORE:LIVE 2 T=
   SAFET:LIVE-OWNERS 0 T=
   SAFET-MAP:LIVE 0 T= ;

\ ---- released backing pages: every NONE path must avoid the arm bytes ---------
PTR-VARIABLE REV-BASE

: REV-MTBL ( -- WSTORE:table )
   2 WSTORE:TABLE-NEW
   0 >IDX 0 >BOFF SLOT-BYTES >BLEN WSTORE:SLOT!
   1 >IDX LEN-I @ ARM-TAIL-BYTES - >BOFF SMALL-ARM-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: MAP-IMAGE ( -- SAFET:mapping )
   BUILD-IMG
   LEN-I @ MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop
   dup REV-BASE !
   IMG over LEN-I @ BYTE-COPY
   LEN-I @ SAFET:LOAD-SPAN
   SAFET:DETACH-MAPPING TAKE-MOVED
   swap SAFET:RELEASE ;

: REVOKE-MAP ( SAFET:mapping -- SAFET:mapping )
   REV-BASE @ LEN-I @ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: REVOKED-MSTORE ( -- WSTORE:store )
   MAP-IMAGE
   REV-MTBL
   swap REVOKE-MAP swap
   WSTORE-STORE:MAPPED ;

: REV-ATBL ( -- WSTORE:table )
   2 WSTORE:TABLE-NEW
   0 >IDX 0 >BOFF SLOT-BYTES >BLEN WSTORE:SLOT!
   1 >IDX ARM-CROSS-OFF >BOFF SMALL-ARM-BYTES >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: REVOKED-BUF ( -- WSTORE:buffer )
   SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop
   dup REV-BASE !
   dup SMALL-FILL
   dup CROSS-GUARD
   SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN WSTORE:BUFFER
   REV-BASE @ SMALL-ARM-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: REVOKED-ASTORE ( -- WSTORE:store )
   REV-ATBL
   REVOKED-BUF
   swap WSTORE-STORE:ALLOCATED ;

: REFUSE ( WSTORE:store n n -- WSTORE:store ) {: slot:n off:n :}
   slot >IDX off >BOFF WSTORE:U32-LE@?
   MATCH option
      none OF ENDOF
      some OF drop E-WST-FIX throw ENDOF
   ;MATCH ;

public

: PROBE-M-BAD  ( -- ) REVOKED-MSTORE FAR-SLOT 0 REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-M-OVER ( -- ) REVOKED-MSTORE 0 WT-MAX-N REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-M-SLOT ( -- ) REVOKED-MSTORE 0 SLOT-CROSS-OFF REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-M-ARM  ( -- ) REVOKED-MSTORE 1 0 REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-A-BAD  ( -- ) REVOKED-ASTORE FAR-SLOT 0 REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-A-OVER ( -- ) REVOKED-ASTORE 0 WT-MAX-N REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-A-SLOT ( -- ) REVOKED-ASTORE 0 SLOT-CROSS-OFF REFUSE WSTORE:DISPOSE RES-DROP ;
: PROBE-A-ARM  ( -- ) REVOKED-ASTORE 1 0 REFUSE WSTORE:DISPOSE RES-DROP ;

private

: SUBJECT-EXITS ( ptr u8 n -- )
   SUBJ-OUT $400 >LEN SUBJ-ERR $400 >LEN 2000 >MS SUBJECT:RUN
   0 T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

: T-REVOKED ( -- )
   s" every mapped refusal avoids its released backing page" T-LABEL
   s" WSTORE-TEST:PROBE-M-BAD" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-M-OVER" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-M-SLOT" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-M-ARM" SUBJECT-EXITS
   s" every allocated refusal avoids its released backing page" T-LABEL
   s" WSTORE-TEST:PROBE-A-BAD" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-A-OVER" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-A-SLOT" SUBJECT-EXITS
   s" WSTORE-TEST:PROBE-A-ARM" SUBJECT-EXITS ;

\ ---- static half: ownership and nominal read roles ----------------------------------
: T-LINEAR ( -- )
   s" a store cannot be duplicated, dropped, or stored" T-LABEL
   s" WST-BAD-STORE-DUP ( WSTORE:store -- WSTORE:store WSTORE:store ) dup" REJECTED
   s" WST-BAD-STORE-DROP ( WSTORE:store -- ) drop" REJECTED
   s" WST-BAD-STORE-STORE ( WSTORE:store ptr n -- ) !" REJECTED
   s" a builder, table, and buffer are linear too" T-LABEL
   s" WST-BAD-TB-DUP ( WSTORE:tbuilder -- WSTORE:tbuilder WSTORE:tbuilder ) dup" REJECTED
   s" WST-BAD-TB-DROP ( WSTORE:tbuilder -- ) drop" REJECTED
   s" WST-BAD-TBL-DUP ( WSTORE:table -- WSTORE:table WSTORE:table ) dup" REJECTED
   s" WST-BAD-TBL-DROP ( WSTORE:table -- ) drop" REJECTED
   s" WST-BAD-BUF-DUP ( WSTORE:buffer -- WSTORE:buffer WSTORE:buffer ) dup" REJECTED
   s" WST-BAD-BUF-DROP ( WSTORE:buffer -- ) drop" REJECTED
   s" SEAL consumes its builder exactly once" T-LABEL
   s" WST-BAD-SEAL-KEEPS ( WSTORE:tbuilder -- WSTORE:tbuilder WSTORE:table ) WSTORE:SEAL" REJECTED
   s" WST-BAD-SEAL-TWICE ( WSTORE:tbuilder -- WSTORE:table WSTORE:table ) WSTORE:SEAL WSTORE:SEAL" REJECTED
   s" a sealed table is immutable and a builder is not a table" T-LABEL
   s" WST-BAD-MUTATE-SEALED ( WSTORE:table CAD-NUM:index CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:table ) WSTORE:SLOT!" REJECTED
   s" the two arms cannot be confused at construction" T-LABEL
   s" WST-BAD-CTOR-MA ( SAFET:mapping WSTORE:table -- WSTORE:store ) WSTORE-STORE:ALLOCATED" REJECTED
   s" WST-BAD-CTOR-AM ( WSTORE:buffer WSTORE:table -- WSTORE:store ) WSTORE-STORE:MAPPED" REJECTED
   s" DISPOSE consumes the store exactly once and its result is not droppable" T-LABEL
   s" WST-BAD-DOUBLE-DISPOSE ( WSTORE:store -- result<n,n> result<n,n> ) WSTORE:DISPOSE WSTORE:DISPOSE" REJECTED
   s" WST-BAD-DISPOSE-KEEPS ( WSTORE:store -- WSTORE:store result<n,n> ) WSTORE:DISPOSE" REJECTED
   s" WST-BAD-USE-AFTER ( WSTORE:store CAD-NUM:index CAD-NUM:byte-off -- result<n,n> WSTORE:store option<n> ) WSTORE:DISPOSE WSTORE:U32-LE@?" REJECTED
   s" WST-BAD-RESULT-DROPPED ( WSTORE:store -- ) WSTORE:DISPOSE" REJECTED
   s" WST-BAD-RESULT-RAW ( WSTORE:store -- n ) WSTORE:DISPOSE 1 +" REJECTED
   s" TABLE-DISPOSE consumes its table exactly once and yields a real union" T-LABEL
   s" WST-BAD-TD-TWICE ( WSTORE:table -- result<n,n> result<n,n> ) WSTORE:TABLE-DISPOSE WSTORE:TABLE-DISPOSE" REJECTED
   s" WST-BAD-TD-KEEPS ( WSTORE:table -- WSTORE:table result<n,n> ) WSTORE:TABLE-DISPOSE" REJECTED
   s" WST-BAD-TD-DROPPED ( WSTORE:table -- ) WSTORE:TABLE-DISPOSE" REJECTED
   s" WST-BAD-TD-RAW ( WSTORE:table -- n ) WSTORE:TABLE-DISPOSE 1 +" REJECTED
   s" an unsealed builder is not a table, so the table exit refuses it" T-LABEL
   s" WST-BAD-TD-BUILDER ( WSTORE:tbuilder -- result<n,n> ) WSTORE:TABLE-DISPOSE" REJECTED
   s" WST-BAD-TD-STORE ( WSTORE:store -- result<n,n> ) WSTORE:TABLE-DISPOSE" REJECTED
   s" BUILDER-DISPOSE consumes its builder exactly once and yields a real union" T-LABEL
   s" WST-BAD-BD-TWICE ( WSTORE:tbuilder -- result<n,n> result<n,n> ) WSTORE:BUILDER-DISPOSE WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-BD-KEEPS ( WSTORE:tbuilder -- WSTORE:tbuilder result<n,n> ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-BD-DROPPED ( WSTORE:tbuilder -- ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-BD-RAW ( WSTORE:tbuilder -- n ) WSTORE:BUILDER-DISPOSE 1 +" REJECTED
   s" WST-BAD-BD-AFTER ( WSTORE:tbuilder -- result<n,n> WSTORE:table ) WSTORE:BUILDER-DISPOSE WSTORE:SEAL" REJECTED
   s" the builder exit takes a builder and nothing else" T-LABEL
   s" WST-BAD-BD-TABLE ( WSTORE:table -- result<n,n> ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-BD-BUFFER ( WSTORE:buffer -- result<n,n> ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-BD-STORE ( WSTORE:store -- result<n,n> ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" BUFFER-DISPOSE consumes its buffer exactly once and yields a real union" T-LABEL
   s" WST-BAD-BFD-TWICE ( WSTORE:buffer -- result<n,n> result<n,n> ) WSTORE:BUFFER-DISPOSE WSTORE:BUFFER-DISPOSE" REJECTED
   s" WST-BAD-BFD-KEEPS ( WSTORE:buffer -- WSTORE:buffer result<n,n> ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" WST-BAD-BFD-DROPPED ( WSTORE:buffer -- ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" WST-BAD-BFD-RAW ( WSTORE:buffer -- n ) WSTORE:BUFFER-DISPOSE 1 +" REJECTED
   s" WST-BAD-BFD-AFTER ( WSTORE:buffer WSTORE:table -- result<n,n> WSTORE:store ) WSTORE:BUFFER-DISPOSE swap WSTORE-STORE:ALLOCATED" REJECTED
   s" the buffer exit takes a buffer and nothing else" T-LABEL
   s" WST-BAD-BFD-TABLE ( WSTORE:table -- result<n,n> ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" WST-BAD-BFD-BUILDER ( WSTORE:tbuilder -- result<n,n> ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" WST-BAD-BFD-STORE ( WSTORE:store -- result<n,n> ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" a disposed owner cannot be handed to another owner's exit either" T-LABEL
   s" WST-BAD-BD-THEN-BFD ( WSTORE:tbuilder -- result<n,n> result<n,n> ) WSTORE:BUILDER-DISPOSE WSTORE:BUFFER-DISPOSE" REJECTED ;

\ Its own word rather than more lines in T-LINEAR: that word already carries some fifty
\ candidates, and appending five more stopped the harness outright instead of reporting a
\ verdict on them. The candidates below are unchanged from what was appended there and
\ pass exactly as written, so the split is the fix.
: T-BUFFER-NEW ( -- )
   s" BUFFER-NEW yields one linear buffer and never raw memory" T-LABEL
   s" WST-BAD-BN-DUP ( CAD-NUM:alloc-byte-len -- WSTORE:buffer WSTORE:buffer ) WSTORE:BUFFER-NEW dup" REJECTED
   s" WST-BAD-BN-DROP ( CAD-NUM:alloc-byte-len -- ) WSTORE:BUFFER-NEW" REJECTED
   s" WST-BAD-BN-RAW ( CAD-NUM:alloc-byte-len -- ptr u8 ) WSTORE:BUFFER-NEW" REJECTED
   s" WST-BAD-BN-BARE ( n -- WSTORE:buffer ) WSTORE:BUFFER-NEW" REJECTED
   s" WST-BAD-BN-TABLE ( CAD-NUM:alloc-byte-len -- WSTORE:table ) WSTORE:BUFFER-NEW" REJECTED
   s" the record allocation stays behind the seal" T-LABEL
   s" WST-BAD-RB-STEP ( -- ) WSTORE:RB-STEP" UNRESOLVED ;

: T-READ-TYPES ( -- )
   s" reads require a sealed store and both nominal roles" T-LABEL
   s" WST-BAD-BUILDER-READ ( WSTORE:tbuilder CAD-NUM:index CAD-NUM:byte-off -- WSTORE:tbuilder option<n> ) WSTORE:U32-LE@?" REJECTED
   s" WST-BAD-READ-AMBIENT ( CAD-NUM:index CAD-NUM:byte-off -- option<n> ) WSTORE:U32-LE@?" REJECTED
   s" WST-BAD-READ-RAW-IDX ( WSTORE:store n CAD-NUM:byte-off -- WSTORE:store option<n> ) WSTORE:U32-LE@?" REJECTED
   s" WST-BAD-READ-RAW-OFF ( WSTORE:store CAD-NUM:index n -- WSTORE:store option<n> ) WSTORE:U32-LE@?" REJECTED
   s" WST-BAD-READ-SWAP ( WSTORE:store CAD-NUM:byte-off CAD-NUM:index -- WSTORE:store option<n> ) WSTORE:U32-LE@?" REJECTED ;

variable PUB-WID
variable PRI-WID

: BIND-WIDS ( -- )
   s" WSTORE" XREF-NAMESPACE-WL XREF-FIND-WL {: ns:ptr :}
   s" package WSTORE has exported and private word lists" T-LABEL
   ns XREF-FOUND? dup TTRUE
   if ns XREF-START PUB-WID ! ns XREF-LEN PRI-WID ! then ;

: IN-GLOBAL? ( ptr u8 n -- bool )   0 XREF-FIND-WL XREF-FOUND? ;
: IN-EXPORTED? ( ptr u8 n -- bool ) PUB-WID @ XREF-FIND-WL XREF-FOUND? ;
: IN-PRIVATE? ( ptr u8 n -- bool )  PRI-WID @ XREF-FIND-WL XREF-FOUND? ;

: GONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IN-GLOBAL? TFALSE
   a u IN-EXPORTED? TFALSE
   a u IN-PRIVATE? TFALSE ;

: T-REMOVED ( -- )
   BIND-WIDS
   s" dictionary probes have live witnesses in all three word lists" T-LABEL
   s" DEFLINEAR" IN-GLOBAL? TTRUE
   s" DISPOSE" IN-EXPORTED? TTRUE
   s" TBL-FREE" IN-PRIVATE? TTRUE
   s" retired access and callback words are absent from every word list" T-LABEL
   s" WITH-SLOT" GONE
   s" PARK" GONE
   s" RUN-PARKED" GONE
   s" WS-BODY" GONE
   s" WS-OFF" GONE
   s" WS-LEN" GONE
   s" WS-RES" GONE
   s" WS-RAN" GONE ;

: T-SURFACE ( -- )
   s" the public surface resolves (controls)" T-LABEL
   s" WST-OK-POLICY-M ( -- WSTORE:residency ) WSTORE-RESIDENCY:MAPPED" ACCEPTED
   s" WST-OK-POLICY-A ( -- WSTORE:residency ) WSTORE-RESIDENCY:ALLOCATED" ACCEPTED
   s" WST-OK-CTOR-M ( SAFET:mapping WSTORE:table -- WSTORE:store ) WSTORE-STORE:MAPPED" ACCEPTED
   s" WST-OK-CTOR-A ( WSTORE:buffer WSTORE:table -- WSTORE:store ) WSTORE-STORE:ALLOCATED" ACCEPTED
   s" WST-OK-SLOT ( WSTORE:tbuilder CAD-NUM:index CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:tbuilder ) WSTORE:SLOT!" ACCEPTED
   s" WST-BAD-SLOT-RAW ( WSTORE:tbuilder n CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:tbuilder ) WSTORE:SLOT!" REJECTED
   s" WST-OK-U32 ( WSTORE:store CAD-NUM:index CAD-NUM:byte-off -- WSTORE:store option<n> ) WSTORE:U32-LE@?" ACCEPTED
   s" WST-OK-SEAL ( WSTORE:tbuilder -- WSTORE:table ) WSTORE:SEAL" ACCEPTED
   s" WST-OK-DISPOSE ( WSTORE:store -- result<n,n> ) WSTORE:DISPOSE" ACCEPTED
   s" WST-OK-TABLE-DISPOSE ( WSTORE:table -- result<n,n> ) WSTORE:TABLE-DISPOSE" ACCEPTED
   s" WST-OK-BUILDER-DISPOSE ( WSTORE:tbuilder -- result<n,n> ) WSTORE:BUILDER-DISPOSE" ACCEPTED
   s" WST-OK-BUFFER-DISPOSE ( WSTORE:buffer -- result<n,n> ) WSTORE:BUFFER-DISPOSE" ACCEPTED
   s" WST-OK-BUFFER-NEW ( CAD-NUM:alloc-byte-len -- WSTORE:buffer ) WSTORE:BUFFER-NEW" ACCEPTED
   s" the representation stays behind the seal" T-LABEL
   s" WST-BAD-MINT-TB ( ptr u8 -- WSTORE:tbuilder ) WSTORE:MINT-TBUILDER" UNRESOLVED
   s" WST-BAD-TB-BLOCK ( WSTORE:tbuilder -- WSTORE:tbuilder ptr n ) WSTORE:TB>BLOCK" UNRESOLVED
   s" WST-BAD-TB-TABLE ( WSTORE:tbuilder -- WSTORE:table ) WSTORE:TB>TABLE" UNRESOLVED
   s" WST-BAD-TBL-BLOCK ( WSTORE:table -- WSTORE:table ptr n ) WSTORE:TBL>BLOCK" UNRESOLVED
   s" WST-BAD-TAKE-TBL ( WSTORE:table -- ptr n ) WSTORE:TAKE-TABLE" UNRESOLVED
   s" WST-BAD-MINT-BUF ( ptr u8 -- WSTORE:buffer ) WSTORE:MINT-BUFFER" UNRESOLVED
   s" WST-BAD-BUF-REC ( WSTORE:buffer -- WSTORE:buffer ptr n ) WSTORE:BUF>REC" UNRESOLVED
   s" WST-BAD-TAKE-BUF ( WSTORE:buffer -- ptr n ) WSTORE:TAKE-BUFFER" UNRESOLVED
   s" WST-BAD-BLK-BYTES ( ptr n -- ptr u8 ) WSTORE:BLK>BYTES" UNRESOLVED
   s" WST-BAD-BLK-FREE ( ptr n -- n ) WSTORE:BLK-FREE" UNRESOLVED
   s" WST-BAD-BOFF ( CAD-NUM:byte-off -- n ) WSTORE:BOFF>N" UNRESOLVED
   s" WST-BAD-BLEN ( CAD-NUM:byte-len -- n ) WSTORE:BLEN>N" UNRESOLVED
   s" WST-BAD-ABLEN ( CAD-NUM:alloc-byte-len -- n ) WSTORE:ABLEN>N" UNRESOLVED
   s" no public word hands back a raw pointer" T-LABEL
   s" WST-BAD-SLOT-PTR ( WSTORE:store n -- WSTORE:store ptr u8 n ) WSTORE:SLOT-PTR" UNRESOLVED
   s" WST-BAD-BASE ( WSTORE:store -- WSTORE:store ptr u8 ) WSTORE:BASE" UNRESOLVED ;

: T-SEALED ( -- )
   s" the package seal refuses new definitions into WSTORE" T-LABEL
   s" package WSTORE : WST-FORGE ( ptr u8 -- WSTORE:buffer ) MINT-BUFFER ; ;package"
   SUBJ-OUT $400 >LEN SUBJ-ERR $400 >LEN 2000 >MS SUBJECT:RUN
   ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

public

\ Runs AFTER ;package (the json-read-test arrangement): the SUBJECT child forks
\ from the running process, so no package may be open when T-SEALED evaluates
\ `package WSTORE` in the child - a fork inside an open package would turn the
\ probe into a nested-package reject instead of the seal refusal under test.
: RUN ( -- )
   T-RESET
   T-LINEAR
   T-READ-TYPES
   T-REMOVED
   T-BUFFER-NEW
   T-SURFACE
   T-SEALED
   T-EQUALITY
   T-WIDE-ROWS
   T-TABLE-DISPOSE
   T-OWNER-EXITS
   T-TABLE-ERRORS
   T-ACCESS
   T-REVOKED
   s" final leak accounting: only the documented throw strands remain" T-LABEL
   WSTORE:LIVE 2 T=                             \ exactly the two SEAL refusals
   SAFET:LIVE-OWNERS 0 T=
   SAFET-MAP:LIVE 0 T=
   T-REPORT ;

;package

WSTORE-TEST:RUN
