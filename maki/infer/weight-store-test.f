\ weight-store-test.f - focused coverage for the weight-store residency value
\ (maki/infer/weight-store.f, inference leaf S3d). Run standalone:
\ bin/hb --load maki/infer/weight-store-test.f
\
\ The equality half is the point of the module: the SAME fixture bytes are
\ served through BOTH residency arms and compared byte for byte. The mapped arm
\ is built from a real SAFET session over a synthetic safetensors file on disk
\ (OPEN / MAP-FILE / PARSE / DETACH / DETACH-MAPPING - the exact production
\ path); the allocated arm is a MEM buffer filled from the same census with
\ COPY-DATA? before the mapping leaves. Disposal is proved on both arms: the
\ mapped DISPOSE gives the mapping back to the kernel (SAFET-MAP:LIVE drops) and
\ the allocated DISPOSE releases the buffer (ok carries its byte count and
\ WSTORE:LIVE drops).
\
\ The refusal half feeds every named throw its minimal trigger: bad slot count,
\ out-of-range and negative slot, double-set slot, row-end overflow, unset-slot
\ SEAL, out-of-range slot at access, a row past the arm's bytes on both arms,
\ and a row past each residency arm's bytes. A throw unwinds past deconstructed
\ linear owners, so each refusal leg asserts the exact leak-counter residue it
\ leaves; the suite's final assertions pin the totals and prove no kernel mapping
\ is leaked (the mapped refusal uses an adopted image).
\
\ WHICH REFUSALS COST A BLOCK, AND WHY THAT SPLIT IS THE POINT. The four SLOT!
\ refusals run their population step as a stack-preserving quotation under `catch`,
\ so the builder survives the refusal and the leg gives it back through
\ BUILDER-DISPOSE - they cost nothing now, and the residue after them is 0. The two
\ SEAL refusals cannot be written that way, because a `catch` over SEAL would have to
\ hold a builder on one arm and a table on the other, so each still strands one
\ builder; the access refusals still strand their arm owner and table, because MATCH
\ had already deconstructed the store when the throw fired. Every number in the leak
\ accounting is therefore a statement about a NAMED missing capability (the
\ linear-scope combinator) rather than an accepted leak, and moving a block from one
\ column to the other is what changes the totals.
\
\ The static half feeds bad definitions to the checker itself: store, table,
\ builder, and buffer linearity (no dup / drop / store / reuse), double DISPOSE,
\ arm confusion in both constructor directions, SLOT! on a sealed table (the
\ no-mutable-table proof), WITH-SLOT on an unsealed builder, a dropped or
\ raw-read cleanup result, and the WITH-SLOT escape negative - a quotation
\ declared to return the span pointer is rejected at check time, mirroring the
\ SAFET fixtures. ACCEPTED controls prove the harness resolves this package;
\ UNRESOLVED probes prove the representation leaves stay behind the seal, and a
\ forked child proves the seal itself refuses new definitions into WSTORE.
\
\ The sealed-table disposal leg covers the case a store never appears: a caller
\ seals a table and then decides not to commit, so the table is the only thing it
\ owns. TABLE-DISPOSE frees it and WSTORE:LIVE returns to where it started. The
\ shape this word retires is worth naming, because it is what the bind
\ transaction would otherwise have had to do to free a table:
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
\ the transaction - and a filled buffer whose store was never built are both reachable
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
create GRAB-BUF 64 allot   variable GRAB-LEN
create KEEP-BUF 64 allot
create SUBJ-OUT $400 allot
create SUBJ-ERR $400 allot

\ ---- fixture geometry: two tensors, data bytes are their own index ----------
: J-SYNTH ( -- ptr u8 n )
   s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`BF16`,`shape`:[4],`data_offsets`:[16,24]}}" ;

24 constant DATA-N
16 constant NB-A       8 constant NB-B
0 constant BEG-A       16 constant BEG-B
108 constant SUM-PAT                            \ 10+11+..+17, the MK-ASTORE pattern

\ ---- the table block TABLE-DISPOSE gives back --------------------------------
\ Mirrors the module's documented table layout: four header cells - slot count,
\ populated count, and the two the residency handle reserves (the parked arm tag and
\ that arm's owner) - then three cells per slot (offset, extent, set flag). Derived
\ from those named parts rather than written as one magic byte count, so a layout
\ change reds this leg instead of quietly agreeing with it; the two residency cells
\ arrived that way, and the number below is the deliberate answer to it. Reserving
\ them in every block is what lets HOLD allocate nothing and so be total.
3 constant WT-ROW-CELLS
2 constant WT-TBL-CORE-CELLS                    \ slot count + populated count
2 constant WT-TBL-RES-CELLS                     \ the residency handle's arm + owner
WT-TBL-CORE-CELLS WT-TBL-RES-CELLS + constant WT-TBL-HDR-CELLS

: TBL-BYTES ( n -- n )                          \ block bytes for a table of n slots
   WT-ROW-CELLS * WT-TBL-HDR-CELLS + cells ;

: SYNTH-PATH ( -- ptr u8 n )  s" /tmp/hb-wst-synth.safetensors" ;

: CLEANUP ( -- )  SYNTH-PATH FS-PATHZ unlink drop ;

\ ---- image builder (the safetensors-test BUILD shape) ------------------------
: BUILD ( ptr u8 n ptr u8 n n -- n )
   {: da:ptr dcap:n ja:ptr ju:n dcount:n :}
   8 ju + dcount + dcap > if E-STR-CAPACITY throw then
   ju 0 ?do
      ja i + c@ dup BT = if drop DQ then
      da 8 i + + c!
   loop
   8 0 ?do  ju i 8 * rshift $FF and  da i +  c!  loop
   dcount 0 ?do  i $FF and  da 8 ju + i + +  c!  loop
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

: >BOFF ( n -- CAD-NUM:byte-off )   CAD-NUM:BYTE-OFF FIX-BOFF ;
: >BLEN ( n -- CAD-NUM:byte-len )   CAD-NUM:BYTE-LEN FIX-BLEN ;

public

\ ---- quotation bodies (public so candidate strings can name them) -------------
: SUM-BODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ + loop ;

: GRAB-BODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 64 > if E-WST-FIX throw then
   a GRAB-BUF u BYTE-COPY
   u GRAB-LEN !
   u ;

: ESC-BODY ( ptr u8 n -- ptr u8 )               \ tries to return the span pointer
   drop ;

private

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
   0 moa >BOFF NB-A >BLEN WSTORE:SLOT!
   1 mob >BOFF NB-B >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: MK-ATBL ( -- WSTORE:table )                   \ rows at the buffer frame
   2 WSTORE:TABLE-NEW
   0 BEG-A >BOFF NB-A >BLEN WSTORE:SLOT!
   1 BEG-B >BOFF NB-B >BLEN WSTORE:SLOT!
   WSTORE:SEAL ;

: GRAB-SLOT ( WSTORE:store n n -- WSTORE:store )   \ slot + expected length
   {: want:n :}
   [: GRAB-BODY ;] WSTORE:WITH-SLOT want T= ;

: KEEP-GRAB ( -- )
   GRAB-BUF KEEP-BUF 64 BYTE-COPY ;

\ ---- the acceptance core: byte equality across both arms -------------------------
: T-EQUALITY ( -- )
   s" both arms serve byte-identical slot bytes from one fixture" T-LABEL
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
   s" slot 0 bytes agree and are the source bytes" T-LABEL
   0 NB-A GRAB-SLOT KEEP-GRAB                   \ allocated arm's slot 0
   swap 0 NB-A GRAB-SLOT                        \ ( astore mstore ) mapped arm's slot 0
   GRAB-BUF NB-A KEEP-BUF NB-A T$=
   KEEP-BUF c@ 0 T=                             \ tensor a's first data byte is index 0
   s" slot 1 bytes agree and are the source bytes" T-LABEL
   1 NB-B GRAB-SLOT KEEP-GRAB                   \ mapped arm's slot 1
   swap 1 NB-B GRAB-SLOT                        \ ( mstore astore )
   GRAB-BUF NB-B KEEP-BUF NB-B T$=
   KEEP-BUF c@ 16 T=                            \ tensor b's first data byte is index 16
   s" checksums agree across arms" T-LABEL
   1 [: SUM-BODY ;] WSTORE:WITH-SLOT {: sumb:n :}
   sumb 156 T=                                  \ 16+17+..+23
   swap 1 [: SUM-BODY ;] WSTORE:WITH-SLOT sumb T=   \ ( astore mstore )
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
\ that declines to commit is left holding, and the leg would be impossible to
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
   0 0 >BOFF 4 >BLEN WSTORE:SLOT!
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

\ ---- the four SLOT! refusals, with the builder given back ----------------------------
\ SLOT! is stack-preserving ( tbuilder -- tbuilder ), so the population step runs as a
\ quotation under `catch` and a refusal leaves the builder exactly where it was - the
\ SAFET:LOAD / GPT2TX:PREPARE discipline. Each leg then hands the block back through
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
   2 0 >BOFF 4 >BLEN WSTORE:SLOT! ;

: SET-NEG ( WSTORE:tbuilder -- WSTORE:tbuilder )
   -1 0 >BOFF 4 >BLEN WSTORE:SLOT! ;

: SET-DUP ( WSTORE:tbuilder -- WSTORE:tbuilder )     \ slot 0 written twice
   0 0 >BOFF 4 >BLEN WSTORE:SLOT!
   0 4 >BOFF 4 >BLEN WSTORE:SLOT! ;

: SET-OVER ( WSTORE:tbuilder -- WSTORE:tbuilder )    \ row end overflows a cell
   0 WT-MAX-N >BOFF 8 >BLEN WSTORE:SLOT! ;

: SL-RANGE ( -- )   2 WSTORE:TABLE-NEW [: SET-RANGE ;] catch 2 BUILDER-BACK ;
: SL-NEG   ( -- )   2 WSTORE:TABLE-NEW [: SET-NEG ;]   catch 2 BUILDER-BACK ;
: SL-DUP   ( -- )   1 WSTORE:TABLE-NEW [: SET-DUP ;]   catch 1 BUILDER-BACK ;
: SL-OVER  ( -- )   1 WSTORE:TABLE-NEW [: SET-OVER ;]  catch 1 BUILDER-BACK ;

\ ---- the two SEAL refusals, which still strand ---------------------------------------
\ These cannot be written the way the four above are. SEAL is ( tbuilder -- table ), so
\ a `catch` over it would have to hold a BUILDER on the refusal arm and a TABLE on the
\ success arm, and no stack effect can say that today. The missing piece is the
\ linear-scope combinator (habu-checker-linear-scope-6218899c) - a scope that disposes
\ the owner it holds when its body throws - and until it lands each of these legs
\ strands its builder. That is the whole of the residue T-TABLE-ERRORS asserts.
: SEAL-UNSET ( -- )
   2 WSTORE:TABLE-NEW
   0 0 >BOFF 4 >BLEN WSTORE:SLOT!
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
   [: SL-NEG ;]     WSTORE:E-SLOT   TTHROWSQ
   [: SL-DUP ;]     WSTORE:E-SET    TTHROWSQ
   [: SL-OVER ;]    WSTORE:E-EXTENT TTHROWSQ
   WSTORE:LIVE 0 T=                             \ all four disposed; nothing left over
   s" a refused SEAL still strands its builder (the linear-scope gap)" T-LABEL
   [: SEAL-UNSET ;] WSTORE:E-UNSET  TTHROWSQ
   [: SEAL-EMPTY ;] WSTORE:E-UNSET  TTHROWSQ
   WSTORE:LIVE 2 T= ;                           \ exactly the two SEAL strands

\ ---- refusal legs: access -------------------------------------------------------------
: MK-ASTORE ( -- WSTORE:store )                 \ 8 pattern bytes 10..17 in slot 0
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   8 0 ?do  i 10 +  base i BYTE+  c!  loop
   base 8 MEM:BYTES-ALLOC-LEN WSTORE:BUFFER
   1 WSTORE:TABLE-NEW
   0 0 >BOFF 8 >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:ALLOCATED ;

: MK-ASTOREZ ( -- WSTORE:store )                \ slot 1 is a zero-extent row at the end
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   8 0 ?do  i 10 +  base i BYTE+  c!  loop
   base 8 MEM:BYTES-ALLOC-LEN WSTORE:BUFFER
   2 WSTORE:TABLE-NEW
   0 0 >BOFF 8 >BLEN WSTORE:SLOT!
   1 8 >BOFF 0 >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:ALLOCATED ;

: MK-EXTSTORE ( -- WSTORE:store )               \ the row runs past the 8-byte buffer
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES WSTORE:BUFFER
   1 WSTORE:TABLE-NEW
   0 4 >BOFF 8 >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:ALLOCATED ;

\ ---- a store held as one cell ---------------------------------------------------------
\ Both arms, entered and left at zero live blocks, so the counters cover the whole life
\ of the handle.
\
\ WHAT THE MIDDLE ASSERTION IN EACH HALF DOES AND DOES NOT PROVE. It proves the handle
\ mints no NEW owner: the block count across HOLD is unchanged, so the resident is the
\ store's own table block retyped rather than a second allocation, which is the
\ mechanism that removes the allocation from the commit's terminal stretch. It is not a
\ proof of totality by itself - WSTORE:LIVE counts owner blocks this module bumps by
\ hand, so a HOLD that allocated without minting an owner would still pass it. That
\ HOLD cannot fail is a property of its call graph (MATCH, an identity retype, two cell
\ writes, one single-cell erasure - no allocation and no throw anywhere on the path),
\ argued in the module header and checkable by reading it; the checker has no effect
\ system that could assert it here. What the counter does close is the one way that
\ property could regress silently, which is a second block appearing.
\
\ What ok carries is not a number invented here either: it is what the underlying
\ DISPOSE reports for that arm - the mapping's own byte length, and the buffer's extent.
: MK-MSTORE ( -- WSTORE:store )                 \ a real mapped store over the fixture
   SAFET:OPEN SYNTH-PATH SAFET:MAP-FILE SAFET:PARSE SAFET:DETACH
   MK-MTBL                                      \ ( c mtbl )
   swap SAFET:DETACH-MAPPING TAKE-MOVED         \ ( mtbl c m )
   swap SAFET:RELEASE                           \ ( mtbl m )
   swap WSTORE-STORE:MAPPED ;

: T-RESIDENT ( -- )
   s" a mapped store can be held as one cell" T-LABEL
   BUILD-SYNTH
   WSTORE:LIVE 0 T=
   MK-MSTORE                                    \ ( store )
   WSTORE:LIVE 1 T=                             \ its table block
   SAFET-MAP:LIVE 1 T=                          \ the kernel mapping the store now serves
   SAFET:LIVE-OWNERS 1 T=                       \ the detached mapping owner
   WSTORE:HOLD                                  \ ( resident )
   s" holding allocated nothing: no counter moved" T-LABEL
   WSTORE:LIVE 1 T=
   SAFET-MAP:LIVE 1 T=
   SAFET:LIVE-OWNERS 1 T=
   s" and the handle's exit gives the mapping back to the kernel" T-LABEL
   WSTORE:RESIDENT-DISPOSE LEN-I @ RES-OK=
   WSTORE:LIVE 0 T=
   SAFET-MAP:LIVE 0 T=
   SAFET:LIVE-OWNERS 0 T=
   s" the allocated arm holds and releases the same way" T-LABEL
   MK-ASTORE
   WSTORE:LIVE 2 T=                             \ its table block and its buffer record
   WSTORE:HOLD
   s" holding the other arm allocated nothing either" T-LABEL
   WSTORE:LIVE 2 T=
   s" and its exit releases the buffer's own bytes" T-LABEL
   WSTORE:RESIDENT-DISPOSE 8 RES-OK=
   WSTORE:LIVE 0 T=
   SAFET-MAP:LIVE 0 T=
   SAFET:LIVE-OWNERS 0 T=
   CLEANUP ;

: WS-GOOD ( -- )
   MK-ASTORE
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT SUM-PAT T=
   WSTORE:DISPOSE 8 RES-OK=
   WSTORE:LIVE 2 T= ;                           \ back to the two SEAL strands

: WS-ZERO ( -- )
   MK-ASTOREZ
   1 [: SUM-BODY ;] WSTORE:WITH-SLOT 0 T=       \ zero-extent slot runs the body on 0 bytes
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT SUM-PAT T=
   WSTORE:DISPOSE 8 RES-OK=
   WSTORE:LIVE 2 T= ;

: WS-RANGE ( -- )
   MK-ASTORE
   9 [: SUM-BODY ;] WSTORE:WITH-SLOT drop
   WSTORE:DISPOSE RES-DROP ;

: WS-EXTENT ( -- )
   MK-EXTSTORE
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT drop
   WSTORE:DISPOSE RES-DROP ;

\ Adopted-image mapped stores: refusals on the mapped arm must not pin a kernel
\ mapping, so these borrow IMG instead of mapping the synthetic file.
: MK-FATROW ( -- WSTORE:store )                 \ a live borrowed mapping, row far past it
   IMG LEN-I @ SAFET:LOAD-SPAN
   SAFET:DETACH-MAPPING TAKE-MOVED
   swap SAFET:RELEASE                           \ ( m )
   1 WSTORE:TABLE-NEW
   0 8 >BOFF 10000 >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:MAPPED ;

: WS-FATROW ( -- )
   MK-FATROW
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT drop
   WSTORE:DISPOSE RES-DROP ;

: MK-BORROWMAP ( -- WSTORE:store )              \ a live borrowed mapping, honest row
   IMG LEN-I @ SAFET:LOAD-SPAN
   SAFET:DETACH-MAPPING TAKE-MOVED
   swap SAFET:RELEASE                           \ ( m )
   1 WSTORE:TABLE-NEW
   0 8 >BOFF 4 >BLEN WSTORE:SLOT!               \ mapping bytes 8.. open the header JSON
   WSTORE:SEAL
   WSTORE-STORE:MAPPED ;

: WS-BORROW ( -- )                              \ the mapped arm over a borrowed image works
   MK-BORROWMAP
   0 [: GRAB-BODY ;] WSTORE:WITH-SLOT 4 T=
   GRAB-BUF c@ $7B T=                           \ '{' - mapping byte 8 is the header's first byte
   WSTORE:DISPOSE 0 RES-OK=                     \ borrowed: nothing given back, nothing leaked
   SAFET:LIVE-OWNERS 0 T= ;

\ ---- nested WITH-SLOT: a second store minted INSIDE a body ---------------------------
\ A body cannot bring the outer store in, but it can mint one; these legs pin
\ the module header's three ordering facts. The poison leg is the sharp one: the
\ INNER call aborts with E-EXTENT (its serve never ran, so it left the parked
\ ran-flag at 0) and the body catches it; the OUTER call must still report ITS
\ body's result. Writing WS-RAN before the body ran - instead of after it
\ returned - passes every plain leg and fails exactly here.
288 constant SUM-HDR4                           \ mapping bytes 8..11 are {, ", a, "

: INNER-ASUM ( -- n )                           \ allocated inner store, full life inside
   MK-ASTORE
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT {: r:n :}
   WSTORE:DISPOSE 8 RES-OK=
   r ;

: NEST-ABODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   INNER-ASUM
   0 u 0 ?do a i + c@ + loop + ;

: INNER-MSUM ( -- n )                           \ mapped inner store, full life inside
   MK-BORROWMAP
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT {: r:n :}
   WSTORE:DISPOSE 0 RES-OK=
   r ;

: NEST-MBODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   INNER-MSUM
   0 u 0 ?do a i + c@ + loop + ;

: NEST-AA ( -- )                                \ allocated outer, allocated inner
   MK-ASTORE
   0 [: NEST-ABODY ;] WSTORE:WITH-SLOT SUM-PAT SUM-PAT + T=
   WSTORE:DISPOSE 8 RES-OK= ;

: NEST-MA ( -- )                                \ mapped outer, allocated inner
   MK-BORROWMAP
   0 [: NEST-ABODY ;] WSTORE:WITH-SLOT SUM-HDR4 SUM-PAT + T=
   WSTORE:DISPOSE 0 RES-OK= ;

: NEST-MM ( -- )                                \ mapped outer, mapped inner
   MK-BORROWMAP
   0 [: NEST-MBODY ;] WSTORE:WITH-SLOT SUM-HDR4 SUM-HDR4 + T=
   WSTORE:DISPOSE 0 RES-OK= ;

: FAT-INNER ( -- )                              \ inner access that aborts with E-EXTENT
   MK-EXTSTORE
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT drop
   WSTORE:DISPOSE RES-DROP ;

: PBODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   [: FAT-INNER ;] catch WSTORE:E-EXTENT <> if E-WST-FIX throw then
   0 u 0 ?do a i + c@ + loop ;

: NEST-POISON ( -- )                            \ strands the inner buffer + table
   MK-ASTORE
   0 [: PBODY ;] WSTORE:WITH-SLOT SUM-PAT T=
   WSTORE:DISPOSE 8 RES-OK= ;

: T-NESTED ( -- )
   s" WITH-SLOT nests: a body can mint, read, and dispose a second store" T-LABEL
   NEST-AA
   NEST-MA
   NEST-MM
   SAFET:LIVE-OWNERS 1 T=                       \ nested mapped stores all disposed
   s" an aborted inner WITH-SLOT does not poison the outer call's frame" T-LABEL
   NEST-POISON
   WSTORE:LIVE 9 T= ;                           \ + the poisoned inner's buffer and table

: T-ACCESS ( -- )
   BUILD-IMG
   s" WITH-SLOT serves and disposes cleanly on the happy paths" T-LABEL
   WS-GOOD
   WS-ZERO
   SAFET:LIVE-OWNERS 0 T=
   WS-BORROW
   s" access refusals throw their named codes" T-LABEL
   [: WS-RANGE ;]   WSTORE:E-SLOT   TTHROWSQ    \ strands buffer + table
   [: WS-EXTENT ;]  WSTORE:E-EXTENT TTHROWSQ    \ strands buffer + table
   [: WS-FATROW ;]  WSTORE:E-EXTENT TTHROWSQ    \ strands borrowed mapping + table
   WSTORE:LIVE 7 T=                             \ 2 + 2 + 2 + 1 documented strands
   SAFET:LIVE-OWNERS 1 T=                       \ the stranded borrowed mapping
   SAFET-MAP:LIVE 0 T= ;                        \ and NO kernel mapping leaked anywhere

\ ---- static half: the checker enforces the ownership and escape rules ---------------
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
   s" WST-BAD-MUTATE-SEALED ( WSTORE:table n CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:table ) WSTORE:SLOT!" REJECTED
   s" WST-BAD-BUILDER-ACCESS ( WSTORE:tbuilder n -- WSTORE:tbuilder n ) [: WSTORE-TEST:SUM-BODY ;] WSTORE:WITH-SLOT" REJECTED
   s" the two arms cannot be confused at construction" T-LABEL
   s" WST-BAD-CTOR-MA ( SAFET:mapping WSTORE:table -- WSTORE:store ) WSTORE-STORE:ALLOCATED" REJECTED
   s" WST-BAD-CTOR-AM ( WSTORE:buffer WSTORE:table -- WSTORE:store ) WSTORE-STORE:MAPPED" REJECTED
   s" DISPOSE consumes the store exactly once and its result is not droppable" T-LABEL
   s" WST-BAD-DOUBLE-DISPOSE ( WSTORE:store -- result<n,n> result<n,n> ) WSTORE:DISPOSE WSTORE:DISPOSE" REJECTED
   s" WST-BAD-DISPOSE-KEEPS ( WSTORE:store -- WSTORE:store result<n,n> ) WSTORE:DISPOSE" REJECTED
   s" WST-BAD-USE-AFTER ( WSTORE:store -- result<n,n> WSTORE:store n ) WSTORE:DISPOSE 0 [: WSTORE-TEST:SUM-BODY ;] WSTORE:WITH-SLOT" REJECTED
   s" WST-BAD-RESULT-DROPPED ( WSTORE:store -- ) WSTORE:DISPOSE" REJECTED
   s" WST-BAD-RESULT-RAW ( WSTORE:store -- n ) WSTORE:DISPOSE 1 +" REJECTED
   s" no reader answers without its store" T-LABEL
   s" WST-BAD-AMBIENT ( n -- n ) [: WSTORE-TEST:SUM-BODY ;] WSTORE:WITH-SLOT" REJECTED
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
   s" WST-BAD-BD-THEN-BFD ( WSTORE:tbuilder -- result<n,n> result<n,n> ) WSTORE:BUILDER-DISPOSE WSTORE:BUFFER-DISPOSE" REJECTED
   s" a resident is linear: no copy, no discard, no store" T-LABEL
   s" WST-BAD-RES-DUP ( WSTORE:resident -- WSTORE:resident WSTORE:resident ) dup" REJECTED
   s" WST-BAD-RES-DROP ( WSTORE:resident -- ) drop" REJECTED
   s" WST-BAD-RES-STORE ( WSTORE:resident ptr n -- ) !" REJECTED
   s" WST-BAD-RES-FORGE ( n -- WSTORE:resident ) " REJECTED
   s" HOLD consumes its store exactly once and its result cannot be dropped" T-LABEL
   s" WST-BAD-HOLD-KEEPS ( WSTORE:store -- WSTORE:store WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" WST-BAD-HOLD-DROPPED ( WSTORE:store -- ) WSTORE:HOLD" REJECTED
   s" WST-BAD-HOLD-TWICE ( WSTORE:store -- WSTORE:resident WSTORE:resident ) WSTORE:HOLD WSTORE:HOLD" REJECTED
   s" a held store is gone: the arm owners cannot be used beside the handle" T-LABEL
   s" WST-BAD-HOLD-THEN-DISPOSE ( WSTORE:store -- WSTORE:resident result<n,n> ) WSTORE:HOLD WSTORE:DISPOSE" REJECTED
   s" HOLD takes a store and nothing else" T-LABEL
   s" WST-BAD-HOLD-TABLE ( WSTORE:table -- WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" WST-BAD-HOLD-BUILDER ( WSTORE:tbuilder -- WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" WST-BAD-HOLD-BUFFER ( WSTORE:buffer -- WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" WST-BAD-HOLD-RESIDENT ( WSTORE:resident -- WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" HOLD answers about the store it is given, never ambient state" T-LABEL
   s" WST-BAD-HOLD-AMBIENT ( -- WSTORE:resident ) WSTORE:HOLD" REJECTED
   s" RESIDENT-DISPOSE consumes its handle exactly once and yields a real union" T-LABEL
   s" WST-BAD-RD-TWICE ( WSTORE:resident -- result<n,n> result<n,n> ) WSTORE:RESIDENT-DISPOSE WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-KEEPS ( WSTORE:resident -- WSTORE:resident result<n,n> ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-DROPPED ( WSTORE:resident -- ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-RAW ( WSTORE:resident -- n ) WSTORE:RESIDENT-DISPOSE 1 +" REJECTED
   s" the handle's exit takes a handle and nothing else" T-LABEL
   s" WST-BAD-RD-STORE ( WSTORE:store -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-TABLE ( WSTORE:table -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-BUILDER ( WSTORE:tbuilder -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" WST-BAD-RD-BUFFER ( WSTORE:buffer -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" REJECTED
   s" and a handle is not accepted by any other owner's exit" T-LABEL
   s" WST-BAD-RES-TO-DISPOSE ( WSTORE:resident -- result<n,n> ) WSTORE:DISPOSE" REJECTED
   s" WST-BAD-RES-TO-TD ( WSTORE:resident -- result<n,n> ) WSTORE:TABLE-DISPOSE" REJECTED
   s" WST-BAD-RES-TO-BD ( WSTORE:resident -- result<n,n> ) WSTORE:BUILDER-DISPOSE" REJECTED
   s" WST-BAD-RES-TO-BFD ( WSTORE:resident -- result<n,n> ) WSTORE:BUFFER-DISPOSE" REJECTED
   s" and a handle is not a table, so it cannot be re-sealed or read as one" T-LABEL
   s" WST-BAD-RES-SEAL ( WSTORE:resident -- WSTORE:table ) WSTORE:SEAL" REJECTED
   s" WST-BAD-RES-WITH-SLOT ( WSTORE:resident n -- WSTORE:resident n ) [: drop 0 ;] WSTORE:WITH-SLOT" REJECTED ;

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

: T-ESCAPE ( -- )
   s" a quotation declared to return the span pointer rejects statically" T-LABEL
   s" WST-BAD-ESC1 ( WSTORE:store n -- WSTORE:store n ) [: WSTORE-TEST:ESC-BODY ;] WSTORE:WITH-SLOT" REJECTED
   s" WST-BAD-ESC2 ( WSTORE:store n -- WSTORE:store ptr u8 ) [: drop ;] WSTORE:WITH-SLOT" REJECTED
   s" the conforming quotation certifies (control)" T-LABEL
   s" WST-OK-ACCESS ( WSTORE:store n -- WSTORE:store n ) [: WSTORE-TEST:SUM-BODY ;] WSTORE:WITH-SLOT" ACCEPTED ;

: T-SURFACE ( -- )
   s" the public surface resolves (controls)" T-LABEL
   s" WST-OK-POLICY-M ( -- WSTORE:residency ) WSTORE-RESIDENCY:MAPPED" ACCEPTED
   s" WST-OK-POLICY-A ( -- WSTORE:residency ) WSTORE-RESIDENCY:ALLOCATED" ACCEPTED
   s" WST-OK-CTOR-M ( SAFET:mapping WSTORE:table -- WSTORE:store ) WSTORE-STORE:MAPPED" ACCEPTED
   s" WST-OK-CTOR-A ( WSTORE:buffer WSTORE:table -- WSTORE:store ) WSTORE-STORE:ALLOCATED" ACCEPTED
   s" WST-OK-SLOT ( WSTORE:tbuilder n CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:tbuilder ) WSTORE:SLOT!" ACCEPTED
   s" WST-OK-SEAL ( WSTORE:tbuilder -- WSTORE:table ) WSTORE:SEAL" ACCEPTED
   s" WST-OK-DISPOSE ( WSTORE:store -- result<n,n> ) WSTORE:DISPOSE" ACCEPTED
   s" WST-OK-TABLE-DISPOSE ( WSTORE:table -- result<n,n> ) WSTORE:TABLE-DISPOSE" ACCEPTED
   s" WST-OK-BUILDER-DISPOSE ( WSTORE:tbuilder -- result<n,n> ) WSTORE:BUILDER-DISPOSE" ACCEPTED
   s" WST-OK-BUFFER-DISPOSE ( WSTORE:buffer -- result<n,n> ) WSTORE:BUFFER-DISPOSE" ACCEPTED
   s" WST-OK-BUFFER-NEW ( CAD-NUM:alloc-byte-len -- WSTORE:buffer ) WSTORE:BUFFER-NEW" ACCEPTED
   s" WST-OK-HOLD ( WSTORE:store -- WSTORE:resident ) WSTORE:HOLD" ACCEPTED
   s" WST-OK-RESIDENT-DISPOSE ( WSTORE:resident -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" ACCEPTED
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
   s" WST-BAD-MINT-RES ( ptr u8 -- WSTORE:resident ) WSTORE:MINT-RESIDENT" UNRESOLVED
   s" WST-BAD-TAKE-RES ( WSTORE:resident -- ptr n ) WSTORE:TAKE-RESIDENT" UNRESOLVED
   s" WST-BAD-MAP-N ( SAFET:mapping -- n ) WSTORE:MAP>N" UNRESOLVED
   s" WST-BAD-N-MAP ( n -- SAFET:mapping ) WSTORE:N>MAP" UNRESOLVED
   s" WST-BAD-BUF-N ( WSTORE:buffer -- n ) WSTORE:BUF>N" UNRESOLVED
   s" WST-BAD-N-BUF ( n -- WSTORE:buffer ) WSTORE:N>BUF" UNRESOLVED
   s" WST-BAD-PARK-ARM ( WSTORE:table n n -- ptr u8 ) WSTORE:PARK-ARM" UNRESOLVED
   s" WST-BAD-UNPARK-TABLE ( ptr n -- WSTORE:table ) WSTORE:UNPARK-TABLE" UNRESOLVED
   s" WST-BAD-PARK ( -- ) WSTORE:PARK" UNRESOLVED
   s" WST-BAD-RUN-PARKED ( ptr u8 n -- ) WSTORE:RUN-PARKED" UNRESOLVED
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
   T-BUFFER-NEW
   T-ESCAPE
   T-SURFACE
   T-SEALED
   T-EQUALITY
   T-TABLE-DISPOSE
   T-OWNER-EXITS
   T-RESIDENT
   T-TABLE-ERRORS
   T-ACCESS
   T-NESTED
   s" final leak accounting: only the documented throw strands remain" T-LABEL
   WSTORE:LIVE 9 T=                             \ 2 SEAL + 5 access-throw + 2 poisoned inner
   SAFET:LIVE-OWNERS 1 T=
   SAFET-MAP:LIVE 0 T=
   T-REPORT ;

;package

WSTORE-TEST:RUN
