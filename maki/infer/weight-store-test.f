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
\ and a byteless mapping. A throw unwinds past the deconstructed linear owners
\ (the documented SAFET:DETACH-MAPPING strand behavior), so each refusal leg
\ asserts the exact leak-counter residue it leaves; the suite's final assertions
\ pin the totals and that NO kernel mapping is ever leaked (the refusal legs use
\ adopted images precisely so SAFET-MAP:LIVE ends at 0).
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
   swap SAFET:DETACH-MAPPING                    \ ( abuf mtbl c m )
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

\ ---- refusal legs: table construction ----------------------------------------------
: TBL-BURN ( WSTORE:table -- )                  \ consume a table through a real store
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES WSTORE:BUFFER
   swap WSTORE-STORE:ALLOCATED
   WSTORE:DISPOSE RES-DROP ;

: TN-ZERO ( -- )   0 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;
: TN-HUGE ( -- )   $10001 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;   \ MAX-SLOTS + 1

: SL-RANGE ( -- )
   2 WSTORE:TABLE-NEW
   2 0 >BOFF 4 >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SL-NEG ( -- )
   2 WSTORE:TABLE-NEW
   -1 0 >BOFF 4 >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SL-DUP ( -- )
   1 WSTORE:TABLE-NEW
   0 0 >BOFF 4 >BLEN WSTORE:SLOT!
   0 4 >BOFF 4 >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SL-OVER ( -- )
   1 WSTORE:TABLE-NEW
   0 WT-MAX-N >BOFF 8 >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SEAL-UNSET ( -- )
   2 WSTORE:TABLE-NEW
   0 0 >BOFF 4 >BLEN WSTORE:SLOT!
   WSTORE:SEAL TBL-BURN ;

: SEAL-EMPTY ( -- )
   1 WSTORE:TABLE-NEW WSTORE:SEAL TBL-BURN ;

\ Each SLOT!/SEAL refusal strands one builder block (the documented throw-strand
\ behavior); the residue is asserted so a silent extra leak cannot hide in it.
: T-TABLE-ERRORS ( -- )
   s" every table-construction refusal throws its named code" T-LABEL
   [: TN-ZERO ;]    WSTORE:E-SLOTS  TTHROWSQ
   [: TN-HUGE ;]    WSTORE:E-SLOTS  TTHROWSQ
   WSTORE:LIVE 0 T=                             \ refused before any allocation
   [: SL-RANGE ;]   WSTORE:E-SLOT   TTHROWSQ
   [: SL-NEG ;]     WSTORE:E-SLOT   TTHROWSQ
   [: SL-DUP ;]     WSTORE:E-SET    TTHROWSQ
   [: SL-OVER ;]    WSTORE:E-EXTENT TTHROWSQ
   [: SEAL-UNSET ;] WSTORE:E-UNSET  TTHROWSQ
   [: SEAL-EMPTY ;] WSTORE:E-UNSET  TTHROWSQ
   WSTORE:LIVE 6 T= ;                           \ exactly the six stranded builders

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

: WS-GOOD ( -- )
   MK-ASTORE
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT SUM-PAT T=
   WSTORE:DISPOSE 8 RES-OK=
   WSTORE:LIVE 6 T= ;                           \ back to the table-error residue

: WS-ZERO ( -- )
   MK-ASTOREZ
   1 [: SUM-BODY ;] WSTORE:WITH-SLOT 0 T=       \ zero-extent slot runs the body on 0 bytes
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT SUM-PAT T=
   WSTORE:DISPOSE 8 RES-OK=
   WSTORE:LIVE 6 T= ;

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
: MK-DEADMAP ( -- WSTORE:store )                \ a mapping that owns no bytes at all
   IMG LEN-I @ SAFET:LOAD-SPAN                  \ ( c )
   SAFET:DETACH-MAPPING                         \ ( c m1 )
   swap SAFET:DETACH-MAPPING                    \ ( m1 c m2 )
   swap SAFET:RELEASE                           \ ( m1 m2 )
   swap SAFET:UNMAP-MAPPING 0 RES-OK=           \ ( m2 ) the first owner borrowed, gave back nothing
   1 WSTORE:TABLE-NEW
   0 0 >BOFF 0 >BLEN WSTORE:SLOT!
   WSTORE:SEAL
   WSTORE-STORE:MAPPED ;

: WS-DEADMAP ( -- )
   MK-DEADMAP
   0 [: SUM-BODY ;] WSTORE:WITH-SLOT drop
   WSTORE:DISPOSE RES-DROP ;

: MK-FATROW ( -- WSTORE:store )                 \ a live borrowed mapping, row far past it
   IMG LEN-I @ SAFET:LOAD-SPAN
   SAFET:DETACH-MAPPING
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
   SAFET:DETACH-MAPPING
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
   SAFET:LIVE-OWNERS 2 T=                       \ nested mapped stores all disposed
   s" an aborted inner WITH-SLOT does not poison the outer call's frame" T-LABEL
   NEST-POISON
   WSTORE:LIVE 14 T= ;                          \ + the poisoned inner's buffer and table

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
   [: WS-DEADMAP ;] WSTORE:E-EXTENT TTHROWSQ    \ strands byteless mapping + table
   [: WS-FATROW ;]  WSTORE:E-EXTENT TTHROWSQ    \ strands borrowed mapping + table
   WSTORE:LIVE 12 T=                            \ 6 + 2 + 2 + 1 + 1, all documented strands
   SAFET:LIVE-OWNERS 2 T=                       \ the two stranded mapping owners
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
   s" WST-BAD-AMBIENT ( n -- n ) [: WSTORE-TEST:SUM-BODY ;] WSTORE:WITH-SLOT" REJECTED ;

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
   T-ESCAPE
   T-SURFACE
   T-SEALED
   T-EQUALITY
   T-TABLE-ERRORS
   T-ACCESS
   T-NESTED
   s" final leak accounting: only the documented throw strands remain" T-LABEL
   WSTORE:LIVE 14 T=
   SAFET:LIVE-OWNERS 2 T=
   SAFET-MAP:LIVE 0 T=
   T-REPORT ;

;package

WSTORE-TEST:RUN
