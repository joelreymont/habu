\ nominal-test.f - focused boundary + failure-matrix + static-rejection suite for
\ the sealed immutable nominal collection (dot habu-add-immutable-nominal-9290a81f).
\ Run: bin/hb --load lib/nominal/nominal-test.f
\
\ Direct-loaded gate home (the substrate has no production consumer yet; CAD-EFFECT
\ consumes it later), following the lib/cad-num-types-test.f precedent - it is not
\ in test/gate-stdlib-cases.f, so no gate slice schedules it.
\ Reopens package NOM so the forgery / protected-span / internal-inspection cases
\ can reach the private mints and arena words the public API never exposes.

require lib/nominal/snapshot.f
require lib/test.f
require test/checker-assert.f

create NT-BUF 4096 allot
create NT-KA 40 allot
create NT-KB 40 allot
variable NT-CLA

package NOM
private

: NT-P2 ( n n -- path )   ROOT swap CONS swap CONS ;      \ ROOT/a/b
: NT-MKB ( n n n -- binding ) {: a:n b:n slot:n :}   a b NT-P2 slot BIND ;
: NT-R1 ( -- row )   NEW 10 20 1 NT-MKB ADD 10 30 2 NT-MKB ADD FREEZE ;
: NT-R1REV ( -- row ) NEW 10 30 2 NT-MKB ADD 10 20 1 NT-MKB ADD FREEZE ;
: NT-R2 ( -- row )   NEW 40 50 9 NT-MKB ADD FREEZE ;

\ ---- paths -------------------------------------------------------------------
: NT-PATHS ( -- )
   ROOT 1 CONS 2 CONS 3 CONS SEGS 3 T=
   ROOT SEGS 0 T= ;

\ ---- build / dedup / order independence --------------------------------------
: NT-BUILD ( -- )
   NT-R1 SIZE 2 T=
   NT-R1 NT-R1REV EQUAL? TTRUE                       \ insertion order is not identity
   NEW 10 20 1 NT-MKB ADD 10 20 1 NT-MKB ADD FREEZE SIZE 1 T= ;   \ full-row dup idempotent

\ ---- union -------------------------------------------------------------------
: NT-UNION ( -- )
   NT-R1 NT-R2 UNION SIZE 3 T=
   NT-R1 NT-R2 UNION  NT-R2 NT-R1 UNION  EQUAL? TTRUE     \ union content is commutative
   NT-R1 NT-R1 UNION  NT-R1  EQUAL? TTRUE                 \ union idempotent
   [: NEW 1 2 3 NT-MKB ADD 1 2 4 NT-MKB ADD FREEZE ROW-IDX drop ;]
      E-NOM-CONFLICT TTHROWSQ ;                           \ same path, disagreeing slot

\ ---- remap: preserves count, prefixes path, keeps slot -----------------------
: NT-REMAP-SLOT@ ( -- n )   NT-R1 99 REMAP ROW-CHECK ROW-START@ CHUNK@ BIND-SLOT@ ;
: NT-REMAP-DEPTH@ ( -- n )  NT-R1 99 REMAP ROW-CHECK ROW-START@ CHUNK@ BIND-PATH@ MINT-PATH SEGS ;
: NT-REMAP ( -- )
   NT-R1 99 REMAP SIZE 2 T=                            \ count preserved
   NT-REMAP-DEPTH@ 3 T=                                \ 99/10/20 -> depth 3
   NT-REMAP-SLOT@ 1 T= ;                               \ slot preserved exactly

\ ---- canonical codec ---------------------------------------------------------
: NT-CODEC-RT ( -- bool )
   NT-R1 {: r:row :}
   r NT-BUF 4096 ENCODE {: len:n :}
   NT-BUF len DECODE r EQUAL? ;
: NT-CODEC ( -- )
   NT-CODEC-RT TTRUE
   [: NT-BUF 5 DECODE ROW-IDX drop ;] E-NOM-WIRE TTHROWSQ    \ truncated wire
   [: NT-R1 NT-BUF 8 ENCODE drop ;] E-NOM-CAPACITY TTHROWSQ ;  \ output buffer too small

\ ---- content key: same content -> same key -----------------------------------
: NT-KEY-EQ ( -- bool )
   NT-R1 NT-KA KEY  NT-R1REV NT-KB KEY
   0 begin dup 32 < while dup NT-KA + c@ over NT-KB + c@ <> if drop 0 0= 0= exit then 1+ repeat drop 0 0= ;
: NT-KEY ( -- )   NT-KEY-EQ TTRUE ;

\ ---- snapshot / AOT ----------------------------------------------------------
: NT-SNAP-ALLOC-INDEP ( -- bool )
   RESET NT-R1 drop NT-R2 drop        NT-BUF 4096 SNAPSHOT {: la:n :}
   RESET NT-R2 drop NT-R1REV drop     NT-KA 4096 SNAPSHOT {: lb:n :}
   la lb <> if 0 0= 0= exit then
   0 begin dup la < while dup NT-BUF + c@ over NT-KA + c@ <> if drop 0 0= 0= exit then 1+ repeat drop 0 0= ;
: NT-SNAP ( -- )
   NT-SNAP-ALLOC-INDEP TTRUE                           \ order-independent snapshot bytes
   RESET NT-R1 drop NT-R2 drop
   NT-BUF 4096 SNAPSHOT NT-CLA !
   RESET  NT-BUF NT-CLA @ RESTORE 2 T=                 \ restore rebuilds both rows
   255 NT-BUF 40 + c!
   [: NT-BUF NT-CLA @ RESTORE drop ;] E-NOM-WIRE TTHROWSQ ;   \ corrupt snapshot

\ ---- failure / recovery matrix -----------------------------------------------
: NT-DOUBLE-OPEN ( -- n )   NEW [: NEW FREEZE ROW-IDX drop ;] catch swap ROLLBACK ;
: NT-BIG-ROW ( -- n )   NEW 300 0 do i 0 i NT-MKB ADD loop FREEZE SIZE ;
: NT-BUDGET ( -- n )
   2 BUDGET! [: NEW 1 1 1 NT-MKB ADD 2 2 2 NT-MKB ADD 3 3 3 NT-MKB ADD FREEZE ROW-IDX drop ;] catch
   NOM-DEFAULT-BUDGET BUDGET! ;
: NT-FAIL ( -- )
   [: 999999 MINT-ROW SIZE drop ;] E-NOM-HANDLE TTHROWSQ    \ forged/stale row handle
   [: 999999 MINT-PATH 7 CONS PATH-IDX drop ;] E-NOM-HANDLE TTHROWSQ   \ stale path handle
   NT-DOUBLE-OPEN E-NOM-OPEN T=                             \ second open rejected
   NT-BUDGET E-NOM-BUDGET T=                                \ resource budget hit
   NT-BIG-ROW 300 T= ;                                      \ but semantic capacity >> old 25-binding limit

\ ---- protected spans: no raw mutation of a sealed span -----------------------
: NT-SEAL ( -- )
   NT-R1 drop
   [: 999 NOM-CHUNK-AR 0 AR-CELL! ;] E-NOM-SEALED TTHROWSQ ;

\ ---- static rejection: the public boundary is fail-closed --------------------
: NT-STATIC ( -- )
   s" NT-OK-CONS ( NOM:path -- NOM:path ) 7 NOM:CONS"  CHECK-QUIET-CANDIDATE! -1 T=
   s" NT-FORGE-ROW ( -- n ) 5 NOM:SIZE"                CHECK-QUIET-CANDIDATE! 0 T=
   s" NT-FORGE-BIND ( -- NOM:binding ) 5"              CHECK-QUIET-CANDIDATE! 0 T=
   s" NT-CROSS ( NOM:path -- n ) NOM:SIZE"             CHECK-QUIET-CANDIDATE! 0 T=
   s" NT-DUP-BLDR ( -- ) NOM:NEW dup NOM:ROLLBACK NOM:ROLLBACK"
      CHECK-QUIET-CANDIDATE! 0 T= ;                    \ linear builder cannot be duplicated

: NT-ALL ( -- )
   T-RESET
   RESET
   NT-PATHS
   NT-BUILD
   NT-UNION
   NT-REMAP
   NT-CODEC
   NT-KEY
   NT-SNAP
   NT-FAIL
   NT-SEAL
   NT-STATIC
   T-REPORT ;

NT-ALL
;package
