\ snapshot.f - snapshot / AOT adapter for the immutable nominal collection
\ (dot habu-add-immutable-nominal-9290a81f).
\
\ This follows the src/core/layout-buffer.f discipline of a lib owning its own
\ persistence WITHOUT touching the engine AOT capture path: the persisted form is
\ the CANONICAL CONTENT bytes, never process-local handle numbers or arena
\ pointers. NOM:SNAPSHOT emits every published row in canonical DIGEST order
\ (not allocation order) behind a validated header, so a fresh process that built
\ the same set of rows in any insertion order snapshots byte-identically - the
\ allocation-order-independent fixpoint the dot requires. NOM:RESTORE validates
\ the header/magic/version, decodes each framed row (bounds, sortedness, path
\ uniqueness, full consumption via the codec), and republishes it into freshly
\ reset arenas; NOM:RESET re-mmaps every arena, resetting the process-local base
\ pointers a restored image must not inherit.
\
\ NOM:ENCODE / NOM:DECODE are the single-row wire pair; a decode failure truncates
\ the chunk high-water and publishes nothing.
\
\ Load after lib/nominal/builder.f. Reopens package NOM.

require lib/nominal/builder.f

$4E4F4D534E415031 constant NOM-SNAP-MAGIC   \ "NOMSNAP1"
1 constant NOM-SNAP-VERSION

package NOM
private

create NOM-SNAP-AR 4 cells allot            \ scratch: row indices to sort by digest
PTR-VARIABLE SNAP-P   variable SNAP-CAP  variable SNAP-U
PTR-VARIABLE SNAP-RP  variable SNAP-RLEN variable SNAP-RU
PTR-VARIABLE DEC-PBUF variable DEC-LEN  variable DEC-ROW

\ ---- single-row decode with chunk rollback -----------------------------------
: DECODE-CORE ( -- )
   DEC-PBUF 0 ptr-field @ DEC-LEN @ CHUNK-DECODE PUBLISH-CHUNK ROW-IDX DEC-ROW ! ;

\ ---- snapshot writer ---------------------------------------------------------
: SNAP-ROOM ( n -- ) {: k:n :}
   SNAP-U @ k + SNAP-CAP @ > if E-NOM-CAPACITY throw then ;
: SNAP-PUT ( n -- ) {: v:n :}
   8 SNAP-ROOM
   v SNAP-P 0 ptr-field @ SNAP-U @ + BE64!
   SNAP-U @ 8 + SNAP-U ! ;

: ROW-DIG-LESS? ( a a -- bool ) {: ra:n rb:n :}   \ canonical digest order (rows interned => distinct)
   0 begin dup 4 < while
      dup {: k:n :}
      ra k ROW-DIGEST-CELL@  rb k ROW-DIGEST-CELL@  {: xa:n xb:n :}
      xa xb <> if drop xa xb < exit then
      1+
   repeat drop 0 0= 0= ;

: SNAP-FILL-SORTED ( -- )
   NOM-SNAP-AR AR-RESET
   ROW-RECORDS 0 ?do  i NOM-SNAP-AR AR-PUSH drop  loop
   NOM-SNAP-AR AR-BASE@ NOM-SNAP-AR AR-USED@ [: ROW-DIG-LESS? ;] SORT! ;

: SNAP-ROW ( n -- ) {: r:n :}               \ append one length-framed canonical row
   SNAP-U @ 8 + {: dataoff:n :}
   dataoff SNAP-CAP @ > if E-NOM-CAPACITY throw then
   r ROW-START@ r ROW-CNT@
   SNAP-P 0 ptr-field @ dataoff +  SNAP-CAP @ dataoff -  CHUNK-ENCODE {: rl:n :}
   rl SNAP-PUT
   SNAP-U @ rl + SNAP-U ! ;

\ ---- snapshot reader ---------------------------------------------------------
: SNAP-GET ( -- n )
   SNAP-RU @ 8 + SNAP-RLEN @ > if E-NOM-WIRE throw then
   SNAP-RP 0 ptr-field @ SNAP-RU @ + BE64@
   SNAP-RU @ 8 + SNAP-RU ! ;

: NOM-SNAP-INIT ( -- )   NOM-SNAP-AR AR-INIT ;

public

: ENCODE ( row ptr u8 n -- n )              \ single-row canonical wire encode
   {: buf:ptr cap:n :}
   ROW-CHECK {: r:n :}
   r ROW-START@ r ROW-CNT@ buf cap CHUNK-ENCODE ;

: DECODE ( ptr u8 n -- row )                \ single-row decode + validate + publish
   {: p:ptr len:n :}
   p DEC-PBUF 0 ptr-field !  len DEC-LEN !
   CHUNK-MARK {: mark:n :}
   [: DECODE-CORE ;] catch {: code:n :}
   code 0 <> if mark CHUNK-TRUNC code throw then
   DEC-ROW @ ROW-BY-IDX ;

: SNAPSHOT ( ptr u8 n -- n )                \ every published row, canonical order
   {: buf:ptr cap:n :}
   buf SNAP-P 0 ptr-field !  cap SNAP-CAP !  0 SNAP-U !
   NOM-SNAP-MAGIC SNAP-PUT
   NOM-SNAP-VERSION SNAP-PUT
   ROW-RECORDS SNAP-PUT
   SNAP-FILL-SORTED
   ROW-RECORDS 0 ?do  NOM-SNAP-AR i AR-CELL@ SNAP-ROW  loop
   SNAP-U @ ;

: RESTORE ( ptr u8 n -- n )                 \ validate + rebuild all rows; returns row count
   {: buf:ptr len:n :}
   buf SNAP-RP 0 ptr-field !  len SNAP-RLEN !  0 SNAP-RU !
   SNAP-GET NOM-SNAP-MAGIC <> if E-NOM-WIRE throw then
   SNAP-GET NOM-SNAP-VERSION <> if E-NOM-WIRE throw then
   SNAP-GET {: cnt:n :}
   cnt 0 < if E-NOM-WIRE throw then
   0 begin dup cnt < while
      SNAP-GET {: rl:n :}
      rl 0 < SNAP-RU @ rl + SNAP-RLEN @ > or if E-NOM-WIRE throw then
      SNAP-RP 0 ptr-field @ SNAP-RU @ +  rl  DECODE drop
      SNAP-RU @ rl + SNAP-RU !
      1+
   repeat drop
   SNAP-RU @ SNAP-RLEN @ <> if E-NOM-WIRE throw then
   cnt ;

private
NOM-SNAP-INIT
;package
