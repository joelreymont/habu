\ row.f - the immutable row arena: interned sorted-unique binding sets
\ (dot habu-add-immutable-nominal-9290a81f).
\
\ A published row is an append-only record [chunk-start, chunk-count, digest x4]
\ whose public value is the opaque nominal `row` over its record index. PUBLISH-
\ CHUNK is the single publication path shared by FREEZE, UNION, and REMAP: it
\ digests the sorted chunk canonically, validates strictly-increasing paths, then
\ FULL-CONTENT-INTERNS by digest - a row whose content already exists returns the
\ existing handle and discards the redundant chunk (high-water restored), so a
\ full-row duplicate is idempotent and published records never mutate. Identity is
\ the digest, never the record index: EQUAL? and KEY read canonical contents.
\
\ UNION streams two rows' sorted chunks into one (conflicting slots fail closed);
\ REMAP prefixes one stable outer atom onto every path, preserving exact relative
\ paths and exact slots. Both route through PUBLISH-CHUNK, so their results intern
\ identically regardless of how they were produced.
\
\ Load after lib/nominal/codec.f. Reopens package NOM.

require lib/nominal/codec.f

64 constant NOM-HT-SEED                      \ initial row-intern table slots (power of two)

package NOM
public

\ The published opaque nominal row handle. External consumers (src/cad/effect.f)
\ name NOM:row directly in signatures (dot habu-export-public-nom-20170121). The
\ record index and the audited mint/erase boundary stay private below.
TYPEFAMILY row 0

private

create NOM-ROW-AR 4 cells allot             \ row r -> cells [6r]=start [6r+1]=count [6r+2..5]=digest
PTR-VARIABLE NOM-HT-A                         \ row-intern hash table (digest -> row index, -1 empty)
variable NOM-HT-CAP  variable NOM-HT-USED
variable HT-I

TRUSTED: MINT-ROW ( n -- row ) ;             \ audited record-index -> handle boundary
TRUSTED: ROW-IDX ( row -- n ) ;              \ proof-erasure: handle -> record index

: ROW-BY-IDX ( n -- row )   MINT-ROW ;        \ row.f-owned re-mint for sibling files

: ROW-RECORDS ( -- n )   NOM-ROW-AR AR-USED@ 6 / ;
: ROW-START@ ( n -- n ) {: r:n :}   NOM-ROW-AR r 6 * AR-CELL@ ;
: ROW-CNT@ ( n -- n ) {: r:n :}     NOM-ROW-AR r 6 * 1 + AR-CELL@ ;
: ROW-DIGEST-CELL@ ( n n -- a ) {: r:n k:n :}   NOM-ROW-AR r 6 * 2 + k + AR-CELL@ ;

: ROW-CHECK ( row -- n )
   ROW-IDX {: r:n :}
   r 0 < r ROW-RECORDS >= or if E-NOM-HANDLE throw then
   r ;

\ ---- row-intern hash table (digest keyed) ------------------------------------
: HT-BASE ( -- ptr a )   NOM-HT-A 0 ptr-field @ ;
: HT@ ( n -- n ) {: i:n :}   HT-BASE i cells + @ ;
: HT! ( n n -- ) {: v:n i:n :}   v HT-BASE i cells + ! ;
: HT-FILL-EMPTY ( -- )
   0 begin dup NOM-HT-CAP @ < while  dup -1 swap HT!  1+  repeat drop ;
: HT-ALLOC ( n -- ) {: cap:n :}
   cap >COUNT MEM-ALLOC-CELLS NOM-HT-A 0 ptr-field !
   cap NOM-HT-CAP !  0 NOM-HT-USED !
   HT-FILL-EMPTY ;

: HT-MIX ( n -- n ) {: x:n :}   x x 33 rshift xor ;
: HT-BUCKET ( -- n )   0 DIGEST-CELL@ HT-MIX NOM-HT-CAP @ 1- and ;

: ROW-DIG-EQ? ( n -- bool ) {: r:n :}       \ row's stored digest vs current NOM-DIGEST
   0 begin dup 4 < while
      dup {: k:n :}
      r k ROW-DIGEST-CELL@  k DIGEST-CELL@  <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: HT-FIND ( -- n )                          \ row index matching NOM-DIGEST, or -1
   HT-BUCKET HT-I !
   begin
      HT-I @ HT@ dup 0 < if drop -1 exit then
      dup ROW-DIG-EQ? if exit then
      drop
      HT-I @ 1+ NOM-HT-CAP @ 1- and HT-I !
   again ;

: HT-PUT ( n -- ) {: r:n :}                 \ insert r at the first empty slot for NOM-DIGEST
   HT-BUCKET HT-I !
   begin
      HT-I @ HT@ 0 < if r HT-I @ HT! exit then
      HT-I @ 1+ NOM-HT-CAP @ 1- and HT-I !
   again ;

: ROW-DIGEST>SCRATCH ( n -- ) {: r:n :}     \ load a row's stored digest into NOM-DIGEST
   0 begin dup 4 < while
      dup {: k:n :}
      r k ROW-DIGEST-CELL@ NOM-DIGEST k cells + !
      1+
   repeat drop ;

: HT-GROW? ( -- bool )   NOM-HT-USED @ 1+ 2 * NOM-HT-CAP @ >= ;

: HT-REHASH ( -- )
   NOM-HT-CAP @ 2 * {: ncap:n :}
   ncap >COUNT MEM-ALLOC-CELLS NOM-HT-A 0 ptr-field !
   ncap NOM-HT-CAP !  0 NOM-HT-USED !
   HT-FILL-EMPTY
   ROW-RECORDS 0 ?do  i ROW-DIGEST>SCRATCH  i HT-PUT  loop
   ROW-RECORDS NOM-HT-USED ! ;

: HT-INSERT ( n -- ) {: r:n :}
   HT-GROW? if HT-REHASH then
   r ROW-DIGEST>SCRATCH
   r HT-PUT
   NOM-HT-USED @ 1+ NOM-HT-USED ! ;

\ ---- publication (shared by FREEZE / UNION / REMAP) --------------------------
: CHUNK-VALIDATE ( n n -- ) {: start:n cnt:n :}   \ strictly increasing paths
   1 begin dup cnt < while
      dup {: k:n :}
      start k 1- + CHUNK@ BIND-PATH@  start k + CHUNK@ BIND-PATH@  PATH-CMP 0 < 0=
      if start CHUNK-TRUNC E-NOM-WIRE throw then
      1+
   repeat drop ;

: ROW-NODE+ ( n n -- n ) {: start:n cnt:n :}      \ append a row record, return its index
   ROW-RECORDS {: r:n :}
   start NOM-ROW-AR AR-PUSH drop
   cnt NOM-ROW-AR AR-PUSH drop
   0 begin dup 4 < while  dup DIGEST-CELL@ NOM-ROW-AR AR-PUSH drop  1+  repeat drop
   NOM-ROW-AR AR-SEAL-COMMIT
   r ;

: PUBLISH-CHUNK ( n n -- row ) {: start:n cnt:n :}
   start cnt CHUNK-VALIDATE
   start cnt CHUNK-DIGEST
   HT-FIND {: hit:n :}
   hit 0 >= if  start CHUNK-TRUNC  hit MINT-ROW exit  then
   CHUNK-COMMIT
   start cnt ROW-NODE+ {: r:n :}
   r HT-INSERT
   r MINT-ROW ;

: NOM-ROW-INIT ( -- )
   NOM-ROW-AR AR-INIT
   NOM-HT-SEED HT-ALLOC ;

public

: SIZE ( row -- n )   ROW-CHECK ROW-CNT@ ;

: EQUAL? ( row row -- bool )                \ content identity: compare canonical digests
   ROW-CHECK {: rb:n :}  ROW-CHECK {: ra:n :}
   0 begin dup 4 < while
      dup {: k:n :}
      ra k ROW-DIGEST-CELL@  rb k ROW-DIGEST-CELL@  <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: KEY ( row ptr u8 -- )                     \ write the 32-byte canonical content key
   swap ROW-CHECK {: r:n :} {: p:ptr :}
   0 begin dup 4 < while
      dup {: k:n :}
      r k ROW-DIGEST-CELL@  p k 8 * + BE64!
      1+
   repeat drop ;

: UNION ( row row -- row )                  \ merge two sorted-unique rows
   ROW-CHECK {: rb:n :}  ROW-CHECK {: ra:n :}
   ra ROW-START@ ra ROW-CNT@ rb ROW-START@ rb ROW-CNT@ CHUNK-MERGE
   PUBLISH-CHUNK ;

: REMAP ( row a -- row )                    \ scope every path under one outer atom
   {: atom:n :}
   ROW-CHECK {: r:n :}
   r ROW-START@ {: s:n :}  r ROW-CNT@ {: cnt:n :}
   CHUNK-MARK {: mark:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      s k + CHUNK@ {: b:n :}
      b BIND-PATH@ atom REMAP-PATH
      b BIND-SLOT@ BIND BIND-IDX CHUNK+ drop
      1+
   repeat drop
   mark  CHUNK-USED mark -  PUBLISH-CHUNK ;

private
NOM-ROW-INIT
;package
