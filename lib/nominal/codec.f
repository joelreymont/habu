\ codec.f - canonical content codec for the immutable nominal collection
\ (dot habu-add-immutable-nominal-9290a81f).
\
\ The canonical form of a chunk is derived ENTIRELY from contents in sorted order:
\ a big-endian binding count, then per binding a big-endian depth, that many
\ big-endian atom cells emitted ROOT-FIRST, then the slot cell. No handle number,
\ arena index, pointer, or allocation-order artefact ever enters the stream, so two
\ equal-content rows built in different insertion orders or in different processes
\ serialise to byte-identical bytes and hash to the same sha256 digest. That digest
\ is the ONLY row identity used for equality, cache keys, interning, wire, and AOT.
\
\ CHUNK-DIGEST streams the canonical bytes straight through sha256 (no full
\ buffer); CHUNK-ENCODE writes them to a caller buffer; CHUNK-DECODE parses a
\ buffer back into a fresh chunk, validating bounds, strictly-increasing paths
\ (sortedness + path uniqueness), depth budget, and full consumption - any breach
\ fails closed with E-NOM-WIRE and publishes nothing. A single deferred value sink
\ (NOM-SINK) shares the walk between the digest and encode emitters.
\
\ Load after lib/nominal/binding.f. Reopens package NOM. sha256 words are baked.

require lib/nominal/binding.f

$10000 constant NOM-WIRE-DEPTH-MAX         \ decode depth budget (wire safety, not capacity)

package NOM
private

create NOM-STAGE 16 allot                  \ 8-byte BE staging cell
create NOM-DIGEST 40 allot                  \ 32-byte canonical digest scratch
create NOM-ENC 4 cells allot                \ path-materialise scratch for the codec
PTR-VARIABLE NOM-ENCB                        \ encode output base
PTR-VARIABLE NOM-DECB                        \ decode input base
variable NOM-ENC-CAP  variable NOM-ENC-U
variable NOM-DEC-CAP  variable NOM-DEC-U
variable DEC-PREV     variable DEC-N  variable DEC-K

defer NOM-SINK ( n -- )                     \ per-value canonical sink (digest or encode)

: BE64@ ( ptr u8 -- n ) {: p:ptr :}         \ read 8 bytes big-endian
   0
   8 0 do  8 lshift  p i + c@ or  loop ;

\ ---- streaming sinks ---------------------------------------------------------
: SHA-PUT ( n -- )
   NOM-STAGE BE64!  NOM-STAGE 8 SHA256-UPDATE ;

: ENC-ROOM ( n -- ) {: k:n :}
   NOM-ENC-U @ k + NOM-ENC-CAP @ > if E-NOM-CAPACITY throw then ;

: ENC-PUT ( n -- ) {: v:n :}
   8 ENC-ROOM
   v NOM-ENCB 0 ptr-field @ NOM-ENC-U @ + BE64!
   NOM-ENC-U @ 8 + NOM-ENC-U ! ;

\ ---- shared canonical walk over a chunk slice --------------------------------
: SINK-PATH ( n -- )                        \ depth + atoms, root-first
   NOM-ENC PATH-MAT
   NOM-ENC AR-USED@ dup NOM-SINK {: d:n :}
   0 begin dup d < while
      dup {: k:n :}
      NOM-ENC d 1- k - AR-CELL@ NOM-SINK
      1+
   repeat drop ;

: SINK-BIND ( n -- ) {: b:n :}
   b BIND-PATH@ SINK-PATH
   b BIND-SLOT@ NOM-SINK ;

: SINK-CHUNK ( n n -- ) {: start:n cnt:n :}
   cnt NOM-SINK
   0 begin dup cnt < while
      dup {: k:n :}
      start k + CHUNK@ SINK-BIND
      1+
   repeat drop ;

\ ---- digest (the row's content key) ------------------------------------------
: CHUNK-DIGEST ( n n -- )                   \ ( start count -- ) leaves 32 bytes in NOM-DIGEST
   [: SHA-PUT ;] is NOM-SINK
   SHA256-RESET
   SINK-CHUNK
   NOM-DIGEST SHA256-FINAL ;

: DIGEST-CELL@ ( n -- a ) {: k:n :}   NOM-DIGEST k cells + @ ;

: DIGEST>OUT ( ptr u8 -- ) {: p:ptr :}      \ copy the current 32-byte digest out
   0 begin dup 32 < while
      dup NOM-DIGEST + c@  over p + c!  1+
   repeat drop ;

: DIG-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup 32 < while
      dup pa + c@  over pb + c@  <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

\ ---- encode (wire) -----------------------------------------------------------
: CHUNK-ENCODE ( n n ptr u8 n -- n )        \ ( start count buf cap -- len )
   NOM-ENC-CAP !  NOM-ENCB 0 ptr-field !
   0 NOM-ENC-U !
   [: ENC-PUT ;] is NOM-SINK
   SINK-CHUNK
   NOM-ENC-U @ ;

\ ---- decode (wire -> fresh validated chunk) ----------------------------------
: DEC-ROOM ( n -- ) {: k:n :}
   NOM-DEC-U @ k + NOM-DEC-CAP @ > if E-NOM-WIRE throw then ;

: DEC-GET ( -- n )
   8 DEC-ROOM
   NOM-DECB 0 ptr-field @ NOM-DEC-U @ + BE64@
   NOM-DEC-U @ 8 + NOM-DEC-U ! ;

: DEC-PATH ( -- path )
   DEC-GET {: d:n :}
   d 0 < d NOM-WIRE-DEPTH-MAX > or if E-NOM-WIRE throw then
   ROOT
   0 DEC-K !
   begin DEC-K @ d < while
      DEC-GET CONS
      DEC-K @ 1+ DEC-K !
   repeat ;

: DEC-BIND ( -- binding )   DEC-PATH DEC-GET BIND ;

: DEC-STEP ( -- )
   DEC-BIND BIND-IDX {: cur:n :}
   DEC-PREV @ 0 >= if
      DEC-PREV @ BIND-PATH@ cur BIND-PATH@ PATH-CMP 0 < 0= if E-NOM-WIRE throw then
   then
   cur CHUNK+ drop
   cur DEC-PREV ! ;

: CHUNK-DECODE ( ptr u8 n -- n n )          \ ( buf len -- start count )
   NOM-DEC-CAP !  NOM-DECB 0 ptr-field !
   0 NOM-DEC-U !
   DEC-GET {: cnt:n :}
   cnt 0 < if E-NOM-WIRE throw then
   CHUNK-USED {: start:n :}
   -1 DEC-PREV !  0 DEC-N !
   begin DEC-N @ cnt < while
      DEC-STEP  DEC-N @ 1+ DEC-N !
   repeat
   NOM-DEC-U @ NOM-DEC-CAP @ <> if E-NOM-WIRE throw then
   start  cnt ;

: NOM-CODEC-INIT ( -- )   NOM-ENC AR-INIT ;

private
NOM-CODEC-INIT
;package
