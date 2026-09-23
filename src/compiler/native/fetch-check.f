\ Execute the frozen tag checks before a native fetch produces a typed value.
require lib/errors.f
require lib/prelude.f
require src/core/engine-error.f
require src/compiler/digest.f

package NFETCH-CHECK
public

\ A descriptor whose structure SHAPE has already accepted. FREEZE is its only
\ producer and the casts below are private, so a frozen value is a witness that
\ the tag walk may read the descriptor's cells without bounding them again. The
\ checker has no term for the integer a pointer is, so the witness carries the
\ sanctioned pointer-minus-pointer distance from the null address.
NEWTYPE frozen 0

private

CAST: >FROZEN ( n -- frozen )
CAST: FROZEN>N ( frozen -- n )

: BAD-DESCRIPTOR ( -- )
   s" hb: bad layout descriptor" 76 die ;

: BAD-TAG ( -- )
   s" hb: bad layout tag" ENGINE-ERROR:BAD-TAG die ;

\ Every certified limit is positive; a negative cell is above that domain when
\ read unsigned, including the all-bits-set representation.
: IN-DOMAIN? ( n n -- bool ) {: value:n limit:n :}
   value 0 >= value limit < and ;

: CELL@ ( ptr u8 n n -- n ) {: data:ptr count:n index:n :}
   index count IN-DOMAIN? 0= if BAD-DESCRIPTOR then
   data index CDIGEST:SLOT@ ;

: FIELD-CK ( n n n -- ) {: offset:n limit:n width:n :}
   offset width IN-DOMAIN? 0= limit 0 <= or if BAD-DESCRIPTOR then ;

: GUARD-CK ( ptr u8 n n n -- )
   {: data:ptr count:n index:n width:n :}
   data count index CELL@ data count index 2 + CELL@ width FIELD-CK
   data count index 1+ CELL@ data count index 2 + CELL@ IN-DOMAIN? 0= if BAD-DESCRIPTOR then ;

: CHECK-END ( ptr u8 n n -- n ) {: data:ptr count:n index:n :}
   data count index 2 + CELL@ {: guards:n :}
   guards 0 < if BAD-DESCRIPTOR then
   guards count index - 3 - 3 / > if BAD-DESCRIPTOR then
   index 3 + guards 3 * + ;

: SHAPE-ROW ( ptr u8 n n n -- n )
   {: data:ptr count:n index:n width:n :}
   data count index CHECK-END {: next:n :}
   data count index CELL@ data count index 1+ CELL@ width FIELD-CK
   data count index 2 + CELL@ 0 ?do
      data count index 3 + i 3 * + width GUARD-CK
   loop
   next ;

: TAG@ ( ptr u8 n n -- n ) {: base:ptr offset:n limit:n :}
   base offset CDIGEST:SLOT@ dup limit IN-DOMAIN? 0= if BAD-TAG then ;

: ACTIVE? ( ptr u8 ptr u8 n n -- bool )
   {: base:ptr data:ptr index:n guards:n :}
   guards 0 ?do
      index i 3 * + {: row:n :}
      base data row CDIGEST:SLOT@ data row 2 + CDIGEST:SLOT@ TAG@
      data row 1+ CDIGEST:SLOT@ <> if false unloop exit then
   loop
   true ;

: RUN-ROW ( ptr u8 ptr u8 n -- n )
   {: base:ptr data:ptr index:n :}
   data index 2 + CDIGEST:SLOT@ {: guards:n :}
   base data index 3 + guards ACTIVE? if
      base data index CDIGEST:SLOT@ data index 1+ CDIGEST:SLOT@ TAG@ drop
   then
   index 3 + guards 3 * + ;

public

\ Byte decoding supports NSTR rows at every alignment; CELL-VIEW alone would
\ reject a descriptor placed after a string of an odd byte length.
: SHAPE ( ptr u8 n n -- ) {: data:ptr bytes:n width:n :}
   bytes 0 <= bytes 8 mod 0<> or width 0 <= or if BAD-DESCRIPTOR then
   bytes 8 / {: count:n :}
   data count 0 CELL@ {: checks:n :}
   checks 0 < checks count 1- 3 / > or if BAD-DESCRIPTOR then
   1
   checks 0 ?do data count rot width SHAPE-ROW loop
   count <> if BAD-DESCRIPTOR then ;

: FREEZE ( ptr u8 n n -- frozen ) {: data:ptr bytes:n width:n :}
   data bytes width SHAPE
   data NULL-PTR BYTE-VIEW - >FROZEN ;

\ The tag walk alone: every index it reads was bounded when FREEZE ran SHAPE
\ over these very bytes.
: TAGS ( ptr u8 frozen -- ) {: base:ptr desc:frozen :}
   NULL-PTR BYTE-VIEW desc FROZEN>N + {: data:ptr :}
   1
   data 0 CDIGEST:SLOT@ 0 ?do base data rot RUN-ROW loop
   drop ;

: CHECK ( ptr u8 ptr u8 n n -- )
   {: base:ptr data:ptr bytes:n width:n :}
   base data bytes width FREEZE TAGS ;

;package
