\ Execute the frozen tag checks before a native fetch produces a typed value.
require src/core/util.f
require src/lib/error.f
require src/lib/prelude.f
require src/lib/string.f
require src/core/engine-error.f
require src/lib/le.f

package NFETCH-CHECK
public

\ A descriptor whose structure SHAPE has already accepted. FREEZE is its only
\ producer and the casts below are private, so a frozen value is a witness that
\ the tag walk may read the descriptor's cells without bounding them again. The
\ checker has no term for the integer a pointer is, so the witness carries the
\ sanctioned pointer-minus-pointer distance from the null address.
NEWTYPE frozen

private

\ The longest message: its text and two numbers of up to 20 bytes each.
80 SCRATCH-BUFFER: MSG

\ Text a u laid at byte at of MSG; answers the byte after it.
: PUT$ ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   a  MSG at +  u BYTE-COPY
   at u + ;

: PUT-INT ( n n -- n ) {: v:n at:n :}
   at  v 0< if s" -" rot PUT$ then
   v ERROR-TEXT:DIGITS$ rot PUT$ ;

public

\ Tier 0 calls this from its inline check of a one-row descriptor with no
\ guards (t0.f TAG-INLINE).
: BAD-TAG ( n n -- ) {: tag:n limit:n :}
   s" E-BAD-TAG habu: layout tag " 0 PUT$
   tag swap PUT-INT
   s"  outside 0.." rot PUT$
   limit 1- swap PUT-INT
   MSG swap  ENGINE-ERROR:BAD-TAG die ;

private

CAST: >FROZEN ( n -- frozen )
CAST: FROZEN>N ( frozen -- n )

: SLOT@ ( ptr u8 n -- n ) 8 * + LE:U64@ ;

: BAD-DESCRIPTOR ( -- )
   s" hb: bad layout descriptor" 76 die ;

\ Every certified limit is positive; a negative cell is above that domain when
\ read unsigned, including the all-bits-set representation.
: IN-DOMAIN? ( n n -- bool ) {: value:n limit:n :}
   value 0 >= value limit < and ;

: CELL@ ( ptr u8 n n -- n ) {: data:ptr count:n index:n :}
   index count IN-DOMAIN? 0= if BAD-DESCRIPTOR then
   data index SLOT@ ;

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
   base offset SLOT@ dup limit IN-DOMAIN? 0= if dup limit BAD-TAG then ;

: ACTIVE? ( ptr u8 ptr u8 n n -- bool )
   {: base:ptr data:ptr index:n guards:n :}
   guards 0 ?do
      index i 3 * + {: row:n :}
      base data row SLOT@ data row 2 + SLOT@ TAG@
      data row 1+ SLOT@ <> if false unloop exit then
   loop
   true ;

: RUN-ROW ( ptr u8 ptr u8 n -- n )
   {: base:ptr data:ptr index:n :}
   data index 2 + SLOT@ {: guards:n :}
   base data index 3 + guards ACTIVE? if
      base data index SLOT@ data index 1+ SLOT@ TAG@ drop
   then
   index 3 + guards 3 * + ;

public

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
   data 0 SLOT@ 0 ?do base data rot RUN-ROW loop
   drop ;

: CHECK ( ptr u8 ptr u8 n n -- )
   {: base:ptr data:ptr bytes:n width:n :}
   base data bytes width FREEZE TAGS ;

;package
