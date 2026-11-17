\ key.f - KEY, an entity's key predicate; part of package ODATA
\ (src/lib/odata.f), which states the contract.
\ Each value's text is read by scanners over ( ^u8 n at ): each answers the
\ text and the index after what it read, and throws E-KEY where the text
\ does not fit, so a check is a chain of scanners ending in WHOLE. An
\ optional part that is not there answers the index it was given.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/uri.f
require src/lib/base64.f
require src/lib/odata/metadata.f

package ODATA
private

\ The predicate being written: what is left of the caller's span, the
\ number of key properties and of those written, and whether the type is V2.
STRUCTURE predicate DERIVE addr
   FIELD rest SPAN:span<u8>
   FIELD keys n
   FIELD written n
   FIELD v2 bool
;STRUCTURE

CAST: >PREDICATE ( ^u8 -- ^predicate )

: FITS ( bool -- ) 0= if E-KEY throw then ;

: PEEK? ( ^u8 n n u8 -- bool )
   {: a:^ u:n at:n c:u8 :}
   at u < if a at + c@ c = else false then ;

: PAST ( ^u8 n n u8 -- ^u8 n n )
   {: a:^ u:n at:n c:u8 :}
   a u at c PEEK? FITS a u at 1+ ;

\ The index after the digits at AT, AT itself for none.
: DIGIT-END ( ^u8 n n -- n )
   {: a:^ u:n at:n :}
   at begin
      dup u < if dup a + c@ STR-DIGIT? else false then
   while 1+ repeat ;

: DIGIT-RUN ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at DIGIT-END {: end:n :}
   end at > FITS a u end ;

: SIGN ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at [char] - PEEK? a u at [char] + PEEK? or
   if a u at 1+ else a u at then ;

: FRACTION ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at [char] . PEEK? if a u at 1+ DIGIT-RUN else a u at then ;

: EXPONENT ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at [char] e PEEK? a u at [char] E PEEK? or
   if a u at 1+ SIGN DIGIT-RUN else a u at then ;

: SHAPE-BYTE? ( u8 u8 -- bool )
   {: c:u8 k:u8 :}
   k [char] d = if c STR-DIGIT? exit then
   k [char] x = if c STR-HEX-DIGIT? exit then
   c k = ;

\ The bytes at AT match SHAPE: `d` a digit, `x` a hex digit, any other
\ byte itself.
: SHAPE ( ^u8 n n ^u8 n -- ^u8 n n )
   {: a:^ u:n at:n s:^ sl:n :}
   at sl + u <= FITS
   sl 0 ?do a at + i + c@ s i + c@ SHAPE-BYTE? FITS loop
   a u at sl + ;

: WHOLE ( ^u8 n n -- ) {: a:^ u:n at:n :} at u = FITS ;

: STAMP ( ^u8 n n -- ^u8 n n ) s" dddd-dd-ddTdd:dd:dd" SHAPE FRACTION ;

\ `Z`, or an offset `+hh:mm` or `-hh:mm`.
: ZONE ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at [char] Z PEEK? if a u at 1+ exit then
   a u at [char] - PEEK? a u at [char] + PEEK? or FITS
   a u at 1+ s" dd:dd" SHAPE ;

\ An optional `1*digit C`.
: UNIT ( ^u8 n n u8 -- ^u8 n n )
   {: a:^ u:n at:n c:u8 :}
   a u at DIGIT-END {: end:n :}
   end at > a u end c PEEK? and if a u end 1+ else a u at then ;

\ An optional `1*digit [. 1*digit] S`.
: SECONDS ( ^u8 n n -- ^u8 n n )
   {: a:^ u:n at:n :}
   a u at DIGIT-END at = if a u at exit then
   a u at DIGIT-RUN FRACTION [char] S PAST ;

\ `T`, then hours, minutes and seconds in that order, one at least.
: CLOCK ( ^u8 n n -- ^u8 n n )
   [char] T PAST {: a:^ u:n t:n :}
   a u t [char] H UNIT [char] M UNIT SECONDS
   dup t > FITS ;

: BOOLEAN ( ^u8 n -- )
   {: a:^ u:n :}
   a u s" true" STR= a u s" false" STR= or FITS ;

: INTEGER ( ^u8 n n n -- )
   {: a:^ u:n lo:n hi:n :}
   a u STR>NUMBER? MATCH option
      none OF E-KEY throw ENDOF
      some OF dup lo >= swap hi <= and FITS ENDOF
   ;MATCH ;

: GUID ( ^u8 n -- )
   0 s" xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" SHAPE WHOLE ;

: DECIMAL ( ^u8 n -- ) 0 SIGN DIGIT-RUN FRACTION WHOLE ;
: FLOAT ( ^u8 n -- ) 0 SIGN DIGIT-RUN FRACTION EXPONENT WHOLE ;
: UTC ( ^u8 n -- ) 0 STAMP [char] Z PAST WHOLE ;
: DATE-OFFSET ( ^u8 n -- ) 0 STAMP ZONE WHOLE ;
: DATE ( ^u8 n -- ) 0 s" dddd-dd-dd" SHAPE WHOLE ;
: TIME ( ^u8 n -- ) 0 [char] P PAST CLOCK WHOLE ;

: TIME-OF-DAY ( ^u8 n -- )
   0 s" dd:dd" SHAPE {: a:^ u:n at:n :}
   a u at [char] : PEEK? if
      a u at 1+ s" dd" SHAPE FRACTION
   else
      a u at
   then WHOLE ;

\ `[-]P`, days, then a clock, one at least.
: DURATION ( ^u8 n -- )
   0 SIGN [char] P PAST {: a:^ u:n at:n :}
   at u < FITS
   a u at [char] D UNIT {: b:^ bl:n t:n :}
   a u t [char] T PEEK? if a u t CLOCK else a u t then WHOLE ;

\ Writing: RAW writes bytes as they are, ESCAPED every byte outside `-._~`,
\ letters and digits as `%XX`; a span too short is E-SPAN-CAPACITY.
: REST+ ( ^predicate n -- )
   {: p:^ len:n :}
   p PREDICATE-REST @ len SPAN:SKIP p PREDICATE-REST ! ;

: RAW ( ^predicate ^u8 n -- )
   {: p:^ a:^ u:n :}
   a u p PREDICATE-REST @ SPAN:COPY p u REST+ ;

: ESCAPED ( ^predicate ^u8 n -- )
   {: p:^ a:^ u:n :}
   p a u p PREDICATE-REST @ URI:QUERY-ENCODE REST+ ;

: QUOTED ( ^predicate ^u8 n -- )
   {: p:^ a:^ u:n :}
   p s" '" RAW
   u 0 ?do
      a i + c@ [char] ' = if p s" ''" else p a i + 1 then ESCAPED
   loop
   p s" '" RAW ;

\ `tag'value'`.
: TAGGED ( ^predicate ^u8 n ^u8 n -- )
   {: p:^ tag:^ tl:n v:^ vl:n :}
   p tag tl RAW p s" '" RAW p v vl ESCAPED p s" '" RAW ;

: V2-TAGGED ( ^predicate ^u8 n ^u8 n -- )
   {: p:^ tag:^ tl:n v:^ vl:n :}
   p PREDICATE-V2 @ if p tag tl v vl TAGGED else p v vl ESCAPED then ;

: V2-SUFFIXED ( ^predicate ^u8 n ^u8 n -- )
   {: p:^ v:^ vl:n suffix:^ sl:n :}
   p v vl ESCAPED p PREDICATE-V2 @ if p suffix sl RAW then ;

: NIBBLE ( ^predicate n -- )
   {: p:^ k:n :}
   p s" 0123456789ABCDEF" drop k + 1 RAW ;

\ A base64 failure in the value is E-KEY; any other code throws on.
: DECODED-LEN ( ^u8 n SPAN:span<u8> [ ^u8 n SPAN:span<u8> -- n ] -- n )
   {: a:^ u:n s dec :}
   0 [: drop a u s dec execute ;] catch {: len:n code:n :}
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and if E-KEY throw then
   code 0<> if code throw then
   len ;

: HEX ( ^predicate ^u8 n ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN {: p:^ v:^ vl:n s :}
   v vl s [: BASE64:DECODE ;] DECODED-LEN {: len:n :}
   s SPAN:$ drop {: bytes:^ :}
   p s" X'" RAW
   len 0 ?do
      bytes i + c@ {: b:n :}
      p b 4 rshift NIBBLE p b 15 and NIBBLE
   loop
   p s" '" RAW ;

: URL-CHECK ( ^u8 n ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN [: BASE64:DECODE-URL ;] DECODED-LEN drop ;

\ Room for what base64 text of N bytes decodes to.
: DECODE-ROOM ( n -- NUM:alloc-byte-len ) 4 / 3 * 3 + MEM:BYTES-ALLOC-LEN ;

: UNPAD ( ^u8 n -- ^u8 n ) 2dup + 1- c@ [char] = = if 1- then ;

\ V2 writes the base64 value's bytes as `X'hex'`; V4 writes the base64url
\ value as `binary'value'`, its `=` padding optional.
: BINARY ( ^predicate ^u8 n -- )
   {: p:^ v:^ vl:n :}
   p PREDICATE-V2 @ if
      p v vl vl DECODE-ROOM [: HEX ;] MEM:WITH-BYTES exit
   then
   v vl vl 3 and 0= vl 0 > and if UNPAD UNPAD then
   dup DECODE-ROOM [: URL-CHECK ;] MEM:WITH-BYTES
   p s" binary" v vl TAGGED ;

\ Whether TYPE is `Edm.` and NAME.
: EDM? ( ^u8 n ^u8 n -- bool )
   {: t:^ tl:n name:^ nl:n :}
   tl nl 4 + = 0= if false exit then
   t 4 s" Edm." STR= t 4 + nl name nl STR= and ;

\ An integer type's least and greatest values, and whether TYPE is one.
: RANGE ( ^u8 n -- n n bool )
   {: t:^ tl:n :}
   t tl s" Byte" EDM? if 0 255 true exit then
   t tl s" SByte" EDM? if -128 127 true exit then
   t tl s" Int16" EDM? if -32768 32767 true exit then
   t tl s" Int32" EDM? if -2147483648 2147483647 true exit then
   t tl s" Int64" EDM? if
      $8000000000000000 $7FFFFFFFFFFFFFFF true exit
   then
   0 0 false ;

\ Each row checks the value, then writes its literal, and answers whether
\ TYPE is its type.
: INTEGRAL ( ^predicate ^u8 n ^u8 n -- bool )
   {: p:^ t:^ tl:n v:^ vl:n :}
   t tl RANGE 0= if 2drop false exit then
   v vl 2swap INTEGER
   p v vl t tl s" Int64" EDM? if s" L" else s" " then V2-SUFFIXED
   true ;

: TEXTUAL ( ^predicate ^u8 n ^u8 n -- bool )
   {: p:^ t:^ tl:n v:^ vl:n :}
   t tl s" String" EDM? if p v vl QUOTED true exit then
   t tl s" Boolean" EDM? if v vl BOOLEAN p v vl ESCAPED true exit then
   t tl s" Guid" EDM? if
      v vl GUID p s" guid" v vl V2-TAGGED true exit
   then
   t tl s" Binary" EDM? if p v vl BINARY true exit then
   false ;

: FRACTIONAL ( ^predicate ^u8 n ^u8 n -- bool )
   {: p:^ t:^ tl:n v:^ vl:n :}
   t tl s" Decimal" EDM? if
      v vl DECIMAL p v vl s" M" V2-SUFFIXED true exit
   then
   t tl s" Double" EDM? if
      v vl FLOAT p v vl s" d" V2-SUFFIXED true exit
   then
   t tl s" Single" EDM? if
      v vl FLOAT p v vl s" f" V2-SUFFIXED true exit
   then
   false ;

\ The text is ISO 8601 UTC; V2's datetime literal drops its `Z`.
: V2-TEMPORAL ( ^predicate ^u8 n ^u8 n -- bool )
   {: p:^ t:^ tl:n v:^ vl:n :}
   t tl s" DateTime" EDM? if
      v vl UTC p s" datetime" v vl 1- TAGGED true exit
   then
   t tl s" DateTimeOffset" EDM? if
      v vl UTC p s" datetimeoffset" v vl TAGGED true exit
   then
   t tl s" Time" EDM? if
      v vl TIME p s" time" v vl TAGGED true exit
   then
   false ;

: V4-TEMPORAL ( ^predicate ^u8 n ^u8 n -- bool )
   {: p:^ t:^ tl:n v:^ vl:n :}
   t tl s" DateTimeOffset" EDM? if
      v vl DATE-OFFSET p v vl ESCAPED true exit
   then
   t tl s" Date" EDM? if v vl DATE p v vl ESCAPED true exit then
   t tl s" TimeOfDay" EDM? if
      v vl TIME-OF-DAY p v vl ESCAPED true exit
   then
   t tl s" Duration" EDM? if
      v vl DURATION p s" duration" v vl TAGGED true exit
   then
   false ;

\ A type no row takes, a version's missing type among them, is E-KEY.
: LITERAL ( ^predicate ^u8 n ^u8 n -- )
   {: p:^ t:^ tl:n v:^ vl:n :}
   p t tl v vl TEXTUAL if exit then
   p t tl v vl INTEGRAL if exit then
   p t tl v vl FRACTIONAL if exit then
   p PREDICATE-V2 @ if
      p t tl v vl V2-TEMPORAL
   else
      p t tl v vl V4-TEMPORAL
   then FITS ;

\ The separator and the name go before the callback, so E-KEY follows the
\ callback that gave the value with nothing written between.
: KEY-PART ( ^property ^predicate [ ^property -- ^u8 n ] -- )
   {: pr:^ p:^ q :}
   p PREDICATE-WRITTEN @ 0 > if p s" ," RAW then
   p PREDICATE-KEYS @ 1 > if
      p pr PROPERTY-NAME$ ESCAPED p s" =" RAW
   then
   p pr PROPERTY-TYPE$ pr q execute LITERAL
   1 p PREDICATE-WRITTEN +! ;

: WRITE-PREDICATE ( ^entity-type [ ^property -- ^u8 n ] SPAN:span<u8> ^u8 NUM:alloc-byte-len -- n )
   drop >PREDICATE {: t:^ q s p:^ :}
   s p PREDICATE-REST !
   t TYPE-V2? p PREDICATE-V2 !
   t [: drop 1 p PREDICATE-KEYS +! ;] EACH-KEY
   p s" (" RAW
   t [: p q KEY-PART ;] EACH-KEY
   p s" )" RAW
   s SPAN:LEN p PREDICATE-REST @ SPAN:LEN - ;

public

\ The key predicate of the entity whose key values CALLBACK gives as text,
\ written into the span; its length.
: KEY ( ^entity-type [ ^property -- ^u8 n ] SPAN:span<u8> -- n )
   PREDICATE-BYTES MEM:BYTES-ALLOC-LEN
   [: WRITE-PREDICATE ;] MEM:WITH-BYTES ;

;package
