split-str
\ key.f - KEY, an entity's key predicate; part of package ODATA
\ (src/lib/odata.f), which states the contract.
\ Each value's text is read by scanners over ( str at ): each answers the
\ text and the index after what it read, and throws E-KEY where the text
\ does not fit, so a check is a chain of scanners ending in WHOLE. An
\ optional part that is not there answers the index it was given.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/uri.f
require src/lib/vector.f
require src/lib/base64.f
require src/lib/reason.f
require src/lib/odata/metadata.f

package ODATA
private

\ The predicate being written: out is the caller's vector.
STRUCTURE predicate DERIVE addr
   FIELD out ^VEC:vector<u8>
   FIELD keys n
   FIELD written n
   FIELD v2 bool
;STRUCTURE

CAST: >PREDICATE ( ^u8 -- ^predicate )

: FITS ( bool -- ) 0= if E-KEY throw then ;

: PEEK? ( str n u8 -- bool )
   {: a:^ u:n at:n c:u8 :}
   at u < if a at + c@ c = else false then ;

: PAST ( str n u8 -- str n )
   {: a:^ u:n at:n c:u8 :}
   a u at c PEEK? FITS a u at 1+ ;

\ The index after the digits at AT, AT itself for none.
: DIGIT-END ( str n -- n )
   {: a:^ u:n at:n :}
   at begin
      dup u < if dup a + c@ STR-DIGIT? else false then
   while 1+ repeat ;

: DIGIT-RUN ( str n -- str n )
   {: a:^ u:n at:n :}
   a u at DIGIT-END {: end:n :}
   end at > FITS a u end ;

: SIGN ( str n -- str n )
   {: a:^ u:n at:n :}
   a u at [char] - PEEK? a u at [char] + PEEK? or
   if a u at 1+ else a u at then ;

: FRACTION ( str n -- str n )
   {: a:^ u:n at:n :}
   a u at [char] . PEEK? if a u at 1+ DIGIT-RUN else a u at then ;

: EXPONENT ( str n -- str n )
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
: SHAPE ( str n str -- str n )
   {: a:^ u:n at:n s:^ sl:n :}
   at sl + u <= FITS
   sl 0 ?do a at + i + c@ s i + c@ SHAPE-BYTE? FITS loop
   a u at sl + ;

: WHOLE ( str n -- ) {: a:^ u:n at:n :} at u = FITS ;

: STAMP ( str n -- str n ) s" dddd-dd-ddTdd:dd:dd" SHAPE FRACTION ;

\ `Z`, or an offset `+hh:mm` or `-hh:mm`.
: ZONE ( str n -- str n )
   {: a:^ u:n at:n :}
   a u at [char] Z PEEK? if a u at 1+ exit then
   a u at [char] - PEEK? a u at [char] + PEEK? or FITS
   a u at 1+ s" dd:dd" SHAPE ;

\ An optional `1*digit C`.
: UNIT ( str n u8 -- str n )
   {: a:^ u:n at:n c:u8 :}
   a u at DIGIT-END {: end:n :}
   end at > a u end c PEEK? and if a u end 1+ else a u at then ;

\ An optional `1*digit [. 1*digit] S`.
: SECONDS ( str n -- str n )
   {: a:^ u:n at:n :}
   a u at DIGIT-END at = if a u at exit then
   a u at DIGIT-RUN FRACTION [char] S PAST ;

\ `T`, then hours, minutes and seconds in that order, one at least.
: CLOCK ( str n -- str n )
   [char] T PAST {: a:^ u:n t:n :}
   a u t [char] H UNIT [char] M UNIT SECONDS
   dup t > FITS ;

: BOOLEAN ( str -- )
   {: a:^ u:n :}
   a u s" true" STR= a u s" false" STR= or FITS ;

: INTEGER ( str n n -- )
   {: a:^ u:n lo:n hi:n :}
   a u STR>NUMBER? MATCH option
      none OF E-KEY throw ENDOF
      some OF dup lo >= swap hi <= and FITS ENDOF
   ;MATCH ;

: GUID ( str -- )
   0 s" xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" SHAPE WHOLE ;

: DECIMAL ( str -- ) 0 SIGN DIGIT-RUN FRACTION WHOLE ;
: FLOAT ( str -- ) 0 SIGN DIGIT-RUN FRACTION EXPONENT WHOLE ;
: UTC ( str -- ) 0 STAMP [char] Z PAST WHOLE ;
: DATE-OFFSET ( str -- ) 0 STAMP ZONE WHOLE ;
: DATE ( str -- ) 0 s" dddd-dd-dd" SHAPE WHOLE ;
: TIME ( str -- ) 0 [char] P PAST CLOCK WHOLE ;

: TIME-OF-DAY ( str -- )
   0 s" dd:dd" SHAPE {: a:^ u:n at:n :}
   a u at [char] : PEEK? if
      a u at 1+ s" dd" SHAPE FRACTION
   else
      a u at
   then WHOLE ;

\ `[-]P`, days, then a clock, one at least.
: DURATION ( str -- )
   0 SIGN [char] P PAST {: a:^ u:n at:n :}
   at u < FITS
   a u at [char] D UNIT {: b:^ bl:n t:n :}
   a u t [char] T PEEK? if a u t CLOCK else a u t then WHOLE ;

: RAW ( ^predicate str -- )
   {: p:^ a:^ u:n :}
   a u p PREDICATE-OUT @ VEC:APPEND ;

: ESCAPED ( ^predicate str -- )
   {: p:^ a:^ u:n :}
   a u p PREDICATE-OUT @ URI:QUERY-ENCODE ;

: QUOTED ( ^predicate str -- )
   {: p:^ a:^ u:n :}
   p s" '" RAW
   u 0 ?do
      a i + c@ [char] ' = if p s" ''" else p a i + 1 then ESCAPED
   loop
   p s" '" RAW ;

\ `tag'value'`.
: TAGGED ( ^predicate str str -- )
   {: p:^ tag:^ tl:n v:^ vl:n :}
   p tag tl RAW p s" '" RAW p v vl ESCAPED p s" '" RAW ;

: V2-TAGGED ( ^predicate str str -- )
   {: p:^ tag:^ tl:n v:^ vl:n :}
   p PREDICATE-V2 @ if p tag tl v vl TAGGED else p v vl ESCAPED then ;

: V2-SUFFIXED ( ^predicate str str -- )
   {: p:^ v:^ vl:n suffix:^ sl:n :}
   p v vl ESCAPED p PREDICATE-V2 @ if p suffix sl RAW then ;

: NIBBLE ( ^predicate n -- )
   {: p:^ k:n :}
   p s" 0123456789ABCDEF" drop k + 1 RAW ;

\ A base64 failure in the value is E-KEY; any other code throws on.
: DECODED-LEN ( str SPAN:span<u8> [ str SPAN:span<u8> -- n ] -- n )
   {: a:^ u:n s dec :}
   [: a u s dec execute ;] catch {: len code:n :}
   code 0= if len exit then
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and if E-KEY throw then
   code throw ;

: HEX ( ^predicate str ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN {: p:^ v:^ vl:n s :}
   v vl s [: BASE64:DECODE ;] DECODED-LEN {: len:n :}
   s SPAN:$ drop {: bytes:^ :}
   p s" X'" RAW
   len 0 ?do
      bytes i + c@ {: b:n :}
      p b 4 rshift NIBBLE p b 15 and NIBBLE
   loop
   p s" '" RAW ;

: URL-CHECK ( str ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN [: BASE64:DECODE-URL ;] DECODED-LEN drop ;

\ Room for what base64 text of N bytes decodes to.
: DECODE-ROOM ( n -- NUM:alloc-byte-len ) 4 / 3 * 3 + MEM:BYTES-ALLOC-LEN ;

: UNPAD ( str -- str ) 2dup + 1- c@ [char] = = if 1- then ;

\ V2 writes the base64 value's bytes as `X'hex'`; V4 writes the base64url
\ value as `binary'value'`, its `=` padding optional.
: BINARY ( ^predicate str -- )
   {: p:^ v:^ vl:n :}
   p PREDICATE-V2 @ if
      p v vl vl DECODE-ROOM [: HEX ;] MEM:WITH-BYTES exit
   then
   v vl vl 3 and 0= vl 0 > and if UNPAD UNPAD then
   dup DECODE-ROOM [: URL-CHECK ;] MEM:WITH-BYTES
   p s" binary" v vl TAGGED ;

: INTEGRAL ( ^predicate str n n -- )
   {: p:^ v:^ vl:n lo:n hi:n :}
   v vl lo hi INTEGER p v vl ESCAPED ;

: INT64 ( ^predicate str -- )
   {: p:^ v:^ vl:n :}
   v vl $8000000000000000 $7FFFFFFFFFFFFFFF INTEGER
   p v vl s" L" V2-SUFFIXED ;

\ V2 writes ISO 8601 UTC tagged, V4 any offset bare.
: DATE-TIME-OFFSET ( ^predicate str -- )
   {: p:^ v:^ vl:n :}
   p PREDICATE-V2 @ if
      v vl UTC p s" datetimeoffset" v vl TAGGED exit
   then
   v vl DATE-OFFSET p v vl ESCAPED ;

\ The index of the first comma at or after AT, the length for none.
: COMMA-END ( str n -- n )
   {: a:^ u:n at:n :}
   at begin
      dup u < if dup a + c@ [char] , <> else false then
   while 1+ repeat ;

: MEMBER-OF? ( ^enum-type str -- bool )
   {: e:^ v:^ vl:n :}
   e ENUM-TYPE-MEMBERS @ begin dup NULL-PTR <> while
      dup ENUM-MEMBER-NAME @ v vl SPAN= if drop true exit then
      ENUM-MEMBER-NEXT @
   repeat drop false ;

\ Member names, each before a comma or the end.
: FLAGS ( ^enum-type str -- )
   {: e:^ a:^ u:n :}
   0 begin {: at:n :}
      a u at COMMA-END {: end:n :}
      e a at + end at - MEMBER-OF? FITS
      end 1+ end u <
   while repeat drop ;

\ `Namespace.Name'value'`, the value one member's name, or a flags type's
\ names joined by `,`.
: ENUM-LITERAL ( ^predicate ^enum-type str -- )
   {: p:^ e:^ v:^ vl:n :}
   e ENUM-TYPE-FLAGS @ if e v vl FLAGS else e v vl MEMBER-OF? FITS then
   p e ENUM-TYPE-NAMESPACE @ SPAN:$ ESCAPED p s" ." RAW
   p e ENUM-TYPE-NAME @ SPAN:$ ESCAPED
   p s" '" RAW p v vl ESCAPED p s" '" RAW ;

\ Each arm checks the value, then writes its literal; V2's datetime
\ literal is the checked UTC text without its `Z`.
: LITERAL ( ^predicate ^key-ref str -- )
   {: p:^ k:^ v:^ vl:n :}
   k KEY-REF-KIND @ MATCH key-kind
      string OF p v vl QUOTED ENDOF
      boolean OF v vl BOOLEAN p v vl ESCAPED ENDOF
      guid OF v vl GUID p s" guid" v vl V2-TAGGED ENDOF
      binary OF p v vl BINARY ENDOF
      byte OF p v vl 0 255 INTEGRAL ENDOF
      sbyte OF p v vl -128 127 INTEGRAL ENDOF
      int16 OF p v vl -32768 32767 INTEGRAL ENDOF
      int32 OF p v vl -2147483648 2147483647 INTEGRAL ENDOF
      int64 OF p v vl INT64 ENDOF
      decimal OF v vl DECIMAL p v vl s" M" V2-SUFFIXED ENDOF
      double OF v vl FLOAT p v vl s" d" V2-SUFFIXED ENDOF
      single OF v vl FLOAT p v vl s" f" V2-SUFFIXED ENDOF
      date-time OF v vl UTC p s" datetime" v vl 1- TAGGED ENDOF
      date-time-offset OF p v vl DATE-TIME-OFFSET ENDOF
      time OF v vl TIME p s" time" v vl TAGGED ENDOF
      date OF v vl DATE p v vl ESCAPED ENDOF
      time-of-day OF v vl TIME-OF-DAY p v vl ESCAPED ENDOF
      duration OF v vl DURATION p s" duration" v vl TAGGED ENDOF
      enumeration OF p k KEY-REF-ENUM-TYPE @ v vl ENUM-LITERAL ENDOF
      undeclared OF E-KEY throw ENDOF
   ;MATCH ;

: SAY-NAME ( ^property -- )
   PROPERTY-NAME$ false REASON:QUOTE ;

: SAY-TYPE ( ^property -- )
   PROPERTY-TYPE$ false REASON:QUOTE ;

\ The value does not fit the property's type.
: VALUE-TEXT ( ^property str -- )
   {: pr:^ v:^ vl:n :}
   s" expected a " REASON:APPEND pr SAY-TYPE
   s"  value for key property " REASON:APPEND pr SAY-NAME
   REASON:FOUND v vl QUOTE
   REASON:FIX s" give " REASON:APPEND pr SAY-NAME
   s"  a value in that type's form" REASON:APPEND ;

\ The property's type is one the document does not declare.
: UNDECLARED-TEXT ( ^property -- )
   {: pr:^ :}
   s" expected " REASON:APPEND pr SAY-NAME
   s"  to have a type the document declares" REASON:APPEND
   REASON:FOUND pr SAY-TYPE
   REASON:FIX s" give METADATA a document that declares " REASON:APPEND
   pr SAY-TYPE ;

: REJECT ( [ -- ] -- )
   REASON:OPEN execute E-KEY REASON:STOP ;

\ The value's literal; a value LITERAL throws E-KEY for is E-KEY with its
\ text, any other code thrown on.
: CHECKED-LITERAL ( ^predicate ^key-ref str -- )
   {: p:^ k:^ v:^ vl:n :}
   [: p k v vl LITERAL ;] catch {: code:n :}
   code E-KEY = if
      k KEY-REF-KIND @ KEY-KIND-UNDECLARED KEY-KIND-EQ if
         [: k KEY-REF-PROPERTY @ UNDECLARED-TEXT ;]
      else
         [: k KEY-REF-PROPERTY @ v vl VALUE-TEXT ;]
      then REJECT
   then
   code 0<> if code throw then ;

\ The separator and the name go before the callback, so E-KEY follows the
\ callback that gave the value with nothing written between.
: KEY-PART ( ^key-ref ^predicate [ ^property -- str ] -- )
   {: k:^ p:^ q :}
   k KEY-REF-PROPERTY @ {: pr:^ :}
   p PREDICATE-WRITTEN @ 0 > if p s" ," RAW then
   p PREDICATE-KEYS @ 1 > if
      p pr PROPERTY-NAME$ ESCAPED p s" =" RAW
   then
   pr q execute {: v:^ vl:n :}
   p k v vl CHECKED-LITERAL
   1 p PREDICATE-WRITTEN +! ;

: WRITE-PREDICATE ( ^entity-type [ ^property -- str ] ^VEC:vector<u8> ^u8 NUM:alloc-byte-len -- )
   drop {: t:^ q v:^ at:^ :}
   at >PREDICATE {: p:^ :}
   v p PREDICATE-OUT !
   t TYPE-V2? p PREDICATE-V2 !
   t [: drop 1 p PREDICATE-KEYS +! ;] KEY-REFS-OF
   p s" (" RAW
   t [: p q KEY-PART ;] KEY-REFS-OF
   p s" )" RAW ;

public

\ Appends to the vector the key predicate of the entity whose key values
\ CALLBACK gives as text.
: KEY ( ^entity-type [ ^property -- str ] ^VEC:vector<u8> -- )
   {: t:^ q v:^ :}
   v VEC:COUNT {: n:n :}
   [: t q v PREDICATE-BYTES MEM:BYTES-ALLOC-LEN
      [: WRITE-PREDICATE ;] MEM:WITH-BYTES ;] catch {: code:n :}
   code 0<> if v n VEC:RESIZE code throw then ;

;package
