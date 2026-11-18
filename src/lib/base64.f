split-str
\ base64.f - RFC 4648 standard base64 and unpadded base64url (section 5's -_).
\ DECODE accepts exactly what ENCODE writes, so every byte string has one
\ encoding: no whitespace or wrapping, padding only at the end of the last group
\ and no bit set below its last byte; the URL decoder rejects either standard
\ symbol. Both decoders check the whole input before writing.
\ A rejection's code carries the text `L:C: expected X, found Y; fix`, L:C the
\ rejected byte's line and column in the input, or its end's for E-BASE64-LENGTH.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/reason.f
require src/core/rejection.f

package BASE64
private

$3D constant PAD-CHAR            \ '='
$3F constant SEXTET              \ the six bits one character carries
$FF constant OCTET
$0F constant ONE-BYTE-SPARE      \ "xx==" carries 12 bits for 8: the second character's low 4
$03 constant TWO-BYTE-SPARE      \ "xxx=" carries 18 bits for 16: the third character's low 2
26 constant LOWER-FIRST          \ the value of 'a' in Table 1
52 constant DIGIT-FIRST          \ the value of '0' in Table 1
62 constant SYMBOL-FIRST         \ '+', or Table 2's '-'; '/' or '_' is next

\ RFC 4648 section 4, Table 1: the character for each six-bit value, in order.
: ALPHABET ( -- ^u8 n )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ;

: URL-ALPHABET ( -- str )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" ;

: SEXTET>CHAR ( n -- n ) {: v:n :}
   ALPHABET drop v + c@ ;

: URL-SEXTET>CHAR ( n -- n ) {: v:n :}
   URL-ALPHABET drop v + c@ ;


\ A character's six bits by its range in Table 1, or as the symbol sym62 or
\ sym63; -1 for any other byte.
: TABLE-SEXTET ( n n n -- n )
   {: c:n sym62:n sym63:n :}
   c [char] A >= c [char] Z <= and if c [char] A - exit then
   c [char] a >= c [char] z <= and if c [char] a - LOWER-FIRST + exit then
   c [char] 0 >= c [char] 9 <= and if c [char] 0 - DIGIT-FIRST + exit then
   c sym62 = if SYMBOL-FIRST exit then
   c sym63 = if SYMBOL-FIRST 1+ exit then
   -1 ;


: CHAR>SEXTET ( n -- n )  [char] + [char] / TABLE-SEXTET ;
: URL-CHAR>SEXTET ( n -- n )  [char] - [char] _ TABLE-SEXTET ;


: LINE-SEP$ ( -- str ) s" :" ;
: EXPECTED-SEP$ ( -- str ) s" : expected " ;
: FOUND-SEP$ ( -- str ) s" , found " ;
: FIX-SEP$ ( -- str ) s" ; " ;
: END$ ( -- str ) s" the end of the input" ;
: TICK$ ( -- str ) s" `" ;
: BYTE$ ( -- str ) s" byte $" ;

: PUT$ ( ^u8 str -- ^u8 ) {: at:^ a:^ u:n :} a at u BYTE-COPY  at u + ;

: GRAPHIC? ( n -- bool ) {: c:n :} c $21 >= c $7E <= and ;

: PUT-FOUND ( ^u8 str n -- ^u8 ) {: at:^ a:^ u:n off:n :}
   off u = if at END$ PUT$ exit then
   a off + c@ {: c:n :}
   c GRAPHIC? if at TICK$ PUT$ a off + 1 PUT$ TICK$ PUT$ exit then
   at BYTE$ PUT$ c 2 FMT:HEX-WIDTH$ PUT$ ;

: FOUND-LEN ( str n -- n ) {: a:^ u:n off:n :}
   off u = if END$ nip exit then
   a off + c@ {: c:n :}
   c GRAPHIC? if TICK$ nip 2 * 1+ exit then
   BYTE$ nip c 2 FMT:HEX-WIDTH$ nip + ;

\ The bytes REJECT-SCOPE writes, which is all the room its buffer has.
: TEXT-SIZE ( n n n n n -- n ) {: line:n col:n flen:n eu:n fu:n :}
   line FMT:INT$ nip col FMT:INT$ nip + LINE-SEP$ nip +
   EXPECTED-SEP$ nip + FOUND-SEP$ nip + FIX-SEP$ nip + eu + fu + flen + ;

: REJECT-SCOPE ( n n str n str str n ^u8 NUM:alloc-byte-len -- )
   drop {: line:n col:n a:^ u:n off:n ea:^ eu:n fa:^ fu:n code:n buf:^ :}
   buf line FMT:INT$ PUT$ LINE-SEP$ PUT$ col FMT:INT$ PUT$
   EXPECTED-SEP$ PUT$ ea eu PUT$ FOUND-SEP$ PUT$
   a u off PUT-FOUND FIX-SEP$ PUT$ fa fu PUT$
   buf - {: tu:n :}
   buf tu code REASON:THROW ;

\ Throws code with the header's text, ea as X and fa as the fix, placed at off
\ in the input a u; the last throw only shows the checker REJECT never returns.
: REJECT ( str n str str n -- )
   {: a:^ u:n off:n ea:^ eu:n fa:^ fu:n code:n :}
   a a off + 1 1 0 REJECTION:POSITION drop {: line:n col:n :}
   line col a u off ea eu fa fu code
   line col a u off FOUND-LEN eu fu TEXT-SIZE MEM:BYTES-ALLOC-LEN
   [: REJECT-SCOPE ;] MEM:WITH-BYTES
   code throw ;

: FIX$ ( bool -- str )
   if s" write the data as BASE64:ENCODE-URL writes it" exit then
   s" write the data as BASE64:ENCODE writes it" ;

: EXPECTED$ ( bool -- str )
   if s" a character of A-Z, a-z, 0-9, - or _" exit then
   s" a character of A-Z, a-z, 0-9, + or /" ;

: REJECT-CHAR ( str n bool -- ) {: a:^ u:n off:n url:bool :}
   a u off url EXPECTED$ url FIX$ E-BASE64-CHAR REJECT ;

: REJECT-PAD ( str n bool -- ) {: a:^ u:n off:n url:bool :}
   a u off url EXPECTED$
   url if s" leave `=` out: DECODE-URL takes no padding"
   else s" use `=` only to end the last group" then
   E-BASE64-PAD REJECT ;

: REJECT-SPARE ( str n bool -- ) {: a:^ u:n off:n url:bool :}
   a u off s" a character whose unused low bits are zero"
   url FIX$ E-BASE64-PAD REJECT ;

: REJECT-LENGTH ( str bool -- ) {: a:^ u:n url:bool :}
   a u u
   url if s" a length that leaves 0, 2 or 3 characters in the last group of four"
   else s" a length that is a multiple of four" then
   url FIX$ E-BASE64-LENGTH REJECT ;

\ The six bits of the byte at off in the input a u, by Table 2 when url; '=' is
\ padding out of place, and any other byte outside the table a bad character.
: CHECKED-SEXTET ( str n bool -- n ) {: a:^ u:n off:n url:bool :}
   a off + c@ {: c:n :}
   url if c URL-CHAR>SEXTET else c CHAR>SEXTET then {: v:n :}
   v 0< if
      c PAD-CHAR = if a u off url REJECT-PAD else a u off url REJECT-CHAR then
   then
   v ;

: GATHER ( ^u8 n -- n ) {: p k:n :}
   0 k 0 ?do p i + c@ 16 i 8 * - lshift or loop ;

\ k bytes make k + 1 characters, and '=' fills the group out to four.
: SCATTER ( n ^u8 n -- ) {: v out k:n :}
   4 0 do
      i k <= if v 18 i 6 * - rshift SEXTET and SEXTET>CHAR else PAD-CHAR then
      out i + c!
   loop ;

: URL-SCATTER ( n ^u8 n -- ) {: v out k:n :}
   k 1+ 0 ?do
      v 18 i 6 * - rshift SEXTET and URL-SEXTET>CHAR out i + c!
   loop ;

: PAD? ( ^u8 -- bool )
   c@ PAD-CHAR = ;

\ The bytes the group at g in the input a u decodes to, once checked: 3, or for
\ the last group 2 when it ends "=" and 1 when "==", its spare bits zero.
: GROUP ( str n bool -- n ) {: a:^ u:n g:n last:bool :}
   a u g false CHECKED-SEXTET drop
   a u g 1+ false CHECKED-SEXTET {: v1:n :}
   last a g + 3 + PAD? and if
      a g + 2 + PAD? if
         v1 ONE-BYTE-SPARE and 0<> if a u g 1+ false REJECT-SPARE then
         1 exit
      then
      a u g 2 + false CHECKED-SEXTET TWO-BYTE-SPARE and
      0<> if a u g 2 + false REJECT-SPARE then
      2 exit
   then
   a u g 3 + false CHECKED-SEXTET drop
   a u g 2 + false CHECKED-SEXTET drop
   3 ;

: CHECKED ( ^u8 n -- n ) {: a u:n :}
   u 3 and 0<> if a u false REJECT-LENGTH then
   u 4 / {: groups:n :}
   0 groups 0 ?do a u i 4 * i 1+ groups = GROUP + loop ;

: URL-CHECKED ( str -- ) {: a u:n :}
   u 0 ?do a u i true CHECKED-SEXTET drop loop
   u 3 and 2 = if
      a u 1- + c@ URL-CHAR>SEXTET ONE-BYTE-SPARE and
      0<> if a u u 1- true REJECT-SPARE then
   then
   u 3 and 3 = if
      a u 1- + c@ URL-CHAR>SEXTET TWO-BYTE-SPARE and
      0<> if a u u 1- true REJECT-SPARE then
   then ;

: BITS ( ^u8 n -- n ) {: g k:n :}
   0 k 1+ 0 ?do g i + c@ CHAR>SEXTET 18 i 6 * - lshift or loop ;

: URL-BITS ( str -- n ) {: g k:n :}
   0 k 1+ 0 ?do g i + c@ URL-CHAR>SEXTET 18 i 6 * - lshift or loop ;

: PUT ( n ^u8 n -- ) {: v out k:n :}
   k 0 ?do v 16 i 8 * - rshift OCTET and out i + c! loop ;

\ Check the number of complete groups before multiplying by four, then the
\ final 2/3/4 characters against the remaining capacity.
: URL-ENCODE-LEN ( n n -- n ) {: u:n cap:n :}
   u 3 / cap 4 / > if E-SPAN-CAPACITY throw then
   u 3 / 4 * {: full:n :}
   u 3 mod {: tail:n :}
   tail 0<> if tail 1+ else 0 then {: extra:n :}
   extra cap full - > if E-SPAN-CAPACITY throw then
   full extra + ;

public

\ A group is gathered whole before its characters are scattered, so one group
\ may be encoded in place.
: ENCODE ( ^u8 n SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   \ The input is compared with what the span holds, three bytes to four of
   \ reach: scaled up first, a length near the maximum cell wraps the encoded
   \ size negative and every span looks large enough.
   s SPAN:$ 4 / 3 * u < if E-SPAN-CAPACITY throw then {: out :}
   u 2 + 3 / {: groups:n :}
   groups 4 * {: len:n :}
   groups 0 ?do
      u i 3 * - 3 min {: k:n :}
      a i 3 * + k GATHER out i 4 * + k SCATTER
   loop
   len ;

: DECODE ( ^u8 n SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   \ Each group of four decodes to three bytes and the last to at least one, so
   \ a span too small for that is rejected before the input is read.
   u 4 / 3 * 2 - s SPAN:$ nip > if E-SPAN-CAPACITY throw then
   a u CHECKED {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out :}
   u 4 / 0 ?do
      len i 3 * - 3 min {: k:n :}
      a i 4 * + k BITS out i 3 * + k PUT
   loop
   len ;

: ENCODE-URL ( str SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ nip u swap URL-ENCODE-LEN {: len:n :}
   s SPAN:$ drop {: out :}
   u 2 + 3 / 0 ?do
      u i 3 * - 3 min {: k:n :}
      a i 3 * + k GATHER out i 4 * + k URL-SCATTER
   loop
   len ;

: DECODE-URL ( str SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   u 3 and 1 = if a u true REJECT-LENGTH then
   u 4 / 3 * u 3 and dup 0<> if 1- then + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out :}
   a u URL-CHECKED
   u 4 / u 3 and 0<> if 1+ then 0 ?do
      len i 3 * - 3 min {: k:n :}
      a i 4 * + k URL-BITS out i 3 * + k PUT
   loop
   len ;

;package
