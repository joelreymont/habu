\ base64.f - RFC 4648 standard base64 and unpadded base64url (section 5's -_).
\ DECODE accepts exactly what ENCODE writes, so every byte string has one
\ encoding: no whitespace or wrapping, padding only at the end of the last group
\ and no bit set below its last byte; the URL decoder refuses either standard
\ symbol. Both decoders check the whole input before writing.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/errors.f
require src/lib/string.f
require src/lib/span.f
require src/lib/adt/option.f

package BASE64
private

$3D constant PAD-CHAR            \ '='
$3F constant SEXTET              \ the six bits one character carries
$FF constant OCTET
$0F constant ONE-BYTE-SPARE      \ "xx==" carries 12 bits for 8: the second character's low 4
$03 constant TWO-BYTE-SPARE      \ "xxx=" carries 18 bits for 16: the third character's low 2

\ RFC 4648 section 4, Table 1: the character for each six-bit value, in order.
: ALPHABET ( -- ptr u8 n )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ;

: URL-ALPHABET ( -- ptr u8 n )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" ;

: SEXTET>CHAR ( n -- n ) {: v:n :}
   ALPHABET drop v + c@ ;

: URL-SEXTET>CHAR ( n -- n ) {: v:n :}
   URL-ALPHABET drop v + c@ ;

\ A character's six bits. Here '=' is padding out of place, and any other byte
\ outside the alphabet is a bad character.
: CHAR>SEXTET ( n -- n ) {: c:n :}
   ALPHABET c INDEX-OF MATCH option
      none OF c PAD-CHAR = if E-BASE64-PAD else E-BASE64-CHAR then throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: URL-CHAR>SEXTET ( n -- n ) {: c:n :}
   URL-ALPHABET c INDEX-OF MATCH option
      none OF c PAD-CHAR = if E-BASE64-PAD else E-BASE64-CHAR then throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: GATHER ( ptr u8 n -- n ) {: p k:n :}
   0 k 0 ?do p i + c@ 16 i 8 * - lshift or loop ;

\ k bytes make k + 1 characters, and '=' fills the group out to four.
: SCATTER ( n ptr u8 n -- ) {: v out k:n :}
   4 0 do
      i k <= if v 18 i 6 * - rshift SEXTET and SEXTET>CHAR else PAD-CHAR then
      out i + c!
   loop ;

: URL-SCATTER ( n ptr u8 n -- ) {: v out k:n :}
   k 1+ 0 ?do
      v 18 i 6 * - rshift SEXTET and URL-SEXTET>CHAR out i + c!
   loop ;

: PAD? ( ptr u8 -- bool )
   c@ PAD-CHAR = ;

\ Check one group of four characters and answer the bytes it decodes to: 3,
\ except that the last group may end "=" for 2 or "==" for 1, with its spare
\ bits zero.
: GROUP ( ptr u8 bool -- n ) {: g last:bool :}
   g c@ CHAR>SEXTET drop
   g 1+ c@ CHAR>SEXTET {: v1:n :}
   last g 3 + PAD? and if
      g 2 + PAD? if
         v1 ONE-BYTE-SPARE and 0<> if E-BASE64-PAD throw then
         1 exit
      then
      g 2 + c@ CHAR>SEXTET TWO-BYTE-SPARE and 0<> if E-BASE64-PAD throw then
      2 exit
   then
   g 3 + c@ CHAR>SEXTET drop
   g 2 + c@ CHAR>SEXTET drop
   3 ;

: CHECKED ( ptr u8 n -- n ) {: a u:n :}
   u 3 and 0<> if E-BASE64-LENGTH throw then
   u 4 / {: groups:n :}
   0 groups 0 ?do a i 4 * + i 1+ groups = GROUP + loop ;

: URL-CHECKED ( ptr u8 n -- ) {: a u:n :}
   u 0 ?do a i + c@ URL-CHAR>SEXTET drop loop
   u 3 and 2 = if
      a u 1- + c@ URL-CHAR>SEXTET ONE-BYTE-SPARE and
      0<> if E-BASE64-PAD throw then
   then
   u 3 and 3 = if
      a u 1- + c@ URL-CHAR>SEXTET TWO-BYTE-SPARE and
      0<> if E-BASE64-PAD throw then
   then ;

: BITS ( ptr u8 n -- n ) {: g k:n :}
   0 k 1+ 0 ?do g i + c@ CHAR>SEXTET 18 i 6 * - lshift or loop ;

: URL-BITS ( ptr u8 n -- n ) {: g k:n :}
   0 k 1+ 0 ?do g i + c@ URL-CHAR>SEXTET 18 i 6 * - lshift or loop ;

: PUT ( n ptr u8 n -- ) {: v out k:n :}
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

: ENCODE ( ptr u8 n SPAN:span<u8> -- n ) {: a u:n s :}
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

: DECODE ( ptr u8 n SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   \ Each group of four decodes to three bytes and the last to at least one, so
   \ a span too small for that is refused before the input is read.
   u 4 / 3 * 2 - s SPAN:$ nip > if E-SPAN-CAPACITY throw then
   a u CHECKED {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out :}
   u 4 / 0 ?do
      len i 3 * - 3 min {: k:n :}
      a i 4 * + k BITS out i 3 * + k PUT
   loop
   len ;

: ENCODE-URL ( ptr u8 n SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ nip u swap URL-ENCODE-LEN {: len:n :}
   s SPAN:$ drop {: out :}
   u 2 + 3 / 0 ?do
      u i 3 * - 3 min {: k:n :}
      a i 3 * + k GATHER out i 4 * + k URL-SCATTER
   loop
   len ;

: DECODE-URL ( ptr u8 n SPAN:span<u8> -- n ) {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   u 3 and 1 = if E-BASE64-LENGTH throw then
   u 4 / 3 * u 3 and dup 0<> if 1- then + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out :}
   a u URL-CHECKED
   u 4 / u 3 and 0<> if 1+ then 0 ?do
      len i 3 * - 3 min {: k:n :}
      a i 4 * + k URL-BITS out i 3 * + k PUT
   loop
   len ;

;package
