\ base64.f - RFC 4648 base64: the standard alphabet, with padding.
\
\ ENCODE writes four characters for every three bytes and pads the last group
\ with '='. DECODE accepts exactly what ENCODE writes and refuses the rest: a
\ byte outside the alphabet is E-BASE64-CHAR; padding anywhere but the end of
\ the last group, or a bit left set below its last byte, is E-BASE64-PAD; a
\ length that is not a multiple of four is E-BASE64-LENGTH. Every byte string
\ therefore has exactly one encoding. There is no whitespace, no line wrapping
\ and no URL-safe alphabet here.
\
\ Both words write into the caller's span and answer the count they wrote. A
\ span too small for the result throws E-SPAN-CAPACITY and a negative input
\ length E-SPAN-LENGTH. DECODE checks the whole input before it writes, so no
\ refusal leaves a byte behind.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/string.f
require lib/span.f
require lib/adt/option.f

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

: SEXTET>CHAR ( n -- n ) {: v:n :}
   ALPHABET drop v + c@ ;

\ A character's six bits. Here '=' is padding out of place, and any other byte
\ outside the alphabet is a bad character.
: CHAR>SEXTET ( n -- n ) {: c:n :}
   ALPHABET c INDEX-OF MATCH option
      none OF c PAD-CHAR = if E-BASE64-PAD else E-BASE64-CHAR then throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ The value of a group's k bytes, high byte first, the missing ones zero.
: GATHER ( ptr u8 n -- n ) {: p k:n :}
   0 k 0 ?do p i + c@ 16 i 8 * - lshift or loop ;

\ k bytes make k + 1 characters, and '=' fills the group out to four.
: SCATTER ( n ptr u8 n -- ) {: v out k:n :}
   4 0 do
      i k <= if v 18 i 6 * - rshift SEXTET and SEXTET>CHAR else PAD-CHAR then
      out i + c!
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

\ Check every group, and answer the length the input decodes to.
: CHECKED ( ptr u8 n -- n ) {: a u:n :}
   u 3 and 0<> if E-BASE64-LENGTH throw then
   u 4 / {: groups:n :}
   0 groups 0 ?do a i 4 * + i 1+ groups = GROUP + loop ;

\ The value a checked group carries in its k + 1 characters, high bits first.
: BITS ( ptr u8 n -- n ) {: g k:n :}
   0 k 1+ 0 ?do g i + c@ CHAR>SEXTET 18 i 6 * - lshift or loop ;

: PUT ( n ptr u8 n -- ) {: v out k:n :}
   k 0 ?do v 16 i 8 * - rshift OCTET and out i + c! loop ;

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
   a u CHECKED {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out :}
   u 4 / 0 ?do
      len i 3 * - 3 min {: k:n :}
      a i 4 * + k BITS out i 3 * + k PUT
   loop
   len ;

;package
