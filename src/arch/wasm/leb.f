\ leb.f - WLEB, the Wasm backend's LEB128 (docs/wasm-backend.md §10.1, §17.1).
\
\ An LEB holds seven bits of its integer a byte, low group first, with the high
\ bit set on every byte but the last. An N-bit integer takes at most ceil(N/7)
\ bytes, 5 for 32 bits and 10 for 64. An LEB still marked for more at that last
\ byte is overlong, and the last byte's bits past N must be zero, or copies of
\ the sign for a signed integer, else the value is out of range.
\
\ The writers write the shortest LEB at the start of a caller's span and answer
\ the bytes written. A padded writer always writes the most bytes, 5 for a u32
\ and 10 for an s64, so the field can be rewritten in place later without
\ moving a byte: a call field is the five bytes after a `call` or
\ `call_indirect` opcode, and an address field the ten after `i64.const`. A
\ value outside its type is E-RANGE and a span short of the LEB E-SPAN-CAPACITY,
\ both before a byte is written. The readers take any LEB Wasm admits, padded
\ ones included, and answer the value and the bytes it took. A padded reader
\ answers the field at an offset and refuses one that is not a padded LEB of
\ its width (E-UNPADDED), and a patch rewrites only such a field.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/span.f

package WLEB
public

\ WLEB's codes open the Wasm back end's block -9800..-9829, which the back end
\ mints in its own files under src/arch/wasm/; -9804 is reserved.
-9800 constant E-WLEB-FIRST
-9804 constant E-WLEB-LAST
-9800 constant E-OVERLONG    \ an LEB still marked for more at its type's last byte
-9801 constant E-TRUNCATED   \ the bytes end inside an LEB
-9802 constant E-RANGE       \ a value outside its type, to write or read
-9803 constant E-UNPADDED    \ a field that is not a padded LEB of its width

private

$7F constant GROUP-BITS               \ the seven value bits of a byte
$80 constant MORE-BIT                 \ set on every byte of an LEB but its last
$40 constant SIGN-BIT                 \ a signed LEB's last byte's sign
$FE00000000000000 constant SIGN-FILL  \ the seven bits ASR7 copies a sign into
$FFFFFFFF constant U32-MAX
$7FFFFFFF constant S32-MAX

: U32? ( n -- bool )
   {: v:n :}
   v 0 >= v U32-MAX <= and ;

: S32? ( n -- bool )
   {: v:n :}
   v S32-MAX invert >= v S32-MAX <= and ;

\ the bytes in the widest LEB of a bits-bit integer, ceil(bits/7)
: WIDEST ( n -- n )
   6 + 7 / ;

\ v shifted right one group, its sign copied in
: ASR7 ( n -- n )
   {: v:n :}
   v 7 rshift v 0< if SIGN-FILL or then ;

\ The bytes in the shortest LEB of v: unsigned, while bits remain above the
\ low group; signed, until the rest fits one signed group, -64..63.
: ULEN ( n -- n )
   1 swap
   begin dup GROUP-BITS invert and 0<> while
      7 rshift swap 1+ swap
   repeat drop ;

: SLEN ( n -- n )
   1 swap
   begin dup -64 < over 63 > or while
      ASR7 swap 1+ swap
   repeat drop ;

\ Write v's low len groups at the start of the span and answer len; a signed v
\ shifts its sign in. A span short of len is refused before a byte is written.
: PUT ( n bool n SPAN:span<u8> -- n )
   {: v:n signed:bool len:n s :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: p :}
   v len 0 ?do
      dup GROUP-BITS and i 1+ len < if MORE-BIT or then p i + c!
      signed if ASR7 else 7 rshift then
   loop drop
   len ;

\ The bytes the LEB at p takes, through its first byte with the more bit clear.
: EXTENT ( ptr u8 n n -- n )
   {: p avail:n most:n :}
   0 begin
      dup most = if E-OVERLONG throw then
      dup avail = if E-TRUNCATED throw then
      p over + c@ MORE-BIT and 0<>
   while 1+ repeat
   1+ ;

\ Whether b, the last byte an N-bit LEB may have, keeps within N, given the
\ rest of N it holds: the bits above that are zero, or copies of the sign.
: FITS? ( n n bool -- bool )
   {: b:n rest:n signed:bool :}
   signed if
      b rest 1- rshift dup 0= swap GROUP-BITS rest 1- rshift = or
   else
      b rest rshift 0=
   then ;

\ The LEB of a bits-bit integer at the start of the avail bytes at p: its value
\ and the bytes it takes.
: DECODE ( ptr u8 n n bool -- n n )
   {: p avail:n bits:n signed:bool :}
   avail 0 < if E-SPAN-LENGTH throw then
   bits WIDEST {: most:n :}
   p avail most EXTENT {: len:n :}
   p len + 1- c@ {: last:n :}
   len most = if
      last bits most 1- 7 * - signed FITS? 0= if E-RANGE throw then
   then
   0 len 0 ?do p i + c@ GROUP-BITS and i 7 * lshift or loop
   signed last SIGN-BIT and 0<> and len 7 * 64 < and if
      -1 len 7 * lshift or
   then
   len ;

\ The padded LEB of a bits-bit integer at offset at of the avail bytes at p.
: FIELD ( ptr u8 n n n bool -- n )
   {: p avail:n at:n bits:n signed:bool :}
   at 0 < at avail > or if E-SPAN-RANGE throw then
   p at + avail at - bits signed DECODE
   bits WIDEST <> if E-UNPADDED throw then ;

public

\ ---- the shortest LEB ----------------------------------------------------
: U32! ( u32 SPAN:span<u8> -- n )
   {: v:n s :}
   v U32? 0= if E-RANGE throw then
   v false v ULEN s PUT ;

: S32! ( n SPAN:span<u8> -- n )
   {: v:n s :}
   v S32? 0= if E-RANGE throw then
   v true v SLEN s PUT ;

: S64! ( n SPAN:span<u8> -- n )
   {: v:n s :}
   v true v SLEN s PUT ;

: U32@ ( ptr u8 n -- u32 n )
   32 false DECODE ;

: S32@ ( ptr u8 n -- n n )
   32 true DECODE ;

: S64@ ( ptr u8 n -- n n )
   64 true DECODE ;

\ ---- padded fields ------------------------------------------------------
: U32-PAD! ( u32 SPAN:span<u8> -- n )
   {: v:n s :}
   v U32? 0= if E-RANGE throw then
   v false 32 WIDEST s PUT ;

: S64-PAD! ( n SPAN:span<u8> -- n )
   {: v:n s :}
   v true 64 WIDEST s PUT ;

: U32-PAD@ ( ptr u8 n n -- u32 )
   32 false FIELD ;

: S64-PAD@ ( ptr u8 n n -- n )
   64 true FIELD ;

: U32-PATCH ( u32 SPAN:span<u8> n -- )
   {: v:n s at:n :}
   s SPAN:$ at U32-PAD@ drop
   v s at SPAN:SKIP U32-PAD! drop ;

: S64-PATCH ( n SPAN:span<u8> n -- )
   {: v:n s at:n :}
   s SPAN:$ at S64-PAD@ drop
   v s at SPAN:SKIP S64-PAD! drop ;

;package
