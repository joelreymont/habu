\ base85.f - Adobe Ascii85, the data between `<~` and `~>` without those marks.
\ Five digits `!`..`u` are one 32-bit value written as four bytes, most
\ significant first; `z` between groups is four zero bytes; white space is
\ skipped anywhere; a last group of 2, 3 or 4 digits is filled out with `u` and
\ gives 1, 2 or 3 bytes. DECODE checks the whole input before writing.
\ The span must not overlap the source: one `z` writes four bytes.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/span.f

package BASE85
public

-9840 constant E-FIRST
-9849 constant E-LAST
-9840 s" a byte that is no digit, z or white space" ERROR:CODE E-CHAR
-9841 s" z inside a group" ERROR:CODE E-Z
-9842 s" a last group of one digit" ERROR:CODE E-LENGTH
-9843 s" a group whose value is above 4294967295" ERROR:CODE E-VALUE

private

$21 constant FIRST-DIGIT         \ '!', digit 0
$75 constant LAST-DIGIT          \ 'u', digit 84, which fills out a last group
$7A constant ZERO-CHAR           \ 'z'
85 constant RADIX
5 constant GROUP-DIGITS
4 constant GROUP-BYTES
$FFFFFFFF constant MAX-VALUE
$FF constant OCTET

: B85-DIGIT? ( n -- bool )
   {: c:n :}
   c FIRST-DIGIT >= c LAST-DIGIT <= and ;


\ Space, tab, LF, FF, CR and NUL.
: SPACE? ( n -- bool )
   {: c:n :}
   c $20 = c $09 = or c $0A = or c $0C = or c $0D = or c 0= or ;


: PUT ( n ptr u8 n -- )
   {: v out k:n :}
   k 0 ?do v 24 i 8 * - rshift OCTET and out i + c! loop ;


: GROUP! ( n ptr u8 n bool -- )
   {: v:n out k:n put:bool :}
   v MAX-VALUE > if E-VALUE throw then
   put if v out k PUT then ;


\ One source byte c on the walk's state: len bytes decoded so far, and the
\ value v of the k digits of the open group.
: TAKE ( n n n n ptr u8 bool -- n n n )
   {: len:n v:n k:n c:n out put:bool :}
   c SPACE? if len v k exit then
   c ZERO-CHAR = if
      k 0<> if E-Z throw then
      0 out len + GROUP-BYTES put GROUP!
      len GROUP-BYTES + 0 0 exit
   then
   c B85-DIGIT? 0= if E-CHAR throw then
   v RADIX * c FIRST-DIGIT - + {: w:n :}
   k 1+ GROUP-DIGITS < if len w k 1+ exit then
   w out len + GROUP-BYTES put GROUP!
   len GROUP-BYTES + 0 0 ;


: FINISH ( n n n ptr u8 bool -- n )
   {: len:n v:n k:n out put:bool :}
   k 0= if len exit then
   k 1 = if E-LENGTH throw then
   v GROUP-DIGITS k - 0 ?do RADIX * LAST-DIGIT FIRST-DIGIT - + loop {: w:n :}
   w out len + k 1- put GROUP!
   len k 1- + ;


\ The decoded length; the bytes too at out when put is set.
: WALK ( ptr u8 n ptr u8 bool -- n )
   {: a u:n out put:bool :}
   0 0 0
   u 0 ?do a i + c@ out put TAKE loop
   out put FINISH ;

public

: DECODE ( ptr u8 n SPAN:span<u8> -- n )
   {: a u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ drop {: out :}
   a u out false WALK {: len:n :}
   s SPAN:$ nip len < if E-SPAN-CAPACITY throw then
   a u out true WALK ;

;package
