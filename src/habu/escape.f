\ escape.f - decoding an escaped string literal's span, package ESCAPE.
\
\ An escaped literal (`s\"`, `c\"`, `.\"`) runs from its first payload byte to
\ a closing double quote. A backslash makes the next byte an escape letter
\ (habu2.f C-ESC-DECODE-BASIC); `\x` and `\X` take two hex digits. SPAN
\ finds the closing quote and counts the bytes the span decodes to, and says
\ whether every escape in it is whole; COPY writes those bytes. The
\ interpreter's literals (src/habu/outer.f ESC-SCAN) read a literal through
\ these two words. Neither allocates, refuses or bounds a length: the caller
\ decides what a failed span or a long one means.

require src/lib/prelude.f

package ESCAPE
private

$22 constant QUOTE               \ a double quote closes the literal
$5C constant BACKSLASH           \ starts an escape
$0A constant NEWLINE


: BETWEEN? ( n n n -- bool ) {: c:n lo:n hi:n :}
   c lo >= c hi <= and ;


\ The value of byte c as a hex digit, and whether it is one.
: HEX ( n -- n bool ) {: c:n :}
   c [char] 0 [char] 9 BETWEEN? if c [char] 0 - true exit then
   c [char] a [char] f BETWEEN? if c [char] a - 10 + true exit then
   c [char] A [char] F BETWEEN? if c [char] A - 10 + true exit then
   0 false ;


\ The byte an escape letter stands for, and whether it is one. `x` and `X`
\ are not: HEX-STEP reads their two digits.
: LETTER ( n -- n bool )
   case
      QUOTE of QUOTE true endof
      [char] q of QUOTE true endof
      BACKSLASH of BACKSLASH true endof
      [char] a of 7 true endof
      [char] b of 8 true endof
      [char] e of 27 true endof
      [char] f of 12 true endof
      [char] l of NEWLINE true endof
      [char] n of NEWLINE true endof
      [char] r of 13 true endof
      [char] t of 9 true endof
      [char] v of 11 true endof
      [char] z of 0 true endof
      0 false rot
   endcase ;


\ A backslash at p, `x` and two hex digits before e.
: HEX-STEP ( ptr u8 ptr u8 -- n ptr u8 bool ) {: p:ptr e:ptr :}
   p 4 + e > if 0 p false exit then
   p 2 + c@ HEX {: hi:n hi-ok:bool :}
   p 3 + c@ HEX {: lo:n lo-ok:bool :}
   hi 4 lshift lo or  p 4 +  hi-ok lo-ok and ;


\ The byte the text at p stands for, where its spelling ends, and whether that
\ spelling is whole before e.
: STEP ( ptr u8 ptr u8 -- n ptr u8 bool ) {: p:ptr e:ptr :}
   p c@ BACKSLASH <> if p c@ p 1 + true exit then
   p 1 + e >= if 0 p false exit then
   p 1 + c@ {: c:n :}
   c [char] x = c [char] X = or if p e HEX-STEP exit then
   c LETTER p 2 + swap ;

public

\ The text from s on, up to e: its closing quote q, the count of bytes s..q
\ decodes to, and whether a quote closes it with every escape whole. When the
\ flag is false, q and the count mean nothing.
: SPAN ( ptr u8 ptr u8 -- ptr u8 n bool ) {: s:ptr e:ptr :}
   s 0
   begin
      over e >= if false exit then
      over c@ QUOTE <>
   while
      {: p:ptr k:n :}
      p e STEP {: nx:ptr ok:bool :} drop
      ok 0= if p k false exit then
      nx k 1 +
   repeat
   true ;


\ The bytes the text from s to its quote q decodes to, written from d on. SPAN
\ must have answered true for s and q.
: COPY ( ptr u8 ptr u8 ptr u8 -- ) {: s:ptr q:ptr d:ptr :}
   s d
   begin over q < while
      {: p:ptr t:ptr :}
      p q STEP drop {: b:n nx:ptr :}
      b t c!
      nx t 1 +
   repeat
   2drop ;

;package
