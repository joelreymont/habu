split-str
\ escape.f - decoding an escaped string literal's span, package ESCAPE.
\
\ A literal (`s\"`, `c\"`, `.\"`) runs to a closing double quote; LETTER
\ decodes an escape letter, and `\x` and `\X` take two hex digits. These words
\ neither allocate, reject nor bound a length: the caller decides what a failed
\ or long span means. The file is a boot row ahead of the prelude, so it calls
\ primitives only.

package ESCAPE
private

$22 constant QUOTE
$5C constant BACKSLASH
$0A constant NEWLINE


: BETWEEN? ( n n n -- bool ) {: c:n lo:n hi:n :}
   c lo >= c hi <= and ;


: HEX ( n -- n bool ) {: c:n :}
   c [char] 0 [char] 9 BETWEEN? if c [char] 0 - 0 0= exit then
   c [char] a [char] f BETWEEN? if c [char] a - 10 + 0 0= exit then
   c [char] A [char] F BETWEEN? if c [char] A - 10 + 0 0= exit then
   0 0 0= 0= ;


\ `x` and `X` answer false: HEX-STEP reads their two digits.
: LETTER ( n -- n bool ) {: c:n :}
   c QUOTE = if QUOTE 0 0= exit then
   c [char] q = if QUOTE 0 0= exit then
   c BACKSLASH = if BACKSLASH 0 0= exit then
   c [char] a = if 7 0 0= exit then
   c [char] b = if 8 0 0= exit then
   c [char] e = if 27 0 0= exit then
   c [char] f = if 12 0 0= exit then
   c [char] l = if NEWLINE 0 0= exit then
   c [char] n = if NEWLINE 0 0= exit then
   c [char] r = if 13 0 0= exit then
   c [char] t = if 9 0 0= exit then
   c [char] v = if 11 0 0= exit then
   c [char] z = if 0 0 0= exit then
   0 0 0= 0= ;


\ A backslash at p, `x` and two hex digits before e.
: HEX-STEP ( ^u8 ^u8 -- n ^u8 bool ) {: p:^ e:^ :}
   p 4 + e > if 0 p 0 0= 0= exit then
   p 2 + c@ HEX {: hi:n hi-ok:bool :}
   p 3 + c@ HEX {: lo:n lo-ok:bool :}
   hi 4 lshift lo or  p 4 +  hi-ok lo-ok and ;


\ The byte the text at p stands for, where its spelling ends, and whether that
\ spelling is whole before e.
: STEP ( ^u8 ^u8 -- n ^u8 bool ) {: p:^ e:^ :}
   p c@ BACKSLASH <> if p c@ p 1 + 0 0= exit then
   p 1 + e >= if 0 p 0 0= 0= exit then
   p 1 + c@ {: c:n :}
   c [char] x = c [char] X = or if p e HEX-STEP exit then
   c LETTER p 2 + swap ;

public

\ The text from s on, up to e: its closing quote q, the count of bytes s..q
\ decodes to, and whether a quote closes it with every escape whole. When the
\ flag is false, q and the count mean nothing.
: SPAN ( ^u8 ^u8 -- ^u8 n bool ) {: s:^ e:^ :}
   s 0
   begin
      over e >= if 0 0= 0= exit then
      over c@ QUOTE <>
   while
      {: p:^ k:n :}
      p e STEP {: nx:^ ok:bool :} drop
      ok 0= if p k 0 0= 0= exit then
      nx k 1 +
   repeat
   0 0= ;


\ SPAN must have answered true for s and q.
: COPY ( ^u8 ^u8 ^u8 -- ) {: s:^ q:^ d:^ :}
   s d
   begin over q < while
      {: p:^ t:^ :}
      p q STEP drop {: b:n nx:^ :}
      b t c!
      nx t 1 +
   repeat
   2drop ;


\ The bytes the u bytes at src decode to, written at dst, which holds u bytes: a
\ decoded text is never longer than its spelling. Answers the count written and
\ whether every escape was whole; a bad one stops the decode with the bytes
\ before it written.
: DECODE ( ^u8 n ^u8 -- n bool ) {: src:^ u:n dst:^ :}
   src u + {: e:^ :}
   src 0
   begin over e < while
      {: p:^ k:n :}
      p e STEP {: b:n nx:^ ok:bool :}
      ok 0= if k 0 0= 0= exit then
      b dst k + c!
      nx k 1 +
   repeat
   nip 0 0= ;

;package
