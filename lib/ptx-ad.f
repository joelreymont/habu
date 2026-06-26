\ ptx-ad.f - reverse-mode autograd transform v0 (the AD pass core).
\
\ Reverse-mode AD is a SYNTACTIC reversal of a concatenative program: a forward
\ pipeline w1 w2 .. wn has gradient VJP[wn] .. VJP[w1] (docs/autograd.md). This is
\ that pass for STRAIGHT-LINE pipelines over the LINEAR (data-free, mutual-adjoint)
\ primitives: +. <-> DUP, BLOCK-SUM <-> BROADCAST, LOAD <-> STORE. It tokenizes a
\ forward body, reverses the word order, and substitutes each word's adjoint -
\ producing the backward body, which is then an ordinary checked kernel.
\
\ v0 SCOPE (named, dotted boundary): linear primitives only; no cotangent-saving
\ for nonlinear ops (*./EXP./SCALE/B-/BLOCK-MAX), no DUP/fan-out cotangent
\ threading beyond the 1:1 adjoint, no algebraic-simplify, no control flow. Those
\ are the autograd dot chain (habu-ad-reverse-pass, habu-ad-vjp-primitive). Load
\ after lib/errors.f and lib/string.f.

\ --- VJP table: forward word -> adjoint word (linear primitives) ---
: VJP-ADJOINT ( ptr u8 n -- ptr u8 n )
   2dup s" +."        STR= if 2drop s" DUP"       exit then
   2dup s" DUP"       STR= if 2drop s" +."        exit then
   2dup s" BLOCK-SUM" STR= if 2drop s" BROADCAST" exit then
   2dup s" BROADCAST" STR= if 2drop s" BLOCK-SUM" exit then
   2dup s" LOAD"      STR= if 2drop s" STORE"     exit then
   2dup s" STORE"     STR= if 2drop s" LOAD"      exit then
   2dup s" ROW-LOAD"  STR= if 2drop s" ROW-STORE" exit then
   2dup s" ROW-STORE" STR= if 2drop s" ROW-LOAD"  exit then
   2dup s" NEG"       STR= if 2drop s" NEG"       exit then
   E-PTX-NOVJP throw ;

\ --- forward token spans (offset,len into the source body) ---
64 constant AD-MAX-TOK
create AD-TOK-OFF AD-MAX-TOK cells allot
create AD-TOK-LEN AD-MAX-TOK cells allot
variable AD-TOK-N
variable AD-START

: AD-PUSH-TOK ( n n -- ) {: off len :}
   AD-TOK-N @ AD-MAX-TOK < 0= if E-PTX-ADCAP throw then
   off AD-TOK-N @ cells AD-TOK-OFF + !
   len AD-TOK-N @ cells AD-TOK-LEN + !
   AD-TOK-N @ 1+ AD-TOK-N ! ;

\ split the body on spaces, recording each non-empty token span in order.
: AD-TOKENIZE ( ptr u8 n -- ) {: a u :}
   0 AD-TOK-N !  0 AD-START !
   begin
      a u $20 AD-START @ SPLIT-NEXT      ( tokptr toklen nextstart found )
   while                                ( tokptr toklen nextstart )
      AD-START !                        ( tokptr toklen )
      dup 0 > if
         swap a - swap AD-PUSH-TOK      \ ( offset=tokptr-a, len ) recorded
      else
         2drop                          \ skip empty token (consecutive spaces)
      then
   repeat
   2drop drop ;                         \ false case left ( a 0 nextstart )

\ emit VJP of token i, reconstructing its ptr from the base.
: AD-EMIT-TOK ( ptr u8 n -- ) {: a ix :}
   a  ix cells AD-TOK-OFF + @ +         \ token ptr<u8>
   ix cells AD-TOK-LEN + @              \ token len
   VJP-ADJOINT SB-APPEND ;

\ emit all tokens in REVERSE, VJP-substituted, single-space joined.
: AD-EMIT-REV ( ptr u8 -- ) {: a :}
   SB-RESET
   AD-TOK-N @ begin dup 0 > while
      1-
      a over AD-EMIT-TOK
      dup 0 > if $20 SB-APPEND-C then
   repeat drop ;

\ AD-REVERSE: forward body -> backward body (the reverse-mode AD pass v0).
: AD-REVERSE ( ptr u8 n -- ptr u8 n ) {: a u :}
   a u AD-TOKENIZE
   a AD-EMIT-REV
   SB$ ;
