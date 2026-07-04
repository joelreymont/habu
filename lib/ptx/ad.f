\ ptx-ad.f - reverse-mode autograd transform v0 (the AD pass core).
\
\ Reverse-mode AD is a SYNTACTIC reversal of a concatenative program: a forward
\ pipeline w1 w2 .. wn has gradient VJP[wn] .. VJP[w1] (docs/autograd.md). This is
\ that pass for STRAIGHT-LINE pipelines over the LINEAR/data-free adjoints:
\ +. <-> DUP, BLOCK-SUM <-> BROADCAST, STORE -> LOAD, LOAD -> SCATTER-ADD
\ by default, and LOAD-ONCE -> STORE-ONCE when the checked once-space witness is
\ present.
\ It tokenizes a forward body, reverses the word order, and substitutes each
\ word's adjoint - producing the backward body, which is then an ordinary checked
\ kernel.
\
\ v0 SCOPE (named, dotted boundary): straight-line pipelines; the VJP table
\ itself lives in src/arch/ptx/vjp.f (habu-ad-vjp-primitive) and this pass
\ substitutes its expansions; full cotangent DAG threading is
\ habu-ad-reverse-pass. Load after lib/errors.f, lib/string.f, and
\ src/arch/ptx/vjp.f.

require src/arch/ptx/vjp.f

\ --- control-flow boundary: v0 reverses straight-line dataflow only ---
: AD-CONTROL? ( ptr u8 n -- bool )
   2dup s" if" STR=CI if 2drop 0 0= exit then
   2dup s" else" STR=CI if 2drop 0 0= exit then
   2dup s" then" STR=CI if 2drop 0 0= exit then
   2dup s" begin" STR=CI if 2drop 0 0= exit then
   2dup s" while" STR=CI if 2drop 0 0= exit then
   2dup s" repeat" STR=CI if 2drop 0 0= exit then
   2dup s" until" STR=CI if 2drop 0 0= exit then
   2dup s" again" STR=CI if 2drop 0 0= exit then
   2dup s" do" STR=CI if 2drop 0 0= exit then
   2dup s" ?do" STR=CI if 2drop 0 0= exit then
   2dup s" loop" STR=CI if 2drop 0 0= exit then
   2dup s" +loop" STR=CI if 2drop 0 0= exit then
   2dup s" leave" STR=CI if 2drop 0 0= exit then
   2dup s" unloop" STR=CI if 2drop 0 0= exit then
   2dup s" exit" STR=CI if 2drop 0 0= exit then
   2dup s" recurse" STR=CI if 2drop 0 0= exit then
   2dup s" case" STR=CI if 2drop 0 0= exit then
   2dup s" of" STR=CI if 2drop 0 0= exit then
   2dup s" endof" STR=CI if 2drop 0 0= exit then
   2dup s" endcase" STR=CI if 2drop 0 0= exit then
   2drop 0 0= 0= ;

: AD-REQUIRE-STRAIGHT ( ptr u8 n -- )
   AD-CONTROL? if E-PTX-AD-CONTROL throw then ;

\ VJP-ADJOINT / VJP-EXPAND: forward word -> adjoint expansion, from the
\ src/arch/ptx/vjp.f table. One lookup serves both the 1:1 linear adjoints
\ (single-token expansions) and the nonlinear multi-token expansions that
\ reference saved primals/outputs by name (SAVED-*); VJP-ADJOINT remains the
\ historical spelling. Missing entries fail closed (E-PTX-NOVJP).
: VJP-ADJOINT ( ptr u8 n -- ptr u8 n )
   2dup AD-REQUIRE-STRAIGHT
   VJP-ADJOINT$ ;

: VJP-EXPAND ( ptr u8 n -- ptr u8 n )
   VJP-ADJOINT ;

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
   VJP-EXPAND SB-APPEND ;

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

\ --- save-vs-recompute: how many forward values an op's backward must save ---
\ Linear (data-free) adjoints save 0; nonlinear ones consume saved primals/outputs
\ (docs/autograd.md "Full VJP table" saves column). This is the tape's replacement,
\ finite and known at compile time; the counts live in the vjp.f table entries.
: VJP-SAVES ( ptr u8 n -- n )
   VJP-SAVES# ;

: VJP-NONLINEAR? ( ptr u8 n -- bool )  VJP-SAVES 0 > ;

\ save-vs-recompute decision (docs/autograd.md "Checkpointing"): recompute the
\ forward slice when that is cheaper than the global save+reload round-trip.
: AD-RECOMPUTE? ( n n -- bool ) {: save-cost recompute-cost :}
   recompute-cost save-cost < ;

\ --- algebraic-simplify (peephole): cancel adjacent NEG NEG (double negation) ---
\ Token i as a string, reconstructed from the recorded span and the base ptr.
: TOK-STR ( ptr u8 n -- ptr u8 n ) {: ix :}
   ix cells AD-TOK-OFF + @  +          \ base(on stack) + offset -> token ptr
   ix cells AD-TOK-LEN + @ ;           \ token len

: TOK-IS-NEG? ( ptr u8 n -- bool )  TOK-STR s" NEG" STR= ;

\ separator: a space before a token unless the builder is still empty
: SB-SEP ( -- )  SB$ nip 0 > if $20 SB-APPEND-C then ;

\ AD-SIMPLIFY: drop adjacent NEG NEG pairs from a body, preserving the rest.
: AD-SIMPLIFY ( ptr u8 n -- ptr u8 n ) {: a u :}
   a u AD-TOKENIZE
   SB-RESET
   0 begin dup AD-TOK-N @ < while             ( ix )
      dup 1+ AD-TOK-N @ <                      \ has a next token?
      over a swap TOK-IS-NEG? and              \ tok[ix] = NEG?
      over 1+ a swap TOK-IS-NEG? and           \ tok[ix+1] = NEG?
      if  2 +                                  \ cancel the pair
      else  SB-SEP  dup a swap TOK-STR SB-APPEND  1+  then
   repeat drop
   SB$ ;
