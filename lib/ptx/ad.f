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

\ VJP-EXPAND: like VJP-ADJOINT but emits the multi-word adjoint EXPANSION for the
\ NONLINEAR unary ops, referencing saved primals/outputs by name (SAVED-*). These
\ are the single-cotangent nonlinear adjoints (docs/autograd.md): EXP.'s backward
\ is dz (.) y (y = saved output); BLOCK-MAX's backward scatters dz to the arg-max
\ lane using the saved x and max. Binary nonlinear ops (*./B-/B//SCALE) have
\ MULTI-output adjoints needing the data-flow/cotangent-threading model (the deeper
\ reverse-pass layer, dot habu-ad-reverse-pass) - they still route through
\ VJP-ADJOINT and fail closed until that lands.
: VJP-EXPAND ( ptr u8 n -- ptr u8 n )
   2dup s" EXP."      STR= if 2drop s" SAVED-Y *."                       exit then
   2dup s" BLOCK-MAX" STR= if 2drop s" SAVED-X SAVED-MX BLOCK-MAX-SELECT" exit then
   \ binary nonlinear ops: 2-output adjoints, the cotangents threaded by the
   \ stack juggling (DUP/SWAP) inside the expansion (docs/autograd.md table):
   2dup s" *." STR= if 2drop s" DUP SAVED-Y *. SWAP SAVED-X *." exit then  \ dx=dz.y, dy=dz.x
   2dup s" B-" STR= if 2drop s" DUP BLOCK-SUM NEG"              exit then  \ dt=dz, ds=-Sum(dz)
   \ B/ : z = x/s. dx = dz/s ; ds = -Sum(dz*z)/s (z = saved output, s = saved uniform).
   2dup s" B/" STR= if 2drop s" DUP SAVED-S B/ SWAP SAVED-Z *. BLOCK-SUM NEG SAVED-S U/" exit then
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
\ finite and known at compile time.
: VJP-SAVES ( ptr u8 n -- n )
   2dup s" EXP."      STR= if 2drop 1 exit then   \ saves output y
   2dup s" SCALE"     STR= if 2drop 2 exit then   \ saves a, x
   2dup s" *."        STR= if 2drop 2 exit then   \ saves x, y
   2dup s" B/"        STR= if 2drop 2 exit then   \ saves s, z
   2dup s" BLOCK-MAX" STR= if 2drop 2 exit then   \ saves x, m (arg-max select)
   2dup s" +."        STR= if 2drop 0 exit then
   2dup s" -."        STR= if 2drop 0 exit then
   2dup s" DUP"       STR= if 2drop 0 exit then
   2dup s" BLOCK-SUM" STR= if 2drop 0 exit then
   2dup s" BROADCAST" STR= if 2drop 0 exit then
   2dup s" LOAD"      STR= if 2drop 0 exit then
   2dup s" STORE"     STR= if 2drop 0 exit then
   2dup s" ROW-LOAD"  STR= if 2drop 0 exit then
   2dup s" ROW-STORE" STR= if 2drop 0 exit then
   2dup s" NEG"       STR= if 2drop 0 exit then
   E-PTX-NOVJP throw ;

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
