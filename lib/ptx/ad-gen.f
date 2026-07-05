\ ad-gen.f - lower a GENERATED straight-line body to PTX kernel compute.
\
\ The reverse pass (lib/ptx/ad.f AD-BACKWARD$) produces a backward BODY as a
\ token string over the vjp.f table vocabulary. This file lowers such a body -
\ forward or generated backward - to emitted PTX inside a kernel scaffold: each
\ token drives its EMIT-* helper over an emit-time register stack.
\
\ Saved-value resolution (habu-ad-thread-saved): ADG-LOWER-BWD RECOMPUTES the
\ forward slice row-locally before lowering the backward, recording each
\ saves-op's SAVED-X/Y/Z/MX/S/A registers; SAVED-* tokens then resolve to those
\ registers and ZERO. lowers to a fresh zero tile. Recompute is the policy for
\ row-local kernels (AD-RECOMPUTE?: register recompute beats a global
\ stash+reload round trip; the cost model is habu-ad-save-vs). v0 contract,
\ fail-closed: one load + one final store/scatter per body, at most ONE
\ saves-op per forward (multi-save threading is the DAG's domain), unbound
\ SAVED-* tokens E-PTX-NOIMPL, unknown tokens E-PTX-AD-UNKNOWN, unbalanced
\ bodies E-PTX-AD-OUTPUT. Load after lib/errors.f, lib/string.f, lib/ptx/ad.f
\ (tokenizer), and lib/ptx/cg-collective.f.

require lib/errors.f
require lib/string.f
require src/arch/ptx/vjp.f
require lib/ptx/ad.f

16 constant ADG-MAX

create ADG-VS ADG-MAX cells allot
variable ADG-VSP
variable ADG-IN-SPAN    \ span reg for the single load
variable ADG-OUT-SPAN   \ span reg for the single final store
variable ADG-CTX        \ row ctx reg
variable ADG-LOADED     \ load-seen count
variable ADG-STORED     \ store-seen count
variable ADG-REC        \ recording mode: bind SAVED-* registers
variable ADG-DISC       \ discard mode: the final store emits nothing (recompute)
variable ADG-SV#        \ saves-op scan counter

variable ADG-SV-X
variable ADG-SV-Y
variable ADG-SV-Z
variable ADG-SV-MX
variable ADG-SV-S
variable ADG-SV-A

: ADG-SV-RESET ( -- )
   -1 ADG-SV-X !  -1 ADG-SV-Y !  -1 ADG-SV-Z !
   -1 ADG-SV-MX !  -1 ADG-SV-S !  -1 ADG-SV-A ! ;

: ADG-RESET ( n n n -- ) {: insp:n outsp:n ctx:n :}
   0 ADG-VSP !
   insp ADG-IN-SPAN !
   outsp ADG-OUT-SPAN !
   ctx ADG-CTX !
   0 ADG-LOADED !
   0 ADG-STORED ! ;

: ADG-PUSH ( n -- ) {: reg:n :}
   ADG-VSP @ ADG-MAX >= if E-PTX-AD-OVERFLOW throw then
   reg ADG-VS ADG-VSP @ cells + !
   ADG-VSP @ 1+ ADG-VSP ! ;

: ADG-POP ( -- n )
   ADG-VSP @ 0 <= if E-PTX-AD-UNDERFLOW throw then
   ADG-VSP @ 1- ADG-VSP !
   ADG-VS ADG-VSP @ cells + @ ;

: ADG-LOAD-TOK ( -- )   \ the single data load: input span at the shared ctx
   ADG-LOADED @ 0 > if E-PTX-NOIMPL throw then
   1 ADG-LOADED !
   ADG-IN-SPAN @ ADG-CTX @ EMIT-ROW-LOAD ADG-PUSH ;

: ADG-STORE-TOK ( n -- ) {: scatter:n :}   \ the single final store or scatter-add
   ADG-STORED @ 0 > if E-PTX-NOIMPL throw then
   1 ADG-STORED !
   ADG-DISC @ 0 <> if ADG-POP drop exit then      \ recompute: value ends in a register
   ADG-POP ADG-OUT-SPAN @ ADG-CTX @
   scatter 0 > if EMIT-ROW-SCATTER-ADD else EMIT-ROW-STORE then ;

\ ---- per-op emit + saved-register recording ------------------------------------

: ADG-EXP-OP ( n -- ) {: in:n :}
   in EMIT-EXP {: out:n :}
   ADG-REC @ 0 <> if in ADG-SV-X ! out ADG-SV-Y ! then
   out ADG-PUSH ;

: ADG-BMAX-OP ( n -- ) {: in:n :}
   in EMIT-BLOCK-MAX {: out:n :}
   ADG-REC @ 0 <> if in ADG-SV-X ! out ADG-SV-MX ! then
   out ADG-PUSH ;

: ADG-BDIV-OP ( n n -- ) {: t:n s:n :}
   t s EMIT-B/ {: out:n :}
   ADG-REC @ 0 <> if s ADG-SV-S ! out ADG-SV-Z ! then
   out ADG-PUSH ;

: ADG-MUL-OP ( n n -- ) {: x:n y:n :}
   ADG-REC @ 0 <> if x ADG-SV-X ! y ADG-SV-Y ! then
   x y EMIT-MUL ADG-PUSH ;

: ADG-SCALE-OP ( n n -- ) {: t:n a:n :}
   ADG-REC @ 0 <> if a ADG-SV-A ! t ADG-SV-X ! then
   t a EMIT-SCALE ADG-PUSH ;

: ADG-BINARY ( ptr u8 n -- bool )   \ tile x tile / tile x uniform binaries
   2dup s" +." STR= if 2drop ADG-POP ADG-POP swap EMIT-ADD ADG-PUSH 0 0= exit then
   2dup s" *." STR= if 2drop ADG-POP ADG-POP swap ADG-MUL-OP 0 0= exit then
   2dup s" SCALE" STR= if 2drop ADG-POP ADG-POP swap ADG-SCALE-OP 0 0= exit then
   2dup s" PTX:B-" STR= if 2drop ADG-POP ADG-POP swap EMIT-B- ADG-PUSH 0 0= exit then
   2dup s" PTX:B/" STR= if 2drop ADG-POP ADG-POP swap ADG-BDIV-OP 0 0= exit then
   2drop 0 0= 0= ;

: ADG-UNARY ( ptr u8 n -- bool )
   2dup s" DUP" STR= if 2drop ADG-POP dup ADG-PUSH ADG-PUSH 0 0= exit then
   2dup s" NEG" STR= if 2drop ADG-POP EMIT-NEG ADG-PUSH 0 0= exit then
   2dup s" BLOCK-SUM" STR= if 2drop ADG-POP EMIT-BLOCK-SUM ADG-PUSH 0 0= exit then
   2dup s" BLOCK-MAX" STR= if 2drop ADG-POP ADG-BMAX-OP 0 0= exit then
   2dup s" BROADCAST" STR= if 2drop ADG-POP EMIT-BROADCAST ADG-PUSH 0 0= exit then
   2dup s" EXP." STR= if 2drop ADG-POP ADG-EXP-OP 0 0= exit then
   2drop 0 0= 0= ;

: ADG-IO ( ptr u8 n -- bool )
   2dup s" ROW-LOAD" STR= if 2drop ADG-LOAD-TOK 0 0= exit then
   2dup s" ROW-STORE" STR= if 2drop 0 ADG-STORE-TOK 0 0= exit then
   2dup s" ROW-SCATTER-ADD" STR= if 2drop 1 ADG-STORE-TOK 0 0= exit then
   2drop 0 0= 0= ;

\ ---- saved-value token resolution ------------------------------------------------

: ADG-SAVED? ( ptr u8 n -- bool )
   s" SAVED-" STARTS-WITH? ;

: ADG-SAVED-CELL ( ptr u8 n -- n )   \ bound register, -1 when unbound/unknown
   2dup s" SAVED-X" STR= if 2drop ADG-SV-X @ exit then
   2dup s" SAVED-Y" STR= if 2drop ADG-SV-Y @ exit then
   2dup s" SAVED-Z" STR= if 2drop ADG-SV-Z @ exit then
   2dup s" SAVED-MX" STR= if 2drop ADG-SV-MX @ exit then
   2dup s" SAVED-S" STR= if 2drop ADG-SV-S @ exit then
   2dup s" SAVED-A" STR= if 2drop ADG-SV-A @ exit then
   2drop -1 ;

: ADG-SAVED-REG ( ptr u8 n -- n )   \ resolve or fail closed (no binding recorded)
   ADG-SAVED-CELL
   dup 0 < if E-PTX-NOIMPL throw then ;

: ADG-TOK ( ptr u8 n -- )
   2dup ADG-SAVED? if ADG-SAVED-REG ADG-PUSH exit then
   2dup s" ZERO." STR= if 2drop EMIT-ZERO ADG-PUSH exit then
   2dup ADG-IO if 2drop exit then
   2dup ADG-UNARY if 2drop exit then
   2dup ADG-BINARY if 2drop exit then
   2drop E-PTX-AD-UNKNOWN throw ;

: ADG-FINISH-CHECK ( -- )   \ the body must end balanced with its store done
   ADG-STORED @ 1 <> if E-PTX-AD-OUTPUT throw then
   ADG-VSP @ 0 <> if E-PTX-AD-OUTPUT throw then ;

: ADG-CORE ( ptr u8 n n n n n n -- )
   {: a:ptr u:n insp:n outsp:n ctx:n rec:n disc:n :}
   insp outsp ctx ADG-RESET
   rec ADG-REC !
   disc ADG-DISC !
   a u AD-TOKENIZE
   AD-TOK-N @ 0 ?do
      a i TOK-STR ADG-TOK
   loop
   ADG-FINISH-CHECK ;

\ Lower one generated body between prepared spans. The caller scaffolds the
\ kernel (entry/params/row/ctx) and passes the span+ctx registers. Saved
\ bindings are cleared: a standalone body may not consume another lowering's
\ recompute registers.
: ADG-LOWER ( ptr u8 n n n n -- ) {: a:ptr u:n insp:n outsp:n ctx:n :}
   ADG-SV-RESET
   a u insp outsp ctx 0 0 ADG-CORE ;

\ ---- the backward lowering with recompute-resolved saved values -------------------

: ADG-TOK-SAVES ( ptr u8 n -- n )   \ 1 when the token is a registered saves-op
   VJP-FIND dup 0 < if drop 0 exit then
   VJP-SAVE@ 0 > if 1 else 0 then ;

: ADG-SAVES-OP# ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u AD-TOKENIZE
   0 ADG-SV# !
   AD-TOK-N @ 0 ?do
      a i TOK-STR ADG-TOK-SAVES ADG-SV# @ + ADG-SV# !
   loop
   ADG-SV# @ ;

: ADG-RECOMPUTE ( ptr u8 n n n -- ) {: a:ptr u:n insp:n ctx:n :}
   a u insp 0 ctx 1 1 ADG-CORE ;

\ ---- the SAVE-path binding: reload the saved value from the primal span ---------

: ADG-SAVES-TOK ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ the slice's saves-op token
   a u AD-TOKENIZE
   AD-TOK-N @ 0 ?do
      a i TOK-STR ADG-TOK-SAVES 0 > if
         a i TOK-STR unloop exit
      then
   loop
   E-PTX-NOIMPL throw ;

: ADG-SAVE-CELL! ( ptr u8 n n -- ) {: a:ptr u:n reg:n :}   \ bind the op's saved cell
   a u s" EXP." STR= if reg ADG-SV-Y ! exit then
   E-PTX-NOIMPL throw ;                        \ only single-buffer saves lower via save

: ADG-SAVE-BIND ( ptr u8 n n n -- ) {: fa:ptr fu:n insp:n ctx:n :}
   fa fu ADG-SAVES-TOK {: ta:ptr tu:n :}
   ta tu VJP-SAVES# 1 <> if E-PTX-NOIMPL throw then
   insp ctx EMIT-ROW-LOAD {: v:n :}
   ta tu v ADG-SAVE-CELL! ;

\ Lower a GENERATED backward. The save-vs-recompute CHOICE comes from the
\ policy (AD-SAVE? - the cost model, or the explicit override; materialized? =
\ false: a generated backward receives the primal, not a stored intermediate):
\ RECOMPUTE re-runs the forward slice from the primal span binding SAVED-*;
\ SAVE reloads the saved value from the primal span instead (the kernel's
\ input-1 is then the saved/materialized value - single-buffer saves only,
\ others fail closed). The backward body runs through the core directly so the
\ bindings stay visible to its SAVED-* tokens.
: ADG-LOWER-BWD ( ptr u8 n ptr u8 n n n n n -- )
   {: fa:ptr fu:n ba:ptr bu:n xsp:n dzsp:n outsp:n ctx:n :}
   ADG-SV-RESET
   fa fu ADG-SAVES-OP# {: nsv:n :}
   nsv 1 > if E-PTX-NOIMPL throw then
   nsv 1 = if
      fa fu 0 0= 0= AD-SAVE? if
         fa fu xsp ctx ADG-SAVE-BIND
      else
         fa fu xsp ctx ADG-RECOMPUTE
      then
   then
   ba bu dzsp outsp ctx 0 0 ADG-CORE ;
