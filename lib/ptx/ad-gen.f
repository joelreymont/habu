\ ad-gen.f - lower a GENERATED straight-line body to PTX kernel compute.
\
\ The reverse pass (lib/ptx/ad.f AD-BACKWARD$) produces a backward BODY as a
\ token string over the vjp.f table vocabulary. This file lowers such a body -
\ forward or generated backward - to emitted PTX inside a kernel scaffold: each
\ token drives its EMIT-* helper over an emit-time register stack. v0 lowering
\ contract, fail-closed: exactly ONE load token (the kernel's data input span)
\ and ONE final store/scatter token (the output span); saved-value tokens and
\ multi-span bodies throw E-PTX-NOIMPL until the save/threading lowering lands
\ (habu-ad-thread-saved); unknown tokens throw E-PTX-AD-UNKNOWN; stack over/
\ underflow throws the AD capacity codes. Load after lib/errors.f,
\ lib/string.f, lib/ptx/ad.f (tokenizer), and lib/ptx/cg-collective.f.

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
   ADG-POP ADG-OUT-SPAN @ ADG-CTX @
   scatter 0 > if EMIT-ROW-SCATTER-ADD else EMIT-ROW-STORE then ;

: ADG-BINARY ( ptr u8 n -- bool )   \ tile x tile / tile x uniform binaries
   2dup s" +." STR= if 2drop ADG-POP ADG-POP swap EMIT-ADD ADG-PUSH 0 0= exit then
   2dup s" PTX:B-" STR= if 2drop ADG-POP ADG-POP swap EMIT-B- ADG-PUSH 0 0= exit then
   2dup s" PTX:B/" STR= if 2drop ADG-POP ADG-POP swap EMIT-B/ ADG-PUSH 0 0= exit then
   2drop 0 0= 0= ;

: ADG-UNARY ( ptr u8 n -- bool )
   2dup s" DUP" STR= if 2drop ADG-POP dup ADG-PUSH ADG-PUSH 0 0= exit then
   2dup s" NEG" STR= if 2drop ADG-POP EMIT-NEG ADG-PUSH 0 0= exit then
   2dup s" BLOCK-SUM" STR= if 2drop ADG-POP EMIT-BLOCK-SUM ADG-PUSH 0 0= exit then
   2dup s" BLOCK-MAX" STR= if 2drop ADG-POP EMIT-BLOCK-MAX ADG-PUSH 0 0= exit then
   2dup s" BROADCAST" STR= if 2drop ADG-POP EMIT-BROADCAST ADG-PUSH 0 0= exit then
   2dup s" EXP." STR= if 2drop ADG-POP EMIT-EXP ADG-PUSH 0 0= exit then
   2drop 0 0= 0= ;

: ADG-IO ( ptr u8 n -- bool )
   2dup s" ROW-LOAD" STR= if 2drop ADG-LOAD-TOK 0 0= exit then
   2dup s" ROW-STORE" STR= if 2drop 0 ADG-STORE-TOK 0 0= exit then
   2dup s" ROW-SCATTER-ADD" STR= if 2drop 1 ADG-STORE-TOK 0 0= exit then
   2drop 0 0= 0= ;

: ADG-SAVED? ( ptr u8 n -- bool )   \ saved-value tokens: not lowerable yet
   s" SAVED-" STARTS-WITH? ;

: ADG-TOK ( ptr u8 n -- )
   2dup ADG-SAVED? if 2drop E-PTX-NOIMPL throw then
   2dup ADG-IO if 2drop exit then
   2dup ADG-UNARY if 2drop exit then
   2dup ADG-BINARY if 2drop exit then
   2drop E-PTX-AD-UNKNOWN throw ;

: ADG-FINISH-CHECK ( -- )   \ the body must end balanced with its store done
   ADG-STORED @ 1 <> if E-PTX-AD-OUTPUT throw then
   ADG-VSP @ 0 <> if E-PTX-AD-OUTPUT throw then ;

\ Lower one generated body between prepared spans. The caller scaffolds the
\ kernel (entry/params/row/ctx) and passes the span+ctx registers.
: ADG-LOWER ( ptr u8 n n n n -- ) {: a:ptr u:n insp:n outsp:n ctx:n :}
   insp outsp ctx ADG-RESET
   a u AD-TOKENIZE
   AD-TOK-N @ 0 ?do
      a i TOK-STR ADG-TOK
   loop
   ADG-FINISH-CHECK ;
