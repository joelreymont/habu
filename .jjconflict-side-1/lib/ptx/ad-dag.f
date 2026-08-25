\ ad-dag.f - reverse-mode autodiff over the concatenative tile/collective IR.
\
\ The real reverse pass (not the v0 string reversal in lib/ptx/ad.f). It symbolic-
\ executes a forward op sequence into a value-numbered dataflow DAG (DUP = fan-out:
\ the same node id pushed twice; binary ops record both input node ids), then runs
\ reverse-mode AD: each node's accumulated cotangent applies the node's VJP to its
\ inputs, ACCUMULATED with +. at every fan-out (the tape replacement). It EMITS a
\ self-contained backward by RECOMPUTING the forward (row-local, so saved primals/
\ outputs are recomputed registers) then the cotangent ops, driving the EMIT-*
\ helpers (lib/ptx/cg.f, lib/ptx/header.f, lib/ptx/cg-collective.f). Load after
\ those. Checked Habu.

\ forward op codes the DAG understands (the softmax-rows primitive set)
0 constant OP-LEAF      \ the input tile (node 0)
1 constant OP-BMAX      \ BLOCK-MAX  ( a -- )
2 constant OP-BSUB      \ B-         ( a b -- )  a - b
3 constant OP-EXP       \ EXP.       ( a -- )
4 constant OP-BSUM      \ BLOCK-SUM  ( a -- )
5 constant OP-BDIV      \ B/         ( a b -- )  a / b
6 constant OP-DUP       \ fan-out
7 constant OP-MUL       \ *.  ( a b -- )  a * b   (row * row, elementwise via EMIT-MUL)
8 constant OP-ADD       \ +.  ( a b -- )  a + b   (row + row, elementwise via EMIT-ADD)

32 constant AD-MAXN
create AD-OP   AD-MAXN cells allot      \ node op code
create AD-A    AD-MAXN cells allot      \ input node id 0  (-1 = none)
create AD-B    AD-MAXN cells allot      \ input node id 1  (-1 = none)
create AD-REG  AD-MAXN cells allot      \ forward result register
create AD-CT   AD-MAXN cells allot      \ accumulated cotangent register (-1 = unset)
variable AD-N                           \ node count
variable AD-OUT                         \ output node id

create AD-VS   AD-MAXN cells allot      \ symbolic value stack (node ids)
variable AD-VSP

: AD-SLOT-CHECK ( n -- )
   dup 0 < if E-PTX-AD-UNKNOWN throw then
   dup AD-MAXN >= if E-PTX-AD-OVERFLOW throw then
   drop ;

: AD-ID-CHECK ( n -- )
   dup AD-SLOT-CHECK
   dup AD-N @ >= if E-PTX-AD-UNKNOWN throw then
   drop ;

: AD-REF-CHECK ( n -- )
   dup -1 = if drop exit then
   AD-ID-CHECK ;

: AD-SLOT-CELL ( ptr a n -- ptr a ) {: base:ptr idx:n :}
   idx AD-SLOT-CHECK
   base idx cells + ;

: AD-ID-CELL ( ptr a n -- ptr a ) {: base:ptr id:n :}
   id AD-ID-CHECK
   base id cells + ;

: AD-VROOM ( -- )
   AD-VSP @ AD-MAXN >= if E-PTX-AD-OVERFLOW throw then ;

: AD-VNEED ( -- )
   AD-VSP @ 0 <= if E-PTX-AD-UNDERFLOW throw then ;

: AD-VONE ( -- )
   AD-VSP @ 1 <> if E-PTX-AD-OUTPUT throw then ;

: AD-VPUSH ( n -- ) {: id:n :}
   id AD-ID-CHECK
   AD-VROOM
   id AD-VS AD-VSP @ AD-SLOT-CELL !
   AD-VSP @ 1+ AD-VSP ! ;

: AD-VPOP  ( -- n )
   AD-VNEED
   AD-VSP @ 1- {: idx:n :}
   idx AD-VSP !
   AD-VS idx AD-SLOT-CELL @ ;

: AD-VTOP  ( -- n )
   AD-VNEED
   AD-VSP @ 1- {: idx:n :}
   AD-VS idx AD-SLOT-CELL @ ;

: AD-OP@  ( n -- n )  AD-OP swap AD-ID-CELL @ ;
: AD-A@   ( n -- n )  AD-A swap AD-ID-CELL @ ;
: AD-B@   ( n -- n )  AD-B swap AD-ID-CELL @ ;
: AD-REG@ ( n -- n )  AD-REG swap AD-ID-CELL @ ;
: AD-REG! ( n n -- )  {: reg:n id:n :} reg AD-REG id AD-ID-CELL ! ;
: AD-CT@  ( n -- n )  AD-CT swap AD-ID-CELL @ ;
: AD-CT!  ( n n -- )  {: ct:n id:n :} ct AD-CT id AD-ID-CELL ! ;

: AD-NODE-OP? ( n -- bool )
   case
      OP-LEAF of 0 0= endof
      OP-BMAX of 0 0= endof
      OP-BSUB of 0 0= endof
      OP-EXP  of 0 0= endof
      OP-BSUM of 0 0= endof
      OP-BDIV of 0 0= endof
      OP-MUL  of 0 0= endof
      OP-ADD  of 0 0= endof
      0 0= 0= swap
   endcase ;

: AD-NODE-OP-CHECK ( n -- )
   dup AD-NODE-OP? 0= if E-PTX-AD-UNKNOWN throw then
   drop ;

: AD-NROOM ( -- )
   AD-N @ AD-MAXN >= if E-PTX-AD-OVERFLOW throw then ;

\ create a node (op a b), return its id
: AD-NODE ( n n n -- n ) {: op:n a:n b:n :}
   op AD-NODE-OP-CHECK
   a AD-REF-CHECK
   b AD-REF-CHECK
   AD-NROOM
   AD-N @ {: id:n :}
   op AD-OP id AD-SLOT-CELL !
   a AD-A id AD-SLOT-CELL !
   b AD-B id AD-SLOT-CELL !
   -1 AD-CT id AD-SLOT-CELL !
   id 1+ AD-N !  id ;

\ --- DAG builder: symbolic-execute a forward op-code list (node 0 = input) ---
: AD-DO-DUP ( -- )
   AD-VTOP AD-VPUSH ;

: AD-DO-UNARY ( n -- ) {: op:n :}
   op AD-VPOP -1 AD-NODE AD-VPUSH ;

: AD-DO-BINARY ( n -- ) {: op:n :}
   AD-VPOP {: b:n :}
   AD-VPOP {: a:n :}
   op a b AD-NODE AD-VPUSH ;

: AD-DO-OP ( n -- )
   case
      OP-DUP  of AD-DO-DUP endof
      OP-BMAX of OP-BMAX AD-DO-UNARY endof
      OP-EXP  of OP-EXP  AD-DO-UNARY endof
      OP-BSUM of OP-BSUM AD-DO-UNARY endof
      OP-BSUB of OP-BSUB AD-DO-BINARY endof
      OP-BDIV of OP-BDIV AD-DO-BINARY endof
      OP-MUL  of OP-MUL  AD-DO-BINARY endof
      OP-ADD  of OP-ADD  AD-DO-BINARY endof
      drop E-PTX-AD-UNKNOWN throw
   endcase ;

: AD-LEN-CHECK ( n -- )
   dup 0 < if E-PTX-AD-UNKNOWN throw then
   drop ;

: AD-BUILD ( ptr a n -- ) {: ops:ptr len:n :}
   len AD-LEN-CHECK
   0 AD-N !  0 AD-VSP !
   OP-LEAF -1 -1 AD-NODE AD-VPUSH              \ node 0 = the input tile
   len 0 ?do  ops i cells + @ AD-DO-OP  loop
   AD-VONE
   AD-VPOP AD-OUT ! ;

\ --- forward recompute emit: fill AD-REG for every node (node 0 reg given) ---
: AD-EMIT-NODE ( n -- ) {: id :}
   id AD-OP@
   case
      OP-BMAX of id AD-A@ AD-REG@ EMIT-BLOCK-MAX              id AD-REG! endof
      OP-EXP  of id AD-A@ AD-REG@ EMIT-EXP                    id AD-REG! endof
      OP-BSUM of id AD-A@ AD-REG@ EMIT-BLOCK-SUM              id AD-REG! endof
      OP-BSUB of id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-B-    id AD-REG! endof
      OP-BDIV of id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-B/    id AD-REG! endof
      OP-MUL  of id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-MUL   id AD-REG! endof
      OP-ADD  of id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-ADD   id AD-REG! endof
      drop E-PTX-AD-UNKNOWN throw
   endcase ;

: AD-EMIT-FWD ( n -- )                      \ node 0 reg = xreg; recompute the rest
   0 AD-REG!
   AD-N @ 1 ?do  i AD-EMIT-NODE  loop ;

\ accumulate cotangent ctreg into node id (fan-out -> +.)
: AD-ACC ( n n -- ) {: id ctreg :}
   id AD-CT@ -1 = if  ctreg id AD-CT!
   else  id AD-CT@ ctreg EMIT-ADD  id AD-CT!  then ;

\ --- reverse VJP per op (node id has its output cotangent in AD-CT) ---
\ BLOCK-MAX: dx = BLOCK-MAX-SELECT(ct, x, mx)
: AD-VJP-BMAX ( n -- ) {: id :}
   id AD-CT@  id AD-A@ AD-REG@  id AD-REG@  EMIT-BLOCK-MAX-SELECT  id AD-A@ swap AD-ACC ;
\ PTX:B- (a - b): da = ct ; db = -Sum(ct)
: AD-VJP-BSUB ( n -- ) {: id :}
   id AD-A@  id AD-CT@  AD-ACC
   id AD-CT@ EMIT-BLOCK-SUM EMIT-NEG  id AD-B@ swap AD-ACC ;
\ EXP (e = exp(a)): da = ct * e
: AD-VJP-EXP ( n -- ) {: id :}
   id AD-CT@  id AD-REG@  EMIT-MUL  id AD-A@ swap AD-ACC ;
\ BLOCK-SUM: da = BROADCAST(ct)
: AD-VJP-BSUM ( n -- ) {: id :}
   id AD-CT@ EMIT-BROADCAST  id AD-A@ swap AD-ACC ;
\ PTX:B/ (y = a/b): da = ct/b ; db = -Sum(ct*y)/b
: AD-VJP-BDIV ( n -- ) {: id :}
   id AD-CT@  id AD-B@ AD-REG@  EMIT-B/  id AD-A@ swap AD-ACC
   id AD-CT@ id AD-REG@ EMIT-MUL EMIT-BLOCK-SUM  id AD-B@ AD-REG@ EMIT-B/ EMIT-NEG
   id AD-B@ swap AD-ACC ;

\ *. (y = a ⊙ b, both rows): da = ct ⊙ b ; db = ct ⊙ a  (row×row, no Sum: both
\ inputs are tiles, so each cotangent stays a per-lane row, unlike B-'s uniform b)
: AD-VJP-MUL ( n -- ) {: id:n :}
   id AD-CT@  id AD-B@ AD-REG@  EMIT-MUL  id AD-A@ swap AD-ACC
   id AD-CT@  id AD-A@ AD-REG@  EMIT-MUL  id AD-B@ swap AD-ACC ;
\ +. (y = a + b, both rows): da = ct ; db = ct  (cotangent splits unchanged)
: AD-VJP-ADD ( n -- ) {: id:n :}
   id AD-A@  id AD-CT@  AD-ACC
   id AD-B@  id AD-CT@  AD-ACC ;

: AD-VJP ( n -- ) {: id :}
   id AD-OP@
   case
      OP-BMAX of id AD-VJP-BMAX endof
      OP-BSUB of id AD-VJP-BSUB endof
      OP-EXP  of id AD-VJP-EXP  endof
      OP-BSUM of id AD-VJP-BSUM endof
      OP-BDIV of id AD-VJP-BDIV endof
      OP-MUL  of id AD-VJP-MUL  endof
      OP-ADD  of id AD-VJP-ADD  endof
      drop E-PTX-AD-UNKNOWN throw
   endcase ;

\ reverse pass: seed dy on the output, propagate to node 0 -> dx
: AD-DAG-EMIT-REV ( n -- n )
   AD-OUT @ AD-CT!                             \ seed the output cotangent
   AD-N @ 1 ?do  AD-N @ i -  AD-VJP  loop      \ nodes high..1 (leaf 0 has no VJP)
   0 AD-CT@ ;                                  \ dx = input node's accumulated cotangent

\ --- top: build + emit the backward compute (x,dy registers -> dx register) ---
: AD-EMIT-BWD ( ptr a n n n -- n ) {: ops len xreg dyreg :}
   ops len AD-BUILD
   xreg AD-EMIT-FWD
   dyreg AD-DAG-EMIT-REV ;
