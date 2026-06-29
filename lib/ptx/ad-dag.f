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

: AD-VPUSH ( n -- )  AD-VS AD-VSP @ cells + !  AD-VSP @ 1+ AD-VSP ! ;
: AD-VPOP  ( -- n )  AD-VSP @ 1- AD-VSP !  AD-VS AD-VSP @ cells + @ ;
: AD-VTOP  ( -- n )  AD-VS AD-VSP @ 1- cells + @ ;

: AD-OP@  ( n -- n )  cells AD-OP + @ ;
: AD-A@   ( n -- n )  cells AD-A + @ ;
: AD-B@   ( n -- n )  cells AD-B + @ ;
: AD-REG@ ( n -- n )  cells AD-REG + @ ;
: AD-REG! ( n n -- )  cells AD-REG + ! ;
: AD-CT@  ( n -- n )  cells AD-CT + @ ;
: AD-CT!  ( n n -- )  cells AD-CT + ! ;

\ create a node (op a b), return its id
: AD-NODE ( n n n -- n ) {: op a b :}
   AD-N @ {: id :}
   op id cells AD-OP + !  a id cells AD-A + !  b id cells AD-B + !
   -1 id cells AD-CT + !
   id 1+ AD-N !  id ;

\ --- DAG builder: symbolic-execute a forward op-code list (node 0 = input) ---
: AD-DO-OP ( n -- )
   dup OP-DUP  = if drop  AD-VTOP AD-VPUSH                              exit then
   dup OP-BMAX = if drop  OP-BMAX AD-VPOP -1 AD-NODE AD-VPUSH           exit then
   dup OP-EXP  = if drop  OP-EXP  AD-VPOP -1 AD-NODE AD-VPUSH           exit then
   dup OP-BSUM = if drop  OP-BSUM AD-VPOP -1 AD-NODE AD-VPUSH           exit then
   dup OP-BSUB = if drop  OP-BSUB AD-VPOP AD-VPOP swap AD-NODE AD-VPUSH exit then
   dup OP-BDIV = if drop  OP-BDIV AD-VPOP AD-VPOP swap AD-NODE AD-VPUSH exit then
   drop ;

: AD-BUILD ( ptr a n -- ) {: ops len :}
   0 AD-N !  0 AD-VSP !
   OP-LEAF -1 -1 AD-NODE AD-VPUSH              \ node 0 = the input tile
   len 0 ?do  ops i cells + @ AD-DO-OP  loop
   AD-VPOP AD-OUT ! ;

\ --- forward recompute emit: fill AD-REG for every node (node 0 reg given) ---
: AD-EMIT-NODE ( n -- ) {: id :}
   id AD-OP@ {: op :}
   op OP-BMAX = if  id AD-A@ AD-REG@ EMIT-BLOCK-MAX        id AD-REG! exit then
   op OP-EXP  = if  id AD-A@ AD-REG@ EMIT-EXP              id AD-REG! exit then
   op OP-BSUM = if  id AD-A@ AD-REG@ EMIT-BLOCK-SUM        id AD-REG! exit then
   op OP-BSUB = if  id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-B-   id AD-REG! exit then
   op OP-BDIV = if  id AD-A@ AD-REG@ id AD-B@ AD-REG@ EMIT-B/   id AD-REG! exit then ;

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

: AD-VJP ( n -- ) {: id :}
   id AD-OP@ {: op :}
   op OP-BMAX = if  id AD-VJP-BMAX exit then
   op OP-BSUB = if  id AD-VJP-BSUB exit then
   op OP-EXP  = if  id AD-VJP-EXP  exit then
   op OP-BSUM = if  id AD-VJP-BSUM exit then
   op OP-BDIV = if  id AD-VJP-BDIV exit then ;

\ reverse pass: seed dy on the output, propagate to node 0 -> dx
: AD-EMIT-REV ( n -- n )
   AD-OUT @ AD-CT!                             \ seed the output cotangent
   AD-N @ 1 ?do  AD-N @ i -  AD-VJP  loop      \ nodes high..1 (leaf 0 has no VJP)
   0 AD-CT@ ;                                  \ dx = input node's accumulated cotangent

\ --- top: build + emit the backward compute (x,dy registers -> dx register) ---
: AD-EMIT-BWD ( ptr a n n n -- n ) {: ops len xreg dyreg :}
   ops len AD-BUILD
   xreg AD-EMIT-FWD
   dyreg AD-EMIT-REV ;
