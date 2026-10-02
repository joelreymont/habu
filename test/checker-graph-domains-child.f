\ A saved effect graph may share a quotation body across binder paths.  The
\ validator must check the bound-variable domain on every distinct path.

: GRAPH-DOMAIN-TWO ( forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> ]> forall<q,[ read-view<q,q,u16> -- read-view<q,q,u16> ]> -- ) 2drop ;
: GD-SEED ( forall<p,[ -- ]> -- ) drop ;

package CHECKER-REG
CHECKER-ASIG-ARM

PTR-VARIABLE DOMAIN-GRAPH
PTR-VARIABLE DOMAIN-FIRST
PTR-VARIABLE DOMAIN-SECOND
PTR-VARIABLE DOMAIN-REGION-TARGET
variable DOMAIN-BODY
$4000 constant DOMAIN-CAP
create DOMAIN-POOL DOMAIN-CAP allot
variable DOMAIN-USED
$10000 constant GD-CAP
create GD-POOL GD-CAP allot
variable GD-USED

TRUSTED: GD-NODE ( n n n n -- n ) {: tag:n a:n b:n d:n :}
   GD-USED @ {: at:n :}
   at EFF-NODE + GD-CAP > IF 79 throw THEN
   GD-POOL at + {: node:ptr :}
   node EFF-NODE ASIG-GRAPH-ZERO
   tag node EN.TAG ! a node EN.A ! b node EN.B ! d node EN.D !
   tag EN-PUSH = IF 2 node EN.C ! THEN
   EFF-NODE GD-USED +! at ;

TRUSTED: GD-LAYER ( n -- n ) {: inner:n :}
   EN-FORALL inner 0 BIND-SCOPE GD-NODE {: scope:n :}
   EN-FORALL inner 0 BIND-REGION GD-NODE {: region:n :}
   EN-PUSH region 0 0 GD-NODE {: tail:n :}
   EN-PUSH scope tail 0 GD-NODE {: head:n :}
   EN-QUOT head 0 0 GD-NODE ;

TRUSTED: GD-BUILD ( n -- ) {: depth:n :}
   s" GD-SEED" CHECKER-FIND-ACTIVE-SYM ASIG-GRAPH-COPY ASIG-STR-P @ + {: src:ptr :}
   src EW.NEXT @ dup GD-CAP > IF 79 throw THEN GD-USED !
   src GD-POOL GD-USED @ USIGS-COPY
   GD-POOL GD-POOL EW.DIN @ + EN.A @ GD-POOL + {: binder:ptr :}
   binder EN.A @ depth 0 ?do GD-LAYER loop binder EN.A !
   GD-USED @ GD-POOL EW.NEXT ! ;

TRUSTED: GD-MEMORY ( n -- n )
   GD-BUILD
   GD-POOL GD-USED @ ASIG-GRAPH-CHECK
   CK-LEX-MEMO-U @ ;

TRUSTED: GD-RUN ( -- )
   8 GD-MEMORY {: small:n :}
   12 GD-MEMORY {: large:n :}
   small . large . cr
   small 0 <= large small 3 * > or IF
      79 throw
   THEN
   s" shared graph bounded" type cr ;
TRUSTED: DOMAIN-CHECK ( -- )
   DOMAIN-GRAPH @ dup EW.NEXT @ ASIG-GRAPH-CHECK ;

TRUSTED: DOMAIN-MODE? ( ptr u8 n -- bool )
   s" HABU_GRAPH_DOMAIN_MODE" GETENV CORE-STR= ;

TRUSTED: DOMAIN-NESTED? ( -- bool )
   s" nested-baseline" DOMAIN-MODE?
   s" nested-region-separate" DOMAIN-MODE? or
   s" nested-region-shared" DOMAIN-MODE? or
   s" nested-scope-shared" DOMAIN-MODE? or ;

TRUSTED: DOMAIN-ALLOC ( -- n )
   DOMAIN-USED @ dup EFF-NODE + DOMAIN-CAP > IF 79 throw THEN
   EFF-NODE DOMAIN-USED +! ;

TRUSTED: DOMAIN-DUP ( ptr u8 -- n ) {: src:ptr :}
   DOMAIN-ALLOC {: off:n :}
   src DOMAIN-GRAPH @ off + EFF-NODE USIGS-COPY
   off ;

TRUSTED: DOMAIN-SHIFT-VIEW ( n -- ) {: rowoff:n :}
   DOMAIN-GRAPH @ {: graph:ptr :}
   graph rowoff + {: row:ptr :}
   row EN.TAG @ EN-PUSH <> IF 79 throw THEN
   graph row EN.A @ + {: view:ptr :}
   view EN.TAG @ EN-PARAM <> IF 79 throw THEN
   view EN.C @ 0 ?do
      graph view EN.D @ + i cells + CELL-VIEW @ graph + {: arg:ptr :}
      arg EN.TAG @ EN-BVAR = IF
         arg EN.A @ dup 0 <> swap 1 <> and IF 79 throw THEN
         1 arg EN.A !
      THEN
   loop ;

TRUSTED: DOMAIN-SHIFT-BODY ( ptr u8 -- ) {: binder:ptr :}
   DOMAIN-GRAPH @ {: graph:ptr :}
   graph binder EN.A @ + {: quot:ptr :}
   quot EN.TAG @ EN-QUOT <> IF 79 throw THEN
   quot EN.A @ DOMAIN-SHIFT-VIEW
   quot EN.B @ DOMAIN-SHIFT-VIEW ;

TRUSTED: DOMAIN-WRAP ( ptr u8 ptr u8 -- n ) {: binder:ptr row:ptr :}
   DOMAIN-GRAPH @ {: graph:ptr :}
   row DOMAIN-DUP {: input:n :}
   0 graph input + EN.B !
   binder EN.A @ graph + DOMAIN-DUP {: quot:n :}
   input graph quot + EN.A !
   0 graph quot + EN.B !
   binder DOMAIN-DUP {: outer:n :}
   quot graph outer + EN.A !
   outer ;

TRUSTED: DOMAIN-BUILD-NESTED ( ptr u8 ptr u8 -- ) {: first-row:ptr second-row:ptr :}
   DOMAIN-FIRST @ DOMAIN-SHIFT-BODY
   DOMAIN-SECOND @ DOMAIN-SHIFT-BODY
   DOMAIN-FIRST @ first-row DOMAIN-WRAP first-row EN.A !
   DOMAIN-SECOND @ second-row DOMAIN-WRAP second-row EN.A !
   DOMAIN-GRAPH @ second-row EN.A @ + DOMAIN-REGION-TARGET !
   DOMAIN-USED @ DOMAIN-GRAPH @ EW.NEXT ! ;

TRUSTED: DOMAIN-PREPARE ( -- )
   s" GRAPH-DOMAIN-TWO" CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym 0= IF 79 throw THEN
   sym ASIG-GRAPH-COPY ASIG-STR-P @ + {: source:ptr :}
   source EW.NEXT @ dup DOMAIN-CAP > IF 79 throw THEN DOMAIN-USED !
   source DOMAIN-POOL DOMAIN-USED @ USIGS-COPY
   DOMAIN-POOL DOMAIN-GRAPH !
   DOMAIN-GRAPH @ {: graph:ptr :}
   graph EW.DIN @ graph + {: first-push:ptr :}
   first-push EN.TAG @ EN-PUSH <> IF 79 throw THEN
   first-push EN.A @ graph + DOMAIN-FIRST !
   first-push EN.B @ graph + {: second-push:ptr :}
   second-push EN.TAG @ EN-PUSH <> IF 79 throw THEN
   second-push EN.A @ graph + DOMAIN-SECOND !
   DOMAIN-FIRST @ EN.TAG @ EN-FORALL <> IF 79 throw THEN
   DOMAIN-SECOND @ EN.TAG @ EN-FORALL <> IF 79 throw THEN
   DOMAIN-SECOND @ DOMAIN-REGION-TARGET !
   DOMAIN-NESTED? IF first-push second-push DOMAIN-BUILD-NESTED THEN
   DOMAIN-FIRST @ EN.A @ DOMAIN-BODY ! ;

TRUSTED: DOMAIN-RUN ( -- )
   s" shared-dag" DOMAIN-MODE? IF GD-RUN EXIT THEN
   DOMAIN-PREPARE
   s" region-separate" DOMAIN-MODE? s" region-shared" DOMAIN-MODE? or
   s" nested-region-separate" DOMAIN-MODE? or
   s" nested-region-shared" DOMAIN-MODE? or IF
      BIND-REGION DOMAIN-REGION-TARGET @ EN.D ! THEN
   s" region-shared" DOMAIN-MODE? s" scope-shared" DOMAIN-MODE? or
   s" nested-region-shared" DOMAIN-MODE? or
   s" nested-scope-shared" DOMAIN-MODE? or IF
      DOMAIN-BODY @ DOMAIN-SECOND @ EN.A ! THEN
   DOMAIN-CHECK
   s" graph domain accepted" type cr ;

DOMAIN-RUN
;package
