\ maki/regions.f - straight-line elementwise-chain region-candidate discovery.
\
\ CAD-PLAN section 5.1: the cad-2 fusion planner grows regions over the model-IR
\ DAG. cad-1 only DISCOVERS candidates - no legality matrix, no cost model. A
\ candidate is a maximal run of CONSECUTIVE nodes (adjacent in node order, the
\ straight-line dataflow the checked MODEL: capture produces) that are all
\ elementwise (op registry class) and linked producer->consumer with a SINGLE-USE
\ producer: node a links to a+1 iff both are elementwise, a+1 consumes a, and a's
\ output is used exactly once. Runs of length >= 2 are emitted as (start,end) node
\ spans for the planner to consume, plus a report row each.
\
\ maki -> habu only; regions owns -5063..-5064.

require maki/model-ir.f
require maki/report.f

-5063 constant E-RGN-CAP   \ candidate table capacity exceeded
-5064 constant E-RGN-IDX   \ candidate index out of range

package MAKI
private

64 constant RGN-CAP
create RGN-S RGN-CAP cells allot        \ candidate span start node
create RGN-E RGN-CAP cells allot        \ candidate span end node
variable RGN-N
variable RGN-CUR                         \ build cursor (avoids in-loop locals)

: RGN-IX ( n -- n )
   dup 0 < over RGN-N @ >= or if E-RGN-IDX throw then ;

\ occurrences of node ref `ref` among node `node`'s operands
: NODE-USES ( n n -- n ) {: node:n ref:n :}
   0  node MIR-IN-COUNT@ 0 ?do
      node i MIR-IN@ ref = if 1+ then
   loop ;

\ total times node `prod`'s output is consumed across the whole table
: REF-USES ( n -- n ) {: prod:n :}
   0  MIR-N@ 0 ?do
      i prod NODE-USES +
   loop ;

: NODE-EW? ( n -- bool )  MIR-OP@ OPR-ELEMENTWISE? ;

\ a -> a+1 links iff both elementwise, a+1 consumes a, a single-use
: RGN-LINK? ( n n -- bool ) {: a:n b:n :}
   a NODE-EW? 0= if 0 0= 0= exit then
   b NODE-EW? 0= if 0 0= 0= exit then
   b a NODE-USES 0= if 0 0= 0= exit then
   a REF-USES 1 <> if 0 0= 0= exit then
   0 0= ;

: RGN-CAN-EXTEND? ( n -- bool ) {: e:n :}
   e 1+ MIR-N@ >= if 0 0= 0= exit then
   e e 1+ RGN-LINK? ;

: RGN-EXTEND ( n -- n )                  \ start -- end (extend the maximal chain)
   begin dup RGN-CAN-EXTEND? while 1+ repeat ;

: RGN-EMIT ( n n -- ) {: s:n e:n :}
   RGN-N @ RGN-CAP >= if E-RGN-CAP throw then
   s RGN-S RGN-N @ cells + !
   e RGN-E RGN-N @ cells + !
   RGN-N @ 1+ RGN-N ! ;

public

: RGN-RESET ( -- )  0 RGN-N ! ;
: RGN-COUNT ( -- n )  RGN-N @ ;
: RGN-START@ ( n -- n )  RGN-IX cells RGN-S + @ ;
: RGN-END@   ( n -- n )  RGN-IX cells RGN-E + @ ;
: RGN-LEN@   ( n -- n ) {: idx:n :}  idx RGN-END@  idx RGN-START@  -  1+ ;

: RGN-BUILD ( -- )                       \ discover candidates over the current IR
   RGN-RESET
   0 RGN-CUR !
   begin RGN-CUR @ MIR-N@ < while
      RGN-CUR @ NODE-EW? if
         RGN-CUR @ RGN-EXTEND                          \ ( e )
         dup RGN-CUR @ > if  dup RGN-CUR @ swap RGN-EMIT  then
         1+ RGN-CUR !                                  \ resume past the chain end
      else
         RGN-CUR @ 1+ RGN-CUR !
      then
   repeat ;

\ ---- report rows (one warning line per candidate span) ---------------------
: RGN-DESC$ ( n -- ptr u8 n ) {: idx:n :}
   SB-RESET
   s" region-candidate: nodes " SB-APPEND
   idx RGN-START@ SB-INT  s" .." SB-APPEND  idx RGN-END@ SB-INT
   s"  len " SB-APPEND  idx RGN-LEN@ SB-INT
   s"  elementwise" SB-APPEND
   SB$ ;

: RGN-REPORT+ ( report -- report )
   RGN-COUNT 0 ?do  i RGN-DESC$ RPT-WARN+  loop ;

end-package
