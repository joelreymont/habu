\ maki/db/budget-ledger-test.f - checked acceptance for the monotonic budget LEDGER
\ (maki/db/budget-ledger.f, dot habu-v2-capability-and-0970a96d). Proves the dot's acceptance,
\ each item by a named test:
\   LT-REMAIN-INIT     : a fresh ledger's remaining equals its declared limit
\   LT-RESERVE-OK/-EXHAUST : RESERVE is a typed fit check; exhaustion names the failing dimension
\   LT-RESERVE-NOMUT   : RESERVE is pure - an exhausted reserve mutates nothing
\   LT-CHARGE-ONCE     : CHARGE deducts the request from the remaining budget (monotonic)
\   LT-CHARGE-IDEMPOTENT : the SAME idempotency key charged twice deducts once (no double charge)
\   LT-CHARGE-KEYS     : two DISTINCT keys deduct twice
\   LT-CHARGE-EXHAUST* : a charge that no longer fits -> exhausted, with NO deduction and NO key
\   LT-REPLAY          : the same charges in EITHER order digest identically (deterministic replay)
\   LT-REPLAY-DIFFERS  : a different charge set digests differently (non-degeneracy)
\   LT-OVERFLOW / LT-KEY-OVERFLOW : the ledger pool + charged-key set fail closed at capacity
\
\ The test reopens package LEDGER (a friend) so RESERVE/CHARGE, the MATCH, and the private widths
\ read bare.

require lib/prelude.f
require lib/test.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f

package LEDGER

\ ---- typed budget-result decoders ----------------------------------------------
: BR-CODE ( budget-result -- n )   \ 0 ok / 1 exhausted
   MATCH budget-result
      ok        OF 0 ENDOF
      exhausted OF drop 1 ENDOF
   ;MATCH ;
: BR-DIM ( budget-result -- n )    \ exhausted dimension ordinal, else -1
   MATCH budget-result
      ok        OF -1 ENDOF
      exhausted OF BUDGET:DIM>N ENDOF
   ;MATCH ;

\ ---- fixtures ------------------------------------------------------------------
create KEY-A KEY-W allot
create KEY-B KEY-W allot
create KEY-C KEY-W allot
create DBUF-A DIGEST-BYTES allot
create DBUF-B DIGEST-BYTES allot

: FILL32 ( ptr u8 n -- ) {: p:ptr b:n :}
   0 begin dup KEY-W < while dup {: k:n :} b p k + c! 1+ repeat drop ;

: KEYS-INIT ( -- )   KEY-A $A1 FILL32  KEY-B $B2 FILL32  KEY-C $C3 FILL32 ;

: DIG-EQ? ( ptr u8 ptr u8 -- bool ) {: a:ptr b:ptr :}
   0 begin dup DIGEST-BYTES < while
      dup {: k:n :}  a k + c@  b k + c@  <> if drop false exit then  1+
   repeat drop true ;

\ A fresh ledger: compute-time limit 100, device-time limit 50.
: MK-L ( -- ledger )
   RESET OPEN {: l:ledger :}
   l BUDGET-DIM:COMPUTE-TIME 100 LIMIT!
   l BUDGET-DIM:DEVICE-TIME  50  LIMIT!
   l ;

: WANT-COMPUTE ( n -- )   BUDGET-DIM:COMPUTE-TIME swap REQ-CLEAR REQ+ ;

\ ---- acceptance ----------------------------------------------------------------
: LT-REMAIN-INIT ( -- n )   MK-L BUDGET-DIM:COMPUTE-TIME REMAINING@ ;

: LT-RESERVE-OK ( -- n )       MK-L {: l:ledger :}  40 WANT-COMPUTE  l RESERVE BR-CODE ;
: LT-RESERVE-EXHAUST ( -- n )  MK-L {: l:ledger :}  200 WANT-COMPUTE  l RESERVE BR-CODE ;
: LT-RESERVE-EXHAUST-DIM ( -- n )   MK-L {: l:ledger :}  200 WANT-COMPUTE  l RESERVE BR-DIM ;
: LT-RESERVE-NOMUT ( -- n )
   MK-L {: l:ledger :}  200 WANT-COMPUTE  l RESERVE drop
   l BUDGET-DIM:COMPUTE-TIME REMAINING@ ;

: LT-CHARGE-ONCE ( -- n )
   MK-L {: l:ledger :}  40 WANT-COMPUTE  l KEY-A CHARGE drop
   l BUDGET-DIM:COMPUTE-TIME REMAINING@ ;
: LT-CHARGE-IDEMPOTENT ( -- n )                     \ same key twice -> deducted once
   MK-L {: l:ledger :}  40 WANT-COMPUTE
   l KEY-A CHARGE drop  l KEY-A CHARGE drop
   l BUDGET-DIM:COMPUTE-TIME REMAINING@ ;
: LT-CHARGE-KEYS ( -- n )                            \ two distinct keys -> deducted twice
   MK-L {: l:ledger :}  40 WANT-COMPUTE
   l KEY-A CHARGE drop  l KEY-B CHARGE drop
   l BUDGET-DIM:COMPUTE-TIME REMAINING@ ;

: LT-CHARGE-EXHAUST ( -- n )                         \ a charge that no longer fits -> exhausted
   MK-L {: l:ledger :}  40 WANT-COMPUTE
   l KEY-A CHARGE drop  l KEY-B CHARGE drop          \ remaining compute = 20
   l KEY-C CHARGE BR-CODE ;
: LT-CHARGE-EXHAUST-NOMUT ( -- n )                   \ exhausted charge leaves remaining unchanged
   MK-L {: l:ledger :}  40 WANT-COMPUTE
   l KEY-A CHARGE drop  l KEY-B CHARGE drop
   l KEY-C CHARGE drop
   l BUDGET-DIM:COMPUTE-TIME REMAINING@ ;
: LT-CHARGE-EXHAUST-NOKEY ( -- n )                   \ exhausted charge did NOT record its key
   MK-L {: l:ledger :}  40 WANT-COMPUTE
   l KEY-A CHARGE drop  l KEY-B CHARGE drop
   l KEY-C CHARGE drop
   l KEY-COUNT ;

\ ---- deterministic replay (order-independent digest, both-order) ---------------
: CHARGE-AB ( ledger -- ) {: l:ledger :}            \ K1(compute 30) then K2(device 20)
   BUDGET-DIM:COMPUTE-TIME 30 REQ-CLEAR REQ+  l KEY-A CHARGE drop
   BUDGET-DIM:DEVICE-TIME  20 REQ-CLEAR REQ+  l KEY-B CHARGE drop ;
: CHARGE-BA ( ledger -- ) {: l:ledger :}            \ K2(device 20) then K1(compute 30) - reversed
   BUDGET-DIM:DEVICE-TIME  20 REQ-CLEAR REQ+  l KEY-B CHARGE drop
   BUDGET-DIM:COMPUTE-TIME 30 REQ-CLEAR REQ+  l KEY-A CHARGE drop ;
: MK-L2 ( -- ledger )                                \ a fresh ledger WITHOUT resetting the pool
   OPEN {: l:ledger :}
   l BUDGET-DIM:COMPUTE-TIME 100 LIMIT!
   l BUDGET-DIM:DEVICE-TIME  50  LIMIT!
   l ;

: LT-REPLAY ( -- bool )
   RESET
   MK-L2 {: a:ledger :}  a CHARGE-AB  a DBUF-A DIGEST-BYTES DIGEST drop
   MK-L2 {: b:ledger :}  b CHARGE-BA  b DBUF-B DIGEST-BYTES DIGEST drop
   DBUF-A DBUF-B DIG-EQ? ;
: LT-REPLAY-DIFFERS ( -- bool )                      \ a different charge set digests differently
   RESET
   MK-L2 {: a:ledger :}  a CHARGE-AB  a DBUF-A DIGEST-BYTES DIGEST drop
   MK-L2 {: b:ledger :}
   BUDGET-DIM:COMPUTE-TIME 30 REQ-CLEAR REQ+  b KEY-A CHARGE drop   \ only K1
   b DBUF-B DIGEST-BYTES DIGEST drop
   DBUF-A DBUF-B DIG-EQ? 0= ;

\ ---- capacity fail-closed ------------------------------------------------------
: LT-OVERFLOW ( -- )   RESET  33 0 ?do OPEN drop loop ;   \ LEDGER-CAP = 32
: LT-KEY-OVERFLOW ( -- )                                  \ MAX-KEYS = 64 distinct keys, then throw
   MK-L {: l:ledger :}
   0 WANT-COMPUTE                                         \ zero request: always fits, keys accumulate
   65 0 ?do  i KEY-A c!  l KEY-A CHARGE drop  loop ;

KEYS-INIT
T-RESET

LT-REMAIN-INIT 100 T=

LT-RESERVE-OK 0 T=
LT-RESERVE-EXHAUST 1 T=
LT-RESERVE-EXHAUST-DIM 0 T=
LT-RESERVE-NOMUT 100 T=

LT-CHARGE-ONCE 60 T=
LT-CHARGE-IDEMPOTENT 60 T=
LT-CHARGE-KEYS 20 T=
LT-CHARGE-EXHAUST 1 T=
LT-CHARGE-EXHAUST-NOMUT 20 T=
LT-CHARGE-EXHAUST-NOKEY 2 T=

LT-REPLAY TTRUE
LT-REPLAY-DIFFERS TTRUE

' LT-OVERFLOW E-LEDGER-CAP TTHROWS
' LT-KEY-OVERFLOW E-LEDGER-KEYS TTHROWS

LEDGER:RESET

T-REPORT

;package
