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
require test/checker-assert.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f

\ ---- declaration-shape reflection ----------------------------------------------
\ budget-result is declared through the unified ENUM front end in full mode, so its
\ exhausted arm publishes a named FIELD as a type-registry row keyed
\ (family, variant). These helpers read those rows through the public read-only
\ registry axioms (the same ones tools/public-signatures-core.f reads; they cannot
\ mutate anything), so the pins below can state the field NAME to payload SLOT
\ mapping the declaration published. A family is identified by its tail plus the
\ constructor package its variants carry - exactly the (package, tail) pair that
\ owns family identity - which also keeps the pins off the unrelated arity-0 `dim`
\ cell family that shares a tail with BUDGET:dim.
\ The readers live in REFLECT (test/checker-assert.f); this package holds only the
\ identity this suite pins.
package LEDGER-PINS
public

: BR$ ( -- ptr u8 n ptr u8 n )   s" budget-result" s" LEDGER-BUDGET--RESULT" ;

;package

package LEDGER

: LT-LEDGER-ROUNDTRIP ( -- n )
   17 LEDGER-LEDGER:MAKE LEDGER-LEDGER:UNMAKE ;

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

\ ---- named-payload round-trip through the production producer -------------------
\ `retries` is ordinal 4: deliberately NOT compute-time, whose ordinal is 0 and
\ would make a dropped or zeroed payload read back as a legitimate dimension.
: LT-BR-DIM ( -- BUDGET:dim )        BUDGET-DIM:RETRIES ;
: LT-BR-DIM-NONZERO ( -- bool )      LT-BR-DIM BUDGET:DIM>N 0<> ;
: LT-BR-MK-EXH ( BUDGET:dim -- budget-result )   BR-EXHAUSTED ;
: LT-BR-RT-CODE ( -- n )             LT-BR-DIM LT-BR-MK-EXH BR-CODE ;
: LT-BR-RT-DIM ( -- n )              LT-BR-DIM LT-BR-MK-EXH BR-DIM ;
: LT-BR-OK-DIM ( -- n )              BR-OK BR-DIM ;   \ the payloadless arm carries none

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
: LT-INVALID-HANDLE ( -- )   RESET 0 >LEDGER KEY-COUNT drop ;
: LT-DIGEST-BUF ( -- )   MK-L DBUF-A DIGEST-BYTES 1- DIGEST drop ;

KEYS-INIT
T-RESET

LT-LEDGER-ROUNDTRIP 17 T=
s" LT-LEDGER-MAKE ( n -- LEDGER:ledger ) LEDGER-LEDGER:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" LT-LEDGER-UNMAKE ( LEDGER:ledger -- n ) LEDGER-LEDGER:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" LT-LEDGER-WRONG ( bool -- LEDGER:ledger ) LEDGER-LEDGER:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=

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
' LT-INVALID-HANDLE E-LEDGER-RANGE TTHROWS
' LT-DIGEST-BUF E-LEDGER-BUF TTHROWS

\ ==== budget-result as a full-mode payload ENUM =================================
\ The generated constructors, by exact spelling and exact effect. A -1 means the
\ checker resolved EXACTLY this name (it answers 1 for a name it cannot resolve),
\ so these also prove the constructor package did not drift.
s" BR-P-OK ( -- LEDGER:budget-result ) LEDGER-BUDGET--RESULT:OK"
   CHECK-QUIET-CANDIDATE! -1 T=
s" BR-P-EXH ( BUDGET:dim -- LEDGER:budget-result ) LEDGER-BUDGET--RESULT:EXHAUSTED"
   CHECK-QUIET-CANDIDATE! -1 T=
\ the dimension payload is mandatory and typed: a payloadless exhausted, a raw cell,
\ a payload on the ok arm, and a bare-scalar result all reject.
s" BR-F-NOPAY ( -- LEDGER:budget-result ) LEDGER-BUDGET--RESULT:EXHAUSTED"
   CHECK-QUIET-CANDIDATE! 0 T=
s" BR-F-RAW ( n -- LEDGER:budget-result ) LEDGER-BUDGET--RESULT:EXHAUSTED"
   CHECK-QUIET-CANDIDATE! 0 T=
s" BR-F-OKPAY ( BUDGET:dim -- LEDGER:budget-result ) LEDGER-BUDGET--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" BR-F-BARE ( BUDGET:dim -- n ) LEDGER-BUDGET--RESULT:EXHAUSTED"
   CHECK-QUIET-CANDIDATE! 0 T=
\ MATCH arm bindings are per-arm: the payloadless ok arm binds nothing.
s" BR-M-OK ( LEDGER:budget-result -- n ) MATCH LEDGER:budget-result ok OF 0 ENDOF exhausted OF {: d:BUDGET:dim :} 1 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! -1 T=
s" BR-M-SWAP ( LEDGER:budget-result -- n ) MATCH LEDGER:budget-result ok OF {: d:BUDGET:dim :} 0 ENDOF exhausted OF {: d:BUDGET:dim :} 1 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! 0 T=

\ the exhausted arm carries exactly one named cell `dim` at payload slot 0, and the
\ ok arm carries none. The identity is (tail, constructor package), so these pins
\ are about LEDGER's own family and nothing else.
LEDGER-PINS:BR$ REFLECT:FAMS 1 T=
LEDGER-PINS:BR$ REFLECT:VARS 2 T=
LEDGER-PINS:BR$ REFLECT:WIDTH 2 T=          \ one payload cell plus one tag cell
LEDGER-PINS:BR$ 0 REFLECT:ARM$ s" ok" T$=
LEDGER-PINS:BR$ 1 REFLECT:ARM$ s" exhausted" T$=
LEDGER-PINS:BR$ 0 REFLECT:ARM-FLDS 0 T=
LEDGER-PINS:BR$ 1 REFLECT:ARM-FLDS 1 T=
LEDGER-PINS:BR$ 1 s" dim" REFLECT:ARM-SLOT 0 T=
LEDGER-PINS:BR$ 0 s" dim" REFLECT:ARM-SLOT -1 T=   \ the name is per-arm
LEDGER-PINS:BR$ 1 s" budget" REFLECT:ARM-SLOT -1 T= \ an undeclared name has no slot

\ constructed directly through the production producer and matched straight back.
\ The dimension under test is `retries` (ordinal 4) rather than `compute-time`,
\ because compute-time's ordinal is 0 and a dropped or zeroed dimension payload
\ would read back as 0 and pass; LT-RESERVE-EXHAUST-DIM above is exactly that
\ zero-ordinal case, so this is the leg that can see a lost payload.
LT-BR-DIM-NONZERO TTRUE                          \ the dimension under test is not ordinal 0
LT-BR-RT-CODE 1 T=                               \ exhausted dispatches to its own arm
LT-BR-RT-DIM 4 T=                                \ and carries `retries`, not a zeroed ordinal
LT-BR-OK-DIM -1 T=                               \ the no-payload arm of BR-DIM is live

public

\ br-twin is budget-result's SHAPE under a different name, for the same reason:
\ identity is nominal, so an identically shaped family never unifies with it in
\ either direction. Public because a private family publishes no constructors.
ENUM br-twin 0
   VARIANT br-twin-ok ;VARIANT
   VARIANT br-twin-exhausted FIELD dim BUDGET:dim ;VARIANT
;ENUM

private

s" BR-TW ( BUDGET:dim -- br-twin ) LEDGER-BR--TWIN:BR-TWIN-EXHAUSTED"
   CHECK-QUIET-CANDIDATE! -1 T=
s" BR-TW-X1 ( BUDGET:dim -- br-twin ) LEDGER-BUDGET--RESULT:EXHAUSTED"
   CHECK-QUIET-CANDIDATE! 0 T=
s" BR-TW-X2 ( BUDGET:dim -- LEDGER:budget-result ) LEDGER-BR--TWIN:BR-TWIN-EXHAUSTED"
   CHECK-QUIET-CANDIDATE! 0 T=

LEDGER:RESET

T-REPORT

;package
