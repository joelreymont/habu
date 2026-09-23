\ A stripped application must retain every address-only quotation target and
\ preserve ordinary DATA and scalar literals beside them.
package STRIPPED-QUOTATION-SUBJECT

$4A constant FAILURE-RC
$D65F03C0 constant CODE-LIKE-BITS

create SAVED-CELL $2A ,

: EXPECT ( bool -- )
   0= if s" stripped-quotation: mismatch" FAILURE-RC die then ;

: APPLY ( n [ n -- n ] -- n )
   execute ;

\ Only its execution token is reachable. Its size keeps the native compiler
\ from satisfying the fixture by inlining its body at the tick site.
: LONG-CALLEE ( n -- n )
   1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+
   1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+
   1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+
   1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;

: NAMED-TICK ( n -- n )
   ['] LONG-CALLEE APPLY ;

\ Primitive ticks are raw engine execution tokens; this fixture boundary gives
\ 1+'s checked scalar effect to the otherwise untyped primitive address. Retire
\ it when the checker derives a quotation effect for primitive ticks.
TRUSTED: PRIMITIVE-XT ( -- [ n -- n ] )
   ['] 1+ ;

: PRIMITIVE-TICK ( n -- n )
   PRIMITIVE-XT APPLY ;

: ANONYMOUS ( n -- n )
   [: 2 * ;] APPLY ;

: INNER-ANONYMOUS ( n -- n )
   [: 1+ ;] APPLY ;

\ The outer address-only body reaches a word that invokes a second anonymous
\ body, so the stripped closure must retain the nested runtime path in order.
: NESTED ( n -- n )
   [: INNER-ANONYMOUS 1+ ;] APPLY ;

: FRAME-CALLEE ( n -- n )
   1+ ;

: SAVED-IN-QUOT ( n n -- n )
   [: >r FRAME-CALLEE r> + ;] execute ;

: SAVED-DEPTH? ( -- bool )
   91 3 7 SAVED-IN-QUOT 11 = swap 91 = and ;

: DATA? ( -- bool )
   SAVED-CELL @ $2A = ;

: CODE-LIKE-BITS? ( -- bool )
   SAVED-CELL @ CODE-LIKE-BITS xor $D65F03EA = ;

\ The empty quotation is this record's last function, so its one RET is the
\ instruction the recorded length leaves out (publish.f RECORDED-LEN). The
\ member the link measures re-adds it (aot-closure.f REC-BYTES through
\ CODE-SPAN:BYTES), so the ADR target lies inside the member, not at its end.
defer EMPTY-HOOK ( -- )

: INSTALL-EMPTY ( -- )
   [: ;] is EMPTY-HOOK ;

public

: RUN ( -- )
   10 NAMED-TICK 42 = EXPECT
   41 PRIMITIVE-TICK 42 = EXPECT
   21 ANONYMOUS 42 = EXPECT
   40 NESTED 42 = EXPECT
   SAVED-DEPTH? EXPECT
   DATA? EXPECT
   CODE-LIKE-BITS? EXPECT
   INSTALL-EMPTY EMPTY-HOOK
   s" stripped-quotation: ok" type cr ;

;package

: MAIN ( -- )
   STRIPPED-QUOTATION-SUBJECT:RUN ;
