\ Loaded after the native build's real replacement-checker handoff
\ (test/native-window-owner-child.f), once the window's own
\ src/core/cell-effects.f has defined the operand declarers. On this freshly
\ transferred owner each declarer is a live binding whose control word holds
\ its intrinsic id and CTL-PARSES, as the selected-binding query the owner
\ record publishes (CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF) answers it.
package PARSES-WINDOW-TEST

: ASSERT ( bool -- )
   if exit then s" parses window assertion failed" 76 die ;

: DECLARER ( n n n n -- )
   {: sym:n eff:n ctl:n id:n :}
   sym 0 <> ASSERT
   eff 0 <> ASSERT
   ctl CTL>INTRINSIC id = ASSERT
   ctl CTL-PARSES and 0 <> ASSERT ;

s" parses:" CHECKER-VERIFY-TOP-BINDING INTRINSIC-PARSES DECLARER
s" parses-through:" CHECKER-VERIFY-TOP-BINDING INTRINSIC-PARSES-THROUGH DECLARER
s" parses window: ok" type cr
;package
