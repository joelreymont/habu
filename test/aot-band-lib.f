\ aot-band-lib.f - the shared half of the prelude-band cases: a capture that runs
\ in a BOOTED bin/hb, the way the chain capture has to.
\
\ WHAT THESE CASES ARE FOR. src/habu/aot-capture.f refuses a capture whose window
\ calls, or holds the address of, a word that exists only in the process doing the
\ capturing. That refusal has no reachable producer in the metabuild host - the
\ host compiles nothing the target's prefix will not carry - so the only way to
\ exercise it is to capture somewhere that DOES have a prelude, which is exactly
\ what a capture running inside a booted engine has: the five files it must load
\ before it can capture anything.
\
\ THE MARKS ARE TAKEN FIRST, before this file requires anything, so the band it
\ declares is every record and every DATA byte the case process added on its own.
\ A `package` line is a dictionary record too, which is why the package opens
\ before the marks are read: the record it writes belongs below the mark, with the
\ engine's own words, and the two variables that hold the marks belong above it,
\ with the rest of the prelude.
\
\ HB_TMP is set by the suite; every case is a child process whose exit code and
\ diagnostic ARE the assertion.

package AOT-BAND
public
ndict@ variable PRE-R  variable PRE-D  PRE-R !
PRE-R PRE-D !
;package

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-BAND
using AOT-BUF
public

\ A prelude DEFERRED word, and its dispatch cell is the DATA-side fixture. The cell
\ is a DATA address this process allotted and no engine this capture could seed
\ has, and `is` is the vehicle that puts that address in the CALLER's own body: its
\ compile handler emits the cell's address as a recorded chain there (habu2.f J-IS
\ through C-DATA-ADDR-RAW), so a window word that re-points this defer carries a
\ prelude address the capture's DATA audit has to classify.
\
\ THE VEHICLE USED TO BE AN INLINED COPY of a `create`d field's body, which is
\ parked: the compile-mode inliner takes the call arm unconditionally (habu2.f
\ C-CALL, "THE INLINE ARM IS OFF, AND TIER 0 ALWAYS CALLS"), so a copied body
\ carries nothing and the window word simply CALLED the field. Dot
\ habu-decide-the-tier-374c95ff owns that decision. `is` is emitted by the engine
\ as it stands and is unaffected by it either way: the chain is created in the
\ window word rather than copied into it, so the inliner's decline
\ (AOT-WINDOW:EMIT-OUTSIDE) has nothing to decline and the case survives the
\ arm being switched back on.
defer SINK ( -- n )

\ A prelude word the window's CALLER calls, and long enough to be worth calling if
\ the inliner is ever switched back on: the call audit is about call sites, so a
\ fixture that got inlined would test nothing. Today nothing is inlined (the C-CALL
\ note above), so the length is insurance rather than the reason; if it ever does
\ start being copied the case fails loudly - the refusal it expects will not arrive.
: CALLEE ( n -- n ) {: v:n :}
   v 1 +  v 2 * +  v 3 * +  v 5 * +  v 7 * +  v 11 * +  v 13 * +
   v 17 * +  v 19 * +  v 23 * +  v 29 * +  v 31 * + ;

: MODE$ ( -- ptr u8 n ) s" HABU_BAND" GETENV ;
: MODE= ( ptr u8 n -- bool ) {: a:ptr u:n :} MODE$ a u STR= ;

\ The band this run declares, read against the window AOT-ARM latched. `real` is
\ what a capture tool must declare; the other three are the ways of getting it
\ wrong, each with its own refusal. `high` and `dhigh` move one mark each, so the
\ two halves of the mark check are told apart: a record mark above the window,
\ and a DATA mark above it.
: MARK ( -- )
   s" none" MODE= if exit then
   s" empty" MODE= if AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK exit then
   s" high" MODE= if AOT-ARM:R1 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK exit then
   s" dhigh" MODE= if AOT-ARM:R0 @ AOT-ARM:D1 @ AOT-CAPTURE:PRELUDE-MARK exit then
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK ;

: GO ( -- )
   MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   s" aot-band: captured recs=" type AOT-REC-N @ .
   s" sites=" type AOT-SITE-N @ . cr ;

;package
