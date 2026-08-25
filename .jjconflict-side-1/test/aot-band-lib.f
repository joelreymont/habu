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

\ A prelude buffer. Its address exists in this process and in no engine this
\ capture could ever seed, which is what makes it the DATA-side fixture.
create BUF 8 allot

\ A prelude word long enough that the engine's compile-mode inliner emits a CALL
\ to it rather than copying its body: the audit is about call sites, so a fixture
\ that gets inlined would test nothing. If it ever starts being copied the case
\ fails loudly - the refusal it expects will not arrive.
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
