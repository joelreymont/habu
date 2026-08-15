\ aot-prelude-band-suite.f - the two-sided prelude-band audit of the AOT capture.
\
\ WHAT THIS LOCKS. A captured call site travels as a NAME and the seed resolves it
\ in the engine it is booting, so a window word may only call something that
\ engine will have: a word of the capturing process whose prefix the target shares,
\ or a word inside the window itself. A capture running in a BOOTED bin/hb - which
\ is what the compiler chain needs, because the metabuild host's dictionary is not
\ the target's - has a third kind in front of it: the files the capture tool had to
\ load before it could capture anything. Those exist in the capturing engine and in
\ no target, and a call into one bakes a name LFIND cannot answer at the boot of a
\ shipped binary. The same goes for an ADDRESS: a window word holding a prelude
\ buffer's address would have it rebased into a pointer at whatever the seeded
\ engine put at that offset.
\
\ WHY IT NEEDS ITS OWN FIXTURE. The refusal has no reachable producer in the
\ metabuild host: that host compiles only what the target's prefix will carry, so
\ its band is empty and stays empty. test/aot-band-lib.f captures inside a booted
\ engine instead, where the band is real - the five files the capture itself needs.
\ Every case is a child process, and its exit code and diagnostic are the assertion.
\
\ THE CASES, and what each would let through if it stopped working:
\   call/real   CALLER calls a prelude word: refused, naming the window word that
\               makes the call and the callee. The same window holds NAMER, which
\               carries the callee's SPELLING as a string and calls nothing - so a
\               diagnostic that named NAMER would be reading text rather than call
\               sites, and this case asserts it names one and not the other.
\   call/empty  the same window with an empty band: captured, two call sites. It is
\               the non-vacuity control twice over - it says the window really does
\               call out (the callee was not inlined away) and that the audit
\               refuses a BAND rather than refusing everything.
\   call/none   a capture that never declared a band at all: refused. A producer
\               that forgets the marks does not get the benefit of the doubt.
\   call/high   a record mark above the window's own first record: refused, because
\               a band that begins after the window it bounds describes nothing.
\   call/dhigh  the same for the DATA mark, moved on its own so the two halves of
\               that check are told apart rather than sharing one fixture.
\   data/real   HOLDER holds a prelude buffer's address: refused by the capture's
\               own totality check, which now names the window word that holds the
\               address and says the address is in the prelude band.
\   data/empty  the same window and the same address with an empty band: refused
\               the same way and still named, but placed BELOW the band instead of
\               inside it. The pair is what shows the band classifies the address
\               rather than decorating every refusal with the same sentence.
\
\ Cost: seven child bin/hb runs, no metabuild. Registered as
\ `SUITE aot-prelude-band` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-prelude-band-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-PRELUDE-BAND

$4000 constant CAP
30000 constant CASE-TIMEOUT-MS
74 constant REFUSE-RC

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: HB$ ( -- ptr u8 n )   s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: SETUP ( -- )
   s" habu-aot-band" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

\ One case: bin/hb --load <case file>, with the band mode in the environment and a
\ private HB_TMP, exactly the way the capture tool will be run.
: RUN-CASE ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n m:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   f fu >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   s" HABU_BAND" >LEN m mu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  CASE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: CALL-CASE ( ptr u8 n -- ) s" test/aot-band-call.f" 2swap RUN-CASE ;
: DATA-CASE ( ptr u8 n -- ) s" test/aot-band-data.f" 2swap RUN-CASE ;

\ The diagnostic goes to stdout ahead of the die, so both streams are searched:
\ which one carries it is the engine's business, not this suite's.
: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT$ a u CONTAINS?  ERR$ a u CONTAINS? or ;

: DIAG. ( -- )
   s" aot-prelude-band: child stdout:" type cr OUT$ type cr
   s" aot-prelude-band: child stderr:" type cr ERR$ type cr ;

: REFUSED ( ptr u8 n -- ) {: r:ptr ru:n :}
   RC @ REFUSE-RC <> if DIAG. then
   RC @ REFUSE-RC T=
   r ru SAID? TTRUE ;

: PROBE-CALL-REFUSED ( -- )
   s" real" CALL-CASE
   s" a window word calling a prelude word is refused" T-LABEL
   s" aot-capture: window call into the prelude band" REFUSED
   s" the refusal names the window word that makes the call" T-LABEL
   s" window word CALLER" SAID? TTRUE
   s" ... and the callee the target has not got" T-LABEL
   s" calls CALLEE" SAID? TTRUE
   s" ... and not the word that merely spells the callee out" T-LABEL
   s" NAMER" SAID? 0= TTRUE ;

: PROBE-CALL-CAPTURED ( -- )
   s" empty" CALL-CASE
   s" the same window with an empty band captures" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... and its call really is a call site, not an inlined copy" T-LABEL
   s" sites=2" SAID? TTRUE ;

: PROBE-UNMARKED ( -- )
   s" none" CALL-CASE
   s" a capture that declared no band at all is refused" T-LABEL
   s" aot-capture: capture without a declared prelude band" REFUSED ;

: PROBE-MARK-ABOVE ( -- )
   s" high" CALL-CASE
   s" a band beginning after the window it bounds is refused" T-LABEL
   s" aot-capture: prelude mark above the window's first record" REFUSED ;

: PROBE-DATA-MARK-ABOVE ( -- )
   s" dhigh" CALL-CASE
   s" a DATA mark above the window's own DATA base is refused" T-LABEL
   s" aot-capture: prelude DATA mark above the window's DATA base" REFUSED ;

: PROBE-DATA-REFUSED ( -- )
   s" real" DATA-CASE
   s" a window word holding a prelude address is refused" T-LABEL
   s" aot-capture: recorded address site outside both window spans" REFUSED
   s" the refusal names the window word that holds it" T-LABEL
   s" window word HOLDER" SAID? TTRUE
   s" ... and places the address in the prelude band" T-LABEL
   s" in the prelude band" SAID? TTRUE ;

: PROBE-DATA-BELOW ( -- )
   s" empty" DATA-CASE
   s" the same address under an empty band is refused as this engine's own" T-LABEL
   s" aot-capture: recorded address site outside both window spans" REFUSED
   s" ... still named, and no longer called a prelude address" T-LABEL
   s" window word HOLDER" SAID? TTRUE
   s" in the prelude band" SAID? 0= TTRUE
   s" below this process's own start" SAID? TTRUE ;

: BODY ( -- )
   SETUP
   PROBE-CALL-REFUSED
   PROBE-CALL-CAPTURED
   PROBE-UNMARKED
   PROBE-MARK-ABOVE
   PROBE-DATA-MARK-ABOVE
   PROBE-DATA-REFUSED
   PROBE-DATA-BELOW ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-prelude-band: ok" type cr ;

;package

AOT-PRELUDE-BAND:RUN
