\ native-window-owner.f - the window's checker is the sole certifier for window
\ source.
\
\ Each case runs test/native-window-owner-child.f - the tools/native-build.f
\ window reduced to the checker handover - in its own engine child, and asserts
\ the verdict that window reached for one fixture included straight after
\ src/core/cell-effects.f:
\   - a family the window itself declares resolves (0);
\   - a family nothing declares does not (E-CAST-FAM 7131). This is the control:
\     without it the accept case above would pass against a checker that
\     certifies nothing at all;
\   - a family only the host that opened the window declares does not
\     (E-CAST-FAM, never E-CAST-OWNER 7135). A product engine hosting the build
\     left its retained checker answering window certifications: that refused
\     the first case and resolved this one in a package the window cannot see.
\
\ The accept case runs again at the optimizing tier, beside a store through a
\ call, and the bindings fixture loads its source closure at tier 0. The five
\ tier-1 source-closure cases, about 22 s apiece, are
\ test/native-window-source.f, test/native-window-boundary.f and
\ test/native-window-payload.f, gate rows of their own: one row running all
\ eleven cases took 219 s of the gate's 360 s child timeout in the pool.
\
\ Run: bin/hb --load test/native-window-owner.f

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f
require test/native-window-owner-lib.f

package NW-OWNER-TEST

180000 constant DEADLINE-MS   \ the child checks the whole core prefix

: ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHILD$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   fx fxu >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV! ;

: WINDOW-IS ( ptr u8 n ptr u8 n -- ) {: fx:ptr fxu:n want:ptr wantu:n :}
   fx fxu ARGS!
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$= ;

\ Ordinary cases stop at the checker handover.
: WINDOW-TIER1-IS ( ptr u8 n ptr u8 n -- ) {: fx:ptr fxu:n want:ptr wantu:n :}
   fx fxu TIER1-ARGS! want wantu WINDOW-TIER1-RESULT ;

: WINDOW-SOURCE-JIT ( ptr u8 n -- )
   ARGS! WINDOW-SOURCE-DEPS ;

: OWNER-CASES ( -- )
   s" native-window-owner" PREPARE
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-IS
   s" test/native-window-cast-bad.f"      S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-host-bad.f" S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-TIER1-IS
   s" test/native-window-call-store.f"    S\" window: 0\n"    WINDOW-TIER1-IS
   s" test/native-window-owner-bindings.f" WINDOW-SOURCE-JIT ;

\ Public so the driver below runs it with the package closed.
public

: RUN ( -- )
   [: OWNER-CASES ;] RUN-CASES
   s" native-window-owner: ok" type cr ;

;package

NW-OWNER-TEST:RUN
