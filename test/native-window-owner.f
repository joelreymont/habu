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
\ HABU_UNDER_TEST picks the host engine (lib/engine-candidate.f), so the same
\ three cases run under the gate's own bin/hb and, pointed at one, under a
\ bootstrap seed.
\
\ Run: bin/hb --load test/native-window-owner.f

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f

package NW-OWNER-TEST

$4000 constant IO-CAP
180000 constant DEADLINE-MS   \ the child checks the whole core prefix

create OUT IO-CAP allot
create ERR IO-CAP allot

: CHILD$ ( -- ptr u8 n ) s" test/native-window-owner-child.f" ;

\ The optimizing tier, selected the way every other tier-1 subject selects it.
: TIER1$ ( -- ptr u8 n ) s" test/compiler/aot-mode.f" ;

: ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHILD$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   fx fxu >LEN PROC-ARGV+
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: TIER1-ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TIER1$ >LEN PROC-ARGV+
   CHILD$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   fx fxu >LEN PROC-ARGV+
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: WINDOW-IS ( ptr u8 n ptr u8 n -- ) {: fx:ptr fxu:n want:ptr wantu:n :}
   fx fxu ARGS!
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$= ;

\ ---- the same window, compiled by the OPTIMIZING tier -------------------------
\ Tier 1 is the one that reaches the checker as a CALLER: it has the checker scan
\ each definition because the scan fills the source tape it elaborates from, so
\ every question this suite is about is asked for real only here. Tier 0 reads the
\ engine's hook cell and asks nothing else, which is why the three cases above
\ pass on a compiler that resolves the checker by name.
\
\ STDERR IS HALF THE CLAIM. A TRUSTED: definition's body is scanned only for that
\ tape and its verdict is never enforced, so nothing may be rendered about it. The
\ suppression is the OWNER's, reached through the declaration record: a compiler
\ that bumped the quiet counter by name bumped the counter of the checker it was
\ compiled into while the window's checker did the rendering, and this window then
\ printed `habu: in install: at 'set-preflight'` for check-hook.f's own INSTALL --
\ measured on the engine before the fix, with the same `window: 0` on stdout. Any
\ byte here means a scan nobody judges reached a renderer again.
\
\ Its own deadline: the window's whole core prefix through the optimizing chain is
\ minutes, not seconds (measured 2m11s on a loaded box against the 3-minute bound
\ the tier-0 cases share). The bound is here to catch a hang, not to time a build.
600000 constant TIER1-DEADLINE-MS

: WINDOW-TIER1-RESULT ( ptr u8 n -- ) {: want:ptr wantu:n :}
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   TIER1-DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$=
   erru LEN>N 0 <> if ERR erru LEN>N type cr then
   erru LEN>N 0 T= ;

\ Ordinary cases stop at the checker handover. The adapter case adds the
\ source loader and layout that its real require closure needs.
: WINDOW-TIER1-IS ( ptr u8 n ptr u8 n -- ) {: fx:ptr fxu:n want:ptr wantu:n :}
   fx fxu TIER1-ARGS! want wantu WINDOW-TIER1-RESULT ;

: WINDOW-SOURCE-DEPS ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" >LEN PROC-ARGV+
      s" src/os/linux/layout.f" >LEN PROC-ARGV+
   else
      s" src/os/macos/target.f" >LEN PROC-ARGV+
      s" src/os/macos/layout.f" >LEN PROC-ARGV+
   then
   s" src/habu/layout.f" >LEN PROC-ARGV+
   s" src/core/bytes.f" >LEN PROC-ARGV+
   s" src/os/env-base.f" >LEN PROC-ARGV+
   s" src/core/include.f" >LEN PROC-ARGV+
   S\" window: 0\n" WINDOW-TIER1-RESULT ;

: WINDOW-SOURCE ( ptr u8 n -- )
   TIER1-ARGS! WINDOW-SOURCE-DEPS ;

: WINDOW-SOURCE-JIT ( ptr u8 n -- )
   ARGS! WINDOW-SOURCE-DEPS ;

: RUN ( -- )
   T-RESET
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-IS
   s" test/native-window-cast-bad.f"      S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-host-bad.f" S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-TIER1-IS
   s" test/native-window-call-store.f"    S\" window: 0\n"    WINDOW-TIER1-IS
   s" test/native-window-owner-bindings.f" WINDOW-SOURCE-JIT
   s" test/native-window-owner-adapter.f" WINDOW-SOURCE
   s" test/native-window-owner-family.f" WINDOW-SOURCE
   s" test/native-window-owner-fixed.f" WINDOW-SOURCE
   s" test/native-window-tape-detach.f" WINDOW-SOURCE
   T-REPORT
   s" native-window-owner: ok" type cr ;

RUN

;package
