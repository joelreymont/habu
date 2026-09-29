\ native-window-owner-lib.f - the fixture the native window owner gate rows
\ share. Loaded by test/native-window-owner.f, test/native-window-source.f,
\ test/native-window-boundary.f and test/native-window-payload.f: the
\ whitebox engine the children run on, the child's argv, the tier-1 verdict and
\ its stderr claim, the source closure the source cases load, and the row
\ driver. It runs nothing; each row file reopens package NW-OWNER-TEST and runs
\ the cases it owns.

require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package NW-OWNER-TEST

$4000 constant IO-CAP

create OUT IO-CAP allot
create ERR IO-CAP allot

\ Every child here reopens the engine's native build window, which the sealed
\ product refuses: `hb: internal engine word: DECLARATIONS`, exit 70. So they
\ all run on the engine test/whitebox-child.f names, under the row's own tag.
: PREPARE ( ptr u8 n -- )
   CLEANUP-RESET
   WHITEBOX-CHILD:PROVIDE ;

: CHILD$ ( -- ptr u8 n ) s" test/native-window-owner-child.f" ;

\ The optimizing tier, selected the way every other tier-1 subject selects it.
: TIER1$ ( -- ptr u8 n ) s" test/compiler/aot-mode.f" ;

: TIER1-ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TIER1$ >LEN PROC-ARGV+
   CHILD$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   fx fxu >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV! ;

\ ---- the same window, compiled by the OPTIMIZING tier -------------------------
\ Tier 1 is the one that reaches the checker as a CALLER: it has the checker scan
\ each definition because the scan fills the source tape it elaborates from, so
\ every question these suites are about is asked for real only here. Tier 0 reads
\ the engine's hook cell and asks nothing else, which is why the three tier-0
\ cases in test/native-window-owner.f pass on a compiler that resolves the
\ checker by name.
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
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   TIER1-DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$=
   erru LEN>N 0 <> if ERR erru LEN>N type cr then
   erru LEN>N 0 T= ;

\ Ordinary cases stop at the checker handover. A source case adds the source
\ loader and layout that its real require closure needs.
: WINDOW-SOURCE-DEPS ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" >LEN PROC-ARGV+
      s" src/os/linux/layout.f" >LEN PROC-ARGV+
   else
      s" src/os/macos/target.f" >LEN PROC-ARGV+
      s" src/os/macos/layout.f" >LEN PROC-ARGV+
   then
   s" src/habu/stack-abi.f" >LEN PROC-ARGV+
   s" src/habu/layout.f" >LEN PROC-ARGV+
   s" src/core/bytes.f" >LEN PROC-ARGV+
   s" src/os/env-base.f" >LEN PROC-ARGV+
   s" src/core/include.f" >LEN PROC-ARGV+
   S\" window: 0\n" WINDOW-TIER1-RESULT ;

: WINDOW-SOURCE ( ptr u8 n -- )
   TIER1-ARGS! WINDOW-SOURCE-DEPS ;

\ A row's cases, with the engine root PREPARE registered removed whether they
\ pass or throw.
: RUN-CASES ( [ -- ] -- )
   T-RESET
   [: CLEANUP-RUN ;] finally
   T-REPORT ;

;package
