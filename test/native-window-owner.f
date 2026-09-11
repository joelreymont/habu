\ native-window-owner.f - the window's checker is the sole certifier for window
\ source.
\
\ Each case runs test/native-window-owner-child.f - the tools/native-build.f
\ window reduced to the checker handover - in its own engine child, and asserts
\ the verdict that window reached for one fixture included straight after
\ src/core/check-hook.f:
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

: ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
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

: RUN ( -- )
   T-RESET
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-IS
   s" test/native-window-cast-bad.f"      S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-host-bad.f" S\" window: 7131\n" WINDOW-IS
   T-REPORT
   s" native-window-owner: ok" type cr ;

RUN

;package
