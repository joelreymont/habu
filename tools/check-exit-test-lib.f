\ check-exit-test-lib.f - run a program under tools/check.f and assert it reached
\ its own exit. A check.f refusal exits 1, 69, 70 or 78 and its usage exits 64,
\ which a program's own 64 could hide, so a case also fails on check.f's
\ `check.f:` prefix or its usage line in either stream.
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/suite-budget.f

package CHECK-EXIT
private
$8000 constant IO-CAP
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: SAYS? ( ptr u8 n -- bool ) {: text:ptr textu:n :}
   OUT OUT-U @ text textu CONTAINS? ERR ERR-U @ text textu CONTAINS? or ;
public
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ src under tools/check.f from the working directory, the repository root a
\ gate row runs in; its streams land in OUT$ and ERR$. HABU_ALLOW_BOOTSTRAP is
\ pinned so a shell that exports it cannot turn tools/ddc-drive.f's refusal
\ into a recovery-host run inside the row.
: RUN ( ptr u8 n -- outcome ) {: src:ptr srcu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" ARG s" tools/check.f" ARG s" --" ARG src srcu ARG
   s" HABU_ALLOW_BOOTSTRAP" >LEN s" 0" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   oc ;

\ RUN's outcome for src ended with src's own exit, want, and neither stream
\ carries check.f's prefix or usage; a failed case prints both streams.
: WANT ( ptr u8 n outcome n -- ) {: src:ptr srcu:n oc want:n :}
   T-FAILURES {: fails:n :}
   src srcu T-LABEL
   src srcu OUT$ ERR$ oc want T-OUTCOME-EXITED=
   src srcu T-LABEL
   s" check.f:" SAYS? s" usage: tools/check.f" SAYS? or TFALSE
   T-FAILURES fails <> if OUT$ type ERR$ type then ;
;package
