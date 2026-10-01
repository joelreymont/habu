\ chain-run.f - the checked generation-chain driver.
\
\ Run from a source snapshot with:
\   bin/hb --load tools/chain-run-build.f -- host gen1 gen2 gen3 HB_TMP
\
\ The host is explicit and every child inherits only the caller's environment
\ plus the private HB_TMP and the engine override, which names the engine that
\ runs that build.  The host builds the first generation, the first builds the
\ second, and the second builds a third only when the first two differ.  The
\ result is a real exit status: a failed build or a non-fixpoint throws or dies
\ and cannot be mistaken for a completed queue item; tools/chain-run-build.f
\ states the statuses.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-command.f

package CHAIN-RUN

public

\ The chain's failure status: a non-fixpoint, and every throw the CLI entry
\ (tools/chain-run-build.f) catches other than an expired deadline.
74 constant FAIL-RC

private

$8000 constant CMP-CAP
create CMP-A CMP-CAP allot
create CMP-B CMP-CAP allot
variable CMP-FDA
variable CMP-FDB
variable CMP-LEFT

: ARG ( n -- ptr u8 n ) SCRIPT-ARGV$ ;

: NEED-ARGS ( -- )
   SCRIPT-ARGC 5 <> if
      s" chain-run: host gen1 gen2 gen3 HB_TMP required" 64 die
   then ;

\ A native build that failed, by its exit status; its stderr names the step.
\ The build exits PROC-TIMEOUT-RC when a deadline expired in it
\ (tools/native-build-args.f), and that verdict is thrown again as
\ E-PROC-TIMEOUT so it reaches the top of the chain. Any other status is a
\ failed build.
: BUILD-FAILED ( n -- )
   s" chain-run: native-build stderr: " type PROC-CMD:ERR$ type cr
   PROC-TIMEOUT-RC = if E-PROC-TIMEOUT throw then
   E-BUILD-STATUS throw ;

: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: host:ptr hostu out:ptr outu tmp:ptr tmpu :}
   PROC-CMD:RESET
   s" --load" >LEN PROC-CMD:ARG+
   s" tools/native-build.f" >LEN PROC-CMD:ARG+
   s" --" >LEN PROC-CMD:ARG+
   out outu >LEN PROC-CMD:ARG+
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-CMD:ENV+
   s" HABU_UNDER_TEST" >LEN host hostu >LEN PROC-CMD:ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN host hostu >LEN PROC-CMD:ENV+
   host hostu >LEN 1800000 >MS PROC-CMD:RUN-RC MATCH result
      ok OF drop ENDOF
      err OF BUILD-FAILED ENDOF
   ;MATCH
   out outu EXISTS? 0= if E-BUILD-PATH throw then ;

public

: SAME-FILES? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n b:ptr bu:n :}
   a au FILE-SIZE b bu FILE-SIZE <> if false exit then
   a au FS-PATHZ open-rd CMP-FDA !
   CMP-FDA @ 0 < if E-FS-OPEN throw then
   b bu FS-PATHZ open-rd CMP-FDB !
   CMP-FDB @ 0 < if CMP-FDA @ close E-FS-OPEN throw then
   a au FILE-SIZE CMP-LEFT !
   begin CMP-LEFT @ 0 > while
      CMP-LEFT @ CMP-CAP min {: take:n :}
      CMP-FDA @ CMP-A take read {: got-a:n :}
      CMP-FDB @ CMP-B take read {: got-b:n :}
      got-a take <> got-b take <> or if
         CMP-FDA @ close CMP-FDB @ close E-FS-IO throw
      then
      CMP-A take CMP-B take STR= 0= if
         CMP-FDA @ close CMP-FDB @ close false exit
      then
      CMP-LEFT @ take - CMP-LEFT !
   repeat
   CMP-FDA @ close CMP-FDB @ close true ;

private

: REPORT ( n -- )
   s" chain-run: fixpoint at generation " type . cr ;

public

: MAIN ( -- )
   NEED-ARGS
   0 ARG 1 ARG 4 ARG BUILD
   1 ARG 2 ARG 4 ARG BUILD
   1 ARG 2 ARG SAME-FILES? if 2 REPORT exit then
   2 ARG 3 ARG 4 ARG BUILD
   2 ARG 3 ARG SAME-FILES? if 3 REPORT exit then
   s" chain-run: generation 3 is not a byte fixpoint" FAIL-RC die ;

;package
