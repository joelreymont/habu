\ chain-run.f - the checked generation-chain driver.
\
\ Run from a source snapshot with:
\   bin/hb --load tools/chain-run.f -- host gen1 gen2 gen3 HB_TMP
\
\ The host is explicit and every child inherits only the caller's environment
\ plus the private HB_TMP and engine override.  A second generation is built
\ from the first.  A third is built only when the first two differ.  The result
\ is a real exit status: a failed build or a non-fixpoint throws and cannot be
\ mistaken for a completed queue item.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-command.f

package CHAIN-RUN

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
      err OF drop s" chain-run: native-build stderr: " type
         PROC-CMD:ERR$ type cr E-BUILD-STATUS throw ENDOF
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

: SAME? ( ptr u8 n ptr u8 n -- bool ) SAME-FILES? ;

: REPORT ( n -- )
   s" chain-run: fixpoint at generation " type . cr ;

public

: MAIN ( -- )
   NEED-ARGS
   0 ARG 1 ARG 4 ARG BUILD
   0 ARG 2 ARG 4 ARG BUILD
   1 ARG 2 ARG SAME? if 2 REPORT exit then
   0 ARG 3 ARG 4 ARG BUILD
   2 ARG 3 ARG SAME? if 3 REPORT exit then
   s" chain-run: generation 3 is not a byte fixpoint" 74 die ;

;package
