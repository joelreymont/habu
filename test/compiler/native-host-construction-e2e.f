\ The child selects host construction under a foreign output target and leaves
\ a readable AOT artifact after the ordinary process-ABI launch refusal.
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/fs.f
require lib/fs-mutate.f
require test/suite-budget.f

package NATIVE-HOST-TEST
private

$8000 constant CAP
create OUT CAP allot
create ERR CAP allot
create OUTPUT FS-PATH-CAP allot
create ARTIFACT FS-PATH-CAP allot
variable OUTPUT-U
variable ARTIFACT-U

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX-X86-64? if
      s" native host construction: ARM64 fixture" type cr exit
   then
   s" HB_TMP" GETENV dup 0<> if 2dup MAKE-DIRS then 2drop
   s" native-host-construction" HB-TMP-MKDIR {: root:ptr size:n :}
   root size s" foreign-output" OUTPUT JOIN-PATH OUTPUT-U !
   root size s" construction.aot" ARTIFACT JOIN-PATH ARTIFACT-U !
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/compiler/native-host-construction.f" ARG
   s" --" ARG
   OUTPUT OUTPUT-U @ ARG
   ARTIFACT ARTIFACT-U @ ARG
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
      {: outu:len erru:len rc:n :}
   rc 0<> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0 T=
   ARTIFACT ARTIFACT-U @ FILE-SIZE 0 > TTRUE
   T-REPORT
   s" native host construction: artifact " type
   ARTIFACT ARTIFACT-U @ type cr ;

RUN
;package
