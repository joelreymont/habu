\ The keyed x86-64 fixture application delegates each emission to the full
\ engine that built it. Its own MAIN returns before this image reads stdin, so
\ callers can still run a program in the writer after the emitted child closes.
require lib/errors.f
require lib/fs.f
require lib/engine-candidate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/suite-budget.f

package NATIVE-FIXTURE-WRITE
private

$10000 constant IO-CAP
create HOST FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable HOST-U

: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;

\ This executes while tools/app-build.f loads the source on the original
\ engine. The saved writer retains that executable's path through its DATA;
\ its content key also contains the path, so a same-byte engine at another
\ pathname cannot reuse a writer holding a stale one.
: HOST! ( -- )
   ENGINE-CANDIDATE:PATH$ {: a:ptr u:n :}
   u 0 <= u FS-PATH-CAP > or if E-FS-PATH throw then
   a HOST u BYTE-COPY
   u HOST-U ! ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/native-fixture-native.f" ARG
   s" --" ARG
   SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ ARG loop ;

: CHILD ( -- )
   ARGS
   HOST$ >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc PROC-TIMEOUT-RC = if
      s" native-fixture: native build timed out" E-PROC-TIMEOUT throw then
   rc 0<> if s" native-fixture: native build failed" rc die then ;

public

: RUN ( -- )
   SCRIPT-ARGC 1 <> SCRIPT-ARGC 3 <> and if
      s" native-fixture: expected output [artifact producer]" 64 die then
   CHILD ;

' HOST!
;package
execute

: MAIN ( -- ) NATIVE-FIXTURE-WRITE:RUN ;
