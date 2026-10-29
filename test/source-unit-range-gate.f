\ Run the range fixture on stdin, preserving its top-level source-frame contract.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/subject.f

package SOURCE-UNIT-RANGE-GATE

$10000 constant CAP
create SOURCE CAP allot
create OUT CAP allot
create ERR CAP allot
create PROGRAM CAP allot  variable PROGRAM-U

: FIXTURE$ ( -- ptr u8 n ) s" test/source-unit-range-e2e.f" ;

\ The engine with the fixture on stdin, as a shell reruns it.
: PROGRAM$ ( -- ptr u8 n )
   0 PROGRAM-U !
   ENGINE-CANDIDATE:PATH$ PROGRAM CAP PROGRAM-U BUF-APPEND
   s"  < " PROGRAM CAP PROGRAM-U BUF-APPEND
   FIXTURE$ PROGRAM CAP PROGRAM-U BUF-APPEND
   PROGRAM PROGRAM-U @ ;

public
: MAIN ( -- )
   T-RESET
   s" source-unit ranges through stdin" T-LABEL
   FIXTURE$ SOURCE CAP FS:READ-ALL {: sourceu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN SOURCE sourceu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   PROGRAM$ OUT outu LEN>N ERR erru LEN>N rc SUBJECT:RC-TIMED-OUT
   rc 0<> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0 T=
   OUT outu LEN>N s" test: ok" CONTAINS? TTRUE
   T-REPORT ;
;package

SOURCE-UNIT-RANGE-GATE:MAIN
