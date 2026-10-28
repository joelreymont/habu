\ The aio capture cleanup, through IMAGE-LIFECYCLE:PREPARE in child processes:
\ PREPARE runs every capture cleanup of the process that calls it, and the held
\ subject's refusal ends that process. test/aio-capture-sweep-subject.f grows
\ records and a group row, captures, and grows them again;
\ test/aio-capture-held-subject.f captures with a ticket still in a group.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package AIO-CAPTURE-SWEEP-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create SWEEP-BUF FS-PATH-CAP allot
variable SWEEP-U
create HELD-BUF FS-PATH-CAP allot
variable HELD-U

: SWEEP$ ( -- ptr u8 n ) SWEEP-BUF SWEEP-U @ ;
: HELD$ ( -- ptr u8 n ) HELD-BUF HELD-U @ ;

: PREPARE ( -- )
   SOURCE-ROOT:CURRENT$ s" aio-capture-sweep-subject.f"
      SWEEP-BUF JOIN-PATH SWEEP-U !
   SOURCE-ROOT:CURRENT$ s" aio-capture-held-subject.f"
      HELD-BUF JOIN-PATH HELD-U ! ;

: CHILD ( ptr u8 n -- len len n )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N ;

: SWEPT ( -- )
   SWEEP$ CHILD {: outu:len erru:len rc:n :}
   OUT outu LEN>N s" test: ok" CONTAINS? TTRUE
   rc 0 T= ;

: HELD-REFUSED ( -- )
   HELD$ CHILD {: outu:len erru:len rc:n :}
   ERR erru LEN>N s" aio: group holds a ticket at capture" CONTAINS? TTRUE
   rc 0 T<> ;

: RUN ( -- )
   T-RESET
   PREPARE
   SWEPT
   HELD-REFUSED
   T-REPORT ;

RUN
;package
