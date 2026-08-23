\ maki/eval/launch-transport-test.f - launch-child transport boundaries.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require maki/eval/device.f

package EVAL

create EDFT-PATH-MAX FS-PATH-CAP allot
$100 constant EDFT-LAUNCH-CAP
create EDFT-LAUNCH EDFT-LAUNCH-CAP allot
variable EDFT-LAUNCH-U

: EDFT-FILL ( ptr u8 n n -- ) {: dst:ptr u:n c:n :}
   0 begin dup u < while
      c dst over + c!
      1+
   repeat drop ;

: EDFT-LAUNCH$ ( -- ptr u8 n )
   EDFT-LAUNCH EDFT-LAUNCH-U @ ;

: EDFT-ARGV$ ( n -- ptr u8 n ) {: idx:n :}
   idx >IDX PROC-ARGV-SLOT @ dup ZLEN ;

: EDFT-READ-LAUNCH ( -- )
   MAKI-GRADE:LAUNCH$ EDFT-LAUNCH EDFT-LAUNCH-CAP READ-ALL
   EDFT-LAUNCH-U ! ;

: EDFT-ASSERT-LAUNCH ( -- )
   EDFT-READ-LAUNCH
   EDFT-LAUNCH$ S\" 0 SCRIPT-ARGV$ EVAL:LAUNCH-CUBIN!\nEVAL:LAUNCH-EXIT\n" T$=
   EDFT-LAUNCH$ MAKI-GRADE:CUBIN$ CONTAINS? TFALSE ;

: EDFT-ASSERT-ARGV ( -- )
   PROC-ARGV-N @ COUNT>N 5 T=
   1 EDFT-ARGV$ s" --load" T$=
   2 EDFT-ARGV$ s" maki/eval/device.f" T$=
   3 EDFT-ARGV$ MAKI-GRADE:LAUNCH$ T$=
   4 EDFT-ARGV$ s" --" T$=
   5 EDFT-ARGV$ MAKI-GRADE:CUBIN$ T$= ;

: EDFT-ASSERT-STORED ( -- )
   ED-LCUBIN$ EDFT-PATH-MAX FS-PATH-CAP T$= ;

: EDFT-ASSERT-TRANSPORT ( -- )
   EDFT-ASSERT-STORED
   EDFT-ASSERT-LAUNCH
   EDFT-ASSERT-ARGV ;

: EDFT-NEGATIVE ( -- )
   EDFT-PATH-MAX -1 LAUNCH-CUBIN! ;

: EDFT-OVER-CAP ( -- )
   EDFT-PATH-MAX FS-PATH-CAP 1+ LAUNCH-CUBIN! ;

: EDFT-BOUNDARY ( -- )
   EDFT-PATH-MAX 0 LAUNCH-CUBIN!
   ED-LCUBIN$ nip 0 T=
   EDFT-PATH-MAX FS-PATH-CAP [char] x EDFT-FILL
   EDFT-PATH-MAX FS-PATH-CAP LAUNCH-CUBIN!
   ED-LCUBIN$ EDFT-PATH-MAX FS-PATH-CAP T$= ;

: EDFT-REJECTS ( -- )
   [: EDFT-NEGATIVE ;] E-FS-PATH TTHROWSQ
   EDFT-ASSERT-TRANSPORT
   [: EDFT-OVER-CAP ;] E-FS-PATH TTHROWSQ
   EDFT-ASSERT-TRANSPORT ;

: EDFT-TRANSPORT ( -- )
   EDFT-BOUNDARY
   s" habu-grade-launch-transport" MAKI-GRADE:PREPARE
   GRADE-WRITE-LAUNCHER
   GRADE-LAUNCH-ARGV
   EDFT-ASSERT-TRANSPORT
   EDFT-REJECTS
   PROC-ARGV-RESET
   MAKI-GRADE:CLEAN ;

: EDFT-RUN ( -- )
   T-RESET
   EDFT-TRANSPORT
   T-REPORT ;

EDFT-RUN

;package
