\ os-cpu-test.f - the online processor count against the host's own report.
\ Run: bin/hb --load src/lib/os-cpu-test.f
\
\ Failure modes: the sysconf name is wrong for the target, so ONLINE answers
\ another limit or throws on sysconf's -1; ONLINE disagrees with what
\ getconf _NPROCESSORS_ONLN prints on the same host.

require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/os-cpu.f

package OS-CPU-TEST

64 constant OUT-CAP
10000 constant GETCONF-MS
create OUT OUT-CAP allot
create ERR OUT-CAP allot

\ getconf's stdout, newline trimmed; a failed run answers an empty string,
\ which no number parses from, so the case fails once.
: GETCONF$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" _NPROCESSORS_ONLN" >LEN PROC-ARGV+
   s" /usr/bin/getconf" >LEN OUT OUT-CAP >LEN ERR OUT-CAP >LEN
   GETCONF-MS >MS RUN-ARGV-CAPTURE MATCH result
      ok OF PCAP-CAPTURED:UNMAKE drop LEN>N ENDOF
      err OF PCAP-FAILED:UNMAKE 2drop drop 0 ENDOF
   ;MATCH {: outu:n :}
   OUT outu TRIM ;


: MATCHES-GETCONF ( -- )
   OS-CPU:ONLINE {: count:n :}
   GETCONF$ STR>NUMBER? MATCH option
      some OF count T= ENDOF
      none OF 1 0 T= ENDOF
   ;MATCH ;


: RUN ( -- )
   T-RESET
   MATCHES-GETCONF
   T-REPORT ;

RUN
;package
