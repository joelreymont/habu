\ os-cpu-test.f - the processor facts against the host's own report.
\ Run: bin/hb --load src/lib/os-cpu-test.f
\ Failure modes: a sysconf name wrong for the target, so ONLINE answers another
\ limit or throws on -1, or disagrees with getconf _NPROCESSORS_ONLN; a getloadavg
\ binding that reads the wrong bytes or type, so LOAD1 is garbage, nonfinite or
\ negative, or disagrees with the host's one-minute average (sysctl, /proc/loadavg).

require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/ieee754.f
require src/lib/f64-text.f
require src/lib/os-cpu.f

package OS-CPU-TEST

64 constant OUT-CAP
10000 constant CAPTURE-MS
32 constant SPACE-BYTE
$7FF8000000000000 constant NAN-BITS
create OUT OUT-CAP allot
create ERR OUT-CAP allot

\ The program's stdout for the argv already staged, newline trimmed; a failed
\ run answers an empty string, which no number parses from, so the case fails
\ once.
: CAPTURE$ ( ptr u8 n -- ptr u8 n )
   >LEN OUT OUT-CAP >LEN ERR OUT-CAP >LEN
   CAPTURE-MS >MS RUN-ARGV-CAPTURE MATCH result
      ok OF PCAP-CAPTURED:UNMAKE drop LEN>N ENDOF
      err OF PCAP-FAILED:UNMAKE 2drop drop 0 ENDOF
   ;MATCH {: outu:n :}
   OUT outu TRIM ;

: GETCONF$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" _NPROCESSORS_ONLN" >LEN PROC-ARGV+
   s" /usr/bin/getconf" CAPTURE$ ;


\ macOS prints `{ 1.23 1.10 0.98 }`, Linux `1.23 1.10 0.98 2/345 6789`.
: HOST-LOAD$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   HB-TARGET-MACOS? if
      s" -n" >LEN PROC-ARGV+  s" vm.loadavg" >LEN PROC-ARGV+
      s" /usr/sbin/sysctl" CAPTURE$ exit
   then
   s" /proc/loadavg" >LEN PROC-ARGV+
   s" /bin/cat" CAPTURE$ ;

\ The host's one-minute average, its report's first number; an unreadable
\ report answers NaN, which no bound holds, so the case fails once.
: HOST-LOAD ( -- r )
   HOST-LOAD$ {: text:ptr len:n :}
   text len SPACE-BYTE 0 SPLIT-NEXT drop {: next:n :}
   HB-TARGET-MACOS? if 2drop text len SPACE-BYTE next SPLIT-NEXT drop drop then
   F64-TEXT:PARSE MATCH result
      ok OF ENDOF
      err OF drop NAN-BITS IEEE754:BITS>F64 ENDOF
   ;MATCH ;


: MATCHES-GETCONF ( -- )
   OS-CPU:ONLINE {: count:n :}
   GETCONF$ STR>NUMBER? MATCH option
      some OF count T= ENDOF
      none OF 1 0 T= ENDOF
   ;MATCH ;


\ The kernel updates the average every few seconds, so it can step between the
\ three reads; the reads before and after bracket that step. The host prints
\ two decimals, so 0.01 covers its rounding and no more: a LOAD1 that answers
\ 0.0 fails on any host whose load is above it. LOAD1 must not be negative, and
\ a NaN or infinite LOAD1 fails these comparisons, so they also prove it finite.
: MATCHES-HOST-LOAD ( -- )
   HOST-LOAD {: before:r :}
   OS-CPU:LOAD1 {: load:r :}
   HOST-LOAD {: after:r :}
   before after f< if before after else after before then {: lo:r hi:r :}
   load 0.0 f>=  load lo 0.01 f- f>= and  load hi 0.01 f+ f<= and TTRUE ;


: RUN ( -- )
   T-RESET
   MATCHES-GETCONF
   MATCHES-HOST-LOAD
   T-REPORT ;

RUN
;package
