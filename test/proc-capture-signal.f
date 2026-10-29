\ proc-capture-signal.f - a child capture survives a signal storm. poll(2) is
\ the one blocking syscall SA_RESTART never restarts, so each tick of the 1 kHz
\ profiler's SIGALRM (prof-on, ITIMER_REAL) returns EINTR, which process.f must
\ not raise as E-PROC-OUTPUT: a quarter-second child is some 250 EINTRs. The
\ capture must also have waited, since output and rc alone pass if the timer
\ stops arming; prof-report before prof-off shows some 260 samples, all in poll.

require src/lib/errors.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/outcome.f
require test/pcap-result.f

package PROC-CAPTURE-SIGNAL

1000000 constant PCS-SAMPLE-LIMIT  \ more samples than this capture can deliver: the limit never fires
30000 constant PCS-TIMEOUT-MS      \ far above the child's quarter second; a timeout is a failure here
200000000 constant PCS-MIN-NS      \ the child's quarter second less a margin, in nanoseconds
64 constant PCS-OUT-CAP
32 constant PCS-ERR-CAP

create PCS-OUT PCS-OUT-CAP allot
create PCS-ERR PCS-ERR-CAP allot

: PCS-MARK ( -- ptr u8 n ) s" habu-eintr-capture-ok" ;

\ /bin/sh sleeps a quarter second, then writes the marker with no trailing newline.
\ The marker is spelled out again here because the shell reads it as text; the
\ two spellings must stay identical or PCS-MARK below stops matching.
: PCS-SCRIPT ( -- ptr u8 n ) s" sleep 0.25; printf %s habu-eintr-capture-ok" ;

: PCS-RUN-SLEEPER ( -- n n n )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   PCS-SCRIPT >LEN PROC-ARGV+
   s" /bin/sh" >LEN PCS-OUT PCS-OUT-CAP >LEN PCS-ERR PCS-ERR-CAP >LEN PCS-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE PCAP-RESULT:>N ;

: CHECK-CAPTURE-UNDER-SIGNALS ( -- )
   PCS-SAMPLE-LIMIT prof-on
   mono-ns {: started :}
   PCS-RUN-SLEEPER {: outn errn code :}
   mono-ns started - {: elapsed :}
   prof-off
   code 0 T=
   errn 0 T=
   outn PCS-MARK nip T=
   PCS-OUT outn PCS-MARK T$=
   elapsed PCS-MIN-NS >= TTRUE ;

100 constant PCS-HOLD-MS             \ the held child's window
99000000 constant PCS-HOLD-MIN-NS    \ that window less the whole-millisecond floor
1000000000 constant PCS-HOLD-MAX-NS  \ ten windows: a window each tick renewed would not end

: PCS-HOLD-SCRIPT ( -- ptr u8 n )
   s" read x; exit 3" ;

\ A child that lives until its stdin closes, then exits 3: the pid and the
\ stdin's write end. Both pipe ends are close-on-exec, so closing that write end
\ releases the child.
: PCS-HOLD ( -- pid n )
   PIPE-PAIR {: in-r:fd in-w:fd :}
   in-r FD-CLOEXEC!  in-w FD-CLOEXEC!
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   PCS-HOLD-SCRIPT >LEN PROC-ARGV+
   s" /bin/sh" >LEN in-r -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
   in-r FD>N close
   in-w FD>N ;

\ The held child's window and its reap under the 1 kHz SIGALRM. The wait sleeps
\ between nonblocking waitpid probes, so a tick lands in TASK:SLEEP (which
\ resumes on its remainder), in a probe or between; no path is asserted. The
\ window must end as a timeout, neither early nor stretched, and the released
\ child is still ours to reap.
: CHECK-WAIT-UNDER-SIGNALS ( -- )
   PCS-HOLD {: pid:pid in-w:n :}
   PCS-SAMPLE-LIMIT prof-on
   mono-ns {: started :}
   pid PCS-HOLD-MS >MS PROC-WAIT-BOUNDED {: held :}
   mono-ns started - {: elapsed :}
   in-w close
   pid PCS-TIMEOUT-MS >MS PROC-WAIT-BOUNDED {: done :}
   prof-off
   held T-OUTCOME-TIMEOUT
   elapsed PCS-HOLD-MIN-NS >= TTRUE
   elapsed PCS-HOLD-MAX-NS < TTRUE
   PCS-HOLD-SCRIPT s" " s" " done 3 T-OUTCOME-EXITED= ;

: RUN ( -- )
   T-RESET
   CHECK-CAPTURE-UNDER-SIGNALS
   CHECK-WAIT-UNDER-SIGNALS
   T-REPORT
   s" proc-capture-signal: ok" type cr ;

RUN

;package
