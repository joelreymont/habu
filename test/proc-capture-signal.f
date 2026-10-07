\ proc-capture-signal.f - a child capture must survive a signal storm.
\
\ poll(2) is the one blocking syscall SA_RESTART never restarts, so every timer
\ tick delivered while the capture loop waits returns EINTR. A primitive that
\ collapses every poll error to -1 makes lib/process.f read that EINTR as a real
\ failure and raise E-PROC-OUTPUT; with the 1 kHz sampling profiler armed the
\ capture died after one millisecond instead of returning the child's output.
\ This arms that exact 1 kHz SIGALRM interval (prof-on drives ITIMER_REAL at the
\ 1 ms default rate) around a quarter-second child and asserts the capture still
\ reports a clean exit and the child's whole output. A quarter second is some
\ 250 EINTRs, any one of which the old primitive failed on.
\
\ It also asserts the capture WAITED. Output and rc alone would still pass if the
\ timer silently stopped arming, and the failure this pins is a fast one: the
\ capture used to come back in about a millisecond, not in a quarter second.
\ Re-verify the signal storm itself by putting prof-report before the prof-off
\ below: the quarter-second child reads some 260 samples, every one in `poll`.
\
\ The same storm runs through PROC-WAIT-BOUNDED's window on a child held alive.
\ That wait sleeps between nonblocking waitpid probes, so a tick lands in
\ TASK:SLEEP, which resumes on its remainder, in a probe or between the two;
\ no path a tick took is asserted. The window must end as a timeout, neither
\ early nor stretched, and the child released afterwards is still ours to reap.
\ Run: bin/hb --load test/proc-capture-signal.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f
require lib/test/outcome.f

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

: PCS-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PCS-RUN-SLEEPER ( -- n n n )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   PCS-SCRIPT >LEN PROC-ARGV+
   s" /bin/sh" >LEN PCS-OUT PCS-OUT-CAP >LEN PCS-ERR PCS-ERR-CAP >LEN PCS-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE PCS-CAPTURE>N ;

\ The capture runs with a 1 kHz SIGALRM landing in it throughout.
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

\ The held child's window and then its reap, with the 1 kHz SIGALRM landing in
\ both throughout.
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
