\ proc-capture-signal.f - a child capture must survive a signal storm.
\
\ poll(2) is the one blocking syscall SA_RESTART never restarts, so every timer
\ tick delivered while the capture loop waits returns EINTR. A primitive that
\ collapses every poll error to -1 makes lib/process.f read that EINTR as a real
\ failure and raise E-PROC-OUTPUT; with the 1 kHz sampling profiler armed the
\ capture died after one millisecond instead of returning the child's output.
\ This arms that exact 1 kHz SIGALRM interval (prof-on drives ITIMER_REAL at the
\ 1 ms default rate) around a two-second child and asserts the capture still
\ reports a clean exit and the child's whole output.
\
\ It also asserts the capture WAITED. Output and rc alone would still pass if the
\ timer silently stopped arming, and the failure this pins is a fast one: the
\ capture used to come back in about a millisecond, not in two seconds. Re-verify
\ the signal storm itself by putting prof-report before the prof-off below; on
\ 2026-09-16 that read 1996 samples, 1996 of them inside `poll`.
\ Run: bin/hb --load test/proc-capture-signal.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f

package PROC-CAPTURE-SIGNAL

1000000 constant PCS-SAMPLE-LIMIT  \ more samples than this capture can deliver: the limit never fires
30000 constant PCS-TIMEOUT-MS      \ far above the child's two seconds; a timeout is a failure here
1900000000 constant PCS-MIN-NS     \ the child's two seconds less a margin, in nanoseconds
64 constant PCS-OUT-CAP
32 constant PCS-ERR-CAP

create PCS-OUT PCS-OUT-CAP allot
create PCS-ERR PCS-ERR-CAP allot

: PCS-MARK ( -- ptr u8 n ) s" habu-eintr-capture-ok" ;

\ /bin/sh sleeps two seconds, then writes the marker with no trailing newline.
\ The marker is spelled out again here because the shell reads it as text; the
\ two spellings must stay identical or PCS-MARK below stops matching.
: PCS-SCRIPT ( -- ptr u8 n ) s" sleep 2; printf %s habu-eintr-capture-ok" ;

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

: RUN ( -- )
   T-RESET
   CHECK-CAPTURE-UNDER-SIGNALS
   T-REPORT
   s" proc-capture-signal: ok" type cr ;

RUN

;package
