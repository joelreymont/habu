\ fd-io-test.f - exact reads and full writes over real pipes, through lib/fd-io.f.
\ Run: bin/hb --load lib/fd-io-test.f
\
\ A pipe holds far less than PAYLOAD (64 KiB by default on Linux and at most on
\ macOS), so one read of PAYLOAD bytes answers short and READ-EXACT has to loop.
\ A blocking write is different: the kernel finishes it unless a caught signal
\ lands after some bytes moved, and then it answers the count so far. The write
\ cases therefore run under the engine's 1 kHz SIGALRM profiler, with a forked
\ reader that holds back until the writer is blocked on a full pipe. Each loop
\ case first proves its premise with one raw call - a short read, a short write
\ - so a host where the premise stopped holding fails here rather than passing
\ without exercising the loop.

require lib/errors.f
require lib/test.f
require lib/span.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f
require lib/test/outcome.f
require lib/fd-io.f

package FD-IO-TEST
using FD-IO
using SPAN
using MEM

$400000 constant PAYLOAD          \ 4 MiB: 64 default Linux pipes, and past Linux's pipe-max-size
251 constant PATTERN-MOD          \ prime, so no power-of-two chunk realigns the pattern
10000 constant WAIT-MS            \ how long a held-back reader waits for the writer's first bytes
200 constant READER-DELAY-MS      \ then how long it holds back, the writer blocked and the storm on
1000000 constant STORM-LIMIT      \ profiler samples allowed; no case comes near it
-1 constant FULL-MARK             \ OUTCOME's answer for `full`
2 constant CHILD-MISMATCH         \ a forked reader's exit code when the bytes are wrong

TYPED-VARIABLE SRC SPAN:span<u8>  \ PAYLOAD bytes of the pattern
TYPED-VARIABLE DST SPAN:span<u8>  \ PAYLOAD bytes a reader fills
16 SPAN-BUFFER: SMALL
variable CHILD-FD                 \ the pipe end a forked child works on
variable CHILD-CODE               \ the exit code a child's part chose
variable ERR-FD                   \ the descriptor a refusal case uses

: OUTCOME ( FD-IO:fill -- n )     \ FULL-MARK, or the bytes before end of file
   MATCH FD-IO:fill
      full OF FULL-MARK ENDOF
      eof OF ENDOF
   ;MATCH ;

: PATTERN! ( SPAN:span<u8> -- )
   $ {: p:ptr u:n :}
   u 0 ?do i PATTERN-MOD mod p i + c! loop ;

: PATTERN? ( SPAN:span<u8> -- bool )
   $ {: p:ptr u:n :}
   u 0 ?do p i + c@ i PATTERN-MOD mod <> if false unloop exit then loop
   true ;

: CHILD-EXIT ( n -- )
   s" " rot die ;

\ The rest of a forked child's life: its part runs under catch, and the exit
\ code is 1 for a throw, else the code the part chose.
: IN-CHILD ( [ -- ] -- )
   catch {: code:n :}
   code 0<> if 1 CHILD-EXIT then
   CHILD-CODE @ CHILD-EXIT ;

: WRITER ( -- )
   CHILD-FD @ >FD SRC @ $ WRITE-FULL
   0 CHILD-CODE ! ;

\ Waits for the writer's first bytes, so the writer is about to block on a full
\ pipe, then leaves it blocked for READER-DELAY-MS while the storm lands.
: HOLD-BACK ( -- )
   CHILD-FD @ >FD WAIT-MS >MS POLL-IN-OR-TIMEOUT drop
   READER-DELAY-MS >MS TASK:SLEEP ;

: DRAIN ( -- )
   HOLD-BACK
   CHILD-FD @ >FD DST @ READ-EXACT OUTCOME drop
   0 CHILD-CODE ! ;

: DELIVERED? ( -- bool )           \ the whole pattern, then end of file
   CHILD-FD @ >FD DST @ READ-EXACT OUTCOME FULL-MARK <> if false exit then
   DST @ PATTERN? 0= if false exit then
   CHILD-FD @ >FD SMALL READ-EXACT OUTCOME 0= ;

: SLOW-READER ( -- )
   HOLD-BACK
   DELIVERED? if 0 else CHILD-MISMATCH then CHILD-CODE ! ;

\ An empty span is full before any read, so it cannot mistake read's zero for
\ end of file or block on a pipe with nothing in it; an empty write writes
\ nothing.
: EMPTY-REQUEST ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r SMALL 0 TAKE READ-EXACT OUTCOME FULL-MARK T=
   w s" " WRITE-FULL
   r 0 >MS POLL-IN COUNT>N 0 T=
   r FD>N close
   w FD>N close ;

\ End of file inside an exact read is an answer, not an error: it carries the
\ count that did arrive, and those bytes are in the span. At a boundary it is
\ `eof` after zero bytes.
: EOF-INSIDE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   w FD>N s" abc" write 3 T=
   w FD>N close
   r SMALL 8 TAKE READ-EXACT OUTCOME 3 T=
   SMALL $ drop 3 s" abc" T$=
   r SMALL READ-EXACT OUTCOME 0 T=
   r FD>N close ;

\ A child writes PAYLOAD bytes and exits. One raw read comes back short - the
\ premise - and READ-EXACT fills the rest across many more short reads; the end
\ of file then lands on the boundary.
: EXACT-OVER-SHORT-READS ( -- )
   0 DST @ FILL
   PIPE-PAIR {: r:fd w:fd :}
   w FD>N CHILD-FD !
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if r FD>N close [: WRITER ;] IN-CHILD then
   w FD>N close
   r FD>N DST @ $ read {: got:n :}
   got 0 > TTRUE
   got PAYLOAD < TTRUE
   r DST @ got SKIP READ-EXACT OUTCOME FULL-MARK T=
   DST @ PATTERN? TTRUE
   r SMALL READ-EXACT OUTCOME 0 T=
   r FD>N close
   s" forked writer" s" " s" " pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

\ The premise of WRITE-FULL's case: with the storm landing on a write blocked by
\ a full pipe, one raw write of PAYLOAD comes back short.
: SHORT-WRITE-PREMISE ( -- )
   0 DST @ FILL
   PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   STORM-LIMIT prof-on
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: DRAIN ;] IN-CHILD then
   r FD>N close
   w FD>N SRC @ $ write {: wrote:n :}
   prof-off
   w FD>N close
   wrote 0 > TTRUE
   wrote PAYLOAD < TTRUE
   s" forked reader" s" " s" " pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

\ The same storm and the same held-back reader: WRITE-FULL meets the short
\ writes the premise proved, and the reader gets every byte in order, then the
\ end of file.
: FULL-WRITE-UNDER-STORM ( -- )
   0 DST @ FILL
   PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   STORM-LIMIT prof-on
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: SLOW-READER ;] IN-CHILD then
   r FD>N close
   w SRC @ $ WRITE-FULL
   prof-off
   w FD>N close
   s" forked reader" s" " s" " pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

: READ-CLOSED ( -- )
   ERR-FD @ >FD SMALL READ-EXACT OUTCOME drop ;

: WRITE-NO-READER ( -- )
   ERR-FD @ >FD s" data" WRITE-FULL ;

: WRITE-NEGATIVE ( -- )
   ERR-FD @ >FD s" x" drop -1 WRITE-FULL ;

\ A read of a descriptor that is not open, and a write to a pipe whose reader is
\ gone (SIGPIPE held off, as a server whose client left must), are refusals by
\ name; so is a source with a negative length.
: ERRORS ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD>N close
   r FD>N ERR-FD !
   [: READ-CLOSED ;] E-FS-IO TTHROWSQ
   w FD-NOSIGPIPE!
   w FD>N ERR-FD !
   [: WRITE-NO-READER ;] E-FS-IO TTHROWSQ
   [: WRITE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   w FD>N close ;

: MAIN ( -- )
   T-RESET
   PAYLOAD BYTES-ALLOC-LEN ALLOC-SPAN SRC !
   PAYLOAD BYTES-ALLOC-LEN ALLOC-SPAN DST !
   SRC @ PATTERN!
   EMPTY-REQUEST
   EOF-INSIDE
   EXACT-OVER-SHORT-READS
   SHORT-WRITE-PREMISE
   FULL-WRITE-UNDER-STORM
   ERRORS
   SRC @ FREE-SPAN
   DST @ FREE-SPAN
   T-REPORT
   s" fd-io-test: ok" type cr ;

MAIN

;using
;using
;using
;package
