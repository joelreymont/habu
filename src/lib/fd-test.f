\ fd-test.f - close-on-exec pipes, exact reads and full writes over real pipes,
\ a write with SIGPIPE held off, how far a tried write got, one write that never
\ waits; a temp file cut back and appended to, and a directory synced.
\ Run: bin/hb --load src/lib/fd-test.f
\ A pipe holds far less than PAYLOAD, so one read answers short and READ-EXACT
\ loops; a blocking write answers short only when a caught signal lands after
\ some bytes moved, so write cases run under the engine's 1 kHz SIGALRM profiler.
\ Each loop case proves its premise with one raw call first.

require src/lib/error.f
require src/lib/test.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/process-argv.f
require src/lib/test/ending.f
require src/lib/fd.f
require src/lib/fs-mutate.f

package FD-TEST
using FD
using SPAN
using MEM

PROCESS-SYMBOLS
FUNCTION: KQUEUE-CALL kqueue ( -- i32 ) ;FUNCTION

$400000 constant PAYLOAD          \ 4 MiB: past any default pipe and Linux's pipe-max-size
251 constant PATTERN-MOD          \ prime, so no power-of-two chunk realigns the pattern
200 constant READER-DELAY-MS      \ then how long it holds back, the writer blocked and the storm on
1000000 constant STORM-LIMIT      \ profiler samples allowed; no case comes near it
-1 constant FULL-MARK             \ OUTCOME's answer for `full`
2 constant CHILD-MISMATCH         \ a forked reader's exit code when the bytes are wrong

TYPED-VARIABLE SRC SPAN:span<u8>
TYPED-VARIABLE DST SPAN:span<u8>
16 SPAN-BUFFER: SMALL
variable CHILD-FD                 \ the pipe end a forked child works on
variable CHILD-CODE               \ the exit code a child's part chose
variable ERR-FD                   \ the descriptor a word run under catch uses
variable UNTIL-AT                 \ deadline used by caught bounded-write cases
variable FLAG-IN                  \ the SIGPIPE flag WITH-NOSIGPIPE's quotation read
variable WROTE-IN                 \ WROTE>N of a write WITH-NOSIGPIPE's quotation tried
1 TYPED-BUFFER NO-ENV ptr u8      \ a NULL slot: the exec'd child's empty environment

: OUTCOME ( FD:fill -- n )        \ FULL-MARK, or the bytes before end of file
   MATCH FD:fill
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
\ code is PROCESS:EXIT-RC's for a throw, else the code the part chose.
: IN-CHILD ( [ -- ] -- )
   catch {: code:n :}
   code 0<> if code 1 PROCESS:EXIT-RC CHILD-EXIT then
   CHILD-CODE @ CHILD-EXIT ;

: WRITER ( -- )
   CHILD-FD @ >FD SRC @ $ WRITE-FULL
   0 CHILD-CODE ! ;

\ Waits for the writer's first bytes, so the writer is about to block on a full
\ pipe, then leaves it blocked for READER-DELAY-MS while the storm lands.
: HOLD-BACK ( -- )
   CHILD-FD @ >FD -1 >MS PROCESS:POLL-IN-OR-TIMEOUT drop
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

\ Reaps a held-back reader and answers its outcome. Its writer holds off
\ SIGPIPE, closes its end and reaps the reader before it acts on its write, so
\ a write into the pipe a failed reader left fails rather than ending the row
\ before the reader's exit is read back.
: REAP-READER ( pid -- PROCESS:ending )
   PROCESS:WAIT PROCESS:STATUS>ENDING ;

: FD-FLAGS ( fd -- n )
   FD>N 3 0 fcntl dup 0 < if E-FS-IO throw then ;

: UNTIL-WRITE ( -- )
   ERR-FD @ >FD SRC @ $ UNTIL-AT @ WRITE-FULL-UNTIL ;

: FULL-WRITE ( -- )
   ERR-FD @ >FD SRC @ $ WRITE-FULL ;

: UNTIL-BROKEN ( -- )
   ERR-FD @ >FD s" broken" UNTIL-AT @ WRITE-FULL-UNTIL ;

: UNTIL-NEGATIVE ( -- )
   ERR-FD @ >FD s" x" drop -1 UNTIL-AT @ WRITE-FULL-UNTIL ;

: UNTIL-EXPIRED ( -- )
   ERR-FD @ >FD s" x" UNTIL-AT @ WRITE-FULL-UNTIL ;

\ A descriptor already nonblocking remains nonblocking. An expired request
\ writes nothing; an empty request has no transfer to time out.
: BOUNDED-EDGES ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   w FD-FLAGS {: flags:n :}
   w FD>N 4 flags FD:O-NONBLOCK or fcntl 0 T=
   w s" abc" mono-ns 1000 1000000 * + WRITE-FULL-UNTIL
   w FD-FLAGS FD:O-NONBLOCK and FD:O-NONBLOCK T=
   r SMALL 3 TAKE READ-EXACT OUTCOME FULL-MARK T=
   SMALL $ drop 3 s" abc" T$=
   w FD>N ERR-FD !
   mono-ns 1- UNTIL-AT !
   [: UNTIL-EXPIRED ;] E-FS-TIMEOUT TTHROWSQ
   [: UNTIL-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   w s" " UNTIL-AT @ WRITE-FULL-UNTIL
   r 0 >MS PROCESS:POLL-IN COUNT>N 0 T=
   w FD>N 4 flags fcntl 0 T=
   r FD>N close
   w FD>N close ;

: NONBLOCK-BROKEN ( -- )
   ERR-FD @ >FD s" x" FD:TRY-WRITE drop ;

: NONBLOCK-NEGATIVE ( -- )
   ERR-FD @ >FD s" x" drop -1 FD:TRY-WRITE drop ;

\ One write to a full non-blocking pipe answers 0 rather than waiting, and
\ moves bytes again once the reader has taken what was there.
: NONBLOCK-WRITE ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   w NONBLOCK!
   SRC @ $ {: src:ptr u:n :}
   w src u FD:TRY-WRITE {: k:n :}
   k 0 > TTRUE
   k u < TTRUE
   w src k + u k - FD:TRY-WRITE 0 T=
   r DST @ k TAKE READ-EXACT OUTCOME FULL-MARK T=
   w src k + u k - FD:TRY-WRITE 0 > TTRUE
   r FD>N close
   w FD>N ERR-FD !
   w [: [: NONBLOCK-BROKEN ;] E-FS-IO TTHROWSQ ;] WITH-NOSIGPIPE
   [: NONBLOCK-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   w FD>N close ;

\ A bounded write empties its whole payload into a slow reader in order. The
\ pipe is made nonblocking inside WRITE-FULL-UNTIL and returns to its original
\ status flags afterwards, including after the short writes in this case. Once
\ the reader is reaped, a throw from the write is rethrown.
: BOUNDED-SUCCESS ( -- )
   0 DST @ FILL
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   w FD-FLAGS {: flags:n :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: SLOW-READER ;] IN-CHILD then
   r FD>N close
   w FD:FD-NOSIGPIPE!
   w FD>N ERR-FD !
   MEM-MAX-N UNTIL-AT !    \ the largest deadline: the reader sets the pace
   [: UNTIL-WRITE ;] catch {: code:n :}
   w FD-FLAGS {: after:n :}
   w FD>N close
   pid REAP-READER {: oc :}
   code 0<> if code throw then
   after FD:O-NONBLOCK and flags FD:O-NONBLOCK and T=
   s" bounded reader" s" " s" " oc 0 T-ENDING-EXITED= ;

\ The child keeps the read end open and never reads. The deadline must stop a
\ large write after filling the pipe, with the original descriptor flags put
\ back even as the operation throws.
: NEVER-READ ( -- )
   300 >MS TASK:SLEEP
   0 CHILD-CODE ! ;

: BOUNDED-TIMEOUT ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   w FD-FLAGS {: flags:n :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: NEVER-READ ;] IN-CHILD then
   r FD>N close
   w FD>N ERR-FD !
   mono-ns 30 1000000 * + UNTIL-AT !
   [: UNTIL-WRITE ;] E-FS-TIMEOUT TTHROWSQ
   w FD-FLAGS FD:O-NONBLOCK and flags FD:O-NONBLOCK and T=
   w FD>N close
   s" nonreading child" s" " s" "
   pid PROCESS:WAIT PROCESS:STATUS>ENDING 0 T-ENDING-EXITED= ;

: BOUNDED-BROKEN ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   w FD:FD-NOSIGPIPE!
   w FD-FLAGS {: flags:n :}
   r FD>N close
   w FD>N ERR-FD !
   mono-ns 1000 1000000 * + UNTIL-AT !
   [: UNTIL-BROKEN ;] E-FS-IO TTHROWSQ
   w FD-FLAGS FD:O-NONBLOCK and flags FD:O-NONBLOCK and T=
   w FD>N close ;

\ An empty span is full before any read, so it cannot mistake read's zero for
\ end of file or block on a pipe with nothing in it; an empty write writes
\ nothing.
: EMPTY-REQUEST ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r SMALL 0 TAKE READ-EXACT OUTCOME FULL-MARK T=
   w s" " WRITE-FULL
   r 0 >MS PROCESS:POLL-IN COUNT>N 0 T=
   r FD>N close
   w FD>N close ;

\ End of file inside an exact read is an answer, not an error: it carries the
\ count that did arrive, and those bytes are in the span. At a boundary it is
\ `eof` after zero bytes.
: EOF-INSIDE ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
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
   FD:PIPE-PAIR {: r:fd w:fd :}
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
   s" forked writer" s" " s" "
   pid PROCESS:WAIT PROCESS:STATUS>ENDING 0 T-ENDING-EXITED= ;

\ The premise of WRITE-FULL's case: with the storm landing on a write blocked by
\ a full pipe, one raw write of PAYLOAD comes back short.
: SHORT-WRITE-PREMISE ( -- )
   0 DST @ FILL
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   STORM-LIMIT prof-on
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: DRAIN ;] IN-CHILD then
   r FD>N close
   w FD:FD-NOSIGPIPE!
   w FD>N SRC @ $ write {: wrote:n :}
   prof-off
   w FD>N close
   pid REAP-READER {: oc :}
   wrote 0 > TTRUE
   wrote PAYLOAD < TTRUE
   s" forked reader" s" " s" " oc 0 T-ENDING-EXITED= ;

\ The same storm and the same held-back reader: WRITE-FULL meets the short
\ writes the premise proved, and the reader gets every byte in order, then the
\ end of file.
: FULL-WRITE-UNDER-STORM ( -- )
   0 DST @ FILL
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N CHILD-FD !
   STORM-LIMIT prof-on
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if w FD>N close [: SLOW-READER ;] IN-CHILD then
   r FD>N close
   w FD:FD-NOSIGPIPE!
   w FD>N ERR-FD !
   [: FULL-WRITE ;] catch {: code:n :}
   prof-off
   w FD>N close
   pid REAP-READER {: oc :}
   code 0<> if code throw then
   s" forked reader" s" " s" " oc 0 T-ENDING-EXITED= ;

\ An append descriptor writes the next part at the end RESIZE leaves, once the
\ part written after the kept one is cut away.
: CUT-THEN-APPEND ( -- )
   s" fd-test" FS:MKTEMP {: pa:ptr pu:n made:fd :}
   made FD>N close
   pa pu FS:OPEN-APPEND-FD >FD {: f:fd :}
   f s" part-one" WRITE-FULL
   f s" partial" WRITE-FULL
   f 8 RESIZE
   f s" part-two" WRITE-FULL
   f SYNC
   f FD>N close
   pa pu SMALL $ FS:READ-ALL 16 T=
   SMALL $ s" part-onepart-two" T$=
   pa pu FS:REMOVE-FILE ;

: DIR-SYNC ( -- )
   s" fd-test" FS:MKTEMP-DIR {: pa:ptr pu:n :}
   pa pu FS:OPEN-DIR-READ {: d:n :}
   d >FD SYNC
   d FS:CLOSE-FD 0 T=
   pa pu FS:REMOVE-DIR ;

: READ-CLOSED ( -- )
   ERR-FD @ >FD SMALL READ-EXACT OUTCOME drop ;

: SYNC-CLOSED ( -- )
   ERR-FD @ >FD SYNC ;

: NONBLOCK-ERR ( -- )
   ERR-FD @ >FD NONBLOCK! ;

: WRITE-NO-READER ( -- )
   ERR-FD @ >FD s" data" WRITE-FULL ;

: WRITE-NEGATIVE ( -- )
   ERR-FD @ >FD s" x" drop -1 WRITE-FULL ;

: RESIZE-PIPE ( -- )
   ERR-FD @ >FD 0 RESIZE ;

: RESIZE-NEGATIVE ( -- )
   ERR-FD @ >FD -1 RESIZE ;

\ The rest of a forked child's life: a minute's sleep, outliving the case.
: SLEEPER ( -- )
   PROC-ARGV-RESET
   s" 60" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN PROC-ARGV-PREPARE 0 NO-ENV execve drop
   E-PROC-SPAWN throw ;

\ With the parent's write end closed, the read end reaches end of file while the
\ exec'd child runs. A write end the child kept holds the poll until the sleep
\ ends a minute later, and the child's ending is then an exit, not the kill.
: PIPE-CLOSES-ON-EXEC ( -- )
   NULL-PTR 0 NO-ENV !
   FD:PIPE {: r:fd w:fd :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if [: SLEEPER ;] IN-CHILD then
   w FD>N close
   r -1 >MS PROCESS:POLL-IN COUNT>N {: ready:n :}
   ready 1 T=
   ready 1 = if r SMALL READ-EXACT OUTCOME 0 T= then
   r FD>N close
   pid PROCESS:SIGKILL PROCESS:KILL-RAW RC>N 0 T=
   s" exec'd sleeper" s" " s" "
   pid PROCESS:WAIT PROCESS:STATUS>ENDING PROCESS:SIGKILL T-ENDING-SIGNALED= ;

\ A closed descriptor, a pipe with no reader (SIGPIPE held off, as a server's
\ must be), a resized pipe and a negative length are each refused by name.
: ERRORS ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N close
   r FD>N ERR-FD !
   [: READ-CLOSED ;] E-FS-IO TTHROWSQ
   [: SYNC-CLOSED ;] E-FS-IO TTHROWSQ
   [: NONBLOCK-ERR ;] E-FS-IO TTHROWSQ
   w FD:FD-NOSIGPIPE!
   w FD>N ERR-FD !
   [: WRITE-NO-READER ;] E-FS-IO TTHROWSQ
   [: WRITE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   [: RESIZE-PIPE ;] E-FS-IO TTHROWSQ
   [: RESIZE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   w FD>N close ;


\ fd's SIGPIPE flag: macOS keeps one per open file description, Linux none.
: NOSIGPIPE@ ( fd -- n )  FD>N F-GETNOSIGPIPE 0 fcntl ;


: HELD-FLAG ( -- )
   HB-TARGET-MACOS? if ERR-FD @ >FD NOSIGPIPE@ FLAG-IN ! then ;


: HELD-RETURN ( -- )
   ERR-FD @ >FD [: HELD-FLAG ;] WITH-NOSIGPIPE ;


: HELD-NO-READER ( -- )
   ERR-FD @ >FD [: HELD-FLAG WRITE-NO-READER ;] WITH-NOSIGPIPE ;


\ On macOS the flag was set inside the quotation and is clear again after it.
: HELD= ( fd -- )
   {: f:fd :}
   HB-TARGET-MACOS? 0= if exit then
   FLAG-IN @ 1 T=
   f NOSIGPIPE@ 0 T= ;


\ A write to a pipe with no reader inside WITH-NOSIGPIPE is E-FS-IO, not the
\ process's death, and the flag it found goes back on a return and on a throw.
: HELD-WRITE ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N close
   w FD>N ERR-FD !
   HB-TARGET-MACOS? if w NOSIGPIPE@ 0 T= then
   -1 FLAG-IN !
   HELD-RETURN
   w HELD=
   -1 FLAG-IN !
   [: HELD-NO-READER ;] E-FS-IO TTHROWSQ
   w HELD=
   w FD-NOSIGPIPE!
   HELD-RETURN
   HB-TARGET-MACOS? if w NOSIGPIPE@ 1 T= then
   w FD>N close ;


0 constant ALL-MARK
1 constant NONE-MARK
2 constant PART-MARK

: WROTE>N ( FD:wrote -- n )
   MATCH FD:wrote all OF ALL-MARK ENDOF none OF NONE-MARK ENDOF part OF PART-MARK ENDOF ;MATCH ;


: TRY-NO-READER ( -- )
   ERR-FD @ >FD s" data" TRY-WRITE-FULL WROTE>N WROTE-IN ! ;


\ TRY-WRITE-FULL answers `all` on an ordinary write, `part` once a full
\ non-blocking pipe refuses after taking bytes, and `none` once the reader is gone.
: FULL-WRITE-OUTCOMES ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   w s" abc" TRY-WRITE-FULL WROTE>N ALL-MARK T=
   r SMALL 3 TAKE READ-EXACT OUTCOME FULL-MARK T=
   SMALL $ drop 3 s" abc" T$=
   w NONBLOCK!
   w SRC @ $ TRY-WRITE-FULL WROTE>N PART-MARK T=
   r 0 >MS PROCESS:POLL-IN COUNT>N 1 T=
   r FD>N close
   w FD>N ERR-FD !
   -1 WROTE-IN !
   w [: TRY-NO-READER ;] WITH-NOSIGPIPE
   WROTE-IN @ NONE-MARK T=
   w FD>N close ;


\ XNU stores the flags before a kqueue refuses FIONBIO, so the refused mark
\ must put them back.
: REFUSED-MARK ( -- )
   HB-TARGET-MACOS? 0= if exit then
   KQUEUE-CALL {: kq:n :}
   kq >FD FD-FLAGS {: before:n :}
   kq ERR-FD !
   [: NONBLOCK-ERR ;] E-FS-IO TTHROWSQ
   kq >FD FD-FLAGS before T=
   kq close ;

: MAIN ( -- )
   T-RESET
   PAYLOAD BYTES-ALLOC-LEN ALLOC-SPAN SRC !
   PAYLOAD BYTES-ALLOC-LEN ALLOC-SPAN DST !
   SRC @ PATTERN!
   EMPTY-REQUEST
   EOF-INSIDE
   PIPE-CLOSES-ON-EXEC
   EXACT-OVER-SHORT-READS
   SHORT-WRITE-PREMISE
   FULL-WRITE-UNDER-STORM
   BOUNDED-EDGES
   NONBLOCK-WRITE
   BOUNDED-SUCCESS
   BOUNDED-TIMEOUT
   BOUNDED-BROKEN
   CUT-THEN-APPEND
   DIR-SYNC
   ERRORS
   HELD-WRITE
   FULL-WRITE-OUTCOMES
   REFUSED-MARK
   SRC @ FREE-SPAN
   DST @ FREE-SPAN
   T-REPORT
   s" fd-test: ok" type cr ;

MAIN

;using
;using
;using
;package
