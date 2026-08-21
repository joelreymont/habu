\ runner-test.f - focused tests for lib/test/runner.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/test/runner.f
\ lib/test/runner-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test/runner.f

create GTT-OK-PATH FS-PATH-CAP allot
create GTT-FAIL-PATH FS-PATH-CAP allot
create GTT-HANG-PATH FS-PATH-CAP allot
create GTT-LINE-PATH FS-PATH-CAP allot
create GTT-REC-PATH FS-PATH-CAP allot
create GTT-DETAIL-PATH FS-PATH-CAP allot
128 constant GTT-LINE-CAP
5000 constant GTT-HB-TIMEOUT-MS
\ A nested child starts its own engine and then runs a child of its own on the
\ runner's default budget, so its ceiling has to outlast that budget.
GT-DEFAULT-TIMEOUT-MS GTT-HB-TIMEOUT-MS + constant GTT-NEST-TIMEOUT-MS
1000 constant GTT-CMD-TIMEOUT-MS
50 constant GTT-SHORT-TIMEOUT-MS
4 constant GTT-OVERFLOW-EXTRA
create GTT-LINE-BUF GTT-LINE-CAP allot
create GTT-READ-BUF GTT-LINE-CAP allot

variable GTT-OK-U
variable GTT-FAIL-U
variable GTT-HANG-U
variable GTT-LINE-U
variable GTT-REC-U
variable GTT-DETAIL-U
variable GTT-LINE-LEN
variable GTT-LINE-FD
variable GTT-OFI

: GTT-OK$ ( -- ptr u8 n )
   GTT-OK-PATH GTT-OK-U @ ;

: GTT-FAIL$ ( -- ptr u8 n )
   GTT-FAIL-PATH GTT-FAIL-U @ ;

: GTT-HANG$ ( -- ptr u8 n )
   GTT-HANG-PATH GTT-HANG-U @ ;

: GTT-LINE$ ( -- ptr u8 n )
   GTT-LINE-PATH GTT-LINE-U @ ;

: GTT-REC$ ( -- ptr u8 n )
   GTT-REC-PATH GTT-REC-U @ ;

: GTT-DETAIL$ ( -- ptr u8 n )
   GTT-DETAIL-PATH GTT-DETAIL-U @ ;

: GTT-LF ( -- )
   10 SB-APPEND-C ;

: GTT-LINES-PARTIAL$ ( -- ptr u8 n )
   SB-RESET
   s" alpha" SB-APPEND GTT-LF
   s" beta" SB-APPEND
   SB$ ;

: GTT-LINES-COMPLETE$ ( -- ptr u8 n )
   SB-RESET
   s" alpha" SB-APPEND GTT-LF
   s" beta" SB-APPEND GTT-LF
   SB$ ;

: GTT-ALPHA-LINE$ ( -- ptr u8 n )
   SB-RESET
   s" alpha" SB-APPEND GTT-LF
   SB$ ;

: GTT-OK-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" 97 emit 108 emit 112 emit 104 emit 97 emit 32 emit" SB-APPEND GTT-LF
   s" 98 emit 101 emit 116 emit 97 emit 10 emit" SB-APPEND GTT-LF
   SB$ ;

: GTT-FAIL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" 98 emit 97 emit 100 emit 45 emit 111 emit 117 emit 116 emit 10 emit" SB-APPEND GTT-LF
   s" create E 98 c, 97 c, 100 c, 45 c, 101 c, 114 c, 114 c, 10 c," SB-APPEND GTT-LF
   s" 2 E 8 write drop" SB-APPEND GTT-LF
   s" 0 0 7 die" SB-APPEND GTT-LF
   SB$ ;

: GTT-HANG-SRC$ ( -- ptr u8 n )
   s" : HANG ( -- ) begin again ; HANG" ;

: GTT-REC-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/test.f" SB-APPEND GTT-LF
   s" T-RESET" SB-APPEND GTT-LF
   S\" s\q record-case\q T-LABEL" SB-APPEND GTT-LF
   s" 1 2 T=" SB-APPEND GTT-LF
   SB$ ;

: GTT-REC-LINE$ ( -- ptr u8 n )
   S\" TFAIL\tassert\t1\trecord-case" ;

: GTT-LIT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   S\" s\q " SB-APPEND
   a u SB-APPEND
   S\" \q" SB-APPEND ;

\ A child that runs the fail fixture through the real runner, then asserts two
\ values that hold and three that do not, so its whole stdout witnesses what a
\ failed comparison reports and what a passed one stays silent about.
: GTT-DETAIL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/errors.f" SB-APPEND GTT-LF
   s" require lib/string.f" SB-APPEND GTT-LF
   s" require lib/fs.f" SB-APPEND GTT-LF
   s" require lib/fs-mutate.f" SB-APPEND GTT-LF
   s" require lib/process.f" SB-APPEND GTT-LF
   s" require lib/process-argv.f" SB-APPEND GTT-LF
   s" require lib/test/runner.f" SB-APPEND GTT-LF
   s" PROC-ARGV-RESET" SB-APPEND GTT-LF
   GTT-FAIL$ GTT-LIT+ s"  >LEN PROC-ARGV+" SB-APPEND GTT-LF
   s" bin/hb" GTT-LIT+ s"  GT-RUN-DEFAULT" SB-APPEND GTT-LF
   s" 7 " SB-APPEND s" rc-pass" GTT-LIT+ s"  GT-RC=" SB-APPEND GTT-LF
   s" GT-OUT$ " SB-APPEND s" out-pass" GTT-LIT+ s"  GT-STDOUT=" SB-APPEND GTT-LF
   s" 0 " SB-APPEND s" rc-detail" GTT-LIT+ s"  GT-RC=" SB-APPEND GTT-LF
   s" want-out" GTT-LIT+ s" out-detail" GTT-LIT+ s"  GT-STDOUT=" SB-APPEND GTT-LF
   s" want-err" GTT-LIT+ s" err-detail" GTT-LIT+ s"  GT-STDERR=" SB-APPEND GTT-LF
   \ Ends itself: a child that falls off the end waits on the stdin it
   \ inherited, so its cost would be the caller's stdin and not its own work.
   s" 0 0 0 die" SB-APPEND GTT-LF
   SB$ ;

\ Golden, written out rather than derived, so a change in what the runner prints
\ cannot rewrite its own expectation. The blank line after each captured stream
\ is the stream's own trailing newline plus the printer's.
: GTT-DETAIL-OUT$ ( -- ptr u8 n )
   SB-RESET
   S\" TFAIL\trunner\t1\trc-detail" SB-APPEND GTT-LF
   s" runner: expected 0" SB-APPEND GTT-LF
   s" got 7" SB-APPEND GTT-LF
   S\" TFAIL\trunner\t2\tout-detail" SB-APPEND GTT-LF
   s" runner: expected string:" SB-APPEND GTT-LF
   s" want-out" SB-APPEND GTT-LF
   s" got string:" SB-APPEND GTT-LF
   s" bad-out" SB-APPEND GTT-LF GTT-LF
   S\" TFAIL\trunner\t3\terr-detail" SB-APPEND GTT-LF
   s" runner: expected string:" SB-APPEND GTT-LF
   s" want-err" SB-APPEND GTT-LF
   s" got string:" SB-APPEND GTT-LF
   s" bad-err" SB-APPEND GTT-LF GTT-LF
   SB$ ;

: GTT-OK-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" alpha beta" SB-APPEND GTT-LF
   SB$ ;

: GTT-FAIL-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" bad-out" SB-APPEND GTT-LF
   SB$ ;

: GTT-FAIL-ERR$ ( -- ptr u8 n )
   SB-RESET
   s" bad-err" SB-APPEND GTT-LF
   SB$ ;

: GTT-ECHO-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" progress-ok" SB-APPEND GTT-LF
   SB$ ;

: GTT-LINE-BUF! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   u GTT-LINE-CAP > if E-STR-CAPACITY throw then
   a GTT-LINE-BUF u BYTE-COPY
   u GTT-LINE-LEN ! ;

: GTT-LINE-FILE-RESET ( -- )
   GTT-LINE$ SB-RESET SB$ WRITE-ALL ;

: GTT-LINE-FLUSH-LINES ( -- )
   GTT-LINE$ OPEN-APPEND-FD GTT-LINE-FD !
   GTT-LINE-FD @ GTT-LINE-BUF GTT-LINE-LEN GT-FLUSH-LINES-FD
   GTT-LINE-FD @ close ;

: GTT-LINE-FLUSH-REMAINDER ( -- )
   GTT-LINE$ OPEN-APPEND-FD GTT-LINE-FD !
   GTT-LINE-FD @ GTT-LINE-BUF GTT-LINE-LEN GT-FLUSH-REMAINDER-FD
   GTT-LINE-FD @ close ;

: GTT-LINE-FILE$ ( -- ptr u8 n )
   GTT-LINE$ GTT-READ-BUF GTT-LINE-CAP READ-ALL
   GTT-READ-BUF swap ;

: GTT-PATHS! ( -- )
   s" ok.f" GTT-OK-PATH GT-PATH GTT-OK-U !
   s" fail.f" GTT-FAIL-PATH GT-PATH GTT-FAIL-U !
   s" hang.f" GTT-HANG-PATH GT-PATH GTT-HANG-U !
   s" lines.txt" GTT-LINE-PATH GT-PATH GTT-LINE-U !
   s" record.f" GTT-REC-PATH GT-PATH GTT-REC-U !
   s" detail.f" GTT-DETAIL-PATH GT-PATH GTT-DETAIL-U ! ;

: GTT-WRITE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu WRITE-ALL ;

: GTT-PREPARE ( -- )
   s" habu-test-runner" GT-START
   GTT-PATHS!
   GTT-OK$ GTT-OK-SRC$ GTT-WRITE
   GTT-FAIL$ GTT-FAIL-SRC$ GTT-WRITE
   GTT-HANG$ GTT-HANG-SRC$ GTT-WRITE
   GTT-REC$ GTT-REC-SRC$ GTT-WRITE
   GTT-DETAIL$ GTT-DETAIL-SRC$ GTT-WRITE ;

: GTT-RUN-HB ( ptr u8 n n -- ) {: script:ptr scriptu timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" timeout GT-RUN ;

: GTT-RUN-OK ( -- )
   GTT-OK$ GTT-HB-TIMEOUT-MS GTT-RUN-HB ;

: GTT-RUN-FAIL ( -- )
   GTT-FAIL$ GTT-HB-TIMEOUT-MS GTT-RUN-HB ;

: GTT-RUN-HANG ( -- )
   GTT-HANG$ GTT-SHORT-TIMEOUT-MS GTT-RUN-HB ;

: GTT-ARGV-CAPTURE-BEGIN ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-SPAWN-ARGV-CAPTURE ;

: GTT-ARGV-STDIN-CAPTURE-BEGIN ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-SETUP-STDIN-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-SPAWN-ARGV-STDIN-CAPTURE ;

: GTT-PROGRESS-ECHO ( -- )
   PROC-ARGV-RESET
   s" progress-ok" >LEN PROC-ARGV+
   s" /bin/echo" GTT-CMD-TIMEOUT-MS GTT-ARGV-CAPTURE-BEGIN
   s" progress capture" GT-PROGRESS-CAPTURE
   PROC-CLOSE-CAPTURE-FDS ;

: GTT-PROGRESS-FLUSH-TRUE ( -- )
   PROC-ARGV-RESET
   s" /usr/bin/true" GTT-CMD-TIMEOUT-MS GTT-ARGV-CAPTURE-BEGIN
   s" progress capture flush" GT-PROGRESS-CAPTURE-FLUSH
   PROC-CLOSE-CAPTURE-FDS ;

: GTT-PROGRESS-STDIN-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" GTT-CMD-TIMEOUT-MS GTT-ARGV-STDIN-CAPTURE-BEGIN
   s" stdin-ok" >LEN s" progress stdin" GT-PROGRESS-STDIN-CAPTURE
   PROC-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS ;

: GTT-TEST-TEMP-ROOT ( -- )
   GT-ROOT DIR? TTRUE
   GTT-OK$ FILE? TTRUE
   GTT-FAIL$ FILE? TTRUE
   GTT-HANG$ FILE? TTRUE ;

: GTT-TEST-PASSING-COMMAND ( -- )
   GTT-RUN-OK
   0 s" ok rc" GT-RC=
   GTT-OK-OUT$ s" ok stdout exact" GT-STDOUT=
   s" alpha" s" ok stdout contains alpha" GT-STDOUT-HAS
   s" beta" s" ok stdout contains beta" GT-STDOUT-HAS
   s" " s" ok stderr empty" GT-STDERR=
   GT-FAILURES 0 T= ;

: GTT-TEST-FAILING-COMMAND ( -- )
   GTT-RUN-FAIL
   7 s" fail rc exact" GT-RC=
   s" fail rc nonzero" GT-RC-NONZERO
   GTT-FAIL-OUT$ s" fail stdout exact" GT-STDOUT=
   s" bad-out" s" fail stdout contains" GT-STDOUT-HAS
   GTT-FAIL-ERR$ s" fail stderr exact" GT-STDERR=
   s" bad-err" s" fail stderr contains" GT-STDERR-HAS
   GT-FAILURES 0 T= ;

: GTT-TEST-TIMEOUT ( -- )
   GTT-RUN-HANG
   s" hang timeout" GT-TIMEOUT
   s" hang nonzero" GT-RC-NONZERO
   GT-FAILURES 0 T= ;

: GTT-TEST-AGGREGATE-FAILURES ( -- )
   GTT-RUN-OK
   1 s" deliberate rc mismatch" GT-RC=
   GT-FAILURES 1 T=
   0 GT-FAIL-NAME$ s" deliberate rc mismatch" T$=
   s" deliberate mismatch" GT-FAIL+
   GT-FAILURES 2 T=
   1 GT-FAIL-NAME$ s" deliberate mismatch" T$=
   GT-RESET
   GT-FAILURES 0 T= ;

: GTT-RUN-REC ( -- )
   GTT-REC$ GTT-HB-TIMEOUT-MS GTT-RUN-HB ;

: GTT-TEST-FAIL-RECORD ( -- )
   GTT-RUN-REC
   0 s" record rc" GT-RC=
   GTT-REC-LINE$ s" record tsv line" GT-STDOUT-HAS
   GT-FAILURES 0 T= ;

: GTT-RUN-DETAIL ( -- )
   GTT-DETAIL$ GTT-NEST-TIMEOUT-MS GTT-RUN-HB ;

: GTT-TEST-CHECK-DETAIL ( -- )
   GTT-RUN-DETAIL
   0 s" detail rc" GT-RC=
   GT-ERR$ s" " T$=
   GT-OUT$ GTT-DETAIL-OUT$ T$=
   GT-FAILURES 0 T= ;

: GTT-FILL-FAILS ( -- )
   GT-RESET
   0 GTT-OFI !
   begin GTT-OFI @ GT-FAIL-MAX GTT-OVERFLOW-EXTRA + < while
      s" overflow-case" GT-FAIL+
      GTT-OFI @ 1+ GTT-OFI !
   repeat ;

: GTT-TEST-FAIL-OVERFLOW ( -- )
   GTT-FILL-FAILS
   GT-FAILURES GT-FAIL-MAX GTT-OVERFLOW-EXTRA + T=
   GT-FAIL-STORED GT-FAIL-MAX T=
   0 GT-FAIL-NAME$ s" overflow-case" T$=
   GT-FAIL-MAX 1- GT-FAIL-NAME$ s" overflow-case" T$=
   GT-RESET
   GT-FAILURES 0 T=
   GT-FAIL-STORED 0 T= ;

: GTT-TEST-LINE-FLUSH-U ( -- )
   s" abc" GT-LINE-FLUSH-U 0 T=
   GTT-LINES-PARTIAL$ GT-LINE-FLUSH-U 6 T=
   GTT-LINES-COMPLETE$ GT-LINE-FLUSH-U 11 T= ;

: GTT-TEST-LINE-FLUSH-FD ( -- )
   GTT-LINE-FILE-RESET
   GTT-LINES-PARTIAL$ GTT-LINE-BUF!
   GTT-LINE-FLUSH-LINES
   GTT-LINE-LEN @ 4 T=
   GTT-LINE-BUF GTT-LINE-LEN @ s" beta" T$=
   GTT-LINE-FILE$ GTT-ALPHA-LINE$ T$=
   GTT-LINE-FLUSH-REMAINDER
   GTT-LINE-LEN @ 0 T=
   GTT-LINE-FILE$ GTT-LINES-PARTIAL$ T$= ;

: GTT-TEST-PROGRESS ( -- )
   s" fixture progress" GT-PROGRESS-RUN
   GT-PROGRESS-LAST-NS @ GT-HEARTBEAT-MS PROC-NS-PER-MS * -
   GT-PROGRESS-LAST-NS !
   s" fixture progress" GT-PROGRESS-WAIT
   s" fixture progress" GT-PROGRESS-PASS ;

: GTT-TEST-PROGRESS-CAPTURE ( -- )
   GTT-PROGRESS-ECHO
   GT-OUT$ GTT-ECHO-OUT$ T$=
   GT-ERR$ s" " T$=
   GT-RC@ 0 T= ;

: GTT-TEST-PROGRESS-CAPTURE-FLUSH ( -- )
   GTT-PROGRESS-FLUSH-TRUE
   GT-RC@ 0 T=
   GT-OUT$ s" " T$=
   GT-ERR$ s" " T$= ;

: GTT-TEST-PROGRESS-STDIN-CAPTURE ( -- )
   GTT-PROGRESS-STDIN-CAT
   GT-OUT$ s" stdin-ok" T$=
   GT-ERR$ s" " T$=
   GT-RC@ 0 T= ;

: TEST-RUNNER-TEST-MAIN ( -- )
   T-RESET
   GTT-PREPARE
   GTT-TEST-TEMP-ROOT
   GTT-TEST-PASSING-COMMAND
   GTT-TEST-FAILING-COMMAND
   GTT-TEST-TIMEOUT
   GTT-TEST-AGGREGATE-FAILURES
   GTT-TEST-FAIL-RECORD
   GTT-TEST-CHECK-DETAIL
   GTT-TEST-FAIL-OVERFLOW
   GTT-TEST-LINE-FLUSH-U
   GTT-TEST-LINE-FLUSH-FD
   GTT-TEST-PROGRESS
   GTT-TEST-PROGRESS-CAPTURE
   GTT-TEST-PROGRESS-CAPTURE-FLUSH
   GTT-TEST-PROGRESS-STDIN-CAPTURE
   GT-CLEANUP
   GT-ROOT EXISTS? TFALSE
   T-REPORT
   GT-REPORT
   s" test-runner-test: ok" type cr ;

TEST-RUNNER-TEST-MAIN
