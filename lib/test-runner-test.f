\ test-runner-test.f - focused tests for lib/test-runner.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/test-runner.f
\ lib/test-runner-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test-runner.f

create GTT-OK-PATH FS-PATH-CAP allot
create GTT-FAIL-PATH FS-PATH-CAP allot
create GTT-HANG-PATH FS-PATH-CAP allot
create GTT-LINE-PATH FS-PATH-CAP allot
128 constant GTT-LINE-CAP
5000 constant GTT-HB-TIMEOUT-MS
1000 constant GTT-CMD-TIMEOUT-MS
50 constant GTT-SHORT-TIMEOUT-MS
create GTT-LINE-BUF GTT-LINE-CAP allot
create GTT-READ-BUF GTT-LINE-CAP allot

variable GTT-OK-U
variable GTT-FAIL-U
variable GTT-HANG-U
variable GTT-LINE-U
variable GTT-LINE-LEN
variable GTT-LINE-FD

: GTT-OK$ ( -- ptr u8 n )
   GTT-OK-PATH GTT-OK-U @ ;

: GTT-FAIL$ ( -- ptr u8 n )
   GTT-FAIL-PATH GTT-FAIL-U @ ;

: GTT-HANG$ ( -- ptr u8 n )
   GTT-HANG-PATH GTT-HANG-U @ ;

: GTT-LINE$ ( -- ptr u8 n )
   GTT-LINE-PATH GTT-LINE-U @ ;

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
   s" lines.txt" GTT-LINE-PATH GT-PATH GTT-LINE-U ! ;

: GTT-WRITE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu WRITE-ALL ;

: GTT-PREPARE ( -- )
   s" habu-test-runner" GT-START
   GTT-PATHS!
   GTT-OK$ GTT-OK-SRC$ GTT-WRITE
   GTT-FAIL$ GTT-FAIL-SRC$ GTT-WRITE
   GTT-HANG$ GTT-HANG-SRC$ GTT-WRITE ;

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
