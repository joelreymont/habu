\ test-runner-test.f - focused tests for lib/test-runner.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/test-runner.f
\ lib/test-runner-test.f

create GTT-OK-PATH FS-PATH-CAP allot
create GTT-FAIL-PATH FS-PATH-CAP allot
create GTT-HANG-PATH FS-PATH-CAP allot

variable GTT-OK-U
variable GTT-FAIL-U
variable GTT-HANG-U

: GTT-OK$ ( -- ptr u8 n )
   GTT-OK-PATH GTT-OK-U @ ;

: GTT-FAIL$ ( -- ptr u8 n )
   GTT-FAIL-PATH GTT-FAIL-U @ ;

: GTT-HANG$ ( -- ptr u8 n )
   GTT-HANG-PATH GTT-HANG-U @ ;

: GTT-LF ( -- )
   10 SB-APPEND-C ;

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

: GTT-PATHS! ( -- )
   s" ok.f" GTT-OK-PATH GT-PATH GTT-OK-U !
   s" fail.f" GTT-FAIL-PATH GT-PATH GTT-FAIL-U !
   s" hang.f" GTT-HANG-PATH GT-PATH GTT-HANG-U ! ;

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
   script scriptu PROC-ARGV+
   s" bin/hb" timeout GT-RUN ;

: GTT-RUN-OK ( -- )
   GTT-OK$ 1000 GTT-RUN-HB ;

: GTT-RUN-FAIL ( -- )
   GTT-FAIL$ 1000 GTT-RUN-HB ;

: GTT-RUN-HANG ( -- )
   GTT-HANG$ 50 GTT-RUN-HB ;

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

: GTT-TEST-PROGRESS ( -- )
   s" fixture progress" GT-PROGRESS-RUN
   s" fixture progress" GT-PROGRESS-PASS ;

: TEST-RUNNER-TEST-MAIN ( -- )
   T-RESET
   GTT-PREPARE
   GTT-TEST-TEMP-ROOT
   GTT-TEST-PASSING-COMMAND
   GTT-TEST-FAILING-COMMAND
   GTT-TEST-TIMEOUT
   GTT-TEST-AGGREGATE-FAILURES
   GTT-TEST-PROGRESS
   GT-CLEANUP
   GT-ROOT EXISTS? TFALSE
   T-REPORT
   GT-REPORT
   s" test-runner-test: ok" type cr ;

TEST-RUNNER-TEST-MAIN
