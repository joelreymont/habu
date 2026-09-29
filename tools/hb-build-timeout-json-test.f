\ hb-build-timeout-json-test.f - empty override and JSON error fixture.
\ Run: bin/hb --load tools/hb-build-timeout-json-test.f

require tools/hb-build-test-lib.f

package HB-BUILD-CLI

: HBT-EMPTY-TIMEOUT ( -- )
   HBT-ARGV-BASE
   HBT-EMPTY$ HBT-TIMEOUT-ENV
   HBT-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 <> TTRUE
   rc HBB-USAGE-RC <> TTRUE
   outu 0 T=
   erru 0 > TTRUE
   HBT-ERR erru s" HB_BUILD_TIMEOUT_MS" CONTAINS? TFALSE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-INVALID-TIMEOUT-JSON ( -- )
   HBT-ARGV-BASE
   s" 2147483648" HBT-TIMEOUT-ENV
   s" --json-errors" >LEN PROC-ARGV+
   HBT-ADD-TIMEOUT-ARGS
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-USAGE-RC T=
   outu 0 T=
   READER-STATE JR:STORAGE-BYTES HBT-ERR erru JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" code" JR:FIND-KEY TTRUE s" E-BUILD-COMMAND" REPORT-STRING=
   s" env" JR:FIND-KEY TTRUE s" HB_BUILD_TIMEOUT_MS" REPORT-STRING=
   JR:CLOSE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

public
: HBT-TIMEOUT-JSON-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-EMPTY-TIMEOUT
   HBT-INVALID-TIMEOUT-JSON
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-timeout-json-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-TIMEOUT-JSON-MAIN
