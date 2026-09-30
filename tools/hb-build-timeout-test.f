\ hb-build-timeout-test.f - checked maker deadline and diagnostic fixture.
\ Run: bin/hb --load tools/hb-build-timeout-test.f

require tools/hb-build-test-lib.f

using BUILD-FIXPOINT

package HB-BUILD-CLI

\ A real stripped image stands in for the maker executable. It writes to both
\ streams before waiting, so the build's timeout path must retain both spans.
: HBT-BLOCK-MAKER$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) s\" maker-out\" type cr 2 s\" maker-err\" write drop mono-ns 10000000000 + begin dup mono-ns > while repeat drop ;\n" ;

: HBT-PREPARE-BLOCK-MAKER ( -- )
   HBT-AOT-SRC HBT-BLOCK-MAKER$ WRITE-ALL
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-AOT-OUT EXECUTABLE? TTRUE
   BF-TMP-RESET ;

: HBT-MAKER-TIMEOUT-JSON ( -- )
   HBT-ARGV-BASE
   s" 2500" HBT-TIMEOUT-ENV
   s" --json-errors" >LEN PROC-ARGV+
   HBT-ADD-TIMEOUT-ARGS
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-UNCAUGHT-RC T=
   outu 0 T=
   READER-STATE JR:STORAGE-BYTES HBT-ERR erru JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" code" JR:FIND-KEY TTRUE s" E-PROC-TIMEOUT" REPORT-STRING=
   s" maker" JR:FIND-KEY TTRUE s" aot" REPORT-STRING=
   s" source" JR:FIND-KEY TTRUE HBT-AOT-SRC REPORT-STRING=
   s" timeout_ms" JR:FIND-KEY TTRUE JR:TOKEN JR:T-INT T= JR:INT 2500 T=
   s" stdout" JR:FIND-KEY TTRUE S\" maker-out\n" REPORT-STRING=
   s" stderr" JR:FIND-KEY TTRUE s" maker-err" REPORT-STRING=
   JR:CLOSE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

\ The text form, on the REPL path. HBB-MAKER-TIMED-OUT writes it for both
\ builds and only its maker= differs; the AOT path's timeout is the JSON case.
: HBT-MAKER-TIMEOUT-REPL ( -- )
   HBT-ARGV-BASE
   s" 2500" HBT-TIMEOUT-ENV
   s" --repl" >LEN PROC-ARGV+
   HBT-ADD-TIMEOUT-ARGS
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-UNCAUGHT-RC T=
   outu 0 T=
   HBT-ERR erru s" maker-out" CONTAINS? TTRUE
   HBT-ERR erru S\" maker-err\nhb-build: code=E-PROC-TIMEOUT maker=repl" CONTAINS? TTRUE
   HBT-ERR erru s" 2500" CONTAINS? TTRUE
   HBT-ERR erru HBT-AOT-SRC CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-MAKER-TIMEOUTS ( -- )
   HBT-PREPARE-BLOCK-MAKER
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-MAKER-TIMEOUT-JSON
   HBT-MAKER-TIMEOUT-REPL ;

\ Run with the package closed, as for the other hb-build fixture rows.
public
: HBT-TIMEOUT-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-MAKER-TIMEOUTS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-timeout-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-TIMEOUT-MAIN
