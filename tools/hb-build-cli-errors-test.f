\ hb-build-cli-errors-test.f - checked fixture for tools/hb-build-lib.f: the
\ errors the hb-build CLI reports - a cache root that is a file, as JSON and as
\ text, and a MAIN whose effect breaks the application contract.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-cli-errors-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

\ The quoted-path fixture below owns the writer's bytes; json-write holds none.
6 constant HBT-ESCAPE-MAX
FS-PATH-CAP HBT-ESCAPE-MAX * 2 + constant HBT-QUOTE-CAP
HBT-QUOTE-CAP BUFFER: HBT-QUOTE-BUF
TYPED-VARIABLE HBT-QUOTE-WRITER JSON-WRITE:writer

: HBT-BAD-CACHE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-BAD-OUT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-ADD-PATH-ERROR ( -- )
   s" --json-errors" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-ADD-PATH-ERROR-TEXT ( -- )
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-PATH-ERROR-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH cache_selected=true cache_root=" SB-APPEND
   HBT-QUOTE-WRITER HBT-QUOTE-BUF HBT-QUOTE-CAP JSON-WRITE:OPEN
   HBT-BAD-OUT JSON-WRITE:STRING JSON-WRITE:$ SB-APPEND
   s"  cache_source=explicit cause=E-FS-DIR" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: CHECK-PATH-JSON ( -- )
   HBT-ARGV-BASE
   HBT-BAD-CACHE-ENV
   HBT-ADD-PATH-ERROR
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-BUILD-RC T=
   outu 0 T=
   READER-STATE JR:STORAGE-BYTES HBT-ERR erru JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" schema" JR:FIND-KEY TTRUE
   s" hb-build-error" REPORT-STRING=
   s" version" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 1 T=
   s" code" JR:FIND-KEY TTRUE
   s" E-BUILD-PATH" REPORT-STRING=
   s" cache_selected" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-TRUE T=
   s" cache_root" JR:FIND-KEY TTRUE
   HBT-BAD-OUT REPORT-STRING=
   s" cache_source" JR:FIND-KEY TTRUE
   s" explicit" REPORT-STRING=
   s" cause" JR:FIND-KEY TTRUE
   s" E-FS-DIR" REPORT-STRING=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: HBT-CHECK-PATH-TEXT ( -- )
   HBT-ARGV-BASE
   HBT-BAD-CACHE-ENV
   HBT-ADD-PATH-ERROR-TEXT
   HBT-RUN-HB-BUILD {: text-outu:n text-erru:n text-rc:n :}
   text-rc HBB-BUILD-RC T=
   text-outu 0 T=
   HBT-ERR text-erru HBT-PATH-ERROR-TEXT$ T$= ;

: CLI-PATH-ERROR ( -- )
   HBT-BAD-OUT s" cache path file" WRITE-ALL
   CHECK-PATH-JSON
   HBT-CHECK-PATH-TEXT
   HBT-BAD-OUT REMOVE-FILE ;

\ MAIN must satisfy the application's empty input/output contract at build time.
: HBT-REFUSE-MAIN ( ptr u8 n -- )
   HBT-REPL-BAD-SRC 2swap WRITE-ALL
   HBT-ARGV-BASE
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   HBT-ERR erru s" in enter" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-BAD-MAIN-EFFECTS ( -- )
   s" : MAIN ( n -- n ) 1+ ;" HBT-REFUSE-MAIN
   s" : MAIN ( -- n ) 7 ;" HBT-REFUSE-MAIN ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-CLI-ERRORS-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   CLI-PATH-ERROR
   HBT-BAD-MAIN-EFFECTS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-cli-errors-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-CLI-ERRORS-MAIN
