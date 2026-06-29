\ checked-boundary-lint-test.f - checked fixtures for tools/checked-boundary-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/warm-run.f tools/checked-boundary-lint-core.f
\ tools/checked-boundary-lint-test.f

4096 constant CBLT-BUF-CAP
10000 constant CBLT-TIMEOUT-MS
1400 constant CBLT-LARGE-LINES

variable CBLT-ROOT-U
variable CBLT-GOOD-U
variable CBLT-BAD-U
variable CBLT-OFF-U
variable CBLT-CROSS-U
variable CBLT-LARGE-U
variable CBLT-TRUSTED-U

create CBLT-ROOT-BUF FS-PATH-CAP allot
create CBLT-GOOD-BUF FS-PATH-CAP allot
create CBLT-BAD-BUF FS-PATH-CAP allot
create CBLT-OFF-BUF FS-PATH-CAP allot
create CBLT-CROSS-BUF FS-PATH-CAP allot
create CBLT-LARGE-BUF FS-PATH-CAP allot
create CBLT-TRUSTED-BUF FS-PATH-CAP allot
create CBLT-OUT CBLT-BUF-CAP allot
create CBLT-ERR CBLT-BUF-CAP allot
create CBLT-LF-BYTE 10 c,

: CBLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CBLT-ROOT ( -- ptr u8 n )
   CBLT-ROOT-BUF CBLT-ROOT-U @ ;

: CBLT-GOOD ( -- ptr u8 n )
   CBLT-GOOD-BUF CBLT-GOOD-U @ ;

: CBLT-BAD ( -- ptr u8 n )
   CBLT-BAD-BUF CBLT-BAD-U @ ;

: CBLT-OFF ( -- ptr u8 n )
   CBLT-OFF-BUF CBLT-OFF-U @ ;

: CBLT-CROSS ( -- ptr u8 n )
   CBLT-CROSS-BUF CBLT-CROSS-U @ ;

: CBLT-LARGE ( -- ptr u8 n )
   CBLT-LARGE-BUF CBLT-LARGE-U @ ;

: CBLT-TRUSTED ( -- ptr u8 n )
   CBLT-TRUSTED-BUF CBLT-TRUSTED-U @ ;

: CBLT-LF ( -- )
   10 SB-APPEND-C ;

: CBLT-APPEND-LF ( ptr u8 n -- )
   CBLT-LF-BYTE 1 APPEND-FILE ;

: CBLT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND CBLT-LF
   s" variable RAW-CELL" SB-APPEND CBLT-LF
   s" : GOOD-CHECK-HOOK ( -- ) CHECK! ;" SB-APPEND CBLT-LF
   s" ' GOOD-CHECK-HOOK set-check" SB-APPEND CBLT-LF
   s" : GOOD ( n -- n ) dup ;" SB-APPEND CBLT-LF
   SB$ ;

: CBLT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND CBLT-LF
   s" : BAD ( n -- n ) dup ;" SB-APPEND CBLT-LF
   SB$ ;

: CBLT-OFF$ ( -- ptr u8 n )
   s" 0 set-check" ;

: CBLT-CROSS$ ( -- ptr u8 n )
   s" : CROSS-BAD ( n -- n ) dup ;" ;

: CBLT-TRUSTED$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: GOOD-HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ;" SB-APPEND CBLT-LF
   s" TRUSTED: INSTALL-HOOK ( -- ) ['] GOOD-HOOK set-check ;" SB-APPEND CBLT-LF
   s" INSTALL-HOOK" SB-APPEND CBLT-LF
   s" : GOOD ( n -- n ) dup ;" SB-APPEND CBLT-LF
   SB$ ;

: CBLT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CBLT-LARGE-LINE$ ( -- ptr u8 n )
   s" \\ boundary lint large fixture padding line 0123456789 abcdefghijklmnopqrstuvwxyz" ;

: CBLT-WRITE-LARGE ( -- )
   CBLT-LARGE s" : LARGE-OK ( n -- n ) dup ;" WRITE-ALL
   CBLT-LARGE CBLT-APPEND-LF
   CBLT-LARGE-LINES 0 ?do
      CBLT-LARGE CBLT-LARGE-LINE$ APPEND-FILE
      CBLT-LARGE CBLT-APPEND-LF
   loop ;

: CBLT-CODE$ ( -- ptr u8 n )
   s" UNCHECKED-DEFINITION" ;

: CBLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-checked-boundary-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CBLT-ROOT-BUF CBLT-ROOT-U CBLT-COPY!
   CBLT-ROOT CLEANUP-DIR+
   CBLT-ROOT s" good.f" CBLT-GOOD-BUF JOIN-PATH CBLT-GOOD-U !
   CBLT-ROOT s" bad.f" CBLT-BAD-BUF JOIN-PATH CBLT-BAD-U !
   CBLT-ROOT s" off.f" CBLT-OFF-BUF JOIN-PATH CBLT-OFF-U !
   CBLT-ROOT s" cross.f" CBLT-CROSS-BUF JOIN-PATH CBLT-CROSS-U !
   CBLT-ROOT s" large.f" CBLT-LARGE-BUF JOIN-PATH CBLT-LARGE-U !
   CBLT-ROOT s" trusted.f" CBLT-TRUSTED-BUF JOIN-PATH CBLT-TRUSTED-U !
   CBLT-GOOD CLEANUP+
   CBLT-BAD CLEANUP+
   CBLT-OFF CLEANUP+
   CBLT-CROSS CLEANUP+
   CBLT-LARGE CLEANUP+
   CBLT-TRUSTED CLEANUP+
   CBLT-GOOD CBLT-GOOD$ WRITE-ALL
   CBLT-BAD CBLT-BAD$ WRITE-ALL
   CBLT-OFF CBLT-OFF$ WRITE-ALL
   CBLT-CROSS CBLT-CROSS$ WRITE-ALL
   CBLT-TRUSTED CBLT-TRUSTED$ WRITE-ALL
   CBLT-WRITE-LARGE ;

: CBLT-CORE-SETUP ( bool -- ) {: strict:bool :}
   CHECKED-BOUNDARY-LINT-RESET
   CBLT-OUT CBLT-BUF-CAP LINT-OUT-BUFFER!
   strict UB-STRICT-BOUNDARY! ;

: CBLT-CORE-FINISH ( -- n n n n )
   [: CHECKED-BOUNDARY-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 PROC-OUTCOME-EXIT rc ;

: CBLT-RUN-CURRENT ( -- n n n n )
   LINT-FALSE CBLT-CORE-SETUP
   s" tools/checked-boundary-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/json-file.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/host-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/filemap-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/parallel-agent-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/checked-boundary-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/signature-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/signature-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/typed-local-diff-lint-test.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/stale-status-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/stale-status-lint.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/trust-lint-core.f" CHECKED-BOUNDARY-LINT-FILE
   s" tools/trust-lint.f" CHECKED-BOUNDARY-LINT-FILE
   CBLT-CORE-FINISH ;

: CBLT-RUN-CORE-FILE ( ptr u8 n bool -- n n n n ) {: path:ptr pathu:n strict:bool :}
   strict CBLT-CORE-SETUP
   path pathu CHECKED-BOUNDARY-LINT-FILE
   CBLT-CORE-FINISH ;

: CBLT-RUN-CORE-GOOD ( -- n n n n )
   CBLT-GOOD LINT-FALSE CBLT-RUN-CORE-FILE ;

: CBLT-RUN-CORE-LARGE ( -- n n n n )
   CBLT-LARGE LINT-FALSE CBLT-RUN-CORE-FILE ;

: CBLT-RUN-CORE-BAD ( -- n n n n )
   CBLT-BAD LINT-FALSE CBLT-RUN-CORE-FILE ;

: CBLT-RUN-CORE-STRICT-GOOD ( -- n n n n )
   CBLT-GOOD LINT-TRUE CBLT-RUN-CORE-FILE ;

: CBLT-RUN-CORE-STRICT-TRUSTED ( -- n n n n )
   CBLT-TRUSTED LINT-TRUE CBLT-RUN-CORE-FILE ;

: CBLT-RUN-CORE-CROSS ( -- n n n n )
   LINT-FALSE CBLT-CORE-SETUP
   CBLT-OFF CHECKED-BOUNDARY-LINT-FILE
   CBLT-CROSS CHECKED-BOUNDARY-LINT-FILE
   CBLT-CORE-FINISH ;

: CBLT-ASSERT-CLEAN ( n n n n -- ) {: outu:n erru:n kind:n code:n :}
   kind PROC-OUTCOME-EXIT T=
   code 0 T=
   CBLT-OUT outu CBLT-EMPTY$ T$=
   CBLT-ERR erru CBLT-EMPTY$ T$= ;

: CBLT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n expect:n :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: CBLT-TEST-CURRENT ( -- )
   CBLT-RUN-CURRENT CBLT-ASSERT-CLEAN ;

: CBLT-TEST-GOOD ( -- )
   CBLT-RUN-CORE-GOOD CBLT-ASSERT-CLEAN ;

: CBLT-TEST-LARGE ( -- )
   CBLT-RUN-CORE-LARGE CBLT-ASSERT-CLEAN ;

: CBLT-TEST-BAD ( -- )
   CBLT-RUN-CORE-BAD 1 CBLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   CBLT-OUT outu CBLT-CODE$ CONTAINS? TTRUE ;

: CBLT-TEST-CROSS ( -- )
   CBLT-RUN-CORE-CROSS 1 CBLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   CBLT-OUT outu s" CROSS-BAD" CONTAINS? TTRUE ;

: CBLT-TEST-STRICT ( -- )
   CBLT-RUN-CORE-STRICT-GOOD 1 CBLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   CBLT-OUT outu s" CHECKER-MUTATION" CONTAINS? TTRUE
   CBLT-OUT outu s" set-check" CONTAINS? TTRUE ;

: CBLT-TEST-STRICT-TRUSTED ( -- )
   CBLT-RUN-CORE-STRICT-TRUSTED CBLT-ASSERT-CLEAN ;

: CBLT-MAIN ( -- )
   T-RESET
   CBLT-PREPARE
   CBLT-TEST-CURRENT
   CBLT-TEST-GOOD
   CBLT-TEST-LARGE
   CBLT-TEST-BAD
   CBLT-TEST-CROSS
   CBLT-TEST-STRICT
   CBLT-TEST-STRICT-TRUSTED
   CLEANUP-RUN
   CBLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" checked-boundary-lint-test: ok" type cr ;
