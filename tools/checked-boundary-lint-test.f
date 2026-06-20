\ checked-boundary-lint-test.f - checked fixtures for tools/checked-boundary-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/checked-boundary-lint-test.f

4096 constant CBLT-BUF-CAP

variable CBLT-ROOT-U
variable CBLT-GOOD-U
variable CBLT-BAD-U

create CBLT-ROOT-BUF FS-PATH-CAP allot
create CBLT-GOOD-BUF FS-PATH-CAP allot
create CBLT-BAD-BUF FS-PATH-CAP allot
create CBLT-OUT CBLT-BUF-CAP allot
create CBLT-ERR CBLT-BUF-CAP allot

: CBLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CBLT-ROOT ( -- ptr u8 n )
   CBLT-ROOT-BUF CBLT-ROOT-U @ ;

: CBLT-GOOD ( -- ptr u8 n )
   CBLT-GOOD-BUF CBLT-GOOD-U @ ;

: CBLT-BAD ( -- ptr u8 n )
   CBLT-BAD-BUF CBLT-BAD-U @ ;

: CBLT-LF ( -- )
   10 SB-APPEND-C ;

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

: CBLT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CBLT-CODE$ ( -- ptr u8 n )
   s" UNCHECKED-DEFINITION" ;

: CBLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-checked-boundary-lint" TMPDIR-MKDIR {: a:ptr u :}
   a u CBLT-ROOT-BUF CBLT-ROOT-U CBLT-COPY!
   CBLT-ROOT CLEANUP-DIR+
   CBLT-ROOT s" good.f" CBLT-GOOD-BUF JOIN-PATH CBLT-GOOD-U !
   CBLT-ROOT s" bad.f" CBLT-BAD-BUF JOIN-PATH CBLT-BAD-U !
   CBLT-GOOD CLEANUP+
   CBLT-BAD CLEANUP+
   CBLT-GOOD CBLT-GOOD$ WRITE-ALL
   CBLT-BAD CBLT-BAD$ WRITE-ALL ;

: CBLT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/checked-boundary-lint.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: CBLT-RUN-CURRENT ( -- n n n )
   CBLT-ARGV-LOAD
   s" tools/checked-boundary-lint.f" PROC-ARGV+
   s" bench/llm/report.f" PROC-ARGV+
   s" bench/llm/parse-resp-lib.f" PROC-ARGV+
   s" bench/llm/parse-resp.f" PROC-ARGV+
   s" bench/llm/validate-results.f" PROC-ARGV+
   s" tools/host-lint.f" PROC-ARGV+
   s" tools/filemap-lint.f" PROC-ARGV+
   s" tools/parallel-agent-lint.f" PROC-ARGV+
   s" tools/signature-lint.f" PROC-ARGV+
   s" tools/stale-status-lint.f" PROC-ARGV+
   s" tools/trust-lint.f" PROC-ARGV+
   s" bin/hb" CBLT-OUT CBLT-BUF-CAP CBLT-ERR CBLT-BUF-CAP 1000 RUN-ARGV-CAPTURE ;

: CBLT-RUN-GOOD ( -- n n n )
   CBLT-ARGV-LOAD
   CBLT-GOOD PROC-ARGV+
   s" bin/hb" CBLT-OUT CBLT-BUF-CAP CBLT-ERR CBLT-BUF-CAP 1000 RUN-ARGV-CAPTURE ;

: CBLT-RUN-BAD ( -- n n n )
   CBLT-ARGV-LOAD
   CBLT-BAD PROC-ARGV+
   s" bin/hb" CBLT-OUT CBLT-BUF-CAP CBLT-ERR CBLT-BUF-CAP 1000 RUN-ARGV-CAPTURE ;

: CBLT-ASSERT-CLEAN ( n n n -- ) {: rc outu erru :}
   rc 0 T=
   CBLT-OUT outu CBLT-EMPTY$ T$=
   CBLT-ERR erru CBLT-EMPTY$ T$= ;

: CBLT-TEST-CURRENT ( -- )
   CBLT-RUN-CURRENT CBLT-ASSERT-CLEAN ;

: CBLT-TEST-GOOD ( -- )
   CBLT-RUN-GOOD CBLT-ASSERT-CLEAN ;

: CBLT-TEST-BAD ( -- )
   CBLT-RUN-BAD 1 T=
   {: outu erru :}
   erru 0 T=
   CBLT-OUT outu CBLT-CODE$ CONTAINS? TTRUE ;

: CBLT-MAIN ( -- )
   T-RESET
   CBLT-PREPARE
   CBLT-TEST-CURRENT
   CBLT-TEST-GOOD
   CBLT-TEST-BAD
   CLEANUP-RUN
   CBLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" checked-boundary-lint-test: ok" type cr ;

CBLT-MAIN
