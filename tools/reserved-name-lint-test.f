\ reserved-name-lint-test.f - checked fixtures for reserved-name-lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/reserved-name-lint-test.f

$1000 constant RNLT-BUF-CAP
$2710 constant RNLT-TIMEOUT-MS

create RNLT-ROOT FS-PATH-CAP allot
create RNLT-GOOD FS-PATH-CAP allot
create RNLT-BAD FS-PATH-CAP allot
create RNLT-CASE FS-PATH-CAP allot
create RNLT-OUT RNLT-BUF-CAP allot
create RNLT-ERR RNLT-BUF-CAP allot

variable RNLT-ROOT-U
variable RNLT-GOOD-U
variable RNLT-BAD-U
variable RNLT-CASE-U

: RNLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: RNLT-ROOT$ ( -- ptr u8 n )
   RNLT-ROOT RNLT-ROOT-U @ ;

: RNLT-GOOD$ ( -- ptr u8 n )
   RNLT-GOOD RNLT-GOOD-U @ ;

: RNLT-BAD$ ( -- ptr u8 n )
   RNLT-BAD RNLT-BAD-U @ ;

: RNLT-CASE$ ( -- ptr u8 n )
   RNLT-CASE RNLT-CASE-U @ ;

: RNLT-LF ( -- )
   $0A SB-APPEND-C ;

: RNLT-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND RNLT-LF
   s" : LOCAL-IJ ( n n -- n ) {: i:n j:n :} i j + ;" SB-APPEND RNLT-LF
   s" variable IX" SB-APPEND RNLT-LF
   s" variable JX" SB-APPEND RNLT-LF
   SB$ ;

: RNLT-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" variable I" SB-APPEND RNLT-LF
   s" 1 constant j" SB-APPEND RNLT-LF
   s" : LOOP ( -- ) ;" SB-APPEND RNLT-LF
   SB$ ;

: RNLT-CASE-SRC$ ( -- ptr u8 n )
   s" : i ( -- n ) 1 ;" ;

: RNLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-reserved-name-lint" TMPDIR-MKDIR RNLT-ROOT RNLT-ROOT-U RNLT-COPY!
   RNLT-ROOT$ CLEANUP-TREE+
   RNLT-ROOT$ s" good.f" RNLT-GOOD JOIN-PATH RNLT-GOOD-U !
   RNLT-ROOT$ s" bad.f" RNLT-BAD JOIN-PATH RNLT-BAD-U !
   RNLT-ROOT$ s" case.f" RNLT-CASE JOIN-PATH RNLT-CASE-U !
   RNLT-GOOD$ RNLT-GOOD-SRC$ WRITE-ALL
   RNLT-BAD$ RNLT-BAD-SRC$ WRITE-ALL
   RNLT-CASE$ RNLT-CASE-SRC$ WRITE-ALL ;

: RNLT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: RNLT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/reserved-name-lint.f" WR-TOOLS-LOAD if exit then
   s" --load" RNLT-ARG+
   s" lib/errors.f" RNLT-ARG+
   s" lib/string.f" RNLT-ARG+
   s" lib/memory.f" RNLT-ARG+
   s" lib/vector.f" RNLT-ARG+
   s" lib/fs.f" RNLT-ARG+
   s" tools/lint/text.f" RNLT-ARG+
   s" tools/lint/token.f" RNLT-ARG+
   s" tools/lint/lib.f" RNLT-ARG+
   s" tools/lint/json-writer.f" RNLT-ARG+
   s" tools/lint/source-lex.f" RNLT-ARG+
   s" tools/reserved-name-lint-core.f" RNLT-ARG+
   s" tools/argv.f" RNLT-ARG+
   s" tools/reserved-name-lint.f" RNLT-ARG+
   s" --" RNLT-ARG+ ;

: RNLT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: RNLT-CAPTURE ( -- n n n n )
   WR-TOOLS$ >LEN RNLT-OUT RNLT-BUF-CAP >LEN RNLT-ERR RNLT-BUF-CAP >LEN
   RNLT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   RNLT-CAPTURE>N ;

: RNLT-RUN-GOOD ( -- n n n n )
   RNLT-ARGV-LOAD
   RNLT-GOOD$ RNLT-ARG+
   RNLT-CAPTURE ;

: RNLT-RUN-BAD ( -- n n n n )
   RNLT-ARGV-LOAD
   RNLT-BAD$ RNLT-ARG+
   RNLT-CAPTURE ;

: RNLT-RUN-JSON ( -- n n n n )
   RNLT-ARGV-LOAD
   s" --json" RNLT-ARG+
   s" --label" RNLT-ARG+
   s" <converted>" RNLT-ARG+
   RNLT-CASE$ RNLT-ARG+
   RNLT-CAPTURE ;

: RNLT-JSON-WORD-I$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" i" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: RNLT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code want :}
   kind PROC-OUTCOME-EXIT T=
   code want T=
   outu erru ;

: RNLT-TEST-GOOD ( -- )
   RNLT-RUN-GOOD 0 RNLT-EXPECT-EXIT {: outu erru :}
   outu 0 T=
   erru 0 T= ;

: RNLT-TEST-BAD ( -- )
   RNLT-RUN-BAD 1 RNLT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `I`" CONTAINS? TTRUE
   RNLT-OUT outu s" `j`" CONTAINS? TTRUE
   RNLT-OUT outu s" `LOOP`" CONTAINS? TTRUE ;

: RNLT-TEST-JSON ( -- )
   RNLT-RUN-JSON 1 RNLT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   RNLT-OUT outu s" schema_version" CONTAINS? TTRUE
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" <converted>" CONTAINS? TTRUE
   RNLT-OUT outu RNLT-JSON-WORD-I$ CONTAINS? TTRUE ;

: RNLT-MAIN ( -- )
   T-RESET
   RNLT-PREPARE
   RNLT-TEST-GOOD
   RNLT-TEST-BAD
   RNLT-TEST-JSON
   CLEANUP-RUN
   RNLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" reserved-name-lint-test: ok" type cr ;

RNLT-MAIN
