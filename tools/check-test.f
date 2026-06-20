\ check-test.f - checked fixture coverage for tools/check.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/check-test.f

$4000 constant CKT-BUF-CAP
10000 constant CKT-TIMEOUT-MS

create CKT-OUT CKT-BUF-CAP allot
create CKT-ERR CKT-BUF-CAP allot
create CKT-ROOT FS-PATH-CAP allot
create CKT-BAD-PATH FS-PATH-CAP allot

variable CKT-ROOT-U
variable CKT-BAD-U

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-BAD$ ( -- ptr u8 n )
   CKT-BAD-PATH CKT-BAD-U @ ;

: CKT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/source.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: CKT-RUN ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   s" bin/hb" src srcu CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-STDIN-CAPTURE ;

: CKT-RUN-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   s" --json-errors" PROC-ARGV+
   s" bin/hb" src srcu CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-STDIN-CAPTURE ;

: CKT-RUN-STRICT-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   s" --strict-signatures" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" bin/hb" src srcu CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-STDIN-CAPTURE ;

: CKT-RUN-ALL-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   s" --json-errors" PROC-ARGV+
   s" --all-errors" PROC-ARGV+
   s" bin/hb" src srcu CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-STDIN-CAPTURE ;

: CKT-RUN-FILE-JSON ( -- n n n )
   CKT-ARGV-BASE
   s" --json-errors" PROC-ARGV+
   CKT-BAD$ PROC-ARGV+
   s" bin/hb" CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: CKT-RUN-ARGS ( -- n n n )
   CKT-ARGV-BASE
   s" --bad-flag" PROC-ARGV+
   s" bin/hb" CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CKT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: CKT-GOOD$ ( -- ptr u8 n )
   s" : OK ( i64 -- i64 ) dup * ;" ;

: CKT-BAD$SRC ( -- ptr u8 n )
   s" : BAD ( i64 -- i64 ) dup ;" ;

: CKT-NOSIG$ ( -- ptr u8 n )
   s" : NOSIG dup ;" ;

: CKT-ALL-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD1 ( i64 -- i64 ) dup ;" SB-APPEND
   10 SB-APPEND-C
   s" : BAD2 ( i64 -- ) >r ;" SB-APPEND
   SB$ ;

: CKT-PARSE-WORDS$ ( -- ptr u8 n )
   SB-RESET
   s" : DQ ( -- ) ." SB-APPEND
   34 SB-APPEND-C
   s"  ok" SB-APPEND
   34 SB-APPEND-C
   s"  ;" SB-APPEND
   10 SB-APPEND-C
   s" : CQ ( -- ptr u8 n ) c" SB-APPEND
   34 SB-APPEND-C
   s"  ok" SB-APPEND
   34 SB-APPEND-C
   s"  count ;" SB-APPEND
   SB$ ;

: CKT-DIE$ ( -- ptr u8 n )
   SB-RESET
   s" : BYE ( -- ) s" SB-APPEND
   34 SB-APPEND-C
   s"  bye" SB-APPEND
   34 SB-APPEND-C
   s"  5 die ;" SB-APPEND
   10 SB-APPEND-C
   s" BYE" SB-APPEND
   SB$ ;

: CKT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" bad.f" CKT-BAD-PATH JOIN-PATH CKT-BAD-U !
   CKT-BAD$ CKT-BAD$SRC WRITE-ALL ;

: CKT-TEST-GOOD ( -- )
   CKT-GOOD$ CKT-RUN 0 T= 0 T= 0 T= ;

: CKT-TEST-JSON-BAD ( -- )
   CKT-BAD$SRC CKT-RUN-JSON 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CKT-ERR erru s" <stdin>" CONTAINS? TTRUE ;

: CKT-TEST-STRICT ( -- )
   CKT-NOSIG$ CKT-RUN-STRICT-JSON 1 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-MISSING-SIGNATURE" CONTAINS? TTRUE ;

: CKT-TEST-ALL ( -- )
   CKT-ALL-BAD$ CKT-RUN-ALL-JSON 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" bad1" CONTAINS? TTRUE
   CKT-ERR erru s" bad2" CONTAINS? TTRUE ;

: CKT-TEST-FILE-LABEL ( -- )
   CKT-RUN-FILE-JSON 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru CKT-BAD$ CONTAINS? TTRUE ;

: CKT-TEST-USAGE ( -- )
   CKT-RUN-ARGS 64 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: CKT-TEST-PARSE-WORDS ( -- )
   CKT-PARSE-WORDS$ CKT-RUN 0 T= 0 T= 0 T= ;

: CKT-TEST-DIE ( -- )
   CKT-DIE$ CKT-RUN 5 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" bye" CONTAINS? TTRUE ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-PREPARE
   CKT-TEST-GOOD
   CKT-TEST-JSON-BAD
   CKT-TEST-STRICT
   CKT-TEST-ALL
   CKT-TEST-FILE-LABEL
   CKT-TEST-USAGE
   CKT-TEST-PARSE-WORDS
   CKT-TEST-DIE
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

CKT-MAIN
