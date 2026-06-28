\ check-test.f - process-boundary smoke coverage for tools/check.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/signature-lint-core.f tools/checked-boundary-lint-core.f
\ tools/reserved-name-lint-core.f
\ tools/trust-lint-core.f tools/check-all-errors-core.f tools/argv.f
\ tools/warm-run.f tools/check-test.f

$4000 constant CKT-BUF-CAP
60000 constant CKT-TIMEOUT-MS

create CKT-ROOT FS-PATH-CAP allot
create CKT-BAD-PATH FS-PATH-CAP allot
create CKT-LIST-PATH FS-PATH-CAP allot

variable CKT-OUT-A
variable CKT-ERR-A
variable CKT-ROOT-U
variable CKT-BAD-U
variable CKT-LIST-U

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-BAD$ ( -- ptr u8 n )
   CKT-BAD-PATH CKT-BAD-U @ ;

: CKT-LIST$ ( -- ptr u8 n )
   CKT-LIST-PATH CKT-LIST-U @ ;

: CKT-OUT-A-FIELD ( -- ptr ptr u8 )
   CKT-OUT-A 0 ptr-field ;

: CKT-ERR-A-FIELD ( -- ptr ptr u8 )
   CKT-ERR-A 0 ptr-field ;

: CKT-OUT-A@ ( -- ptr u8 )
   CKT-OUT-A-FIELD @ ;

: CKT-ERR-A@ ( -- ptr u8 )
   CKT-ERR-A-FIELD @ ;

: CKT-OUT-A! ( ptr u8 -- )
   CKT-OUT-A-FIELD ! ;

: CKT-ERR-A! ( ptr u8 -- )
   CKT-ERR-A-FIELD ! ;

: CKT-ALLOC-BUF ( -- ptr u8 )
   CKT-BUF-CAP MEM-ALLOC-BYTES drop ;

: CKT-OUT ( -- ptr u8 )
   CKT-OUT-A @ 0= if CKT-ALLOC-BUF CKT-OUT-A! then
   CKT-OUT-A@ ;

: CKT-ERR ( -- ptr u8 )
   CKT-ERR-A @ 0= if CKT-ALLOC-BUF CKT-ERR-A! then
   CKT-ERR-A@ ;

: CKT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CKT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" tools/check.f" WR-TOOLS-LOAD if exit then
   s" --load"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/diag-origin-core.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-only-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/checked-boundary-lint-core.f"  >LEN PROC-ARGV+
   s" tools/reserved-name-lint-core.f"  >LEN PROC-ARGV+
   s" tools/trust-lint-core.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: CKT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CKT-STDIN-CAPTURE ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   WR-TOOLS$ >LEN src srcu >LEN CKT-OUT CKT-BUF-CAP >LEN CKT-ERR CKT-BUF-CAP >LEN
   CKT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE
   CKT-CAPTURE>N ;

: CKT-CAPTURE ( -- n n n )
   WR-TOOLS$ >LEN CKT-OUT CKT-BUF-CAP >LEN CKT-ERR CKT-BUF-CAP >LEN
   CKT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   CKT-CAPTURE>N ;

: CKT-RUN ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   src srcu CKT-STDIN-CAPTURE ;

: CKT-RUN-JSON-ALL ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   CKT-ARGV-BASE
   s" --json-errors"  >LEN PROC-ARGV+
   s" --all-errors"  >LEN PROC-ARGV+
   src srcu CKT-STDIN-CAPTURE ;

: CKT-RUN-FILE-JSON ( -- n n n )
   CKT-ARGV-BASE
   s" --json-errors"  >LEN PROC-ARGV+
   CKT-BAD$  >LEN PROC-ARGV+
   CKT-CAPTURE ;

: CKT-RUN-ARGS ( -- n n n )
   CKT-ARGV-BASE
   s" --bad-flag"  >LEN PROC-ARGV+
   CKT-CAPTURE ;

: CKT-RUN-SOURCE-LIST-PATH ( ptr u8 n -- n n n )
   CKT-ARGV-BASE
   s" --source-list"  >LEN PROC-ARGV+
    >LEN PROC-ARGV+
   CKT-CAPTURE ;

: CKT-RUN-SOURCE-LIST ( ptr u8 n -- n n n )
   CKT-LIST$ 2swap WRITE-ALL
   CKT-LIST$ CKT-RUN-SOURCE-LIST-PATH ;

: CKT-GOOD$ ( -- ptr u8 n )
   s" : CKT-OK ( i64 -- i64 ) dup * ;" ;

: CKT-BAD$SRC ( -- ptr u8 n )
   s" : CKT-BAD-WORD ( i64 -- i64 ) dup ;" ;

: CKT-RESERVED$ ( -- ptr u8 n )
   s" variable I" ;

: CKT-DIE$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-BYE ( -- ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  bye" SB-APPEND
   $22 SB-APPEND-C
   s"  5 die ;" SB-APPEND
   $0a SB-APPEND-C
   s" CKT-BYE" SB-APPEND
   SB$ ;

: CKT-UNTERM-SDQ$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-UNTERM-SDQ ( -- ptr u8 n ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" bad.f" CKT-BAD-PATH JOIN-PATH CKT-BAD-U !
   CKT-ROOT$ s" local-test.f" CKT-LIST-PATH JOIN-PATH CKT-LIST-U !
   CKT-BAD$ CKT-BAD$SRC WRITE-ALL ;

: CKT-TEST-GOOD ( -- )
   CKT-GOOD$ CKT-RUN 0 T= 0 T= 0 T= ;

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

: CKT-TEST-DIE ( -- )
   CKT-DIE$ CKT-RUN 5 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" bye" CONTAINS? TTRUE ;

: CKT-EXPECT-UNTERM-STRING ( ptr u8 n -- ) {: src:ptr srcu :}
   src srcu CKT-RUN-JSON-ALL 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-" CONTAINS? TTRUE ;

: CKT-TEST-UNTERM-STRING ( -- )
   CKT-UNTERM-SDQ$ CKT-EXPECT-UNTERM-STRING ;

: CKT-TEST-SOURCE-LIST-RESERVED ( -- )
   CKT-RESERVED$ CKT-RUN-SOURCE-LIST 1 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CKT-ERR erru s" <source-list>" CONTAINS? TTRUE ;

: CKT-TEST-SOURCE-LIST-AUDITED-LIB ( -- )
   s" lib/test.f" CKT-RUN-SOURCE-LIST-PATH 0 T=
   {: outu erru :}
   outu 0 T=
   erru 0 T= ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-PREPARE
   CKT-TEST-GOOD
   CKT-TEST-FILE-LABEL
   CKT-TEST-USAGE
   CKT-TEST-DIE
   CKT-TEST-UNTERM-STRING
   CKT-TEST-SOURCE-LIST-RESERVED
   CKT-TEST-SOURCE-LIST-AUDITED-LIB
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

CKT-MAIN
