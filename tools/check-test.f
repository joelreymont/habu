\ check-test.f - process-boundary smoke coverage for tools/check.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/diag-origin-core.f tools/argv.f tools/check-test.f

$4000 constant CKT-BUF-CAP
10000 constant CKT-TIMEOUT-MS

create CKT-OUT CKT-BUF-CAP allot
create CKT-ERR CKT-BUF-CAP allot
create CKT-ROOT FS-PATH-CAP allot
create CKT-BAD-PATH FS-PATH-CAP allot
create CKT-LIST-PATH FS-PATH-CAP allot

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

: CKT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CKT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/diag-origin-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: CKT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CKT-STDIN-CAPTURE ( ptr u8 n -- n n n ) {: src:ptr srcu :}
   s" bin/hb" >LEN src srcu >LEN CKT-OUT CKT-BUF-CAP >LEN CKT-ERR CKT-BUF-CAP >LEN
   CKT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE
   CKT-CAPTURE>N ;

: CKT-CAPTURE ( -- n n n )
   s" bin/hb" >LEN CKT-OUT CKT-BUF-CAP >LEN CKT-ERR CKT-BUF-CAP >LEN
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
   s" : OK ( i64 -- i64 ) dup * ;" ;

: CKT-BAD$SRC ( -- ptr u8 n )
   s" : BAD ( i64 -- i64 ) dup ;" ;

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

: CKT-UNTERM-SDQ$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD ( -- ptr u8 n ) s" SB-APPEND
   34 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-UNTERM-CQ$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD ( -- ptr u8 ) c" SB-APPEND
   34 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-UNTERM-DOTQ$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD ( -- ) ." SB-APPEND
   34 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-LOCAL-TRUSTED$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: LOCAL-TEST ( -- ) ;" SB-APPEND
   10 SB-APPEND-C
   s" : OK ( -- ) LOCAL-TEST ;" SB-APPEND
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

: CKT-TEST-UNTERM-STRINGS ( -- )
   CKT-UNTERM-SDQ$ CKT-EXPECT-UNTERM-STRING
   CKT-UNTERM-CQ$ CKT-EXPECT-UNTERM-STRING
   CKT-UNTERM-DOTQ$ CKT-EXPECT-UNTERM-STRING ;

: CKT-TEST-SOURCE-LIST-LOCAL-TRUST ( -- )
   CKT-LOCAL-TRUSTED$ CKT-RUN-SOURCE-LIST 0 T<>
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" UNMANIFESTED" CONTAINS? TTRUE
   CKT-ERR erru s" LOCAL-TEST" CONTAINS? TTRUE ;

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
   CKT-TEST-UNTERM-STRINGS
   CKT-TEST-SOURCE-LIST-LOCAL-TRUST
   CKT-TEST-SOURCE-LIST-AUDITED-LIB
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

CKT-MAIN
