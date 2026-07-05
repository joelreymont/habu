\ check-all-errors-test.f - checked fixtures for tools/check-all-errors.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/check-all-errors-core.f tools/argv.f
\ tools/check-all-errors-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require tools/argv.f

4096 constant CAE-BUF-CAP
1400 constant CAE-LARGE-LINES
530 constant CAE-MANY-DEFS
530 constant CAE-MANY-SUPPORT
32 constant CAE-NUM-CAP

variable CAE-ROOT-U
variable CAE-IN-U
variable CAE-LARGE-U
variable CAE-XSUP-PATH-U
variable CAE-NUM-I
variable CAE-RUN-A
variable CAE-RUN-U
variable CAE-RC
variable CAE-CASE-A
variable CAE-CASE-U
variable CAE-BUF-SRC-A
variable CAE-BUF-SRC-U
variable CAE-START-NS

create CAE-ROOT-BUF FS-PATH-CAP allot
create CAE-IN-BUF FS-PATH-CAP allot
create CAE-LARGE-BUF FS-PATH-CAP allot
create CAE-XSUP-PATH-BUF FS-PATH-CAP allot
create CAE-OUT CAE-BUF-CAP allot
create CAE-ERR CAE-BUF-CAP allot
create CAE-NUM CAE-NUM-CAP allot
create CAE-LF-BYTE 10 c,

: CAE-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CAE-ROOT ( -- ptr u8 n )
   CAE-ROOT-BUF CAE-ROOT-U @ ;

: CAE-IN ( -- ptr u8 n )
   CAE-IN-BUF CAE-IN-U @ ;

: CAE-LARGE ( -- ptr u8 n )
   CAE-LARGE-BUF CAE-LARGE-U @ ;

: CAE-XSUP ( -- ptr u8 n )
   CAE-XSUP-PATH-BUF CAE-XSUP-PATH-U @ ;

: CAE-RUN-A-FIELD ( -- ptr ptr u8 )
   CAE-RUN-A 0 ptr-field ;

: CAE-RUN-A@ ( -- ptr u8 )
   CAE-RUN-A-FIELD @ ;

: CAE-RUN-A! ( ptr u8 -- )
   CAE-RUN-A-FIELD ! ;

: CAE-RUN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CAE-RUN-U !
   a CAE-RUN-A! ;

: CAE-RUN$ ( -- ptr u8 n )
   CAE-RUN-A@ CAE-RUN-U @ ;

: CAE-CASE-A-FIELD ( -- ptr ptr u8 )
   CAE-CASE-A 0 ptr-field ;

: CAE-CASE-A@ ( -- ptr u8 )
   CAE-CASE-A-FIELD @ ;

: CAE-CASE-A! ( ptr u8 -- )
   CAE-CASE-A-FIELD ! ;

: CAE-CASE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CAE-CASE-A!
   u CAE-CASE-U ! ;

: CAE-CASE$ ( -- ptr u8 n )
   CAE-CASE-A@ CAE-CASE-U @ ;

: CAE-BUF-SRC-A-FIELD ( -- ptr ptr u8 )
   CAE-BUF-SRC-A 0 ptr-field ;

: CAE-BUF-SRC-A@ ( -- ptr u8 )
   CAE-BUF-SRC-A-FIELD @ ;

: CAE-BUF-SRC-A! ( ptr u8 -- )
   CAE-BUF-SRC-A-FIELD ! ;

: CAE-BUF-SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CAE-BUF-SRC-A!
   u CAE-BUF-SRC-U ! ;

: CAE-BUF-SRC$ ( -- ptr u8 n )
   CAE-BUF-SRC-A@ CAE-BUF-SRC-U @ ;

: CAE-LF ( -- )
   $0a SB-APPEND-C ;

: CAE-U$ ( n -- ptr u8 n ) {: u:n :}
   CAE-NUM-CAP CAE-NUM-I !
   u 0= if
      CAE-NUM-I @ 1- CAE-NUM-I !
      48 CAE-NUM CAE-NUM-I @ + c!
      CAE-NUM CAE-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      CAE-NUM-I @ 1- CAE-NUM-I !
      CAE-NUM CAE-NUM-I @ + c!
      10 /
   repeat drop
   CAE-NUM CAE-NUM-I @ + CAE-NUM-CAP CAE-NUM-I @ - ;

: CAE-U-TYPE ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: CAE-DQ ( -- )
   $22 SB-APPEND-C ;

: CAE-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-OK ( i64 -- i64 ) dup * ;" SB-APPEND CAE-LF
   s" : CAE-SEMI ( -- i64 ) [char] ; ;" SB-APPEND CAE-LF
   s" : CAE-BAD1 ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   s" : CAE-BAD2 ( i64 -- ) >r ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-SUPPORT-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" 7 constant CAE-SUP-K" SB-APPEND CAE-LF
   s" variable CAE-SUP-V" SB-APPEND CAE-LF
   s" create CAE-SUP-B 1 cells allot" SB-APPEND CAE-LF
   s" defer CAE-SUP-XV ( i64 -- i64 )" SB-APPEND CAE-LF
   s" TRUSTED: CAE-SUP-T ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   s" : CAE-OK-XV ( i64 -- i64 ) 1 + ;" SB-APPEND CAE-LF
   s" : CAE-OK-SUP ( i64 -- i64 ) [: CAE-OK-XV ;] is CAE-SUP-XV CAE-SUP-XV CAE-SUP-T CAE-SUP-K + ;" SB-APPEND CAE-LF
   s" : CAE-BAD-SUP ( i64 -- i64 ) CAE-SUP-T CAE-SUP-K + CAE-SUP-V @ drop CAE-SUP-B drop dup ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-UNDEF-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-UDEF ( i64 -- i64 ) dup NOPE ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-DUP-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-DUP ( i64 -- i64 ) 1 + ;" SB-APPEND CAE-LF
   s" : CAE-DUP ( i64 -- i64 ) 2 + ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-AS-LEAK-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" 0 constant CAE-BM-T-ID" SB-APPEND CAE-LF
   s" variable CAE-AS-COUNT" SB-APPEND CAE-LF
   s" TRUSTED: CAE-AS-LINE$ ( -- ptr u8 n ) s" SB-APPEND
   CAE-DQ 32 SB-APPEND-C s" 1	SQUARE	(i64 -- i64)" SB-APPEND CAE-DQ
   s"  ;" SB-APPEND CAE-LF
   s" : CAE-BM-TASK-FIELD$ ( ptr u8 n n -- ptr u8 n ) drop ;" SB-APPEND CAE-LF
   s" : CAE-AS-REQUIRE-NEW-ID ( ptr u8 n -- ) 2drop ;" SB-APPEND CAE-LF
   s" : CAE-AS-ADD-TASK ( -- )" SB-APPEND CAE-LF
   s"    CAE-AS-LINE$ CAE-BM-T-ID CAE-BM-TASK-FIELD$ CAE-AS-REQUIRE-NEW-ID" SB-APPEND CAE-LF
   s"    CAE-AS-COUNT @" SB-APPEND CAE-LF
   s"    CAE-AS-COUNT @ 1+ CAE-AS-COUNT ! ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-LARGE-START$ ( -- ptr u8 n )
   s" : CAE-LARGE-BAD ( i64 -- i64 ) ( " ;

: CAE-LARGE-LINE$ ( -- ptr u8 n )
   s" check all errors generated program padding 0123456789 abcdefghijklmnopqrstuvwxyz" ;

: CAE-LARGE-END$ ( -- ptr u8 n )
   s" ) dup ;" ;

: CAE-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CAE-WORD-LARGE$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-large-bad" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BAD1$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-bad1" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BAD2$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-bad2" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BADSUP$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-bad-sup" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-SUPK$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" CAE-SUP-K" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-ASADD$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-as-add-task" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-CAPSUPBAD$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" cae-cap-sup-bad" SB-APPEND CAE-DQ
   SB$ ;

: CAE-CODE-UNDEFINED$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" code" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" E-UNDEFINED" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-NOPE$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" NOPE" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-CAPSUP$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" CAE-CAP-SUP" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-BMTID$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" CAE-BM-T-ID" SB-APPEND CAE-DQ
   SB$ ;

: CAE-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-all" TMPDIR-MKDIR {: a:ptr u :}
   a u CAE-ROOT-BUF CAE-ROOT-U CAE-COPY!
   CAE-ROOT CLEANUP-DIR+
   CAE-ROOT s" input.f" CAE-IN-BUF JOIN-PATH CAE-IN-U !
   CAE-ROOT s" large.f" CAE-LARGE-BUF JOIN-PATH CAE-LARGE-U !
   CAE-ROOT s" xsup-a.f" CAE-XSUP-PATH-BUF JOIN-PATH CAE-XSUP-PATH-U !
   CAE-IN CLEANUP+
   CAE-LARGE CLEANUP+
   CAE-XSUP CLEANUP+
   CAE-IN CAE-SOURCE$ WRITE-ALL ;

: CAE-APPEND-LF ( ptr u8 n -- )
   CAE-LF-BYTE 1 APPEND-FILE ;

: CAE-WRITE-LARGE ( -- )
   CAE-LARGE CAE-LARGE-START$ WRITE-ALL
   CAE-LARGE CAE-APPEND-LF
   CAE-LARGE-LINES 0 ?do
      CAE-LARGE CAE-LARGE-LINE$ APPEND-FILE
      CAE-LARGE CAE-APPEND-LF
   loop
   CAE-LARGE CAE-LARGE-END$ APPEND-FILE
   CAE-LARGE CAE-APPEND-LF ;

: CAE-WRITE-MANY-DEFS-OK ( -- )
   CAE-LARGE CAE-EMPTY$ WRITE-ALL
   CAE-MANY-DEFS 0 ?do
      CAE-LARGE s" : CAE-CAP-OK-" APPEND-FILE
      CAE-LARGE i CAE-U$ APPEND-FILE
      CAE-LARGE s"  ( i64 -- i64 ) 1 + ;" APPEND-FILE
      CAE-LARGE CAE-APPEND-LF
   loop ;

: CAE-WRITE-MANY-SUPPORT ( -- )
   CAE-LARGE CAE-EMPTY$ WRITE-ALL
   CAE-MANY-SUPPORT 0 ?do
      CAE-LARGE s" 7 constant CAE-CAP-SUP-" APPEND-FILE
      CAE-LARGE i CAE-U$ APPEND-FILE
      CAE-LARGE CAE-APPEND-LF
   loop
   CAE-LARGE s" : CAE-CAP-SUP-BAD ( i64 -- i64 ) CAE-CAP-SUP-0 + dup ;" APPEND-FILE
   CAE-LARGE CAE-APPEND-LF ;

: CAE-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: CAE-RUN-CORE-ACT ( -- )
   CAE-RUN$ CAE-RUN$ CHECK-ALL-ERRORS-FILE ;

: CAE-RUN-BUF-ACT ( -- )
   CAE-RUN$ CAE-BUF-SRC$ CHECK-ALL-ERRORS-BUF ;

: CAE-CORE-CAPTURE ( ptr u8 n -- n n n n )
   CAE-RUN!
   CAE-ERR CAE-BUF-CAP CAE-OUT CAE-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CAE-RUN-CORE-ACT ;] catch CAE-RC !
   0 CHECK-ALL-ERRORS-OUT$ nip PROC-OUTCOME-EXIT CAE-RC @ ;

: CAE-BUF-CAPTURE ( ptr u8 n -- n n n n )
   CAE-BUF-SRC!
   CAE-IN CAE-RUN!
   CAE-ERR CAE-BUF-CAP CAE-OUT CAE-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CAE-RUN-BUF-ACT ;] catch CAE-RC !
   0 CHECK-ALL-ERRORS-OUT$ nip PROC-OUTCOME-EXIT CAE-RC @ ;

: CAE-ARGV-CHECK ( ptr u8 n -- ) {: file:ptr fileu :}
   ARGV-MOCK-CLEAR
   s" --json-errors" ARGV-MOCK+
   s" --label" ARGV-MOCK+
   file fileu ARGV-MOCK+
   file fileu ARGV-MOCK+ ;

: CAE-RUN-ARGV-ACT ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV-USAGE!
   ARGV-PARSE
   ARGV-REQUIRE-LABEL
   1 ARGV-EXPECT-POS-EXACT
   CAE-ERR CAE-BUF-CAP CAE-OUT CAE-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   ARGV-JSON? CHECK-ALL-ERRORS-JSON!
   ARGV-LABEL$ 0 ARGV-POS$ CHECK-ALL-ERRORS-FILE ;

: CAE-RUN ( -- n n n n )
   CAE-IN CAE-CORE-CAPTURE ;

: CAE-RUN-LARGE ( -- n n n n )
   CAE-LARGE CAE-CORE-CAPTURE ;

: CAE-RUN-CLI ( -- n n n n )
   CAE-IN CAE-ARGV-CHECK
   [: CAE-RUN-ARGV-ACT ;] catch CAE-RC !
   ARGV-USE-SCRIPT
   ARGV-RESET
   0 CHECK-ALL-ERRORS-OUT$ nip PROC-OUTCOME-EXIT CAE-RC @ ;

: CAE-OUTCOME. ( n -- ) {: kind:n :}
   kind PROC-OUTCOME-EXIT = if s" exit" type exit then
   kind PROC-OUTCOME-SIGNAL = if s" signal" type exit then
   kind PROC-OUTCOME-TIMEOUT = if s" timeout" type exit then
   s" unknown" type ;

: CAE-DUMP-CAPTURE ( n n n n n -- )
   {: outu:n erru:n kind:n code:n expect:n :}
   s" check-all-errors-test failure" type cr
   s" case: " type CAE-CASE$ type cr
   s" source: " type CAE-RUN$ type cr
   s" expected exit: " type expect . cr
   s" outcome: " type kind CAE-OUTCOME.
   s"  code: " type code . cr
   s" stdout bytes: " type outu . s" / " type CAE-BUF-CAP . cr
   s" stderr bytes: " type erru . s" / " type CAE-BUF-CAP . cr
   s" stdout:" type cr
   CAE-OUT outu type
   s" stderr:" type cr
   CAE-ERR erru type ;

: CAE-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT <> if outu erru kind code expect CAE-DUMP-CAPTURE then
   code expect <> if outu erru kind code expect CAE-DUMP-CAPTURE then
   CAE-CASE$ T-LABEL
   kind PROC-OUTCOME-EXIT T=
   CAE-CASE$ T-LABEL
   code expect T=
   outu erru ;

: CAE-TEST-SUPPORT-SOURCE ( -- )
   s" support-source" CAE-CASE!
   CAE-IN CAE-SUPPORT-SOURCE$ WRITE-ALL
   CAE-RUN 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" support-source stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" support-source word" T-LABEL
   CAE-ERR erru CAE-WORD-BADSUP$ CONTAINS? TTRUE
   s" support-source support token" T-LABEL
   CAE-ERR erru CAE-TOKEN-SUPK$ CONTAINS? TFALSE
   s" support-source diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-AS-ADD-TASK-LEAK ( -- )
   s" as-add-task-leak" CAE-CASE!
   CAE-IN CAE-AS-LEAK-SOURCE$ WRITE-ALL
   CAE-RUN 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" as-add-task stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" as-add-task word" T-LABEL
   CAE-ERR erru CAE-WORD-ASADD$ CONTAINS? TTRUE
   s" as-add-task private token" T-LABEL
   CAE-ERR erru CAE-TOKEN-BMTID$ CONTAINS? TFALSE
   s" as-add-task diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-MANY-DEFS-OK ( -- )
   s" many-defs-ok" CAE-CASE!
   CAE-WRITE-MANY-DEFS-OK
   CAE-RUN-LARGE 0 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" many-defs stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" many-defs stderr" T-LABEL
   CAE-ERR erru CAE-EMPTY$ T$= ;

: CAE-TEST-MANY-SUPPORT ( -- )
   s" many-support" CAE-CASE!
   CAE-WRITE-MANY-SUPPORT
   CAE-RUN-LARGE 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" many-support stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" many-support word" T-LABEL
   CAE-ERR erru CAE-WORD-CAPSUPBAD$ CONTAINS? TTRUE
   s" many-support private token" T-LABEL
   CAE-ERR erru CAE-TOKEN-CAPSUP$ CONTAINS? TFALSE
   s" many-support diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-UNDEFINED-JSON ( -- )
   s" undefined-json" CAE-CASE!
   CAE-IN CAE-UNDEF-SOURCE$ WRITE-ALL
   CAE-RUN 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" undefined-json stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" undefined-json code" T-LABEL
   CAE-ERR erru CAE-CODE-UNDEFINED$ CONTAINS? TTRUE
   s" undefined-json token" T-LABEL
   CAE-ERR erru CAE-TOKEN-NOPE$ CONTAINS? TTRUE
   s" undefined-json diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-BUF-CORE ( -- )
   s" buffer-core" CAE-CASE!
   CAE-SOURCE$ CAE-BUF-CAPTURE 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" buffer-core stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" buffer-core bad1" T-LABEL
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   s" buffer-core bad2" T-LABEL
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   s" buffer-core diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 2 T= ;

: CAE-TEST-DUP-BUF ( -- )
   s" duplicate-buffer" CAE-CASE!
   CAE-DUP-SOURCE$ CAE-BUF-CAPTURE CA-DUP-RC CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" duplicate-buffer stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" duplicate-buffer code" T-LABEL
   CAE-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   s" duplicate-buffer text" T-LABEL
   CAE-ERR erru s" duplicate-definition" CONTAINS? TTRUE
   s" duplicate-buffer diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-CLI-SMOKE ( -- )
   s" cli-smoke" CAE-CASE!
   CAE-IN CAE-SOURCE$ WRITE-ALL
   CAE-RUN-CLI 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" cli-smoke stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" cli-smoke bad1" T-LABEL
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   s" cli-smoke bad2" T-LABEL
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   s" cli-smoke diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 2 T= ;

\ typed-local-lint: allow-bare-local - q is the test action quotation.
: CAE-CASE-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns CAE-START-NS !
   q execute
   s" PASS: " type label labelu type
   s"  (" type mono-ns CAE-START-NS @ - PROC-NS-PER-MS / CAE-U-TYPE s" ms)" type cr ;

: CAE-TEST-BASE ( -- )
   s" base-two-errors" CAE-CASE!
   CAE-RUN 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" base stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" base bad1" T-LABEL
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   s" base bad2" T-LABEL
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   s" base diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 2 T= ;

: CAE-TEST-LARGE ( -- )
   CAE-WRITE-LARGE
   s" large-source" CAE-CASE!
   CAE-RUN-LARGE 70 CAE-EXPECT-EXIT {: loutu lerru :}
   s" large stdout" T-LABEL
   CAE-OUT loutu CAE-EMPTY$ T$=
   s" large word" T-LABEL
   CAE-ERR lerru CAE-WORD-LARGE$ CONTAINS? TTRUE ;

\ ---- TFAM 5: all-errors support-parity fixtures (census gap4) ----------------
\ verify-source's full re-drive replays deftype/deflinear/value-record before
\ later definitions; per-def all-errors redrive must collect the same support
\ or a good definition whose signature references the declared type is spuriously
\ rejected. Each fixture pairs a good type-using def with a genuinely-bad def:
\ the bad def forces per-def mode; before the fix the good def is ALSO reported.

: CAE-DEFTYPE-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" deftype cae-dt" SB-APPEND CAE-LF
   s" : CAE-DT-USE ( cae-dt -- cae-dt ) ;" SB-APPEND CAE-LF
   s" : CAE-DT-BAD ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-DEFLINEAR-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" deflinear cae-lin" SB-APPEND CAE-LF
   s" : CAE-LIN-USE ( cae-lin -- cae-lin ) ;" SB-APPEND CAE-LF
   s" : CAE-LIN-BAD ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-VREC-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" value-record cae-vr x i64 y i64 END-VALUE-RECORD" SB-APPEND CAE-LF
   s" : CAE-VR-USE ( cae-vr -- cae-vr ) ;" SB-APPEND CAE-LF
   s" : CAE-VR-BAD ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   SB$ ;

\ ---- TFAM 5 const-b89c90f0: layout-constant one-cell narrowing parity --------
\ `constant` bakes exactly one physical cell, so a value of a multi-cell layout
\ family (here the 2-field `cae-cv`) is narrowed to the one-cell `-- a` model in
\ EVERY replay path (native C-CONSTANT, verify-source RECORD-DEFINER?, this
\ all-errors funnel, and public-signatures). No path invents a multi-cell shape.
\ The checker-backed paths then fail-closed on any layout USE of the constant,
\ proving the narrowing is observable and not silently widened. Sound rejection /
\ shape-carrying at the constant itself is width-aware interpret-mode work owned
\ by TFAM 12 (habu-tfam-12-layout: `constant/depth/.s`, interpret mode); when it
\ lands this fixture flips from "layout USE rejected" to accepted/shape-carried.
: CAE-CONST-LAYOUT-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" value-record cae-cv x i64 y i64 END-VALUE-RECORD" SB-APPEND CAE-LF
   s" TRUSTED: CAE-CV-MK ( -- cae-cv ) 0 ;" SB-APPEND CAE-LF
   s" CAE-CV-MK constant CAE-CV-K" SB-APPEND CAE-LF
   s" : CAE-CV-USE ( -- cae-cv ) CAE-CV-K ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-TFAM-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" TYPEFAMILY cae-tf 1" SB-APPEND CAE-LF
   s" SUMTYPE cae-rs 2" SB-APPEND CAE-LF
   s"   VARIANT ok  a ;VARIANT" SB-APPEND CAE-LF
   s"   VARIANT err b ;VARIANT" SB-APPEND CAE-LF
   s" ;SUMTYPE" SB-APPEND CAE-LF
   s" : CAE-TF-USE ( cae-tf<n> cae-rs<n,f> -- cae-tf<n> cae-rs<n,f> ) ;" SB-APPEND CAE-LF
   s" : CAE-TF-BAD ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-WORD-JSON$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ a u SB-APPEND CAE-DQ
   SB$ ;

: CAE-CHECK-SUPPORT-PARITY ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n good:ptr goodu:n bad:ptr badu:n :}
   src srcu CAE-BUF-CAPTURE 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   CAE-CASE$ T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-CASE$ T-LABEL
   CAE-ERR erru bad badu CAE-WORD-JSON$ CONTAINS? TTRUE
   CAE-CASE$ T-LABEL
   CAE-ERR erru good goodu CAE-WORD-JSON$ CONTAINS? TFALSE
   CAE-CASE$ T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-DEFTYPE-SUPPORT ( -- )
   s" deftype-support" CAE-CASE!
   CAE-DEFTYPE-SOURCE$ s" cae-dt-use" s" cae-dt-bad" CAE-CHECK-SUPPORT-PARITY ;

: CAE-TEST-DEFLINEAR-SUPPORT ( -- )
   s" deflinear-support" CAE-CASE!
   CAE-DEFLINEAR-SOURCE$ s" cae-lin-use" s" cae-lin-bad" CAE-CHECK-SUPPORT-PARITY ;

: CAE-TEST-VREC-SUPPORT ( -- )
   s" value-record-support" CAE-CASE!
   CAE-VREC-SOURCE$ s" cae-vr-use" s" cae-vr-bad" CAE-CHECK-SUPPORT-PARITY ;

\ The layout constant is narrowed to one-cell `-- a`, so `CAE-CV-USE` (which
\ declares the 2-field `cae-cv` layout) is fail-closed rejected: the inferred
\ effect is `-- a`, the declared effect is `-- field<cae-cv,x,i64> ...`.
: CAE-TEST-CONST-LAYOUT ( -- )
   s" const-layout-narrow" CAE-CASE!
   CAE-CONST-LAYOUT-SOURCE$ CAE-BUF-CAPTURE 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   CAE-CASE$ T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-CASE$ T-LABEL
   CAE-ERR erru s" cae-cv-use" CAE-WORD-JSON$ CONTAINS? TTRUE
   CAE-CASE$ T-LABEL
   CAE-ERR erru s" field<cae-cv" CONTAINS? TTRUE ;

\ PENDING (TFAM 12 slice 3, habu-tfam-12-layout): staged flip of the fixture
\ above, NOT in the runner yet. When width-aware `constant` shape-carries the
\ whole logical value at the value-pop (native C-CONSTANT + verify-source
\ RECORD-DEFINER? + PS-MAYBE-TRUST-DEFINER + CA-ADD-SUPPORT-CONSTANT drop the
\ one-cell `-- a` narrowing), the same source loads clean: CAE-CV-K carries the
\ layout shape and CAE-CV-USE certifies. Slice 3 replaces the
\ "const-layout-narrow" runner row with this case and deletes the narrow
\ variant plus the `-- a` boundary comments at all four sites.
: CAE-TEST-CONST-CARRY ( -- )
   s" const-layout-carry" CAE-CASE!
   CAE-CONST-LAYOUT-SOURCE$ CAE-BUF-CAPTURE 0 CAE-EXPECT-EXIT {: outu:n erru:n :}
   CAE-CASE$ T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-CASE$ T-LABEL
   CAE-ERR erru s" cae-cv-use" CAE-WORD-JSON$ CONTAINS? TFALSE
   CAE-CASE$ T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 0 T= ;

: CAE-TEST-TFAM-SUPPORT ( -- )
   s" type-family-support" CAE-CASE!
   CAE-TFAM-SOURCE$ s" cae-tf-use" s" cae-tf-bad" CAE-CHECK-SUPPORT-PARITY ;

\ ---- TFAM 5: verify-source top-level TRUST replay (census gap5) ---------------
\ all-errors collects a top-level `s" NAME" s" SIG" TRUST` line and replays it
\ through verify-source before later definitions. verify-source's RECORD-DEFINER?
\ must dispatch that TRUST so a definition using the trusted word passes. A clean
\ fixture (no genuinely-bad def) exits 0 only when the TRUST replay works.

: CAE-SQ-LIT ( ptr u8 n -- ) {: a:ptr u:n :}
   115 SB-APPEND-C
   CAE-DQ
   32 SB-APPEND-C
   a u SB-APPEND
   CAE-DQ
   32 SB-APPEND-C ;

: CAE-TRUST-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" CAE-TRO-HELP" CAE-SQ-LIT
   s" i64 -- i64" CAE-SQ-LIT
   s" TRUST" SB-APPEND CAE-LF
   s" : CAE-TRO-USE ( i64 -- i64 ) CAE-TRO-HELP ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-IMMEDIATE-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-IM ( i64 -- i64 ) 1 + ;" SB-APPEND CAE-LF
   s" immediate" SB-APPEND CAE-LF
   s" : CAE-IM-USE ( i64 -- i64 ) CAE-IM ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-EXPORT-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-EX ( i64 -- i64 ) 1 + ;" SB-APPEND CAE-LF
   s" EXPORT CAE-EX" SB-APPEND CAE-LF
   s" : CAE-EX-USE ( i64 -- i64 ) CAE-EX ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-CHECK-CLEAN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu CAE-BUF-CAPTURE 0 CAE-EXPECT-EXIT {: outu:n erru:n :}
   CAE-CASE$ T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-CASE$ T-LABEL
   CAE-ERR erru CAE-EMPTY$ T$= ;

: CAE-TEST-TRUST-SUPPORT ( -- )
   s" trust-support" CAE-CASE!
   CAE-TRUST-SOURCE$ CAE-CHECK-CLEAN ;

: CAE-TEST-IMMEDIATE-SUPPORT ( -- )
   s" immediate-support" CAE-CASE!
   CAE-IMMEDIATE-SOURCE$ CAE-CHECK-CLEAN ;

: CAE-TEST-EXPORT-SUPPORT ( -- )
   s" export-support" CAE-CASE!
   CAE-EXPORT-SOURCE$ CAE-CHECK-CLEAN ;

\ Cross-file support: a prior source-list file's type and word are in scope
\ for the checked buffer only when its path is registered through
\ CHECK-ALL-ERRORS-SUPPORT+; the same buffer without registration fail-closed
\ rejects. This is the hook the check source-list redrive drives per file.
: CAE-XSUP-SUP$ ( -- ptr u8 n )
   SB-RESET
   s" deftype cae-xt" SB-APPEND CAE-LF
   s" : CAE-XT-ID ( cae-xt -- cae-xt ) ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-XSUP-USE$ ( -- ptr u8 n )
   SB-RESET
   s" : CAE-XT-USE ( cae-xt -- cae-xt ) CAE-XT-ID ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-TEST-XSUP-REPLAY ( -- )
   s" xsup-replay" CAE-CASE!
   CAE-XSUP CAE-XSUP-SUP$ WRITE-ALL
   CHECK-ALL-ERRORS-SUPPORT-RESET
   CAE-XSUP-USE$ CAE-BUF-CAPTURE 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   CAE-CASE$ T-LABEL
   CAE-ERR erru s" cae-xt-use" CAE-WORD-JSON$ CONTAINS? TTRUE
   CAE-XSUP CHECK-ALL-ERRORS-SUPPORT+
   CAE-XSUP-USE$ CAE-BUF-CAPTURE 0 CAE-EXPECT-EXIT {: outu2:n erru2:n :}
   CAE-CASE$ T-LABEL
   outu2 0 T=
   CAE-CASE$ T-LABEL
   erru2 0 T=
   CHECK-ALL-ERRORS-SUPPORT-RESET ;

: CAE-MAIN ( -- )
   T-RESET
   CAE-PREPARE
   s" base-two-errors" [: CAE-TEST-BASE ;] CAE-CASE-RUN
   s" deftype-support" [: CAE-TEST-DEFTYPE-SUPPORT ;] CAE-CASE-RUN
   s" deflinear-support" [: CAE-TEST-DEFLINEAR-SUPPORT ;] CAE-CASE-RUN
   s" value-record-support" [: CAE-TEST-VREC-SUPPORT ;] CAE-CASE-RUN
   s" const-layout-narrow" [: CAE-TEST-CONST-LAYOUT ;] CAE-CASE-RUN
   s" type-family-support" [: CAE-TEST-TFAM-SUPPORT ;] CAE-CASE-RUN
   s" trust-support" [: CAE-TEST-TRUST-SUPPORT ;] CAE-CASE-RUN
   s" immediate-support" [: CAE-TEST-IMMEDIATE-SUPPORT ;] CAE-CASE-RUN
   s" export-support" [: CAE-TEST-EXPORT-SUPPORT ;] CAE-CASE-RUN
   s" xsup-replay" [: CAE-TEST-XSUP-REPLAY ;] CAE-CASE-RUN
   s" large-source" [: CAE-TEST-LARGE ;] CAE-CASE-RUN
   s" support-source" [: CAE-TEST-SUPPORT-SOURCE ;] CAE-CASE-RUN
   s" as-add-task-leak" [: CAE-TEST-AS-ADD-TASK-LEAK ;] CAE-CASE-RUN
   s" many-defs-ok" [: CAE-TEST-MANY-DEFS-OK ;] CAE-CASE-RUN
   s" many-support" [: CAE-TEST-MANY-SUPPORT ;] CAE-CASE-RUN
   s" undefined-json" [: CAE-TEST-UNDEFINED-JSON ;] CAE-CASE-RUN
   s" buffer-core" [: CAE-TEST-BUF-CORE ;] CAE-CASE-RUN
   s" duplicate-buffer" [: CAE-TEST-DUP-BUF ;] CAE-CASE-RUN
   s" cli-smoke" [: CAE-TEST-CLI-SMOKE ;] CAE-CASE-RUN
   CLEANUP-RUN
   s" cleanup root removed" T-LABEL
   CAE-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-all-errors-test: ok" type cr ;

CAE-MAIN
