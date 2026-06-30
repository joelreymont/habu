\ check-all-errors-test.f - checked fixtures for tools/check-all-errors.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/check-all-errors-core.f tools/argv.f tools/warm-run.f
\ tools/check-all-errors-test.f

4096 constant CAE-BUF-CAP
10000 constant CAE-TIMEOUT-MS
1400 constant CAE-LARGE-LINES
530 constant CAE-MANY-DEFS
530 constant CAE-MANY-SUPPORT
32 constant CAE-NUM-CAP

variable CAE-ROOT-U
variable CAE-IN-U
variable CAE-LARGE-U
variable CAE-NUM-I
variable CAE-RUN-A
variable CAE-RUN-U
variable CAE-RC
variable CAE-CASE-A
variable CAE-CASE-U
variable CAE-BUF-SRC-A
variable CAE-BUF-SRC-U

create CAE-ROOT-BUF FS-PATH-CAP allot
create CAE-IN-BUF FS-PATH-CAP allot
create CAE-LARGE-BUF FS-PATH-CAP allot
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
   CAE-IN CLEANUP+
   CAE-LARGE CLEANUP+
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

: CAE-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CAE-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/check-all-errors.f" WR-TOOLS-LOAD if exit then
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: CAE-ARGV-CHECK ( ptr u8 n -- ) {: file:ptr fileu :}
   CAE-ARGV-LOAD
   s" --json-errors"  >LEN PROC-ARGV+
   s" --label"  >LEN PROC-ARGV+
   file fileu  >LEN PROC-ARGV+
   file fileu  >LEN PROC-ARGV+ ;

: CAE-HB-CAPTURE ( -- n n n n )
   WR-TOOLS$ >LEN CAE-OUT CAE-BUF-CAP >LEN CAE-ERR CAE-BUF-CAP >LEN
   CAE-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   CAE-CAPTURE>N ;

: CAE-RUN ( -- n n n n )
   CAE-IN CAE-CORE-CAPTURE ;

: CAE-RUN-LARGE ( -- n n n n )
   CAE-LARGE CAE-CORE-CAPTURE ;

: CAE-RUN-CLI ( -- n n n n )
   CAE-IN CAE-RUN!
   CAE-IN CAE-ARGV-CHECK
   CAE-HB-CAPTURE ;

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

: CAE-MAIN ( -- )
   T-RESET
   CAE-PREPARE
   s" base-two-errors" CAE-CASE!
   CAE-RUN 70 CAE-EXPECT-EXIT {: outu:n erru:n :}
   s" base stdout" T-LABEL
   CAE-OUT outu CAE-EMPTY$ T$=
   s" base bad1" T-LABEL
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   s" base bad2" T-LABEL
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   s" base diag count" T-LABEL
   CAE-ERR erru 10 COUNT-CHAR 2 T=
   CAE-WRITE-LARGE
   s" large-source" CAE-CASE!
   CAE-RUN-LARGE 70 CAE-EXPECT-EXIT {: loutu lerru :}
   s" large stdout" T-LABEL
   CAE-OUT loutu CAE-EMPTY$ T$=
   s" large word" T-LABEL
   CAE-ERR lerru CAE-WORD-LARGE$ CONTAINS? TTRUE
   CAE-TEST-SUPPORT-SOURCE
   CAE-TEST-AS-ADD-TASK-LEAK
   CAE-TEST-MANY-DEFS-OK
   CAE-TEST-MANY-SUPPORT
   CAE-TEST-UNDEFINED-JSON
   CAE-TEST-BUF-CORE
   CAE-TEST-DUP-BUF
   CAE-TEST-CLI-SMOKE
   CLEANUP-RUN
   s" cleanup root removed" T-LABEL
   CAE-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-all-errors-test: ok" type cr ;

CAE-MAIN
