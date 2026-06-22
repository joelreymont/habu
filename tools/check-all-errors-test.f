\ check-all-errors-test.f - checked fixtures for tools/check-all-errors.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/check-all-errors-test.f

4096 constant CAE-BUF-CAP
1400 constant CAE-LARGE-LINES

variable CAE-ROOT-U
variable CAE-IN-U
variable CAE-LARGE-U

create CAE-ROOT-BUF FS-PATH-CAP allot
create CAE-IN-BUF FS-PATH-CAP allot
create CAE-LARGE-BUF FS-PATH-CAP allot
create CAE-OUT CAE-BUF-CAP allot
create CAE-ERR CAE-BUF-CAP allot
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

: CAE-LF ( -- )
   10 SB-APPEND-C ;

: CAE-DQ ( -- )
   34 SB-APPEND-C ;

: CAE-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( i64 -- i64 ) dup * ;" SB-APPEND CAE-LF
   s" : SEMI ( -- i64 ) [char] ; ;" SB-APPEND CAE-LF
   s" : BAD1 ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   s" : BAD2 ( i64 -- ) >r ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-SUPPORT-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" 7 constant SUP-K" SB-APPEND CAE-LF
   s" variable SUP-V" SB-APPEND CAE-LF
   s" create SUP-B 1 cells allot" SB-APPEND CAE-LF
   s" TRUSTED: SUP-T ( i64 -- i64 ) dup ;" SB-APPEND CAE-LF
   s" : OK-SUP ( i64 -- i64 ) SUP-T SUP-K + ;" SB-APPEND CAE-LF
   s" : BAD-SUP ( i64 -- i64 ) SUP-T SUP-K + SUP-V @ drop SUP-B drop dup ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-AS-LEAK-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" 0 constant BM-T-ID" SB-APPEND CAE-LF
   s" variable AS-COUNT" SB-APPEND CAE-LF
   s" TRUSTED: AS-LINE$ ( -- ptr u8 n ) s" SB-APPEND
   CAE-DQ 32 SB-APPEND-C s" 1	SQUARE	(i64 -- i64)" SB-APPEND CAE-DQ
   s"  ;" SB-APPEND CAE-LF
   s" : BM-TASK-FIELD$ ( ptr u8 n n -- ptr u8 n ) drop ;" SB-APPEND CAE-LF
   s" : AS-REQUIRE-NEW-ID ( ptr u8 n -- ) 2drop ;" SB-APPEND CAE-LF
   s" : AS-ADD-TASK ( -- )" SB-APPEND CAE-LF
   s"    AS-LINE$ BM-T-ID BM-TASK-FIELD$ AS-REQUIRE-NEW-ID" SB-APPEND CAE-LF
   s"    AS-COUNT @" SB-APPEND CAE-LF
   s"    AS-COUNT @ 1+ AS-COUNT ! ;" SB-APPEND CAE-LF
   SB$ ;

: CAE-LARGE-START$ ( -- ptr u8 n )
   s" : LARGE-BAD ( i64 -- i64 ) ( " ;

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
   CAE-DQ s" large-bad" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BAD1$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" bad1" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BAD2$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" bad2" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-BADSUP$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" bad-sup" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-SUPK$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" SUP-K" SB-APPEND CAE-DQ
   SB$ ;

: CAE-WORD-ASADD$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" word" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" as-add-task" SB-APPEND CAE-DQ
   SB$ ;

: CAE-TOKEN-BMTID$ ( -- ptr u8 n )
   SB-RESET
   CAE-DQ s" token" SB-APPEND CAE-DQ
   58 SB-APPEND-C
   CAE-DQ s" BM-T-ID" SB-APPEND CAE-DQ
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

: CAE-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CAE-RUN ( -- n n n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
   s" --label"  >LEN PROC-ARGV+
   CAE-IN  >LEN PROC-ARGV+
   CAE-IN  >LEN PROC-ARGV+
   s" bin/hb" >LEN CAE-OUT CAE-BUF-CAP >LEN CAE-ERR CAE-BUF-CAP >LEN 2000 >MS RUN-ARGV-CAPTURE
   CAE-CAPTURE>N ;

: CAE-RUN-LARGE ( -- n n n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
   s" --label"  >LEN PROC-ARGV+
   CAE-LARGE  >LEN PROC-ARGV+
   CAE-LARGE  >LEN PROC-ARGV+
   s" bin/hb" >LEN CAE-OUT CAE-BUF-CAP >LEN CAE-ERR CAE-BUF-CAP >LEN 2000 >MS RUN-ARGV-CAPTURE
   CAE-CAPTURE>N ;

: CAE-TEST-SUPPORT-PRELUDE ( -- )
   CAE-IN CAE-SUPPORT-SOURCE$ WRITE-ALL
   CAE-RUN 70 T=
   {: outu erru :}
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-ERR erru CAE-WORD-BADSUP$ CONTAINS? TTRUE
   CAE-ERR erru CAE-TOKEN-SUPK$ CONTAINS? TFALSE
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-TEST-AS-ADD-TASK-LEAK ( -- )
   CAE-IN CAE-AS-LEAK-SOURCE$ WRITE-ALL
   CAE-RUN 70 T=
   {: outu erru :}
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-ERR erru CAE-WORD-ASADD$ CONTAINS? TTRUE
   CAE-ERR erru CAE-TOKEN-BMTID$ CONTAINS? TFALSE
   CAE-ERR erru 10 COUNT-CHAR 1 T= ;

: CAE-MAIN ( -- )
   T-RESET
   CAE-PREPARE
   CAE-RUN 70 T=
   {: outu erru :}
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   CAE-ERR erru 10 COUNT-CHAR 2 T=
   CAE-WRITE-LARGE
   CAE-RUN-LARGE 70 T=
   {: loutu lerru :}
   CAE-OUT loutu CAE-EMPTY$ T$=
   CAE-ERR lerru CAE-WORD-LARGE$ CONTAINS? TTRUE
   CAE-TEST-SUPPORT-PRELUDE
   CAE-TEST-AS-ADD-TASK-LEAK
   CLEANUP-RUN
   CAE-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-all-errors-test: ok" type cr ;

CAE-MAIN
