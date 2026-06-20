\ check-all-errors-test.f - checked fixtures for tools/check-all-errors.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/check-all-errors-test.f

4096 constant CAE-BUF-CAP

variable CAE-ROOT-U
variable CAE-IN-U

create CAE-ROOT-BUF FS-PATH-CAP allot
create CAE-IN-BUF FS-PATH-CAP allot
create CAE-OUT CAE-BUF-CAP allot
create CAE-ERR CAE-BUF-CAP allot

: CAE-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CAE-ROOT ( -- ptr u8 n )
   CAE-ROOT-BUF CAE-ROOT-U @ ;

: CAE-IN ( -- ptr u8 n )
   CAE-IN-BUF CAE-IN-U @ ;

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

: CAE-EMPTY$ ( -- ptr u8 n )
   SB-RESET
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

: CAE-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-all" TMPDIR-MKDIR {: a:ptr u :}
   a u CAE-ROOT-BUF CAE-ROOT-U CAE-COPY!
   CAE-ROOT CLEANUP-DIR+
   CAE-ROOT s" input.f" CAE-IN-BUF JOIN-PATH CAE-IN-U !
   CAE-IN CLEANUP+
   CAE-IN CAE-SOURCE$ WRITE-ALL ;

: CAE-RUN ( -- n n n )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/lint/json-writer.f" PROC-ARGV+
   s" tools/lint/source-lex.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check-all-errors.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" --label" PROC-ARGV+
   CAE-IN PROC-ARGV+
   CAE-IN PROC-ARGV+
   s" bin/hb" CAE-OUT CAE-BUF-CAP CAE-ERR CAE-BUF-CAP 2000 RUN-ARGV-CAPTURE ;

: CAE-MAIN ( -- )
   T-RESET
   CAE-PREPARE
   CAE-RUN 70 T=
   {: outu erru :}
   CAE-OUT outu CAE-EMPTY$ T$=
   CAE-ERR erru CAE-WORD-BAD1$ CONTAINS? TTRUE
   CAE-ERR erru CAE-WORD-BAD2$ CONTAINS? TTRUE
   CAE-ERR erru 10 COUNT-CHAR 2 T=
   CLEANUP-RUN
   CAE-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-all-errors-test: ok" type cr ;

CAE-MAIN
