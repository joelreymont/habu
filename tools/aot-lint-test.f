\ aot-lint-test.f - checked fixtures for tools/aot-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/aot-lint-test.f

4096 constant ALT-BUF-CAP
10000 constant ALT-TIMEOUT-MS

variable ALT-ROOT-U
variable ALT-GOOD-U
variable ALT-BAD-U

create ALT-ROOT-BUF FS-PATH-CAP allot
create ALT-GOOD-BUF FS-PATH-CAP allot
create ALT-BAD-BUF FS-PATH-CAP allot
create ALT-OUT ALT-BUF-CAP allot
create ALT-ERR ALT-BUF-CAP allot

: ALT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ALT-ROOT ( -- ptr u8 n )
   ALT-ROOT-BUF ALT-ROOT-U @ ;

: ALT-GOOD ( -- ptr u8 n )
   ALT-GOOD-BUF ALT-GOOD-U @ ;

: ALT-BAD ( -- ptr u8 n )
   ALT-BAD-BUF ALT-BAD-U @ ;

: ALT-LF ( -- )
   10 SB-APPEND-C ;

: ALT-DQ ( -- )
   34 SB-APPEND-C ;

: ALT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" \\ here in comment" SB-APPEND ALT-LF
   115 SB-APPEND-C ALT-DQ s"  here in string" SB-APPEND ALT-DQ ALT-LF
   s" : MAIN ( -- ) 42 . CR ;" SB-APPEND ALT-LF
   SB$ ;

: ALT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" : MAIN ( -- ) here . CR ;" SB-APPEND ALT-LF
   SB$ ;

: ALT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: ALT-CODE$ ( -- ptr u8 n )
   s" E-AOT-UNSUPPORTED" ;

: ALT-JSON-CODE$ ( -- ptr u8 n )
   SB-RESET
   ALT-DQ s" code" SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ ALT-CODE$ SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-LABEL$ ( -- ptr u8 n )
   SB-RESET
   ALT-DQ s" file" SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ s" <stdin>" SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-STR-FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu val:ptr valu :}
   SB-RESET
   ALT-DQ key keyu SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ val valu SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-TOKEN$ ( -- ptr u8 n )
   s" token" s" here" ALT-JSON-STR-FIELD$ ;

: ALT-JSON-WORD$ ( -- ptr u8 n )
   s" word" s" MAIN" ALT-JSON-STR-FIELD$ ;

: ALT-JSON-REASON$ ( -- ptr u8 n )
   s" reason" s" stripped AOT has no persistent data region" ALT-JSON-STR-FIELD$ ;

: ALT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-aot-lint" TMPDIR-MKDIR {: a:ptr u :}
   a u ALT-ROOT-BUF ALT-ROOT-U ALT-COPY!
   ALT-ROOT CLEANUP-DIR+
   ALT-ROOT s" good.f" ALT-GOOD-BUF JOIN-PATH ALT-GOOD-U !
   ALT-ROOT s" bad.f" ALT-BAD-BUF JOIN-PATH ALT-BAD-U !
   ALT-GOOD CLEANUP+
   ALT-BAD CLEANUP+
   ALT-GOOD ALT-GOOD$ WRITE-ALL
   ALT-BAD ALT-BAD$ WRITE-ALL ;

: ALT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: ALT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/aot-lint.f" WR-TOOLS-LOAD if exit then
   s" --load" ALT-ARG+
   s" lib/errors.f" ALT-ARG+
   s" lib/memory.f" ALT-ARG+
   s" lib/vector.f" ALT-ARG+
   s" tools/lint/text.f" ALT-ARG+
   s" tools/lint/token.f" ALT-ARG+
   s" tools/lint/lib.f" ALT-ARG+
   s" tools/lint/json-writer.f" ALT-ARG+
   s" tools/lint/source-lex.f" ALT-ARG+
   s" tools/argv.f" ALT-ARG+
   s" tools/aot-lint.f" ALT-ARG+
   s" --" ALT-ARG+ ;

: ALT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: ALT-RUN-GOOD ( -- n n n n )
   ALT-ARGV-LOAD
   ALT-GOOD ALT-ARG+
   WR-TOOLS$ >LEN ALT-OUT ALT-BUF-CAP >LEN ALT-ERR ALT-BUF-CAP >LEN
   ALT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   ALT-CAPTURE>N ;

: ALT-RUN-BAD ( -- n n n n )
   ALT-ARGV-LOAD
   ALT-BAD ALT-ARG+
   WR-TOOLS$ >LEN ALT-OUT ALT-BUF-CAP >LEN ALT-ERR ALT-BUF-CAP >LEN
   ALT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   ALT-CAPTURE>N ;

: ALT-RUN-BAD-JSON ( -- n n n n )
   ALT-ARGV-LOAD
   s" --json" ALT-ARG+
   s" --label" ALT-ARG+
   s" <stdin>" ALT-ARG+
   ALT-BAD ALT-ARG+
   WR-TOOLS$ >LEN ALT-OUT ALT-BUF-CAP >LEN ALT-ERR ALT-BUF-CAP >LEN
   ALT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   ALT-CAPTURE>N ;

: ALT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: ALT-TEST-GOOD ( -- )
   ALT-RUN-GOOD 0 ALT-EXPECT-EXIT {: outu erru :}
   ALT-OUT outu ALT-EMPTY$ T$=
   ALT-ERR erru ALT-EMPTY$ T$= ;

: ALT-TEST-BAD ( -- )
   ALT-RUN-BAD 1 ALT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   ALT-OUT outu ALT-CODE$ CONTAINS? TTRUE ;

: ALT-TEST-BAD-JSON ( -- )
   ALT-RUN-BAD-JSON 1 ALT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   ALT-OUT outu ALT-JSON-CODE$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-LABEL$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-TOKEN$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-WORD$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-REASON$ CONTAINS? TTRUE ;

: ALT-MAIN ( -- )
   T-RESET
   ALT-PREPARE
   ALT-TEST-GOOD
   ALT-TEST-BAD
   ALT-TEST-BAD-JSON
   CLEANUP-RUN
   ALT-ROOT EXISTS? TFALSE
   T-REPORT
   s" aot-lint-test: ok" type cr ;

ALT-MAIN
