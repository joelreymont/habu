\ signature-lint-test.f - checked fixtures for tools/signature-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/signature-lint-core.f tools/signature-lint-test.f

4096 constant SLT-BUF-CAP
10000 constant SLT-TIMEOUT-MS

variable SLT-ROOT-U
variable SLT-GOOD-U
variable SLT-MISSING-U
variable SLT-OPTOUT-U
variable SLT-NAME-U

create SLT-ROOT-BUF FS-PATH-CAP allot
create SLT-GOOD-BUF FS-PATH-CAP allot
create SLT-MISSING-BUF FS-PATH-CAP allot
create SLT-OPTOUT-BUF FS-PATH-CAP allot
create SLT-NAME-BUF FS-PATH-CAP allot
create SLT-OUT SLT-BUF-CAP allot
create SLT-ERR SLT-BUF-CAP allot

: SLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: SLT-ROOT ( -- ptr u8 n )
   SLT-ROOT-BUF SLT-ROOT-U @ ;

: SLT-GOOD ( -- ptr u8 n )
   SLT-GOOD-BUF SLT-GOOD-U @ ;

: SLT-MISSING ( -- ptr u8 n )
   SLT-MISSING-BUF SLT-MISSING-U @ ;

: SLT-OPTOUT ( -- ptr u8 n )
   SLT-OPTOUT-BUF SLT-OPTOUT-U @ ;

: SLT-NAME ( -- ptr u8 n )
   SLT-NAME-BUF SLT-NAME-U @ ;

: SLT-LF ( -- )
   10 SB-APPEND-C ;

: SLT-DQ ( -- )
   34 SB-APPEND-C ;

: SLT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( n -- n ) dup ;" SB-APPEND SLT-LF
   s" \\ : COMMENTED dup ;" SB-APPEND SLT-LF
   115 SB-APPEND-C SLT-DQ s"  : STRING ;" SB-APPEND SLT-DQ SLT-LF
   s" ( : PAREN dup ; )" SB-APPEND SLT-LF
   SB$ ;

: SLT-MISSING$ ( -- ptr u8 n )
   SB-RESET
   s" : NOSIG dup ;" SB-APPEND SLT-LF
   SB$ ;

: SLT-OPTOUT$ ( -- ptr u8 n )
   SB-RESET
   s" : X ( infer ) dup ;" SB-APPEND SLT-LF
   SB$ ;

: SLT-NAME$ ( -- ptr u8 n )
   SB-RESET
   s" : ( n -- n ) dup ;" SB-APPEND SLT-LF
   SB$ ;

: SLT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: SLT-MISSING-CODE$ ( -- ptr u8 n )
   s" E-MISSING-SIGNATURE" ;

: SLT-UNVERIFIED-CODE$ ( -- ptr u8 n )
   s" E-UNVERIFIED-SIGNATURE" ;

: SLT-NAME-CODE$ ( -- ptr u8 n )
   s" E-MISSING-NAME" ;

: SLT-JSON-MISSING$ ( -- ptr u8 n )
   SB-RESET
   SLT-DQ s" code" SB-APPEND SLT-DQ
   58 SB-APPEND-C
   SLT-DQ SLT-MISSING-CODE$ SB-APPEND SLT-DQ
   SB$ ;

: SLT-JSON-UNVERIFIED$ ( -- ptr u8 n )
   SB-RESET
   SLT-DQ s" code" SB-APPEND SLT-DQ
   58 SB-APPEND-C
   SLT-DQ SLT-UNVERIFIED-CODE$ SB-APPEND SLT-DQ
   SB$ ;

: SLT-JSON-LABEL$ ( -- ptr u8 n )
   SB-RESET
   SLT-DQ s" file" SB-APPEND SLT-DQ
   58 SB-APPEND-C
   SLT-DQ s" <stdin>" SB-APPEND SLT-DQ
   SB$ ;

: SLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-signature-lint" TMPDIR-MKDIR {: a:ptr u :}
   a u SLT-ROOT-BUF SLT-ROOT-U SLT-COPY!
   SLT-ROOT CLEANUP-DIR+
   SLT-ROOT s" good.f" SLT-GOOD-BUF JOIN-PATH SLT-GOOD-U !
   SLT-ROOT s" missing.f" SLT-MISSING-BUF JOIN-PATH SLT-MISSING-U !
   SLT-ROOT s" optout.f" SLT-OPTOUT-BUF JOIN-PATH SLT-OPTOUT-U !
   SLT-ROOT s" missing-name.f" SLT-NAME-BUF JOIN-PATH SLT-NAME-U !
   SLT-GOOD CLEANUP+
   SLT-MISSING CLEANUP+
   SLT-OPTOUT CLEANUP+
   SLT-NAME CLEANUP+
   SLT-GOOD SLT-GOOD$ WRITE-ALL
   SLT-MISSING SLT-MISSING$ WRITE-ALL
   SLT-OPTOUT SLT-OPTOUT$ WRITE-ALL
   SLT-NAME SLT-NAME$ WRITE-ALL ;

: SLT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SLT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/signature-lint.f" WR-TOOLS-LOAD if exit then
   s" --load" SLT-ARG+
   s" lib/errors.f" SLT-ARG+
   s" lib/string.f" SLT-ARG+
   s" lib/memory.f" SLT-ARG+
   s" lib/vector.f" SLT-ARG+
   s" tools/lint/text.f" SLT-ARG+
   s" tools/lint/token.f" SLT-ARG+
   s" tools/lint/lib.f" SLT-ARG+
   s" tools/lint/json-writer.f" SLT-ARG+
   s" tools/lint/source-lex.f" SLT-ARG+
   s" tools/signature-lint-core.f" SLT-ARG+
   s" tools/argv.f" SLT-ARG+
   s" tools/signature-lint.f" SLT-ARG+
   s" --" SLT-ARG+ ;

: SLT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: SLT-RUN-JSON-LABEL ( ptr u8 n -- n n n n ) {: a:ptr u :}
   SLT-ARGV-LOAD
   s" --json" SLT-ARG+
   s" --label" SLT-ARG+
   s" <stdin>" SLT-ARG+
   a u SLT-ARG+
   WR-TOOLS$ >LEN SLT-OUT SLT-BUF-CAP >LEN SLT-ERR SLT-BUF-CAP >LEN
   SLT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   SLT-CAPTURE>N ;

: SLT-CORE-SETUP ( bool -- ) {: json:bool :}
   SIGNATURE-LINT-RESET
   SLT-OUT SLT-BUF-CAP LINT-OUT-BUFFER!
   json SL-JSON! ;

: SLT-CORE-FINISH ( -- n n n n )
   [: SIGNATURE-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 PROC-OUTCOME-EXIT rc ;

: SLT-RUN-CORE ( ptr u8 n -- n n n n )
   LINT-FALSE SLT-CORE-SETUP
   SIGNATURE-LINT-FILE
   SLT-CORE-FINISH ;

: SLT-RUN-CORE-JSON ( ptr u8 n -- n n n n )
   LINT-TRUE SLT-CORE-SETUP
   SIGNATURE-LINT-FILE
   SLT-CORE-FINISH ;

: SLT-RUN-CORE-JSON-LABEL ( ptr u8 n -- n n n n )
   {: a:ptr u:n :}
   LINT-TRUE SLT-CORE-SETUP
   a u s" <stdin>" SIGNATURE-LINT-FILE-AS
   SLT-CORE-FINISH ;

: SLT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: SLT-TEST-GOOD ( -- )
   SLT-GOOD SLT-RUN-CORE 0 SLT-EXPECT-EXIT {: outu:n erru:n :}
   SLT-OUT outu SLT-EMPTY$ T$=
   SLT-ERR erru SLT-EMPTY$ T$= ;

: SLT-TEST-MISSING ( -- )
   SLT-MISSING SLT-RUN-CORE 1 SLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   SLT-OUT outu SLT-MISSING-CODE$ CONTAINS? TTRUE ;

: SLT-TEST-MISSING-JSON ( -- )
   SLT-MISSING SLT-RUN-JSON-LABEL 1 SLT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   SLT-OUT outu SLT-JSON-MISSING$ CONTAINS? TTRUE
   SLT-OUT outu SLT-JSON-LABEL$ CONTAINS? TTRUE ;

: SLT-TEST-GOOD-JSON-LABEL ( -- )
   SLT-GOOD SLT-RUN-CORE-JSON-LABEL 0 SLT-EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: SLT-TEST-OPTOUT-JSON ( -- )
   SLT-OPTOUT SLT-RUN-CORE-JSON 1 SLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   SLT-OUT outu SLT-JSON-UNVERIFIED$ CONTAINS? TTRUE ;

: SLT-TEST-MISSING-NAME ( -- )
   SLT-NAME SLT-RUN-CORE 1 SLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   SLT-OUT outu SLT-NAME-CODE$ CONTAINS? TTRUE ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-PREPARE
   SLT-TEST-GOOD
   SLT-TEST-MISSING
   SLT-TEST-MISSING-JSON
   SLT-TEST-GOOD-JSON-LABEL
   SLT-TEST-OPTOUT-JSON
   SLT-TEST-MISSING-NAME
   CLEANUP-RUN
   SLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" signature-lint-test: ok" type cr ;

SLT-MAIN
