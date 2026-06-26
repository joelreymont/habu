\ public-signatures-test.f - checked fixtures for tools/public-signatures.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/public-signatures-test.f

8192 constant PST-BUF-CAP
10000 constant PST-TIMEOUT-MS

variable PST-ROOT-U
variable PST-FIX-U

create PST-ROOT-BUF FS-PATH-CAP allot
create PST-FIX-BUF FS-PATH-CAP allot
create PST-OUT PST-BUF-CAP allot
create PST-ERR PST-BUF-CAP allot

: PST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PST-ROOT ( -- ptr u8 n )
   PST-ROOT-BUF PST-ROOT-U @ ;

: PST-FIX ( -- ptr u8 n )
   PST-FIX-BUF PST-FIX-U @ ;

: PST-LF ( -- )
   10 SB-APPEND-C ;

: PST-DQ ( -- )
   34 SB-APPEND-C ;

: PST-FIXTURE$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  public signature fixture" SB-APPEND PST-LF
   s" EXPORT lower" SB-APPEND PST-LF
   s" EXPORT 1+" SB-APPEND PST-LF
   s" : lower (   x -- x   ) dup ;" SB-APPEND PST-LF
   s" : CAPS ( i64 [ i64 -- i64 ] -- i64 ) execute ;" SB-APPEND PST-LF
   s" : Mixed ( i64 -- i64 ) dup ;" SB-APPEND PST-LF
   s" : 1+ ( i64 -- i64 ) 1 + ;" SB-APPEND PST-LF
   s" 99 constant KFIX" SB-APPEND PST-LF
   s" : BAD ( i64 ) dup ;" SB-APPEND PST-LF
   115 SB-APPEND-C PST-DQ s"  : STRINGED ( i64 -- i64 ) dup ;" SB-APPEND PST-DQ PST-LF
   s" ( : COMMENTED ( i64 -- i64 ) dup ; )" SB-APPEND PST-LF
   SB$ ;

: PST-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: PST-JPAIR$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: va:ptr vu ka:ptr ku :}
   SB-RESET
   PST-DQ ka ku SB-APPEND PST-DQ
   58 SB-APPEND-C
   PST-DQ va vu SB-APPEND PST-DQ
   SB$ ;

: PST-WORD$ ( ptr u8 n -- ptr u8 n )
   s" word" PST-JPAIR$ ;

: PST-SIG$ ( ptr u8 n -- ptr u8 n )
   s" signature" PST-JPAIR$ ;

: PST-SCHEMA$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" schema_version" SB-APPEND PST-DQ
   s" :1" SB-APPEND
   SB$ ;

: PST-EXPORTED-TRUE$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" exported" SB-APPEND PST-DQ
   s" :true" SB-APPEND
   SB$ ;

: PST-EXPORTED-FALSE$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" exported" SB-APPEND PST-DQ
   s" :false" SB-APPEND
   SB$ ;

: PST-USAGE$ ( -- ptr u8 n )
   s" usage: tools/public-signatures.f [--trust] file ..." ;

: PST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-public-signatures" TMPDIR-MKDIR {: a:ptr u :}
   a u PST-ROOT-BUF PST-ROOT-U PST-COPY!
   PST-ROOT CLEANUP-DIR+
   PST-ROOT s" public-signatures-fixture.f" PST-FIX-BUF JOIN-PATH PST-FIX-U !
   PST-FIX CLEANUP+
   PST-FIX PST-FIXTURE$ WRITE-ALL ;

: PST-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: PST-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/public-signatures.f" WR-TOOLS-LOAD if exit then
   s" --load" PST-ARG+
   s" lib/errors.f" PST-ARG+
   s" lib/memory.f" PST-ARG+
   s" lib/vector.f" PST-ARG+
   s" tools/lint/text.f" PST-ARG+
   s" tools/lint/intern.f" PST-ARG+
   s" tools/lint/token.f" PST-ARG+
   s" tools/lint/lib.f" PST-ARG+
   s" tools/public-signatures.f" PST-ARG+
   s" --" PST-ARG+ ;

: PST-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: PST-RUN ( ptr u8 n -- n n n n ) {: a:ptr u :}
   PST-ARGV-LOAD
   a u PST-ARG+
   WR-TOOLS$  >LEN PST-OUT PST-BUF-CAP >LEN PST-ERR PST-BUF-CAP >LEN
   PST-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME PST-CAPTURE>N ;

: PST-RUN-NOARG ( -- n n n n )
   PST-ARGV-LOAD
   WR-TOOLS$  >LEN PST-OUT PST-BUF-CAP >LEN PST-ERR PST-BUF-CAP >LEN
   PST-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME PST-CAPTURE>N ;

: PST-RUN-TRUST ( ptr u8 n -- n n n n ) {: a:ptr u :}
   PST-ARGV-LOAD
   s" --trust" PST-ARG+
   a u PST-ARG+
   WR-TOOLS$  >LEN PST-OUT PST-BUF-CAP >LEN PST-ERR PST-BUF-CAP >LEN
   PST-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME PST-CAPTURE>N ;

: PST-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: PST-TEST-GOOD ( -- )
   s" examples/llm/good.f" PST-RUN 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu PST-SCHEMA$ CONTAINS? TTRUE
   PST-OUT outu s" SQUARE" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (i64 -- i64)" PST-SIG$ CONTAINS? TTRUE
   PST-OUT outu s" APPLY" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (i64 [ i64 -- i64 ] -- i64)" PST-SIG$ CONTAINS? TTRUE ;

: PST-TEST-FIXTURE ( -- )
   PST-FIX PST-RUN 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu s" LOWER" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (x -- x)" PST-SIG$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-TRUE$ CONTAINS? TTRUE
   PST-OUT outu s" CAPS" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-FALSE$ CONTAINS? TTRUE
   PST-OUT outu s" 1+" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" MIXED" PST-WORD$ CONTAINS? TFALSE
   PST-OUT outu s" BAD" PST-WORD$ CONTAINS? TFALSE ;

: PST-SQ ( ptr u8 n -- ) {: a:ptr u :}
   115 SB-APPEND-C
   PST-DQ
   STR-SPACE SB-APPEND-C
   a u SB-APPEND
   PST-DQ ;

: PST-TRUST$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: wa:ptr wu sa:ptr su :}
   SB-RESET
   wa wu PST-SQ
   STR-SPACE SB-APPEND-C
   sa su PST-SQ
   s"  TRUST" SB-APPEND
   SB$ ;

: PST-TRUST-LOWER$ ( -- ptr u8 n )
   s" LOWER" s" x -- x" PST-TRUST$ ;

: PST-TRUST-CAPS$ ( -- ptr u8 n )
   s" CAPS" s" i64 [ i64 -- i64 ] -- i64" PST-TRUST$ ;

: PST-TRUST-KFIX$ ( -- ptr u8 n )
   s" KFIX" s" -- a" PST-TRUST$ ;

: PST-TEST-TRUST ( -- )
   PST-FIX PST-RUN-TRUST 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu PST-TRUST-LOWER$ CONTAINS? TTRUE
   PST-OUT outu PST-TRUST-CAPS$ CONTAINS? TTRUE
   PST-OUT outu PST-TRUST-KFIX$ CONTAINS? TTRUE
   PST-OUT outu s" MIXED" CONTAINS? TFALSE ;

: PST-TEST-NOARG ( -- )
   PST-RUN-NOARG 64 PST-EXPECT-EXIT {: outu erru :}
   PST-OUT outu PST-EMPTY$ T$=
   PST-ERR erru PST-USAGE$ CONTAINS? TTRUE ;

: PST-MAIN ( -- )
   T-RESET
   PST-PREPARE
   PST-TEST-GOOD
   PST-TEST-FIXTURE
   PST-TEST-TRUST
   PST-TEST-NOARG
   CLEANUP-RUN
   PST-ROOT EXISTS? TFALSE
   T-REPORT
   s" public-signatures-test: ok" type cr ;

PST-MAIN
