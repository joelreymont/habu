\ diag-origin-test.f - checked fixtures for tools/diag-origin.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/lint/text.f
\ tools/lint/token.f tools/lint/lib.f tools/diag-origin-core.f
\ tools/warm-run.f tools/diag-origin-test.f

4096 constant DGT-BUF-CAP
10000 constant DGT-TIMEOUT-MS

variable DGT-ROOT-U
variable DGT-IN-U

create DGT-ROOT-BUF FS-PATH-CAP allot
create DGT-IN-BUF FS-PATH-CAP allot
create DGT-OUT DGT-BUF-CAP allot
create DGT-ERR DGT-BUF-CAP allot

: DGT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: DGT-ROOT ( -- ptr u8 n )
   DGT-ROOT-BUF DGT-ROOT-U @ ;

: DGT-IN ( -- ptr u8 n )
   DGT-IN-BUF DGT-IN-U @ ;

: DGT-LF ( -- )
   10 SB-APPEND-C ;

: DGT-DQ ( -- )
   34 SB-APPEND-C ;

: DGT-SQ-LINE ( -- )
   115 SB-APPEND-C
   DGT-DQ
   s"  : STRING ;" SB-APPEND
   DGT-DQ
   DGT-LF ;

: DGT-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  : COMMENTED ;" SB-APPEND DGT-LF
   DGT-SQ-LINE
   s" : OK ( n -- n ) dup ;" SB-APPEND DGT-LF
   s" ( : PAREN ; )" SB-APPEND DGT-LF
   s" : ;" SB-APPEND DGT-LF
   SB$ ;

: DGT-MARKER-OK ( -- )
   s" 3 3 33 DIAG-ORIGIN!" SB-APPEND DGT-LF ;

: DGT-MARKER-BAD ( -- )
   s" 5 3 69 DIAG-ORIGIN!" SB-APPEND DGT-LF ;

: DGT-WANT$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  : COMMENTED ;" SB-APPEND DGT-LF
   DGT-SQ-LINE
   DGT-LF
   DGT-MARKER-OK
   s" : OK ( n -- n ) dup ;" SB-APPEND DGT-LF
   s" ( : PAREN ; )" SB-APPEND DGT-LF
   DGT-LF
   DGT-MARKER-BAD
   s" : ;" SB-APPEND DGT-LF
   SB$ ;

: DGT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: DGT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-diag-origin" TMPDIR-MKDIR {: a:ptr u:n :}
   a u DGT-ROOT-BUF DGT-ROOT-U DGT-COPY!
   DGT-ROOT CLEANUP-DIR+
   DGT-ROOT s" input.f" DGT-IN-BUF JOIN-PATH DGT-IN-U !
   DGT-IN CLEANUP+ ;

: DGT-CAPTURE>N ( len len n n -- n n n n ) {: outu:len erru:len kind:n code:n :}
   outu LEN>N erru LEN>N kind code ;

: DGT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: DGT-RUN ( -- n n n n )
   PROC-ARGV-RESET
   s" tools/diag-origin.f" WR-TOOLS-LOAD if DGT-IN DGT-ARG+ else
   s" --load" DGT-ARG+
   s" lib/errors.f" DGT-ARG+
   s" lib/string.f" DGT-ARG+
   s" lib/memory.f" DGT-ARG+
   s" tools/lint/text.f" DGT-ARG+
   s" tools/lint/token.f" DGT-ARG+
   s" tools/lint/lib.f" DGT-ARG+
   s" tools/diag-origin-core.f" DGT-ARG+
   s" tools/diag-origin.f" DGT-ARG+
   s" --" DGT-ARG+
   DGT-IN DGT-ARG+
   then
   WR-TOOLS$ >LEN DGT-OUT DGT-BUF-CAP >LEN DGT-ERR DGT-BUF-CAP >LEN
   DGT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   DGT-CAPTURE>N ;

: DGT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n expect:n :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: DGT-RUN-CORE ( -- n )
   DGT-IN DGT-OUT DGT-BUF-CAP >LEN DIAG-ORIGIN>BUF LEN>N ;

: DGT-TEST-CORE ( -- )
   DGT-RUN-CORE {: outu:n :}
   DGT-OUT outu DGT-WANT$ T$= ;

: DGT-TEST-CLI ( -- )
   DGT-RUN 0 DGT-EXPECT-EXIT {: outu:n erru:n :}
   DGT-OUT outu DGT-WANT$ T$=
   DGT-ERR erru DGT-EMPTY$ T$= ;

: DGT-MAIN ( -- )
   T-RESET
   DGT-PREPARE
   DGT-IN DGT-SOURCE$ WRITE-ALL
   DGT-TEST-CORE
   DGT-TEST-CLI
   CLEANUP-RUN
   DGT-ROOT EXISTS? TFALSE
   T-REPORT
   s" diag-origin-test: ok" type cr ;
