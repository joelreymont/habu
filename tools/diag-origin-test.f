\ diag-origin-test.f - checked fixtures for tools/diag-origin.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/diag-origin-test.f

4096 constant DGT-BUF-CAP

variable DGT-ROOT-U
variable DGT-IN-U

create DGT-ROOT-BUF FS-PATH-CAP allot
create DGT-IN-BUF FS-PATH-CAP allot
create DGT-OUT DGT-BUF-CAP allot
create DGT-ERR DGT-BUF-CAP allot

: DGT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
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
   s" habu-diag-origin" TMPDIR-MKDIR {: a:ptr u :}
   a u DGT-ROOT-BUF DGT-ROOT-U DGT-COPY!
   DGT-ROOT CLEANUP-DIR+
   DGT-ROOT s" input.f" DGT-IN-BUF JOIN-PATH DGT-IN-U !
   DGT-IN CLEANUP+ ;

: DGT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: DGT-RUN ( -- n n n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/diag-origin.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   DGT-IN  >LEN PROC-ARGV+
   s" bin/hb" >LEN DGT-OUT DGT-BUF-CAP >LEN DGT-ERR DGT-BUF-CAP >LEN 1000 >MS RUN-ARGV-CAPTURE
   DGT-CAPTURE>N ;

: DGT-MAIN ( -- )
   T-RESET
   DGT-PREPARE
   DGT-IN DGT-SOURCE$ WRITE-ALL
   DGT-RUN 0 T=
   {: outu erru :}
   DGT-OUT outu DGT-WANT$ T$=
   DGT-ERR erru DGT-EMPTY$ T$=
   CLEANUP-RUN
   DGT-ROOT EXISTS? TFALSE
   T-REPORT
   s" diag-origin-test: ok" type cr ;

DGT-MAIN
