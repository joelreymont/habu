\ diag-origin-test.f - checked fixtures for tools/diag-origin.f.
\ Run: bin/hb --load tools/diag-origin-test.f

4096 constant DGT-BUF-CAP
180000 constant DGT-TIMEOUT-MS       \ includes checked compilation of the CLI

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
   s"  3 3 33 DIAG-ORIGIN! " SB-APPEND ;

: DGT-MARKER-BAD ( -- )
   s"  5 3 69 DIAG-ORIGIN! " SB-APPEND ;

\ Each marker sits on its definition's line: no source line moves.
: DGT-WANT$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  : COMMENTED ;" SB-APPEND DGT-LF
   DGT-SQ-LINE
   DGT-MARKER-OK
   s" : OK ( n -- n ) dup ;" SB-APPEND DGT-LF
   s" ( : PAREN ; )" SB-APPEND DGT-LF
   DGT-MARKER-BAD
   s" : ;" SB-APPEND DGT-LF
   SB$ ;

: DGT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

\ `create :` names a word `:`, so the next line holds the only definition.
: DGT-DEF-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" create :" SB-APPEND DGT-LF
   s" : F ( -- n ) 7 ;" SB-APPEND DGT-LF
   s" F ." SB-APPEND DGT-LF
   SB$ ;

\ No marker stands between `create` and its name: both colons are marked
\ before the words that end at them, and F's marker, the later, is in force.
: DGT-DEF-WANT$ ( -- ptr u8 n )
   SB-RESET
   s"  2 1 9 DIAG-ORIGIN!  2 3 11 DIAG-ORIGIN! create :" SB-APPEND DGT-LF
   s" : F ( -- n ) 7 ;" SB-APPEND DGT-LF
   s" F ." SB-APPEND DGT-LF
   SB$ ;

\ A definer the file defines names `:` the same way.
: DGT-USER-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : MK ( n -- ) create , does> ( -- ptr n ) ;" SB-APPEND DGT-LF
   s" 5 MK :" SB-APPEND DGT-LF
   s" : G ( -- n ) 8 ;" SB-APPEND DGT-LF
   s" G ." SB-APPEND DGT-LF
   SB$ ;

: DGT-LINE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   SB-APPEND DGT-LF
   SB$ ;

: DGT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-diag-origin" HB-TMP-MKDIR {: a:ptr u:n :}
   a u DGT-ROOT-BUF DGT-ROOT-U DGT-COPY!
   DGT-ROOT CLEANUP-DIR+ ;

\ Write a case's source in the case directory; DGT-IN names it until the next.
: DGT-INPUT! ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n src:ptr srcu:n :}
   DGT-ROOT name nameu DGT-IN-BUF JOIN-PATH DGT-IN-U !
   DGT-IN CLEANUP+
   DGT-IN src srcu WRITE-ALL ;

: DGT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: DGT-RUN ( ptr u8 n -- len len outcome )
   {: tool:ptr toolu:n :}
   PROC-ARGV-RESET
   s" --load" DGT-ARG+
   tool toolu DGT-ARG+
   s" --" DGT-ARG+
   DGT-IN DGT-ARG+
   ENGINE-CANDIDATE:PATH$ >LEN DGT-OUT DGT-BUF-CAP >LEN DGT-ERR DGT-BUF-CAP >LEN
   DGT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME ;

\ Run a tool on DGT-IN, require exit 0 and an empty stderr, and answer the
\ length of its stdout in DGT-OUT.
: DGT-RUN-OK ( ptr u8 n -- n )
   {: tool:ptr toolu:n :}
   tool toolu DGT-RUN {: outu:len erru:len oc :}
   tool toolu DGT-OUT outu LEN>N DGT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   DGT-ERR erru LEN>N DGT-EMPTY$ T$=
   outu LEN>N ;

: DGT-TEST-CLI ( -- )
   s" input.f" DGT-SOURCE$ DGT-INPUT!
   s" tools/diag-origin.f" DGT-RUN-OK {: outu:n :}
   DGT-OUT outu DGT-WANT$ T$= ;

: DGT-TEST-DEFINER-MARKS ( -- )
   s" definer.f" DGT-DEF-SOURCE$ DGT-INPUT!
   s" tools/diag-origin.f" DGT-RUN-OK {: outu:n :}
   DGT-OUT outu DGT-DEF-WANT$ T$= ;

\ check.f runs what it checks, so a file that loads prints the same there.
: DGT-TEST-DEFINER-CHECKS ( -- )
   s" tools/check.f" DGT-RUN-OK {: outu:n :}
   DGT-OUT outu s" 7" DGT-LINE$ T$= ;

: DGT-TEST-USER-DEFINER ( -- )
   s" user-definer.f" DGT-USER-SOURCE$ DGT-INPUT!
   s" tools/check.f" DGT-RUN-OK {: outu:n :}
   DGT-OUT outu s" 8" DGT-LINE$ T$= ;

: DGT-MAIN ( -- )
   T-RESET
   DGT-PREPARE
   DGT-TEST-CLI
   DGT-TEST-DEFINER-MARKS
   DGT-TEST-DEFINER-CHECKS
   DGT-TEST-USER-DEFINER
   CLEANUP-RUN
   DGT-ROOT EXISTS? TFALSE
   T-REPORT
   s" diag-origin-test: ok" type cr ;
