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

\ A tool on the input, its standard output captured into the given buffer.
: DGT-RUN ( ptr u8 n ptr u8 n -- len len outcome )
   {: tool:ptr toolu:n out:ptr outcap:n :}
   PROC-ARGV-RESET
   s" --load" DGT-ARG+
   tool toolu DGT-ARG+
   s" --" DGT-ARG+
   DGT-IN DGT-ARG+
   ENGINE-CANDIDATE:PATH$ >LEN out outcap >LEN DGT-ERR DGT-BUF-CAP >LEN
   DGT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME ;

\ Run a tool on DGT-IN, require exit 0 and an empty stderr, and answer the
\ length of its stdout in DGT-OUT.
: DGT-RUN-OK ( ptr u8 n -- n )
   {: tool:ptr toolu:n :}
   tool toolu DGT-OUT DGT-BUF-CAP DGT-RUN {: outu:len erru:len oc :}
   tool toolu DGT-OUT outu LEN>N DGT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   DGT-ERR erru LEN>N DGT-EMPTY$ T$=
   outu LEN>N ;

\ The diag-origin run, its output captured into out, exited with the expected
\ status; its output and error lengths.
: DGT-EXPECT-EXIT ( ptr u8 len len outcome n -- n n )
   {: out:ptr outu:len erru:len oc expect:n :}
   s" tools/diag-origin.f" out outu LEN>N DGT-ERR erru LEN>N oc expect
   T-OUTCOME-EXITED=
   outu LEN>N erru LEN>N ;

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

\ The CLI reads a source as large as tools/check.f reads, $100000 bytes, and
\ marks the definition at its end; a byte more is refused, naming the file.
\ The allocation holds the source, then the capture of its output.
$100000 constant DGT-CAP-LEN
DGT-CAP-LEN 1+ constant DGT-OVERCAP-LEN
DGT-CAP-LEN DGT-BUF-CAP + constant DGT-SIZED-OUT-CAP
DGT-OVERCAP-LEN DGT-SIZED-OUT-CAP + constant DGT-SIZED-ALLOC

: DGT-SIZED-DEF$ ( -- ptr u8 n )
   s" : DGT-SIZED ( -- ) ;" ;

: DGT-MARKED-DEF$ ( -- ptr u8 n )
   SB-RESET
   s" DIAG-ORIGIN! " SB-APPEND
   DGT-SIZED-DEF$ SB-APPEND
   SB$ ;

\ Blank lines, then the definition.
: DGT-SIZED-FILL ( ptr u8 n -- ) {: a:ptr u:n :}
   DGT-SIZED-DEF$ {: d:ptr du:n :}
   u du - 0 ?do $0a a i + c! loop
   d a u + du - du BYTE-COPY ;

: DGT-SIZED-RUN ( ptr u8 n ptr u8 -- ptr u8 len len outcome ) {: src:ptr u:n out:ptr :}
   src u DGT-SIZED-FILL
   DGT-IN src u WRITE-ALL
   out s" tools/diag-origin.f" out DGT-SIZED-OUT-CAP DGT-RUN ;

: DGT-CAP-CASE ( ptr u8 ptr u8 -- ) {: src:ptr out:ptr :}
   src DGT-CAP-LEN out DGT-SIZED-RUN 0 DGT-EXPECT-EXIT {: outu:n erru:n :}
   out outu src DGT-CAP-LEN DGT-SIZED-DEF$ nip - STARTS-WITH? TTRUE
   out outu DGT-MARKED-DEF$ ENDS-WITH? TTRUE
   DGT-ERR erru DGT-EMPTY$ T$= ;

: DGT-OVERCAP-CASE ( ptr u8 ptr u8 -- ) {: src:ptr out:ptr :}
   src DGT-OVERCAP-LEN out DGT-SIZED-RUN 1 DGT-EXPECT-EXIT {: outu:n erru:n :}
   DGT-ERR erru s" file exceeds buffer" CONTAINS? TTRUE ;

: DGT-SIZED-BODY ( ptr u8 NUM:alloc-byte-len -- ) {: a:ptr extent:NUM:alloc-byte-len :}
   a DGT-OVERCAP-LEN + {: out:ptr :}
   a out DGT-CAP-CASE
   a out DGT-OVERCAP-CASE ;

: DGT-TEST-SIZED ( -- )
   DGT-SIZED-ALLOC MEM:BYTES-ALLOC-LEN [: DGT-SIZED-BODY ;] MEM:WITH-BYTES ;

: DGT-MAIN ( -- )
   T-RESET
   DGT-PREPARE
   DGT-TEST-CLI
   DGT-TEST-DEFINER-MARKS
   DGT-TEST-DEFINER-CHECKS
   DGT-TEST-USER-DEFINER
   DGT-TEST-SIZED
   CLEANUP-RUN
   DGT-ROOT EXISTS? TFALSE
   T-REPORT
   s" diag-origin-test: ok" type cr ;
