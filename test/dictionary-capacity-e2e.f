\ A checked design-sized dictionary survives lookup, overlay, and REPL save/reopen.
\ Run: bin/hb --load test/dictionary-capacity-e2e.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package DICTIONARY-CAPACITY-TEST

53000 constant WORDS
2000000 constant SOURCE-CAP
8192 constant CAPTURE-CAP
create SOURCE SOURCE-CAP allot  variable SOURCE-U
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAPTURE-CAP allot  variable OUT-U
create ERR CAPTURE-CAP allot  variable ERR-U

: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr size:n :}
   ROOT ROOT-U @ name size PATH JOIN-PATH PATH swap ;

: SOURCE-RESET ( -- ) 0 SOURCE-U ! ;

: SOURCE-C ( n -- )
   SOURCE-U @ SOURCE-CAP >= if E-TEST-CAPACITY throw then
   SOURCE SOURCE-U @ + c!
   SOURCE-U @ 1+ SOURCE-U ! ;

: SOURCE+ ( ptr u8 n -- )
   {: a:ptr size:n :}
   SOURCE-U @ size + SOURCE-CAP > if E-TEST-CAPACITY throw then
   a SOURCE SOURCE-U @ + size BYTE-COPY
   SOURCE-U @ size + SOURCE-U ! ;

: SOURCE-LINE ( ptr u8 n -- ) SOURCE+ 10 SOURCE-C ;

: DIGITS ( n -- )
   {: number:n :}
   100000
   begin dup 0 > while
      number over / 10 mod 48 + SOURCE-C
      10 /
   repeat drop ;

: DEFINITION ( n -- )
   {: index:n :}
   s" : DC" SOURCE+ index DIGITS
   s"  ( -- n ) " SOURCE+
   index 0 = if s" 7" SOURCE+ else
      index WORDS 1- = if s" 35" SOURCE+ else s" 1" SOURCE+ then
   then
   s"  ;" SOURCE-LINE ;

: WRITE-SOURCE ( -- )
   SOURCE-RESET
   WORDS 0 ?do i DEFINITION loop
   s" : CAP-SUM ( -- n ) DC000000 DC052999 + ;" SOURCE-LINE
   s" source.f" AT SOURCE SOURCE-U @ WRITE-ALL ;

: WRITE-CHECK ( -- )
   SOURCE-RESET
   s" require src/habu/verify-source.f" SOURCE-LINE
   s" include " SOURCE+
   s" source.f" AT SOURCE+ 10 SOURCE-C
   s" ndict@ 65536 > . CAP-SUM ." SOURCE-LINE
   s" CHECKER-SCOPE-START-NEUTRAL" SOURCE-LINE
   s" s" SOURCE+ 34 SOURCE-C s"  : CAP-OVER ( -- n ) DC052999 ;" SOURCE+
   34 SOURCE-C s"  VERIFY:SOURCE-BUF-IN-SCOPE" SOURCE-LINE
   s" s" SOURCE+ 34 SOURCE-C s"  CAP-PROBE ( -- n ) CAP-OVER" SOURCE+
   34 SOURCE-C s"  VERIFY:CANDIDATE-IN-SCOPE ." SOURCE-LINE
   s" CHECKER-SCOPE-DONE" SOURCE-LINE
   s" check.f" AT SOURCE SOURCE-U @ WRITE-ALL ;

: WRITE-APP ( -- )
   SOURCE-RESET
   s" include " SOURCE+
   s" source.f" AT SOURCE+ 10 SOURCE-C
   s" : MAIN ( -- ) CAP-SUM . ;" SOURCE-LINE
   s" app.f" AT SOURCE SOURCE-U @ WRITE-ALL ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   PROC-ENV-INHERIT-MISSING
   exe exeu >LEN stdin stdinu >LEN
   OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: CHECK-LOAD ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" ARG s" check.f" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE SUCCESS
   OUT OUT-U @ S\" -1\n42\n-1\n" CONTAINS? TTRUE ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG
   s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE SUCCESS ;

: REOPEN ( -- )
   PROC-ARGV-ENV-RESET
   s" app-hb" AT
   S\" CAP-SUM .\n: CAP-FRESH ( -- n ) DC052999 1+ ;\nCAP-FRESH .\n"
   CAPTURE SUCCESS
   OUT OUT-U @ S\" 42\n36\n" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" 53,000 checked definitions cross 65,536 total records and survive REPL save" T-LABEL
   s" habu-dictionary-capacity" HB-TMP-MKDIR {: a:ptr size:n :}
   a ROOT size BYTE-COPY size ROOT-U !
   s" cache" AT MAKE-DIR
   WRITE-SOURCE
   WRITE-CHECK
   WRITE-APP
   CHECK-LOAD
   BUILD
   REOPEN
   T-REPORT
   s" dictionary-capacity-e2e: ok" type cr ;
;package

DICTIONARY-CAPACITY-TEST:MAIN
