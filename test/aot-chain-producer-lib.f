\ aot-chain-producer-lib.f - the fixture the chain producer gate rows share.
\ Loaded by test/aot-chain-producer-suite.f, test/aot-chain-location-suite.f
\ and test/aot-chain-target-suite.f: a private source-built host, a copy of
\ tools/aot-chain-capture.f that ends in test/aot-chain-row-checks.f, and the
\ case runner. Every case captures the whole compiler in its own child, so the
\ cases are split across rows and each row builds its own host.

\ tools/build-fixpoint.f requires none of the libraries its header lists.
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/aot-chain-capture-lib.f

package AOT-CHAIN-SUITE

\ Run the actual producer in a source-only host. Its private copy adds two
\ declarations around the window and replaces only the final MAIN invocation
\ with row checks; no production test switch or alternate row writer is used.
$10000 constant TOOL-CAP
create TOOL-SOURCE TOOL-CAP allot variable TOOL-U
create TOOL-PATH FS-PATH-CAP allot variable TOOL-PATH-U
create HOST-PATH FS-PATH-CAP allot variable HOST-PATH-U

: TOOL$ ( -- ptr u8 n ) TOOL-PATH TOOL-PATH-U @ ;
: HOST$ ( -- ptr u8 n ) HOST-PATH HOST-PATH-U @ ;
: TOOL+ ( ptr u8 n -- ) {: a:ptr u:n :} TOOL$ a u APPEND-FILE ;
: TOOL-PART ( n n -- ) {: start:n finish:n :}
   TOOL-SOURCE start + finish start - TOOL+ ;

: TOOL-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   TOOL-SOURCE TOOL-U @ a u FIND-SUB MATCH option
      some OF IDX>N ENDOF
      none OF s" chain-address-rows: producer source boundary missing" 75 die ENDOF
   ;MATCH ;

: PREPARE-PRODUCER ( -- )
   ROOT$ s" row-producer.f" TOOL-PATH JOIN-PATH TOOL-PATH-U !
   ROOT$ s" hb-stdin" HOST-PATH JOIN-PATH HOST-PATH-U !
   s" tools/aot-chain-capture.f" TOOL-SOURCE TOOL-CAP READ-ALL TOOL-U !
   S\" AOT-CHAIN:OPEN\n" TOOL-AT {: opened:n :}
   S\" AOT-CHAIN:CLOSE\n" TOOL-AT {: closed:n :}
   S\" AOT-CHAIN:MAIN\n" TOOL-AT {: called:n :}
   called S\" AOT-CHAIN:MAIN\n" nip + TOOL-U @ T=
   TOOL$ TOOL-SOURCE opened WRITE-ALL
   S\" package CHAIN-ROW-OUTSIDE\npublic\nPERSISTED-PTR-VARIABLE SLOT\n;package\n" TOOL+
   opened closed TOOL-PART
   S\" package CHAIN-ROW-INSIDE\npublic\nPERSISTED-PTR-VARIABLE NIL\nvariable TARGET\n;package\nCHAIN-ROW-INSIDE:TARGET CHAIN-ROW-OUTSIDE:SLOT !\n" TOOL+
   closed called TOOL-PART
   S\" require test/aot-chain-row-checks.f\n" TOOL+
   ROOT$ BUILD-FIXPOINT:BF-TMP!
   BUILD-FIXPOINT:BF-PREFLIGHT
   BUILD-FIXPOINT:BF-STAGE-FIXPOINT
   s" src/habu/stdin.f" BUILD-FIXPOINT:BF-EMIT-ENGINE
   BUILD-FIXPOINT:BF-TMP-RESET ;

: PRODUCER-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   HOST$ RUN-ENGINE
   want ROW-RC
   want 0= if s" chain-address-rows: ok" SAID? else
      s" declared address rows do not match the live window" ERR-SAID?
   then ;

;package
