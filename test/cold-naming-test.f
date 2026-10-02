\ cold-naming-test.f - what a checked body may name from before the check hook.
\
\ The core prefix up to src/core/check-hook.f is compiled before a checker
\ exists. An engine that boots that prefix from source holds those words in its
\ dictionary and not in its checker, so a checked body that names one is
\ E-UNDEFINED there unless an axiom row states the word's effect: a PRIM: row
\ in src/core/cell-effects.f or src/core/checker.f. A
\ tools/native-build.f engine certifies the same body, because its checker
\ holds the rows the build host recorded for the prefix. Only a from-source
\ boot shows the refusal, so every case here is a load on the cold fixture host
\ (test/cold-engine.f), which reads this tree's prefix.
\
\ Run: bin/hb --load test/cold-naming-test.f

require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/cold-engine.f

package COLD-NAMING-TEST
private

$1000 constant IO-CAP
120000 constant TIMEOUT-MS
70 constant REJECT-RC            \ a non-certified definition (src/core/check-hook.f)

create ROOT FS-PATH-CAP allot    variable ROOT-U
create HOST FS-PATH-CAP allot    variable HOST-U
create PROBE FS-PATH-CAP allot   variable PROBE-U
create OUT IO-CAP allot
create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;
: PROBE$ ( -- ptr u8 n ) PROBE PROBE-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-cold-naming" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-cold" HOST JOIN-PATH HOST-U !
   ROOT$ s" probe.f" PROBE JOIN-PATH PROBE-U !
   HOST$ COLD-ENGINE:PROVIDE ;

\ Load one definition on the cold host, after its own prefix.
: LOAD ( ptr u8 n -- n n ) {: src:ptr srcu:n :}   \ erru rc
   PROBE$ EXISTS? if PROBE$ REMOVE-FILE then
   PROBE$ src srcu WRITE-ALL
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   PROBE$ >LEN PROC-ARGV+
   HOST$ >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   erru LEN>N rc ;

: AXIOM-CASES ( -- )
   s" a checked body names PATH-CAP, which has an axiom row" T-LABEL
   S\" : CN-CAP ( -- n ) PATH-CAP ;\n" LOAD nip 0 T=
   s" ... and E-PATH-RANGE, which has one too" T-LABEL
   S\" : CN-RANGE ( -- n ) E-PATH-RANGE ;\n" LOAD nip 0 T= ;

: NO-AXIOM-CASE ( -- )
   S\" : CN-REG ( -- n ) REG-PROT-CAP ;\n" LOAD {: erru:n rc:n :}
   s" a pre-hook word with no axiom row is not certified" T-LABEL
   rc REJECT-RC T=
   s" ... and is named as an undefined word" T-LABEL
   ERR erru s" undefined word 'REG-PROT-CAP'" CONTAINS? TTRUE ;

\ The source verifier tools/check.f loads names checker cells defined before the
\ hook (VERIFY-DEFINER-N, MULTI-ERR), so it loads only while each has its row.
: VERIFIER-CASE ( -- )
   s" the source verifier loads on the cold host" T-LABEL
   S\" require src/habu/verify-source.f\n" LOAD nip 0 T= ;

public

: COLD-NAMING-TEST-MAIN ( -- )
   T-RESET
   SETUP
   AXIOM-CASES
   NO-AXIOM-CASE
   VERIFIER-CASE
   CLEANUP-RUN
   T-REPORT
   s" cold-naming-test: ok" type cr ;

;package

COLD-NAMING-TEST:COLD-NAMING-TEST-MAIN
