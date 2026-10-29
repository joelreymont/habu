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
\ A signed `:` word of the prefix has its declaration as its row, recorded
\ without authority: after src/core/checker.f claims the source the owner
\ records it at the definition, and before the claim the engine logs it and the
\ claim records it (CK-DECLARED-LOG-DRAIN). The boot's seal pass marks it
\ internal all the same (no external row, no axiom), so the engine refuses the
\ name before the checker asks.
\
\ Run: bin/hb --load test/cold-naming-test.f

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/cold-engine.f

package COLD-NAMING-TEST
private

$1000 constant IO-CAP
120000 constant TIMEOUT-MS
70 constant REJECT-RC            \ a non-certified definition (src/core/check-hook.f)

create ROOT PATH-CAP allot    variable ROOT-U
create HOST PATH-CAP allot    variable HOST-U
create PROBE PATH-CAP allot   variable PROBE-U
create OUT IO-CAP allot
create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;
: PROBE$ ( -- ptr u8 n ) PROBE PROBE-U @ ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-cold-naming" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" hb-cold" HOST FS:JOIN-PATH HOST-U !
   ROOT$ s" probe.f" PROBE FS:JOIN-PATH PROBE-U !
   HOST$ COLD-ENGINE:PROVIDE ;

: PROBE! ( ptr u8 n -- ) {: src:ptr srcu:n :}
   PROBE$ FS:EXISTS? if PROBE$ FS:REMOVE-FILE then
   PROBE$ src srcu FS:WRITE-ALL
   PROC-ARGV-ENV-RESET ;

: SPAWN ( -- n n )   \ erru rc
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   PROBE$ >LEN PROC-ARGV+
   HOST$ >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   erru LEN>N rc ;

\ Load one definition on the cold host, after its own prefix.
: LOAD ( ptr u8 n -- n n ) PROBE! SPAWN ;

\ Four pre-hook words with axiom rows, in one load: SCOPE-FIND-AMBIGUOUS is
\ the one src/core/dict.f names in SCOPE-REC.
: AXIOM-CASES ( -- )
   s" checked bodies name PATH-CAP, E-PATH-RANGE, E-PATH-NUL and SCOPE-FIND-AMBIGUOUS, which have axiom rows" T-LABEL
   S\" : CN-CAP ( -- n ) PATH-CAP ;\n: CN-RANGE ( -- n ) E-PATH-RANGE ;\n: CN-NUL ( -- n ) E-PATH-NUL ;\n: CN-AMB ( -- n ) SCOPE-FIND-AMBIGUOUS ;\n"
   LOAD nip 0 T= ;

: NO-AXIOM-CASE ( -- )
   S\" : CN-REG ( -- n ) REG-PROT-CAP ;\n" LOAD {: erru:n rc:n :}
   s" a pre-hook word with no axiom row is not certified" T-LABEL
   rc REJECT-RC T=
   s" ... and is named as an undefined word" T-LABEL
   ERR erru s" undefined word 'REG-PROT-CAP'" CONTAINS? TTRUE ;

\ SCHEMA-REG:SCHEMA-CON (src/core/type-schema.f) is declared ( n -- n ).
: SEALED-NAME-CASE ( -- )
   S\" : CN-SCH ( n -- n ) SCHEMA-REG:SCHEMA-CON ;\n" LOAD {: erru:n rc:n :}
   s" a sealed boot refuses a declared pre-hook word by name" T-LABEL
   rc REJECT-RC T=
   ERR erru s" E-UNDEFINED: SCHEMA-REG:SCHEMA-CON" CONTAINS? TTRUE ;

\ CON-OF ( ptr u8 n -- n ) in src/core/checker.f is declared before checker.f
\ claims the source.
: SEALED-PRE-CLAIM-CASE ( -- )
   S\" : CN-CON ( ptr u8 n -- n ) CON-OF ;\n" LOAD {: erru:n rc:n :}
   s" a sealed boot refuses a word declared before the claim by name" T-LABEL
   rc REJECT-RC T=
   ERR erru s" E-UNDEFINED: CON-OF" CONTAINS? TTRUE ;

\ The source verifier tools/check.f loads names checker cells defined before the
\ hook (VERIFY-DEFINER-N, MULTI-ERR), so it loads only while each has its row,
\ and its owner bridges read the owner ABI offsets, which have none, through
\ constants it binds at top level (RECORD-SYM-OFF).
: VERIFIER-CASE ( -- )
   s" the source verifier loads on the cold host" T-LABEL
   S\" require src/habu/verify-source.f\n" LOAD nip 0 T= ;

\ The Habu loop's checked bodies throw ENGINE-ERROR's `using` failure codes
\ (src/habu/packages.f, src/habu/outer.f), constants src/core/engine-error.f
\ defines before the hook, so it loads only while each has its row. Its REPL
\ reads lines through src/habu/repl.f, which needs the host OS's terminal
\ constants; the cold host has none, so the probe includes them first, as
\ test/aot-wid-build.f does.
: INTERPRETER-SRC$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if
      S\" include src/os/linux-x86-64/repl-term.f\nrequire src/habu/interpret.f\n" exit
   then
   HB-TARGET-LINUX? if
      S\" include src/os/linux/repl-term.f\nrequire src/habu/interpret.f\n" exit
   then
   S\" include src/os/macos/repl-term.f\nrequire src/habu/interpret.f\n" ;

: INTERPRETER-CASE ( -- )
   s" the Habu loop, which names ENGINE-ERROR's using codes, loads on the cold host" T-LABEL
   INTERPRETER-SRC$ LOAD nip 0 T= ;

public

: COLD-NAMING-TEST-MAIN ( -- )
   T-RESET
   SETUP
   AXIOM-CASES
   NO-AXIOM-CASE
   SEALED-NAME-CASE
   SEALED-PRE-CLAIM-CASE
   VERIFIER-CASE
   INTERPRETER-CASE
   FS:CLEANUP-RUN
   T-REPORT
   s" cold-naming-test: ok" type cr ;

;package

COLD-NAMING-TEST:COLD-NAMING-TEST-MAIN
