\ native-rstack-input.f - no signature written in source declares a
\ return-stack cell, at either tier.
\
\ A definition reads only the return-stack cells it pushed with `>r`; a written
\ `|` clause carries one row variable, the same on both sides (docs/effects.md
\ "The return-stack clause"). The checker refuses any other clause at
\ the declaration, so tier 0 and tier 1 refuse the same program with the same
\ diagnostic before either runs it.
\
\ WAYS THE RULE CAN FAIL, and where each is held:
\   - a cell declared on the input side, the output side or both is admitted
\     (rs1, rs2 below);
\   - a TRUSTED: declaration, which is stored rather than checked, escapes it
\     (rs1's RT1 alone);
\   - a quotation type nested in a signature moves a cell (RUNQ);
\   - a quotation literal, which declares nothing, pushes a cell its caller
\     pops or pops one its caller pushed (QPUSH, QPOP);
\   - an unsigned definition, which declares nothing, reads or pops a cell its
\     caller pushed (UREAD, UPOP);
\   - a bar on one side only, which pairs the written row with a fresh one, is
\     admitted, and so are its callers (ONE);
\   - the neutral spellings lib/c2-memory.f uses, `| U -- S | U` and
\     `[ R ... -- S ... | U -- U ]`, stop certifying (APPLY, and that library's
\     own suites);
\   - a primitive's return row (`>r`, `r>`) is refused with the signatures, so
\     a body that parks a value stops compiling (PARK);
\   - the tiers diverge: one admits and runs what the other refuses.
\
\ Every case is one program run through a real engine process, stdin-piped, at
\ tier 0 and at tier 1. A refused program exits 70, prints nothing, and its
\ stderr begins with the checker's diagnostic at both tiers. An admitted program
\ exits 0 and prints exactly the case's text at both tiers. A refused
\ `generates:` row is a throw rather than a refusal to certify: it exits 67, the
\ uncaught-throw code, with the row's diagnostic and the throw's code.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f

package NRS-INPUT-TEST
private

$4000 constant CAP
20000 constant TIMEOUT-MS
70 constant REFUSED-RC
67 constant UNCAUGHT-RC

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: TIER-LINE ( n -- ptr u8 n )
   0= if S\" 0 set-tier\n" exit then
   S\" 1 set-tier\n" ;

\ The program a run feeds the engine: the tier line, then the case.
: AT-TIER ( ptr u8 n n -- ptr u8 n ) {: src:ptr u:n tier:n :}
   SB-RESET
   tier TIER-LINE SB-APPEND
   src u SB-APPEND
   SB$ ;

: EXEC ( ptr u8 n -- len len outcome ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  src u >LEN  OUT CAP >LEN  ERR CAP >LEN
   TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: SHOW ( ptr u8 n n -- ) {: src:ptr u:n before:n :}
   T-FAILURES before = if exit then
   s" program:" type cr src u type
   s" stderr:" type cr ERR$ type ;

\ One admitted run: the child exits 0 and prints exactly WANT.
: ADMITTED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr lu:n src:ptr u:n want:ptr wu:n :}
   T-FAILURES {: before:n :}
   src u EXEC {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   label lu T-LABEL  src u OUT$ ERR$ oc 0 T-OUTCOME-EXITED=
   label lu T-LABEL  OUT$ want wu T$=
   src u before SHOW ;

\ One refused run: the child exits RC, prints nothing, and its stderr begins
\ with DIAG. Tier 1 adds the native compiler's own line after it.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: label:ptr lu:n src:ptr u:n diag:ptr du:n rc:n :}
   T-FAILURES {: before:n :}
   src u EXEC {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   label lu T-LABEL  src u OUT$ ERR$ oc rc T-OUTCOME-EXITED=
   label lu T-LABEL  OUT$ s" " T$=
   label lu T-LABEL  ERR ERR-U @ du min diag du T$=
   src u before SHOW ;

: ADMITTED-BOTH ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr lu:n src:ptr u:n want:ptr wu:n :}
   2 0 do
      label lu  src u i AT-TIER  want wu  ADMITTED
   loop ;

: REFUSED-AT ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: label:ptr lu:n src:ptr u:n diag:ptr du:n rc:n :}
   2 0 do
      label lu  src u i AT-TIER  diag du  rc REFUSED
   loop ;

: REFUSED-BOTH ( ptr u8 n ptr u8 n ptr u8 n -- ) REFUSED-RC REFUSED-AT ;
: THROWN-BOTH ( ptr u8 n ptr u8 n ptr u8 n -- ) UNCAUGHT-RC REFUSED-AT ;

\ ---- refused at the declaration ----------------------------------------------
\ rs1 and rs2 certified and ran at tier 0 and failed in the native compiler at
\ tier 1 (E-NELAB-UNDER); RUNQ certified and ran at tier 0; ONE certified at
\ both tiers; QPUSH and QPOP certified at tier 0 and failed in the native
\ compiler at tier 1 (-8304, -8503); UREAD certified at tier 0 and failed in
\ the native compiler at tier 1 (-8304).
: REFUSALS ( -- )
   s" rs1: a body replaces a declared return-stack cell"
   S\" : RC1 ( | n -- | n ) r> drop 0 >r ;\nTRUSTED: RT1 ( | n -- | n ) r> drop 0 >r ;\n: USE ( -- n ) 5 >r RC1 r> ;\nUSE . cr\n"
   S\" habu: in rc1: return-stack effect in signature ' | n -- | n '; after '|' write one row variable, the same on both sides, or no '|'\nhook: non-certified definition: rc1 at ' | n -- | n '\n"
   REFUSED-BOTH
   s" rs1: a TRUSTED: body replaces a declared return-stack cell"
   S\" TRUSTED: RT1 ( | n -- | n ) r> drop 0 >r ;\n: USE ( -- n ) 5 >r RT1 r> ;\nUSE . cr\n"
   S\" habu: in rt1: bad stored signature '| n -- | n'; return-stack effect: after '|' write one row variable, the same on both sides, or no '|'\n"
   REFUSED-BOTH
   s" rs2: a body pops a declared return-stack input"
   S\" : RP ( | n -- n | ) r> ;\n: USE ( -- n ) 5 >r RP ;\nUSE . cr\n"
   S\" habu: in rp: return-stack effect in signature ' | n -- n | '; after '|' write one row variable, the same on both sides, or no '|'\nhook: non-certified definition: rp at ' | n -- n | '\n"
   REFUSED-BOTH
   s" a quotation type moves a cell to the return stack"
   S\" : RUNQ ( n [ n -- | U -- U n ] | U -- n | U ) execute r> ;\n: USE ( -- n ) 5 [: >r ;] RUNQ ;\nUSE . cr\n"
   S\" habu: in runq: return-stack effect in signature ' n [ n -- | U -- U n ] | U -- n | U '; after '|' write one row variable, the same on both sides, or no '|'\nhook: non-certified definition: runq at ' n [ n -- | U -- U n ] | U -- n | U '\n"
   REFUSED-BOTH
   s" a generates: row declares a return-stack cell"
   S\" package GP\n: G ( n -- ) drop ;\ngenerates: G ( | -- | n )\npublic\n;package\n"
   S\" E-GENERATES-ROW habu: generates: row for 'G': return-stack effect: after '|' write one row variable, the same on both sides, or no '|'\nhb: uncaught throw code 7153\n"
   THROWN-BOTH
   s" a bar on one side only"
   S\" : ONE ( R n | S -- R ) drop ;\n"
   S\" habu: in one: return-stack effect in signature ' R n | S -- R '; after '|' write one row variable, the same on both sides, or no '|'\nhook: non-certified definition: one at ' R n | S -- R '\n"
   REFUSED-BOTH
   s" a quotation literal pushes a cell its caller pops"
   S\" : QPUSH ( n -- n ) [: >r ;] execute r> ;\n: USE ( -- n ) 5 QPUSH ;\nUSE . cr\n"
   S\" habu: in qpush: at ';]'\nhook: non-certified definition: qpush at ';]'\n"
   REFUSED-BOTH
   s" a quotation literal pops a cell its caller pushed"
   S\" : QPOP ( n -- n ) >r [: r> ;] execute ;\n: USE ( -- n ) 5 QPOP ;\nUSE . cr\n"
   S\" habu: in qpop: at ';]'\nhook: non-certified definition: qpop at ';]'\n"
   REFUSED-BOTH
   s" an unsigned definition reads a cell it did not push"
   S\" : UREAD r@ ;\n"
   S\" habu: in uread: at 'r@'\nhook: non-certified definition: uread at 'r@'\n"
   REFUSED-BOTH
   s" an unsigned definition pops a cell it did not push"
   S\" : UPOP r> ;\n"
   S\" habu: in upop: at 'r>'\nhook: non-certified definition: upop at 'r>'\n"
   REFUSED-BOTH ;

\ ---- still admitted -----------------------------------------------------------
: ADMISSIONS ( -- )
   s" a neutral clause at the top and in a quotation type"
   S\" : APPLY ( R n [ R n -- S n | U -- U ] | U -- S n | U ) execute ;\n: USE ( -- n ) 3 [: 1+ ;] APPLY ;\nUSE . cr\n"
   S\" 4\n\n" ADMITTED-BOTH
   s" a body parks a value with >r and takes it back"
   S\" : PARK ( n n -- n ) >r 10 * r> + ;\n: USE ( -- n ) 4 2 PARK ;\nUSE . cr\n"
   S\" 42\n\n" ADMITTED-BOTH ;

public

: RUN ( -- )
   T-RESET
   REFUSALS
   ADMISSIONS
   T-REPORT
   s" native-rstack-input: ok" type cr ;

;package

NRS-INPUT-TEST:RUN
