\ native-rstack-input.f - no signature written in source declares a return-stack
\ cell: a written `|` clause carries one row variable, the same on both sides
\ (docs/forth.md "The return-stack clause"), and the checker refuses any other
\ clause at the declaration. Each case is one stdin-piped engine run.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f

package NRS-INPUT-TEST
private

$4000 constant CAP
70 constant REFUSED-RC

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: OUT$ ( -- ^u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ^u8 n ) ERR ERR-U @ ;

: EXEC ( ^u8 n -- len len PROCESS:ending ) {: src:^ u:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  src u >LEN  OUT CAP >LEN  ERR CAP >LEN
   PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: SHOW ( ^u8 n n -- ) {: src:^ u:n before:n :}
   T-FAILURES before = if exit then
   s" program:" type cr src u type
   s" stderr:" type cr ERR$ type ;

\ One admitted run: the child exits 0 and prints exactly WANT.
: ADMITTED ( ^u8 n ^u8 n ^u8 n -- )
   {: label:^ lu:n src:^ u:n want:^ wu:n :}
   T-FAILURES {: before:n :}
   src u EXEC {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   label lu T-LABEL  src u OUT$ ERR$ oc 0 T-ENDING-EXITED=
   label lu T-LABEL  OUT$ want wu T$=
   src u before SHOW ;

\ One refused run: the child exits RC, prints nothing, and its stderr begins
\ with DIAG, the record's headline, and holds NOTE.
: REFUSED ( ^u8 n ^u8 n ^u8 n ^u8 n n -- )
   {: label:^ lu:n src:^ u:n diag:^ du:n note:^ nu:n rc:n :}
   T-FAILURES {: before:n :}
   src u EXEC {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   label lu T-LABEL  src u OUT$ ERR$ oc rc T-ENDING-EXITED=
   label lu T-LABEL  OUT$ s" " T$=
   label lu T-LABEL  ERR ERR-U @ du min diag du T$=
   label lu T-LABEL  ERR$ note nu CONTAINS? TTRUE
   src u before SHOW ;

\ ---- refused at the declaration ----------------------------------------------
\ Held here: an input, output or both-sides cell (rs1, rs2); a TRUSTED:
\ declaration, stored rather than checked (RT1); a nested quotation type (RUNQ);
\ a quotation literal or unsigned definition pushing or popping a caller's cell
\ (QPUSH, QPOP, UREAD, UPOP); a primitive's return row, so a body that parks a
\ value stops compiling (PARK).
: REFUSALS ( -- )
   s" rs1: a body replaces a declared return-stack cell"
   S\" : RC1 ( | n -- | n ) r> drop 0 >r ;\nTRUSTED: RT1 ( | n -- | n ) r> drop 0 >r ;\n: USE ( -- n ) 5 >r RC1 r> ;\nUSE . cr\n"
   s" error[E-BAD-SIGNATURE]: return-stack effect in signature ` | n -- | n `"
   s" --> <input>:1:8"  REFUSED-RC REFUSED
   s" rs1: a TRUSTED: body replaces a declared return-stack cell"
   S\" TRUSTED: RT1 ( | n -- | n ) r> drop 0 >r ;\n: USE ( -- n ) 5 >r RT1 r> ;\nUSE . cr\n"
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `RT1` cannot be recorded"
   s" |          ^^^ its signature moves the return stack"  REFUSED-RC REFUSED
   s" rs2: a body pops a declared return-stack input"
   S\" : RP ( | n -- n | ) r> ;\n: USE ( -- n ) 5 >r RP ;\nUSE . cr\n"
   s" error[E-BAD-SIGNATURE]: return-stack effect in signature ` | n -- n | `"
   s" --> <input>:1:7"  REFUSED-RC REFUSED
   s" a quotation type moves a cell to the return stack"
   S\" : RUNQ ( n [ n -- | U -- U n ] | U -- n | U ) execute r> ;\n: USE ( -- n ) 5 [: >r ;] RUNQ ;\nUSE . cr\n"
   s" error[E-BAD-SIGNATURE]: return-stack effect in signature ` n [ n -- | U -- U n ] | U -- n | U `"
   s" --> <input>:1:9"  REFUSED-RC REFUSED
   s" a generates: row declares a return-stack cell"
   S\" package GP\n: G ( n -- ) drop parse-name 2drop ;\ngenerates: G ( | -- | n )\npublic\n;package\n"
   s" error[E-GENERATES-ROW]: the `generates:` row names `G`"
   s" its signature moves the return stack"  REFUSED-RC REFUSED
   s" a bar on one side only"
   S\" : ONE ( R n | S -- R ) drop ;\n"
   s" error[E-BAD-SIGNATURE]: return-stack effect in signature ` R n | S -- R `"
   s" --> <input>:1:8"  REFUSED-RC REFUSED
   s" a quotation literal pushes a cell its caller pops"
   S\" : QPUSH ( n -- n ) [: >r ;] execute r> ;\n: USE ( -- n ) 5 QPUSH ;\nUSE . cr\n"
   s" error[E-RETURN-STACK]: return stack: the quotation leaves its return row unbalanced"
   s" --> <input>:1:26"  REFUSED-RC REFUSED
   s" a quotation literal pops a cell its caller pushed"
   S\" : QPOP ( n -- n ) >r [: r> ;] execute ;\n: USE ( -- n ) 5 QPOP ;\nUSE . cr\n"
   s" error[E-RETURN-STACK]: return stack: the quotation reads below the return-stack cells it pushed"
   s" --> <input>:1:25"  REFUSED-RC REFUSED
   s" an unsigned definition reads a cell it did not push"
   S\" : UREAD r@ ;\n"
   s" error[E-RETURN-STACK]: return stack: this reads below the return-stack cells pushed here"
   s" --> <input>:1:9"  REFUSED-RC REFUSED
   s" an unsigned definition pops a cell it did not push"
   S\" : UPOP r> ;\n"
   s" error[E-RETURN-STACK]: return stack: this reads below the return-stack cells pushed here"
   s" --> <input>:1:8"  REFUSED-RC REFUSED ;

\ ---- still admitted -----------------------------------------------------------
\ A bar on one side only pairs the written row with a fresh one and is admitted,
\ with its callers (ONE); so are the neutral spellings src/lib/c2-memory.f uses,
\ `| U -- S | U` and `[ R ... -- S ... | U -- U ]` (APPLY).
: ADMISSIONS ( -- )
   s" a neutral clause at the top and in a quotation type"
   S\" : APPLY ( R n [ R n -- S n | U -- U ] | U -- S n | U ) execute ;\n: USE ( -- n ) 3 [: 1+ ;] APPLY ;\nUSE . cr\n"
   S\" 4\n\n" ADMITTED
   s" a body parks a value with >r and takes it back"
   S\" : PARK ( n n -- n ) >r 10 * r> + ;\n: USE ( -- n ) 4 2 PARK ;\nUSE . cr\n"
   S\" 42\n\n" ADMITTED ;

public

: RUN ( -- )
   T-RESET
   REFUSALS
   ADMISSIONS
   T-REPORT
   s" native-rstack-input: ok" type cr ;

;package

NRS-INPUT-TEST:RUN
