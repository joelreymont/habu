\ checker-verify-order.f - a reconstruction binds each definition as of its own
\ record, so a later shadow of a spelling does not change an earlier verdict.
\
\ The program below is loaded first, exactly as a source file is. Its text is
\ then verified warm, in this same engine, the way a build certifies a generated
\ prefix: every definition already exists, and so does everything after it.
\ A replay would have seen only what existed when each word was compiled; the
\ verifier must reconstruct that order, and it must not hide anything older.
require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

\ A global, an earlier package dependency, and a package that shadows the
\ global after using it (the engine refuses a duplicate definition in one
\ scope, so a shadow can only come from another scope). Every word here runs;
\ the values pin the bindings.
: CVO-F ( n -- n ) 1 + ;
package CVO-DEP
public
: OP ( n -- n ) 10 + ;
;package
package CVO-P
public
: RUN ( n -- n ) CVO-F ;              \ binds the global CVO-F: nothing else exists yet
: USES-DEP ( n -- n ) CVO-DEP:OP ;    \ an existing package dependency
: CVO-F ( n n -- n ) + ;              \ the later shadow, with another effect
: LATER ( n n -- n ) CVO-F ;          \ binds the package CVO-F: it exists by now
;package

\ The used-publics leg. A `using` scope resolves a bare tail through the used
\ package's publics after the globals, and refuses a tail that a global and a
\ used public both export (E-USING-SHADOW-GLOBAL) or that two used publics
\ export (E-USING-AMBIGUOUS). Each collision here arises only AFTER the
\ definition that names the tail, so the engine bound it without complaint.
: CVR-F ( n -- n ) 1 + ;
package CVR-U
public
;package
using CVR-U
: CVR-R ( n -- n ) CVR-F ;            \ CVR-U exports nothing yet: binds the global
;using
package CVR-U
public
: CVR-F ( n n -- n ) + ;              \ the later used-public twin of the global
;package
package CVR-V
public
: CVR-G ( n -- n ) 3 + ;
: CVR-H ( n -- n ) 5 + ;
;package
package CVR-W
public
;package
using CVR-V
: CVR-S ( n -- n ) CVR-G ;            \ no global CVR-G yet: binds CVR-V's
using CVR-W
: CVR-T ( n -- n ) CVR-H ;            \ CVR-W exports nothing yet: binds CVR-V's
;using
;using
: CVR-G ( n n -- n ) * ;              \ the later global twin of the used public
package CVR-W
public
: CVR-H ( n -- n ) 7 + ;              \ the later second used public
;package

package CVO-TEST
private

create DIAG-BUF $1000 allot
variable DIAG-U
TYPED-VARIABLE SRC-A ptr u8
variable SRC-U

: ACT ( -- )
   SRC-A @ SRC-U @ VERIFY:SOURCE-BUF ;

\ The verdict of verifying a text warm: 0 certified, else the throw code.
: VERDICT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A !
   u SRC-U !
   DIAG-BUF $1000 DIAG-BUFFER!
   [: ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc ;

: DIAG$ ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   S\" : CVO-F ( n -- n ) 1 + ;\npackage CVO-DEP\npublic\n: OP ( n -- n ) 10 + ;\n;package\npackage CVO-P\npublic\n: RUN ( n -- n ) CVO-F ;\n: USES-DEP ( n -- n ) CVO-DEP:OP ;\n: CVO-F ( n n -- n ) + ;\n: LATER ( n n -- n ) CVO-F ;\n;package\n" ;

: USING-PROGRAM$ ( -- ptr u8 n )
   S\" : CVR-F ( n -- n ) 1 + ;\npackage CVR-U\npublic\n;package\nusing CVR-U\n: CVR-R ( n -- n ) CVR-F ;\n;using\npackage CVR-U\npublic\n: CVR-F ( n n -- n ) + ;\n;package\npackage CVR-V\npublic\n: CVR-G ( n -- n ) 3 + ;\n: CVR-H ( n -- n ) 5 + ;\n;package\npackage CVR-W\npublic\n;package\nusing CVR-V\n: CVR-S ( n -- n ) CVR-G ;\nusing CVR-W\n: CVR-T ( n -- n ) CVR-H ;\n;using\n;using\n: CVR-G ( n n -- n ) * ;\npackage CVR-W\npublic\n: CVR-H ( n -- n ) 7 + ;\n;package\n" ;

: LOADED-CASE ( -- )
   s" the program loaded with source-order bindings" T-LABEL
   1 CVO-P:RUN 2 T=
   1 CVO-P:USES-DEP 11 T=
   2 3 CVO-P:LATER 5 T=
   1 CVR-R 2 T=
   1 CVR-S 4 T=
   1 CVR-T 6 T= ;

: RECONSTRUCT-CASE ( -- )
   s" the whole program verifies warm: every definition binds as of its record" T-LABEL
   PROGRAM$ VERDICT 0 T=
   DIAG$ nip 0 T= ;

: SHADOW-CASE ( -- )
   s" a definition after the shadow still binds the package word" T-LABEL
   S\" package CVO-P\npublic\n: LATER ( n n -- n ) CVO-F ;\n;package\n" VERDICT 0 T=
   s" and a definition the store does not know binds the newest record" T-LABEL
   S\" package CVO-P\npublic\n: FRESH ( n n -- n ) CVO-F ;\n;package\n" VERDICT 0 T=
   S\" package CVO-P\npublic\n: FRESH1 ( n -- n ) CVO-F ;\n;package\n" VERDICT 70 T=
   DIAG$ s" fresh1" CONTAINS? TTRUE ;

: DEPENDENCY-CASE ( -- )
   s" an existing package dependency stays visible under the horizon" T-LABEL
   S\" package CVO-P\npublic\n: USES-DEP ( n -- n ) CVO-DEP:OP ;\n;package\n" VERDICT 0 T= ;

\ The text a pass verifies need not be the text on record: a build certifies the
\ next source in the engine built from the last one. What the pass has defined
\ so far is what a cold compile of its text would see, so it binds before any
\ record the store already had, wherever that record sits.
: PASS-CASE ( -- )
   s" a word the pass defined earlier binds, though its old record is later" T-LABEL
   S\" package CVO-P\npublic\n: CVO-F ( n n -- n ) + ;\n: RUN ( n n -- n ) CVO-F ;\n;package\n" VERDICT 0 T=
   S\" package CVO-P\npublic\n: CVO-F ( n n -- n ) + ;\n: RUN ( n -- n ) CVO-F ;\n;package\n" VERDICT 70 T=
   s" a word the pass defined that the store never had binds under a horizon" T-LABEL
   S\" package CVO-P\npublic\n: NEWER ( n -- n ) 5 + ;\n: RUN ( n -- n ) CVO-F NEWER ;\n;package\n" VERDICT 0 T= ;

\ A collision that arose after the definition is not one it can see; the same
\ text under a name the store does not know sees the whole store and is refused.
: USING-CASE ( -- )
   s" the whole using program verifies warm" T-LABEL
   USING-PROGRAM$ VERDICT 0 T=
   s" a later used-public twin of a global does not shadow an earlier reference" T-LABEL
   S\" using CVR-U\n: CVR-R ( n -- n ) CVR-F ;\n;using\n" VERDICT 0 T=
   S\" using CVR-U\n: CVR-Q ( n -- n ) CVR-F ;\n;using\n" VERDICT 7141 T=
   s" a later global twin of a used public does not shadow an earlier reference" T-LABEL
   S\" using CVR-V\n: CVR-S ( n -- n ) CVR-G ;\n;using\n" VERDICT 0 T=
   S\" using CVR-V\n: CVR-Q ( n -- n ) CVR-G ;\n;using\n" VERDICT 7141 T=
   s" a later second used public does not make an earlier reference ambiguous" T-LABEL
   S\" using CVR-V\nusing CVR-W\n: CVR-T ( n -- n ) CVR-H ;\n;using\n;using\n" VERDICT 0 T=
   S\" using CVR-V\nusing CVR-W\n: CVR-Q ( n -- n ) CVR-H ;\n;using\n;using\n" VERDICT 7144 T= ;

: REJECT-CASE ( -- )
   s" a real mismatch inside the horizon is still rejected and named" T-LABEL
   S\" package CVO-P\npublic\n: RUN ( n -- n n ) CVO-F ;\n;package\n" VERDICT 70 T=
   DIAG$ s" run" CONTAINS? TTRUE ;

public

: RUN ( -- )
   LOADED-CASE
   RECONSTRUCT-CASE
   SHADOW-CASE
   DEPENDENCY-CASE
   PASS-CASE
   USING-CASE
   REJECT-CASE ;

;package

T-RESET
CVO-TEST:RUN
T-REPORT
