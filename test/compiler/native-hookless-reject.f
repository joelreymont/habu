\ native-hookless-reject.f - a definition the checker rejects is rejected naming
\ it and the token rejected, and keeps no row.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/process.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP

create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC
\ Texts that outrun a source line.
create TEXT CAP allot                  variable TEXT-U

: OUT$ ( -- str ) OUT OUT-U @ >str ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;

: TEXT$ ( -- str ) TEXT TEXT-U BUF-LEN@ >str ;
: TEXT+ ( str -- ) TEXT CAP TEXT-U BUF-APPEND ;

: SHOW ( bool -- bool )
   dup 0= if OUT$ type ERR$ type then ;

: SAYS ( str -- ) {: s:str :}
   ERR$ s STR:CONTAINS? SHOW TTRUE ;

: PRINTS ( str -- ) {: s:str :}
   OUT$ s STR:CONTAINS? SHOW TTRUE ;

: SUBJECT-STORE! ( len len PROCESS:ending str -- ) {: outu:len erru:len oc src:str :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH PROCESS:ending
      timeout OF src OUT$ ERR$ T-TIMED-OUT ENDOF
      exited OF RC ! ENDOF
      signaled OF drop -1 RC ! ENDOF
   ;MATCH ;

: SESSION ( str -- ) {: src:str :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   src SUBJECT-STORE! ;

\ A TRUSTED: row whose stored signature does not parse is rejected at that token.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: BAD-SIGNATURE ( -- )
   s" a rejected stored signature is rejected naming its token" T-LABEL
   s" TRUSTED: TB8 ( -- nosuch:fam ) 1 NOSUCHWORD ;" SESSION
   RC @ 70 T=
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `TB8` cannot be recorded" SAYS
   s" = evaluated: TRUSTED: TB8 ( -- nosuch:fam )" SAYS
   s" no type has this name" SAYS ;

\ A public whose private twin moves other cells is rejected at its own name.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: SHADOWED-ARITY ( -- )
   s" a public its private tail shadows is rejected naming the definition" T-LABEL
   s" package SAT-A : F ( n n -- n ) + ; public : F ( n -- n ) 1 + ; ;package" SESSION
   RC @ 70 T=
   s" error[E-SHADOWED-ARITY]: public `SAT-A:F` has a different stack effect" SAYS
   s" = evaluated: package SAT-A" SAYS
   s" its name binds the private word" SAYS ;

\ A TRUSTED: body whose `@` cannot be applied over a two-cell family value
\ loses that value's width, so nothing can place its cells; a checked does>
\ clause that applies `@` to one is rejected for the type first.
\ Its own process: the rejection's status ends it, and is tested.
: INCOMPLETE-SCAN ( -- )
   s" a scan that loses a wide value's width is rejected" T-LABEL
   TEXT-U BUF-RESET
   s" ENUM hr-v VARIANT none ;VARIANT VARIANT some FIELD x n ;VARIANT ;ENUM " TEXT+
   S\" : HR-TRY ( -- n ) [: s\" TRUSTED: HR-DEF ( n -- ) create , does> ( -- n ) @ construct hr-v some @ ;\" evaluate-closed ;] catch ; " TEXT+
   s" HR-TRY . " TEXT+
   s" TRUSTED: HR-LOST ( n -- n ) construct hr-v some @ ;" TEXT+
   TEXT$ SESSION
   RC @ 70 T=
   s" error[E-MISMATCH]: `@` takes ^a; the stack holds hr-v" SAYS
   S\" 70\n" PRINTS
   s" error[E-NELAB-BUNDLE]: the body of `HR-LOST` loses the width of a multi-cell value" SAYS
   s" = family: hr-v" SAYS ;

\ ---- a rejected definition keeps no row ---------------------------------------
: W+ ( -- ) s" : W-FORGET ( -- n ) 2 ; " TEXT+ ;
: Q+ ( -- ) s" package Q-FORGET public " TEXT+ ;

\ TRY-FORGET prints the rejection's code, 70. A row kept
\ from the rejected ( -- n n ) would certify a T-FORGET ( -- n n ) over
\ W-FORGET.
: REJECTED+ ( -- )
   S\" : TRY-FORGET ( -- n ) [: s\" : W-FORGET ( -- n n ) 3 ;\" evaluate-closed ;] catch ; " TEXT+
   s" TRY-FORGET . cr " TEXT+ ;

\ The rejected W-FORGET keeps no row, so T-FORGET binds the global W-FORGET.
: REJECTED-CAUGHT ( -- )
   s" a caught rejection leaves a call to the global word" T-LABEL
   TEXT-U BUF-RESET W+ Q+ REJECTED+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ 70 T=
   S\" 70\n" PRINTS
   s" error[E-MISMATCH]: T-FORGET ends with n on the stack; its signature promises n n" SAYS
   s" = evaluated: ...; TRY-FORGET . cr : T-FORGET ( -- n n ) W-FORGET ;" SAYS ;

public
: RUN ( -- )
   T-RESET
   BAD-SIGNATURE
   SHADOWED-ARITY
   INCOMPLETE-SCAN
   REJECTED-CAUGHT
   T-REPORT ;

;package
HOOKLESS-REJECT-TEST:RUN
