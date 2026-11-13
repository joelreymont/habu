\ native-hookless-reject.f - a definition the checker refuses is refused naming
\ it and the token refused, and keeps no row.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP
20000 constant SUBJECT-TIMEOUT-MS
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)
67 constant UNCAUGHT-RC              \ an uncaught throw's exit status

create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC
\ Texts that outrun a source line.
create TEXT CAP allot                  variable TEXT-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT CAP TEXT-U BUF-APPEND ;

: SHOW ( bool -- bool )
   dup 0= if OUT$ type ERR$ type then ;

: SAYS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? SHOW TTRUE ;

: PRINTS ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? SHOW TTRUE ;

: SUBJECT-STORE! ( len len PROCESS:ending ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH PROCESS:ending
      timeout OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
      exited OF RC ! ENDOF
      signaled OF drop -1 RC ! ENDOF
   ;MATCH ;

: SESSION ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN SUBJECT-TIMEOUT-MS >MS SUBJECT:RUN
   src u SUBJECT-STORE! ;

\ The checker's ambiguity refusal makes the native compiler exit with the
\ reject status under the definition's name and the callee it refused.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: AMBIGUOUS-CALLEE ( -- )
   s" an ambiguous bare callee is refused naming the callee" T-LABEL
   s" ;package package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   RC @ RC-REJECT T=
   s" E-USING-AMBIGUOUS habu: bare 'AW' is ambiguous under using" SAYS
   s" compiler: cannot compile UT-AMB at AW in" SAYS ;

\ A used public that shadows a global is refused the same way.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: SHADOWING-CALLEE ( -- )
   s" a bare callee a used public shadows is refused naming the callee" T-LABEL
   s" ;package : SW ( -- n ) 11 ; package US public : SW ( -- n ) 33 ; ;package using US : UT-SHADOW ( -- n ) SW ; ;using" SESSION
   RC @ RC-REJECT T=
   s" E-USING-SHADOW-GLOBAL habu: bare 'SW' is ambiguous under using" SAYS
   s" compiler: cannot compile UT-SHADOW at SW in" SAYS ;

\ A TRUSTED: row whose stored signature does not parse is refused at that token.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: BAD-SIGNATURE ( -- )
   s" a refused stored signature is refused naming its token" T-LABEL
   s" ;package TRUSTED: TB8 ( -- nosuch:fam ) 1 NOSUCHWORD ;" SESSION
   RC @ RC-REJECT T=
   s" habu: in tb8: bad stored signature '-- nosuch:fam'" SAYS
   s" compiler: cannot compile TB8 at nosuch:fam in" SAYS ;

\ A public whose private twin moves other cells is refused at its own name.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: SHADOWED-ARITY ( -- )
   s" a public its private tail shadows is refused naming the definition" T-LABEL
   s" ;package package SAT-A : F ( n n -- n ) + ; public : F ( n -- n ) 1 + ; ;package" SESSION
   RC @ RC-REJECT T=
   s" E-SHADOWED-ARITY habu: public 'sat-a:f'" SAYS
   s" compiler: cannot compile F at F in" SAYS ;

\ A TRUSTED: body whose `@` cannot be applied over a two-cell family value
\ loses that value's width, so nothing can place its cells; a does> clause that
\ loses one is refused the same way.
\ Its own process: the uncaught throw ends it, and its exit status is tested.
: INCOMPLETE-SCAN ( -- )
   s" a scan that loses a wide value's width is refused" T-LABEL
   TEXT-U BUF-RESET
   s" ENUM hr-v VARIANT none ;VARIANT VARIANT some FIELD x n ;VARIANT ;ENUM " TEXT+
   S\" : HR-TRY ( -- n ) [: s\" TRUSTED: HR-DEF ( n -- ) create , does> ( -- n ) @ construct hr-v some @ ;\" evaluate-closed ;] catch ; " TEXT+
   s" HR-TRY . " TEXT+
   s" TRUSTED: HR-LOST ( n -- n ) construct hr-v some @ ;" TEXT+
   TEXT$ SESSION
   RC @ UNCAUGHT-RC T=
   s" compiler: cannot compile HR-DEF" SAYS
   S\" -8519\n" PRINTS
   s" compiler: cannot compile HR-LOST" SAYS
   s" hb: uncaught throw code -8519" SAYS ;

\ ---- a refused definition keeps no row ---------------------------------------
\ `;package` first makes the texts global.
: REFUSED-TEXT ( -- )
   TEXT-U BUF-RESET  s" ;package " TEXT+ ;

: P-W+ ( -- ) s" package P-FORGET public : W-FORGET ( -- n ) 2 ; ;package " TEXT+ ;
: Q-USING-P+ ( -- ) s" package Q-FORGET public using P-FORGET " TEXT+ ;

\ TRY-FORGET prints the refusal's code, 70 (RC-REJECT). A row kept from the
\ refused ( -- n n ) would certify a T-FORGET ( -- n n ) over W-FORGET.
: REFUSED+ ( -- )
   S\" : TRY-FORGET ( -- n ) [: s\" : W-FORGET ( -- n n ) 3 ;\" evaluate-closed ;] catch ; " TEXT+
   s" TRY-FORGET . cr " TEXT+ ;

\ The refused W-FORGET keeps no row, so T-FORGET binds P-FORGET:W-FORGET.
: REFUSED-CAUGHT ( -- )
   s" a caught refusal leaves a call to the used package's word" T-LABEL
   REFUSED-TEXT P-W+ Q-USING-P+ REFUSED+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ RC-REJECT T=
   S\" 70\n" PRINTS
   s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS ;

: RUN ( -- )
   T-RESET
   AMBIGUOUS-CALLEE
   SHADOWING-CALLEE
   BAD-SIGNATURE
   SHADOWED-ARITY
   INCOMPLETE-SCAN
   REFUSED-CAUGHT
   T-REPORT ;

RUN
;package
