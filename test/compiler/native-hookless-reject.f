\ native-hookless-reject.f - the native compiler refuses an ambiguous bare
\ callee under the definition's name, and a refused definition keeps no row.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP
20000 constant SUBJECT-TIMEOUT-MS
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)

create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: SHOW ( bool -- bool )
   dup 0= if OUT$ type ERR$ type then ;

: SAYS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? SHOW TTRUE ;

: PRINTS ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? SHOW TTRUE ;

: SUBJECT-STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH outcome
      timeout OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
      exited OF RC ! ENDOF
      signaled OF drop -1 RC ! ENDOF
   ;MATCH ;

: SESSION ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN SUBJECT-TIMEOUT-MS >MS SUBJECT:RUN
   src u SUBJECT-STORE! ;

\ The checker's ambiguity refusal makes the native compiler exit with the
\ reject status under the definition's name.
: AMBIGUOUS-CALLEE ( -- )
   s" an ambiguous bare callee is refused naming the callee" T-LABEL
   s" ;package package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   RC @ RC-REJECT T=
   s" E-USING-AMBIGUOUS habu: bare 'AW' is ambiguous under using" SAYS
   s" ncomp: cannot compile UT-AMB" SAYS ;

\ ---- a refused definition keeps no row ---------------------------------------
\ The texts outrun a source line, so TEXT holds them; `;package` first makes
\ them global.
create TEXT CAP allot                  variable TEXT-U

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT CAP TEXT-U BUF-APPEND ;

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
   REFUSED-CAUGHT
   T-REPORT ;

RUN
;package
