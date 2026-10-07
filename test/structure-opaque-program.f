\ structure-opaque-program.f - a consumer of test/structure-opaque-fixture.f,
\ run by test/structure-opaque-e2e.f on the engine under test. OP's `box` is
\ OPAQUE: its type crosses packages and its generated pair does not. OPC
\ declares the same family without the clause, the control every refusal is
\ measured against. Each refused text runs in a forked child (lib/test/subject.f)
\ and must exit with its status and name its reason on stderr.
require lib/test.f
require lib/string.f
require lib/test/subject.f
require test/structure-opaque-fixture.f

package OPC
public
STRUCTURE box 0 FIELD x n ;STRUCTURE
;package

package OPQ
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

\ The child's exit status is `want` and its stderr contains `why`.
: STATUS? ( ptr u8 n n ptr u8 n -- bool ) {: src:ptr srcu:n want:n why:ptr whyu:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF want = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: outu:len erru:len exited:bool :}
   exited ERR erru LEN>N why whyu CONTAINS? and ;

: ADMITTED? ( ptr u8 n -- bool ) 0 s" " STATUS? ;
: REFUSED? ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu:n why:ptr whyu:n :}
   src srcu 70 why whyu STATUS? ;

public

: SIG ( OP:box -- OP:box ) ;
: ROUND ( n -- n ) OP:WRAP SIG OP:PEEK ;
: CONTROL ( n -- n ) OPC-BOX:MAKE OPC-BOX:UNMAKE ;

private

: ADMITTED ( -- )
   s" another package names OP:box and constructs it through OP's own words" T-LABEL
   7 ROUND 7 T=
   s" the control family's constructor is callable from another package" T-LABEL
   9 CONTROL 9 T=
   s" the probe admits the control spelling" T-LABEL
   s" package OPQA public : FD ( -- n ) 7 OPC-BOX:MAKE OPC-BOX:UNMAKE ; ;package" ADMITTED? TTRUE ;

: CALLS ( -- )
   s" the bare generated pair is undefined outside OP" T-LABEL
   s" package OPQA public : FA ( -- n ) 7 BOX-MAKE BOX-UNMAKE ; ;package"
   s" E-UNDEFINED: BOX-MAKE" REFUSED? TTRUE
   s" no constructor namespace is reserved" T-LABEL
   s" package OPQA public : FB ( -- n ) 7 OP-BOX:MAKE OP-BOX:UNMAKE ; ;package"
   s" E-UNDEFINED: OP-BOX:MAKE" REFUSED? TTRUE
   s" the generated pair is not a public word of OP" T-LABEL
   s" package OPQA public : FC ( -- n ) 7 OP:BOX-MAKE OP:BOX-UNMAKE ; ;package"
   s" E-UNDEFINED: OP:BOX-MAKE" REFUSED? TTRUE ;

: REACH ( -- )
   s" another package cannot export the constructor" T-LABEL
   s" package OPQA public EXPORT OP-BOX:MAKE ;package" s" OP-BOX:MAKE" REFUSED? TTRUE
   s" a tick outside OP reaches no constructor" T-LABEL
   s" ' OP-BOX:MAKE drop" s" OP-BOX:MAKE" REFUSED? TTRUE
   s" package OPQA public : FE ( -- ) ['] BOX-MAKE drop ; ;package"
   s" BOX-MAKE" REFUSED? TTRUE
   s" OP's generated words are protected by name" T-LABEL
   s" package OP undefine BOX-MAKE ;package" 67 s" 7111" STATUS? TTRUE ;

: DEFINER ( -- )
   s" OPAQUE is a header clause" T-LABEL
   s" package OPQA public STRUCTURE olate 0 FIELD x n OPAQUE ;STRUCTURE ;package"
   s" header clause after the first field" REFUSED? TTRUE
   s" OPAQUE appears at most once" T-LABEL
   s" package OPQA public STRUCTURE otwice 0 OPAQUE OPAQUE FIELD x n ;STRUCTURE ;package"
   s" a second OPAQUE clause in one declaration" REFUSED? TTRUE
   s" a fieldless structure generates nothing to hide" T-LABEL
   s" package OPQA public STRUCTURE obare 0 OPAQUE ;STRUCTURE ;package"
   s" opaque requires a family with fields" REFUSED? TTRUE
   s" ENUM has no OPAQUE clause" T-LABEL
   s" package OPQA public ENUM oshade 0 OPAQUE VARIANT dark ;VARIANT ;ENUM ;package"
   s" unexpected token in enum declaration" REFUSED? TTRUE ;

: REDECLARE ( -- )
   s" OP cannot redeclare box without the clause" T-LABEL
   s" package OP public STRUCTURE box 0 FIELD x n ;STRUCTURE ;package"
   s" duplicate family" REFUSED? TTRUE
   s" another package's box constructs only its own family" T-LABEL
   s" package OPQB public STRUCTURE box 0 FIELD x n ;STRUCTURE : FORGE ( n -- OP:box ) OPQB-BOX:MAKE ; ;package"
   s" expected: op:box" REFUSED? TTRUE ;

\ Outside any package the clause has no private wordlist to place the pair in;
\ the reject is a declaration reject the loader survives, and the name stays free.
: LOOSE ( -- )
   s" OPAQUE outside a package is refused, and loading goes on" T-LABEL
   s" STRUCTURE oloose 0 OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 7207 T=
   s" STRUCTURE oloose 0 FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T= ;

public

: RUN ( -- )
   T-RESET
   ADMITTED CALLS REACH DEFINER REDECLARE LOOSE
   T-REPORT
   s" structure-opaque-program: ok" type cr ;

;package

OPQ:RUN
