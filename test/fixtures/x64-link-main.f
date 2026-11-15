\ The linked x86 program checks local reads through calls and unwinding.
require src/lib/string.f

package X64-LINK-PROGRAM
private

create MESSAGE 72 c, 105 c, 10 c,
variable CLEANUPS
$100000007 constant E-BODY

: VALUE ( n -- ) create , does> ( -- n ) @ ;
41 VALUE BASE
defer NEXT ( n -- n )
: INC ( n -- n ) 1+ ;
' INC is NEXT

: SAME ( n n -- ) <> if 1 throw then ;
: SAY ( -- ) MESSAGE 3 type ;

: CAPTURE ( n -- n )
   {: a:n :}
   [: a NEXT a + ;] execute ;

: NESTED ( n -- n )
   {: a:n :}
   [: [: a 2 + ;] execute ;] execute ;

: SCOPE ( ptr n n -- )
   {: closed:ptr code:n :}
   [: code 0<> if code throw then ;]
   [: 1 closed +! ;] finally ;

: CAUGHT ( ptr n n -- n )
   {: closed:ptr code:n :}
   [: closed code SCOPE ;] catch ;

public
: RUN ( -- )
   BASE NEXT 42 SAME
   20 CAPTURE 41 SAME
   40 NESTED 42 SAME
   CLEANUPS 0 CAUGHT 0 SAME
   CLEANUPS @ 1 SAME
   CLEANUPS E-BODY CAUGHT E-BODY SAME
   CLEANUPS @ 2 SAME
   [: SAY ;] execute ;

;package

: MAIN ( -- ) X64-LINK-PROGRAM:RUN ;
