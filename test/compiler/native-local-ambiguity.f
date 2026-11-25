\ A local binding must win over equally named public words imported from two
\ packages.
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package NATIVE-LOCAL-AMBIGUITY

$1000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: OUT$ ( -- str ) OUT OUT-U @ >str ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;

: STORE ( len len PROCESS:ending str -- ) {: outu:len erru:len oc src:str :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH PROCESS:ending
      exited OF RC ! true EXITED ! ENDOF
      signaled OF RC ! false EXITED ! ENDOF
      timeout OF src OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN-SOURCE ( str -- ) {: src:str :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN src STORE ;

: EXPECT-RC ( n -- ) {: want:n :}
   EXITED @ 0= RC @ want <> or if ERR$ type OUT$ type then
   EXITED @ TTRUE
   RC @ want T= ;

: EXPECT-OUT ( str -- ) {: exp:str :}
   0 EXPECT-RC
   ERR-U @ 0 T=
   OUT$ exp STR:= TTRUE ;

: SOURCE$ ( -- str )
   S\" package PRE\npublic\n: TARGET ( n -- n ) 9 + ;\n;package\npackage ONE\npublic\n: LAYERS ( n -- n ) 1 + ;\n;package\npackage TWO\npublic\n: LAYERS ( n -- n ) 2 + ;\n;package\npackage SUBJECT\nusing PRE\nusing ONE\nusing TWO\nprivate\n: TARGET ( n n -- n ) {: layers:n :} layers + ;\npublic\n: NLA-RUN ( -- n ) 3 4 TARGET ;\n;package\nSUBJECT:NLA-RUN .\n" ;

public

: RUN ( -- )
   T-RESET
   s" native locals resolve before ambiguous used publics" T-LABEL
   SOURCE$ RUN-SOURCE
   S\" 7\n" EXPECT-OUT
   T-REPORT ;

;package

NATIVE-LOCAL-AMBIGUITY:RUN
