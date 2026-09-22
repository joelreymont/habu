\ A local binding must win over equally named public words imported from two
\ packages. The native prior-binding pass runs before the local table exists.
\ Tier-neutral by design: each subject is compiled in a child this file runs at
\ tier 0 and at tier 1, so the tier of this row selects nothing.
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package NATIVE-LOCAL-AMBIGUITY

$1000 constant CAP
20000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: STORE ( len len outcome -- )
   MATCH outcome
      exited OF RC ! true EXITED ! ENDOF
      signaled OF RC ! false EXITED ! ENDOF
      timeout OF 0 RC ! false EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U ! LEN>N OUT-U ! ;

: RUN-SOURCE ( ptr u8 n n -- ) {: src:ptr u:n tier:n :}
   SB-RESET
   tier 0= if s" 0 set-tier " else s" 1 set-tier " then SB-APPEND
   src u SB-APPEND
   SB$ OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN STORE ;

: EXPECT-RC ( n -- ) {: want:n :}
   EXITED @ 0= RC @ want <> or if ERR$ type OUT$ type then
   EXITED @ TTRUE
   RC @ want T= ;

: EXPECT-OUT ( ptr u8 n -- ) {: exp:ptr expu:u :}
   0 EXPECT-RC
   ERR-U @ 0 T=
   OUT$ exp expu STR= TTRUE ;

: SOURCE$ ( -- ptr u8 n )
   S\" package PRE\npublic\n: TARGET ( n -- n ) 9 + ;\n;package\npackage ONE\npublic\n: LAYERS ( n -- n ) 1 + ;\n;package\npackage TWO\npublic\n: LAYERS ( n -- n ) 2 + ;\n;package\npackage SUBJECT\nusing PRE\nusing ONE\nusing TWO\nprivate\n: TARGET ( n n -- n ) {: layers:n :} layers + ;\npublic\n: NLA-RUN ( -- n ) 3 4 TARGET ;\n;package\nSUBJECT:NLA-RUN .\n" ;

public

: RUN ( -- )
   T-RESET
   s" native locals resolve before ambiguous used publics" T-LABEL
   SOURCE$ 1 RUN-SOURCE
   S\" 7\n" EXPECT-OUT
   T-REPORT ;

;package

NATIVE-LOCAL-AMBIGUITY:RUN
