\ checker-soundness-suite.f - a certified body must not exceed its declared effect.
\ Each case pins one way a body used to launder a value past its contract:
\ a quotation argument consuming a narrower input than the parameter supplies,
\ a quotation whose output keeps the input stack a declared callable leaves open,
\ a return-row read or pop below the frame this definition may see, a
\ declared row unified with another or given fixed cells, and a signature with
\ tokens after its output side.

require src/lib/test.f
require src/lib/test/subject.f
require src/lib/values.f

package CHECKER-SOUNDNESS
public

PRODUCT point FIELD x n FIELD y n ;PRODUCT

: TAKE-U8 ( u8 -- ) drop ;
: TAKE-I64 ( i64 -- ) drop ;
: MK-I64 ( -- i64 ) 300 ;
: MK-U8 ( -- u8 ) 3 ;
: CALL-I64 ( [ i64 -- ] -- ) MK-I64 swap execute ;
: CALL-U8 ( [ u8 -- ] -- ) MK-U8 swap execute ;
: CALL-I64-Q ( [ [ i64 -- ] -- ] -- ) ['] TAKE-I64 swap execute ;
: CALL-U8-Q ( [ [ u8 -- ] -- ] -- ) ['] TAKE-U8 swap execute ;

\ A callable declared `[ -- S ]` runs on whatever stack the callee holds.
: TAIL-TWICE ( R [ -- S ] -- S ) dup >r execute r> execute ;
: TAIL-ONCE ( R [ -- S ] -- S ) execute ;
: TAIL-NAMED ( R [ R -- S ] -- S ) execute ;
: TAIL-KEEP ( R [ -- ] -- R ) execute ;
: TAIL-ARG ( R [ n -- ] -- R ) 1 swap execute ;
\ Its output callable is declared `[ -- S ]` and runs on whatever it leaves.
: TAIL-NEST ( R [ R -- R [ -- S ] ] -- S ) execute execute ;
\ Its input callable is declared `[ -- ]` and runs on a stack one cell deeper.
: TAIL-UNDER ( R [ -- ] -- R ) >r 5 r> execute drop ;
: TAIL-APPLY ( R [ R [ R -- R ] -- R ] [ R -- R ] -- R ) swap execute ;
\ It runs its callable one cell deeper and keeps that cell.
: TAIL-G ( R [ -- S ] -- S ) >r 5 r> execute ;
\ Generic one-of-two: a join of its two arguments' types.
: TAIL-PICK1 ( a a -- a ) nip ;
: TAIL-PICK2 ( a a -- a ) drop ;
\ Three deep: an output's output, an input's input, and both.
: TAIL-N3 ( R [ R -- R [ R -- R [ -- S ] ] ] -- S ) execute execute execute ;
: TAIL-D3 ( R [ R [ R [ -- S ] -- S ] -- S ] -- S ) [: TAIL-G ;] swap execute ;
: TAIL-M4 ( R [ R -- R [ R [ R [ -- S ] -- S ] -- S ] ] -- S ) execute [: TAIL-G ;] swap execute ;
: TAIL-RC3 ( R [ -- S ] n -- S ) dup 0 <> IF 1 - >r 7 swap r> RECURSE EXIT THEN drop execute ;
: TAIL-MKT ( -- ) create does> ( R [ -- S ] -- S ) drop execute ;
TAIL-MKT TAIL-T1
: TAIL-MKU ( -- ) create does> ( R [ -- ] -- R ) drop >r 9 r> execute drop ;
TAIL-MKU TAIL-U1
\ Declared callables a store demands and a load provides.
TYPED-VARIABLE TAIL-QA [ -- n ]
TYPED-VARIABLE TAIL-QB [ -- S ]
TYPED-VARIABLE TAIL-QD [ [ -- S ] -- S ]
TYPED-VARIABLE TAIL-QN [ R [ R -- S ] -- S ]
TYPED-VARIABLE TAIL-QR [ R [ R -- R ] -- R ]
defer TAIL-DF ( R [ R -- S ] -- S )
\ A family argument is read and written, as a pointee is.
: TAIL-VK ( R VALUES:values<[ R -- R ]> -- R ) [: ;] swap VALUES:ADD ;
TYPED-VARIABLE TAIL-QV VALUES:values<[ -- S ]>
TYPED-VARIABLE TAIL-QVV VALUES:values<VALUES:values<[ -- S ]>>
: TAIL-VKN ( R VALUES:values<VALUES:values<[ R -- R ]>> -- R ) drop ;
\ Generic retags: each relabels its storage with a type it takes as a value.
: TAIL-RETAG ( VALUES:values<a> a -- VALUES:values<a> ) drop ;
: TAIL-RETAGN ( VALUES:values<VALUES:values<a>> a -- VALUES:values<VALUES:values<a>> ) drop ;
: TAIL-RETAGP ( ptr a a -- ptr a ) drop ;
: TAIL-STORE ( R ptr [ R -- R ] -- R ) [: ;] swap ! ;
\ A view's element type names its storage, whatever record holds the view.
STRUCTURE tailview<a,b,c,d> FIELD view mut-view<a,b,c,d> ;STRUCTURE
: TAIL-TAKEV ( R tailview<p,q,a,[ R -- R ]> -- R tailview<p,q,a,[ R -- R ]> ) ;
\ A literal runs on any stack, so it takes the row of the cell it is stored in.
TYPED-VARIABLE TAIL-QI [ n -- n ]
TYPED-VARIABLE TAIL-Q0 [ -- ]
: TAIL-ST ( ptr a a -- ) drop drop ;
: TAIL-PIN ( R [ R -- R ] -- R [ R -- R ] ) ;
: TAIL-TAKE0 ( [ -- ] -- ) drop ;
\ A family built around a literal is a value, as the literal is.
STRUCTURE tailhook<a> DERIVE addr FIELD fn a ;STRUCTURE
TYPED-VARIABLE TAIL-QH tailhook<[ n -- n ]>
\ A pinned callable a family argument holds behind a pointer, bare and inside
\ a callable's output.
: TAIL-TV ( R [ -- a ] -- R a ) execute ;
defer TAIL-HF ( R [ R -- R ] -- R ptr tailhook<[ R -- R ]> )
defer TAIL-HG ( R [ R -- R ] -- R [ -- ptr tailhook<[ R -- R ]> ] )

\ A type whose name is the longest a family tail may be.
NEWTYPE wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
CAST: >WIDE ( n -- wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww )


\ CS-K90's literal leaves 40 values of that type, so its escape refusal renders
\ about 10 KB as the inferred effect and as much again as the actual callable,
\ passing the renderer's 16 KiB record buffer while it shows row tails: the
\ capture throws E-DIAG-CAPACITY.
: CAPTURE-PAST$ ( -- ptr u8 n )
   s" CS-K90 ( -- ) [: 0 CHECKER-SOUNDNESS:>WIDE dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup ;] CHECKER-SOUNDNESS:TAIL-ONCE" ;


\ CHECK-QUIET-CANDIDATE!: -1 certified, 0 rejected, 1 uncheckable.
: QUOTATION-INPUTS ( -- )
   s" a quotation consuming a narrower input does not satisfy a wider parameter" T-LABEL
   s" CS-Q1 ( -- ) [: CHECKER-SOUNDNESS:TAKE-U8 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a quotation consuming a wider input satisfies a narrower parameter" T-LABEL
   s" CS-Q2 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-U8"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" an exact quotation input still passes" T-LABEL
   s" CS-Q3 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a nested quotation input reverses the direction twice" T-LABEL
   s" CS-Q6 ( -- ) [: CHECKER-SOUNDNESS:CALL-U8 ;] CHECKER-SOUNDNESS:CALL-I64-Q"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-Q7 ( -- ) [: CHECKER-SOUNDNESS:CALL-I64 ;] CHECKER-SOUNDNESS:CALL-U8-Q"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the accepted shapes execute" T-LABEL
   [: TAKE-I64 ;] CALL-U8
   [: CALL-U8 ;] CALL-I64-Q
   depth 0 T= ;


: CALLABLE-TAILS ( -- )
   s" a quotation whose output keeps its input stack is not such a callable" T-LABEL
   s" CS-K4 ( -- n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-TWICE" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K6 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K7 ( -- n ) [: ;] CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared callable passes on, and a callee naming the input row takes the quotation" T-LABEL
   s" CS-K8 ( R [ -- S ] -- S ) CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K9 ( -- n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T=
   [: 1 ;] TAIL-NAMED 1 T=
   s" the refusal shows the callable's output keeping its input row" T-LABEL
   s" : CS-K10 ( -- n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" expected: [ A -- B ] actual: [ C -- C n ]" SUBJECT:REFUSED? TTRUE
   s" : CS-K11 ( -- n ) [: ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" expected: [ A -- B ] actual: [ C -- C ]" SUBJECT:REFUSED? TTRUE
   s" the refusal reads the same whatever the definition holds beneath it" T-LABEL
   s" : CS-K19 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" expected: [ A -- B ] actual: [ C -- C n ]" SUBJECT:REFUSED? TTRUE
   s" its inferred effect is the body's up to the refusal, not the declared one" T-LABEL
   s" -1 JSON-DIAGS ! : CS-K22 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" a -- a [ -- n ] " SUBJECT:REFUSED? TTRUE
   s" a capture past the render buffer leaves later renders without row tails" T-LABEL
   [: CAPTURE-PAST$ CHECK-QUIET-CANDIDATE! drop ;] catch E-DIAG-CAPACITY T=
   \ The subject is a fork of this process, so it renders after the caught throw.
   s" : CS-K91 ( -- ) [: CHECKER-SOUNDNESS:TAKE-U8 ;] CHECKER-SOUNDNESS:CALL-I64 ;" 70
   s" expected: [ i64 -- ] actual: [ u8 -- ]" SUBJECT:REFUSED? TTRUE
   s" a forwarder may not pass the word's own input row as the callable's" T-LABEL
   s" CS-K12 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-TWICE" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K13 ( [ A -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K14 ( R [ A -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! 0 T=
   \ The named row met from the callable's output side instead of its input.
   s" CS-K16 ( R [ R n -- R ] -- R ) CHECKER-SOUNDNESS:TAIL-ARG" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K17 ( R [ R -- R ] -- R ) CHECKER-SOUNDNESS:TAIL-KEEP" CHECK-QUIET-CANDIDATE! 0 T=
   \ A row-preserving callable is no `[ -- S ]` either: a callee
   \ `( R [ -- S ] -- S ) >r 1 r> execute` runs it on `R n`, so S = R is unsound.
   s" CS-K15 ( R [ -- ] -- R ) CHECKER-SOUNDNESS:TAIL-ONCE" CHECK-QUIET-CANDIDATE! 0 T=
   s" the refusal shows the word's own row standing in for the callable's" T-LABEL
   s" : CS-K26 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" held row A would escape into C expected: [ A -- B ] actual: [ C -- D ]" SUBJECT:REFUSED? TTRUE
   s" : CS-K27 ( R [ A -- A ] -- R ) CHECKER-SOUNDNESS:TAIL-KEEP ;" 70
   s" held row A would escape into B expected: [ A -- A ] actual: [ B -- B ]" SUBJECT:REFUSED? TTRUE
   s" -1 JSON-DIAGS ! : CS-K28 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" Declare the callable [ R -- S ] from the word's input row" SUBJECT:REFUSED? TTRUE
   s" -1 JSON-DIAGS ! : CS-K72 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" ; held row A would escape into C\"" SUBJECT:REFUSED? TTRUE
   s" and the declarations it names certify" T-LABEL
   s" CS-K29 ( R [ -- ] -- R ) CHECKER-SOUNDNESS:TAIL-KEEP" CHECK-QUIET-CANDIDATE! -1 T=
   s" a callable's output callable is held the same way" T-LABEL
   s" CS-K23 ( R [ R -- R [ A -- B ] ] -- B ) CHECKER-SOUNDNESS:TAIL-NEST" CHECK-QUIET-CANDIDATE! 0 T=
   \ A nested literal that keeps its stack is held at depth 2.
   s" CS-K24 ( x -- x n n ) [: [: 1 ;] ;] CHECKER-SOUNDNESS:TAIL-NEST" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K25 ( R [ R -- R [ -- B ] ] -- B ) CHECKER-SOUNDNESS:TAIL-NEST" CHECK-QUIET-CANDIDATE! -1 T=
   s" and so is a callable's input callable" T-LABEL
   s" CS-K30 ( R [ R -- R ] -- R ) [: CHECKER-SOUNDNESS:TAIL-UNDER ;] swap CHECKER-SOUNDNESS:TAIL-APPLY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared callable passes on to a callee naming the input row" T-LABEL
   s" CS-K18 ( R [ -- S ] -- S ) CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T= ;


: CALLABLE-HOLDS ( -- )
   s" CS-K30's callable inside a literal, run or returned, is held the same way" T-LABEL
   s" CS-K31 ( R [ R -- R ] -- R ) [: [: CHECKER-SOUNDNESS:TAIL-UNDER ;] swap CHECKER-SOUNDNESS:TAIL-APPLY ;] execute"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K32 ( R -- R [ R [ R -- R ] -- R ] ) [: [: CHECKER-SOUNDNESS:TAIL-UNDER ;] swap CHECKER-SOUNDNESS:TAIL-APPLY ;]"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a join or a generic word does not let a literal take a declared callable's row" T-LABEL
   s" CS-K33 ( R [ R [ R -- R ] -- R ] [ R -- R ] f -- R ) swap >r IF drop [: CHECKER-SOUNDNESS:TAIL-UNDER ;] THEN r> CHECKER-SOUNDNESS:TAIL-APPLY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K34 ( R [ R [ R -- R ] -- R ] [ R -- R ] f -- R ) swap >r IF drop [: CHECKER-SOUNDNESS:TAIL-UNDER ;] ELSE THEN r> CHECKER-SOUNDNESS:TAIL-APPLY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K35 ( R [ R [ R -- R ] -- R ] [ R -- R ] -- R ) >r [: CHECKER-SOUNDNESS:TAIL-UNDER ;] CHECKER-SOUNDNESS:TAIL-PICK1 r> CHECKER-SOUNDNESS:TAIL-APPLY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K36 ( R [ R [ R -- R ] -- R ] [ R -- R ] -- R ) >r [: CHECKER-SOUNDNESS:TAIL-UNDER ;] swap CHECKER-SOUNDNESS:TAIL-PICK2 r> CHECKER-SOUNDNESS:TAIL-APPLY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the refusal names the side whose row escaped, whichever branch holds it" T-LABEL
   s" : CS-K71 ( R [ R [ R -- R ] -- R ] [ R -- R ] f -- R ) swap >r IF drop [: CHECKER-SOUNDNESS:TAIL-UNDER ;] ELSE THEN r> CHECKER-SOUNDNESS:TAIL-APPLY ;" 70
   s" held row A would escape into B expected: [ A -- A ] actual: [ B -- B ]" SUBJECT:REFUSED? TTRUE
   \ Joins of two values: neither may narrow the other's declared row.
   s" CS-K44 ( R [ -- S ] -- S ) [: 1 ;] swap CHECKER-SOUNDNESS:TAIL-PICK2 execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K45 ( R [ -- S ] -- S ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-PICK2 execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K47 ( R [ -- S ] f -- S ) IF drop [: 1 ;] THEN execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" a family argument demands its declared callable" T-LABEL
   s" CS-K49 ( R -- R ) CHECKER-SOUNDNESS:TAIL-QV @ CHECKER-SOUNDNESS:TAIL-VK" CHECK-QUIET-CANDIDATE! 0 T=
   s" a type variable may not take a held tail inside a family argument" T-LABEL
   s" CS-K88 ( R -- R ) [: [: ;] CHECKER-SOUNDNESS:TAIL-HF ;] CHECKER-SOUNDNESS:TAIL-TV drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K89 ( R -- R ) [: [: ;] CHECKER-SOUNDNESS:TAIL-HG ;] CHECKER-SOUNDNESS:TAIL-TV drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" a family argument stays demanded through a retag" T-LABEL
   s" CS-K75 ( R -- R ) CHECKER-SOUNDNESS:TAIL-QV @ CHECKER-SOUNDNESS:TAIL-QB @ CHECKER-SOUNDNESS:TAIL-RETAG CHECKER-SOUNDNESS:TAIL-VK" CHECK-QUIET-CANDIDATE! 0 T=
   s" a pointee stays demanded through a retag" T-LABEL
   s" CS-K76 ( R -- R ) CHECKER-SOUNDNESS:TAIL-QB CHECKER-SOUNDNESS:TAIL-QB @ CHECKER-SOUNDNESS:TAIL-RETAGP CHECKER-SOUNDNESS:TAIL-STORE" CHECK-QUIET-CANDIDATE! 0 T=
   s" a nested family argument stays demanded through a retag" T-LABEL
   s" CS-K77 ( R -- R ) CHECKER-SOUNDNESS:TAIL-QVV @ CHECKER-SOUNDNESS:TAIL-QB @ CHECKER-SOUNDNESS:TAIL-RETAGN CHECKER-SOUNDNESS:TAIL-VKN" CHECK-QUIET-CANDIDATE! 0 T=
   s" a view's element type stays demanded inside a record" T-LABEL
   s" CS-K78 ( R CHECKER-SOUNDNESS:tailview<p,q,a,[ -- ]> -- R CHECKER-SOUNDNESS:tailview<p,q,a,[ R -- R ]> ) CHECKER-SOUNDNESS:TAIL-TAKEV" CHECK-QUIET-CANDIDATE! 0 T=
   s" a literal stored beside a cell takes the cell's row" T-LABEL
   s" CS-K79 ( -- ) CHECKER-SOUNDNESS:TAIL-QI [: 1 + ;] CHECKER-SOUNDNESS:TAIL-ST" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K80 ( -- ) CHECKER-SOUNDNESS:TAIL-Q0 [: ;] CHECKER-SOUNDNESS:TAIL-ST" CHECK-QUIET-CANDIDATE! -1 T=
   s" a literal pinned to the caller's row is not the cell's row" T-LABEL
   s" CS-K81 ( R -- R ) [: ;] CHECKER-SOUNDNESS:TAIL-PIN CHECKER-SOUNDNESS:TAIL-Q0 swap CHECKER-SOUNDNESS:TAIL-ST" CHECK-QUIET-CANDIDATE! 0 T=
   s" a provided callable is instantiated on a copy, never retyping its cell" T-LABEL
   s" CS-K82 ( R [ R -- R ] -- R ) {: q :} CHECKER-SOUNDNESS:TAIL-Q0 [: ;] 2dup CHECKER-SOUNDNESS:TAIL-ST {: p l :} l CHECKER-SOUNDNESS:TAIL-PIN drop q p !"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K83 ( R [ R -- R ] -- R ) {: q :} CHECKER-SOUNDNESS:TAIL-Q0 dup @ {: p l :} l CHECKER-SOUNDNESS:TAIL-PIN drop q p !"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K84 ( R [ R -- R ] -- R ) {: q :} CHECKER-SOUNDNESS:TAIL-Q0 [: ;] 2dup swap ! {: p l :} l CHECKER-SOUNDNESS:TAIL-PIN drop q p !"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared callable pinned to the caller's row still runs on an empty stack" T-LABEL
   s" CS-K87 ( R [ -- ] -- R ) {: q :} q CHECKER-SOUNDNESS:TAIL-PIN drop q CHECKER-SOUNDNESS:TAIL-TAKE0"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a family built around a literal takes the declared callable's row" T-LABEL
   s" CS-K74 ( -- ) [: 1 + ;] CHECKER--SOUNDNESS-TAILHOOK:MAKE CHECKER-SOUNDNESS:TAIL-QH !" CHECK-QUIET-CANDIDATE! -1 T=
   s" a does> clause forwarding its row is refused" T-LABEL
   s" : CS-K37 ( -- ) create does> ( R [ R -- S ] -- S ) drop CHECKER-SOUNDNESS:TAIL-G ;" 70
   s" callable row escape" SUBJECT:REFUSED? TTRUE
   s" : CS-K38 ( -- ) create does> ( R [ A -- B ] -- B ) drop CHECKER-SOUNDNESS:TAIL-G ;" 70
   s" callable row escape" SUBJECT:REFUSED? TTRUE
   s" a store or an install demands the declared callable" T-LABEL
   s" CS-K39 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-G CHECKER-SOUNDNESS:TAIL-QN !" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K48 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-UNDER CHECKER-SOUNDNESS:TAIL-QR !" CHECK-QUIET-CANDIDATE! 0 T=
   s" ' CHECKER-SOUNDNESS:TAIL-G CHECKER-SOUNDNESS:TAIL-QN !" 70
   s" callable row escape" SUBJECT:REFUSED? TTRUE
   s" ' CHECKER-SOUNDNESS:TAIL-UNDER CHECKER-SOUNDNESS:TAIL-QR !" 70
   s" callable row escape" SUBJECT:REFUSED? TTRUE
   s" CS-K41 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-G is CHECKER-SOUNDNESS:TAIL-DF" CHECK-QUIET-CANDIDATE! 0 T=
   s" an install with is names the escape" T-LABEL
   s" : CS-K40 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s" at 'is' callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on; held row A would escape into C expected: [ A -- B ] actual: [ C -- D ]" SUBJECT:REFUSED? TTRUE
   s" -1 JSON-DIAGS ! : CS-K73 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\",\"verdict\":\"rejected\",\"word\":\"cs-k73\",\"token\":\"is\",\"reason\":\"callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on; held row A would escape into C\"" SUBJECT:REFUSED? TTRUE
   s" -1 JSON-DIAGS ! : CS-K73 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" \"inferred_effect\":\"-- [ [ -- ] -- ] \",\"return_stack\"" SUBJECT:REFUSED? TTRUE
   s" CS-K42 ( -- ) [: ;] CHECKER-SOUNDNESS:TAIL-QB !" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K43 ( R [ R -- S ] -- R ) CHECKER-SOUNDNESS:TAIL-QB !" CHECK-QUIET-CANDIDATE! 0 T= ;


: CALLABLE-SOUND ( -- )
   s" and the sound shapes still certify" T-LABEL
   s" CS-K50 ( R [ R -- R [ R -- R [ -- B ] ] ] -- B ) CHECKER-SOUNDNESS:TAIL-N3" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K51 ( R [ R [ R [ -- S ] -- S ] -- S ] -- S ) CHECKER-SOUNDNESS:TAIL-D3" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K52 ( R [ R [ R [ -- B ] -- B ] -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-D3" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K53 ( R [ R -- R [ R [ R [ -- S ] -- S ] -- S ] ] -- S ) CHECKER-SOUNDNESS:TAIL-M4" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K54 ( R -- R [ R [ -- B ] -- B ] ) [: CHECKER-SOUNDNESS:TAIL-ONCE ;]" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K55 ( R -- R [ R -- R n ] ) [: 1 ;]" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K56 ( R [ -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-T1" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K57 ( -- ) [: ;] CHECKER-SOUNDNESS:TAIL-U1" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K59 ( R [ R -- S ] n -- S ) dup 0 <> IF 1 - RECURSE EXIT THEN drop execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K60 ( R [ -- S ] n -- S ) dup 0 <> IF 1 - CHECKER-SOUNDNESS:TAIL-RC3 EXIT THEN drop execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K61 ( R [ -- ] n -- R ) dup 0 <> IF 1 - >r 7 swap r> RECURSE drop EXIT THEN drop execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K62 ( A -- A ) [: ;] CHECKER-SOUNDNESS:TAIL-KEEP" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K64 ( x -- x ) [: ;] CHECKER-SOUNDNESS:TAIL-UNDER" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K66 ( A -- A ) [: ;] dup CHECKER-SOUNDNESS:TAIL-KEEP CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T=
   \ A literal demanding a named row is a value: its own row instantiates.
   s" CS-K67 ( -- ) [: CHECKER-SOUNDNESS:TAIL-NAMED ;] CHECKER-SOUNDNESS:TAIL-QD !" CHECK-QUIET-CANDIDATE! -1 T=
   \ A loaded callable is provided whatever its cell declares.
   s" CS-K68 ( x -- x n ) CHECKER-SOUNDNESS:TAIL-QA @ CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K69 ( R [ -- S ] -- R ) CHECKER-SOUNDNESS:TAIL-QB !" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K70 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-ONCE CHECKER-SOUNDNESS:TAIL-QD !" CHECK-QUIET-CANDIDATE! -1 T= ;


: RETURN-ROW ( -- )
   s" a read below the definition's own return frame is refused" T-LABEL
   s" CS-R1 ( -- n ) r@" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R2 ( -- n n ) 2r@" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R3 ( n -- n n n ) >r 2r@ r>" CHECK-QUIET-CANDIDATE! 0 T=
   s" a pop below the frame is a borrow even when the balance is restored" T-LABEL
   s" CS-R4 ( -- n ) r> dup >r" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R5 ( -- n n ) 2r> 2dup 2>r" CHECK-QUIET-CANDIDATE! 0 T=
   s" a quotation cannot borrow on the definition's behalf" T-LABEL
   s" CS-R6 ( -- n ) [: r> dup >r ;] execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R7 ( n -- n ) [: drop r@ ;] catch drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R7B ( n -- n ) [: 1 + ;] catch drop" CHECK-QUIET-CANDIDATE! -1 T=
   s" a product below the frame cannot be borrowed by a pair read" T-LABEL
   s" CS-R8 ( CHECKER-SOUNDNESS:point -- n CHECKER-SOUNDNESS:point ) >r 2r@ r> drop"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" cells this definition pushed may be read" T-LABEL
   s" CS-R9 ( n -- n n ) >r r@ r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R10 ( n n -- n n n n ) 2>r 2r@ 2r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R11 ( n -- n n ) >r begin r@ 0= until r@ r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R12 ( CHECKER-SOUNDNESS:point -- CHECKER-SOUNDNESS:point CHECKER-SOUNDNESS:point ) >r r@ r>"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a signature may not declare a return cell" T-LABEL
   s" CS-R13 ( R | S n -- R n | S n ) r@" CHECK-QUIET-CANDIDATE! 0 T= ;

\ An R-to-S quotation is the only way to S: a body that never runs it on some
\ path unifies the declared rows, and the caller could pick any S.
: ROW-QUANTIFIERS ( -- )
   s" a body that drops its R-to-S quotation is refused" T-LABEL
   s" CS-W1 ( R [ R -- S ] -- S ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W2 ( R CHECKER-SOUNDNESS:point [ R CHECKER-SOUNDNESS:point -- S CHECKER-SOUNDNESS:point ] -- S CHECKER-SOUNDNESS:point ) drop"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W3 ( R f [ R -- S ] -- S ) swap IF execute ELSE drop THEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W4 ( R [ R -- S ] [ S -- T ] -- T ) nip execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared row stays distinct from the unnamed row" T-LABEL
   s" CS-W5 ( R n -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared row is not given fixed cells" T-LABEL
   s" CS-W6 ( R -- S ) 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" the unnamed row is not given fixed cells either" T-LABEL
   s" CS-W10 ( R n [ R n -- ] -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W11 ( R f [ R -- ] -- ) swap IF execute ELSE drop 1 THEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W12 ( R [ R -- ] -- ) drop 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W13 ( R -- ) 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W14 ( R n -- )" CHECK-QUIET-CANDIDATE! 0 T=
   s" running the quotation on every path keeps the rows distinct" T-LABEL
   s" CS-W7 ( R [ R -- S ] -- S ) execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-W8 ( R f [ R -- S ] -- S ) swap IF execute ELSE execute THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-W9 ( R [ R -- S ] [ S -- R ] -- R ) drop drop" CHECK-QUIET-CANDIDATE! -1 T= ;

: SIGNATURE-END ( -- )
   s" tokens after the output side are a syntax error" T-LABEL
   s" CS-S1 ( n -- n -- n n ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-S2 ( n -- n n ) dup" CHECK-QUIET-CANDIDATE! -1 T=
   s" a quotation effect inside the signature still closes cleanly" T-LABEL
   s" CS-S3 ( n [ n -- n ] -- n ) execute" CHECK-QUIET-CANDIDATE! -1 T= ;


: RUN ( -- )
   T-RESET
   QUOTATION-INPUTS
   CALLABLE-TAILS
   CALLABLE-HOLDS
   CALLABLE-SOUND
   RETURN-ROW
   ROW-QUANTIFIERS
   SIGNATURE-END
   T-REPORT ;

RUN
;package
