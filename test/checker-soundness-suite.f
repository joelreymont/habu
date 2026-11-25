\ checker-soundness-suite.f - a certified body must not exceed its declared effect.

require src/lib/adt/option.f
require src/lib/test/assert.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require src/lib/process.f

package CHECKER-SOUNDNESS
public

STRUCTURE point FIELD x n FIELD y n ;STRUCTURE

: TAKE-U8 ( u8 -- ) drop ;
: TAKE-I64 ( i64 -- ) drop ;
: MK-I64 ( -- i64 ) 300 ;
: MK-U8 ( -- u8 ) 3 ;
: CALL-I64 ( [ i64 -- ] -- ) MK-I64 swap execute ;
: CALL-U8 ( [ u8 -- ] -- ) MK-U8 swap execute ;
: CALL-I64-Q ( [ [ i64 -- ] -- ] -- ) ['] TAKE-I64 swap execute ;
: CALL-U8-Q ( [ [ u8 -- ] -- ] -- ) ['] TAKE-U8 swap execute ;
defer SET-U8 ( u8 -- )
defer SET-I64 ( i64 -- )
defer GET-U8 ( -- u8 )
defer GET-I64 ( -- i64 )

\ A callable declared `[ -- S ]` runs on whatever stack the callee holds.
: TAIL-TWICE ( R [ -- S ] -- S ) dup >r execute r> execute ;
: TAIL-ONCE ( R [ -- S ] -- S ) execute ;
: TAIL-NAMED ( R [ R -- S ] -- S ) execute ;
: TAIL-KEEP ( R [ -- ] -- R ) execute ;
: TAIL-ARG ( R [ n -- ] -- R ) 1 swap execute ;
: TAIL-NEST ( R [ R -- R [ -- S ] ] -- S ) execute execute ;
: TAIL-UNDER ( R [ -- ] -- R ) >r 5 r> execute drop ;
: TAIL-APPLY ( R [ R [ R -- R ] -- R ] [ R -- R ] -- R ) swap execute ;
: TAIL-G ( R [ -- S ] -- S ) >r 5 r> execute ;
: TAIL-PICK1 ( a a -- a ) nip ;
: TAIL-PICK2 ( a a -- a ) drop ;
: TAIL-EQ2 ( a a -- ) 2drop ;
: TAIL-K ( a b [ a b -- ] -- ) execute ;
: TAIL-N3 ( R [ R -- R [ R -- R [ -- S ] ] ] -- S ) execute execute execute ;
: TAIL-D3 ( R [ R [ R [ -- S ] -- S ] -- S ] -- S ) [: TAIL-G ;] swap execute ;
: TAIL-M4 ( R [ R -- R [ R [ R [ -- S ] -- S ] -- S ] ] -- S ) execute [: TAIL-G ;] swap execute ;
: TAIL-RC3 ( R [ -- S ] n -- S ) dup 0 <> IF 1 - >r 7 swap r> RECURSE EXIT THEN drop execute ;
: TAIL-MKT ( -- ) create does> ( R [ -- S ] -- S ) drop execute ;
TAIL-MKT TAIL-T1
: TAIL-MKU ( -- ) create does> ( R [ -- ] -- R ) drop >r 9 r> execute drop ;
TAIL-MKU TAIL-U1
TYPED-VARIABLE TAIL-QA [ -- n ]
TYPED-VARIABLE TAIL-QR [ R [ R -- R ] -- R ]
defer TAIL-DF ( R [ R -- S ] -- S )
\ A family argument is read and written, as a pointee is.
STRUCTURE tailvals<a> OPAQUE FIELD data ^a ;STRUCTURE
: TAIL-VPUT ( a tailvals<a> -- ) TAILVALS-UNMAKE ! ;
: TAIL-VK ( R tailvals<[ R -- R ]> -- R ) [: ;] swap TAIL-VPUT ;
: TAIL-VKN ( R tailvals<tailvals<[ R -- R ]>> -- R ) drop ;
: TAIL-RETAG ( tailvals<a> a -- tailvals<a> ) drop ;
: TAIL-RETAGN ( tailvals<tailvals<a>> a -- tailvals<tailvals<a>> ) drop ;
: TAIL-RETAGP ( ^a a -- ^a ) drop ;
: TAIL-STORE ( R ^[ R -- R ] -- R ) [: ;] swap ! ;
\ A view's element type names its storage, whatever record holds the view.
STRUCTURE tailview<a,b,c,d> FIELD view mut-view<a,b,c,d> ;STRUCTURE
: TAIL-TAKEV ( R tailview<p,q,a,[ R -- R ]> -- R tailview<p,q,a,[ R -- R ]> ) ;
\ A literal runs on any stack, so it takes the row of the cell it is stored in.
TYPED-VARIABLE TAIL-QI [ n -- n ]
TYPED-VARIABLE TAIL-Q0 [ -- ]
: TAIL-ST ( ^a a -- ) drop drop ;
: TAIL-PIN ( R [ R -- R ] -- R [ R -- R ] ) ;
: TAIL-TAKE0 ( [ -- ] -- ) drop ;
\ A family built around a literal is a value, as the literal is.
STRUCTURE tailhook<a> DERIVE addr FIELD fn a ;STRUCTURE
TYPED-VARIABLE TAIL-QH tailhook<[ n -- n ]>
: TAIL-TV ( R [ -- a ] -- R a ) execute ;
defer TAIL-HF ( R [ R -- R ] -- R ^tailhook<[ R -- R ]> )
defer TAIL-HG ( R [ R -- R ] -- R [ -- ^tailhook<[ R -- R ]> ] )
\ A literal's open input row binds to the linear value its callee keeps.
DEFLINEAR CHECKER-SOUNDNESS:ltok
: LTOK-KEEP ( CHECKER-SOUNDNESS:ltok -- CHECKER-SOUNDNESS:ltok ) ;
defer LTOK-END ( CHECKER-SOUNDNESS:ltok -- )
: TAIL-DUPAPPLY ( a [ a -- b ] -- a b ) over swap execute ;
: TAIL-APPLY2 ( a [ a -- b ] -- b b ) over over execute >r execute r> ;

\ An inferred input that is a literal's own callable, met by a caller's.
: TAIL-T6 IF drop [: ;] THEN [: ;] swap execute execute ;
: TAIL-F6 IF drop [: ;] THEN [: ;] swap finally ;
: TAIL-PICK3 ( a a bool -- a ) IF drop ELSE nip THEN ;
: TAIL-T13 [: ;] true TAIL-PICK3 [: ;] swap execute execute ;
: TAIL-RERUN ( R [ R -- R ] -- R ) dup >r execute r> execute ;

\ A type whose name is the longest a family tail may be.
NEWTYPE wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
CAST: >WIDE ( n -- wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww )


\ CS-K90's literal leaves 40 values of that type, so its escape rejection renders
\ about 10 KB as the inferred effect and as much again as the actual callable.
: CAPTURE-PAST$ ( -- str )
   s" CS-K90 ( -- ) [: 0 CHECKER-SOUNDNESS:>WIDE dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup ;] CHECKER-SOUNDNESS:TAIL-ONCE" ;


$1000 constant CAP
create OUT CAP allot   variable OUT-U
create ERR CAP allot   variable ERR-U
: OUT$ ( -- str ) OUT OUT-U @ >str ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;
\ Source src, run as the subject, exits with code want; OUT$ and ERR$ hold its
\ streams until the next run.
: RUNS ( str n -- ) {: src:str want:n :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   src OUT$ ERR$ oc want T-ENDING-EXITED= ;


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
   s" an install with is takes a wider input and a narrower output, as a call does" T-LABEL
   s" CS-Q8 ( -- ) ['] CHECKER-SOUNDNESS:TAKE-I64 is CHECKER-SOUNDNESS:SET-U8"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-Q9 ( -- ) ['] CHECKER-SOUNDNESS:TAKE-U8 is CHECKER-SOUNDNESS:SET-I64"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-Q10 ( -- ) ['] CHECKER-SOUNDNESS:MK-U8 is CHECKER-SOUNDNESS:GET-I64"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-Q11 ( -- ) ['] CHECKER-SOUNDNESS:MK-I64 is CHECKER-SOUNDNESS:GET-U8"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the accepted shapes execute" T-LABEL
   [: TAKE-I64 ;] CALL-U8
   [: CALL-U8 ;] CALL-I64-Q
   ['] TAKE-I64 is SET-U8  MK-U8 SET-U8
   ['] MK-U8 is GET-I64  GET-I64 3 T=
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
   s" the rejection shows the callable's output keeping its input row" T-LABEL
   s" : CS-K10 ( -- n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" error[E-MISMATCH]: callable row escape" SUBJECT:REJECTED? TTRUE
   s" : CS-K10 ( -- n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" = found: [ C -- C n ]\n" SUBJECT:REJECTED? TTRUE
   s" : CS-K11 ( -- n ) [: ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" = found: [ C -- C ]\n" SUBJECT:REJECTED? TTRUE
   s" a callable left under its own copy names the row that would contain itself" T-LABEL
   s" : CS-K98 ( R [ R -- R ] -- R ) dup execute ;" 70 SUBJECT:REJECTED {: e:str ok:bool :}
   ok TTRUE
   e s" error[E-MISMATCH]: `execute` would make a stack row contain itself" STR:CONTAINS? TTRUE
   \ the rendering indents each note by the width of the line number's gutter
   e s\" = expected: R\n" STR:CONTAINS? TTRUE
   e s\" = found: R [ R -- R ]\n" STR:CONTAINS? TTRUE
   s" a named word's input names the row that would contain itself" T-LABEL
   s" : CS-TAKE ( R [ R -- R ] -- R ) execute ; : CS-K105 ( R [ R -- R ] -- R ) dup CS-TAKE ;" 70
   s" error[E-MISMATCH]: `CS-TAKE` would make a stack row contain itself"
      SUBJECT:REJECTED? TTRUE
   s" a recursive call's input names the row that would contain itself" T-LABEL
   s" : CS-K106 ( R [ R -- R ] bool -- R ) IF dup true RECURSE EXIT THEN execute ;" 70
   s" error[E-MISMATCH]: `RECURSE` would make a stack row contain itself"
      SUBJECT:REJECTED? TTRUE
   s" a callable copy left at the body's end is a value left over" T-LABEL
   s" : CS-K99 ( R [ R -- R ] -- R ) dup >r execute r> ;" 70
   s" error[E-MISMATCH]: CS-K99 ends with [ -- ] on the stack; its signature promises nothing"
      SUBJECT:REJECTED? TTRUE
   s" a callable copy met at an exit's join, not a call's input, keeps the mismatch text" T-LABEL
   s" : W ( S -- S [ S -- S ] ) [: ;] ; : XE ( R bool -- R [ R -- R ] ) IF W EXIT THEN W W nip ;" 70
   s" error[E-MISMATCH]: `nip` takes [ [ -- ] -- [ -- ] ]; the stack holds [ -- ]"
      SUBJECT:REJECTED? TTRUE
   s" a callable argument that leaves a callable on its own row keeps the mismatch text" T-LABEL
   s" : W ( S -- S [ S -- S ] ) [: ;] ; : CS-T ( [ A -- A ] -- ) drop ;  : CS-K103 ( -- ) [: W ;] CS-T ;" 70
   s" error[E-MISMATCH]: `CS-T` takes [ -- ]; the stack holds [ -- [ -- ] ]"
      SUBJECT:REJECTED? TTRUE
   s" a call missing the argument its callable takes keeps the mismatch text" T-LABEL
   s" : W ( S -- S [ S -- S ] ) [: ;] ; : CS-K104 ( R [ R [ R -- R ] -- R ] -- R ) execute ;" 70
   s" error[E-MISMATCH]: `execute` takes [ -- ]; the stack holds nothing"
      SUBJECT:REJECTED? TTRUE
   s" a callable copy on the return row it runs on is a return-stack rejection" T-LABEL
   s" : CS-K100 ( R [ R -- R | U -- U ] -- R ) dup >r execute r> drop ;" 70
   s" error[E-RETURN-STACK]" SUBJECT:REJECTED? TTRUE
   s" the rejection reads the same whatever the definition holds beneath it" T-LABEL
   s" : CS-K19 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" error[E-MISMATCH]: callable row escape" SUBJECT:REJECTED? TTRUE
   s" : CS-K19 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" = found: [ C -- C n ]\n" SUBJECT:REJECTED? TTRUE
   s" its inferred effect is the body's up to the rejection, not the declared one" T-LABEL
   s" -1 REJECTION:JSON ! : CS-K22 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" \"code\":\"E-MISMATCH\"" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K22 ( x -- x n n ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" \"inferred\":\"x -- x [ -- n ]\"" SUBJECT:REJECTED? TTRUE
   s" a capture past the render buffers' first sizes grows them and leaves later renders without row tails" T-LABEL
   CAPTURE-PAST$ CHECK-QUIET-CANDIDATE! 0 T=
   \ The subject is a fork of this process, so it renders with the grown buffers.
   s" : CS-K91 ( -- ) [: CHECKER-SOUNDNESS:TAKE-U8 ;] CHECKER-SOUNDNESS:CALL-I64 ;" 70
   s\" error[E-MISMATCH]: `CHECKER-SOUNDNESS:CALL-I64` takes [ i64 -- ]; the stack holds [ u8 -- ]\n"
      SUBJECT:REJECTED? TTRUE
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
   s" the rejection shows the word's own row standing in for the callable's" T-LABEL
   s" : CS-K26 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s" error[E-MISMATCH]: callable row escape" SUBJECT:REJECTED? TTRUE
   s" : CS-K26 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" = held: A would escape into R\n" SUBJECT:REJECTED? TTRUE
   s" : CS-K26 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" = found: [ R -- B ]\n" SUBJECT:REJECTED? TTRUE
   s" : CS-K27 ( R [ A -- A ] -- R ) CHECKER-SOUNDNESS:TAIL-KEEP ;" 70
   s\" = held: B would escape into A\n" SUBJECT:REJECTED? TTRUE
   s" : CS-K27 ( R [ A -- A ] -- R ) CHECKER-SOUNDNESS:TAIL-KEEP ;" 70
   s\" = found: [ A -- A ]\n" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K28 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" \"help\":\"declare the callable `[ R -- S ]` from the word's input row" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K72 ( R [ R -- B ] -- B ) CHECKER-SOUNDNESS:TAIL-ONCE ;" 70
   s\" ; held row A would escape into R\"" SUBJECT:REJECTED? TTRUE
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
   s" CS-K18 ( R [ -- S ] -- S ) CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T=
   s" a literal running a captured callable keeps the callable's row" T-LABEL
   s" : CS-K120 ( R [ R -- R ] -- R ) {: f :} 1 [: f execute ;] execute drop ;" 70
   s" error[E-MISMATCH]: `execute` takes nothing; the stack holds n" SUBJECT:REJECTED? TTRUE
   s" : CS-K121 ( R [ R -- R ] -- R ) {: f :} 1 [: f execute ;] drop f execute drop ;" 70
   s" error[E-MISMATCH]: `execute` takes nothing; the stack holds n" SUBJECT:REJECTED? TTRUE
   s" : CS-K123 ( R [ R -- R ] -- R ) {: f :} 1 [: f CHECKER-SOUNDNESS:TAIL-NAMED ;] execute drop ;" 70
   s" error[E-MISMATCH]: `execute` takes nothing; the stack holds n" SUBJECT:REJECTED? TTRUE
   s" : CS-K124 ( R [ R -- R ] bool -- R ) {: f b :} 1 [: b IF f execute ELSE f execute THEN ;] execute drop ;" 70
   s" error[E-MISMATCH]: `execute` takes nothing; the stack holds n" SUBJECT:REJECTED? TTRUE
   s" a literal's run renames its tail inside its cells too" T-LABEL
   s" : CS-EXEC ( R [ R -- R ] -- R [ R -- R ] ) dup >r execute r> ; : CS-K125 ( R [ R -- R ] -- R ) 5 swap [: CS-EXEC ;] execute drop drop ;" 70
   s" error[E-MISMATCH]: `execute` takes [ -- ]; the stack holds n [ -- ]" SUBJECT:REJECTED? TTRUE
   s" and the copy of a literal's tail stays extensible to the caller's window" T-LABEL
   s" CS-K126 ( R [ R -- S ] -- S ) [: CHECKER-SOUNDNESS:TAIL-NAMED ;] execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" and a stale tail a binding made a push is renamed with the push's tail" T-LABEL
   s" : CS-EXEC ( R [ R -- R ] -- R [ R -- R ] ) dup >r execute r> ; : CS-K132 ( R [ R n -- R n ] -- R ) [: 5 swap [: [: CS-EXEC ;] catch drop drop ;] execute ;] execute ;" 70
   s" error[E-MISMATCH]: CS-K132 ends with stale<R> on the stack" SUBJECT:REJECTED? TTRUE
   s" a literal's call keeps the linear value its open row was bound to" T-LABEL
   s" CS-K127 ( CHECKER-SOUNDNESS:ltok -- CHECKER-SOUNDNESS:ltok ) [: ['] CHECKER-SOUNDNESS:LTOK-KEEP CHECKER-SOUNDNESS:TAIL-NAMED ;] CHECKER-SOUNDNESS:TAIL-NAMED"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K128 ( CHECKER-SOUNDNESS:ltok -- CHECKER-SOUNDNESS:ltok ) [: ['] CHECKER-SOUNDNESS:LTOK-KEEP ['] CHECKER-SOUNDNESS:TAIL-NAMED execute ;] CHECKER-SOUNDNESS:TAIL-NAMED"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" and one that copies it is rejected" T-LABEL
   s" : CS-K129 ( CHECKER-SOUNDNESS:ltok -- CHECKER-SOUNDNESS:ltok CHECKER-SOUNDNESS:ltok ) [: ['] CHECKER-SOUNDNESS:LTOK-KEEP CHECKER-SOUNDNESS:TAIL-DUPAPPLY ;] CHECKER-SOUNDNESS:TAIL-NAMED ;" 70
   s" error[E-LINEAR-COPY]: a linear value is copied here" SUBJECT:REJECTED? TTRUE
   s" : CS-K130 ( CHECKER-SOUNDNESS:ltok -- CHECKER-SOUNDNESS:ltok CHECKER-SOUNDNESS:ltok ) [: ['] CHECKER-SOUNDNESS:LTOK-KEEP CHECKER-SOUNDNESS:TAIL-APPLY2 ;] CHECKER-SOUNDNESS:TAIL-NAMED ;" 70
   s" error[E-LINEAR-COPY]: a linear value is copied here" SUBJECT:REJECTED? TTRUE
   s" and one that drops it is rejected" T-LABEL
   s" : CS-K131 ( CHECKER-SOUNDNESS:ltok -- ) [: ['] CHECKER-SOUNDNESS:LTOK-END CHECKER-SOUNDNESS:TAIL-NAMED ;] execute ;" 70
   s" error[E-LINEAR-DROP]: a linear value is discarded here" SUBJECT:REJECTED? TTRUE
   s" a literal's callable demanded by a caller's input is held rigid" T-LABEL
   s" CS-K94 ( -- ) 5 [: drop 7 ;] false CHECKER-SOUNDNESS:TAIL-T6 ." CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K95 ( -- ) 5 [: drop 7 ;] false [: IF drop [: ;] THEN [: ;] swap execute execute ;] execute ." CHECK-QUIET-CANDIDATE! 0 T=
   s" a literal's cleanup demanded by a caller's input is held rigid" T-LABEL
   s" CS-K113 ( -- ) [: swap ;] false CHECKER-SOUNDNESS:TAIL-F6" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K114 ( -- ) [: swap ;] false [: IF drop [: ;] THEN [: ;] swap finally ;] execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K96 ( -- ) 5 [: drop 7 ;] CHECKER-SOUNDNESS:TAIL-T13 ." CHECK-QUIET-CANDIDATE! 0 T=
   s" a back edge tying one tail of a run quotation to another rejects" T-LABEL
   s" : CS-K115 ( n -- ) [: CHECKER-SOUNDNESS:TAIL-NAMED ;] swap 0 ?do [: 1 ;] over execute drop drop [: CHECKER-SOUNDNESS:TAIL-RERUN ;] loop drop ;" 70
   s" = found: [ C [ C -- C ] -- C ]" SUBJECT:REJECTED? TTRUE
   s" a callable run through a quotation leaves nothing, so `.` after it underflows" T-LABEL
   s" : CS-RUN ( R [ R -- S ] -- S ) execute ; : CS-K107 ( -- ) [: ;] [: CS-RUN ;] execute . ; CS-K107" 70
   s" error[E-INPUT-UNDERFLOW]" SUBJECT:REJECTED? TTRUE
   s" a quotation kept in a local runs callables of two effects, each on its own" T-LABEL
   s" : CS-RUN ( R [ R -- S ] -- S ) execute ; : CS-K109 ( -- ) [: CS-RUN ;] {: k :} 1 2 [: + ;] k execute . 5 [: 1 + ;] k execute . ; CS-K109"
      0 RUNS
   OUT$ s\" 3\n6\n" T$=
   s" a literal handed to a word naming its input row is copied, not bound in place" T-LABEL
   s" : CS-RUN2 ( R [ R -- S ] -- S ) execute ; : CS-K110 ( -- ) [: ;] dup CS-RUN2 drop ; CS-K110" 0
      SUBJECT:EXITED? TTRUE
   s" a word's fixed [ -- ] does not meet a declared [ n -- n ] slot" T-LABEL
   s" : CS-KQ ( -- [ -- ] ) [: ;] ; : CS-QW1 ( [ n -- n ] -- ) drop ; : CS-K111 ( -- ) CS-KQ CS-QW1 ; CS-K111" 70
   s" error[E-MISMATCH]: `CS-QW1` takes [ n -- n ]; the stack holds [ -- ]" SUBJECT:REJECTED? TTRUE ;


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
   s" CS-K92 ( R [ R [ R -- R ] -- R ] -- R ) [: CHECKER-SOUNDNESS:TAIL-UNDER ;] [: CHECKER-SOUNDNESS:TAIL-EQ2 ;] CHECKER-SOUNDNESS:TAIL-K"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K93 ( R [ R [ R -- R ] -- R ] -- R ) [: CHECKER-SOUNDNESS:TAIL-UNDER ;] swap [: CHECKER-SOUNDNESS:TAIL-EQ2 ;] CHECKER-SOUNDNESS:TAIL-K"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the rejection names the side whose row escaped, whichever branch holds it" T-LABEL
   s" : CS-K71 ( R [ R [ R -- R ] -- R ] [ R -- R ] f -- R ) swap >r IF drop [: CHECKER-SOUNDNESS:TAIL-UNDER ;] ELSE THEN r> CHECKER-SOUNDNESS:TAIL-APPLY ;" 70
   s\" = held: A would escape into R\n" SUBJECT:REJECTED? TTRUE
   s" : CS-K71 ( R [ R [ R -- R ] -- R ] [ R -- R ] f -- R ) swap >r IF drop [: CHECKER-SOUNDNESS:TAIL-UNDER ;] ELSE THEN r> CHECKER-SOUNDNESS:TAIL-APPLY ;" 70
   s\" = found: [ R -- R ]\n" SUBJECT:REJECTED? TTRUE
   \ Joins of two values: neither may narrow the other's declared row.
   s" CS-K44 ( R [ -- S ] -- S ) [: 1 ;] swap CHECKER-SOUNDNESS:TAIL-PICK2 execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K45 ( R [ -- S ] -- S ) [: 1 ;] CHECKER-SOUNDNESS:TAIL-PICK2 execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K47 ( R [ -- S ] f -- S ) IF drop [: 1 ;] THEN execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" a family argument demands its declared callable" T-LABEL
   s" CS-K49 ( R ^CHECKER-SOUNDNESS:tailvals<[ -- R ]> -- R ) @ CHECKER-SOUNDNESS:TAIL-VK" CHECK-QUIET-CANDIDATE! 0 T=
   s" a type variable may not take a held tail inside a family argument" T-LABEL
   s" CS-K88 ( R -- R ) [: [: ;] CHECKER-SOUNDNESS:TAIL-HF ;] CHECKER-SOUNDNESS:TAIL-TV drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K89 ( R -- R ) [: [: ;] CHECKER-SOUNDNESS:TAIL-HG ;] CHECKER-SOUNDNESS:TAIL-TV drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" a family argument stays demanded through a retag" T-LABEL
   s" CS-K75 ( R ^CHECKER-SOUNDNESS:tailvals<[ -- R ]> ^[ -- R ] -- R ) swap @ swap @ CHECKER-SOUNDNESS:TAIL-RETAG CHECKER-SOUNDNESS:TAIL-VK" CHECK-QUIET-CANDIDATE! 0 T=
   s" a pointee stays demanded through a retag" T-LABEL
   s" CS-K76 ( R ^[ -- R ] -- R ) dup @ CHECKER-SOUNDNESS:TAIL-RETAGP CHECKER-SOUNDNESS:TAIL-STORE" CHECK-QUIET-CANDIDATE! 0 T=
   s" a nested family argument stays demanded through a retag" T-LABEL
   s" CS-K77 ( R ^CHECKER-SOUNDNESS:tailvals<CHECKER-SOUNDNESS:tailvals<[ -- R ]>> ^[ -- R ] -- R ) swap @ swap @ CHECKER-SOUNDNESS:TAIL-RETAGN CHECKER-SOUNDNESS:TAIL-VKN" CHECK-QUIET-CANDIDATE! 0 T=
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
   s" a does> clause forwarding its row is rejected" T-LABEL
   s" : CS-K37 ( -- ) create does> ( R [ R -- S ] -- S ) drop CHECKER-SOUNDNESS:TAIL-G ;" 70
   s" callable row escape" SUBJECT:REJECTED? TTRUE
   s" : CS-K38 ( -- ) create does> ( R [ A -- B ] -- B ) drop CHECKER-SOUNDNESS:TAIL-G ;" 70
   s" callable row escape" SUBJECT:REJECTED? TTRUE
   s" a store or an install demands the declared callable" T-LABEL
   s" CS-K39 ( ^[ R [ R -- S ] -- S ] -- ) ['] CHECKER-SOUNDNESS:TAIL-G swap !" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K48 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-UNDER CHECKER-SOUNDNESS:TAIL-QR !" CHECK-QUIET-CANDIDATE! 0 T=
   s" ' CHECKER-SOUNDNESS:TAIL-UNDER CHECKER-SOUNDNESS:TAIL-QR !" 70
   s" callable row escape" SUBJECT:REJECTED? TTRUE
   s" CS-K41 ( -- ) ['] CHECKER-SOUNDNESS:TAIL-G is CHECKER-SOUNDNESS:TAIL-DF" CHECK-QUIET-CANDIDATE! 0 T=
   s" an install with is names the escape" T-LABEL
   s" : CS-K40 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s" error[E-MISMATCH]: callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on; held row A would escape into C" SUBJECT:REJECTED? TTRUE
   s" : CS-K40 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" = found: [ C -- D ]\n" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K73 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" \"code\":\"E-MISMATCH\",\"message\":\"callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on; held row A would escape into C\"" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K73 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" \"token\":\"is\"" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-K73 ( -- ) [: CHECKER-SOUNDNESS:TAIL-G ;] is CHECKER-SOUNDNESS:TAIL-DF ;" 70
   s\" \"inferred\":\"-- [ E [ A -- F ] -- F ]\"" SUBJECT:REJECTED? TTRUE
   s" CS-K42 ( ^[ -- S ] -- ) [: ;] swap !" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-K43 ( R [ R -- S ] ^[ -- S ] -- R ) !" CHECK-QUIET-CANDIDATE! 0 T=
   s" a cell over a callable whose tails differ is rejected at its declaration" T-LABEL
   s" TYPED-VARIABLE CS-QB [ -- S ]" 70
   s" error[E-BAD-STORAGE]: the type of `CS-QB` may hold a linear value" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QB [ -- S ]" 70
   s" = evaluated: TYPED-VARIABLE CS-QB [ -- S ]" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QD [ [ -- S ] -- S ]" 70
   s" error[E-BAD-STORAGE]: the type of `CS-QD` may hold a linear value" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QD [ [ -- S ] -- S ]" 70
   s" = evaluated: TYPED-VARIABLE CS-QD [ [ -- S ] -- S ]" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QN [ R [ R -- S ] -- S ]" 70
   s" error[E-BAD-STORAGE]: the type of `CS-QN` may hold a linear value" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QN [ R [ R -- S ] -- S ]" 70
   s" = evaluated: TYPED-VARIABLE CS-QN [ R [ R -- S ] -- S ]" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QV CHECKER-SOUNDNESS:tailvals<[ -- S ]>" 70
   s" error[E-BAD-STORAGE]: the type of `CS-QV` may hold a linear value" SUBJECT:REJECTED? TTRUE
   s" TYPED-VARIABLE CS-QVV CHECKER-SOUNDNESS:tailvals<CHECKER-SOUNDNESS:tailvals<[ -- S ]>>" 70
   s" error[E-BAD-STORAGE]: the type of `CS-QVV` may hold a linear value" SUBJECT:REJECTED? TTRUE ;


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
   s" CS-K67 ( ^[ [ -- S ] -- S ] -- ) [: CHECKER-SOUNDNESS:TAIL-NAMED ;] swap !" CHECK-QUIET-CANDIDATE! -1 T=
   \ A loaded callable is provided whatever its cell declares.
   s" CS-K68 ( x -- x n ) CHECKER-SOUNDNESS:TAIL-QA @ CHECKER-SOUNDNESS:TAIL-NAMED" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K69 ( R [ -- S ] ^[ -- S ] -- R ) !" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-K70 ( ^[ [ -- S ] -- S ] -- ) ['] CHECKER-SOUNDNESS:TAIL-ONCE swap !" CHECK-QUIET-CANDIDATE! -1 T= ;


: RETURN-ROW ( -- )
   s" a read below the definition's own return frame is rejected" T-LABEL
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
   s" a body that drops its R-to-S quotation is rejected" T-LABEL
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

\ A local, typed or not, never captures a cell whose width is open where it binds,
\ the word's own declared variable included: it would store one cell of a wider value.
: OPEN-WIDTH-LOCALS ( -- )
   s" a local over an open-width cell is rejected at its bind" T-LABEL
   s" : CS-L1 ( -- ) OPTION:NONE {: x:option<t> :} ;" 70
   s\" error[E-MISMATCH]: `:}` takes option<t>; the stack holds option<a>\n" SUBJECT:REJECTED? TTRUE
   s" : CS-L2 ( a -- n option<a> ) OPTION:SOME {: x:option<a> :} 7 x ;" 70
   s\" error[E-MISMATCH]: `:}` takes option<a>; the stack holds option<a>\n" SUBJECT:REJECTED? TTRUE
   s" : CS-L3 ( option<a> -- n option<a> ) {: o:option<a> :} 7 o ;" 70
   s\" error[E-MISMATCH]: `:}` takes option<a>; the stack holds option<a>\n" SUBJECT:REJECTED? TTRUE
   s" : CS-L3U {: o:option<a> :} 7 o ;" 70
   s\" error[E-MISMATCH]: `:}` takes option<a>; the stack holds option<a>\n" SUBJECT:REJECTED? TTRUE
   s" : CS-L4 ( -- ) OPTION:NONE 5 {: x:option<t> y:t :} ;" 70
   s\" error[E-MISMATCH]: `:}` takes option<a>; the stack holds option<b>\n" SUBJECT:REJECTED? TTRUE
   s" : CS-L6 ( -- ) OPTION:NONE 5 OPTION:SOME {: x y :} ;" 70
   s\" error[E-MISMATCH]: `:}` takes a; the stack holds option<b>\n" SUBJECT:REJECTED? TTRUE
   s" CS-L5 ( ptr<s,option<a>> -- ptr<s,option<a>> ) {: p:ptr<s,option<a>> :} p"
      CHECK-QUIET-CANDIDATE! -1 T= ;

: LOCALS-UNDERFLOW ( -- )
   s" a locals group that reads below the declared inputs is an underflow" T-LABEL
   s" : CS-L7 ( -- ) 5 OPTION:SOME {: a u :} ;" 70
   s\" error[E-INPUT-UNDERFLOW]: input underflow: the call takes more cells than the definition's declared inputs leave (needs 3, has 2)\n"
      SUBJECT:REJECTED? TTRUE ;

: SIGNATURE-END ( -- )
   s" tokens after the output side are a syntax error" T-LABEL
   s" CS-S1 ( n -- n -- n n ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-S2 ( n -- n n ) dup" CHECK-QUIET-CANDIDATE! -1 T=
   s" a quotation effect inside the signature still closes cleanly" T-LABEL
   s" CS-S3 ( n [ n -- n ] -- n ) execute" CHECK-QUIET-CANDIDATE! -1 T= ;


: WIDE-COUNT ( -- )
   s" depth and .s over a wide value or one of open width are rejected, naming the word and the value; a one-cell value is counted" T-LABEL
   s" ENUM csdone solo pair ;ENUM : CS-D2 ( -- option<a> option<a> ) OPTION:NONE OPTION:NONE ; : CS-D3 ( option<n> -- ) drop ; : CS-D5 ( -- option<n> csdone n ) CS-D2 CS-D3 CHECKER--SOUNDNESS-CSDONE:SOLO depth ;" 70
   s" error[E-DEPTH-WIDE]: cannot count `option<n>`: it is one value held in 2 cells" SUBJECT:REJECTED? TTRUE
   s\" ENUM csdone solo pair ;ENUM : CS-D6 ( -- csdone n ) CHECKER--SOUNDNESS-CSDONE:SOLO depth ; : CS-D7 ( -- ) CS-D6 nip 1 <> IF s\" depth counted a one-cell enum as other than 1\" 1 die THEN ; CS-D7" 0
      SUBJECT:EXITED? TTRUE
   s" -1 REJECTION:JSON ! : CS-D4 ( -- option<n> ) 3 OPTION:SOME .s ;" 70
   s\" \"code\":\"E-DEPTH-WIDE\",\"message\":\"cannot count `option<n>`: it is one value held in 2 cells\"" SUBJECT:REJECTED? TTRUE
   s" -1 REJECTION:JSON ! : CS-D4 ( -- option<n> ) 3 OPTION:SOME .s ;" 70
   s\" \"token\":\".s\"" SUBJECT:REJECTED? TTRUE
   s" : CS-D8 ( option<a> -- option<a> n ) depth ;" 70
   s" error[E-DEPTH-WIDE]: cannot count `option<a>`: it is one value, and how many cells it holds is not known here" SUBJECT:REJECTED? TTRUE ;


: RUN ( -- )
   T-RESET
   QUOTATION-INPUTS
   CALLABLE-TAILS
   CALLABLE-HOLDS
   CALLABLE-SOUND
   RETURN-ROW
   ROW-QUANTIFIERS
   OPEN-WIDTH-LOCALS
   LOCALS-UNDERFLOW
   SIGNATURE-END
   WIDE-COUNT
   T-REPORT ;

RUN
;package
