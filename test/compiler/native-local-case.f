\ A local answers to its declared spelling byte for byte; word lookup stays
\ case-insensitive, so another spelling is the word (global, package public,
\ builtin, loop index, control word) or undefined. The checker (src/core/checker.f
\ LOC-REF?) and tier 0 (src/habu/t0.f DO-LOCAL) must agree: a disagreement
\ compiles a certified body differently with no diagnostic.
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package LOCAL-CASE-TEST

$1000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: STORE ( len len PROCESS:ending ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH PROCESS:ending
      exited OF RC ! true EXITED ! ENDOF
      signaled OF RC ! false EXITED ! ENDOF
      timeout OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN-SOURCE ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN src u STORE ;

: EXPECT-RC ( n -- ) {: want:n :}
   EXITED @ 0= RC @ want <> or if ERR$ type OUT$ type then
   EXITED @ TTRUE
   RC @ want T= ;

: EXPECT-OUT ( ptr u8 n -- )
   0 EXPECT-RC
   ERR-U @ 0 T=
   OUT$ STR= TTRUE ;

: SCOPE-SOURCE$ ( -- ptr u8 n )
   S\" package LC-SUBJECT\npublic\n: LATEST ( n n -- n ) {: item:n ITEM:n :} ITEM ;\n: EARLIER ( n n -- n ) {: item:n ITEM:n :} item ;\n: REPEATED ( n n -- n ) {: v:n v:n :} v ;\n: SHADOW ( n -- n ) {: value:n :} value 0 > if false {: value:bool :} value if 1 else 2 then else 0 then value + ;\n: DISTINCT ( n -- n ) {: value:n :} value 0 > if 100 {: VALUE:n :} VALUE value + else 0 then ;\n;package\n3 9 LC-SUBJECT:LATEST . 3 9 LC-SUBJECT:EARLIER . 3 9 LC-SUBJECT:REPEATED . 5 LC-SUBJECT:SHADOW . -1 LC-SUBJECT:SHADOW . 5 LC-SUBJECT:DISTINCT .\n" ;

\ A name whose scope has closed, a name spelled like a control word, and names
\ that are punctuation: the second half of the scope question.
: CONTROL-SOURCE$ ( -- ptr u8 n )
   S\" package LC-SUBJECT\npublic\n99 constant V\n: AFTER ( n -- n ) dup 0 > if {: v:n :} v else drop 0 then V + ;\n: INNER-V ( n -- n ) dup 0 > if {: v:n :} V else drop 0 then ;\n: TAIL ( n -- n ) dup 0 > if {: endof:n :} endof 2 * else drop 0 then ;\n: LOOPTAIL ( n -- n ) {: k:n :} 0 2 0 ?do k i + {: again:n :} again + loop ;\n: PUNCT ( n n -- n ) {: [v:n {v:n :} [v {v - ;\n;package\n5 LC-SUBJECT:AFTER . -1 LC-SUBJECT:AFTER . 5 LC-SUBJECT:INNER-V . 5 LC-SUBJECT:TAIL . -1 LC-SUBJECT:TAIL . 5 LC-SUBJECT:LOOPTAIL . 3 9 LC-SUBJECT:PUNCT .\n" ;

: SCOPE-CASE ( -- )
   s" two spellings are two names, and one spelling keeps its latest binding and its scope" T-LABEL
   SCOPE-SOURCE$ RUN-SOURCE
   S\" 9\n3\n9\n7\n-1\n105\n" EXPECT-OUT ;

: CONTROL-CASE ( -- )
   s" a closed scope, a control word and punctuation names read the same way" T-LABEL
   CONTROL-SOURCE$ RUN-SOURCE
   S\" 104\n99\n99\n10\n0\n11\n-6\n" EXPECT-OUT ;

: QUOTATION-CASE ( -- )
   s" inside a quotation the local's spelling is the local, not the word" T-LABEL
   S\" : REVIEW-WIDTH ( -- n ) 3 ;\n: OK-QLOCAL ( n -- n ) {: review-width:n :} [: review-width ;] execute ;\n17 OK-QLOCAL .\n"
   RUN-SOURCE
   S\" 17\n" EXPECT-OUT

   s" the other spelling in a quotation is the word, which a quotation may call" T-LABEL
   S\" : REVIEW-WIDTH ( -- n ) 3 ;\n: OK-QWORD ( n -- n ) {: review-width:n :} [: REVIEW-WIDTH ;] execute ;\n17 OK-QWORD .\n"
   RUN-SOURCE
   S\" 3\n" EXPECT-OUT

   s" a quotation cannot declare a local in either case" T-LABEL
   S\" : BAD-QDECLARE ( n -- n ) [: {: REVIEW-WIDTH:n :} REVIEW-WIDTH ;] execute ;\n"
   RUN-SOURCE
   70 EXPECT-RC
   OUT-U @ 0 T=
   ERR$ s" E-BAD-LOCAL-SHAPE" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SCOPE-CASE CONTROL-CASE QUOTATION-CASE
   T-REPORT ;

;package

LOCAL-CASE-TEST:RUN
