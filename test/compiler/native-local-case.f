\ A local answers to its DECLARED SPELLING, and the checker and both compilers
\ read every name here the same way. A reference binds the local only when it
\ matches the declaration byte for byte; word lookup stays case-insensitive, so
\ the other spelling of those letters is the WORD - a global, a package public, a
\ builtin, the loop index or a control word - and a spelling that names neither
\ is undefined. Each subject runs at tier 0 and tier 1, because the three places
\ that read a local name (src/core/checker.f LOC-REF?, src/habu/habu2.f
\ EMIT-LOC-FIND, src/compiler/native/elaborate.f LOCAL-OF) must not disagree:
\ two of them disagreeing is a body the checker certifies and the compiler
\ compiles differently, which is the one failure no diagnostic would name.
\ Tier-neutral by design: each subject is compiled in a child this file runs at
\ tier 0 and at tier 1, so the tier of this row selects nothing.
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package LOCAL-CASE-TEST

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

: EXPECT-OUT ( ptr u8 n -- )
   0 EXPECT-RC
   ERR-U @ 0 T=
   OUT$ STR= TTRUE ;

: SHADOW-SOURCE$ ( -- ptr u8 n )
   S\" : REVIEW-WIDTH ( -- n ) 3 ;\n: MIXED-GLOBAL ( n -- n ) {: review-width:n :} REVIEW-WIDTH ;\n: SAME-GLOBAL ( n -- n ) {: review-width:n :} review-width ;\npackage LC-SUBJECT\npublic\n: P-WIDTH ( -- n ) 4 ;\n: SAME ( n -- n ) {: P-WIDTH:n :} P-WIDTH ;\n: MIXED-PKG ( n -- n ) {: p-width:n :} P-WIDTH ;\n: MIXED-UP ( n -- n ) {: P-WIDTH:n :} p-width ;\n: BUILTIN ( n -- n ) {: DUP:n :} DUP ;\n: INDEX ( n -- n ) {: I:n :} 0 3 0 ?do i + loop ;\n: TURNS ( n -- n ) {: i:n :} 0 3 0 ?do i + loop ;\n;package\n17 MIXED-GLOBAL . 17 SAME-GLOBAL . 9 LC-SUBJECT:SAME . 9 LC-SUBJECT:MIXED-PKG . 9 LC-SUBJECT:MIXED-UP . 7 LC-SUBJECT:BUILTIN . 5 LC-SUBJECT:INDEX . 5 LC-SUBJECT:TURNS .\n" ;

: SCOPE-SOURCE$ ( -- ptr u8 n )
   S\" package LC-SUBJECT\npublic\n: LATEST ( n n -- n ) {: item:n ITEM:n :} ITEM ;\n: EARLIER ( n n -- n ) {: item:n ITEM:n :} item ;\n: REPEATED ( n n -- n ) {: v:n v:n :} v ;\n: SHADOW ( n -- n ) {: value:n :} value 0 > if false {: value:bool :} value if 1 else 2 then else 0 then value + ;\n: DISTINCT ( n -- n ) {: value:n :} value 0 > if 100 {: VALUE:n :} VALUE value + else 0 then ;\n;package\n3 9 LC-SUBJECT:LATEST . 3 9 LC-SUBJECT:EARLIER . 3 9 LC-SUBJECT:REPEATED . 5 LC-SUBJECT:SHADOW . -1 LC-SUBJECT:SHADOW . 5 LC-SUBJECT:DISTINCT .\n" ;

\ A name whose scope has closed, a name spelled like a control word, and names
\ that are punctuation: the second half of the scope question, held apart
\ because one subject source has to fit the 1024-byte builder.
: CONTROL-SOURCE$ ( -- ptr u8 n )
   S\" package LC-SUBJECT\npublic\n99 constant V\n: AFTER ( n -- n ) dup 0 > if {: v:n :} v else drop 0 then V + ;\n: INNER-V ( n -- n ) dup 0 > if {: v:n :} V else drop 0 then ;\n: TAIL ( n -- n ) dup 0 > if {: endof:n :} endof 2 * else drop 0 then ;\n: LOOPTAIL ( n -- n ) {: k:n :} 0 2 0 ?do k i + {: again:n :} again + loop ;\n: PUNCT ( n n -- n ) {: [v:n {v:n :} [v {v - ;\n;package\n5 LC-SUBJECT:AFTER . -1 LC-SUBJECT:AFTER . 5 LC-SUBJECT:INNER-V . 5 LC-SUBJECT:TAIL . -1 LC-SUBJECT:TAIL . 5 LC-SUBJECT:LOOPTAIL . 3 9 LC-SUBJECT:PUNCT .\n" ;

: SHADOW-CASE ( n -- )
   s" a local binds its declared spelling; the other spelling is the word" T-LABEL
   SHADOW-SOURCE$ rot RUN-SOURCE
   S\" 3\n17\n9\n4\n4\n7\n3\n15\n" EXPECT-OUT ;

: SCOPE-CASE ( n -- )
   s" two spellings are two names, and one spelling keeps its latest binding and its scope" T-LABEL
   SCOPE-SOURCE$ rot RUN-SOURCE
   S\" 9\n3\n9\n7\n-1\n105\n" EXPECT-OUT ;

: CONTROL-CASE ( n -- )
   s" a closed scope, a control word and punctuation names read the same way" T-LABEL
   CONTROL-SOURCE$ rot RUN-SOURCE
   S\" 104\n99\n99\n10\n0\n11\n-6\n" EXPECT-OUT ;

: QUOTATION-CASE ( n -- ) {: tier:n :}
   s" a local reference cannot escape into a quotation" T-LABEL
   S\" : REVIEW-WIDTH ( -- n ) 3 ;\n: BAD-QLOCAL ( n -- n ) {: review-width:n :} [: review-width ;] execute ;\n"
   tier RUN-SOURCE
   tier 0= if 75 else 70 then EXPECT-RC
   OUT-U @ 0 T=
   tier 0<> if ERR$ s" E-BAD-LOCAL-SHAPE" CONTAINS? TTRUE then

   s" the other spelling in a quotation is the word, which a quotation may call" T-LABEL
   S\" : REVIEW-WIDTH ( -- n ) 3 ;\n: OK-QWORD ( n -- n ) {: review-width:n :} [: REVIEW-WIDTH ;] execute ;\n17 OK-QWORD .\n"
   tier RUN-SOURCE
   S\" 3\n" EXPECT-OUT

   s" a quotation cannot declare a local in either case" T-LABEL
   S\" : BAD-QDECLARE ( n -- n ) [: {: REVIEW-WIDTH:n :} REVIEW-WIDTH ;] execute ;\n"
   tier RUN-SOURCE
   tier 0= if 75 else 70 then EXPECT-RC
   OUT-U @ 0 T=
   tier 0<> if ERR$ s" E-BAD-LOCAL-SHAPE" CONTAINS? TTRUE then ;

public

: RUN ( -- )
   T-RESET
   2 0 do i SHADOW-CASE i SCOPE-CASE i CONTROL-CASE i QUOTATION-CASE loop
   T-REPORT ;

;package

LOCAL-CASE-TEST:RUN
