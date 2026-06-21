\ attempt-solutions-test.f - focused tests for harness=forth reference extraction.

4096 constant AST-READ-CAP

create AST-ROOT FS-PATH-CAP allot
create AST-OUT FS-PATH-CAP allot
create AST-READ AST-READ-CAP allot
create AST-PATH FS-PATH-CAP allot

variable AST-ROOT-U
variable AST-OUT-U

: AST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: AST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: AST-ROOT$ ( -- ptr u8 n )
   AST-ROOT AST-ROOT-U @ ;

: AST-OUT$ ( -- ptr u8 n )
   AST-OUT AST-OUT-U @ ;

: AST-TASKS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-
2	CUBE	(i64 -- i64)	arith	2 -> 8	forth	stack	Cube.	-	v1	-	-
3	STR-TRIM-OK?	(-- bool)	strings	empty -> -1	stdlib	stack	Trim.	-	v2	-	-
" ;

: AST-SOLUTIONS$ ( -- ptr u8 n )
   s" \ leading comment
: SQUARE ( i64 -- i64 ) dup * ;

: CUBE
  ( i64 -- i64 )
  dup dup * * ;
" ;

: AST-EXTRA$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup * ;
: CUBE ( i64 -- i64 ) dup dup * * ;
: EXTRA ( -- i64 ) 1 ;
" ;

: AST-MISSING$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup * ;
" ;

: AST-DUP-SOL$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup * ;
: SQUARE ( i64 -- i64 ) dup * ;
: CUBE ( i64 -- i64 ) dup dup * * ;
" ;

: AST-DUP-TASKS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-
2	SQUARE	(i64 -- i64)	arith	2 -> 4	forth	stack	Square again.	-	v1	-	-
" ;

: AST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-attempt-solutions" TMPDIR-MKDIR AST-ROOT AST-ROOT-U AST-COPY!
   AST-ROOT$ CLEANUP-TREE+
   AST-ROOT$ s" ref" AST-OUT AST-OUT-U AST-PATH! ;

: AST-REF-PATH ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu :}
   AST-OUT$ name nameu AST-PATH JOIN-PATH {: u :}
   AST-PATH u ;

: AST-READ-REF ( ptr u8 n -- ptr u8 n )
   AST-REF-PATH AST-READ AST-READ-CAP READ-ALL AST-READ swap ;

: AST-EXPECT-VALID ( -- )
   AST-TASKS$ AST-SOLUTIONS$ AST-OUT$ AS-EXTRACT-DATA
   s" 1.f" AST-READ-REF s" : SQUARE ( i64 -- i64 ) dup * ;
" T$=
   s" 2.f" AST-READ-REF s" : CUBE
  ( i64 -- i64 )
  dup dup * * ;
" T$=
   s" 3.f" AST-REF-PATH EXISTS? TFALSE ;

: AST-EXPECT-EXTRA ( -- )
   AST-TASKS$ AST-EXTRA$ AST-OUT$ AS-EXTRACT-DATA ;

: AST-EXPECT-MISSING ( -- )
   AST-TASKS$ AST-MISSING$ AST-OUT$ AS-EXTRACT-DATA ;

: AST-EXPECT-DUP-SOL ( -- )
   AST-TASKS$ AST-DUP-SOL$ AST-OUT$ AS-EXTRACT-DATA ;

: AST-EXPECT-DUP-TASK ( -- )
   AST-DUP-TASKS$ AST-SOLUTIONS$ AST-OUT$ AS-EXTRACT-DATA ;

: AST-MAIN ( -- )
   T-RESET
   AST-PREPARE
   AST-EXPECT-VALID
   ['] AST-EXPECT-EXTRA E-AS-EXTRA TTHROWS
   ['] AST-EXPECT-MISSING E-AS-MISSING TTHROWS
   ['] AST-EXPECT-DUP-SOL E-AS-DUPLICATE TTHROWS
   ['] AST-EXPECT-DUP-TASK E-AS-DUPLICATE TTHROWS
   CLEANUP-RUN
   AST-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" attempt-solutions-test: ok" type cr ;

AST-MAIN
