\ forth-task-lines-test.f - focused tests for harness=forth task rows.

4096 constant FTLT-READ-CAP

create FTLT-ROOT FS-PATH-CAP allot
create FTLT-OUT FS-PATH-CAP allot
create FTLT-IN FS-PATH-CAP allot
create FTLT-READ FTLT-READ-CAP allot

variable FTLT-ROOT-U
variable FTLT-OUT-U
variable FTLT-IN-U
variable FTLT-READ-U

: FTLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: FTLT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: FTLT-ROOT$ ( -- ptr u8 n )
   FTLT-ROOT FTLT-ROOT-U @ ;

: FTLT-OUT$ ( -- ptr u8 n )
   FTLT-OUT FTLT-OUT-U @ ;

: FTLT-IN$ ( -- ptr u8 n )
   FTLT-IN FTLT-IN-U @ ;

: FTLT-TASKS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-
2	STR-TRIM-OK?	(-- bool)	strings	empty -> -1	stdlib	stack	Trim.	-	v2	-	-

# comment
3	CUBE	(i64 -- i64)	arith	2 -> 8	forth	stack	Cube.	-	v1	-	-
" ;

: FTLT-EXPECTED$ ( -- ptr u8 n )
   s" 1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-
3	CUBE	(i64 -- i64)	arith	2 -> 8	forth	stack	Cube.	-	v1	-	-
" ;

: FTLT-NO-FORTH$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
2	STR-TRIM-OK?	(-- bool)	strings	empty -> -1	stdlib	stack	Trim.	-	v2	-	-
" ;

: FTLT-BAD-HEADER$ ( -- ptr u8 n )
   s" id	name
1	SQUARE
" ;

: FTLT-BAD-FIELDS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE
" ;

: FTLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-forth-task-lines" TMPDIR-MKDIR FTLT-ROOT FTLT-ROOT-U FTLT-COPY!
   FTLT-ROOT$ CLEANUP-TREE+
   FTLT-ROOT$ s" tasks.tsv" FTLT-IN FTLT-IN-U FTLT-PATH!
   FTLT-ROOT$ s" tasks.body" FTLT-OUT FTLT-OUT-U FTLT-PATH! ;

: FTLT-READ-OUT ( -- ptr u8 n )
   FTLT-OUT$ FTLT-READ FTLT-READ-CAP READ-ALL FTLT-READ-U !
   FTLT-READ FTLT-READ-U @ ;

: FTLT-EXPECT-DATA ( -- )
   FTLT-TASKS$ FTL-EMIT-DATA FTLT-EXPECTED$ T$=
   FTLT-NO-FORTH$ FTL-EMIT-DATA s" " T$= ;

: FTLT-EXPECT-FILE ( -- )
   FTLT-TASKS$ FTLT-OUT$ FTL-WRITE-DATA
   FTLT-READ-OUT FTLT-EXPECTED$ T$=
   FTLT-IN$ FTLT-TASKS$ WRITE-ALL
   FTLT-IN$ FTL-FILE$ FTLT-EXPECTED$ T$= ;

: FTLT-EXPECT-BAD-HEADER ( -- )
   FTLT-BAD-HEADER$ FTL-EMIT-DATA 2drop ;

: FTLT-EXPECT-BAD-FIELDS ( -- )
   FTLT-BAD-FIELDS$ FTL-EMIT-DATA 2drop ;

: FTLT-MAIN ( -- )
   T-RESET
   FTLT-PREPARE
   FTLT-EXPECT-DATA
   FTLT-EXPECT-FILE
   ['] FTLT-EXPECT-BAD-HEADER E-BM-SCHEMA TTHROWS
   ['] FTLT-EXPECT-BAD-FIELDS E-BM-SCHEMA TTHROWS
   CLEANUP-RUN
   FTLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" forth-task-lines-test: ok" type cr ;

FTLT-MAIN
