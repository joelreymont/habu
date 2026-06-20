\ manifest.f - checked TSV scanners for LLM benchmark manifests.
\
\ Load after lib/errors.f and lib/string.f.

9 constant BM-TAB
10 constant BM-LF
35 constant BM-HASH

12 constant BM-TASK-FIELDS
7 constant BM-MODEL-FIELDS
-3200 constant E-BM-SCHEMA
-3201 constant E-BM-FIELD

0 constant BM-T-ID
1 constant BM-T-NAME
2 constant BM-T-SIGNATURE
3 constant BM-T-CATEGORY
4 constant BM-T-TESTS
5 constant BM-T-HARNESS
6 constant BM-T-CONV
7 constant BM-T-SPEC
8 constant BM-T-VECTORS
9 constant BM-T-TAGS
10 constant BM-T-JS-SIGNATURE
11 constant BM-T-RUST-SIGNATURE

0 constant BM-M-ID
1 constant BM-M-LABEL
2 constant BM-M-COMMAND
3 constant BM-M-ARGS
4 constant BM-M-PARSER
5 constant BM-M-TOKEN-FIELDS
6 constant BM-M-TIMEOUT

variable BM-I
variable BM-CUR
variable BM-START

: BM-TRUE ( -- bool )
   0 0= ;

: BM-FALSE ( -- bool )
   BM-TRUE 0= ;

: BM-FIELD-COUNT ( ptr u8 n -- n ) {: a:ptr u :}
   u 0= if 0 exit then
   1 0 begin dup u < while
      dup a + c@ BM-TAB = if swap 1+ swap then
      1+
   repeat drop ;

: BM-CHECK-FIELD-IDX ( ptr u8 n n -- ) {: a:ptr u idx :}
   idx 0 < if E-BM-FIELD throw then
   idx a u BM-FIELD-COUNT >= if E-BM-FIELD throw then ;

: BM-FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u idx :}
   a u idx BM-CHECK-FIELD-IDX
   0 BM-I ! 0 BM-CUR ! 0 BM-START !
   begin BM-I @ u < while
      a BM-I @ + c@ BM-TAB = if
         BM-CUR @ idx = if a BM-START @ + BM-I @ BM-START @ - exit then
         BM-CUR @ 1+ BM-CUR !
         BM-I @ 1+ BM-START !
      then
      BM-I @ 1+ BM-I !
   repeat
   a BM-START @ + u BM-START @ - ;

: BM-REQUIRE-FIELDS ( ptr u8 n n -- ) {: a:ptr u n :}
   a u BM-FIELD-COUNT n <> if E-BM-SCHEMA throw then ;

: BM-TASK-FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u idx :}
   a u BM-TASK-FIELDS BM-REQUIRE-FIELDS
   a u idx BM-FIELD$ ;

: BM-MODEL-FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u idx :}
   a u BM-MODEL-FIELDS BM-REQUIRE-FIELDS
   a u idx BM-FIELD$ ;

: BM-TASK-HEADER$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature" ;

: BM-MODEL-HEADER$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s" ;

: BM-TASK-HEADER? ( ptr u8 n -- bool )
   BM-TASK-HEADER$ STR= ;

: BM-MODEL-HEADER? ( ptr u8 n -- bool )
   BM-MODEL-HEADER$ STR= ;

: BM-REQUIRE-TASK-HEADER ( ptr u8 n -- )
   BM-TASK-HEADER? 0= if E-BM-SCHEMA throw then ;

: BM-REQUIRE-MODEL-HEADER ( ptr u8 n -- )
   BM-MODEL-HEADER? 0= if E-BM-SCHEMA throw then ;

: BM-LINE-NEXT ( ptr u8 n n -- ptr u8 n n bool ) {: a:ptr u start :}
   start u >= if a 0 start BM-FALSE exit then
   start begin dup u < while
      dup a + c@ BM-LF = if
         a start + over start - rot 1+ BM-TRUE exit
      then
      1+
   repeat drop
   a start + u start - u BM-TRUE ;

: BM-BLANK-OR-COMMENT? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u TRIM {: b:ptr v :}
   v 0= if BM-TRUE exit then
   b c@ BM-HASH = ;

: BM-SIGNATURE-INNER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u TRIM {: b:ptr v :}
   v 1 > if
      b c@ 40 = b v 1- + c@ 41 = and if
         b 1+ v 2 - TRIM exit
      then
   then
   b v ;

: BM-TASK-SIG$ ( ptr u8 n -- ptr u8 n )
   BM-T-SIGNATURE BM-TASK-FIELD$ BM-SIGNATURE-INNER$ ;

: BM-LIST-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u item:ptr itemu :}
   u 0= if BM-TRUE exit then
   0 BM-I !
   begin
      a u 44 BM-I @ SPLIT-NEXT
   while
      BM-I !
      TRIM item itemu STR= if BM-TRUE exit then
   repeat
   drop 2drop BM-FALSE ;

: BM-TASK-SELECTED? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u ids:ptr idsu :}
   ids idsu a u BM-T-ID BM-TASK-FIELD$ BM-LIST-CONTAINS? ;
