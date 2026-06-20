\ manifest-audit-test.f - focused tests for bench/llm/manifest-audit.f.

: BMA-FIXTURE$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
56	CALL-TWICE	(-- n)	quotation	empty -> 1	forth	stack	spec	-	v2	-	-
62	DATE-PARSE-OK?	(-- bool)	date	empty -> -1	stdlib	stack	spec	-	date,time,stdlib,parse-ymd	-	-
122	DIAG-TRUST-BOUNDARY	(-- bool)	diagnostic-repair	empty -> -1	forth	stack	spec	-	v2,trusted_boundary_required,trust	-	-

# comment
" ;

: BMA-BAD-HEADER$ ( -- ptr u8 n )
   s" id	name
56	CALL-TWICE
" ;

: BMA-BAD-FIELDS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
56	CALL-TWICE
" ;

: BMA-EXPECT-BAD-HEADER ( -- )
   BMA-BAD-HEADER$ BMA-DATA!
   BMA-CHECK-SHAPE ;

: BMA-EXPECT-BAD-FIELDS ( -- )
   BMA-BAD-FIELDS$ BMA-DATA!
   BMA-CHECK-SHAPE ;

: BMA-EXPECT-MISSING-ROW ( -- )
   BMA-FIXTURE$ BMA-DATA!
   s" 99" s" NOPE" s" date" s" stdlib" s" stack" s" v2" BMA-REQ ;

: BMA-EXPECT-MISSING-TAG ( -- )
   BMA-FIXTURE$ BMA-DATA!
   s" 62" s" DATE-PARSE-OK?" s" date" s" stdlib" s" stack" s" v2,missing" BMA-REQ ;

: BMA-AUDIT-TEST-MAIN ( -- )
   T-RESET
   BMA-FIXTURE$ BMA-DATA!
   BMA-CHECK-SHAPE
   s" 56" s" CALL-TWICE" s" quotation" s" forth" s" stack" s" v2" BMA-REQ
   s" 62" s" DATE-PARSE-OK?" s" date" s" stdlib" s" stack" s" parse-ymd" BMA-REQ
   s" 122" s" DIAG-TRUST-BOUNDARY" s" diagnostic-repair" s" forth" s" stack" s" v2,trust" BMA-REQ
   ['] BMA-EXPECT-BAD-HEADER E-BM-SCHEMA TTHROWS
   ['] BMA-EXPECT-BAD-FIELDS E-BM-SCHEMA TTHROWS
   ['] BMA-EXPECT-MISSING-ROW E-BM-SCHEMA TTHROWS
   ['] BMA-EXPECT-MISSING-TAG E-BM-SCHEMA TTHROWS
   s" bench/llm/tasks.tsv" BMA-LOAD
   BMA-CHECK-SHAPE
   BMA-REQUIRE-EXPANDED-TASKS
   T-REPORT
   s" manifest-audit-test: ok" type cr ;

BMA-AUDIT-TEST-MAIN
