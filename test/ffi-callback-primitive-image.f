\ A stripped callback program can also name the public seed-entry primitive.
\ The shared thunk and that primitive must have disjoint owned code spans.
\ Two numeric fallbacks force descriptor growth before the AOT DATA scan.
require test/gate-common.f
require test/aot-maker-engine.f

package FFI-CB-PRIMITIVE-IMAGE-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: BUILD ( -- )
   AOT-MAKER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   GE-HB-RESET
   maker makeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   maker makeru
   AOT-MAKER-ENGINE:STDIN$
   TIMEOUT-MS GE-RUN-STDIN
   s" callback primitive image build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" callback primitive image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" callback primitive image run" GE-EXPECT-OK
   S\" callback-primitive-image: ok\n" s" callback primitive image output" GE-EXPECT-OUT
   s" " s" callback primitive image stderr" GE-EXPECT-ERR ;

: BODY ( -- )
   s" ffi-callback-primitive-image" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U !
   SUBJECT$ S\" require src/lib/ffi-callback.f\nCALLBACK: CB ( -- ) ;CALLBACK\nCALLBACK: FIRST ( -- n ) 456 FALLBACK ;CALLBACK\nCALLBACK: SECOND ( -- n ) 456 FALLBACK ;CALLBACK\n: MAIN ( -- ) 0 FFI:CALLBACK-ENTRY drop s\" callback-primitive-image: ok\" type cr ;\n" WRITE-ALL
   BUILD RUN-IMAGE
   s" PASS: callback primitive image" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
