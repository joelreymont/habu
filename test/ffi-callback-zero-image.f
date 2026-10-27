\ A stripped image with the callback package loaded but no declarations must
\ leave TASK's row allocation absent rather than claiming a zero-byte extent.
require test/gate-common.f
require test/aot-builder-engine.f

package FFI-CB-ZERO-IMAGE-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: BUILD ( -- )
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   GE-HB-RESET
   maker makeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" TMPDIR" >LEN GT-ROOT >LEN PROC-ENV+
   maker makeru
   AOT-BUILDER-ENGINE:STDIN$
   TIMEOUT-MS GE-RUN-STDIN
   s" zero callback image build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" zero callback image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" zero callback image run" GE-EXPECT-OK
   S\" zero-callback-image: ok\n" s" zero callback image output" GE-EXPECT-OUT
   s" " s" zero callback image stderr" GE-EXPECT-ERR ;

: BODY ( -- )
   s" ffi-callback-zero-image" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U !
   SUBJECT$ S\" require src/lib/ffi-callback.f\n: MAIN ( -- ) s\" zero-callback-image: ok\" type cr ;\n" WRITE-ALL
   BUILD RUN-IMAGE
   s" PASS: zero callback stripped image" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
