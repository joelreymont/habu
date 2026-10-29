\ Cold and warm definition reuse must rerun callback initialization and build
\ the same runnable stripped image with fresh runtime callback bindings.
require src/lib/string.f
require test/gate-common.f
require test/aot-builder-engine.f

package FFI-CB-FILE-CACHE-TEST

600000 constant TIMEOUT-MS
create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: BUILD ( bool -- ) {: hit:bool :}
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   GE-HB-RESET
   maker makeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+ s" 1" GE-ARG+
   s" TMPDIR" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_OUT" >LEN GT-ROOT >LEN PROC-ENV+
   maker makeru
   AOT-BUILDER-ENGINE:STDIN$
   TIMEOUT-MS GE-RUN-STDIN
   s" callback FILE image build" GE-EXPECT-OK
   GT-OUT$ s" callback declaration source ran" CONTAINS? 0= if
      s" callback initializer execution" GE-FAIL
   then
   s" files reused " s" file reuse counts" GE-EXPECT-OUT-HAS
   hit if
      GT-OUT$ s" files reused 0" CONTAINS? if
         s" callback file reuse" GE-FAIL
      then
   else
      GT-OUT$ s" files reused 0" CONTAINS? 0= if
         s" cold callback file build" GE-FAIL
      then
   then
   IMAGE$ FS:EXECUTABLE? 0= if s" callback FILE image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" callback FILE image run" GE-EXPECT-OK
   S\" 0\n\n0\n\nfresh callback binding: ok\nstripped-callback: ok\n" s" callback FILE image output" GE-EXPECT-OUT
   s" " s" callback FILE image stderr" GE-EXPECT-ERR ;

: WRITE-SUBJECT ( bool -- ) {: unbound:bool :}
   SUBJECT$ S\" require src/lib/ffi-callback.f\nCALLBACK: FILE-BOUND ( -- ) ;CALLBACK\n" FS:WRITE-ALL
   \ Numeric fallbacks force descriptor growth and retirement before capture.
   \ MAIN also reaches the public seed-entry primitive: its code span must be
   \ disjoint from the shared thunk's, with FILE-BOUND still the first row.
   SUBJECT$ S\" CALLBACK: FIRST ( -- n ) 456 FALLBACK ;CALLBACK\nCALLBACK: SECOND ( -- n ) 456 FALLBACK ;CALLBACK\n" FS:APPEND-FILE
   SUBJECT$ S\" FILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n" FS:APPEND-FILE
   SUBJECT$ S\" s\q callback declaration source ran\q type cr\n" FS:APPEND-FILE
   unbound if
      SUBJECT$ S\" FILE-BOUND FFI-CB:UNBIND\n" FS:APPEND-FILE
   then
   SUBJECT$ S\" PERSISTED-PTR-VARIABLE FILE-ROWS\nvariable FILE-ROW-VISITS\n" FS:APPEND-FILE
   SUBJECT$ S\" : KEEP-ROW ( ptr u8 n -- ) {: a:ptr u:n :} FILE-ROW-VISITS @ 0= if a 0 ptr-field @ FILE-ROWS ! then 1 FILE-ROW-VISITS +! ;\n" FS:APPEND-FILE
   SUBJECT$ S\" : BOUND-REGION ( -- n ) 0 FILE-ROW-VISITS ! ['] KEEP-ROW TASK:OWNED-CALLBACK-ROWS FILE-ROWS @ CB-ROW-REGION + CELL-VIEW atomic@ ;\n" FS:APPEND-FILE
   SUBJECT$ S\" : ROW-MOVES ( -- n ) FILE-ROWS @ 2 cells + CELL-VIEW atomic@ ;\n: FRESH-ROW ( -- ) BOUND-REGION dup . cr 0<> if 74 throw then ROW-MOVES dup . cr 0<> if 74 throw then ;\n" FS:APPEND-FILE
   SUBJECT$ S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) 0 FFI:CALLBACK-ENTRY drop FRESH-ROW s\q fresh callback binding: ok\q type cr STRIPPED-CALLBACK-SUBJECT:RUN ;\n" FS:APPEND-FILE ;

: BODY ( -- )
   s" ffi-callback-file-cache" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U !
   false WRITE-SUBJECT
   false BUILD RUN-IMAGE
   true BUILD RUN-IMAGE
   \ Changing a source action still reruns initialization around cached bodies.
   true WRITE-SUBJECT
   true BUILD RUN-IMAGE
   s" PASS: callback definition cold/warm image" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
