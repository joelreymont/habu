\ Cold and warm FILE replay of a callback declaration must build the same
\ runnable stripped image. The declaration lives in a required child file, so
\ its table controls precede the parent source's native range.
require src/lib/string.f
require test/gate-common.f
require test/aot-maker-engine.f

package FFI-CB-FILE-CACHE-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: BUILD ( ptr u8 n bool -- ) {: expected:ptr expectu:n source-ran:bool :}
   AOT-MAKER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   GE-HB-RESET
   maker makeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+ s" 1" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   maker makeru
   AOT-MAKER-ENGINE:STDIN$
   TIMEOUT-MS GE-RUN-STDIN
   s" callback FILE image build" GE-EXPECT-OK
   GT-OUT$ s" callback declaration source ran" CONTAINS? source-ran if 0= then if
      s" callback FILE source execution" GE-FAIL
   then
   expected expectu s" callback FILE cache counts" GE-EXPECT-OUT-HAS
   IMAGE$ EXECUTABLE? 0= if s" callback FILE image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" callback FILE image run" GE-EXPECT-OK
   S\" 0\n\n0\n\nfresh callback binding: ok\nstripped-callback: ok\n" s" callback FILE image output" GE-EXPECT-OUT
   s" " s" callback FILE image stderr" GE-EXPECT-ERR ;

: WRITE-SUBJECT ( bool -- ) {: unbound:bool :}
   SUBJECT$ S\" require src/lib/ffi-callback.f\nCALLBACK: FILE-BOUND ( -- ) ;CALLBACK\nFILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n" WRITE-ALL
   SUBJECT$ S\" s\q callback declaration source ran\q type cr\n" APPEND-FILE
   unbound if
      SUBJECT$ S\" FILE-BOUND FFI-CB:UNBIND\n" APPEND-FILE
   then
   SUBJECT$ S\" PERSISTED-PTR-VARIABLE FILE-ROWS\nvariable FILE-ROW-VISITS\n" APPEND-FILE
   SUBJECT$ S\" : KEEP-ROW ( ptr u8 n -- ) {: a:ptr u:n :} FILE-ROW-VISITS @ 0= if a 0 ptr-field @ FILE-ROWS ! then 1 FILE-ROW-VISITS +! ;\n" APPEND-FILE
   SUBJECT$ S\" : BOUND-REGION ( -- n ) 0 FILE-ROW-VISITS ! ['] KEEP-ROW TASK:OWNED-CALLBACK-ROWS FILE-ROWS @ CB-ROW-REGION + CELL-VIEW atomic@ ;\n" APPEND-FILE
   SUBJECT$ S\" : ROW-MOVES ( -- n ) FILE-ROWS @ 2 cells + CELL-VIEW atomic@ ;\n: FRESH-ROW ( -- ) BOUND-REGION dup . cr 0<> if 74 throw then ROW-MOVES dup . cr 0<> if 74 throw then ;\n" APPEND-FILE
   SUBJECT$ S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) FRESH-ROW s\q fresh callback binding: ok\q type cr STRIPPED-CALLBACK-SUBJECT:RUN ;\n" APPEND-FILE ;

: BODY ( -- )
   s" ffi-callback-file-cache" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U !
   false WRITE-SUBJECT
   S\" file unit hits 0\n compiled 3\n" true BUILD RUN-IMAGE
   S\" file unit hits 3\n compiled 0\n" false BUILD RUN-IMAGE
   \ Rewrite the whole fixture so the unbind is in the replayed parent FILE unit.
   true WRITE-SUBJECT
   S\" file unit hits 1\n compiled 2\n" true BUILD RUN-IMAGE
   S\" file unit hits 3\n compiled 0\n" false BUILD RUN-IMAGE
   s" PASS: callback FILE cold/warm image" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
