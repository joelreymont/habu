\ Compiler lookup tables must refuse a stripped link. The retained-literal
\ image is tested by test/stripped-image.f; mutable pre-window DATA is tested
\ by test/compiler/aot-data-cell-refusals.f.
require test/gate-common.f
require lib/engine-candidate.f

package STRIPPED-LITERAL-TEST

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" stripped-literal" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u WRITE-ALL ;

: REFUSE-COMPILER-ROWS ( -- )
   S\" PERSISTED-PTR-VARIABLE SLT-COMPILER-ROWS\n: MAIN ( -- ) SLT-COMPILER-ROWS @ @ . ;\nNSTR:SOURCE-ROWS SLT-COMPILER-ROWS ! drop 2drop\n" WRITE-SUBJECT
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   TIMEOUT-MS GE-RUN-STDIN
   70 s" stripped compiler literal rows refusal" GE-EXPECT-RC
   s" declared data cell holds an engine address outside the restored span"
      s" stripped compiler literal rows stay outside the window" GE-EXPECT-ERR-HAS
   s" word=SLT-COMPILER-ROWS"
      s" stripped compiler literal rows name the retaining cell" GE-EXPECT-ERR-HAS
   IMAGE$ EXISTS? if s" stripped compiler rows emitted an image" GE-FAIL then ;

: BODY ( -- )
   PREPARE
   REFUSE-COMPILER-ROWS
   s" PASS: stripped compiler rows refusal" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
