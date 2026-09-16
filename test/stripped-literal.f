\ Retained library literals must survive stripped linking; mutable pre-window
\ DATA must still refuse. Exercise both through the real native build driver.
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
   s" application" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SUBJECT$ a u WRITE-ALL ;

: BUILD-LITERAL ( -- )
   \ This deliberately invalid munmap range reaches MEM's baked error string.
   \ The syscall refuses the unaligned address before touching any memory.
   S\" package STRIPPED-LITERAL-SUBJECT\nprivate\nTRUSTED: BAD-SPAN ( -- ptr u8 NUM:byte-len ) 4097 4096 ;\npublic\n: RUN ( -- ) BAD-SPAN MEM:UNMAP ;\n;package\n: MAIN ( -- ) STRIPPED-LITERAL-SUBJECT:RUN ;\n" WRITE-SUBJECT
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV
   s" stripped retained literal build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" stripped retained literal executable" GE-FAIL then ;

: RUN-LITERAL ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   71 s" stripped retained literal error path" GE-EXPECT-RC
   GT-OUT$ nip 0<> if s" stripped retained literal stdout" GE-FAIL then
   GT-ERR$ s" memory: unmap failed" STR= 0= if
      s" stripped retained literal exact stderr" GE-FAIL
   then ;

: REFUSE-MUTABLE ( -- )
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U !
   S\" : MAIN ( -- ) SLT-PREWINDOW-CELL @ . ;\n" WRITE-SUBJECT
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$
   S\" create SLT-PREWINDOW-CELL 41 ,\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   TIMEOUT-MS GE-RUN-STDIN
   74 s" stripped mutable pre-window DATA refusal" GE-EXPECT-RC
   s" aot: address refers to data outside the restored span"
      s" stripped mutable pre-window DATA diagnostic" GE-EXPECT-ERR-HAS
   IMAGE$ EXISTS? if s" stripped mutable DATA emitted an image" GE-FAIL then ;

: BODY ( -- )
   PREPARE
   BUILD-LITERAL
   RUN-LITERAL
   REFUSE-MUTABLE
   s" PASS: stripped retained literals and mutable DATA refusal" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
