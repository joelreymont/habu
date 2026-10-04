\ The first segment's compiler lookup tables must refuse a stripped link, and
\ the engine literals an application reaches must link however full its literal
\ pool is. The retained-literal image is tested by test/stripped-image.f;
\ mutable pre-window DATA is tested by test/compiler/aot-data-cell-refusals.f.
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

: LINK-SUBJECT ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   TIMEOUT-MS GE-RUN-STDIN ;

: REFUSE-COMPILER-ROWS ( -- )
   S\" PERSISTED-PTR-VARIABLE SLT-COMPILER-ROWS\n: MAIN ( -- ) SLT-COMPILER-ROWS @ @ . ;\n0 NSTR:SOURCE-ROWS SLT-COMPILER-ROWS ! drop 2drop\n" WRITE-SUBJECT
   LINK-SUBJECT
   70 s" stripped compiler literal rows refusal" GE-EXPECT-RC
   s" declared data cell holds an engine address outside the restored span"
      s" stripped compiler literal rows stay outside the window" GE-EXPECT-ERR-HAS
   s" word=SLT-COMPILER-ROWS"
      s" stripped compiler literal rows name the retaining cell" GE-EXPECT-ERR-HAS
   IMAGE$ EXISTS? if s" stripped compiler rows emitted an image" GE-FAIL then ;

\ The application's own bodies fill its pool's segment to the last of its 512 KB
\ ($80000), so the link's copy of the engine literal RELEASE-RANGE dies with
\ (lib/memory.f) takes room the window has to have kept for it.
: WRITE-FULL-SUBJECT ( -- )
   SB-RESET
   S\" require lib/memory.f\npackage SLT-FULL\nprivate\n$2000 BUFFER: SRC\nvariable SRC-U\n" SB-APPEND
   S\" TRUSTED: BAD-SPAN ( -- ptr u8 NUM:byte-len ) 4097 4096 ;\nTRUSTED: EV ( ptr u8 n -- ) evaluate ;\n" SB-APPEND
   S\" : C+ ( n -- ) SRC SRC-U @ + c!  1 SRC-U +! ;\n" SB-APPEND
   S\" : S+ ( ptr u8 n -- ) {: a:ptr u:n :} u 0 ?do a i + c@ C+ loop ;\n" SB-APPEND
   S\" : DEF ( n n -- ) {: k:n size:n :}\n   0 SRC-U !  s\" : F\" S+  97 k 26 / + C+  97 k 26 mod + C+\n" SB-APPEND
   S\"    s\"  ( -- ptr u8 n ) s\" S+  34 C+  32 C+\n" SB-APPEND
   S\"    k 0 ?do 121 C+ loop  size k - 0 ?do 122 C+ loop\n" SB-APPEND
   S\"    34 C+  s\"  ;\" S+  SRC SRC-U @ EV ;\n" SB-APPEND
   S\" : FILL ( -- ) 74 0 ?do i 7000 DEF loop  74 $80000 NSTR:BYTES - DEF ;\n" SB-APPEND
   S\" public\n: RUN ( -- ) BAD-SPAN MEM:UNMAP ;\nFILL\n;package\n: MAIN ( -- ) SLT-FULL:RUN ;\n" SB-APPEND
   SB$ WRITE-SUBJECT ;

: LINK-INTO-FULL-POOL ( -- )
   WRITE-FULL-SUBJECT
   LINK-SUBJECT
   s" a link into a full literal segment" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" a link into a full literal segment emitted no image" GE-FAIL then
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   71 s" the full-pool image exits through MEM:UNMAP" GE-EXPECT-RC
   S\" memory: unmap failed\n" s" the full-pool image prints the engine literal it reached" GE-EXPECT-ERR ;

: BODY ( -- )
   PREPARE
   REFUSE-COMPILER-ROWS
   s" PASS: stripped compiler rows refusal" type cr
   LINK-INTO-FULL-POOL
   s" PASS: stripped link into a full literal segment" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
