\ AOT builds follow the actual load, including environment queries made by a
\ compiled source loader. The linked image also runs GETENV. Maker selection
\ keeps an enclosing input recording and loader pair intact.
\ Run: bin/hb --load tools/hb-build-aot-inputs-test.f

require tools/hb-build-test-lib.f
require test/linker-builder-engine.f

using BUILD-FIXPOINT                     \ the engine the nested child selects

package HB-BUILD-CLI

create HBT-ENV-DEP FS-PATH-CAP allot
variable HBT-ENV-DEP-U

create HBT-ENV-ROOT-A FS-PATH-CAP allot
create HBT-ENV-ROOT-B FS-PATH-CAP allot
variable HBT-ENV-ROOT-A-U
variable HBT-ENV-ROOT-B-U

: HBT-ENV-BUILD ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   HBT-ARGV-BASE
   s" SHARED_ROOT" >LEN root rootu >LEN PROC-ENV-SET
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0<> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T= erru 0 T= ;

: HBT-ENV-RUN ( n -- )
   {: value:n :}
   PROC-ARGV-ENV-RESET
   s" AOT_RUNTIME" >LEN s" runtime" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
   HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outu:n erru:n rc:n :}
   rc 0 T= erru 0 T=
   SB-RESET value FS-MUT-SB-U HBB-LF SB-APPEND-C s" runtime" SB-APPEND
   HBT-RUN-OUT outu SB$ T$= ;

: HBT-ENV-SELECTED-INPUT ( -- )
   s" AOT builds follow an observed environment source root" T-LABEL
   HBT-ROOT s" env-a" HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U HBT-PATH!
   HBT-ROOT s" env-b" HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U HBT-PATH!
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ MAKE-DIR
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ MAKE-DIR
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ s" value.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package ENV-VALUE public : VALUE ( -- n ) 42 ; ;package" WRITE-ALL
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ s" value.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package ENV-VALUE public : VALUE ( -- n ) 99 ; ;package" WRITE-ALL
   HBT-AOT-SRC S\" require src/lib/fs.f\ncreate LOAD-PATH FS-PATH-CAP allot\n: LOAD-SHARED ( -- ) s\" SHARED_ROOT\" GETENV s\" value.f\" LOAD-PATH JOIN-PATH LOAD-PATH swap required ;\nLOAD-SHARED\n: MAIN ( -- ) ENV-VALUE:VALUE . s\" AOT_RUNTIME\" GETENV type ;\n" WRITE-ALL
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   42 HBT-ENV-RUN
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   42 HBT-ENV-RUN
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ s" value.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package ENV-VALUE public : VALUE ( -- n ) 43 ; ;package" WRITE-ALL
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   43 HBT-ENV-RUN
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ HBT-ENV-BUILD
   99 HBT-ENV-RUN
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ HBT-ENV-BUILD
   99 HBT-ENV-RUN
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

: HBT-ENV-SAVE ( ptr u8 n -- )
   HBT-ROOT 2swap HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-AOT-OUT HBT-ENV-DEP HBT-ENV-DEP-U @ COPY-FILE-STREAM
   HBT-ENV-DEP HBT-ENV-DEP-U @ CHMOD-X
   HBT-AOT-OUT HBT-ENV-DEP HBT-ENV-DEP-U @ HBB-MAP-COPY ;


\ A nested entry scopes its imports from its own canonical source path. The
\ source run and the native maker must load the same leaf from outside the
\ fixture tree; each build exercises cache selection at that process boundary.
: HBT-SCOPED-SOURCE ( -- )
   HBT-ROOT s" run.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" MAIN" WRITE-ALL
   PROC-ARGV-ENV-RESET
   s" AOT_RUNTIME" >LEN s" runtime" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   HBT-ENV-DEP HBT-ENV-DEP-U @ >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0<> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T= erru 0 T=
   HBT-OUT outu S\" 42\nruntime" T$= ;


: HBT-SCOPED-REFUSED ( n ptr u8 n -- )
   {: want:n diagnostic:ptr size:n :}
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc want <> HBT-ERR erru diagnostic size CONTAINS? 0= or if
      HBT-OUT outu type HBT-ERR erru type
   then
   rc want T=
   HBT-ERR erru diagnostic size CONTAINS? TTRUE
   43 HBT-ENV-RUN ;


: HBT-SCOPED-INPUT ( -- )
   s" AOT builds follow scoped imports from a nested entry" T-LABEL
   HBT-ROOT s" src/server" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ MAKE-DIRS
   HBT-ROOT s" src/leaf.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package SCOPED-LEAF public : VALUE ( -- n ) 42 ; ;package" WRITE-ALL
   HBT-ROOT s" src/server/main.f" HBT-AOT-SRC-BUF HBT-AOT-SRC-U HBT-PATH!
   HBT-AOT-SRC S\" package SCOPED-ENTRY\npublic\n: LOAD ( -- )\n   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop\n   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME\n   [: s\" ../src/leaf.f\" required ;] SOURCE-ROOT:WITH ;\n\n\n: UNUSED ( -- ) s\" absent.f\" required ;\n;package\nSCOPED-ENTRY:LOAD\n: MAIN ( -- ) SCOPED-LEAF:VALUE . s\" AOT_RUNTIME\" GETENV type ;\n" WRITE-ALL
   HBT-SCOPED-SOURCE
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   42 HBT-ENV-RUN s" scoped-cold" HBT-ENV-SAVE
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   42 HBT-ENV-RUN
   HBT-ROOT s" src/leaf.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package SCOPED-LEAF public : VALUE ( -- n ) 43 ; ;package" WRITE-ALL
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   43 HBT-ENV-RUN s" scoped-edited" HBT-ENV-SAVE
   HBT-ROOT s" src/leaf.f" HBT-ENV-DEP HBT-ENV-DEP-U HBT-PATH!
   HBT-ENV-DEP HBT-ENV-DEP-U @ REMOVE-FILE
   74 s" include: cannot open " HBT-SCOPED-REFUSED
   HBT-ENV-DEP HBT-ENV-DEP-U @ s" package SCOPED-LEAF public : VALUE ( -- n ) drop ; ;package" WRITE-ALL
   70 s" input underflow" HBT-SCOPED-REFUSED
   HBT-ROOT s" aot.f" HBT-AOT-SRC-BUF HBT-AOT-SRC-U HBT-PATH! ;


\ The load and four tasks each make 1000 observed queries, a manifest far larger
\ than a text fragment, which the cache key must accept.
: HBT-ENV-WORKERS ( -- )
   s" load-time tasks collect complete environment inputs" T-LABEL
   HBT-AOT-SRC S\" require src/lib/task.f\npackage INPUT-WORKERS\nTASK:MIN-STACK TASK:TASK A\nTASK:MIN-STACK TASK:TASK B\nTASK:MIN-STACK TASK:TASK C\nTASK:MIN-STACK TASK:TASK D\nhere data-base - negate 7 and allot\nvariable READY\nvariable START\nvariable ANSWER\n: WORK ( -- )\n   1 READY atomic-add drop\n   begin START atomic@ 0= while TASK:PAUSE repeat\n   1000 0 ?do s\" SHARED_ROOT\" GETENV 2drop loop\n   s\" SHARED_ROOT\" GETENV 1- + c@ ANSWER atomic! 0 TASK:RETURN ;\n: JOIN ( ptr n -- )\n   TASK:JOIN MATCH result ok OF drop ENDOF err OF throw ENDOF ;MATCH ;\n: LOAD ( -- )\n   ['] WORK A TASK:ACTIVATE ['] WORK B TASK:ACTIVATE\n   ['] WORK C TASK:ACTIVATE ['] WORK D TASK:ACTIVATE\n   begin READY atomic@ 4 < while TASK:PAUSE repeat\n   1 START atomic!\n   1000 0 ?do s\" SHARED_ROOT\" GETENV 2drop loop\n   A JOIN B JOIN C JOIN D JOIN ;\nLOAD\npublic\n: VALUE ( -- n ) ANSWER atomic@ ;\n;package\n: MAIN ( -- ) INPUT-WORKERS:VALUE . s\" AOT_RUNTIME\" GETENV type ;\n" WRITE-ALL
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   97 HBT-ENV-RUN s" workers-a-cold" HBT-ENV-SAVE
   HBT-ENV-ROOT-A HBT-ENV-ROOT-A-U @ HBT-ENV-BUILD
   97 HBT-ENV-RUN s" workers-a-warm" HBT-ENV-SAVE
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ HBT-ENV-BUILD
   98 HBT-ENV-RUN s" workers-b-cold" HBT-ENV-SAVE
   HBT-ENV-ROOT-B HBT-ENV-ROOT-B-U @ HBT-ENV-BUILD
   98 HBT-ENV-RUN s" workers-b-warm" HBT-ENV-SAVE ;

\ Maker selection inside an enclosing input recording, counted through a
\ loader pair that tallies each canon and read.
create HBT-OUTER-ROWS FS-PATH-CAP 2 * allot
variable HBT-OUTER-CANON-N
variable HBT-OUTER-READ-N
create HBT-NESTED-OUT FS-PATH-CAP allot
variable HBT-NESTED-OUT-U

: HBT-OUTER-CANON ( ptr u8 n -- ptr u8 n bool )
   1 HBT-OUTER-CANON-N +! SOURCE-ROOT:CANON-OS ;
: HBT-OUTER-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   1 HBT-OUTER-READ-N +! SOURCE-ROOT:READ-OS ;

: HBT-MAKER-OUTER-HOLDS ( n n -- )
   {: end:n active:n :}
   NFILE-INPUTS:MARK end T=
   NFILE-INPUTS:BYTES$ nip active T=
   0 end NFILE-INPUTS:SPAN HBT-OUTER-ROWS end T$=
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK
   0 HBT-OUTER-CANON-N ! 0 HBT-OUTER-READ-N !
   HBT-AOT-SRC SOURCE-INPUT:CANON 2drop drop
   HBT-AOT-SRC s" " SOURCE-INPUT:READ 2drop
   HBT-OUTER-CANON-N @ 1 T=
   HBT-OUTER-READ-N @ 1 T= ;

\ Model the input range and loader pair owned by an enclosing
\ NDEF-CACHE:LOAD-WITH-INPUTS. Maker selection returns both even when
\ publishing the selector fails after discovery. The steps delete and damage
\ the maker-support record, <out root>/<engine>/img/aot-builder.inputs
\ (HBB-MAKER-SUPPORT-PATH$), which every build on that engine under the out
\ root shares, so they run in HBT-MAKER-NESTED-INPUTS's child.
: HBT-MAKER-NESTED-STEPS ( -- )
   HBB-RESET-OPTIONS
   HBB-MAKER-KEY!
   HBB-MAKER-SUPPORT-PATH$ REMOVE-FILE
   NFILE-INPUTS:RESET
   HBT-AOT-SRC s" " NFILE-INPUTS:ADD-CONTENT
   NFILE-INPUTS:MARK NFILE-INPUTS:FROM
   HBT-AOT-SRC s" " NFILE-INPUTS:ADD-CONTENT
   NFILE-INPUTS:MARK {: end:n :}
   NFILE-INPUTS:BYTES$ nip {: active:n :}
   0 end NFILE-INPUTS:SPAN drop HBT-OUTER-ROWS end BYTE-COPY
   [: HBT-OUTER-CANON ;] [: HBT-OUTER-READ ;] SOURCE-INPUT:USE
   HBB-MAKER-KEY!
   end active HBT-MAKER-OUTER-HOLDS
   HBB-MAKER-SUPPORT-PATH$ REMOVE-FILE
   HBB-MAKER-SUPPORT-PATH$ MAKE-DIR
   [: HBB-MAKER-KEY! ;] E-FS-IO TTHROWSQ
   end active HBT-MAKER-OUTER-HOLDS
   HBB-MAKER-SUPPORT-PATH$ REMOVE-TREE
   SOURCE-INPUT:RESET
   NFILE-INPUTS:CLOSE ;


\ The child's argv after `nested`: the source and the engine. Its environment
\ names the temp, cache and out roots.
: HBT-MAKER-NESTED-CHILD ( -- )
   SCRIPT-ARGC 3 <> if E-TBL-BOUNDS throw then
   0 SCRIPT-ARGV$ s" nested" STR= 0= if E-TBL-BOUNDS throw then
   1 SCRIPT-ARGV$ HBT-AOT-SRC-BUF HBT-AOT-SRC-U HBT-COPY!
   2 SCRIPT-ARGV$ BF-ENGINE!
   T-RESET
   HBT-MAKER-NESTED-STEPS
   T-REPORT ;


\ A fresh process by claim: the out root is resolved once per process from
\ HABU_OUT, so only a child whose HABU_OUT is HBT-TMP/out puts the damaged
\ record in a private out root, beside its private cache root HBT-TMP.
: HBT-MAKER-NESTED-INPUTS ( -- )
   HBT-TMP s" out" HBT-NESTED-OUT HBT-NESTED-OUT-U HBT-PATH!
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_OUT" >LEN HBT-NESTED-OUT HBT-NESTED-OUT-U @ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build-aot-inputs-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" nested" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   BF-ENGINE$ >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outn:n errn:n rc:n :}
   rc 0<> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= ;

public
: HBT-AOT-INPUTS-MAIN ( -- )
   SCRIPT-ARGC 0 > if HBT-MAKER-NESTED-CHILD exit then
   T-RESET
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   HBT-LINKER HBT-ENGINE!
   HBT-PREPARE
   HBT-MAKER-NESTED-INPUTS
   HBT-ENV-SELECTED-INPUT
   HBT-SCOPED-INPUT
   HBT-ENV-WORKERS
   CLEANUP-RESET
   T-REPORT
   s" observed input artifacts: " type HBT-ROOT type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-INPUTS-MAIN
