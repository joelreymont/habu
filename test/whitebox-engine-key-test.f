\ Exercise direct whitebox publication, private-copy execution and reuse.
\ The unified builder owns source invalidation; native-build-edit-e2e.f covers
\ changed-source runtime behavior and cached/cold equivalence. Here an unloaded
\ file preserves its artifact, and build failures leave no supplier or product.
\ Run: bin/hb --load test/whitebox-engine-key-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/process-cwd.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f
require src/os/script-argv.f
require test/suite-budget.f

package WHITEBOX-KEY-TEST

65536 constant CAP
32 constant DIGEST-LEN
create ROOT FS-PATH-CAP allot
create HB FS-PATH-CAP allot
create PREFIX FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
create TMP FS-PATH-CAP allot
create CACHE FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create ENG-A DIGEST-LEN allot
create ENG-B DIGEST-LEN allot
create SHA SHA256-FILE-CTX-BYTES allot
variable ROOT-U
variable HB-U
variable PREFIX-U
variable IMAGE-U
variable TMP-U
variable CACHE-U
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: PREFIX$ ( -- ptr u8 n ) PREFIX PREFIX-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: CACHE$ ( -- ptr u8 n ) CACHE CACHE-U @ ;

: ENGINE-DIGEST! ( ptr u8 -- ) {: dst:ptr :}
   SHA ENGINE-CANDIDATE:PATH$ dst SHA256-FILE-IN dup 0<> if throw then drop ;

: PREPARE ( -- )
   s" habu-whitebox-fresh" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   s" test" ROOT$ TREE-COPY:TREE
   ROOT$ s" bin/hb" HB JOIN-PATH HB-U !
   ROOT$ s" src/core/internal-mark.f" PREFIX JOIN-PATH PREFIX-U !
   ROOT$ s" whitebox" IMAGE JOIN-PATH IMAGE-U !
   ROOT$ s" tmp" TMP JOIN-PATH TMP-U !
   ROOT$ s" cache" CACHE JOIN-PATH CACHE-U !
   HB$ SOURCE-ROOT:DIRNAME MAKE-DIRS
   ENGINE-CANDIDATE:PATH$ HB$ COPY-FILE-STREAM
   HB$ CHMOD-X
   TMP$ MAKE-DIR CACHE$ MAKE-DIR
   ENG-A ENGINE-DIGEST! ;

: ENV ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" HABU_UNDER_TEST" >LEN HB$ >LEN PROC-ENV+
   s" HABU_GATE_COMPILER_IMAGES" >LEN s" " >LEN PROC-ENV+
   s" HABU_GATE_IMAGES" >LEN s"  whitebox-engine " >LEN PROC-ENV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: RESULT-RC ( len len outcome -- n ) {: outu:len erru:len oc :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   oc PROC-OUTCOME>RC RC>N RC !
   RC @ 0<> if OUT OUT-U @ type 2 ERR ERR-U @ write drop then
   RC @ ;

: RESULT ( len len outcome -- ) RESULT-RC 0 T= ;

: PROVIDER$ ( -- ptr u8 n )
   S\" require test/whitebox-engine.f\nrequire src/lib/test.f\npackage WB-REQUEST\nFS-PATH-CAP BUFFER: SAVED\nvariable USED\n: SAVED$ ( -- ptr u8 n ) SAVED USED @ ;\n: RUN ( -- )\n T-RESET\n WHITEBOX-ENGINE:PATH$ {: a:ptr u:n :}\n a SAVED u BYTE-COPY u USED !\n s\" supplier: \" type SAVED$ type cr\n s\" BEGIN-REUSE\" type cr\n SAVED$ WHITEBOX-ENGINE:PATH$ T$=\n s\" whitebox\" WHITEBOX-ENGINE:PROVIDE\n SAVED$ WHITEBOX-ENGINE:PATH$ T$=\n SAVED$ EXECUTABLE? TTRUE\n s\" END-REUSE\" type cr\n T-REPORT ;\n' RUN\n;package\nexecute\n" ;

: PROVIDE ( -- )
   ENV
   HB$ >LEN ROOT$ >LEN PROVIDER$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT
   OUT OUT-U @ S\" BEGIN-REUSE\nEND-REUSE\n" CONTAINS? TTRUE ;

: RUN-IMAGE ( ptr u8 n -- ) {: want:ptr wantu:n :}
   ENV
   IMAGE$ >LEN ROOT$ >LEN s" WHITEBOX-KEY-PROBE . cr" >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT
   OUT OUT-U @ want wantu T$= ;

: ORIGINAL-PREFIX ( -- )
   s" src/core/internal-mark.f" ROOT$ TREE-COPY:FILE ;

: PREFIX-41 ( -- )
   ORIGINAL-PREFIX
   PREFIX$ S\" \n: WHITEBOX-KEY-PROBE ( -- n ) 41 ;\n" APPEND-FILE ;

: NO-SUPPLIER ( ptr u8 n -- )
   s" hb-whitebox-" STARTS-WITH? TFALSE ;

: FAILURE-REQUEST$ ( -- ptr u8 n )
   S\" require test/whitebox-engine.f\nrequire src/lib/test.f\nT-RESET\n: REQUEST ( -- ) s\" failed-whitebox\" WHITEBOX-ENGINE:PROVIDE ;\n' REQUEST E-PROC-TIMEOUT TTHROWS\nT-REPORT\n" ;

: FAILURE-REQUEST ( -- n )
   HB$ >LEN ROOT$ >LEN FAILURE-REQUEST$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT-RC ;

: ASSERT-NO-PRODUCT ( -- )
   IMAGE$ EXISTS? TFALSE
   CACHE$ [: NO-SUPPLIER ;] FS-LIST:EACH ;

: FAILURE-CASES ( -- )
   ROOT$ s" timeout-cache" CACHE JOIN-PATH CACHE-U !
   CACHE$ MAKE-DIR
   ROOT$ s" failed-whitebox" IMAGE JOIN-PATH IMAGE-U !
   s" a fresh whitebox build deadline remains a caught timeout" T-LABEL
   ENV
   s" HB_BUILD_TIMEOUT_MS" >LEN s" 1" >LEN PROC-ENV-SET
   FAILURE-REQUEST 0 T=
   ASSERT-NO-PRODUCT
   s" an ordinary builder failure keeps exit 67" T-LABEL
   ROOT$ s" tools/native-build-support.f" PREFIX JOIN-PATH PREFIX-U !
   PREFIX$ S\" \ns\" whitebox-missing-input.f\" FILE-SIZE drop\n" APPEND-FILE
   ENV
   FAILURE-REQUEST 67 T=
   ASSERT-NO-PRODUCT ;

public

: RUN ( -- )
   T-RESET PREPARE
   s" whitebox freshness artifacts: " type ROOT$ type cr
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" --timeout-only" STR= if
         FAILURE-CASES T-REPORT exit
      then
   then
   s" the supplied whitebox runs its copied boot prefix" T-LABEL
   PREFIX-41 PROVIDE S\" 41\n\n" RUN-IMAGE
   s" an unloaded file does not invalidate the builder's artifact" T-LABEL
   ROOT$ s" src/core/whitebox-key-unloaded.f" PREFIX JOIN-PATH PREFIX-U !
   PREFIX$ s" unreachable fixture" WRITE-ALL
   PROVIDE
   OUT OUT-U @ s" engine artifact hit" CONTAINS? TTRUE
   S\" 41\n\n" RUN-IMAGE
   s" and bin/hb was never written" T-LABEL
   ENG-B ENGINE-DIGEST!
   ENG-A DIGEST-LEN ENG-B DIGEST-LEN T$=
   FAILURE-CASES
   s" whitebox freshness artifacts: " type ROOT$ type cr
   T-REPORT ;

;package

WHITEBOX-KEY-TEST:RUN
