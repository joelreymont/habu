\ Exercise direct whitebox publication, private-copy execution and reuse.
\ The unified builder owns source invalidation; native-build-edit-e2e.f covers
\ changed-source runtime behavior, unloaded files and cached/cold equivalence.
\ Here direct publication uses the ordinary out root, and build failures leave
\ no supplier, work directory or product under a private one.
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
create ROOT PATH-CAP allot
create HB PATH-CAP allot
create PREFIX PATH-CAP allot
create IMAGE PATH-CAP allot
create TMP PATH-CAP allot
create OUT-ROOT PATH-CAP allot
create DIR PATH-CAP allot
create IMG-DIR PATH-CAP allot
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
variable OUT-ROOT-U
variable DIR-U
variable IMG-DIR-U
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: PREFIX$ ( -- ptr u8 n ) PREFIX PREFIX-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: OUT-ROOT$ ( -- ptr u8 n ) OUT-ROOT OUT-ROOT-U @ ;
: DIR$ ( -- ptr u8 n ) DIR DIR-U @ ;
: IMG-DIR$ ( -- ptr u8 n ) IMG-DIR IMG-DIR-U @ ;

: ENGINE-DIGEST! ( ptr u8 -- ) {: dst:ptr :}
   SHA ENGINE-CANDIDATE:PATH$ dst SHA256-FILE-IN dup 0<> if throw then drop ;

: PREPARE ( -- )
   SOURCE-ROOT:CWD$ SOURCE-ROOT:CANONICAL TTRUE
   {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE
   {: hb:ptr hbu:n :}
   hb HB hbu BYTE-COPY hbu HB-U !
   s" habu-whitebox-copy" FS:MKTEMP-DIR SOURCE-ROOT:CANONICAL TTRUE
   {: tmp:ptr tmpu:n :}
   tmp TMP tmpu BYTE-COPY tmpu TMP-U !
   TMP$ s" whitebox" IMAGE FS:JOIN-PATH IMAGE-U !
   ENG-A ENGINE-DIGEST! ;

: PREPARE-FAILURES ( -- )
   s" habu-whitebox-fresh" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   s" test" ROOT$ TREE-COPY:TREE
   ROOT$ s" bin/hb" HB FS:JOIN-PATH HB-U !
   ROOT$ s" tmp" TMP FS:JOIN-PATH TMP-U !
   HB$ SOURCE-ROOT:DIRNAME FS:MAKE-DIRS
   ENGINE-CANDIDATE:PATH$ HB$ FS:COPY-FILE-STREAM
   HB$ FS:CHMOD-X
   TMP$ FS:MAKE-DIR ;

: ENV ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" HABU_UNDER_TEST" >LEN HB$ >LEN PROC-ENV+
   s" HABU_GATE_COMPILER_IMAGES" >LEN s" " >LEN PROC-ENV+
   s" HABU_GATE_IMAGES" >LEN s"  whitebox-engine " >LEN PROC-ENV+
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: RESULT-RC ( len len outcome -- n ) {: outu:len erru:len oc :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   oc PROC-OUTCOME>RC RC>N RC !
   RC @ 0<> if OUT OUT-U @ type 2 ERR ERR-U @ write drop then
   RC @ ;

: RESULT ( len len outcome -- ) RESULT-RC 0 T= ;

: PROVIDER$ ( -- ptr u8 n )
   S\" require test/whitebox-engine.f\nrequire src/lib/test.f\nrequire src/os/script-argv.f\npackage WB-REQUEST\nPATH-CAP BUFFER: SAVED\nvariable USED\n: SAVED$ ( -- ptr u8 n ) SAVED USED @ ;\n: RUN ( -- )\n T-RESET\n WHITEBOX-ENGINE:PATH$ {: a:ptr u:n :}\n a SAVED u BYTE-COPY u USED !\n s\" supplier: \" type SAVED$ type cr\n s\" BEGIN-REUSE\" type cr\n SAVED$ WHITEBOX-ENGINE:PATH$ T$=\n 0 SCRIPT-ARGV$ WHITEBOX-ENGINE:PROVIDE\n SAVED$ WHITEBOX-ENGINE:PATH$ T$=\n SAVED$ FS:EXECUTABLE? TTRUE\n s\" END-REUSE\" type cr\n T-REPORT ;\n' RUN\n;package\nexecute\n" ;

: PROVIDE ( -- )
   ENV
   s" --" >LEN PROC-ARGV+ IMAGE$ >LEN PROC-ARGV+
   HB$ >LEN ROOT$ >LEN PROVIDER$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT
   OUT OUT-U @ S\" BEGIN-REUSE\nEND-REUSE\n" CONTAINS? TTRUE ;

: RUN-IMAGE ( -- )
   ENV
   IMAGE$ >LEN ROOT$ >LEN
   s" USIGS drop ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX = bool>n . cr" >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT
   OUT OUT-U @ S\" -1\n\n" T$= ;

: NO-SUPPLIER ( ptr u8 n -- )
   s" whitebox-engine" STARTS-WITH? TFALSE ;

\ The img/ directory of one engine's out root holds no whitebox image, key or
\ work directory.
: IMG-CLEAN ( ptr u8 n -- ) {: name:ptr nameu:n :}
   OUT-ROOT$ name nameu DIR FS:JOIN-PATH DIR-U !
   DIR$ s" img" IMG-DIR FS:JOIN-PATH IMG-DIR-U !
   IMG-DIR$ FS:EXISTS? if IMG-DIR$ [: NO-SUPPLIER ;] FS-LIST:EACH then ;

: FAILURE-REQUEST$ ( -- ptr u8 n )
   S\" require test/whitebox-engine.f\nrequire src/lib/test.f\nT-RESET\n: REQUEST ( -- ) s\" failed-whitebox\" WHITEBOX-ENGINE:PROVIDE ;\n' REQUEST E-PROC-TIMEOUT TTHROWS\nT-REPORT\n" ;

: FAILURE-REQUEST ( -- n )
   HB$ >LEN ROOT$ >LEN FAILURE-REQUEST$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME RESULT-RC ;

: ASSERT-NO-PRODUCT ( -- )
   IMAGE$ FS:EXISTS? TFALSE
   OUT-ROOT$ FS:EXISTS? if OUT-ROOT$ [: IMG-CLEAN ;] FS-LIST:EACH then ;

: FAILURE-CASES ( -- )
   PREPARE-FAILURES
   s" whitebox failure artifacts: " type ROOT$ type cr
   ROOT$ s" out" OUT-ROOT FS:JOIN-PATH OUT-ROOT-U !
   ROOT$ s" failed-whitebox" IMAGE FS:JOIN-PATH IMAGE-U !
   s" a fresh whitebox build deadline remains a caught timeout" T-LABEL
   ENV
   s" HABU_OUT" >LEN OUT-ROOT$ >LEN PROC-ENV-SET
   s" HB_BUILD_TIMEOUT_MS" >LEN s" 1" >LEN PROC-ENV-SET
   FAILURE-REQUEST 0 T=
   ASSERT-NO-PRODUCT
   s" an ordinary builder failure keeps exit 67" T-LABEL
   ROOT$ s" tools/native-build-support.f" PREFIX FS:JOIN-PATH PREFIX-U !
   \ Fail in the real support child before unrelated compiler initialization.
   PREFIX$ S\" require src/lib/fs.f\ns\" whitebox-missing-input.f\" FS:FILE-SIZE drop\n" FS:WRITE-ALL
   ENV
   s" HABU_OUT" >LEN OUT-ROOT$ >LEN PROC-ENV-SET
   FAILURE-REQUEST 67 T=
   ASSERT-NO-PRODUCT ;

public

: RUN ( -- )
   T-RESET PREPARE
   s" whitebox private copy: " type IMAGE$ type cr
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" --timeout-only" STR= if
         FAILURE-CASES T-REPORT exit
      then
   then
   s" the direct supplier publishes a private unsealed engine" T-LABEL
   PROVIDE RUN-IMAGE
   s" and bin/hb was never written" T-LABEL
   ENG-B ENGINE-DIGEST!
   ENG-A DIGEST-LEN ENG-B DIGEST-LEN T$=
   FAILURE-CASES
   T-REPORT ;

;package

WHITEBOX-KEY-TEST:RUN
