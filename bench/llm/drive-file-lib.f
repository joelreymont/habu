\ drive-file-lib.f - native stdlib file benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f.

64 constant DF-FIX-CAP
3 constant DF-FIX-SMALL-CAP

create DF-PRELUDE-PATH FS-PATH-CAP allot
create DF-READ-PATH FS-PATH-CAP allot
create DF-WRITE-PATH FS-PATH-CAP allot
create DF-APPEND-PATH FS-PATH-CAP allot
create DF-BIG-PATH FS-PATH-CAP allot

variable DF-PRELUDE-U
variable DF-READ-U
variable DF-WRITE-U
variable DF-APPEND-U
variable DF-BIG-U

: DF-PRELUDE$ ( -- ptr u8 n )
   DF-PRELUDE-PATH DF-PRELUDE-U @ ;

: DF-READ$ ( -- ptr u8 n )
   DF-READ-PATH DF-READ-U @ ;

: DF-WRITE$ ( -- ptr u8 n )
   DF-WRITE-PATH DF-WRITE-U @ ;

: DF-APPEND$ ( -- ptr u8 n )
   DF-APPEND-PATH DF-APPEND-U @ ;

: DF-BIG$ ( -- ptr u8 n )
   DF-BIG-PATH DF-BIG-U @ ;

: DF-PATHS! ( -- )
   s" prelude.f" DF-PRELUDE-PATH DF-PRELUDE-U DS-JOIN!
   s" read.txt" DF-READ-PATH DF-READ-U DS-JOIN!
   s" write.txt" DF-WRITE-PATH DF-WRITE-U DS-JOIN!
   s" append.txt" DF-APPEND-PATH DF-APPEND-U DS-JOIN!
   s" big.txt" DF-BIG-PATH DF-BIG-U DS-JOIN! ;

: DF-READ-TASK? ( -- bool )
   DS-NAME$ s" FS-READ-ALL-OK?" STR= ;

: DF-WRITE-TASK? ( -- bool )
   DS-NAME$ s" FS-WRITE-ALL-OK?" STR= ;

: DF-APPEND-TASK? ( -- bool )
   DS-NAME$ s" FS-APPEND-OK?" STR= ;

: DF-CAPACITY-TASK? ( -- bool )
   DS-NAME$ s" FS-READ-CAPACITY" STR= ;

: DF-PREPARE-FIXTURE ( -- )
   DF-PATHS!
   DF-READ$ s" alpha-beta" WRITE-ALL
   DF-APPEND$ s" alpha" WRITE-ALL
   DF-BIG$ s" abcd" WRITE-ALL ;

: DF-BUILD-PRELUDE ( -- )
   DS-TEST-RESET
   DF-FIX-CAP s" FS-FIX-CAP" DFH-CONSTANT
   DF-FIX-SMALL-CAP s" FS-FIX-SMALL-CAP" DFH-CONSTANT
   s" create FS-FIX-BUF FS-FIX-CAP allot" DS-TEST-LN
   DF-READ$ s" FS-FIX-READ-PATH-BUF" s" FS-FIX-READ-PATH$" DFH-STRING
   DF-WRITE$ s" FS-FIX-WRITE-PATH-BUF" s" FS-FIX-WRITE-PATH$" DFH-STRING
   DF-APPEND$ s" FS-FIX-APPEND-PATH-BUF" s" FS-FIX-APPEND-PATH$" DFH-STRING
   DF-BIG$ s" FS-FIX-BIG-PATH-BUF" s" FS-FIX-BIG-PATH$" DFH-STRING
   s" alpha-beta" s" FS-FIX-READ-WANT-BUF" s" FS-FIX-READ-WANT$" DFH-STRING
   s" omega" s" FS-FIX-WRITE-DATA-BUF" s" FS-FIX-WRITE-DATA$" DFH-STRING
   s" -beta" s" FS-FIX-APPEND-DATA-BUF" s" FS-FIX-APPEND-DATA$" DFH-STRING
   s" alpha-beta" s" FS-FIX-APPEND-WANT-BUF" s" FS-FIX-APPEND-WANT$" DFH-STRING
   DF-PRELUDE$ DS-TEST$ WRITE-ALL ;

: DF-BUILD-PROMPT ( -- )
   DS-BUILD-PROMPT
   s" " DS-PROMPT-LN
   s" The driver preloads a checked fixture vocabulary for deterministic file tests." DS-PROMPT-LN
   s" Use FS-FIX-BUF, FS-FIX-CAP, and the FS-FIX-* path/data words." DS-PROMPT-LN
   s" FS-FIX-READ-PATH$ points at a file containing alpha-beta." DS-PROMPT-LN
   s" FS-FIX-WRITE-PATH$ is the path to create and read back." DS-PROMPT-LN
   s" FS-FIX-APPEND-PATH$ starts with alpha; append FS-FIX-APPEND-DATA$." DS-PROMPT-LN
   s" FS-FIX-BIG-PATH$ contains abcd; FS-FIX-SMALL-CAP is too small." DS-PROMPT-LN
   s" Do not use SCRIPT-ARGV$ and do not define or shadow FS-FIX-* words." DS-PROMPT-LN ;

: DF-CAND-SHADOW-FIXTURE? ( -- bool )
   DS-CAND$ s" : FS-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" create FS-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" variable FS-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" constant FS-FIX-" CONTAINS? ;

: DF-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? if DS-TRUE exit then
   DF-CAND-SHADOW-FIXTURE? ;

: DF-CAND-USES-REQUIRED? ( -- bool )
   DF-READ-TASK? if
      s" READ-ALL" DS-CAND-CONTAINS?
      s" FS-FIX-READ-PATH$" DS-CAND-CONTAINS? and
      s" FS-FIX-BUF" DS-CAND-CONTAINS? and
      s" FS-FIX-CAP" DS-CAND-CONTAINS? and
      s" FS-FIX-READ-WANT$" DS-CAND-CONTAINS? and
      s" STR=" DS-CAND-CONTAINS? and exit
   then
   DF-WRITE-TASK? if
      s" WRITE-ALL" DS-CAND-CONTAINS?
      s" READ-ALL" DS-CAND-CONTAINS? and
      s" FS-FIX-WRITE-PATH$" DS-CAND-CONTAINS? and
      s" FS-FIX-WRITE-DATA$" DS-CAND-CONTAINS? and
      s" FS-FIX-BUF" DS-CAND-CONTAINS? and
      s" FS-FIX-CAP" DS-CAND-CONTAINS? and
      s" STR=" DS-CAND-CONTAINS? and exit
   then
   DF-APPEND-TASK? if
      s" APPEND-FILE" DS-CAND-CONTAINS?
      s" READ-ALL" DS-CAND-CONTAINS? and
      s" FS-FIX-APPEND-PATH$" DS-CAND-CONTAINS? and
      s" FS-FIX-APPEND-DATA$" DS-CAND-CONTAINS? and
      s" FS-FIX-APPEND-WANT$" DS-CAND-CONTAINS? and
      s" FS-FIX-BUF" DS-CAND-CONTAINS? and
      s" FS-FIX-CAP" DS-CAND-CONTAINS? and
      s" STR=" DS-CAND-CONTAINS? and exit
   then
   DF-CAPACITY-TASK? if
      s" READ-ALL" DS-CAND-CONTAINS?
      s" FS-FIX-BIG-PATH$" DS-CAND-CONTAINS? and
      s" FS-FIX-BUF" DS-CAND-CONTAINS? and
      s" FS-FIX-SMALL-CAP" DS-CAND-CONTAINS? and exit
   then
   DS-FALSE ;

: DF-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DF-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DF-CAND-USES-REQUIRED? ;

: DF-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-file" LR-ARM!
      exit
   then
   DF-CAND-FORBIDDEN? if
      s" forbidden fixture boundary" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-file" LR-ARM!
      exit
   then
   DS-CAND-COMPLETE? if
      s" required stdlib word missing" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-file" LR-ARM!
      exit
   then
   s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
   s" reject" DS-LR-REJECT
   s" habu-stdlib-file" LR-ARM! ;

: DF-LR-REJECT ( ptr u8 n -- )
   DS-LR-REJECT
   s" habu-stdlib-file" LR-ARM! ;

: DF-LR-PASS ( -- )
   DS-LR-PASS
   s" habu-stdlib-file" LR-ARM! ;

: DF-LR-FAIL ( -- )
   DS-LR-FAIL
   s" habu-stdlib-file" LR-ARM! ;

: DF-RUN-CHECK ( -- )
   PROC-ARGV-ENV-RESET
   DS-ADD-LIBS
   DF-PRELUDE$ PROC-ARGV+
   DS-CAND-PATH$ PROC-ARGV+
   DS-HB-CAPTURE
   DS-CHECK-CLEAN? if
      DS-DIAG-PATH$ s" " WRITE-ALL
      0 DS-DIAG-COUNT !
      exit
   then
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DF-RUN-TESTS ( -- )
   PROC-ARGV-ENV-RESET
   DS-ADD-LIBS
   DF-PRELUDE$ PROC-ARGV+
   DS-CAND-PATH$ PROC-ARGV+
   DS-BUNDLE-PATH$ PROC-ARGV+
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DF-BUILD-POSITIVE-TESTS ( -- )
   DS-TEST-RESET
   DS-STACK-DSL
   s" T{  " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  -> -1 }T" DS-TEST-LN
   s" DST-REPORT" DS-TEST-LN ;

: DF-BUILD-NEGATIVE-TESTS ( -- )
   DS-TEST-RESET
   s" : DF-NEG-MAIN ( -- )" DS-TEST-LN
   s"    ['] FS-READ-CAPACITY catch" DS-TEST-LN
   s"    dup E-FS-CAPACITY = if drop " DS-TEST+
   s" code E-FS-CAPACITY" DFH-SOURCE-S"
   s"  type cr exit then" DS-TEST-LN
   s"    0= if " DS-TEST+
   s" silent success" DFH-SOURCE-S"
   s"  type cr 1 die then" DS-TEST-LN
   s"    " DS-TEST+
   s" wrong error code" DFH-SOURCE-S"
   s"  type cr 1 die ;" DS-TEST-LN
   s" DF-NEG-MAIN" DS-TEST-LN ;

: DF-WRITE-BUNDLE ( -- )
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DF-FINISH-NEGATIVE ( -- )
   s" code E-FS-CAPACITY" DS-LR-NEGATIVE ;

: DF-BUILD-TESTS ( -- )
   DF-CAPACITY-TASK? if DF-BUILD-NEGATIVE-TESTS exit then
   DF-BUILD-POSITIVE-TESTS ;

: DF-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DF-BUILD-TESTS
   DF-WRITE-BUNDLE
   DF-CAND-VALID? 0= if DF-INVALID-CANDIDATE exit then
   DF-RUN-CHECK
   DS-RC @ 0 <> if s" reject" DF-LR-REJECT exit then
   DF-RUN-TESTS
   DF-CAPACITY-TASK? if DF-FINISH-NEGATIVE exit then
   DS-TEST-PASS? if DF-LR-PASS else DF-LR-FAIL then ;

: DF-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DF-PREPARE-FIXTURE
   DF-BUILD-PRELUDE
   DF-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DF-MODEL-ERROR ( -- )
   DS-MODEL-ERROR
   s" habu-stdlib-file" LR-ARM! ;

: DF-RUN-MODEL ( -- )
   DF-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DF-MODEL-ERROR exit then
   MRUN-TEXT$ DF-EVALUATE-TEXT ;

: DF-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DF-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DF-EVALUATE-TEXT ;

: DF-USAGE ( -- )
   s" usage: bench/llm/drive-file.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DF-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DF-USAGE then
   SCRIPT-ARGC 7 > if DF-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   3 SCRIPT-ARGV$ DS-CATEGORY!
   4 SCRIPT-ARGV$ DS-TESTS!
   5 SCRIPT-ARGV$ DS-SPEC!
   SCRIPT-ARGC 6 > if 6 SCRIPT-ARGV$ DS-PARSE-U else 1 then DS-MAX-REPAIRS !
   DS-DEFAULTS
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DF-MAIN ( -- )
   DF-CONFIG
   DF-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
