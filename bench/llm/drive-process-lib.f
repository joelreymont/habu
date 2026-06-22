\ drive-process-lib.f - native stdlib process benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f and bench/llm/driver-fixture-helpers.f.

32 constant DP-FIX-CAP
3 constant DP-FIX-SMALL-CAP
1000 constant DP-FIX-TIMEOUT-MS
100 constant DP-FIX-SHORT-TIMEOUT-MS
10 constant DP-LF

create DP-PRELUDE-PATH FS-PATH-CAP allot
create DP-RC-PATH FS-PATH-CAP allot
create DP-CAPTURE-PATH FS-PATH-CAP allot
create DP-NONZERO-PATH FS-PATH-CAP allot
create DP-HANG-PATH FS-PATH-CAP allot
create DP-LONG-PATH FS-PATH-CAP allot

variable DP-PRELUDE-U
variable DP-RC-U
variable DP-CAPTURE-U
variable DP-NONZERO-U
variable DP-HANG-U
variable DP-LONG-U

: DP-PRELUDE$ ( -- ptr u8 n )
   DP-PRELUDE-PATH DP-PRELUDE-U @ ;

: DP-RC$ ( -- ptr u8 n )
   DP-RC-PATH DP-RC-U @ ;

: DP-CAPTURE$ ( -- ptr u8 n )
   DP-CAPTURE-PATH DP-CAPTURE-U @ ;

: DP-NONZERO$ ( -- ptr u8 n )
   DP-NONZERO-PATH DP-NONZERO-U @ ;

: DP-HANG$ ( -- ptr u8 n )
   DP-HANG-PATH DP-HANG-U @ ;

: DP-LONG$ ( -- ptr u8 n )
   DP-LONG-PATH DP-LONG-U @ ;

: DP-PATHS! ( -- )
   s" prelude.f" DP-PRELUDE-PATH DP-PRELUDE-U DS-JOIN!
   s" proc-rc-ok" DP-RC-PATH DP-RC-U DS-JOIN!
   s" proc-capture-out-err" DP-CAPTURE-PATH DP-CAPTURE-U DS-JOIN!
   s" proc-capture-nonzero" DP-NONZERO-PATH DP-NONZERO-U DS-JOIN!
   s" proc-capture-hang" DP-HANG-PATH DP-HANG-U DS-JOIN!
   s" proc-capture-long" DP-LONG-PATH DP-LONG-U DS-JOIN! ;

: DP-RC-TASK? ( -- bool )
   DS-NAME$ s" PROC-RUN-RC-OK?" STR= ;

: DP-CAPTURE-TASK? ( -- bool )
   DS-NAME$ s" PROC-CAPTURE-OUTERR-OK?" STR= ;

: DP-NONZERO-TASK? ( -- bool )
   DS-NAME$ s" PROC-CAPTURE-NONZERO-OK?" STR= ;

: DP-TIMEOUT-TASK? ( -- bool )
   DS-NAME$ s" PROC-CAPTURE-TIMEOUT" STR= ;

: DP-TRUNCATED-TASK? ( -- bool )
   DS-NAME$ s" PROC-CAPTURE-TRUNCATED" STR= ;

: DP-NEGATIVE-TASK? ( -- bool )
   DP-TIMEOUT-TASK? if DS-TRUE exit then
   DP-TRUNCATED-TASK? ;

: DP-SB-LF ( -- )
   DP-LF SB-APPEND-C ;

: DP-SHEBANG ( -- )
   s" #!/usr/bin/env bin/hb" SB-APPEND
   DP-SB-LF ;

: DP-SCRIPT$ ( ptr u8 n -- ptr u8 n ) {: body:ptr bodyu :}
   SB-RESET
   DP-SHEBANG
   body bodyu SB-APPEND
   DP-SB-LF
   SB$ ;

: DP-WRITE-SCRIPT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu body:ptr bodyu :}
   path pathu body bodyu DP-SCRIPT$ WRITE-ALL
   path pathu CHMOD-X ;

: DP-WRITE-SCRIPTS ( -- )
   DP-RC$ s" 0 0 0 die" DP-WRITE-SCRIPT
   DP-CAPTURE$ s" 111 emit 117 emit 116 emit create E 101 c, 114 c, 114 c, 2 E 3 write drop 0 0 0 die" DP-WRITE-SCRIPT
   DP-NONZERO$ s" 0 0 7 die" DP-WRITE-SCRIPT
   DP-HANG$ s" : HANG ( -- ) begin again ; HANG" DP-WRITE-SCRIPT
   DP-LONG$ s" 97 emit 98 emit 99 emit 100 emit 101 emit 102 emit" DP-WRITE-SCRIPT ;

: DP-PREPARE-FIXTURE ( -- )
   DP-PATHS!
   DP-WRITE-SCRIPTS ;

: DP-BUILD-PRELUDE ( -- )
   DS-TEST-RESET
   DP-FIX-CAP s" PROC-FIX-CAP" DFH-CONSTANT
   DP-FIX-SMALL-CAP s" PROC-FIX-SMALL-CAP" DFH-CONSTANT
   DP-FIX-TIMEOUT-MS s" PROC-FIX-TIMEOUT-MS" DFH-CONSTANT
   DP-FIX-SHORT-TIMEOUT-MS s" PROC-FIX-SHORT-TIMEOUT-MS" DFH-CONSTANT
   s" create PROC-FIX-OUT PROC-FIX-CAP allot" DS-TEST-LN
   s" create PROC-FIX-ERR PROC-FIX-CAP allot" DS-TEST-LN
   DP-RC$ s" PROC-FIX-RC-PATH-BUF" s" PROC-FIX-RC-PATH$" DFH-STRING
   DP-CAPTURE$ s" PROC-FIX-CAPTURE-PATH-BUF" s" PROC-FIX-CAPTURE-PATH$" DFH-STRING
   DP-NONZERO$ s" PROC-FIX-NONZERO-PATH-BUF" s" PROC-FIX-NONZERO-PATH$" DFH-STRING
   DP-HANG$ s" PROC-FIX-HANG-PATH-BUF" s" PROC-FIX-HANG-PATH$" DFH-STRING
   DP-LONG$ s" PROC-FIX-LONG-PATH-BUF" s" PROC-FIX-LONG-PATH$" DFH-STRING
   s" out" s" PROC-FIX-OUT-WANT-BUF" s" PROC-FIX-OUT-WANT$" DFH-STRING
   s" err" s" PROC-FIX-ERR-WANT-BUF" s" PROC-FIX-ERR-WANT$" DFH-STRING
   DP-PRELUDE$ DS-TEST$ WRITE-ALL ;

: DP-BUILD-PROMPT ( -- )
   DS-BUILD-PROMPT
   s" " DS-PROMPT-LN
   s" The driver preloads a checked fixture vocabulary for deterministic process tests." DS-PROMPT-LN
   s" Use PROC-FIX-* path words, PROC-FIX-OUT, PROC-FIX-ERR, and the timeout/capacity constants." DS-PROMPT-LN
   s" PROC-FIX-RC-PATH$ exits 0; use RUN-RC for the rc task." DS-PROMPT-LN
   s" PROC-FIX-CAPTURE-PATH$ emits stdout out, stderr err, and exits 0." DS-PROMPT-LN
   s" PROC-FIX-NONZERO-PATH$ exits 7 without output." DS-PROMPT-LN
   s" PROC-FIX-HANG-PATH$ exceeds PROC-FIX-SHORT-TIMEOUT-MS." DS-PROMPT-LN
   s" PROC-FIX-LONG-PATH$ emits more bytes than PROC-FIX-SMALL-CAP." DS-PROMPT-LN
   s" Do not use SCRIPT-ARGV$, RUN-ARGV*, evaluate, or define/shadow PROC-FIX-* words." DS-PROMPT-LN ;

: DP-CAND-SHADOW-FIXTURE? ( -- bool )
   DS-CAND$ s" : PROC-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" create PROC-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" variable PROC-FIX-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" constant PROC-FIX-" CONTAINS? ;

: DP-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-ARGV" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? if DS-TRUE exit then
   DP-CAND-SHADOW-FIXTURE? ;

: DP-CAND-USES-REQUIRED? ( -- bool )
   DP-RC-TASK? if
      s" RUN-RC" DS-CAND-CONTAINS?
      s" PROC-FIX-RC-PATH$" DS-CAND-CONTAINS? and exit
   then
   DP-CAPTURE-TASK? if
      s" RUN-CAPTURE" DS-CAND-CONTAINS?
      s" PROC-FIX-CAPTURE-PATH$" DS-CAND-CONTAINS? and
      s" PROC-FIX-OUT" DS-CAND-CONTAINS? and
      s" PROC-FIX-ERR" DS-CAND-CONTAINS? and
      s" PROC-FIX-CAP" DS-CAND-CONTAINS? and
      s" PROC-FIX-TIMEOUT-MS" DS-CAND-CONTAINS? and
      s" PROC-FIX-OUT-WANT$" DS-CAND-CONTAINS? and
      s" PROC-FIX-ERR-WANT$" DS-CAND-CONTAINS? and
      s" STR=" DS-CAND-CONTAINS? and exit
   then
   DP-NONZERO-TASK? if
      s" RUN-CAPTURE" DS-CAND-CONTAINS?
      s" PROC-FIX-NONZERO-PATH$" DS-CAND-CONTAINS? and
      s" PROC-FIX-OUT" DS-CAND-CONTAINS? and
      s" PROC-FIX-ERR" DS-CAND-CONTAINS? and
      s" PROC-FIX-CAP" DS-CAND-CONTAINS? and
      s" PROC-FIX-TIMEOUT-MS" DS-CAND-CONTAINS? and exit
   then
   DP-TIMEOUT-TASK? if
      s" RUN-CAPTURE" DS-CAND-CONTAINS?
      s" PROC-FIX-HANG-PATH$" DS-CAND-CONTAINS? and
      s" PROC-FIX-OUT" DS-CAND-CONTAINS? and
      s" PROC-FIX-ERR" DS-CAND-CONTAINS? and
      s" PROC-FIX-CAP" DS-CAND-CONTAINS? and
      s" PROC-FIX-SHORT-TIMEOUT-MS" DS-CAND-CONTAINS? and exit
   then
   DP-TRUNCATED-TASK? if
      s" RUN-CAPTURE" DS-CAND-CONTAINS?
      s" PROC-FIX-LONG-PATH$" DS-CAND-CONTAINS? and
      s" PROC-FIX-OUT" DS-CAND-CONTAINS? and
      s" PROC-FIX-ERR" DS-CAND-CONTAINS? and
      s" PROC-FIX-SMALL-CAP" DS-CAND-CONTAINS? and exit
   then
   DS-FALSE ;

: DP-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DP-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DP-CAND-USES-REQUIRED? ;

: DP-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-process" LR-ARM!
      exit
   then
   DP-CAND-FORBIDDEN? if
      s" forbidden fixture boundary" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-process" LR-ARM!
      exit
   then
   DS-CAND-COMPLETE? if
      s" required stdlib word missing" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-process" LR-ARM!
      exit
   then
   s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
   s" reject" DS-LR-REJECT
   s" habu-stdlib-process" LR-ARM! ;

: DP-LR-REJECT ( ptr u8 n -- )
   DS-LR-REJECT
   s" habu-stdlib-process" LR-ARM! ;

: DP-LR-PASS ( -- )
   DS-LR-PASS
   s" habu-stdlib-process" LR-ARM! ;

: DP-LR-FAIL ( -- )
   DS-LR-FAIL
   s" habu-stdlib-process" LR-ARM! ;

: DP-ADD-LIBS ( -- )
   DS-ADD-LIBS
   s" lib/process.f"  >LEN PROC-ARGV+ ;

: DP-RUN-CHECK ( -- )
   PROC-ARGV-ENV-RESET
   DP-ADD-LIBS
   DP-PRELUDE$  >LEN PROC-ARGV+
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-CHECK-CLEAN? if
      DS-DIAG-PATH$ s" " WRITE-ALL
      0 DS-DIAG-COUNT !
      exit
   then
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DP-RUN-TESTS ( -- )
   PROC-ARGV-ENV-RESET
   DP-ADD-LIBS
   DP-PRELUDE$  >LEN PROC-ARGV+
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-BUNDLE-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DP-BUILD-POSITIVE-TESTS ( -- )
   DS-TEST-RESET
   DS-STACK-DSL
   s" T{  " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  -> -1 }T" DS-TEST-LN
   s" DST-REPORT" DS-TEST-LN ;

: DP-EXPECTED-CODE$ ( -- ptr u8 n )
   DP-TIMEOUT-TASK? if s" code E-PROC-TIMEOUT" exit then
   s" code E-PROC-TRUNCATED" ;

: DP-EXPECTED-CONST$ ( -- ptr u8 n )
   DP-TIMEOUT-TASK? if s" E-PROC-TIMEOUT" exit then
   s" E-PROC-TRUNCATED" ;

: DP-BUILD-NEGATIVE-TESTS ( -- )
   DS-TEST-RESET
   s" : DP-NEG-MAIN ( -- )" DS-TEST-LN
   s"    ['] " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  catch" DS-TEST-LN
   s"    dup " DS-TEST+
   DP-EXPECTED-CONST$ DS-TEST+
   s"  = if drop " DS-TEST+
   DP-EXPECTED-CODE$ DFH-SOURCE-S"
   s"  type cr exit then" DS-TEST-LN
   s"    0= if " DS-TEST+
   s" silent success" DFH-SOURCE-S"
   s"  type cr 1 die then" DS-TEST-LN
   s"    " DS-TEST+
   s" wrong error code" DFH-SOURCE-S"
   s"  type cr 1 die ;" DS-TEST-LN
   s" DP-NEG-MAIN" DS-TEST-LN ;

: DP-WRITE-BUNDLE ( -- )
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DP-FINISH-NEGATIVE ( -- )
   DP-EXPECTED-CODE$ DS-LR-NEGATIVE ;

: DP-BUILD-TESTS ( -- )
   DP-NEGATIVE-TASK? if DP-BUILD-NEGATIVE-TESTS exit then
   DP-BUILD-POSITIVE-TESTS ;

: DP-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DP-BUILD-TESTS
   DP-WRITE-BUNDLE
   DP-CAND-VALID? 0= if DP-INVALID-CANDIDATE exit then
   DP-RUN-CHECK
   DS-RC @ 0 <> if s" reject" DP-LR-REJECT exit then
   DP-RUN-TESTS
   DP-NEGATIVE-TASK? if DP-FINISH-NEGATIVE exit then
   DS-TEST-PASS? if DP-LR-PASS else DP-LR-FAIL then ;

: DP-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DP-PREPARE-FIXTURE
   DP-BUILD-PRELUDE
   DP-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DP-MODEL-ERROR ( -- )
   DS-MODEL-ERROR
   s" habu-stdlib-process" LR-ARM! ;

: DP-RUN-MODEL ( -- )
   DP-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DP-MODEL-ERROR exit then
   MRUN-TEXT$ DP-EVALUATE-TEXT ;

: DP-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DP-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DP-EVALUATE-TEXT ;

: DP-USAGE ( -- )
   s" usage: bench/llm/drive-process.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DP-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DP-USAGE then
   SCRIPT-ARGC 7 > if DP-USAGE then
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

: DP-MAIN ( -- )
   DP-CONFIG
   DP-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
