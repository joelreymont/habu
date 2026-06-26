\ drive-property-lib.f - native stdlib property benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f, bench/llm/drive-stdlib-live.f,
\ bench/llm/driver-token-helpers.f, and bench/llm/driver-fixture-helpers.f.

: DPR-DEFAULTS-TASK? ( -- bool )
   DS-NAME$ s" PROP-DEFAULTS-OK?" STR= ;

: DPR-RND-TASK? ( -- bool )
   DS-NAME$ s" PROP-RND-SEQ-OK?" STR= ;

: DPR-GEN-TASK? ( -- bool )
   DS-NAME$ s" PROP-GEN-SCRIPT-OK?" STR= ;

: DPR-SHRINK-TASK? ( -- bool )
   DS-NAME$ s" PROP-SHRINK-OK?" STR= ;

: DPR-BAD-SEED-TASK? ( -- bool )
   DS-NAME$ s" PROP-BAD-SEED" STR= ;

: DPR-BUILD-PROMPT ( -- )
   DS-BUILD-PROMPT
   s" " DS-PROMPT-LN
   s" The driver preloads lib/property.f for deterministic property-test helpers." DS-PROMPT-LN
   s" Use PROP-DEFAULTS, PROP-RUN-RESET, PROP-RND, PROP-RND%, PROP-SEED@, PROP-COUNT@, generator buffer words, and PROP-SHRINK as requested." DS-PROMPT-LN
   s" Do not use TRUST, TRUSTED:, set-check, evaluate, or host/process/argv boundaries." DS-PROMPT-LN ;

: DPR-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-RC" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-CAPTURE" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-ARGV" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? ;

: DPR-CAND-USES-REQUIRED? ( -- bool )
   DPR-DEFAULTS-TASK? if s" PROP-DEFAULTS" DS-CAND-HAS-WORD? exit then
   DPR-RND-TASK? if
      s" PROP-RUN-RESET" DS-CAND-HAS-WORD?
      s" PROP-RND" DS-CAND-HAS-WORD? and
      s" PROP-RND%" DS-CAND-HAS-WORD? and
      s" PROP-SEED@" DS-CAND-HAS-WORD? and
      s" PROP-COUNT@" DS-CAND-HAS-WORD? and exit
   then
   DPR-GEN-TASK? if
      s" PROP-GEN-START" DS-CAND-HAS-WORD?
      s" PROP-GEN-STEP" DS-CAND-HAS-WORD? and
      s" PROP-GEN-DEPTH@" DS-CAND-HAS-WORD? and
      s" PROP-BUF$" DS-CAND-HAS-WORD? and
      s" STR=" DS-CAND-HAS-WORD? and exit
   then
   DPR-SHRINK-TASK? if
      s" PROP-SHRINK" DS-CAND-HAS-WORD?
      s" PROP-BUF+" DS-CAND-HAS-WORD? and
      s" PROP-BUF$" DS-CAND-HAS-WORD? and
      s" STR=" DS-CAND-HAS-WORD? and exit
   then
   DPR-BAD-SEED-TASK? if
      s" PROP-RUN-RESET" DS-CAND-HAS-WORD? exit
   then
   DS-FALSE ;

: DPR-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DPR-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DPR-CAND-USES-REQUIRED? ;

: DPR-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-property" LR-ARM!
      exit
   then
   DPR-CAND-FORBIDDEN? if
      s" forbidden property boundary" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-property" LR-ARM!
      exit
   then
   DS-CAND-COMPLETE? if
      s" required stdlib word missing" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-property" LR-ARM!
      exit
   then
   s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
   s" reject" DS-LR-REJECT
   s" habu-stdlib-property" LR-ARM! ;

: DPR-LR-REJECT ( ptr u8 n -- )
   DS-LR-REJECT
   s" habu-stdlib-property" LR-ARM! ;

: DPR-LR-PASS ( -- )
   DS-LR-PASS
   s" habu-stdlib-property" LR-ARM! ;

: DPR-LR-FAIL ( -- )
   DS-LR-FAIL
   s" habu-stdlib-property" LR-ARM! ;

: DPR-ADD-LIBS ( -- )
   DS-ADD-LIBS
   s" lib/property.f"  >LEN PROC-ARGV+ ;

: DPR-RUN-CHECK ( -- )
   PROC-ARGV-ENV-RESET
   DPR-ADD-LIBS
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-CHECK-CLEAN? if
      DS-DIAG-PATH$ s" " WRITE-ALL
      0 DS-DIAG-COUNT !
      exit
   then
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DPR-RUN-TESTS ( -- )
   PROC-ARGV-ENV-RESET
   DPR-ADD-LIBS
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-BUNDLE-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DPR-BUILD-POSITIVE-TESTS ( -- )
   DS-TEST-RESET
   DS-STACK-DSL
   s" T{  " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  -> -1 }T" DS-TEST-LN
   s" DST-REPORT" DS-TEST-LN ;

: DPR-BUILD-NEGATIVE-TESTS ( -- )
   DS-TEST-RESET
   s" : DPR-NEG-MAIN ( -- )" DS-TEST-LN
   s"    [: PROP-BAD-SEED ;] catch" DS-TEST-LN
   s" code E-PROP-SEED" s" E-PROP-SEED" DS-NEGATIVE-TEST-TAIL
   s" DPR-NEG-MAIN" DS-TEST-LN ;

: DPR-WRITE-BUNDLE ( -- )
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DPR-FINISH-NEGATIVE ( -- )
   s" code E-PROP-SEED" DS-LR-NEGATIVE ;

: DPR-BUILD-TESTS ( -- )
   DPR-BAD-SEED-TASK? if DPR-BUILD-NEGATIVE-TESTS exit then
   DPR-BUILD-POSITIVE-TESTS ;

: DPR-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DPR-BUILD-TESTS
   DPR-WRITE-BUNDLE
   DPR-CAND-VALID? 0= if DPR-INVALID-CANDIDATE exit then
   DPR-RUN-CHECK
   DS-RC @ 0 <> if s" reject" DPR-LR-REJECT exit then
   DPR-RUN-TESTS
   DPR-BAD-SEED-TASK? if DPR-FINISH-NEGATIVE exit then
   DS-TEST-PASS? if DPR-LR-PASS else DPR-LR-FAIL then ;

: DPR-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DPR-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DPR-MODEL-ERROR ( -- )
   DS-MODEL-ERROR
   s" habu-stdlib-property" LR-ARM! ;

: DPR-RUN-MODEL ( -- )
   DPR-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DPR-MODEL-ERROR exit then
   MRUN-TEXT$ DPR-EVALUATE-TEXT ;

: DPR-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-OUT-TEXT!
   DPR-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DPR-EVALUATE-TEXT ;

: DPR-USAGE ( -- )
   s" usage: bench/llm/drive-property.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DPR-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DPR-USAGE then
   SCRIPT-ARGC 7 > if DPR-USAGE then
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

: DPR-MAIN ( -- )
   DPR-CONFIG
   DPR-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
