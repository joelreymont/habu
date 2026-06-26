\ drive-aot-lib.f - native stripped-AOT benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f, bench/llm/drive-stdlib-live.f,
\ and bench/llm/driver-token-helpers.f.

120000 constant DA-TIMEOUT-MS

create DA-TMP-PATH FS-PATH-CAP allot
create DA-BIN-PATH FS-PATH-CAP allot

variable DA-TMP-U
variable DA-BIN-U

: DA-TMP$ ( -- ptr u8 n )
   DA-TMP-PATH DA-TMP-U @ ;

: DA-BIN$ ( -- ptr u8 n )
   DA-BIN-PATH DA-BIN-U @ ;

: DA-PATHS! ( -- )
   s" aot-tmp" DA-TMP-PATH DA-TMP-U DS-JOIN!
   s" aot-bin" DA-BIN-PATH DA-BIN-U DS-JOIN!
   DA-TMP$ MAKE-DIR ;

: DA-NEGATIVE? ( -- bool )
   DS-CATEGORY$ s" aot-unsupported" STR= ;

: DA-ALLOT-TASK? ( -- bool )
   DS-NAME$ s" AOT-UNSAFE-ALLOT" STR= ;

: DA-EXPECTED-TOKEN$ ( -- ptr u8 n )
   DA-ALLOT-TASK? if s" allot" exit then
   s" here" ;

: DA-EXPECTED-OUT$ ( -- ptr u8 n )
   SB-RESET
   DS-NAME$ s" AOT-MAIN-STRING" STR= if s" 50" else s" 42" then SB-APPEND
   DS-LF SB-APPEND-C
   SB$ ;

: DA-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   s" You write Habu, a checked Forth. Return checked source only." DS-PROMPT-LN
   s" Define the executable entrypoint exactly as:" DS-PROMPT-LN
   s" : MAIN ( -- ) ... ;" DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Task:" DS-PROMPT-LN
   DS-SPEC$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Expected AOT behavior:" DS-PROMPT-LN
   DS-TESTS$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" The driver builds your source with native tools/hb-build.f into a stripped AOT executable." DS-PROMPT-LN
   s" For stdout tasks, MAIN must print the requested value with `.` and should not add an extra CR after `.`." DS-PROMPT-LN
   s" For reject tasks, MAIN must use the requested unsupported token so hb-build reports E-AOT-UNSUPPORTED." DS-PROMPT-LN
   s" Do not use TRUST, TRUSTED:, trust, set-check, evaluate, process helpers, or script argv." DS-PROMPT-LN ;

: DA-DEF-NEEDLE$ ( -- ptr u8 n )
   s" : MAIN" ;

: DA-PUBLIC-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u DA-DEF-NEEDLE$ CONTAINS? ;

: DA-EXTRACT-LINE ( ptr u8 n -- ) {: a:ptr u :}
   DS-EXTRACT-DONE @ if exit then
   DS-EXTRACT-STARTED @ 0= if
      a u DS-CODE-LINE? 0= if exit then
      -1 DS-EXTRACT-STARTED !
   then
   a u DS-EXTRACT-APPEND
   a u DA-PUBLIC-LINE? if -1 DS-EXTRACT-SEEN ! then
   DS-EXTRACT-SEEN @ if
      a u DS-LINE-SEMI? if -1 DS-EXTRACT-DONE ! then
   then ;

: DA-EXTRACT-CANDIDATE ( ptr u8 n -- ) {: a:ptr u :}
   DS-EXTRACT-RESET
   begin
      a u DS-LINE-NEXT @ BM-LINE-NEXT
   while
      DS-LINE-NEXT !
      DS-LINE!
      DS-LINE$ DA-EXTRACT-LINE
   repeat
   drop 2drop
   DS-CAND-U @ 0= if s" \ no candidate extracted" DS-CAND-LN then ;

: DA-CAND-HAS-MAIN? ( -- bool )
   DS-CAND$ DA-DEF-NEEDLE$ CONTAINS? ;

: DA-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? ;

: DA-CAND-USES-EXPECTED? ( -- bool )
   DA-NEGATIVE? if DA-EXPECTED-TOKEN$ DS-CAND-HAS-WORD? exit then
   DS-NAME$ s" AOT-MAIN-ARITH" STR= if
      s" 6" DS-CAND-HAS-WORD?
      s" 7" DS-CAND-HAS-WORD? and
      s" *" DS-CAND-HAS-WORD? and exit
   then
   DS-TRUE ;

: DA-CAND-VALID? ( -- bool )
   DA-CAND-HAS-MAIN? 0= if DS-FALSE exit then
   DA-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DA-CAND-USES-EXPECTED? ;

: DA-LR-REJECT ( ptr u8 n -- )
   DS-LR-REJECT
   s" habu-aot" LR-ARM!
   s" aot_rejection" LR-REPAIR-CLASS! ;

: DA-LR-PASS ( -- )
   DS-LR-PASS
   s" habu-aot" LR-ARM! ;

: DA-LR-FAIL ( -- )
   DS-LR-FAIL
   s" habu-aot" LR-ARM! ;

: DA-INVALID-CANDIDATE ( -- )
   DA-CAND-HAS-MAIN? 0= if
      s" missing MAIN definition" DS-WRITE-INVALID-DIAG
      s" reject" DA-LR-REJECT
      exit
   then
   DA-CAND-FORBIDDEN? if
      s" forbidden AOT benchmark boundary" DS-WRITE-INVALID-DIAG
      s" reject" DA-LR-REJECT
      exit
   then
   DS-CAND-COMPLETE? if
      s" required AOT token missing" DS-WRITE-INVALID-DIAG
      s" reject" DA-LR-REJECT
      exit
   then
   s" incomplete MAIN definition" DS-WRITE-INVALID-DIAG
   s" reject" DA-LR-REJECT ;

: DA-ADD-HB-BUILD-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+ ;

: DA-HB-BUILD-ARGS ( bool -- ) {: json? :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN DA-TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   DA-ADD-HB-BUILD-LOADS
   s" --"  >LEN PROC-ARGV+
   s" --strict-signatures"  >LEN PROC-ARGV+
   json? if s" --json-errors"  >LEN PROC-ARGV+ then
   DS-CAND-PATH$  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   DA-BIN$  >LEN PROC-ARGV+ ;

: DA-HB-BUILD-CAPTURE ( -- )
   s" bin/hb" >LEN DS-OUT-BUF DS-OUT-CAP >LEN
   DS-ERR-BUF DS-ERR-CAP >LEN DA-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   rc RC>N DS-RC !
   erru LEN>N DS-ERR-U !
   outu LEN>N DS-OUT-U ! ;

: DA-RUN-HB-BUILD ( bool -- )
   DA-HB-BUILD-ARGS
   DA-HB-BUILD-CAPTURE ;

: DA-CAPTURE-HAS? ( ptr u8 n -- bool ) {: a:ptr u :}
   DS-OUT-BUF DS-OUT-U @ a u CONTAINS? if DS-TRUE exit then
   DS-ERR-BUF DS-ERR-U @ a u CONTAINS? ;

: DA-WRITE-DIAG ( -- )
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DA-WRITE-TEST ( -- )
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DA-DIAG-EXPECTED? ( -- bool )
   s" E-AOT-UNSUPPORTED" DA-CAPTURE-HAS? 0= if DS-FALSE exit then
   DA-EXPECTED-TOKEN$ DA-CAPTURE-HAS? ;

: DA-RUN-BINARY ( -- )
   DA-BIN$ >LEN DS-OUT-BUF DS-OUT-CAP >LEN
   DS-ERR-BUF DS-ERR-CAP >LEN DA-TIMEOUT-MS >MS
   RUN-CAPTURE {: outu erru rc :}
   rc RC>N DS-RC !
   erru LEN>N DS-ERR-U !
   outu LEN>N DS-OUT-U ! ;

: DA-RUN-PASS? ( -- bool )
   DS-RC @ 0= if
      DS-ERR-U @ 0= if
         DS-OUT-BUF DS-OUT-U @ DA-EXPECTED-OUT$ STR= exit
      then
   then
   DS-FALSE ;

: DA-FINISH-POSITIVE ( -- )
   DS-FALSE DA-RUN-HB-BUILD
   DS-RC @ 0 <> if DA-WRITE-DIAG s" reject" DA-LR-REJECT exit then
   DA-RUN-BINARY
   DA-WRITE-TEST
   DA-RUN-PASS? if DA-LR-PASS else DA-LR-FAIL then ;

: DA-LR-NEGATIVE-CORRECT ( -- )
   DA-WRITE-DIAG
   s" reject" DA-LR-REJECT
   s" aot_unsupported" LR-REPAIR-CLASS! ;

: DA-LR-NEGATIVE-UNEXPECTED ( -- )
   DA-LR-FAIL
   1 LR-DIAG-COUNT !
   s" aot_unexpected_success" LR-REPAIR-CLASS! ;

: DA-FINISH-NEGATIVE ( -- )
   DS-TRUE DA-RUN-HB-BUILD
   DS-RC @ 0= if
      DS-DIAG-PATH$ s" AOT build unexpectedly succeeded" WRITE-ALL
      1 DS-DIAG-COUNT !
      DA-LR-NEGATIVE-UNEXPECTED
      exit
   then
   DA-DIAG-EXPECTED? if DA-LR-NEGATIVE-CORRECT exit then
   DA-WRITE-DIAG
   DA-LR-FAIL ;

: DA-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DA-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DS-BUNDLE-PATH$ DS-CAND$ WRITE-ALL
   DA-CAND-VALID? 0= if DA-INVALID-CANDIDATE exit then
   DA-NEGATIVE? if DA-FINISH-NEGATIVE else DA-FINISH-POSITIVE then ;

: DA-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DA-PATHS!
   DA-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DA-MODEL-ERROR ( -- )
   DS-MODEL-ERROR
   s" habu-aot" LR-ARM! ;

: DA-RUN-MODEL ( -- )
   DA-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DA-MODEL-ERROR exit then
   MRUN-TEXT$ DA-EVALUATE-TEXT ;

: DA-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-OUT-TEXT!
   DA-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DA-EVALUATE-TEXT ;

: DA-USAGE ( -- )
   s" usage: bench/llm/drive-aot.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DA-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DA-USAGE then
   SCRIPT-ARGC 7 > if DA-USAGE then
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

: DA-MAIN ( -- )
   DA-CONFIG
   DA-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
