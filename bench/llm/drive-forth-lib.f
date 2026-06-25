\ drive-forth-lib.f - native Habu Forth benchmark driver library.
\
\ Load after lib/memory.f, lib/vector.f, bench/llm/drive-stdlib-lib.f,
\ bench/llm/drive-stdlib-live.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f,
\ bench/llm/attempt-solutions-lib.f, bench/llm/forth-candidate.f,
\ and bench/llm/forth-bundle.f.

0 constant DFH-FIRST-NONE
1 constant DFH-FIRST-PASS
2 constant DFH-FIRST-FAIL
3 constant DFH-FIRST-REJECT
10 constant DFH-LF
40 constant DFH-LPAREN
41 constant DFH-RPAREN
1 constant DFH-PAREN-EDGE
2 constant DFH-PAREN-PAIR
64 constant DFH-USAGE-RC
66 constant DFH-DATA-RC

create DFH-REF-PATH FS-PATH-CAP allot

variable DFH-REF-U
variable DFH-TASK-P
variable DFH-TASK-CAP-U
variable DFH-TASK-U
variable DFH-FEEDBACK-A
variable DFH-FEEDBACK-U
variable DFH-ARM-A
variable DFH-ARM-U
variable DFH-ROUND
variable DFH-FIRST-KIND
variable DFH-TRUST-USES
variable DFH-TEST-KIND
variable DFH-TEST-CODE
variable DFH-BUNDLE-A
variable DFH-BUNDLE-CAP
variable DFH-BUNDLE-U
variable DFH-SCRATCH-A
variable DFH-SCRATCH-CAP

: DFH-FEEDBACK! ( ptr u8 n -- )
   DFH-FEEDBACK-A DFH-FEEDBACK-U DS-SET$ ;

: DFH-ARM! ( ptr u8 n -- )
   DFH-ARM-A DFH-ARM-U DS-SET$ ;

TRUSTED: DFH-FEEDBACK$ ( -- ptr u8 n )
   DFH-FEEDBACK-A @ DFH-FEEDBACK-U @ ;

TRUSTED: DFH-ARM$ ( -- ptr u8 n )
   DFH-ARM-A @ DFH-ARM-U @ ;

: DFH-REF$ ( -- ptr u8 n )
   DFH-REF-PATH DFH-REF-U @ ;

TRUSTED: DFH-TASK-BUF ( -- ptr u8 )
   DFH-TASK-P @ ;

: DFH-TASK-CAP ( -- n )
   DFH-TASK-CAP-U @ ;

: DFH-TASKS$ ( -- ptr u8 n )
   DFH-TASK-BUF DFH-TASK-U @ ;

TRUSTED: DFH-BUNDLE-BUF$ ( -- ptr u8 n )
   DFH-BUNDLE-A @ DFH-BUNDLE-CAP @ ;

TRUSTED: DFH-SCRATCH-BUF$ ( -- ptr u8 n )
   DFH-SCRATCH-A @ DFH-SCRATCH-CAP @ ;

TRUSTED: DFH-BUNDLE$ ( -- ptr u8 n )
   DFH-BUNDLE-A @ DFH-BUNDLE-U @ ;

: DFH-REPAIR? ( -- bool )
   DFH-FEEDBACK$ s" repair" STR= ;

: DFH-RAW? ( -- bool )
   DFH-FEEDBACK$ s" raw" STR= ;

: DFH-BLIND? ( -- bool )
   DFH-FEEDBACK$ s" blind" STR= ;

: DFH-FEEDBACK-VALID? ( -- bool )
   DFH-REPAIR? if DS-TRUE exit then
   DFH-RAW? if DS-TRUE exit then
   DFH-BLIND? ;

: DFH-DEFAULT-ARM$ ( -- ptr u8 n )
   DFH-REPAIR? if s" habu-forth" exit then
   DFH-RAW? if s" habu-forth-raw" exit then
   s" habu-forth-blind" ;

: DFH-LR-ARM! ( -- )
   DFH-ARM$ LR-ARM! ;

: DFH-FIRST! ( n -- ) {: kind :}
   DFH-FIRST-KIND @ DFH-FIRST-NONE = if kind DFH-FIRST-KIND ! then ;

: DFH-REPAIR-ROUNDS ( -- n )
   DFH-ROUND @ 0 > if DFH-ROUND @ 1- exit then
   0 ;

: DFH-APPLY-FIRST ( -- )
   DFH-FIRST-KIND @ DFH-FIRST-REJECT = if
      s" rejected" LR-FIRST-CHECKER!
      0 LR-FIRST-PASS !
      0 LR-FIRST-TESTS !
      exit
   then
   DFH-FIRST-KIND @ DFH-FIRST-FAIL = if
      s" certified" LR-FIRST-CHECKER!
      -1 LR-FIRST-PASS !
      0 LR-FIRST-TESTS !
      exit
   then
   DFH-FIRST-KIND @ DFH-FIRST-PASS = if
      s" certified" LR-FIRST-CHECKER!
      -1 LR-FIRST-PASS !
      -1 LR-FIRST-TESTS !
   then ;

: DFH-APPLY-ROW-STATS ( -- )
   DFH-ROUND @ LR-ROUNDS !
   DFH-REPAIR-ROUNDS LR-REPAIR-ITERATIONS !
   DFH-ROUND @ LR-CHECKER-ITERATIONS !
   DS-DIAG-COUNT @ LR-DIAG-COUNT !
   DFH-TRUST-USES @ LR-TRUST-USES !
   DFH-APPLY-FIRST
   DFH-LR-ARM! ;

: DFH-LR-REJECT ( ptr u8 n -- )
   DFH-FIRST-REJECT DFH-FIRST!
   DS-LR-REJECT
   0 LR-ALL-ERRORS-STABLE !
   DFH-APPLY-ROW-STATS ;

: DFH-LR-PASS ( -- )
   DFH-FIRST-PASS DFH-FIRST!
   DS-LR-PASS
   DFH-APPLY-ROW-STATS ;

: DFH-LR-FAIL ( -- )
   DFH-FIRST-FAIL DFH-FIRST!
   DS-LR-FAIL
   DFH-APPLY-ROW-STATS ;

: DFH-LR-CERTIFIED-OUTCOME ( ptr u8 n -- )
   DFH-FIRST-FAIL DFH-FIRST!
   DS-LR-FAIL
   DFH-LR-ARM!
   LR-OUTCOME!
   DFH-APPLY-ROW-STATS ;

: DFH-PATHS! ( -- )
   s" ref" DFH-REF-PATH DFH-REF-U DS-JOIN! ;

: DFH-ID$ ( -- ptr u8 n )
   DS-MSG-RESET
   DS-ID @ DS-U+
   DS-MSG$ ;

: DFH-MIN-ONE ( n -- n )
   dup 1 < if drop 1 then ;

: DFH-STORE-TASK-SPAN ( ptr u8 n -- )
   DFH-TASK-CAP-U ! DFH-TASK-P ! ;

: DFH-ENSURE-TASK-CAP ( n -- ) {: need :}
   need DFH-MIN-ONE DFH-TASK-CAP <= if exit then
   need DFH-MIN-ONE MEM-ALLOC-64K-SPAN DFH-STORE-TASK-SPAN ;

: DFH-COPY-TASKS ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-DS-CAPACITY throw then
   u DFH-ENSURE-TASK-CAP
   a DFH-TASK-BUF u BYTE-COPY
   u DFH-TASK-U ! ;

: DFH-PREPARE-REFERENCES ( -- )
   s" bench/llm/tasks.tsv" FTL-FILE$ DFH-COPY-TASKS
   s" bench/llm/tasks.tsv" s" bench/llm/solutions.f" DFH-REF$ AS-EXTRACT-FILES ;

: DFH-ENSURE-BUNDLE ( n -- ) {: need :}
   need 0 <= if E-DS-CAPACITY throw then
   DFH-BUNDLE-CAP @ need < if
      need MEM-ALLOC-BYTES DFH-BUNDLE-CAP ! DFH-BUNDLE-A !
   then ;

: DFH-ENSURE-SCRATCH ( n -- ) {: need :}
   need 0 <= if E-DS-CAPACITY throw then
   DFH-SCRATCH-CAP @ need < if
      need MEM-ALLOC-BYTES DFH-SCRATCH-CAP ! DFH-SCRATCH-A !
   then ;

: DFH-ENSURE-BUFFERS ( n n -- ) {: total maxfile :}
   total DFH-ENSURE-BUNDLE
   maxfile DFH-ENSURE-SCRATCH ;

: DFH-BUNDLE-LIMITS ( -- n n )
   DFH-TASKS$ DFH-REF$ DFH-ID$ DS-CAND-PATH$ s" bench/llm/tests.f" FB-BUNDLE-LIMITS ;

: DFH-BUILD-BUNDLE ( -- )
   DFH-BUNDLE-LIMITS DFH-ENSURE-BUFFERS
   DFH-TASKS$ DFH-REF$ DFH-ID$ DS-CAND-PATH$ s" bench/llm/tests.f"
   DFH-BUNDLE-BUF$ DFH-SCRATCH-BUF$ FB-BUILD-BUNDLE-INTO DFH-BUNDLE-U !
   DS-BUNDLE-PATH$ DFH-BUNDLE$ WRITE-ALL ;

: DFH-HB-STDIN-CAPTURE ( ptr u8 n -- ) {: in:ptr inu :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN in inu >LEN DS-OUT-BUF DS-OUT-CAP >LEN
   DS-ERR-BUF DS-ERR-CAP >LEN DS-RUN-TIMEOUT >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: outu erru kind code :}
   code DFH-TEST-CODE !
   kind DFH-TEST-KIND !
   erru LEN>N DS-ERR-U !
   outu LEN>N DS-OUT-U !
   kind code PROC-OUTCOME>RC RC>N DS-RC ! ;

: DFH-CHECK-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/diag-origin-core.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-only-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/checked-boundary-lint-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
   s" --all-errors"  >LEN PROC-ARGV+
   DS-CAND-PATH$  >LEN PROC-ARGV+ ;

: DFH-RUN-CHECK ( -- )
   DFH-CHECK-ARGV
   DS-HB-CAPTURE ;

: DFH-FINISH-REJECT ( -- )
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   DS-RUN-REPAIR
   s" reject" DFH-LR-REJECT ;

: DFH-RUN-TESTS ( -- )
   DFH-BUNDLE$ DFH-HB-STDIN-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DFH-TEST-PASS? ( -- bool )
   DS-RC @ 0 <> if DS-FALSE exit then
   DS-OUT-BUF DS-OUT-U @ TRIM s" ok" STR= ;

: DFH-FINISH-TESTS ( -- )
   DFH-RUN-TESTS
   DFH-TEST-KIND @ PROC-OUTCOME-TIMEOUT = if s" timeout" DFH-LR-CERTIFIED-OUTCOME exit then
   DFH-TEST-KIND @ PROC-OUTCOME-SIGNAL = if s" trap" DFH-LR-CERTIFIED-OUTCOME exit then
   DFH-TEST-KIND @ PROC-OUTCOME-EXIT <> if s" error" DFH-LR-CERTIFIED-OUTCOME exit then
   DFH-TEST-PASS? if DFH-LR-PASS else DFH-LR-FAIL then ;

: DFH-EXTRACT-CANDIDATE ( ptr u8 n -- ) {: a:ptr u :}
   DS-CAND-RESET
   a u DS-CAND-BUF DS-CAND-CAP FC-EXTRACT-CANDIDATE if
      DS-CAND-U !
      exit
   then
   drop
   s" \ no candidate extracted" DS-CAND-LN ;

: DFH-CAND-NAME-OK? ( -- bool )
   DS-CAND$ FC-FIRST-NAME$ if
      DS-NAME$ STR=
      exit
   then
   2drop
   DS-FALSE ;

: DFH-CAND-VALID? ( -- bool )
   DS-CAND$ FC-COMPLETE? 0= if DS-FALSE exit then
   DFH-CAND-NAME-OK? 0= if DS-FALSE exit then
   DS-CAND$ FC-FORBIDDEN? if DS-FALSE exit then
   DS-TRUE ;

: DFH-INVALID-CANDIDATE ( -- )
   DS-CAND$ FC-COMPLETE? 0= if
      s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
      s" {}" DS-REPAIR-PATH$ 2swap WRITE-ALL
      s" reject" DFH-LR-REJECT
      exit
   then
   DFH-CAND-NAME-OK? 0= if
      s" wrong or missing public task definition name" DS-WRITE-INVALID-DIAG
      s" {}" DS-REPAIR-PATH$ 2swap WRITE-ALL
      s" reject" DFH-LR-REJECT
      exit
   then
   s" forbidden unchecked boundary" DS-WRITE-INVALID-DIAG
   1 DFH-TRUST-USES !
   s" {}" DS-REPAIR-PATH$ 2swap WRITE-ALL
   s" reject" DFH-LR-REJECT ;

: DFH-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DFH-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DFH-BUILD-BUNDLE
   DFH-CAND-VALID? 0= if DFH-INVALID-CANDIDATE exit then
   DFH-RUN-CHECK
   DS-CHECK-CLEAN? 0= if DFH-FINISH-REJECT exit then
   DS-DIAG-COUNT @ 0= if
      DS-DIAG-PATH$ s" " WRITE-ALL
      DS-REPAIR-PATH$ s" {}" WRITE-ALL
   then
   DFH-FINISH-TESTS ;

: DFH-STATE-RESET ( -- )
   0 DS-TOKENS !
   0 DS-DIAG-COUNT !
   0 DFH-ROUND !
   0 DFH-TRUST-USES !
   PROC-OUTCOME-EXIT DFH-TEST-KIND !
   0 DFH-TEST-CODE !
   DFH-FIRST-NONE DFH-FIRST-KIND ! ;

: DFH-NEXT-ROUND ( -- )
   DFH-ROUND @ 1+ DFH-ROUND ! ;

: DFH-PROMPT-CASES ( -- )
   0 DS-LINE-NEXT !
   begin DS-TESTS$ DS-LINE-NEXT @ BM-LINE-NEXT while
      DS-LINE-NEXT !
      2dup TRIM nip 0 > if DS-PROMPT-LN else 2drop then
   repeat
   drop 2drop ;

: DFH-SIG-PAREN? ( ptr u8 n -- bool ) {: a:ptr u :}
   u DFH-PAREN-PAIR < if DS-FALSE exit then
   a c@ DFH-LPAREN =
   a u DFH-PAREN-EDGE - + c@ DFH-RPAREN =
   and ;

: DFH-SIG-STRIP-PARENS ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u DFH-SIG-PAREN? if
      a DFH-PAREN-EDGE + u DFH-PAREN-PAIR - TRIM exit
   then
   a u ;

: DFH-SIG-INNER$ ( -- ptr u8 n )
   DS-SIG$ TRIM DFH-SIG-STRIP-PARENS ;

: DFH-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   s" Define exactly one checked Habu Forth word:" DS-PROMPT-LN
   s"   : " DS-PROMPT+
   DS-NAME$ DS-PROMPT+
   s"  ( " DS-PROMPT+
   DFH-SIG-INNER$ DS-PROMPT+
   s"  ) ... ;" DS-PROMPT-LN
   s" " DS-PROMPT-LN
   DS-SPEC$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Expected examples:" DS-PROMPT-LN
   DFH-PROMPT-CASES
   s" " DS-PROMPT-LN
   s" Rules:" DS-PROMPT-LN
   s" - Output only the definition, no markdown or prose." DS-PROMPT-LN
   s" - Keep the word name and stack effect exactly as shown." DS-PROMPT-LN
   s" - Use checked Forth; do not use TRUST, trust, or set-check." DS-PROMPT-LN
   s" - Project words are UPPER-CASE; built-in words stay lower-case." DS-PROMPT-LN ;

: DFH-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DFH-PATHS!
   DFH-PREPARE-REFERENCES
   DFH-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DFH-PROMPT-FILE+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu DS-OUT-BUF DS-OUT-CAP READ-ALL DS-OUT-U !
   DS-OUT-BUF DS-OUT-U @ DS-PROMPT-LN ;

: DFH-REJECT-FEEDBACK-REPAIR ( -- )
   s" " DS-PROMPT-LN
   s" The checker rejected the previous candidate. Use this repair packet:" DS-PROMPT-LN
   DS-REPAIR-PATH$ DFH-PROMPT-FILE+
   s" Fix it so it certifies. Output only corrected Habu code." DS-PROMPT-LN ;

: DFH-REJECT-FEEDBACK-RAW ( -- )
   s" " DS-PROMPT-LN
   s" The checker rejected the previous candidate. Raw checker diagnostics:" DS-PROMPT-LN
   DS-DIAG-PATH$ DFH-PROMPT-FILE+
   s" Fix it so it certifies. Output only corrected Habu code." DS-PROMPT-LN ;

: DFH-REJECT-FEEDBACK-BLIND ( -- )
   s" " DS-PROMPT-LN
   s" Your attempt did not certify against the declared stack effect." DS-PROMPT-LN
   s" Fix the body. Output only the corrected definition." DS-PROMPT-LN ;

: DFH-REJECT-FEEDBACK ( -- )
   DFH-REPAIR? if DFH-REJECT-FEEDBACK-REPAIR exit then
   DFH-RAW? if DFH-REJECT-FEEDBACK-RAW exit then
   DFH-REJECT-FEEDBACK-BLIND ;

: DFH-FAIL-FEEDBACK ( -- )
   s" " DS-PROMPT-LN
   DFH-BLIND? if
      s" Your attempt certified, but failed the benchmark tests. Fix the logic. Output only the corrected definition." DS-PROMPT-LN
      exit
   then
   s" The previous candidate certified but failed the benchmark tests. Test output:" DS-PROMPT-LN
   DS-TEST-PATH$ DFH-PROMPT-FILE+
   s" Expected examples:" DS-PROMPT-LN
   DS-TESTS$ DS-PROMPT-LN
   s" Fix the logic. Output only corrected Habu code." DS-PROMPT-LN ;

: DFH-ADD-FEEDBACK ( -- )
   LR-OUTCOME$ s" reject" STR= if DFH-REJECT-FEEDBACK exit then
   DFH-FAIL-FEEDBACK ;

: DFH-OUTCOME= ( ptr u8 n -- bool ) {: a:ptr u :}
   LR-OUTCOME$ a u STR= ;

: DFH-DONE? ( -- bool )
   s" pass" DFH-OUTCOME= if DS-TRUE exit then
   s" error" DFH-OUTCOME= ;

: DFH-MAX-ROUNDS ( -- n )
   DS-MAX-REPAIRS @ 0 > if DS-MAX-REPAIRS @ exit then
   1 ;

: DFH-MODEL-ERROR ( -- )
   DFH-FIRST-REJECT DFH-FIRST!
   1 DS-DIAG-COUNT !
   DS-MODEL-ERROR
   DFH-APPLY-ROW-STATS ;

: DFH-RUN-MODEL-ROUND ( -- )
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   DS-TOKENS @ MRUN-TOKENS @ + DS-TOKENS !
   MRUN-RC @ 0= 0= if DFH-MODEL-ERROR exit then
   MRUN-TEXT$ DFH-EVALUATE-TEXT ;

: DFH-RUN-MODEL ( -- )
   DFH-PREPARE
   DFH-STATE-RESET
   begin DFH-ROUND @ DFH-MAX-ROUNDS < while
      DFH-NEXT-ROUND
      DFH-RUN-MODEL-ROUND
      DFH-DONE? if exit then
      DFH-ROUND @ DFH-MAX-ROUNDS >= if exit then
      DFH-ADD-FEEDBACK
   repeat ;

: DFH-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DFH-PREPARE
   DFH-STATE-RESET
   DFH-NEXT-ROUND
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DFH-EVALUATE-TEXT ;

: DFH-CLI-MAX-REPAIRS ( -- n )
   SCRIPT-ARGC 6 > if 6 SCRIPT-ARGV$ DS-PARSE-U exit then
   s" BENCH_MAX_REPAIRS" 5 DS-ENV-U ;

: DFH-USAGE ( -- )
   s" usage: bench/llm/drive-forth.f <id> <name> <sig> <category> <tests> <spec> [maxr]" DFH-USAGE-RC die ;

: DFH-CONFIG-FEEDBACK ( -- )
   s" BENCH_FORTH_FEEDBACK" s" repair" DS-ENV$ DFH-FEEDBACK!
   DFH-FEEDBACK-VALID? 0= if DFH-USAGE then
   s" BENCH_FORTH_ARM" GETENV dup 0= if
      2drop DFH-DEFAULT-ARM$
   then
   DFH-ARM! ;

: DFH-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DFH-USAGE then
   SCRIPT-ARGC 7 > if DFH-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   3 SCRIPT-ARGV$ DS-CATEGORY!
   4 SCRIPT-ARGV$ DS-TESTS!
   5 SCRIPT-ARGV$ DS-SPEC!
   DS-DEFAULTS
   DFH-CLI-MAX-REPAIRS DS-MAX-REPAIRS !
   DFH-CONFIG-FEEDBACK
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DFH-MAIN ( -- )
   DFH-CONFIG
   DFH-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
