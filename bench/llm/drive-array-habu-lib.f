\ drive-array-habu-lib.f - native Habu array benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f and
\ bench/llm/driver-token-helpers.f.

5 constant DAH-GRADE-TIMEOUT-S
58 constant DAH-COLON
16 constant DAH-COUNT-KEY-LEN
1000000 constant DAH-NS-PER-MS
0 constant DAH-FIRST-NONE
1 constant DAH-FIRST-PASS
2 constant DAH-FIRST-FAIL
3 constant DAH-FIRST-REJECT

create DAH-VEC-PATH FS-PATH-CAP allot
create DAH-SCRIPT-PATH FS-PATH-CAP allot
create DAH-RUN-BUNDLE-PATH FS-PATH-CAP allot

variable DAH-VEC-U
variable DAH-SCRIPT-U
variable DAH-RUN-BUNDLE-U
variable DAH-CONV-A
variable DAH-CONV-U
variable DAH-VECTORS-A
variable DAH-VECTORS-U
variable DAH-ARM-A
variable DAH-ARM-U
variable DAH-BODY-N
variable DAH-AFTER-KEY
variable DAH-AFTER-COLON
variable DAH-START
variable DAH-STOP
variable DAH-ROUND
variable DAH-FIRST-KIND

: DAH-CONV! ( ptr u8 n -- )
   DAH-CONV-A DAH-CONV-U DS-SET$ ;

: DAH-VECTORS! ( ptr u8 n -- )
   DAH-VECTORS-A DAH-VECTORS-U DS-SET$ ;

: DAH-ARM! ( ptr u8 n -- )
   DAH-ARM-A DAH-ARM-U DS-SET$ ;

TRUSTED: DAH-CONV$ ( -- ptr u8 n )
   DAH-CONV-A @ DAH-CONV-U @ ;

TRUSTED: DAH-VECTORS$ ( -- ptr u8 n )
   DAH-VECTORS-A @ DAH-VECTORS-U @ ;

TRUSTED: DAH-ARM$ ( -- ptr u8 n )
   DAH-ARM-A @ DAH-ARM-U @ ;

: DAH-VEC$ ( -- ptr u8 n )
   DAH-VEC-PATH DAH-VEC-U @ ;

: DAH-SCRIPT$ ( -- ptr u8 n )
   DAH-SCRIPT-PATH DAH-SCRIPT-U @ ;

: DAH-RUN-BUNDLE$ ( -- ptr u8 n )
   DAH-RUN-BUNDLE-PATH DAH-RUN-BUNDLE-U @ ;

: DAH-RAW? ( -- bool )
   DAH-ARM$ s" a" STR= ;

: DAH-LIB? ( -- bool )
   DAH-ARM$ s" lib" STR= ;

: DAH-STDLIB? ( -- bool )
   DAH-ARM$ s" stdlib" STR= ;

: DAH-SKELETON? ( -- bool )
   DAH-ARM$ s" skeleton" STR= ;

: DAH-BUNDLED? ( -- bool )
   DAH-LIB? if DS-TRUE exit then
   DAH-STDLIB? ;

: DAH-CHECK-SOURCE$ ( -- ptr u8 n )
   DAH-BUNDLED? if DAH-RUN-BUNDLE$ exit then
   DS-BUNDLE-PATH$ ;

: DAH-CHECK-SOURCE-LIST ( -- )
   s" --source-list" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/array.f" PROC-ARGV+
   DAH-STDLIB? if
      s" lib/string.f" PROC-ARGV+
      s" lib/map.f" PROC-ARGV+
      s" lib/fs.f" PROC-ARGV+
      s" lib/argv.f" PROC-ARGV+
      s" lib/test.f" PROC-ARGV+
      s" lib/time.f" PROC-ARGV+
      s" lib/date.f" PROC-ARGV+
   then
   DAH-SCRIPT$ PROC-ARGV+ ;

: DAH-CHECK-SOURCES ( -- )
   DAH-BUNDLED? if DAH-CHECK-SOURCE-LIST exit then
   DAH-CHECK-SOURCE$ PROC-ARGV+ ;

: DAH-ARM-VALID? ( -- bool )
   DAH-RAW? if DS-TRUE exit then
   DAH-LIB? if DS-TRUE exit then
   DAH-STDLIB? if DS-TRUE exit then
   DAH-SKELETON? ;

: DAH-LR-ARM! ( -- )
   DAH-RAW? if s" habu-a" LR-ARM! exit then
   DAH-LIB? if s" habu-lib" LR-ARM! exit then
   DAH-STDLIB? if s" habu-stdlib" LR-ARM! exit then
   s" habu-skeleton" LR-ARM! ;

: DAH-FIRST! ( n -- ) {: kind :}
   DAH-FIRST-KIND @ DAH-FIRST-NONE = if kind DAH-FIRST-KIND ! then ;

: DAH-REPAIR-ROUNDS ( -- n )
   DAH-ROUND @ 0 > if DAH-ROUND @ 1- exit then
   0 ;

: DAH-APPLY-FIRST ( -- )
   DAH-FIRST-KIND @ DAH-FIRST-REJECT = if
      s" rejected" LR-FIRST-CHECKER!
      0 LR-FIRST-PASS !
      0 LR-FIRST-TESTS !
      exit
   then
   DAH-FIRST-KIND @ DAH-FIRST-FAIL = if
      s" certified" LR-FIRST-CHECKER!
      -1 LR-FIRST-PASS !
      0 LR-FIRST-TESTS !
      exit
   then
   DAH-FIRST-KIND @ DAH-FIRST-PASS = if
      s" certified" LR-FIRST-CHECKER!
      -1 LR-FIRST-PASS !
      -1 LR-FIRST-TESTS !
   then ;

: DAH-APPLY-ROW-STATS ( -- )
   DAH-ROUND @ LR-ROUNDS !
   DAH-REPAIR-ROUNDS LR-REPAIR-ITERATIONS !
   DAH-ROUND @ LR-CHECKER-ITERATIONS !
   DS-DIAG-COUNT @ LR-DIAG-COUNT !
   DAH-APPLY-FIRST ;

: DAH-LR-REJECT ( ptr u8 n -- )
   DAH-FIRST-REJECT DAH-FIRST!
   DS-LR-REJECT
   DAH-LR-ARM!
   0 LR-ALL-ERRORS-STABLE !
   DAH-APPLY-ROW-STATS ;

: DAH-LR-PASS ( -- )
   DAH-FIRST-PASS DAH-FIRST!
   DS-LR-PASS
   DAH-LR-ARM!
   DAH-APPLY-ROW-STATS ;

: DAH-LR-CERTIFIED-OUTCOME ( ptr u8 n -- ) {: out:ptr outu :}
   DAH-FIRST-FAIL DAH-FIRST!
   DS-LR-FAIL
   DAH-LR-ARM!
   out outu LR-OUTCOME!
   DAH-APPLY-ROW-STATS ;

: DAH-PATHS! ( -- )
   s" vectors.f" DAH-VEC-PATH DAH-VEC-U DS-JOIN!
   s" array-script.f" DAH-SCRIPT-PATH DAH-SCRIPT-U DS-JOIN!
   s" run-bundle.f" DAH-RUN-BUNDLE-PATH DAH-RUN-BUNDLE-U DS-JOIN! ;

: DAH-MODE-TEXT ( -- )
   DAH-CONV$ s" as" STR= if
      s" For this task return one integer result." DS-PROMPT-LN
      exit
   then
   s" For this task modify the array in place and return nothing." DS-PROMPT-LN ;

: DAH-PROMPT-LIBS ( -- )
   DAH-LIB? if
      s" The driver preloads lib/errors.f, lib/array.f, and the benchmark array shim." DS-PROMPT-LN
      s" Use A@, A!, A+!, A-SWAP, LAST-INDEX, MIRROR-INDEX, EVEN?, or whole-array A-* helpers where they fit." DS-PROMPT-LN
      exit
   then
   DAH-STDLIB? if
      s" The driver preloads the checked public stdlib including lib/array.f." DS-PROMPT-LN
      s" Prefer direct array helpers such as A-SUM, A-MAX, A-ARGMAX, A-REVERSE!, A-SCAN1!, A-MAP!, and A-FIND-INDEX." DS-PROMPT-LN
      exit
   then
   DAH-SKELETON? if
      s" Complete only the definition body. The driver wraps it with locals {: arr:ptr len :}." DS-PROMPT-LN
   then ;

: DAH-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   s" You write Habu, a checked Forth. Return checked source only." DS-PROMPT-LN
   DAH-SKELETON? if
      s" Complete this checked definition skeleton:" DS-PROMPT-LN
      s" : " DS-PROMPT+
      DS-NAME$ DS-PROMPT+
      s"  ( " DS-PROMPT+
      DS-SIG$ DS-PROMPT+
      s"  ) {: arr:ptr len :}" DS-PROMPT-LN
      s"   ... your body here ..." DS-PROMPT-LN
      s" ;" DS-PROMPT-LN
      s" Output only the body, without the definition header or semicolon." DS-PROMPT-LN
   else
      s" Define the public benchmark word exactly as:" DS-PROMPT-LN
      s" : " DS-PROMPT+
      DS-NAME$ DS-PROMPT+
      s"  ( " DS-PROMPT+
      DS-SIG$ DS-PROMPT+
      s"  ) ... ;" DS-PROMPT-LN
   then
   s" " DS-PROMPT-LN
   s" Task:" DS-PROMPT-LN
   DS-SPEC$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Expected array examples:" DS-PROMPT-LN
   DAH-VECTORS$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" The input array is passed as pointer and length." DS-PROMPT-LN
   DAH-MODE-TEXT
   DAH-PROMPT-LIBS
   s" Do not use TRUST, TRUSTED:, trust, set-check, evaluate, process helpers, or script argv." DS-PROMPT-LN ;

: DAH-SKELETON-HEADER ( -- )
   s" : " DS-CAND+
   DS-NAME$ DS-CAND+
   s"  ( " DS-CAND+
   DS-SIG$ DS-CAND+
   s"  ) {: arr:ptr len :}" DS-CAND-LN ;

: DAH-BODY-LINE ( ptr u8 n -- ) {: a:ptr u :}
   a u TRIM {: b:ptr v :}
   v 0= if exit then
   b v s" ```" STARTS-WITH? if exit then
   b v DS-CAND-LN
   DAH-BODY-N @ 1+ DAH-BODY-N ! ;

: DAH-WRAP-SKELETON ( ptr u8 n -- ) {: a:ptr u :}
   DS-CAND-RESET
   0 DAH-BODY-N !
   DAH-SKELETON-HEADER
   0 DS-LINE-NEXT !
   begin
      a u DS-LINE-NEXT @ BM-LINE-NEXT
   while
      DS-LINE-NEXT !
      DAH-BODY-LINE
   repeat
   drop 2drop
   DAH-BODY-N @ 0= if
      DS-CAND-RESET
      s" \ no candidate extracted" DS-CAND-LN
      exit
   then
   s" ;" DS-CAND-LN ;

: DAH-EXTRACT-CANDIDATE ( ptr u8 n -- ) {: a:ptr u :}
   DAH-SKELETON? if
      a u DS-DEF-NEEDLE$ CONTAINS? if a u DS-EXTRACT-CANDIDATE else a u DAH-WRAP-SKELETON then
      exit
   then
   a u DS-EXTRACT-CANDIDATE ;

: DAH-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? ;

: DAH-CAND-USES-ARRAY? ( -- bool )
   s" A@" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A+!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-SWAP" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" LAST-INDEX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" MIRROR-INDEX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" EVEN?" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-SUM" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-MIN" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-MAX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-COUNT-EVEN" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-ARGMAX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-MAX-INDEX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-REVERSE!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-REVERSE-RANGE!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-PREFIX-SUM!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-RUNMAX!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-MAP!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-MAPI!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-SCAN!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-SCAN1!" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-FOLD" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-FOLDI" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-FIND-INDEX" DS-CAND-HAS-WORD? if DS-TRUE exit then
   s" A-FIND-INDEXI" DS-CAND-HAS-WORD? ;

: DAH-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DAH-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DAH-BUNDLED? if DAH-CAND-USES-ARRAY? exit then
   DS-TRUE ;

: DAH-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DAH-LR-REJECT
      exit
   then
   DAH-CAND-FORBIDDEN? if
      s" forbidden Habu array benchmark boundary" DS-WRITE-INVALID-DIAG
      s" reject" DAH-LR-REJECT
      exit
   then
   DS-CAND-COMPLETE? 0= if
      s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
      s" reject" DAH-LR-REJECT
      exit
   then
   s" required array helper missing" DS-WRITE-INVALID-DIAG
   s" reject" DAH-LR-REJECT ;

: DAH-APPEND-FILE-TO ( ptr u8 n ptr u8 n -- )
   {: file:ptr fileu dst:ptr dstu :}
   file fileu DS-OUT-BUF DS-OUT-CAP READ-ALL DS-OUT-U !
   dst dstu DS-OUT-BUF DS-OUT-U @ APPEND-FILE ;

: DAH-WRITE-SCRIPT ( -- )
   DAH-SCRIPT$ s" " WRITE-ALL
   s" bench/llm/habu-array-lib.f" DAH-SCRIPT$ DAH-APPEND-FILE-TO
   DS-CAND-PATH$ DAH-SCRIPT$ DAH-APPEND-FILE-TO ;

: DAH-BUNDLE-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" tools/bundle-lib.f" PROC-ARGV+
   s" -o" PROC-ARGV+
   DAH-RUN-BUNDLE$ PROC-ARGV+
   s" errors" PROC-ARGV+
   s" array" PROC-ARGV+
   DAH-STDLIB? if
      s" string" PROC-ARGV+
      s" map" PROC-ARGV+
      s" fs" PROC-ARGV+
      s" argv" PROC-ARGV+
      s" test" PROC-ARGV+
      s" time" PROC-ARGV+
      s" date" PROC-ARGV+
   then
   s" --" PROC-ARGV+
   DAH-SCRIPT$ PROC-ARGV+ ;

: DAH-RUN-BUNDLE ( -- )
   DAH-WRITE-SCRIPT
   DAH-BUNDLE-ARGV
   DS-HB-CAPTURE
   DS-RC @ 0 <> if E-DS-CANDIDATE throw then ;

: DAH-WRITE-BUNDLE ( -- )
   DS-BUNDLE-PATH$ DS-CAND$ WRITE-ALL
   DAH-BUNDLED? if DAH-RUN-BUNDLE then ;

: DAH-WRITE-VECTORS ( -- )
   DAH-VEC$ DAH-CONV$ DS-NAME$ DAH-VECTORS$ BV-HABU-TESTS WRITE-ALL ;

: DAH-CHECK-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/source.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   DAH-CHECK-SOURCES ;

: DAH-RUN-CHECK ( -- )
   DAH-CHECK-ARGV
   DS-HB-CAPTURE ;

: DAH-FIND-CHAR-FROM ( ptr u8 n n n -- n ) {: a:ptr u start ch :}
   start begin dup u < while
      dup a + c@ ch = if exit then
      1+
   repeat
   drop
   -1 ;

: DAH-DIGIT-START ( ptr u8 n n -- n ) {: a:ptr u start :}
   start begin dup u < while
      dup a + c@ DS-DIGIT? if exit then
      1+
   repeat ;

: DAH-DIGIT-END ( ptr u8 n n -- n ) {: a:ptr u start :}
   start begin dup u < while
      dup a + c@ DS-DIGIT? 0= if exit then
      1+
   repeat ;

: DAH-PACKET-DIAG-COUNT ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" diagnostic_count" FIND-SUB dup 0 < if drop 1 exit then
   DAH-COUNT-KEY-LEN + DAH-AFTER-KEY !
   a u DAH-AFTER-KEY @ DAH-COLON DAH-FIND-CHAR-FROM dup 0 < if drop 1 exit then
   1+ DAH-AFTER-COLON !
   a u DAH-AFTER-COLON @ DAH-DIGIT-START DAH-START !
   DAH-START @ u >= if 1 exit then
   a u DAH-START @ DAH-DIGIT-END DAH-STOP !
   a DAH-START @ + DAH-STOP @ DAH-START @ - STR>NUMBER? 0= if drop 1 exit then
   dup 0 <= if drop 1 exit then ;

: DAH-REPAIR-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/repair-packet.f" PROC-ARGV+
   s" --" PROC-ARGV+
   DS-DIAG-PATH$ PROC-ARGV+ ;

: DAH-RUN-REPAIR ( -- )
   DAH-REPAIR-ARGV
   DS-HB-CAPTURE
   DS-RC @ 0= if
      DS-OUT-BUF DS-OUT-U @ DAH-PACKET-DIAG-COUNT DS-DIAG-COUNT !
      DS-REPAIR-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
      DS-TEST-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
      exit
   then
   DS-REPAIR-PATH$ s" {}" WRITE-ALL
   DS-TEST-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   1 DS-DIAG-COUNT ! ;

: DAH-FINISH-REJECT ( -- )
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   DAH-RUN-REPAIR
   s" reject" DAH-LR-REJECT ;

: DAH-ARG-U ( n -- )
   DS-MSG-RESET
   DS-U+
   DS-MSG$ PROC-ARGV+ ;

: DAH-GRADE-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" bench/llm/grade.f" PROC-ARGV+
   s" --" PROC-ARGV+
   DAH-GRADE-TIMEOUT-S DAH-ARG-U
   DAH-CHECK-SOURCE$ PROC-ARGV+
   DAH-VEC$ PROC-ARGV+ ;

: DAH-FINISH-GRADE ( -- )
   mono-ns {: t0 :}
   DAH-GRADE-ARGV
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE
   mono-ns t0 - DAH-NS-PER-MS 1- + DAH-NS-PER-MS / {: runtime-ms :}
   DS-RC @ 0 <> if s" error" DAH-LR-CERTIFIED-OUTCOME exit then
   DS-OUT-BUF DS-OUT-U @ s" pass" CONTAINS? if
      DAH-LR-PASS
      runtime-ms LR-RUNTIME-MS!
      1 0 LR-RUNTIME-COUNTS!
      s" ok" LR-RUNTIME-STATUS!
      exit
   then
   DS-OUT-BUF DS-OUT-U @ s" fail" CONTAINS? if s" fail" DAH-LR-CERTIFIED-OUTCOME exit then
   DS-OUT-BUF DS-OUT-U @ s" reject" CONTAINS? if s" reject" DAH-LR-CERTIFIED-OUTCOME exit then
   DS-OUT-BUF DS-OUT-U @ s" trap" CONTAINS? if s" trap" DAH-LR-CERTIFIED-OUTCOME exit then
   DS-OUT-BUF DS-OUT-U @ s" timeout" CONTAINS? if s" timeout" DAH-LR-CERTIFIED-OUTCOME exit then
   s" error" DAH-LR-CERTIFIED-OUTCOME ;

: DAH-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DAH-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DAH-WRITE-VECTORS
   DAH-CAND-VALID? 0= if DAH-INVALID-CANDIDATE exit then
   DAH-WRITE-BUNDLE
   DAH-RUN-CHECK
   DS-CHECK-CLEAN? 0= if DAH-FINISH-REJECT exit then
   DS-DIAG-COUNT @ 0= if
      DS-DIAG-PATH$ s" " WRITE-ALL
      DS-REPAIR-PATH$ s" {}" WRITE-ALL
   then
   DAH-FINISH-GRADE ;

: DAH-STATE-RESET ( -- )
   0 DS-TOKENS !
   0 DS-DIAG-COUNT !
   0 DAH-ROUND !
   DAH-FIRST-NONE DAH-FIRST-KIND ! ;

: DAH-NEXT-ROUND ( -- )
   DAH-ROUND @ 1+ DAH-ROUND ! ;

: DAH-PROMPT-FILE+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu DS-OUT-BUF DS-OUT-CAP READ-ALL DS-OUT-U !
   DS-OUT-BUF DS-OUT-U @ DS-PROMPT-LN ;

: DAH-REJECT-FEEDBACK ( -- )
   s" " DS-PROMPT-LN
   s" The checker rejected the previous candidate. Use this repair packet:" DS-PROMPT-LN
   DS-REPAIR-PATH$ DAH-PROMPT-FILE+
   s" Raw checker diagnostics:" DS-PROMPT-LN
   DS-DIAG-PATH$ DAH-PROMPT-FILE+
   s" Fix it so it certifies. Output only corrected Habu code." DS-PROMPT-LN ;

: DAH-FAIL-FEEDBACK ( -- )
   s" " DS-PROMPT-LN
   s" The previous candidate certified but failed the vector tests." DS-PROMPT-LN
   s" Test output:" DS-PROMPT-LN
   DS-TEST-PATH$ DAH-PROMPT-FILE+
   s" Expected array examples:" DS-PROMPT-LN
   DAH-VECTORS$ DS-PROMPT-LN
   s" Fix the logic. Output only corrected Habu code." DS-PROMPT-LN ;

: DAH-ADD-FEEDBACK ( -- )
   LR-OUTCOME$ s" reject" STR= if DAH-REJECT-FEEDBACK exit then
   DAH-FAIL-FEEDBACK ;

: DAH-OUTCOME= ( ptr u8 n -- bool ) {: a:ptr u :}
   LR-OUTCOME$ a u STR= ;

: DAH-DONE? ( -- bool )
   s" pass" DAH-OUTCOME= if DS-TRUE exit then
   s" error" DAH-OUTCOME= ;

: DAH-MAX-ROUNDS ( -- n )
   DS-MAX-REPAIRS @ 0 > if DS-MAX-REPAIRS @ exit then
   1 ;

: DAH-MODEL-ERROR ( -- )
   DAH-FIRST-REJECT DAH-FIRST!
   1 DS-DIAG-COUNT !
   DS-MODEL-ERROR
   DAH-LR-ARM!
   DAH-APPLY-ROW-STATS ;

: DAH-RUN-MODEL-ROUND ( -- )
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   DS-TOKENS @ MRUN-TOKENS @ + DS-TOKENS !
   MRUN-RC @ 0= 0= if DAH-MODEL-ERROR exit then
   MRUN-TEXT$ DAH-EVALUATE-TEXT ;

: DAH-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DAH-PATHS!
   DAH-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DAH-RUN-MODEL ( -- )
   DAH-PREPARE
   DAH-STATE-RESET
   begin
      DAH-ROUND @ DAH-MAX-ROUNDS <
   while
      DAH-NEXT-ROUND
      DAH-RUN-MODEL-ROUND
      DAH-DONE? if exit then
      DAH-ROUND @ DAH-MAX-ROUNDS >= if exit then
      DAH-ADD-FEEDBACK
   repeat ;

: DAH-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DAH-PREPARE
   DAH-STATE-RESET
   DAH-NEXT-ROUND
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DAH-EVALUATE-TEXT ;

: DAH-CLI-MAX-REPAIRS ( -- n )
   SCRIPT-ARGC 7 > if 7 SCRIPT-ARGV$ DS-PARSE-U exit then
   s" BENCH_MAX_REPAIRS" 5 DS-ENV-U ;

: DAH-USAGE ( -- )
   s" usage: bench/llm/drive-array-habu.f <id> <name> <sig> <spec> <conv> <vectors> <a|lib|stdlib|skeleton> [maxr]" E-DS-USAGE die ;

: DAH-CONFIG ( -- )
   SCRIPT-ARGC 7 < if DAH-USAGE then
   SCRIPT-ARGC 8 > if DAH-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   s" arrays" DS-CATEGORY!
   3 SCRIPT-ARGV$ DS-SPEC!
   4 SCRIPT-ARGV$ DAH-CONV!
   5 SCRIPT-ARGV$ DAH-VECTORS!
   5 SCRIPT-ARGV$ DS-TESTS!
   6 SCRIPT-ARGV$ DAH-ARM!
   DAH-ARM-VALID? 0= if DAH-USAGE then
   DS-DEFAULTS
   DAH-CLI-MAX-REPAIRS DS-MAX-REPAIRS !
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DAH-MAIN ( -- )
   DAH-CONFIG
   DAH-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
