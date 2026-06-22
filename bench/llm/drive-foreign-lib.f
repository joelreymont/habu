\ drive-foreign-lib.f - native foreign-language benchmark driver library.
\
\ Load after lib/memory.f, bench/llm/drive-stdlib-lib.f,
\ bench/llm/foreign-vectors.f, and bench/llm/model-run.f.

0 constant DFG-FIRST-NONE
1 constant DFG-FIRST-PASS
2 constant DFG-FIRST-FAIL
10 constant DFG-LF
11 constant DFG-RUNTIME-KEY-LEN
16 constant DFG-CONV-CAP
5000 constant DFG-RUN-TIMEOUT-MS
1000000 constant DFG-NS-PER-MS

create DFG-CONV-BUF DFG-CONV-CAP allot
create DFG-RUNTIME-PATH FS-PATH-CAP allot
create DFG-NODE-PATH FS-PATH-CAP allot

variable DFG-CONV-U
variable DFG-RUNTIME-U
variable DFG-NODE-U
variable DFG-ROUND
variable DFG-FIRST-KIND
variable DFG-TEST-KIND
variable DFG-TEST-CODE
variable DFG-RUNTIME-KIND
variable DFG-RUNTIME-CODE
variable DFG-TIMEOUT-U
variable DFG-START-NS
variable DFG-WALL-MS
variable DFG-START

: DFG-CONV! ( ptr u8 n -- ) {: a:ptr u :}
   u DFG-CONV-CAP > if E-DS-CAPACITY throw then
   a DFG-CONV-BUF u BYTE-COPY
   u DFG-CONV-U ! ;

: DFG-CONV$ ( -- ptr u8 n )
   DFG-CONV-BUF DFG-CONV-U @ ;

: DFG-RUNTIME$ ( -- ptr u8 n )
   DFG-RUNTIME-PATH DFG-RUNTIME-U @ ;

: DFG-NODE$ ( -- ptr u8 n )
   DFG-NODE-PATH DFG-NODE-U @ ;

: DFG-EMPTY$ ( -- ptr u8 n )
   s" " drop 0 ;

: DFG-TIMEOUT ( -- n )
   DFG-TIMEOUT-U @ 0 > if DFG-TIMEOUT-U @ exit then
   DFG-RUN-TIMEOUT-MS ;

: DFG-WALL-SNAPSHOT ( -- )
   mono-ns DFG-START-NS ! ;

: DFG-WALL-CAPTURE ( -- )
   mono-ns DFG-START-NS @ - DFG-NS-PER-MS 1- + DFG-NS-PER-MS / DFG-WALL-MS ! ;

: DFG-PATHS! ( -- )
   s" runtime.js" DFG-RUNTIME-PATH DFG-RUNTIME-U DS-JOIN! ;

: DFG-RESOLVE-NODE ( -- )
   s" node" >LEN DFG-NODE-PATH RESOLVE-EXECUTABLE LEN>N DFG-NODE-U ! ;

: DFG-FIRST! ( n -- ) {: kind :}
   DFG-FIRST-KIND @ DFG-FIRST-NONE = if kind DFG-FIRST-KIND ! then ;

: DFG-REPAIR-ROUNDS ( -- n )
   DFG-ROUND @ 0 > if DFG-ROUND @ 1- exit then
   0 ;

: DFG-APPLY-FIRST ( -- )
   DFG-FIRST-KIND @ DFG-FIRST-PASS = if
      s" certified" LR-FIRST-CHECKER!
      -1 LR-FIRST-PASS !
      -1 LR-FIRST-TESTS !
      exit
   then
   s" rejected" LR-FIRST-CHECKER!
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS ! ;

: DFG-CONFIG-LR-COMMON ( -- )
   DS-CONFIG-LR-COMMON
   s" js" LR-ARM!
   DFG-WALL-MS @ LR-WALL-MS !
   DFG-ROUND @ LR-ROUNDS !
   DFG-REPAIR-ROUNDS LR-REPAIR-ITERATIONS !
   DFG-ROUND @ LR-CHECKER-ITERATIONS !
   0 LR-DIAG-COUNT !
   DFG-APPLY-FIRST ;

: DFG-LR-PASS ( -- )
   DFG-WALL-CAPTURE
   DFG-FIRST-PASS DFG-FIRST!
   DFG-CONFIG-LR-COMMON
   s" pass" LR-OUTCOME!
   -1 LR-TESTS-PASSED ! ;

: DFG-LR-OUTCOME ( ptr u8 n -- )
   DFG-WALL-CAPTURE
   DFG-FIRST-FAIL DFG-FIRST!
   DFG-CONFIG-LR-COMMON
   LR-OUTCOME!
   0 LR-TESTS-PASSED ! ;

: DFG-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   s" Write a JavaScript function with this exact signature:" DS-PROMPT-LN
   s"   function f(a) { ... }" DS-PROMPT-LN
   s" where a is an array of integers." DS-PROMPT-LN
   DFG-CONV$ s" as" STR= if
      s" It must return one integer result." DS-PROMPT-LN
   else
      s" It must return a new array of integers." DS-PROMPT-LN
   then
   DS-SPEC$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Expected examples:" DS-PROMPT-LN
   DS-TESTS$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Use integer arithmetic. Output ONLY the function definition." DS-PROMPT-LN
   s" No prose, no markdown, no code fences." DS-PROMPT-LN ;

: DFG-FENCE-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u TRIM s" ```" STARTS-WITH? ;

: DFG-CAND-LINE ( ptr u8 n -- ) {: a:ptr u :}
   a u DFG-FENCE-LINE? if exit then
   a u DS-CAND-LN ;

: DFG-EXTRACT-CANDIDATE ( ptr u8 n -- ) {: a:ptr u :}
   DS-CAND-RESET
   0 DS-LINE-NEXT !
   begin
      a u DFG-LF DS-LINE-NEXT @ SPLIT-NEXT
   while
      DS-LINE-NEXT !
      DFG-CAND-LINE
   repeat
   drop 2drop
   DS-CAND-U @ 0= if s" // no candidate extracted" DS-CAND-LN then ;

: DFG-BUILD-TEST-BUNDLE ( -- )
   DS-TEST-RESET
   DS-CAND$ DS-TEST+
   s" " DS-TEST-LN
   s" function check(g,w,a){ if(JSON.stringify(g)!==JSON.stringify(w)){ console.error('FAIL f('+a+') = '+JSON.stringify(g)+' expected '+JSON.stringify(w)); process.exit(1); } }" DS-TEST-LN
   DFG-CONV$ DS-TESTS$ FV-JS-TESTS DS-TEST+
   s" console.log('ALL-OK');" DS-TEST-LN
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DFG-BUILD-RUNTIME-BUNDLE ( -- )
   DS-TEST-RESET
   DS-CAND$ DS-TEST+
   s" " DS-TEST-LN
   DS-TESTS$ 10 100 FV-JS-BENCH DS-TEST+
   DFG-RUNTIME$ DS-TEST$ WRITE-ALL ;

: DFG-NODE-CAPTURE ( ptr u8 n -- ) {: script:ptr scriptu :}
   PROC-ARGV-ENV-RESET
   script scriptu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   DFG-NODE$ >LEN DFG-EMPTY$ >LEN DS-OUT-BUF DS-OUT-CAP >LEN
   DS-ERR-BUF DS-ERR-CAP >LEN DFG-TIMEOUT >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: outu erru kind code :}
   code DFG-TEST-CODE !
   kind DFG-TEST-KIND !
   code DFG-RUNTIME-CODE !
   kind DFG-RUNTIME-KIND !
   erru LEN>N DS-ERR-U !
   outu LEN>N DS-OUT-U !
   kind PROC-OUTCOME-EXIT = if code else 128 code + then DS-RC ! ;

: DFG-RUN-TESTS ( -- )
   DS-BUNDLE-PATH$ DFG-NODE-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DFG-TEST-PASS? ( -- bool )
   DS-RC @ 0 <> if DS-FALSE exit then
   DS-OUT-BUF DS-OUT-U @ s" ALL-OK" CONTAINS? ;

: DFG-RUNTIME-MS? ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u s" RUNTIME-MS " FIND-SUB dup 0 < if drop 0 DS-FALSE exit then
   DFG-RUNTIME-KEY-LEN + DFG-START !
   a DFG-START @ + u DFG-START @ - TRIM STR>NUMBER? ;

: DFG-FINISH-RUNTIME ( -- )
   DFG-BUILD-RUNTIME-BUNDLE
   DFG-RUNTIME$ DFG-NODE-CAPTURE
   DFG-RUNTIME-KIND @ PROC-OUTCOME-EXIT = 0= if
      s" error" DFG-LR-OUTCOME
      s" error" LR-RUNTIME-STATUS!
      exit
   then
   DS-RC @ 0 <> if
      s" error" DFG-LR-OUTCOME
      s" error" LR-RUNTIME-STATUS!
      exit
   then
   DS-OUT-BUF DS-OUT-U @ DFG-RUNTIME-MS? 0= if
      drop
      s" error" DFG-LR-OUTCOME
      s" error" LR-RUNTIME-STATUS!
      exit
   then
   DFG-LR-PASS
   LR-RUNTIME-MS!
   100 10 LR-RUNTIME-COUNTS!
   s" ok" LR-RUNTIME-STATUS! ;

: DFG-FINISH-TESTS ( -- )
   DFG-RUN-TESTS
   DFG-TEST-KIND @ PROC-OUTCOME-TIMEOUT = if s" timeout" DFG-LR-OUTCOME exit then
   DFG-TEST-KIND @ PROC-OUTCOME-SIGNAL = if s" trap" DFG-LR-OUTCOME exit then
   DFG-TEST-KIND @ PROC-OUTCOME-EXIT <> if s" error" DFG-LR-OUTCOME exit then
   DFG-TEST-PASS? if DFG-FINISH-RUNTIME else s" fail" DFG-LR-OUTCOME then ;

: DFG-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DFG-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DFG-BUILD-TEST-BUNDLE
   DFG-FINISH-TESTS ;

: DFG-STATE-RESET ( -- )
   0 DS-TOKENS !
   0 DS-DIAG-COUNT !
   0 DFG-ROUND !
   0 DFG-WALL-MS !
   PROC-OUTCOME-EXIT DFG-TEST-KIND !
   PROC-OUTCOME-EXIT DFG-RUNTIME-KIND !
   0 DFG-TEST-CODE !
   0 DFG-RUNTIME-CODE !
   DFG-FIRST-NONE DFG-FIRST-KIND ! ;

: DFG-NEXT-ROUND ( -- )
   DFG-ROUND @ 1+ DFG-ROUND ! ;

: DFG-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DFG-PATHS!
   DFG-RESOLVE-NODE
   DFG-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DFG-PROMPT-FILE+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu DS-OUT-BUF DS-OUT-CAP READ-ALL DS-OUT-U !
   DS-OUT-BUF DS-OUT-U @ DS-PROMPT-LN ;

: DFG-ADD-FEEDBACK ( -- )
   s" " DS-PROMPT-LN
   s" Your previous attempt:" DS-PROMPT-LN
   DS-CAND-PATH$ DFG-PROMPT-FILE+
   s" It failed with this output:" DS-PROMPT-LN
   DS-TEST-PATH$ DFG-PROMPT-FILE+
   s" Fix it. Output ONLY the corrected function definition." DS-PROMPT-LN ;

: DFG-OUTCOME= ( ptr u8 n -- bool ) {: a:ptr u :}
   LR-OUTCOME$ a u STR= ;

: DFG-DONE? ( -- bool )
   s" pass" DFG-OUTCOME= if DS-TRUE exit then
   s" error" DFG-OUTCOME= ;

: DFG-MAX-ROUNDS ( -- n )
   DS-MAX-REPAIRS @ 0 > if DS-MAX-REPAIRS @ exit then
   1 ;

: DFG-REQUIRE-CONV ( -- )
   DFG-CONV$ s" as" STR= if exit then
   DFG-CONV$ s" aa" STR= if exit then
   E-DS-USAGE throw ;

: DFG-MODEL-ERROR ( -- )
   DS-DIAG-PATH$ MRUN-ERR$ WRITE-ALL
   MRUN-RC @ 0= if DS-DIAG-PATH$ s" model parse failed" WRITE-ALL then
   1 DS-DIAG-COUNT !
   s" error" DFG-LR-OUTCOME ;

: DFG-RUN-MODEL-ROUND ( -- )
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   DS-TOKENS @ MRUN-TOKENS @ + DS-TOKENS !
   MRUN-RC @ 0= 0= if DFG-MODEL-ERROR exit then
   MRUN-TEXT$ DFG-EVALUATE-TEXT ;

: DFG-RUN-MODEL ( -- )
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   begin DFG-ROUND @ DFG-MAX-ROUNDS < while
      DFG-NEXT-ROUND
      DFG-RUN-MODEL-ROUND
      DFG-DONE? if exit then
      DFG-ROUND @ DFG-MAX-ROUNDS >= if exit then
      DFG-ADD-FEEDBACK
   repeat ;

: DFG-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   DFG-NEXT-ROUND
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DFG-EVALUATE-TEXT ;

: DFG-CLI-MAX-REPAIRS ( -- n )
   SCRIPT-ARGC 6 > if 6 SCRIPT-ARGV$ DS-PARSE-U exit then
   s" BENCH_MAX_REPAIRS" 5 DS-ENV-U ;

: DFG-USAGE ( -- )
   s" usage: bench/llm/drive-js.f <id> <name> <sig> <spec> <conv> <vectors> [maxr]" E-DS-USAGE die ;

: DFG-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DFG-USAGE then
   SCRIPT-ARGC 7 > if DFG-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   s" arrays" DS-CATEGORY!
   3 SCRIPT-ARGV$ DS-SPEC!
   4 SCRIPT-ARGV$ DFG-CONV!
   5 SCRIPT-ARGV$ DS-TESTS!
   DFG-REQUIRE-CONV
   DS-DEFAULTS
   DFG-CLI-MAX-REPAIRS DS-MAX-REPAIRS !
   s" BENCH_JS_TIMEOUT_MS" DFG-RUN-TIMEOUT-MS DS-ENV-U DFG-TIMEOUT-U !
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DFG-MAIN ( -- )
   DFG-CONFIG
   DFG-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
