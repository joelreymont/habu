\ drive-foreign-lib.f - native foreign-language benchmark driver library.
\
\ Load after lib/memory.f, bench/llm/drive-stdlib-lib.f, and
\ bench/llm/foreign-vectors.f.

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
create DFG-EXEC-PATH FS-PATH-CAP allot
create DFG-BIN-PATH FS-PATH-CAP allot
create DFG-RUNTIME-BIN-PATH FS-PATH-CAP allot

variable DFG-LANG
variable DFG-CONV-U
variable DFG-RUNTIME-U
variable DFG-EXEC-U
variable DFG-BIN-U
variable DFG-RUNTIME-BIN-U
variable DFG-ROUND
variable DFG-FIRST-KIND
variable DFG-TEST-KIND
variable DFG-TEST-CODE
variable DFG-RUNTIME-KIND
variable DFG-RUNTIME-CODE
variable DFG-TIMEOUT-U
variable DFG-COMPILE-TIMEOUT-U
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

: DFG-BIN$ ( -- ptr u8 n )
   DFG-BIN-PATH DFG-BIN-U @ ;

: DFG-RUNTIME-BIN$ ( -- ptr u8 n )
   DFG-RUNTIME-BIN-PATH DFG-RUNTIME-BIN-U @ ;

: DFG-EXEC$ ( -- ptr u8 n )
   DFG-EXEC-PATH DFG-EXEC-U @ ;

: DFG-JS! ( -- )
   FV-LANG-JS DFG-LANG ! ;

: DFG-PY! ( -- )
   FV-LANG-PY DFG-LANG ! ;

: DFG-TS! ( -- )
   FV-LANG-TS DFG-LANG ! ;

: DFG-RUST! ( -- )
   FV-LANG-RUST DFG-LANG ! ;

: DFG-PY? ( -- bool )
   DFG-LANG @ FV-LANG-PY = ;

: DFG-TS? ( -- bool )
   DFG-LANG @ FV-LANG-TS = ;

: DFG-RUST? ( -- bool )
   DFG-LANG @ FV-LANG-RUST = ;

: DFG-ARM$ ( -- ptr u8 n )
   DFG-PY? if s" python" exit then
   DFG-TS? if s" ts" exit then
   DFG-RUST? if s" rust" exit then
   s" js" ;

: DFG-RUNTIME-CMD$ ( -- ptr u8 n )
   DFG-PY? if s" PYTHON" s" python3" DS-ENV$ exit then
   DFG-TS? if s" bun" exit then
   DFG-RUST? if s" RUSTC" s" rustc" DS-ENV$ exit then
   s" node" ;

: DFG-TIMEOUT-ENV$ ( -- ptr u8 n )
   DFG-PY? if s" BENCH_PY_TIMEOUT_MS" exit then
   DFG-TS? if s" BENCH_TS_TIMEOUT_MS" exit then
   DFG-RUST? if s" BENCH_RUST_TIMEOUT_MS" exit then
   s" BENCH_JS_TIMEOUT_MS" ;

: DFG-COMPILE-TIMEOUT-ENV$ ( -- ptr u8 n )
   DFG-RUST? if s" BENCH_RUST_COMPILE_TIMEOUT_MS" exit then
   DFG-TIMEOUT-ENV$ ;

: DFG-EMPTY$ ( -- ptr u8 n )
   s" " drop 0 ;

: DFG-RUN-TIMEOUT ( -- n )
   DFG-TIMEOUT-U @ 0 > if DFG-TIMEOUT-U @ exit then
   DFG-RUN-TIMEOUT-MS ;

: DFG-COMPILE-TIMEOUT ( -- n )
   DFG-COMPILE-TIMEOUT-U @ 0 > if DFG-COMPILE-TIMEOUT-U @ exit then
   DFG-RUN-TIMEOUT-MS ;

: DFG-WALL-SNAPSHOT ( -- )
   mono-ns DFG-START-NS ! ;

: DFG-WALL-CAPTURE ( -- )
   mono-ns DFG-START-NS @ - DFG-NS-PER-MS 1- + DFG-NS-PER-MS / DFG-WALL-MS ! ;

: DFG-PATHS! ( -- )
   DFG-PY? if
      s" test.py" DS-BUNDLE-PATH DS-BUNDLE-PATH-U DS-JOIN!
      s" runtime.py" DFG-RUNTIME-PATH DFG-RUNTIME-U DS-JOIN!
      exit
   then
   DFG-TS? if
      s" test.ts" DS-BUNDLE-PATH DS-BUNDLE-PATH-U DS-JOIN!
      s" runtime.ts" DFG-RUNTIME-PATH DFG-RUNTIME-U DS-JOIN!
      exit
   then
   DFG-RUST? if
      s" test.rs" DS-BUNDLE-PATH DS-BUNDLE-PATH-U DS-JOIN!
      s" runtime.rs" DFG-RUNTIME-PATH DFG-RUNTIME-U DS-JOIN!
      s" test-bin" DFG-BIN-PATH DFG-BIN-U DS-JOIN!
      s" runtime-bin" DFG-RUNTIME-BIN-PATH DFG-RUNTIME-BIN-U DS-JOIN!
      exit
   then
   s" test.js" DS-BUNDLE-PATH DS-BUNDLE-PATH-U DS-JOIN!
   s" runtime.js" DFG-RUNTIME-PATH DFG-RUNTIME-U DS-JOIN! ;

: DFG-RESOLVE-EXEC ( -- )
   DFG-RUNTIME-CMD$ >LEN DFG-EXEC-PATH RESOLVE-EXECUTABLE LEN>N DFG-EXEC-U ! ;

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
   DFG-ARM$ LR-ARM!
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

: DFG-PROMPT-RETURN-LN ( -- )
   DFG-CONV$ s" as" STR= if
      s" It must return one integer result." DS-PROMPT-LN
      exit
   then
   DFG-PY? if
      s" It must return a new list of integers." DS-PROMPT-LN
      exit
   then
   DFG-RUST? if
      s" It must return a new Vec<i64>." DS-PROMPT-LN
      exit
   then
   s" It must return a new array of integers." DS-PROMPT-LN ;

: DFG-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   DFG-PY? if
      s" Write a Python function with this exact signature:" DS-PROMPT-LN
      s"   def f(a):" DS-PROMPT-LN
      s"       ..." DS-PROMPT-LN
   else DFG-TS? if
      s" Write a TypeScript function with this exact signature:" DS-PROMPT-LN
      DFG-CONV$ s" as" STR= if
         s"   function f(a: number[]): number { ... }" DS-PROMPT-LN
      else
         s"   function f(a: number[]): number[] { ... }" DS-PROMPT-LN
      then
   else DFG-RUST? if
      s" Write a Rust function with this exact signature:" DS-PROMPT-LN
      DFG-CONV$ s" as" STR= if
         s"   fn f(a: &[i64]) -> i64 { ... }" DS-PROMPT-LN
      else
         s"   fn f(a: &[i64]) -> Vec<i64> { ... }" DS-PROMPT-LN
      then
   else
      s" Write a JavaScript function with this exact signature:" DS-PROMPT-LN
      s"   function f(a) { ... }" DS-PROMPT-LN
   then then then
   DFG-PY? if
      s" where a is a list of integers." DS-PROMPT-LN
   else DFG-RUST? if
      s" where a is a slice of integers." DS-PROMPT-LN
   else
      s" where a is an array of integers." DS-PROMPT-LN
   then then
   DFG-PROMPT-RETURN-LN
   DFG-PY? if s" Use only the Python standard library." DS-PROMPT-LN then
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
   DS-CAND-U @ 0= if
      DFG-PY? if s" # no candidate extracted" else s" // no candidate extracted" then
      DS-CAND-LN
   then ;

: DFG-BUILD-TEST-BUNDLE ( -- )
   DS-TEST-RESET
   DS-CAND$ DS-TEST+
   s" " DS-TEST-LN
   DFG-PY? if
      s" import sys" DS-TEST-LN
      s" def check(g,w,a):" DS-TEST-LN
      s"     if g != w:" DS-TEST-LN
      s"         print('FAIL f(' + a + ') = ' + repr(g) + ' expected ' + repr(w), file=sys.stderr)" DS-TEST-LN
      s"         sys.exit(1)" DS-TEST-LN
      DFG-CONV$ DS-TESTS$ FV-PY-TESTS DS-TEST+
      s" print('ALL-OK')" DS-TEST-LN
      DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL
      exit
   then
   DFG-TS? if
      s" function check(g: unknown,w: unknown,a: string): void { if(JSON.stringify(g)!==JSON.stringify(w)){ console.error('FAIL f('+a+') = '+JSON.stringify(g)+' expected '+JSON.stringify(w)); process.exit(1); } }" DS-TEST-LN
      DFG-CONV$ DS-TESTS$ FV-TS-TESTS DS-TEST+
   else DFG-RUST? if
      s" fn main() {" DS-TEST-LN
      DFG-CONV$ DS-TESTS$ FV-RUST-TESTS DS-TEST+
      s"     println!(" DS-TEST+
      DS-DQ DS-TEST-C
      s" ALL-OK" DS-TEST+
      DS-DQ DS-TEST-C
      s" );" DS-TEST-LN
      s" }" DS-TEST-LN
      DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL
      exit
   else
      s" function check(g,w,a){ if(JSON.stringify(g)!==JSON.stringify(w)){ console.error('FAIL f('+a+') = '+JSON.stringify(g)+' expected '+JSON.stringify(w)); process.exit(1); } }" DS-TEST-LN
      DFG-CONV$ DS-TESTS$ FV-JS-TESTS DS-TEST+
   then then
   s" console.log('ALL-OK');" DS-TEST-LN
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DFG-BUILD-RUNTIME-BUNDLE ( -- )
   DS-TEST-RESET
   DS-CAND$ DS-TEST+
   s" " DS-TEST-LN
   DFG-PY? if
      DS-TESTS$ 10 100 FV-PY-BENCH DS-TEST+
   else DFG-TS? if
      DS-TESTS$ 10 100 FV-TS-BENCH DS-TEST+
   else DFG-RUST? if
      s" fn main() {" DS-TEST-LN
      DS-TESTS$ 10 100 FV-RUST-BENCH DS-TEST+
      s" }" DS-TEST-LN
   else
      DS-TESTS$ 10 100 FV-JS-BENCH DS-TEST+
   then then then
   DFG-RUNTIME$ DS-TEST$ WRITE-ALL ;

: DFG-STORE-OUTCOME ( len len n n -- ) {: outu erru kind code :}
   code DFG-TEST-CODE !
   kind DFG-TEST-KIND !
   code DFG-RUNTIME-CODE !
   kind DFG-RUNTIME-KIND !
   erru LEN>N DS-ERR-U !
   outu LEN>N DS-OUT-U !
   kind code PROC-OUTCOME>RC RC>N DS-RC ! ;

: DFG-CAPTURE-ARGV-TIMEOUT ( ptr u8 n n -- ) {: exe:ptr exeu ms :}
   PROC-ENV-INHERIT-MISSING
   exe exeu >LEN DFG-EMPTY$ >LEN DS-OUT-BUF DS-OUT-CAP >LEN
   DS-ERR-BUF DS-ERR-CAP >LEN ms >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME DFG-STORE-OUTCOME ;

: DFG-CAPTURE-ARGV ( ptr u8 n -- )
   DFG-RUN-TIMEOUT DFG-CAPTURE-ARGV-TIMEOUT ;

: DFG-SCRIPT-CAPTURE ( ptr u8 n -- ) {: script:ptr scriptu :}
   PROC-ARGV-ENV-RESET
   script scriptu >LEN PROC-ARGV+
   DFG-EXEC$ DFG-CAPTURE-ARGV ;

: DFG-EXECUTABLE-CAPTURE ( ptr u8 n -- ) {: exe:ptr exeu :}
   PROC-ARGV-ENV-RESET
   exe exeu DFG-CAPTURE-ARGV ;

: DFG-RUST-COMPILE? ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu out:ptr outu :}
   PROC-ARGV-ENV-RESET
   src srcu >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   DFG-EXEC$ DFG-COMPILE-TIMEOUT DFG-CAPTURE-ARGV-TIMEOUT
   DFG-TEST-KIND @ PROC-OUTCOME-EXIT <> if DS-FALSE exit then
   DS-RC @ 0= if DS-TRUE exit then
   DS-FALSE ;

: DFG-RUN-TESTS ( -- )
   DS-BUNDLE-PATH$ DFG-SCRIPT-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DFG-RUN-RUST-TESTS? ( -- bool )
   DS-BUNDLE-PATH$ DFG-BIN$ DFG-RUST-COMPILE? 0= if
      DS-DIAG-PATH$ DS-WRITE-CAPTURE
      DS-TEST-PATH$ DS-WRITE-CAPTURE
      s" reject" DFG-LR-OUTCOME
      1 LR-DIAG-COUNT !
      DS-FALSE exit
   then
   DFG-BIN$ DFG-EXECUTABLE-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE
   DS-TRUE ;

: DFG-TEST-PASS? ( -- bool )
   DS-RC @ 0 <> if DS-FALSE exit then
   DS-OUT-BUF DS-OUT-U @ s" ALL-OK" CONTAINS? ;

: DFG-RUNTIME-MS? ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u s" RUNTIME-MS " FIND-SUB dup 0 < if drop 0 DS-FALSE exit then
   DFG-RUNTIME-KEY-LEN + DFG-START !
   a DFG-START @ + u DFG-START @ - TRIM STR>NUMBER? ;

: DFG-FINISH-RUNTIME ( -- )
   DFG-BUILD-RUNTIME-BUNDLE
   DFG-RUST? if
      DFG-RUNTIME$ DFG-RUNTIME-BIN$ DFG-RUST-COMPILE? if
         DFG-RUNTIME-BIN$ DFG-EXECUTABLE-CAPTURE
      then
   else
      DFG-RUNTIME$ DFG-SCRIPT-CAPTURE
   then
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
   DFG-RUST? if DFG-RUN-RUST-TESTS? 0= if exit then else DFG-RUN-TESTS then
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
   DFG-RESOLVE-EXEC
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
   s" usage: bench/llm/drive-js.f|drive-python.f|drive-rust.f|drive-ts.f <id> <name> <sig> <spec> <conv> <vectors> [maxr]" E-DS-USAGE die ;

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
   DFG-TIMEOUT-ENV$ DFG-RUN-TIMEOUT-MS DS-ENV-U DFG-TIMEOUT-U !
   DFG-COMPILE-TIMEOUT-ENV$ DFG-RUN-TIMEOUT-MS DS-ENV-U DFG-COMPILE-TIMEOUT-U !
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;
