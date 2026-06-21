\ foreign-vectors.f - checked foreign-language vector snippet emitters.
\
\ Load after lib/errors.f, lib/string.f, bench/llm/manifest.f, and
\ bench/llm/vectors.f.

59 constant FV-SEMI
10 constant FV-LF
32 constant FV-SPACE
45 constant FV-DASH
44 constant FV-COMMA
48 constant FV-ZERO
10 constant FV-BASE
8192 constant FV-OUT-CAP

0 constant FV-LANG-JS
1 constant FV-LANG-PY
2 constant FV-LANG-TS
3 constant FV-LANG-RUST

create FV-OUT FV-OUT-CAP allot

variable FV-OUT-LEN
variable FV-CASE-NEXT
variable FV-TOK-NEXT
variable FV-COUNT#
variable FV-FIRST?

: FV-RESET ( -- )
   0 FV-OUT-LEN ! ;

: FV$ ( -- ptr u8 n )
   FV-OUT FV-OUT-LEN @ ;

: FV-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-FIELD throw then
   add FV-OUT-CAP FV-OUT-LEN @ - > if E-BM-FIELD throw then ;

: FV-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   u FV-ROOM
   a FV-OUT FV-OUT-LEN @ + u BYTE-COPY
   FV-OUT-LEN @ u + FV-OUT-LEN ! ;

: FV-C ( n -- ) {: c :}
   1 FV-ROOM
   c FV-OUT FV-OUT-LEN @ + c!
   FV-OUT-LEN @ 1+ FV-OUT-LEN ! ;

: FV-NL ( -- )
   FV-LF FV-C ;

: FV-U+ ( n -- ) {: n :}
   n 0 < if FV-DASH FV-C n negate recurse exit then
   n FV-BASE >= if n FV-BASE / recurse then
   n FV-BASE mod FV-ZERO + FV-C ;

: FV-APPEND-COMMA-TOKEN ( ptr u8 n -- ) {: a:ptr u :}
   FV-FIRST? @ 0= if FV-COMMA FV-C then
   a u FV-APPEND
   0 FV-FIRST? ! ;

: FV-APPEND-ARRAY-CSV ( ptr u8 n -- n ) {: a:ptr u :}
   a u BV-ARRAY-INNER$ {: b:ptr v :}
   0 FV-COUNT# !
   0 FV-TOK-NEXT !
   1 FV-FIRST? !
   begin
      b v FV-SPACE FV-TOK-NEXT @ SPLIT-NEXT
   while
      FV-TOK-NEXT !
      TRIM dup 0 > if
         2dup BV-REQUIRE-NUM
         FV-APPEND-COMMA-TOKEN
         FV-COUNT# @ 1+ FV-COUNT# !
      else
         2drop
      then
   repeat
   drop 2drop
   FV-COUNT# @ ;

: FV-APPEND-CASE-LABEL ( ptr u8 n -- )
   s" [" FV-APPEND
   FV-APPEND-ARRAY-CSV drop
   s" ]" FV-APPEND ;

: FV-APPEND-EXPECT ( ptr u8 n ptr u8 n n -- ) {: conv:ptr convu lang :}
   conv convu s" as" STR= if BV-SCALAR$ FV-APPEND exit then
   conv convu s" aa" STR= 0= if E-BM-FIELD throw then
   lang FV-LANG-RUST = if s" vec![" FV-APPEND else s" [" FV-APPEND then
   FV-APPEND-ARRAY-CSV drop
   s" ]" FV-APPEND ;

: FV-APPEND-JS-LIKE-TEST ( ptr u8 n ptr u8 n n -- ) {: conv:ptr convu case:ptr caseu lang :}
   lang FV-LANG-JS = if s"   check(f([" FV-APPEND else s" check(f([" FV-APPEND then
   case caseu BV-LHS$ FV-APPEND-ARRAY-CSV drop
   s" ]), " FV-APPEND
   case caseu BV-RHS$ conv convu lang FV-APPEND-EXPECT
   s" , " FV-APPEND
   34 FV-C
   case caseu BV-LHS$ FV-APPEND-CASE-LABEL
   34 FV-C
   s" );" FV-APPEND
   FV-NL ;

: FV-APPEND-PY-TEST ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu case:ptr caseu :}
   s" check(f([" FV-APPEND
   case caseu BV-LHS$ FV-APPEND-ARRAY-CSV drop
   s" ]), " FV-APPEND
   case caseu BV-RHS$ conv convu FV-LANG-PY FV-APPEND-EXPECT
   s" , " FV-APPEND
   34 FV-C
   case caseu BV-LHS$ FV-APPEND-CASE-LABEL
   34 FV-C
   s" )" FV-APPEND
   FV-NL ;

: FV-APPEND-RUST-TEST ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu case:ptr caseu :}
   s"     assert_eq!(f(&[" FV-APPEND
   case caseu BV-LHS$ FV-APPEND-ARRAY-CSV drop
   s" ]), " FV-APPEND
   case caseu BV-RHS$ conv convu FV-LANG-RUST FV-APPEND-EXPECT
   s" );" FV-APPEND
   FV-NL ;

: FV-EACH-TEST ( ptr u8 n n ptr u8 n -- ) {: conv:ptr convu lang vec:ptr vecu :}
   conv convu BV-REQUIRE-CONV
   conv convu s" stack" STR= if E-BM-FIELD throw then
   0 FV-CASE-NEXT !
   begin
      vec vecu FV-SEMI FV-CASE-NEXT @ SPLIT-NEXT
   while
      FV-CASE-NEXT !
      TRIM dup 0 > if
         lang FV-LANG-PY = if conv convu 2swap FV-APPEND-PY-TEST else
            lang FV-LANG-RUST = if conv convu 2swap FV-APPEND-RUST-TEST else
               conv convu 2swap lang FV-APPEND-JS-LIKE-TEST
            then
         then
      else
         2drop
      then
   repeat
   drop 2drop ;

: FV-JS-TESTS ( ptr u8 n ptr u8 n -- ptr u8 n ) {: conv:ptr convu vec:ptr vecu :}
   FV-RESET
   conv convu FV-LANG-JS vec vecu FV-EACH-TEST
   FV$ ;

: FV-PY-TESTS ( ptr u8 n ptr u8 n -- ptr u8 n ) {: conv:ptr convu vec:ptr vecu :}
   FV-RESET
   conv convu FV-LANG-PY vec vecu FV-EACH-TEST
   FV$ ;

: FV-TS-TESTS ( ptr u8 n ptr u8 n -- ptr u8 n ) {: conv:ptr convu vec:ptr vecu :}
   FV-RESET
   conv convu FV-LANG-TS vec vecu FV-EACH-TEST
   FV$ ;

: FV-RUST-TESTS ( ptr u8 n ptr u8 n -- ptr u8 n ) {: conv:ptr convu vec:ptr vecu :}
   FV-RESET
   conv convu FV-LANG-RUST vec vecu FV-EACH-TEST
   FV$ ;

: FV-REQUIRE-REPS ( n -- ) {: n :}
   n 0 < if E-BM-FIELD throw then ;

: FV-APPEND-BENCH-CALL ( ptr u8 n n -- ) {: case:ptr caseu lang :}
   lang FV-LANG-PY = if s"     f([" FV-APPEND then
   lang FV-LANG-JS = if s"   void f([" FV-APPEND then
   lang FV-LANG-TS = if s"   void f([" FV-APPEND then
   lang FV-LANG-RUST = if s"         std::hint::black_box(f(&[" FV-APPEND then
   case caseu BV-LHS$ FV-APPEND-ARRAY-CSV drop
   lang FV-LANG-RUST = if s" ]));" FV-APPEND else
      lang FV-LANG-PY = if s" ])" FV-APPEND else s" ]);" FV-APPEND then
   then
   FV-NL ;

: FV-APPEND-BENCH-CASES ( ptr u8 n n -- ) {: vec:ptr vecu lang :}
   0 FV-CASE-NEXT !
   begin
      vec vecu FV-SEMI FV-CASE-NEXT @ SPLIT-NEXT
   while
      FV-CASE-NEXT !
      TRIM dup 0 > if lang FV-APPEND-BENCH-CALL else 2drop then
   repeat
   drop 2drop ;

: FV-APPEND-JS-RT ( ptr u8 n n n -- ) {: vec:ptr vecu warm reps :}
   warm FV-REQUIRE-REPS reps FV-REQUIRE-REPS
   s" function benchOnce(){" FV-APPEND FV-NL
   vec vecu FV-LANG-JS FV-APPEND-BENCH-CASES
   s" }" FV-APPEND FV-NL
   s" for (let i = 0; i < " FV-APPEND warm FV-U+ s" ; i++) benchOnce();" FV-APPEND FV-NL
   s" const __benchStart = process.hrtime.bigint();" FV-APPEND FV-NL
   s" for (let i = 0; i < " FV-APPEND reps FV-U+ s" ; i++) benchOnce();" FV-APPEND FV-NL
   s" const __benchNs = process.hrtime.bigint() - __benchStart;" FV-APPEND FV-NL
   s" const __benchMs = Number((__benchNs + 999999n) / 1000000n);" FV-APPEND FV-NL
   s" console.log(" FV-APPEND 34 FV-C s" RUNTIME-MS " FV-APPEND 34 FV-C s"  + __benchMs);" FV-APPEND FV-NL ;

: FV-APPEND-TS-RT ( ptr u8 n n n -- ) {: vec:ptr vecu warm reps :}
   warm FV-REQUIRE-REPS reps FV-REQUIRE-REPS
   s" function benchOnce(): void {" FV-APPEND FV-NL
   vec vecu FV-LANG-TS FV-APPEND-BENCH-CASES
   s" }" FV-APPEND FV-NL
   s" for (let i = 0; i < " FV-APPEND warm FV-U+ s" ; i++) benchOnce();" FV-APPEND FV-NL
   s" const __benchStart = process.hrtime.bigint();" FV-APPEND FV-NL
   s" for (let i = 0; i < " FV-APPEND reps FV-U+ s" ; i++) benchOnce();" FV-APPEND FV-NL
   s" const __benchNs = process.hrtime.bigint() - __benchStart;" FV-APPEND FV-NL
   s" const __benchMs = Number((__benchNs + 999999n) / 1000000n);" FV-APPEND FV-NL
   s" console.log(" FV-APPEND 34 FV-C s" RUNTIME-MS " FV-APPEND 34 FV-C s"  + __benchMs);" FV-APPEND FV-NL ;

: FV-APPEND-PY-RT ( ptr u8 n n n -- ) {: vec:ptr vecu warm reps :}
   warm FV-REQUIRE-REPS reps FV-REQUIRE-REPS
   s" import time" FV-APPEND FV-NL
   s" def bench_once():" FV-APPEND FV-NL
   vec vecu FV-LANG-PY FV-APPEND-BENCH-CASES
   s" for _ in range(" FV-APPEND warm FV-U+ s" ):" FV-APPEND FV-NL
   s"     bench_once()" FV-APPEND FV-NL
   s" __bench_start = time.perf_counter_ns()" FV-APPEND FV-NL
   s" for _ in range(" FV-APPEND reps FV-U+ s" ):" FV-APPEND FV-NL
   s"     bench_once()" FV-APPEND FV-NL
   s" __bench_ns = time.perf_counter_ns() - __bench_start" FV-APPEND FV-NL
   s" __bench_ms = (__bench_ns + 999_999) // 1_000_000" FV-APPEND FV-NL
   s" print(" FV-APPEND 34 FV-C s" RUNTIME-MS " FV-APPEND 34 FV-C s"  + str(__bench_ms))" FV-APPEND FV-NL ;

: FV-APPEND-RUST-RT ( ptr u8 n n n -- ) {: vec:ptr vecu warm reps :}
   warm FV-REQUIRE-REPS reps FV-REQUIRE-REPS
   s"     fn bench_once() {" FV-APPEND FV-NL
   vec vecu FV-LANG-RUST FV-APPEND-BENCH-CASES
   s"     }" FV-APPEND FV-NL
   s"     for _ in 0.." FV-APPEND warm FV-U+ s"  { bench_once(); }" FV-APPEND FV-NL
   s"     let __bench_start = std::time::Instant::now();" FV-APPEND FV-NL
   s"     for _ in 0.." FV-APPEND reps FV-U+ s"  { bench_once(); }" FV-APPEND FV-NL
   s"     let __bench_ms = (__bench_start.elapsed().as_nanos() + 999_999) / 1_000_000;" FV-APPEND FV-NL
   s"     println!(" FV-APPEND 34 FV-C s" RUNTIME-MS {}" FV-APPEND 34 FV-C s" , __bench_ms);" FV-APPEND FV-NL ;

: FV-JS-BENCH ( ptr u8 n n n -- ptr u8 n ) {: vec:ptr vecu warm reps :}
   FV-RESET
   vec vecu warm reps FV-APPEND-JS-RT
   FV$ ;

: FV-PY-BENCH ( ptr u8 n n n -- ptr u8 n ) {: vec:ptr vecu warm reps :}
   FV-RESET
   vec vecu warm reps FV-APPEND-PY-RT
   FV$ ;

: FV-TS-BENCH ( ptr u8 n n n -- ptr u8 n ) {: vec:ptr vecu warm reps :}
   FV-RESET
   vec vecu warm reps FV-APPEND-TS-RT
   FV$ ;

: FV-RUST-BENCH ( ptr u8 n n n -- ptr u8 n ) {: vec:ptr vecu warm reps :}
   FV-RESET
   vec vecu warm reps FV-APPEND-RUST-RT
   FV$ ;
