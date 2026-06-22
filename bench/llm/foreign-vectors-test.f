\ foreign-vectors-test.f - focused tests for foreign vector emitters.

34 constant FVT-DQ
10 constant FVT-LF

: FVT+ ( ptr u8 n -- )
   SB-APPEND ;

: FVT-DQ+ ( -- )
   FVT-DQ SB-APPEND-C ;

: FVT-NL+ ( -- )
   FVT-LF SB-APPEND-C ;

: FVT-WANT$ ( -- ptr u8 n )
   SB$ ;

: FVT-AS-VECTORS$ ( -- ptr u8 n )
   s" [3 1 4] -> 8; [] -> 0; [-2 -3] -> -5" ;

: FVT-AA-VECTORS$ ( -- ptr u8 n )
   s" [1 2] -> [2 1]; [] -> []; [-1] -> [-1]" ;

: FVT-SINGLE-VECTOR$ ( -- ptr u8 n )
   s" [7] -> 7" ;

: FVT-RUNTIME-VECTORS$ ( -- ptr u8 n )
   s" [3 1 4] -> 8; [] -> 0" ;

: FVT-BAD-CONV ( -- )
   s" stack" FVT-AS-VECTORS$ FV-JS-TESTS 2drop ;

: FVT-BAD-REPS ( -- )
   FVT-AS-VECTORS$ -1 5 FV-JS-BENCH 2drop ;

: FVT-JS-CHECK-LINE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: got:ptr gotu want:ptr wantu label:ptr labelu :}
   s"   check(f(" FVT+ got gotu FVT+ s" ), " FVT+
   want wantu FVT+ s" , " FVT+
   FVT-DQ+ label labelu FVT+ FVT-DQ+
   s" );" FVT+ FVT-NL+ ;

: FVT-PY-CHECK-LINE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: got:ptr gotu want:ptr wantu label:ptr labelu :}
   s" check(f(" FVT+ got gotu FVT+ s" ), " FVT+
   want wantu FVT+ s" , " FVT+
   FVT-DQ+ label labelu FVT+ FVT-DQ+
   s" )" FVT+ FVT-NL+ ;

: FVT-TS-CHECK-LINE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: got:ptr gotu want:ptr wantu label:ptr labelu :}
   s" check(f(" FVT+ got gotu FVT+ s" ), " FVT+
   want wantu FVT+ s" , " FVT+
   FVT-DQ+ label labelu FVT+ FVT-DQ+
   s" );" FVT+ FVT-NL+ ;

: FVT-RUST-CHECK-LINE ( ptr u8 n ptr u8 n -- ) {: got:ptr gotu want:ptr wantu :}
   s"     assert_eq!(f(&" FVT+ got gotu FVT+ s" ), " FVT+
   want wantu FVT+ s" );" FVT+ FVT-NL+ ;

: FVT-WANT-JS-AS ( -- ptr u8 n )
   SB-RESET
   s" [3,1,4]" s" 8" s" [3,1,4]" FVT-JS-CHECK-LINE
   s" []" s" 0" s" []" FVT-JS-CHECK-LINE
   s" [-2,-3]" s" -5" s" [-2,-3]" FVT-JS-CHECK-LINE
   FVT-WANT$ ;

: FVT-WANT-JS-AA ( -- ptr u8 n )
   SB-RESET
   s" [1,2]" s" [2,1]" s" [1,2]" FVT-JS-CHECK-LINE
   s" []" s" []" s" []" FVT-JS-CHECK-LINE
   s" [-1]" s" [-1]" s" [-1]" FVT-JS-CHECK-LINE
   FVT-WANT$ ;

: FVT-WANT-PY-SINGLE ( -- ptr u8 n )
   SB-RESET
   s" [7]" s" 7" s" [7]" FVT-PY-CHECK-LINE
   FVT-WANT$ ;

: FVT-WANT-PY-AA ( -- ptr u8 n )
   SB-RESET
   s" [1,2]" s" [2,1]" s" [1,2]" FVT-PY-CHECK-LINE
   s" []" s" []" s" []" FVT-PY-CHECK-LINE
   s" [-1]" s" [-1]" s" [-1]" FVT-PY-CHECK-LINE
   FVT-WANT$ ;

: FVT-WANT-TS-AS ( -- ptr u8 n )
   SB-RESET
   s" [3,1,4]" s" 8" s" [3,1,4]" FVT-TS-CHECK-LINE
   s" []" s" 0" s" []" FVT-TS-CHECK-LINE
   s" [-2,-3]" s" -5" s" [-2,-3]" FVT-TS-CHECK-LINE
   FVT-WANT$ ;

: FVT-WANT-RUST-AA ( -- ptr u8 n )
   SB-RESET
   s" [1,2]" s" vec![2,1]" FVT-RUST-CHECK-LINE
   s" []" s" vec![]" FVT-RUST-CHECK-LINE
   s" [-1]" s" vec![-1]" FVT-RUST-CHECK-LINE
   FVT-WANT$ ;

: FVT-CHECK-JS-TESTS ( -- )
   s" as" FVT-AS-VECTORS$ FV-JS-TESTS FVT-WANT-JS-AS T$=
   s" aa" FVT-AA-VECTORS$ FV-JS-TESTS FVT-WANT-JS-AA T$= ;

: FVT-CHECK-PY-TESTS ( -- )
   s" as" FVT-SINGLE-VECTOR$ FV-PY-TESTS FVT-WANT-PY-SINGLE T$=
   s" aa" FVT-AA-VECTORS$ FV-PY-TESTS FVT-WANT-PY-AA T$= ;

: FVT-CHECK-TS-TESTS ( -- )
   s" as" FVT-AS-VECTORS$ FV-TS-TESTS FVT-WANT-TS-AS T$= ;

: FVT-CHECK-RUST-TESTS ( -- )
   s" aa" FVT-AA-VECTORS$ FV-RUST-TESTS FVT-WANT-RUST-AA T$= ;

: FVT-WANT-JS-RT ( -- ptr u8 n )
   SB-RESET
   s" function benchOnce(){" FVT+ FVT-NL+
   s"   void f([3,1,4]);" FVT+ FVT-NL+
   s"   void f([]);" FVT+ FVT-NL+
   s" }" FVT+ FVT-NL+
   s" for (let i = 0; i < 2; i++) benchOnce();" FVT+ FVT-NL+
   s" const __benchStart = process.hrtime.bigint();" FVT+ FVT-NL+
   s" for (let i = 0; i < 5; i++) benchOnce();" FVT+ FVT-NL+
   s" const __benchNs = process.hrtime.bigint() - __benchStart;" FVT+ FVT-NL+
   s" const __benchMs = Number((__benchNs + 999999n) / 1000000n);" FVT+ FVT-NL+
   s" console.log(" FVT+ FVT-DQ+ s" RUNTIME-MS " FVT+ FVT-DQ+ s"  + __benchMs);" FVT+ FVT-NL+
   FVT-WANT$ ;

: FVT-WANT-PY-RT ( -- ptr u8 n )
   SB-RESET
   s" import time" FVT+ FVT-NL+
   s" def bench_once():" FVT+ FVT-NL+
   s"     f([3,1,4])" FVT+ FVT-NL+
   s"     f([])" FVT+ FVT-NL+
   s" for _ in range(2):" FVT+ FVT-NL+
   s"     bench_once()" FVT+ FVT-NL+
   s" __bench_start = time.perf_counter_ns()" FVT+ FVT-NL+
   s" for _ in range(5):" FVT+ FVT-NL+
   s"     bench_once()" FVT+ FVT-NL+
   s" __bench_ns = time.perf_counter_ns() - __bench_start" FVT+ FVT-NL+
   s" __bench_ms = (__bench_ns + 999_999) // 1_000_000" FVT+ FVT-NL+
   s" print(" FVT+ FVT-DQ+ s" RUNTIME-MS " FVT+ FVT-DQ+ s"  + str(__bench_ms))" FVT+ FVT-NL+
   FVT-WANT$ ;

: FVT-WANT-TS-RT ( -- ptr u8 n )
   SB-RESET
   s" function benchOnce(): void {" FVT+ FVT-NL+
   s"   void f([3,1,4]);" FVT+ FVT-NL+
   s"   void f([]);" FVT+ FVT-NL+
   s" }" FVT+ FVT-NL+
   s" for (let i = 0; i < 2; i++) benchOnce();" FVT+ FVT-NL+
   s" const __benchStart = process.hrtime.bigint();" FVT+ FVT-NL+
   s" for (let i = 0; i < 5; i++) benchOnce();" FVT+ FVT-NL+
   s" const __benchNs = process.hrtime.bigint() - __benchStart;" FVT+ FVT-NL+
   s" const __benchMs = Number((__benchNs + 999999n) / 1000000n);" FVT+ FVT-NL+
   s" console.log(" FVT+ FVT-DQ+ s" RUNTIME-MS " FVT+ FVT-DQ+ s"  + __benchMs);" FVT+ FVT-NL+
   FVT-WANT$ ;

: FVT-WANT-RUST-RT ( -- ptr u8 n )
   SB-RESET
   s"     fn bench_once() {" FVT+ FVT-NL+
   s"         std::hint::black_box(f(&[3,1,4]));" FVT+ FVT-NL+
   s"         std::hint::black_box(f(&[]));" FVT+ FVT-NL+
   s"     }" FVT+ FVT-NL+
   s"     for _ in 0..2 { bench_once(); }" FVT+ FVT-NL+
   s"     let __bench_start = std::time::Instant::now();" FVT+ FVT-NL+
   s"     for _ in 0..5 { bench_once(); }" FVT+ FVT-NL+
   s"     let __bench_ms = (__bench_start.elapsed().as_nanos() + 999_999) / 1_000_000;" FVT+ FVT-NL+
   s"     println!(" FVT+ FVT-DQ+ s" RUNTIME-MS {}" FVT+ FVT-DQ+ s" , __bench_ms);" FVT+ FVT-NL+
   FVT-WANT$ ;

: FVT-CHECK-RUNTIME ( -- )
   FVT-RUNTIME-VECTORS$ 2 5 FV-JS-BENCH FVT-WANT-JS-RT T$=
   FVT-RUNTIME-VECTORS$ 2 5 FV-PY-BENCH FVT-WANT-PY-RT T$=
   FVT-RUNTIME-VECTORS$ 2 5 FV-TS-BENCH FVT-WANT-TS-RT T$=
   FVT-RUNTIME-VECTORS$ 2 5 FV-RUST-BENCH FVT-WANT-RUST-RT T$= ;

: FVT-MAIN ( -- )
   T-RESET
   FVT-CHECK-JS-TESTS
   FVT-CHECK-PY-TESTS
   FVT-CHECK-TS-TESTS
   FVT-CHECK-RUST-TESTS
   FVT-CHECK-RUNTIME
   [: FVT-BAD-CONV ;] E-BM-FIELD TTHROWSQ
   [: FVT-BAD-REPS ;] E-BM-FIELD TTHROWSQ
   T-REPORT
   s" foreign-vectors-test: ok" type cr ;

FVT-MAIN
