\ maki/eval-fixture.f - the checker-as-judge eval over a Habu-PTX kernel-authoring
\ fixture (the eval matrix's pass@k arm, complementing the GB/s arm in
\ tools/ptx/bandwidth.f). The task is "author a SAXPY tile-DSL kernel"; the fixture
\ is 8 candidate kernels - 3 correct phrasings and 5 with DISTINCT type/stack
\ authoring errors. maki/eval.f's CHECK-PASSES? (the checker as the correctness
\ judge) certifies the 3 correct candidates and rejects every error category, so
\ pass = 3 / total = 8, pass@1 holds. This is the Habu-PTX side of the comparison;
\ the live-LLM generation + Triton arm (tokens-to-green, repair rounds) is external.
require lib/ptx/test-prelude.f
require maki/eval.f

T-RESET

\ --- the judge discriminates each candidate (correct certifies, each error rejects) ---
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"            CHECK-PASSES? TTRUE   \ c1 standard
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE +. y g STORE"            CHECK-PASSES? TTRUE   \ c2 reordered (a*x+y commutes)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE swap drop drop x g LOAD a SCALE y g LOAD +. y g STORE" CHECK-PASSES? TTRUE   \ c3 verbose-but-typed

s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."                       CHECK-PASSES? TFALSE  \ b1 missing STORE (stack imbalance)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +. y g STORE"             CHECK-PASSES? TFALSE  \ b2 SCALE given a span, not a uniform
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x x LOAD a SCALE y x LOAD +. y x STORE"                                CHECK-PASSES? TFALSE  \ b3 LOAD given a span, not a gridctx
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE +. y g STORE"                       CHECK-PASSES? TFALSE  \ b4 +. underflow (one tile, needs two)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE a SCALE"      CHECK-PASSES? TFALSE  \ b5 extra op after STORE (underflow)

\ --- batch pass@k: 8 candidates, exactly 3 certify ---
EV-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"            EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE +. y g STORE"            EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE swap drop drop x g LOAD a SCALE y g LOAD +. y g STORE" EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."                       EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +. y g STORE"             EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x x LOAD a SCALE y x LOAD +. y x STORE"                                EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE +. y g STORE"                       EV-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE a SCALE"      EV-SCORE
EV-TOTAL @  8 T=
EV-PASS  @  3 T=
EV-PASS@1?  TTRUE

s" Habu-PTX SAXPY checker-as-judge eval: pass=" type EV-PASS @ . s" / total=" type EV-TOTAL @ . s"  pass@1=yes" type cr

T-REPORT
