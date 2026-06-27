\ maki/eval-compare.f - the comparative eval: checked Habu-PTX vs a runtime-only
\ (Triton-class) baseline, isolating the thesis variable - the static stack-effect checker.
\
\ The thesis is "checked kernels are a better LLM target than Triton". Triton itself
\ is unavailable here (no triton/torch, not installable offline), but the variable
\ under test is the STATIC checker: with it, a type/stack bug is rejected at AUTHOR
\ time with a located diagnostic and no GPU; without it (the Triton-class experience
\ for this bug class) the same bug is found only by emitting, assembling, and running
\ - or worse, slips through as a plausible-looking wrong number. GRADE-CANDIDATE's
\ verdict already encodes this: 0 = checker-rejected (caught statically, NO device
\ run), 1 = certifies-but-device-wrong (semantic, needs a run), 2 = green. So we tally
\ a fixture and report, per arm, how many bugs are caught BEFORE execution and how
\ many GPU runs the loop costs. Honest caveat: a controlled isolation of the checker
\ variable, not a full Triton port (language/perf differ). Load after maki/eval-device.f.

variable NC0  variable NC1  variable NC2     \ counts of verdict 0 / 1 / 2
: CMP-RESET ( -- )  0 NC0 !  0 NC1 !  0 NC2 ! ;
: CMP-SCORE ( ptr u8 n -- )
   GRADE-CANDIDATE
   dup 0 = if NC0 @ 1+ NC0 ! then
   dup 1 = if NC1 @ 1+ NC1 ! then
        2 = if NC2 @ 1+ NC2 ! then ;
: CMP-TOTAL  ( -- n )  NC0 @ NC1 @ + NC2 @ + ;
: CMP-BUGS   ( -- n )  NC0 @ NC1 @ + ;            \ rejected + semantic-wrong
: CMP-STATIC ( -- n )  NC0 @ ;                    \ bugs the checker caught before any run
: CMP-HB-RUNS ( -- n )  NC1 @ NC2 @ + ;           \ Habu-PTX runs only the CERTIFIED candidates

T-RESET
CMP-RESET
\ fixture: 3 correct SAXPY phrasings, 5 distinct type/stack errors, 1 semantic (x+y)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  CMP-SCORE  \ correct
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE +. y g STORE"  CMP-SCORE  \ correct
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} y g LOAD x g LOAD a SCALE swap drop drop x g LOAD a SCALE y g LOAD +. y g STORE"  CMP-SCORE  \ correct
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."             CMP-SCORE  \ type/stack: no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +. y g STORE"   CMP-SCORE  \ type/stack: span as uniform
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x x LOAD a SCALE y x LOAD +. y x STORE"                      CMP-SCORE  \ type/stack: span as gridctx
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE +. y g STORE"             CMP-SCORE  \ type/stack: +. underflow
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE a SCALE"  CMP-SCORE  \ type/stack: extra op
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y g LOAD +. y g STORE"           CMP-SCORE  \ SEMANTIC: x+y (certifies, device-wrong)

\ fixture shape: 3 green, 5 type/stack rejects, 1 semantic
NC2 @  3 T=
NC0 @  5 T=
NC1 @  1 T=
\ the checker caught the 5 type/stack bugs with ZERO device runs; the semantic one needs a run
CMP-STATIC  5 T=

s" === comparative eval: checked Habu-PTX vs runtime-only (Triton-class) baseline ===" type cr
s" fixture: " type CMP-TOTAL . s" candidates, " type CMP-BUGS . s" bugs (" type NC0 @ . s" type/stack + " type NC1 @ . s" semantic)" type cr
s" Habu-PTX (static checker): " type CMP-STATIC . s" / " type CMP-BUGS . s" bugs caught BEFORE execution (located diagnostics); GPU runs = " type CMP-HB-RUNS . cr
s" Triton-class (runtime-only): 0 / " type CMP-BUGS . s" bugs caught before execution; GPU runs = " type CMP-TOTAL . s"  (must run every candidate)" type cr
s" => the checker catches the type/stack bug class statically; only the semantic bug needs a device run on either side." type cr

T-REPORT
bye
