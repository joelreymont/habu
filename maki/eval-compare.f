\ maki/eval-compare.f - CHECKER ABLATION: Habu-PTX with vs without its static checker.
\
\ This is the COMPLEMENTARY, internal experiment to the real Habu-PTX-vs-Triton eval
\ matrix (now run on the Orin; see docs/eval-triton.md). The vs-Triton comparison has
\ a confound: Triton is a different compiler, language, and launch path. This ablates
\ the ONE variable in isolation - Habu-PTX with vs without its own static checker - so
\ the checker's contribution is attributable. With the static checker, a type/stack
\ bug is rejected at AUTHOR time with a located diagnostic and no GPU; without any
\ static check, the same bug is found only by emitting, assembling, and running -
\ later, unlocated, GPU-costly, and a plausible-looking wrong number can slip through.
\ GRADE-CANDIDATE's verdict encodes the with-checker arm: 0 = checker-rejected
\ (caught statically, NO device run), 1 = certifies-but-device-wrong (semantic,
\ needs a run), 2 = green. GRADE-NOCHECK-CANDIDATE encodes the without-checker arm:
\ every candidate enters the emit -> ptxas -> device path, then lands in emit-fail,
\ ptxas-fail, device-wrong, or green. The real-Triton matrix confirms the same
\ mechanism against the actual target: Triton catches name/type errors at compile
\ but the stack-discipline class only at runtime. Load after maki/eval-device.f.

\ eval-compare reopens package EVAL as the CMP- ablation module. Its verdict-source
\ words (GRADE-CANDIDATE / GRADE-NOCHECK-CANDIDATE / EVN-*) live in the same package
\ (maki/eval-device.f), so they resolve bare here; the CMP- state + API and the
\ device fixture stay private to this file.
package EVAL

private

variable NC0  variable NC1  variable NC2     \ counts of verdict 0 / 1 / 2
variable NU-EMIT  variable NU-PTXAS  variable NU-WRONG  variable NU-GREEN

: CMP-RESET ( -- )
   0 NC0 !  0 NC1 !  0 NC2 !
   0 NU-EMIT !  0 NU-PTXAS !  0 NU-WRONG !  0 NU-GREEN ! ;

: CMP-CHECKED-RECORD ( n -- )
   dup 0 = if NC0 @ 1+ NC0 ! then
   dup 1 = if NC1 @ 1+ NC1 ! then
        2 = if NC2 @ 1+ NC2 ! then ;

: CMP-NOCHECK-RECORD ( n -- )
   case
      EVN-EMIT-FAIL of NU-EMIT @ 1+ NU-EMIT ! endof
      EVN-PTXAS-FAIL of NU-PTXAS @ 1+ NU-PTXAS ! endof
      EVN-DEVICE-WRONG of NU-WRONG @ 1+ NU-WRONG ! endof
      EVN-GREEN of NU-GREEN @ 1+ NU-GREEN ! endof
   endcase ;

: CMP-SCORE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u GRADE-CANDIDATE CMP-CHECKED-RECORD
   a u GRADE-NOCHECK-CANDIDATE CMP-NOCHECK-RECORD ;

: CMP-TOTAL  ( -- n )  NC0 @ NC1 @ + NC2 @ + ;
: CMP-BUGS   ( -- n )  NC0 @ NC1 @ + ;            \ rejected + semantic-wrong
: CMP-STATIC ( -- n )  NC0 @ ;                    \ bugs the checker caught before any run
: CMP-HB-RUNS ( -- n )  NC1 @ NC2 @ + ;           \ Habu-PTX runs only the CERTIFIED candidates
: CMP-NOCHECK-RUNS ( -- n )  NU-EMIT @ NU-PTXAS @ + NU-WRONG @ + NU-GREEN @ + ;
: CMP-NOCHECK-LATE-FAILS ( -- n )  NU-EMIT @ NU-PTXAS @ + NU-WRONG @ + ;

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
\ the unchecked arm attempts every candidate, including checker rejects
CMP-NOCHECK-RUNS  9 T=
NU-EMIT @  0 T=
NU-PTXAS @  0 T=
NU-WRONG @  6 T=
NU-GREEN @  3 T=
CMP-NOCHECK-LATE-FAILS  6 T=

s" === checker ablation: Habu-PTX WITH vs WITHOUT its static checker ===" type cr
s" fixture: " type CMP-TOTAL . s" candidates, " type CMP-BUGS . s" bugs (" type NC0 @ . s" type/stack + " type NC1 @ . s" semantic)" type cr
s" WITH checker:    " type CMP-STATIC . s" / " type CMP-BUGS . s" bugs caught BEFORE execution (located diagnostics); GPU runs = " type CMP-HB-RUNS . cr
s" WITHOUT checker: 0 / " type CMP-BUGS . s" bugs caught before execution; attempted emit/ptxas/device = " type CMP-NOCHECK-RUNS . cr
s" WITHOUT checker later failures: emit=" type NU-EMIT @ . s" ptxas=" type NU-PTXAS @ . s" device-wrong=" type NU-WRONG @ . s" green=" type NU-GREEN @ . cr
s" => the static checker catches the type/stack bug class for free; that is the checker's value. Confirmed vs real Triton on the Orin (docs/eval-triton.md): Triton catches name/type errors at compile but the stack-discipline class only at runtime (3/5 battery bugs slipped to runtime)." type cr

T-REPORT

;package
