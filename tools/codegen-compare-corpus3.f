\ codegen-compare-corpus3.f - the THIRD pinned word corpus the codegen
\ comparison measures: ten float words taken from the places this system does
\ its float arithmetic. One concern: the subject words themselves, and the
\ engine's float semantics they are measured under.
\
\ WHY A THIRD CORPUS AND NOT MORE ROWS IN THE SECOND. The same reason the second
\ is not more rows in the first, and it is written out at the head of
\ tools/codegen-compare-corpus2.f: a committed table is the yardstick a compiler
\ change is read against, and adding a row to one is a change to the artifact the
\ measurement is compared with. This file is a corpus of its own with a committed
\ table of its own, measured by the same engine in the same run. Neither of the
\ first two tables moves.
\
\ WHY IT EXISTS AT ALL. The next campaign is floats, and the benchmark comes
\ first: the old emitter's numbers for the real float bodies are measured and
\ committed BEFORE any float capability is built, so the day the chain compiles
\ one there is something to compare it with that nobody chose after the fact.
\ Every row of the new column is a gap today, naming the `floats` capability -
\ the old comparison declared them and its account refused a pass in which a
\ corpus word was neither
\ compiled nor declared.
\
\ ============================================================================
\ THE SEMANTICS SURVEY - THE CONTRACT THE FLOAT COMPILER HAS TO MATCH
\ ============================================================================
\ A benchmark for a compiler that does not exist yet is also the specification
\ it will be judged against, so what the engine does with floats today is
\ written down here, with the evidence for each fact: the engine source that
\ decides it, and where a reader could reasonably doubt the source, the answer
\ this engine actually printed. Every number below was produced by bin/hb.
\
\ (1) REPRESENTATION: ONE UNBOXED CELL, THE RAW IEEE754 BITS.
\ A double lives in one 64-bit data-stack cell as its own bit pattern. There is
\ no separate float stack, no box and no tag: the engine's float primitives move
\ the cell straight into a floating register and back
\ (src/habu/habu1.f, BF+: `B G-POP A G-POP 0 A FMOVXD, 1 B FMOVXD,
\ 0 0 1 FADD, A 0 FMOVDX, A G-PUSH`). Read through the checked projection
\ CODEGEN-COMPARE:REAL-BITS:
\
\     1.0   -> 4607182418800017408   ($3FF0000000000000)
\     0.5   -> 4602678819172646912   ($3FE0000000000000)
\     0.0   -> 0
\     -0.0  -> -9223372036854775808  ($8000000000000000)
\
\ That is why this corpus can record a float output at all, and why it records
\ the CELL rather than a rounded decimal: see (7) for what the comparison means.
\
\ (2) THE WHOLE FLOAT VOCABULARY, AND THE INSTRUCTION BEHIND EACH WORD.
\ src/habu/habu1.f EMIT-FP-PRIMS publishes exactly fifteen words. Fourteen of
\ them are one AArch64 instruction wrapped in a pop and a push; the fifteenth,
\ f., is a whole decimal printer and no part of the arithmetic:
\
\     f+  FADD      f-  FSUB      f*  FMUL      f/  FDIV
\     fnegate FNEG  fabs FABS     fsqrt FSQRT
\     f<  FCMP + CSET MI          f>  FCMP + CSET GT
\     f=  FCMP + CSET EQ
\     f0< FCMP #0.0 + CSET MI     f0= FCMP #0.0 + CSET EQ
\     s>f SCVTF     f>s FCVTZS    f.  (the decimal printer)
\
\ There is no f<=, no f>=, no fmin, no fmax, no float remainder and no
\ transcendental: lib/fmath.f builds exp and ln out of these in Habu, and
\ maki/fmath.f builds the activation suite on top of that. A compiler that
\ models this list models the whole float surface.
\
\ FOUR OF THEM ARE INLINED IN COMPILED CODE AND THE REST ARE CALLS.
\ src/habu/habu2.f EM-COMPILE-FLOAT-OPS gives a compile-state entry to f+, f-,
\ f* and f/ only, with the encodings $1E602800, $1E603800, $1E600800 and
\ $1E601800; every other float word compiles to an ordinary call to the
\ primitive above. Measured, as bytes of machine code for a one-operation body
\ against the 16 bytes an empty word costs:
\
\     : W ( r r -- r ) f+ ;      56 bytes      : W ( r r -- bool ) f< ;   20
\     : W ( r r -- r ) f* ;      56 bytes      : W ( r -- bool ) f0< ;    48
\     : W ( r -- r ) fsqrt ;     44 bytes      : W ( n -- r ) s>f ;       40
\     : W ( n n -- n ) + ;       44 bytes      : W ( r -- n ) f>s ;       40
\
\ f< at four bytes over an empty word is the single call the source predicts.
\ The rest of each number is the engine's own stack machinery, which this file
\ does not model and does not have to: what it pins is the whole word.
\
\ (3) A COMPARISON ANSWERS A HABU FLAG, NOT A ZERO OR A ONE.
\ (FCMP) reads `A FP-COND @ CSET, A SP A SUB,`: CSET leaves 0 or 1 and the
\ subtraction from the zero register turns it into 0 or -1, which is what every
\ other Habu predicate answers. `0.0 fnegate f0=` printed -1.
\
\ (4) NaN COMPARES FALSE, EVERY WAY ROUND, AND THAT DECIDES A BRANCH.
\ AArch64 FCMP raises the unordered condition, and MI, GT and EQ are all false
\ when it is set, so every float comparison this engine has answers FALSE if
\ either operand is NaN. Printed, with NaN spelled `0.0 0.0 f/`:
\
\     nan f< nan   0        nan f< 1.0   0        1.0 f< nan   0
\     nan f> 1.0   0        1.0 f> nan   0        nan f= nan   0
\     nan f0<      0        nan f0=      0
\
\ The consequence is a control-flow fact, not a numeric one: `x f0< if A else B
\ then` takes the ELSE arm when x is NaN. A compiler that lowered f0< as "not
\ (x >= 0)" would take the other one, and the RELU-F row below is pinned on a
\ NaN input for exactly that reason.
\
\ (5) THE NaN THIS ENGINE PRODUCES IS ONE VALUE, AND IT IS DETERMINISTIC.
\ Every invalid operation answers the AArch64 default NaN,
\ $7FF8000000000000 = 9221120237041090560:
\
\     0.0 0.0 f/    -> 9221120237041090560
\     -1.0 fsqrt    -> 9221120237041090560
\     inf inf f-    -> 9221120237041090560
\
\ It is the same bit pattern from all three, so a NaN row is a pinnable fact on
\ this engine rather than platform luck, and this corpus pins it. It is worth
\ saying what that rests on: the engine emits AArch64 and only AArch64, and the
\ default NaN is AArch64's. A float compiler for another target has to answer
\ this question again rather than inherit the number.
\
\ (6) ZERO, INFINITY AND THE SIGN OF ZERO.
\ Division by zero does not trap and does not throw; it answers an infinity:
\
\     1.0 0.0 f/   ->  9218868437227405312   ($7FF0000000000000, +inf)
\     -1.0 0.0 f/  -> -4503599627370496      ($FFF0000000000000, -inf)
\     0.0 0.0 f/   ->  the default NaN
\
\ Negative zero exists, is written `-0.0` or reached with `0.0 fnegate`, and is
\ EQUAL to zero as a number while being a different cell:
\
\     -0.0 f0=      -1          -0.0 f0<       0
\     -0.0 f= 0.0   -1          -0.0 fsqrt  -> -0.0
\
\ (7) WHAT EQUALITY THIS CORPUS RECORDS, AND WHY.
\ A row's outputs are the CELLS the word left, compared exactly. For doubles
\ that is honest and it is the strictest of the available comparisons: two cells
\ that differ by one bit are two different doubles, and two code generators that
\ agree on every bit agree on everything a caller could observe. It is strictly
\ finer than float equality in two places, and both are deliberate:
\ +0.0 and -0.0 are equal numbers and different cells, and no NaN is equal to
\ itself while two NaNs of the same payload are the same cell. A compiler that
\ turned -0.0 into 0.0, or that produced a different NaN payload, would be
\ reported here - and should be.
\
\ (8) CONVERSION: TWO WORDS, TWO DIFFERENT ROUNDINGS.
\ s>f is SCVTF, which rounds to nearest with ties to even, and is exact up to
\ 2^53; f>s is FCVTZS, which truncates toward zero, saturates instead of
\ wrapping, and answers zero for NaN:
\
\     9007199254740993 s>f     -> 4845873199050653696, the double 2^53
\     9007199254740993 s>f f>s -> 9007199254740992
\     2.7 f>s   ->  2        -2.7 f>s  -> -2        -0.5 f>s -> 0
\     inf f>s   ->  9223372036854775807
\     -inf f>s  -> -9223372036854775808
\     nan f>s   ->  0
\
\ Truncation is why lib/fmath.f has FROUND at all, and FROUND is a row below.
\
\ (9) FLOAT ADDITION IS NOT ASSOCIATIVE, AND THE ENGINE'S ORDER IS A LEFT FOLD.
\ Every accumulation in maki opens with 0.0 and adds one element per turn in
\ ascending index order - `0.0 len 0 ?do base i T-GET f+ loop` - so the value is
\ ((0.0 + x0) + x1) + ... A compiler that reassociated, vectorised or reordered
\ would compute a different number for the same input. Pinned, and measured on
\ the sum vector this file owns, [2^53, 1.0, 1.0]:
\
\     0.0 2^53 f+ 1.0 f+ 1.0 f+   -> 4845873199050653696   (2^53)
\     0.0 1.0 1.0 f+ 2^53 f+ f+   -> 4845873199050653697   (2^53 + 2)
\
\ The two orders differ in the recorded cell, which is what makes the T-SUM row
\ a statement about the order and not only about the arithmetic. SUM-REVERSED
\ below is that second order, kept as a witness so the claim is checked rather
\ than described: the old comparison's member asserted the two disagree.
\
\ (10) THE LITERAL READER, AND TWO THINGS A CONSTANT-MATERIALISING COMPILER
\ MUST KNOW. The engine reads `int.frac` and nothing else - there is no
\ exponent form - and computes the value as int + frac/10^k in double
\ arithmetic (src/habu/habu1.f, C-NUM-FLOAT-FINISH: two SCVTFs, one FDIV, one
\ FADD). Two consequences, both measured:
\
\   a. Up to three roundings, so a literal is not always the nearest double to
\      the decimal it spells. `1.9482199351819093` reads as
\      4611452821746767930 where the nearest double is 4611452821746767931,
\      and `0.11471049746507529` reads one below its nearest double too.
\   b. The fraction is accumulated in an integer cell, so past eighteen
\      fractional digits it wraps and the value is silently wrong - and can
\      even change sign: `0.1234567890123456789` reads as the cell
\      -4628938082669329042, a negative number. Dot
\      habu-fix-the-float-1d1467c8 carries the repair; nothing in this
\      corpus depends on it, because every literal pinned here is short and
\      exactly representable.
\
\ (11) WHAT STOPPED THE NATIVE CHAIN WHEN THIS CORPUS WAS COMMITTED, WHICH IS WHY
\ EVERY ROW OF IT STARTED AS A GAP. Two refusals, in two different stages, both
\ reached by handing a float body to the migration the way
\ tools/codegen-compare-migrated2.f hands it an integer one:
\
\     : FADD-N ( r r -- r ) f+ ;      threw -8286  E-HIR-UNMODELED
\     : FLIT-N ( r -- r ) 1.0 f+ ;    threw -8404  E-NFEED-KIND
\
\ The tape had no kind for a real literal (src/compiler/native/feed.f says so in
\ as many words) and the dialect had no model for a float operation. The `floats`
\ capability the gap rows named covered both, because a row that named only one
\ of them would have read as half a job.
\
\ ALL TEN ARE COMPILED NOW, and where the chain stands is recorded in
\ the old comparison's own notes rather than here: this file is the pinned corpus
\ and the semantics it is measured under, and both of those are facts about the
\ ENGINE that do not move when the compiler gains a capability. What closed the
\ ten was three leaves - scalar float arithmetic, the five comparisons, and the
\ placement of a double across a block edge, a call and a loop's back edge - and
\ the two refusals above are gone rather than worked around: the same two bodies
\ compile.
\
\ ============================================================================
\ THE CORPUS
\ ============================================================================
\
\ WHERE THESE TEN COME FROM, AND WHAT SHAPE EACH ONE IS. Every body below is a
\ real word of this system, copied verbatim, and each is here because it is a
\ float SHAPE the other two corpora do not have:
\
\   T-SUM        the plain accumulation: one buffer, one accumulator, one f+ per
\                turn. maki/array.f:16.
\   T-DIST2      the two-pointer accumulation: two buffers walked together, a
\                subtraction, a square through `dup f*`, and the accumulator.
\                maki/array.f:32.
\   T-NORM2      the one-pointer square accumulation - the same fold with one
\                load instead of two, which is the shape a norm has.
\                maki/array.f:39.
\   T-SGD!       the in-place mutation loop: read a weight, read a gradient,
\                scale and subtract, store the weight back. Its point is the
\                stores. maki/array.f:20.
\   T-REL-L2     two accumulations, two square roots and a division, reached
\                through two calls. maki/array.f:46.
\   RELU-F       a scalar branch on a float compare against zero.
\                maki/autograd.f:23.
\   MAX-F        a two-armed branch on a two-operand float compare - the float
\                twin of the second corpus's MAX-DIM. maki/autograd.f:48.
\   SGD          straight-line scalar float arithmetic over a locals frame: one
\                multiply, one subtract, no branch and no loop. It is this
\                corpus's ADD3. maki/optim.f:12.
\   SEG-1/SQRT   the int-to-float conversion, in the body that really does it:
\                a length becomes a double, is square-rooted and divided into
\                one. maki/segment.f:61.
\   FROUND       the float-to-int conversion, in the body that really does it:
\                a sign-aware bias and then f>s. lib/fmath.f:36.
\
\ WHY THE CONVERSION SHAPE IS TWO ROWS AND NOT ONE. The two directions do not
\ round the same way - s>f is nearest-even and f>s truncates toward zero (survey
\ (8)) - and no single row can pin two roundings. Both rows are real bodies;
\ inventing a round-trip word to make one row out of them would have measured a
\ program nothing runs.
\
\ WHY THERE IS NO EXPONENTIAL OR SIGMOID ROW. Those bodies call FEXP, which is a
\ range reduction plus a degree-six polynomial plus a power-of-two loop
\ (lib/fmath.f), so their row would measure a whole subsystem rather than a
\ float shape. They are the natural second float corpus once the chain compiles
\ this one; the dot for this leaf names them as out of scope on purpose.
\
\ ONE ROW IS SHORT OF WHAT THE DOT ASKED FOR, AND HERE IS THE HONEST ACCOUNT.
\ The dot describes row four as "T-NORM2 accumulation + fsqrt". The real
\ T-NORM2 has no fsqrt in it - it is the sum of squares and nothing else - and
\ adding one would have made the row a body maki does not run. T-NORM2 goes in
\ verbatim, and the accumulate-then-square-root shape is T-REL-L2, which does it
\ twice over two accumulations and divides the results.
\
\ THE CORPUS IS PINNED, exactly as the first two are. Do not "improve" a word
\ here. Adding a word adds a row; changing a body invalidates the recorded
\ baseline for that word and must be done in the same change that regenerates
\ it, with the reason written down.
\
\ THE ROW WHOSE POINT IS A SIDE EFFECT is T-SGD!, and the weight buffer it steps
\ is this file's own storage, published for reading. Both columns of the
\ comparison will step the SAME memory, so the head-to-head check is about the
\ loads and the stores and not only about the arithmetic between them - the
\ discipline CELL-BUMP established in the first corpus. Because a step is not
\ idempotent, the buffer is refilled by W-RESET before the recorded call, so a
\ second measurement pass records what the first one did rather than the state
\ the timing loop left behind. The timed body calls the subject alone, as every
\ other row's does.

require lib/prelude.f

package CODEGEN-CORPUS3

public

\ ---- how much of the pinned data each row is measured over --------------------
\ Public because the case list hands these lengths to the subjects, the way the
\ second corpus publishes COPY-LEN: the length a row is measured at is part of
\ its pinned input and belongs beside the data rather than retyped in another
\ file.
4 constant VEC-LEN
3 constant SUM-LEN
5 constant W-CELLS

private

\ ---- the pinned data the rows are measured over ------------------------------
\ Four cells each, and every value exactly representable, so a recorded output
\ says something about the arithmetic rather than about the literal reader
\ (survey (10)). The pinned inputs carry a negative, a zero and a value on
\ either side of one, because a row measured only on positive whole numbers
\ cannot see a sign that went missing.
create VEC-A VEC-LEN cells allot          \ -2.5  0.0  1.5  0.25
create VEC-B VEC-LEN cells allot          \  1.0 -1.0  1.5  0.0
create VEC-Z VEC-LEN cells allot          \ all zero: the degenerate reference

\ The sum vector, chosen so that summing it in another order gives another
\ answer - survey (9). Its first element is 2^53, where the gap between one
\ double and the next is 2, so adding one to it changes nothing and adding two
\ does.
create SUM-VEC SUM-LEN cells allot        \ 2^53  1.0  1.0

\ The weight buffer T-SGD! steps and the gradients it steps against. The buffer
\ holds one cell more than the step touches, and that cell is left alone on
\ purpose: a loop that ran one turn too far shows up as a changed output rather
\ than as nothing at all.
create VEC-W W-CELLS cells allot          \ 1.0 -1.0 0.5 0.0 | 2.5 untouched
create VEC-G VEC-LEN cells allot          \ 0.5 0.5 -2.0 3.0

public

\ ---- the ten subjects --------------------------------------------------------

\ maki/array.f:9-11, verbatim: the three words every tensor body reaches an
\ element through. They are callees rather than rows - what they are is one
\ address computation and one memory access, which the first corpus already
\ measures in CELL-BUMP - but the rows that call them are the real two-word
\ programs because these are here.
: T-AT  ( ptr a n -- ptr a )  cells + ;
: T-GET ( ptr a n -- r )      T-AT @ ;
: T-SET ( r ptr a n -- )      T-AT ! ;

\ maki/array.f:16, verbatim, bare locals and all - the corpus is pinned to the
\ surveyed body and an annotation would be an edit to it.
\ typed-local-lint: allow-bare-local - base and len are maki/array.f:16's own
\ spelling; this corpus records the body that runs, not a rewrite of it.
: T-SUM ( ptr a n -- r ) {: base len :}
   0.0  len 0 ?do  base i T-GET f+  loop ;

\ maki/array.f:20, verbatim. The in-place step: w[i] -= lr * g[i].
\ typed-local-lint: allow-bare-local - lr, wbase, gbase and len are
\ maki/array.f:20's own spelling.
: T-SGD! ( r ptr a ptr a n -- ) {: lr wbase gbase len :}
   len 0 ?do
      wbase i T-GET   lr  gbase i T-GET  f*  f-   wbase i T-SET
   loop ;

\ maki/array.f:32, verbatim: sum_i (a[i]-b[i])^2.
: T-DIST2 ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :}
   0.0
   len 0 ?do
      abase i T-GET  bbase i T-GET  f-  dup f*  f+
   loop ;

\ maki/array.f:39, verbatim: sum_i b[i]^2.
: T-NORM2 ( ptr a n -- r ) {: bbase:ptr len:n :}
   0.0
   len 0 ?do
      bbase i T-GET  dup f*  f+
   loop ;

\ maki/array.f:46, verbatim over the two rows above: ||a-b|| / ||b||. Two calls,
\ two square roots and a division in one straight line.
: T-REL-L2 ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :}
   abase bbase len T-DIST2 fsqrt
   bbase len T-NORM2 fsqrt
   f/ ;

\ maki/autograd.f:23, verbatim, bare local and all.
\ typed-local-lint: allow-bare-local - x is maki/autograd.f:23's own spelling.
: RELU-F ( r -- r ) {: x :}   x f0< if 0.0 else x  then ;

\ maki/autograd.f:48, verbatim: the two-armed branch on a two-operand compare.
: MAX-F ( r r -- r ) {: x:r y:r :}  x y f< if y else x then ;

\ maki/optim.f:12, verbatim: w' = w - lr*g, the smallest honest piece of
\ straight-line float arithmetic this system runs.
\ typed-local-lint: allow-bare-local - w, g and lr are maki/optim.f:12's own
\ spelling.
: SGD ( r r r -- r ) {: w g lr :}
   w  lr g f*  f- ;

\ maki/segment.f:61, verbatim: an integer length becomes a double, and 1/sqrt(d)
\ comes back.
: SEG-1/SQRT ( n -- r ) {: d:n :}  1.0 d s>f fsqrt f/ ;

\ lib/fmath.f:36, verbatim: round to nearest, written the way a truncating f>s
\ makes necessary.
: FROUND ( r -- n )  dup f0< if 0.5 f- else 0.5 f+ then f>s ;

\ ---- what the harness reads the pinned data through --------------------------

: A-VEC ( -- ptr r ) VEC-A ;
: B-VEC ( -- ptr r ) VEC-B ;
: Z-VEC ( -- ptr r ) VEC-Z ;
: S-VEC ( -- ptr r ) SUM-VEC ;
: W-VEC ( -- ptr r ) VEC-W ;
: G-VEC ( -- ptr r ) VEC-G ;

\ The step's learning rate, as a word rather than a constant because `constant`
\ carries an integer.
: STEP-LR ( -- r ) 0.25 ;

\ One cell of the weight buffer, read the checked way through the corpus's own
\ accessor. This is what both columns verify a step through.
: W-CELL ( n -- r ) {: k:n :}
   VEC-W k T-GET ;

private

\ The pinned fill, in one place, so that the buffer a step is recorded over is
\ the same buffer every time - the timing loop below runs the step a million
\ times and leaves the weights far from where they started, which is harmless
\ precisely because the recorded call refills them first.
: FILL-W ( -- )
   1.0   VEC-W 0 T-SET
   -1.0  VEC-W 1 T-SET
   0.5   VEC-W 2 T-SET
   0.0   VEC-W 3 T-SET
   2.5   VEC-W 4 T-SET ;

public

: W-RESET ( -- )
   FILL-W ;

\ The other order, kept as the witness that the sum row's pinned input
\ distinguishes one evaluation order from another (survey (9)). It is not a row
\ and it is not a subject: it exists so a member can assert
\ that it disagrees with what T-SUM recorded, which turns the claim into a check.
: SUM-REVERSED ( -- r )
   0.0
   SUM-LEN 0 ?do
      SUM-VEC  SUM-LEN 1- i -  T-GET f+
   loop ;

private

: FILL-A ( -- )
   -2.5  VEC-A 0 T-SET
   0.0   VEC-A 1 T-SET
   1.5   VEC-A 2 T-SET
   0.25  VEC-A 3 T-SET ;

: FILL-B ( -- )
   1.0   VEC-B 0 T-SET
   -1.0  VEC-B 1 T-SET
   1.5   VEC-B 2 T-SET
   0.0   VEC-B 3 T-SET ;

: FILL-Z ( -- )
   VEC-LEN 0 ?do  0.0 VEC-Z i T-SET  loop ;

: FILL-G ( -- )
   0.5   VEC-G 0 T-SET
   0.5   VEC-G 1 T-SET
   -2.0  VEC-G 2 T-SET
   3.0   VEC-G 3 T-SET ;

\ 2^53, where one double and the next are two apart, then two ones.
: FILL-SUM ( -- )
   9007199254740992.0  SUM-VEC 0 T-SET
   1.0                 SUM-VEC 1 T-SET
   1.0                 SUM-VEC 2 T-SET ;

public

: SETUP ( -- )
   FILL-A
   FILL-B
   FILL-Z
   FILL-G
   FILL-SUM
   FILL-W ;

SETUP

;package
