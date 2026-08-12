\ judge/corpus3.f - the judged rows of tools/codegen-compare-corpus3.f: ten
\ float shapes taken from the places this system does its float arithmetic. One
\ concern: which subject is judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES is what tools/judge/corpus4.f states, plus one thing
\ those corpora do not need: the C CALL SHAPE of each twin. A twin whose
\ arguments are all integers and whose answer is one is reached through the shape
\ the subject's own arity gives, and nobody writes a number down. These ten take
\ and answer doubles, which live in a different register bank, so there is no
\ arity to read the shape off and the row states it - `IID`, `DIII`, `DD` - the
\ way tools/codegen-compare-cabi.f spells it.
\
\ THE ANSWERS ARE COMPARED BIT FOR BIT. A double is one unboxed cell holding its
\ own IEEE754 bit pattern (the survey at the head of the corpus file establishes
\ that from the engine's source and from what it prints), so a generated body
\ projects it through CODEGEN-COMPARE:REAL-BITS - the one route the comparison
\ already records floats by - and the columns are held against the whole cell.
\ Two cells that differ by a bit are two different doubles: a column that
\ reassociated a sum, flushed a subnormal, lost the sign of a zero or produced
\ another NaN payload is a disagreement here rather than a faster number.
\
\ THE POINTERS ARE THE ONE THING WRITTEN TWICE, and it is not a choice: the twin
\ is a different program and cannot share the corpus's buffers, so it carries its
\ own filled from the same constants by hc3_setup. Every LENGTH and every
\ learning rate is the corpus's own word, named once and pushed in both worlds -
\ a reference body is a habu body that makes a foreign call, so it can push a
\ habu constant.
\
\ T-SGD! LEAVES NOTHING AND IS NOT IDEMPOTENT. What it does is write, so its row
\ refills the weight buffer before the call and reads it back after: the five
\ cells, four the step touches and the fifth it must not. Without that its two
\ columns would be agreeing that zero equals zero.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus3.f
require tools/judge/pass.f

package JUDGE-CORPUS3

private

variable A-PTR
variable B-PTR
variable W-PTR
variable G-PTR

public

\ The twins' own buffers, asked for once each: a generated TIMING body runs its
\ inputs a quarter of a million times, and a foreign call inside that loop would
\ be timed as part of the row. Each reader touches its own cell, which is the
\ shape a `ptr` local cannot carry - docs/forth.md, ptr locals and cell access.
: C-A ( -- n )
   A-PTR @ 0<> if A-PTR @ exit then
   s" hc3_a_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup A-PTR ! ;

: C-B ( -- n )
   B-PTR @ 0<> if B-PTR @ exit then
   s" hc3_b_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup B-PTR ! ;

: C-W ( -- n )
   W-PTR @ 0<> if W-PTR @ exit then
   s" hc3_w_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup W-PTR ! ;

: C-G ( -- n )
   G-PTR @ 0<> if G-PTR @ exit then
   s" hc3_g_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup G-PTR ! ;

\ The twins' own refill of the weight buffer, which is what the corpus's W-RESET
\ is in the habu world.
: C-W-RESET ( -- )
   s" hc3_w_reset" CODEGEN-CABI:FN CODEGEN-CABI:I0 drop ;

private

: W-BITS ( n -- n )
   CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:REAL-BITS ;

: C-W@ ( n -- n )
   s" hc3_w_get" CODEGEN-CABI:FN CODEGEN-CABI:ID CODEGEN-COMPARE:REAL-BITS ;

public

\ What the step left in the weight buffer: the four cells it touches and the
\ fifth it must not, each as the cell a double already is.
: W-READ ( -- n )
   0 W-BITS
   1 W-BITS xor
   2 W-BITS xor
   3 W-BITS xor
   4 W-BITS xor ;

: C-W-READ ( -- n )
   0 C-W@
   1 C-W@ xor
   2 C-W@ xor
   3 C-W@ xor
   4 C-W@ xor ;

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus3.f" ;

: SUFFIX$ ( -- ptr u8 n )
   s" -J3" ;

\ The package this corpus publishes its subjects in. Every subject reaches its
\ elements through T-AT, T-GET and T-SET, which the chain publishes first
\ because a call is compiled against a callee that already exists.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS3:" ;

\ ---- the buffers each row reads -----------------------------------------------

: A+ ( -- )
   s" CODEGEN-CORPUS3:A-VEC " s" JUDGE-CORPUS3:C-A " JUDGE-PASS:STORE+ ;

: B+ ( -- )
   s" CODEGEN-CORPUS3:B-VEC " s" JUDGE-CORPUS3:C-B " JUDGE-PASS:STORE+ ;

: W-G+ ( -- )
   s" CODEGEN-CORPUS3:W-VEC CODEGEN-CORPUS3:G-VEC "
   s" JUDGE-CORPUS3:C-W JUDGE-CORPUS3:C-G " JUDGE-PASS:STORE+
   s" CODEGEN-CORPUS3:W-RESET " s" JUDGE-CORPUS3:C-W-RESET " JUDGE-PASS:SETUP+
   s" JUDGE-CORPUS3:W-READ" s" JUDGE-CORPUS3:C-W-READ" JUDGE-PASS:READ+ ;

: LEN+ ( -- )
   s" CODEGEN-CORPUS3:VEC-LEN" JUDGE-PASS:IN+ ;

\ ---- the ten rows, written once -----------------------------------------------
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ -- ] -- ) {: row :}
   s" T-SUM" s" hc3_t_sum" s" IID" JUDGE-PASS:ROW-ABI!
      A+  LEN+  row execute
   s" T-SGD!" s" hc3_t_sgd" s" DIII" JUDGE-PASS:ROW-ABI!
      s" CODEGEN-CORPUS3:STEP-LR " JUDGE-PASS:IN+  W-G+  LEN+  row execute
   s" T-DIST2" s" hc3_t_dist2" s" IIID" JUDGE-PASS:ROW-ABI!
      A+  B+  LEN+  row execute
   s" T-NORM2" s" hc3_t_norm2" s" IID" JUDGE-PASS:ROW-ABI!
      A+  LEN+  row execute
   s" T-REL-L2" s" hc3_t_rel_l2" s" IIID" JUDGE-PASS:ROW-ABI!
      A+  B+  LEN+  row execute
   s" RELU-F" s" hc3_relu_f" s" DD" JUDGE-PASS:ROW-ABI!
      s" -2.5" JUDGE-PASS:IN+  row execute
   s" MAX-F" s" hc3_max_f" s" DDD" JUDGE-PASS:ROW-ABI!
      s" 1.5 -2.5" JUDGE-PASS:IN+  row execute
   s" SGD" s" hc3_sgd" s" DDDD" JUDGE-PASS:ROW-ABI!
      s" 1.0 0.5 0.25" JUDGE-PASS:IN+  row execute
   s" SEG-1/SQRT" s" hc3_seg_rsqrt" s" ID" JUDGE-PASS:ROW-ABI!
      s" 4" JUDGE-PASS:IN+  row execute
   s" FROUND" s" hc3_fround" s" DI" JUDGE-PASS:ROW-ABI!
      s" 2.5" JUDGE-PASS:IN+  row execute ;

: OPEN-CORPUS ( -- )
   SOURCE$ SUFFIX$ QUALIFIER$ JUDGE-PASS:CORPUS! ;

public

\ Read the corpus source and compile every subject through the chain. Runs at
\ load, from inside the corpus's package.
: PUBLISH-ALL ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:PUBLISH ;] EACH ;

: JUDGE ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:BYTES ;] EACH
   [: JUDGE-PASS:VALUE ;] EACH
   JUDGE-PASS:TIME-PASSES 0 ?do [: JUDGE-PASS:TIME ;] EACH loop
   JUDGE-PASS:FLOOR ;

;package

package CODEGEN-CORPUS3
public

JUDGE-CORPUS3:PUBLISH-ALL

;package
