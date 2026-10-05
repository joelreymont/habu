\ select-f64.f - WSEL's f64 rows: src/arch/wasm/select.f under Habu's NaN rule
\ (docs/wasm-backend.md 17.3, src/compiler/native/hir-word.f DEF-FLOAT).
\
\ Each row compiles Habu source, selects it under WPROF's V1 and compares
\ function zero's shape (test/wasm/select-lib.f) with every operation's
\ operands: `[a,b]` names the values it reads by their ordinals, each block
\ argument and result numbered in the order the function makes it. So a row
\ states which value each f64.select answers and which one its f64.ne tests,
\ and with them the rule: the left operand when it is a NaN, else the right,
\ else 9221120237041090560 - $7FF8000000000000 - when the answer is a NaN,
\ else the answer. Executing the rows bit-exactly is the rows dot's.
\
\ W05 runs first, in a fork of this process (lib/test/subject.f): a profile is
\ installed once, so the one without saturating-float-to-int has a process of
\ its own, and the rows after it run under V1.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/test/outcome.f
require lib/test/subject.f
require src/compiler/ir/id.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f
require src/compiler/native/hir.f
require src/arch/wasm/profile.f
require src/arch/wasm/select.f
require test/wasm/select-lib.f

package WSEL-TEST
private

\ ---- the shape with operands -----------------------------------------------
\ The values an operation reads, `[a,b]`, each by its ordinal in the module,
\ which holds function zero alone.
: OPNDS+ ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NFROZEN:OPERANDS-OF 0= if exit then
   o NFROZEN:OPERANDS-OF 0 ?do
      i 0= if s" [" else s" ," then SB-APPEND
      o i NFROZEN:OPERAND-AT IR-ID:VALUE-LOCAL FMT:SB-U
   loop
   s" ]" SB-APPEND ;

: FOP+ ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NAME+  o OPNDS+  o SUCCS+  s"  " SB-APPEND ;

\ A block as its argument count and every operation with its operands and
\ destinations: `2: f64.add[4,5] return[7,6]`.
: FLOW+ ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:ARG-COUNT FMT:SB-U  s" : " SB-APPEND
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT FOP+  loop
   s" | " SB-APPEND ;

: FLOW-RUN ( IR-CTX:ctx -- )    COMPILE 0 [: FLOW+ ;] FUN$ KEEP ;

\ The source, its arity and the shape with operands function zero must select
\ to.
: FROW ( ptr u8 n n n ptr u8 n -- )
   {: src:ptr sn:n in:n out:n want:ptr wn:n :}
   src sn in out SOURCE!
   [: FLOW-RUN ;] OUTCOME
   want wn KEPT ;

\ ---- fconst, built straight in HIR -----------------------------------------
\ The source fixture lexes no real literal, so `PQ ( -- n )` is staged: two
\ fconst NaNs whose payloads are not the canonical one, $ABC on the left and
\ $DEF on the right, their fadd, and the sum's bits returned.
$7FF8000000000ABC constant NAN-ABC
$7FF8000000000DEF constant NAN-DEF

\ The operation staged so far, given its one result of type t and closed.
: Z-CLOSE1 ( IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-type-id :}
   ZC ZB t IR-BUILD:ADD-RESULT
   ZC ZB IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   ZC ZB o 0 IR-BUILD:OP-RESULT@ ;

: Z-FCONST ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:FCONST Z-OPEN
   ZC ZB HIR:KEY-VALUE v Z-INT
   ZC ZB HIR:REAL-TYPE Z-CLOSE1 ;

: FCONST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c s" PQ" Z-SOURCE
   s" PQ" 0 1 Z-BEGIN
   NAN-ABC Z-FCONST {: a:IR-ID:ir-value-id :}
   NAN-DEF Z-FCONST {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:FADD Z-OPEN  a Z-USE  b Z-USE
   ZC ZB HIR:REAL-TYPE Z-CLOSE1 {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:REALBITS Z-OPEN  s Z-USE
   ZC ZB HIR:CELL-TYPE Z-CLOSE1 {: r:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN Z-OPEN  r Z-USE
   ZC ZB IR-BUILD:END-OP drop
   c Z-SELECT 0 [: FLOW+ ;] FUN$ KEEP ;

\ PQ's shape with operands.
: FCONST-ROW ( ptr u8 n -- )
   {: want:ptr wn:n :}
   0 DECL-IN !  1 DECL-OUT !
   [: FCONST-BODY ;] OUTCOME
   want wn KEPT ;

\ ---- W05, in a process of its own ------------------------------------------
TYPED-VARIABLE NO-SAT-P WPROF:profile

\ V1 without saturating-float-to-int.
: NO-SAT ( -- WPROF:profile )
   WPROF:V1 NO-SAT-P !
   NO-SAT-P WPROF-PROFILE:FEATURES @
   WPROF-FEATURE:SATURATING-FLOAT-TO-INT WPROF:BIT invert and
   NO-SAT-P WPROF-PROFILE:FEATURES !
   NO-SAT-P @ ;

$400 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
30000 constant TIMEOUT-MS   \ a hang guard: the child selects one word

public

\ The fork's whole program: that profile installed, `f>s` selected under it,
\ and the code the selection threw printed.
: W05-EVAL ( -- )
   NO-SAT INSTALL
   s" PS f>s" 1 1 SOURCE!
   [: [: COMPILE drop ;] WRAPPED ;] catch {: code:n :}
   SB-RESET  code FMT:SB-INT  SB$ type ;

private

: W05 ( -- )
   s" W05: f>s is refused by name without saturating-float-to-int" T-LABEL ;

\ Green when the child exits 0, printed E-WSEL-REFUSED and wrote no error.
: W05-ROW ( -- )
   s" WSEL-TEST:W05-EVAL"
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   W05  s" WSEL-TEST:W05-EVAL" OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   W05  SB-RESET  E-WSEL-REFUSED FMT:SB-INT  OUT outu LEN>N SB$ T$=
   W05  erru LEN>N 0 T= ;

\ ---- the rows --------------------------------------------------------------
\ In PA, 6 and 7 are the operands, 8 their sum and 9 the made NaN; 11 answers 9
\ where 8 is a NaN, 13 answers 7 where 7 is one, and 15, the answer, 6 where 6
\ is one.
: NAN-ROWS ( -- )
   s" f+: the left NaN, else the right, else $7FF8000000000000, else the sum" T-LABEL
   s" PA f+" 2 1
   s" 4: br[1,2]>1 | 2: f64.reinterpret_i64[4] f64.reinterpret_i64[5] f64.add[6,7] f64.const=9221120237041090560 f64.ne[8,8] f64.select[9,8,10] f64.ne[7,7] f64.select[7,11,12] f64.ne[6,6] f64.select[6,13,14] i64.reinterpret_f64[15] i32.const=0 return[17,16] | " FROW
   s" N06's -1 fsqrt: a root's NaN is $7FF8000000000000 unless its operand was one" T-LABEL
   s" PR -1 s>f fsqrt" 0 1
   s" 2: br>1 | 0: i64.const=-1 f64.convert_i64_s[2] f64.sqrt[3] f64.const=9221120237041090560 f64.ne[4,4] f64.select[5,4,6] f64.ne[3,3] f64.select[3,7,8] i64.reinterpret_f64[9] i32.const=0 return[11,10] | " FROW
   s" N06's 0 0 f/: a quotient's NaN is $7FF8000000000000" T-LABEL
   s" PD 0 s>f 0 s>f f/" 0 1
   s" 2: br>1 | 0: i64.const=0 f64.convert_i64_s[2] f64.convert_i64_s[2] f64.div[3,4] f64.const=9221120237041090560 f64.ne[5,5] f64.select[6,5,7] f64.ne[4,4] f64.select[4,8,9] f64.ne[3,3] f64.select[3,10,11] i64.reinterpret_f64[12] i32.const=0 return[14,13] | " FROW
   s" N06's inf inf f-: each quotient, then their difference, by the rule" T-LABEL
   s" PI 1 s>f 0 s>f f/ 1 s>f 0 s>f f/ f-" 0 1
   s" 2: br>1 | 0: i64.const=1 f64.convert_i64_s[2] i64.const=0 f64.convert_i64_s[4] f64.div[3,5] f64.const=9221120237041090560 f64.ne[6,6] f64.select[7,6,8] f64.ne[5,5] f64.select[5,9,10] f64.ne[3,3] f64.select[3,11,12] f64.convert_i64_s[2] f64.convert_i64_s[4] f64.div[14,15] f64.const=9221120237041090560 f64.ne[16,16] f64.select[17,16,18] f64.ne[15,15] f64.select[15,19,20] f64.ne[14,14] f64.select[14,21,22] f64.sub[13,23] f64.const=9221120237041090560 f64.ne[24,24] f64.select[25,24,26] f64.ne[23,23] f64.select[23,27,28] f64.ne[13,13] f64.select[13,29,30] i64.reinterpret_f64[31] i32.const=0 return[33,32] | " FROW
   s" a quiet NaN left operand of payload $ABC passes over $DEF, both fconst bits" T-LABEL
   s" 2: br>1 | 0: f64.const=9221120237041093308 f64.const=9221120237041094127 f64.add[2,3] f64.const=9221120237041090560 f64.ne[4,4] f64.select[5,4,6] f64.ne[3,3] f64.select[3,7,8] f64.ne[2,2] f64.select[2,9,10] i64.reinterpret_f64[11] i32.const=0 return[13,12] | " FCONST-ROW ;

: SIGN-ROWS ( -- )
   s" fnegate and fabs are the sign-bit operations, and nothing is added" T-LABEL
   s" PG fnegate fabs" 1 1
   s" 3: br[1]>1 | 1: f64.reinterpret_i64[3] f64.neg[4] f64.abs[5] i64.reinterpret_f64[6] i32.const=0 return[8,7] | " FROW ;

\ A comparison's predicate becomes the mask 0 - extend_u(p); f0< and f0= compare
\ against +0.0, whose bits are zero.
: COMPARE-ROWS ( -- )
   s" f< is the left operand below the right, as a mask" T-LABEL
   s" PL f<" 2 1
   s" 4: br[1,2]>1 | 2: f64.reinterpret_i64[4] f64.reinterpret_i64[5] f64.lt[6,7] i64.extend_i32_u[8] i64.const=0 i64.sub[10,9] i32.const=0 return[12,11] | " FROW
   s" f> is the left operand above the right" T-LABEL
   s" PL f>" 2 1
   s" 4: br[1,2]>1 | 2: f64.reinterpret_i64[4] f64.reinterpret_i64[5] f64.gt[6,7] i64.extend_i32_u[8] i64.const=0 i64.sub[10,9] i32.const=0 return[12,11] | " FROW
   s" f= is IEEE equality, the left operand first" T-LABEL
   s" PL f=" 2 1
   s" 4: br[1,2]>1 | 2: f64.reinterpret_i64[4] f64.reinterpret_i64[5] f64.eq[6,7] i64.extend_i32_u[8] i64.const=0 i64.sub[10,9] i32.const=0 return[12,11] | " FROW
   s" N06's -0.0 f0=: f64.eq against +0.0, so -0.0 is zero" T-LABEL
   s" PZ 0 s>f fnegate f0=" 0 1
   s" 2: br>1 | 0: i64.const=0 f64.convert_i64_s[2] f64.neg[3] f64.const=0 f64.eq[4,5] i64.extend_i32_u[6] i64.const=0 i64.sub[8,7] i32.const=0 return[10,9] | " FROW
   s" -0.0 f0<: f64.lt against +0.0, so -0.0 is not below zero" T-LABEL
   s" PZ 0 s>f fnegate f0<" 0 1
   s" 2: br>1 | 0: i64.const=0 f64.convert_i64_s[2] f64.neg[3] f64.const=0 f64.lt[4,5] i64.extend_i32_u[6] i64.const=0 i64.sub[8,7] i32.const=0 return[10,9] | " FROW
   s" N06's -0.0 0.0 f=: the two zeros compare as doubles, never as bits" T-LABEL
   s" PZ 0 s>f fnegate 0 s>f f=" 0 1
   s" 2: br>1 | 0: i64.const=0 f64.convert_i64_s[2] f64.neg[3] f64.convert_i64_s[2] f64.eq[4,5] i64.extend_i32_u[6] i64.const=0 i64.sub[8,7] i32.const=0 return[10,9] | " FROW
   s" N06's largest double f< inf: an infinity compares as a double" T-LABEL
   s" PI 9218868437227405311 1 s>f 0 s>f f/ f<" 0 1
   s" 2: br>1 | 0: i64.const=9218868437227405311 i64.const=1 f64.convert_i64_s[3] i64.const=0 f64.convert_i64_s[5] f64.div[4,6] f64.const=9221120237041090560 f64.ne[7,7] f64.select[8,7,9] f64.ne[6,6] f64.select[6,10,11] f64.ne[4,4] f64.select[4,12,13] f64.reinterpret_i64[2] f64.lt[15,14] i64.extend_i32_u[16] i64.const=0 i64.sub[18,17] i32.const=0 return[20,19] | " FROW ;

\ In PB the join's argument, 9, takes the doubles 6 and 8: the full freeze types
\ each branch operand against the argument it becomes, so it is an f64.
: CONVERT-ROWS ( -- )
   s" s>f is f64.convert_i64_s and f>s exactly i64.trunc_sat_f64_s" T-LABEL
   s" PV s>f f>s" 1 1
   s" 3: br[1]>1 | 1: f64.convert_i64_s[3] i64.trunc_sat_f64_s[4] i32.const=0 return[6,5] | " FROW
   s" a join takes a double as its f64 argument" T-LABEL
   s" PB if 1 s>f else 2 s>f then f>s" 1 1
   s" 3: br[1]>1 | 1: i64.eqz[3] brz[4]>3,2 | 0: br>4 | 0: i64.const=1 f64.convert_i64_s[5] br[6]>5 | 0: i64.const=2 f64.convert_i64_s[7] br[8]>5 | 1: i64.trunc_sat_f64_s[9] i32.const=0 return[11,10] | " FROW ;

\ No contraction: the difference reads 17, the product's answer by the rule,
\ so no multiply and add can fuse.
: CONTRACT-ROWS ( -- )
   s" f* then f-: each operation is selected alone" T-LABEL
   s" PE f* f-" 3 1
   s" 5: br[1,2,3]>1 | 3: f64.reinterpret_i64[6] f64.reinterpret_i64[7] f64.mul[8,9] f64.const=9221120237041090560 f64.ne[10,10] f64.select[11,10,12] f64.ne[9,9] f64.select[9,13,14] f64.ne[8,8] f64.select[8,15,16] f64.reinterpret_i64[5] f64.sub[18,17] f64.const=9221120237041090560 f64.ne[19,19] f64.select[20,19,21] f64.ne[17,17] f64.select[17,22,23] f64.ne[18,18] f64.select[18,24,25] i64.reinterpret_f64[26] i32.const=0 return[28,27] | " FROW ;

public
: F64-RUN ( -- )
   W05-ROW
   WPROF:V1 INSTALL
   NAN-ROWS
   SIGN-ROWS
   COMPARE-ROWS
   CONVERT-ROWS
   CONTRACT-ROWS
   T-REPORT ;

;package

WSEL-TEST:F64-RUN
