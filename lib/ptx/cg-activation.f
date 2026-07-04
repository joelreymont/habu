\ cg-activation.f - PTX codegen for the elementwise activations gelu / silu.
\
\ The emit dual of the maki activation references (maki/gelu.f GELU-F, maki/silu.f
\ SILU-F): EMIT-GELU / EMIT-SILU lower each op to f32 PTX that MIRRORS the host
\ scalar formula op-for-op, so a device result matches F64>F32(host) within the
\ CAD-PLAN section 11 f32 tolerance (atol 1e-6, rtol 1e-5). Same emit-mode
\ discipline as lib/ptx/cg.f and lib/ptx/cg-collective.f: each op consumes and
\ returns a %f register NUMBER, allocated with CG-NEXT-F; running the op in emit
\ mode appends its PTX lines. exp() reuses EMIT-EXP (ex2.approx(z*log2e)) and NEG
\ reuses EMIT-NEG from cg-collective.f; a Habu float literal becomes a PTX
\ 0f-immediate via CG-IMM (F64>F32 -> 8 hex digits) so the same rounding constants
\ the host uses land in the kernel. Checked Habu. Load after lib/ptx/cg.f and
\ lib/ptx/cg-collective.f.

\ ---- float literal -> PTX 0f-immediate (F64>F32 bit pattern as 8 hex digits) ----
: CG-HEX1 ( n -- )                          \ one hex nibble (0..15) as an uppercase digit
   dup 10 < if 48 + else 55 + then SB-APPEND-C ;
: CG-HEX8 ( n -- ) {: v:n :}                 \ 8 hex digits, most-significant first
   8 0 ?do  v 28 i 4 * - rshift $F and CG-HEX1  loop ;
: CG-IMM ( r -- )  s" 0f" SB-APPEND  F64>F32 CG-HEX8 ;   \ append "0fXXXXXXXX"

\ ---- scalar-by-constant emitters (tile OP literal -> tile) ----
: EMIT-MULC ( n r -- n ) {: x:n c:r :}       \ r = x * c
   CG-NEXT-F {: r:n :}
   SB-RESET s" mul.f32 " CG-S r CG-F s" , " CG-S x CG-F s" , " CG-S c CG-IMM s" ;" CG-S CG-LINE
   r ;
: EMIT-ADDC ( n r -- n ) {: x:n c:r :}       \ r = x + c
   CG-NEXT-F {: r:n :}
   SB-RESET s" add.f32 " CG-S r CG-F s" , " CG-S x CG-F s" , " CG-S c CG-IMM s" ;" CG-S CG-LINE
   r ;
: EMIT-SUBC ( n r -- n ) {: x:n c:r :}       \ r = x - c
   CG-NEXT-F {: r:n :}
   SB-RESET s" sub.f32 " CG-S r CG-F s" , " CG-S x CG-F s" , " CG-S c CG-IMM s" ;" CG-S CG-LINE
   r ;
: EMIT-CDIV ( n r -- n ) {: d:n c:r :}       \ r = c / d  (IEEE round-to-nearest, matches host f/)
   CG-NEXT-F {: r:n :}
   SB-RESET s" div.rn.f32 " CG-S r CG-F s" , " CG-S c CG-IMM s" , " CG-S d CG-F s" ;" CG-S CG-LINE
   r ;

\ ---- sigmoid / tanh (host maki/fmath.f, f32) ----
\ sigmoid(x) = 1/(exp(-x)+1)   (SIGMOID-F: x fnegate FEXP 1.0 f+ 1.0 swap f/)
: EMIT-SIGMOID ( n -- n ) {: x:n :}
   x EMIT-NEG EMIT-EXP {: e:n :}            \ e = exp(-x)
   e 1.0 EMIT-ADDC {: d:n :}                \ d = e + 1
   d 1.0 EMIT-CDIV ;                        \ 1 / d

\ tanh(x) = 2*sigmoid(2x) - 1   (TANH-F: x 2.0 f* SIGMOID-F 2.0 f* 1.0 f-)
: EMIT-TANH ( n -- n ) {: x:n :}
   x 2.0 EMIT-MULC EMIT-SIGMOID {: s:n :}   \ s = sigmoid(2x)
   s 2.0 EMIT-MULC 1.0 EMIT-SUBC ;          \ 2s - 1

\ ---- GELU (tanh approximation, maki/gelu.f) ----
\ u = (x + 0.044715 x^3) * 0.7978845608 ; g = 0.5 (1 + tanh u) x
: EMIT-GELU-U ( n -- n ) {: x:n :}
   x x EMIT-MUL {: x2:n :}                  \ x^2
   x2 x EMIT-MUL {: x3:n :}                 \ x^3
   x3 0.044715 EMIT-MULC {: t:n :}          \ 0.044715 x^3
   x t EMIT-ADD 0.7978845608 EMIT-MULC ;    \ (x + ...) * c

: EMIT-GELU ( n -- n ) {: x:n :}
   x EMIT-GELU-U EMIT-TANH {: t:n :}        \ tanh(u)
   t 1.0 EMIT-ADDC 0.5 EMIT-MULC {: h:n :}  \ 0.5 (1 + tanh u)
   h x EMIT-MUL ;                           \ * x

\ ---- SiLU / swish (maki/silu.f): s = x * sigmoid(x) ----
: EMIT-SILU ( n -- n ) {: x:n :}
   x EMIT-SIGMOID {: s:n :}
   x s EMIT-MUL ;
