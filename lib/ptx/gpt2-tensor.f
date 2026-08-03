\ gpt2-tensor.f - six fixed F32 tensor kernels used by GPT-2 decode.

require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/cg-activation.f
require lib/ptx/tile.f
require lib/ptx/collective.f

package GPT2-PTX

private

256 %BLOCK

: OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<64>;" PTX-L
   s" .reg .f32 %f<64>;" PTX-L
   s" .reg .b32 %r<64>;" PTX-L
   s" .reg .b64 %rd<64>;" PTX-L ;

: CLOSE ( -- )
   s" DONE:" PTX-L
   s" ret;" PTX-L
   s" }" PTX-L ;

: TID ( -- n )
   CG-NEXT-R {: c:n :}  CG-NEXT-R {: b:n :}
   CG-NEXT-R {: t:n :}  CG-NEXT-R {: i:n :}
   SB-RESET s" mov.u32 " CG-S c CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S b CG-R s" , %ntid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S t CG-R s" , %tid.x;" CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S i CG-R s" , " CG-S c CG-R s" , " CG-S b CG-R s" , " CG-S t CG-R s" ;" CG-S CG-LINE
   i ;

: LANE ( -- n )
   CG-NEXT-R {: r:n :}
   SB-RESET s" mov.u32 " CG-S r CG-R s" , %tid.x;" CG-S CG-LINE
   r ;

: NTID ( -- n )
   CG-NEXT-R {: r:n :}
   SB-RESET s" mov.u32 " CG-S r CG-R s" , %ntid.x;" CG-S CG-LINE
   r ;

: R-MUL ( n n -- n ) {: a:n b:n :}
   CG-NEXT-R {: r:n :}
   SB-RESET s" mul.lo.u32 " CG-S r CG-R s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE
   r ;

: R-MAD ( n n n -- n ) {: a:n b:n c:n :}
   CG-NEXT-R {: r:n :}
   SB-RESET s" mad.lo.u32 " CG-S r CG-R s" , " CG-S a CG-R s" , " CG-S b CG-R s" , " CG-S c CG-R s" ;" CG-S CG-LINE
   r ;

: R-DIV ( n n -- n ) {: a:n b:n :}
   CG-NEXT-R {: r:n :}
   SB-RESET s" div.u32 " CG-S r CG-R s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE
   r ;

: R-REM ( n n -- n ) {: a:n b:n :}
   CG-NEXT-R {: r:n :}
   SB-RESET s" rem.u32 " CG-S r CG-R s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE
   r ;

: R-ADD! ( n n -- ) {: a:n b:n :}
   SB-RESET s" add.u32 " CG-S a CG-R s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE ;

: R-INC! ( n -- ) {: r:n :}
   SB-RESET s" add.u32 " CG-S r CG-R s" , " CG-S r CG-R s" , 1;" CG-S CG-LINE ;

: OFF ( n -- n ) {: i:n :}
   CG-NEXT-RD {: r:n :}
   SB-RESET s" mul.wide.u32 " CG-S r CG-RD s" , " CG-S i CG-R s" , 4;" CG-S CG-LINE
   r ;

: LD-U32 ( n n -- n ) {: base:n off:n :}
   CG-NEXT-RD {: g:n :}  CG-NEXT-RD {: a:n :}
   CG-NEXT-R {: r:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S base CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S g CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" ld.global.u32 " CG-S r CG-R s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   r ;

: F-ZERO ( -- n )
   CG-NEXT-F {: r:n :}
   SB-RESET s" mov.f32 " CG-S r CG-F s" , 0f00000000;" CG-S CG-LINE
   r ;

: F-ADD! ( n n -- ) {: a:n b:n :}
   SB-RESET s" add.rn.f32 " CG-S a CG-F s" , " CG-S a CG-F s" , " CG-S b CG-F s" ;" CG-S CG-LINE ;

: FMA! ( n n n -- ) {: acc:n a:n b:n :}
   SB-RESET s" fma.rn.f32 " CG-S acc CG-F s" , " CG-S a CG-F s" , " CG-S b CG-F s" , " CG-S acc CG-F s" ;" CG-S CG-LINE ;

: BRA ( n -- ) {: l:n :}
   SB-RESET s" bra " CG-S l CG-L s" ;" CG-S CG-LINE ;

: BGE ( n n n -- ) {: a:n b:n l:n :}
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra " CG-S l CG-L s" ;" CG-S CG-LINE ;

: BGE-DONE ( n n -- ) {: a:n b:n :}
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S a CG-R s" , " CG-S b CG-R s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE ;

\ Fixed ABI mints: the entry parameters carry these exact shape relations.
TRUSTED: EMBED-ABI ( -- span<space-global,u32,t> matrix<space-global,f32,v,h> matrix<space-global,f32,t,h> matrix<space-global,f32,t,h> ) 1 2 3 4 ;
TRUSTED: LN-ABI ( -- matrix<space-global,f32,r,c> span<space-global,f32,c> span<space-global,f32,c> matrix<space-global,f32,r,c> ) 1 2 3 4 ;
TRUSTED: LINEAR-ABI ( -- matrix<space-global,f32,r,k> matrix<space-global,f32,k,c> span<space-global,f32,c> matrix<space-global,f32,r,c> ) 1 2 3 4 ;
TRUSTED: UNEMBED-ABI ( -- span<space-global,f32,h> matrix<space-global,f32,v,h> span<space-global,f32,v> ) 1 2 3 ;

: EMIT-EMBED ( n n n n -- ) {: ids:n wte:n wpe:n out:n :}
   TID {: i:n :}
   1 2 R-MUL {: n:n :}
   i n BGE-DONE
   i 2 R-DIV {: row:n :}
   i 2 R-REM {: col:n :}
   ids row OFF LD-U32 {: tok:n :}
   tok 2 col R-MAD OFF {: toff:n :}
   row 2 col R-MAD OFF {: poff:n :}
   wte toff EMIT-LOAD  wpe poff EMIT-LOAD  EMIT-ADD
   out i OFF EMIT-STORE ;

: EMBED. ( span<space-global,u32,t> matrix<space-global,f32,v,h> matrix<space-global,f32,t,h> matrix<space-global,f32,t,h> -- )
   [: EMIT-EMBED ;] PTXREP:SINK4 ;

KERNEL: EMBED-K ( span<space-global,u32,t> matrix<space-global,f32,v,h> matrix<space-global,f32,t,h> matrix<space-global,f32,t,h> -- ) GRID: ceil-n-256
   EMBED. ;

: EMIT-LN ( n n n n -- ) {: x:n gamma:n beta:n out:n :}
   EMIT-ROW {: row:n :}
   row 2 BGE-DONE
   x row EMIT-ROW-SPAN {: xs:n :}
   out row EMIT-ROW-SPAN {: ys:n :}
   NTID {: step:n :}
   F-ZERO {: sum:n :}
   LANE {: col:n :}
   CG-NEXT-L {: l1:n :}  CG-NEXT-L {: e1:n :}
   l1 CG-LDEF
   col 1 e1 BGE
   xs col OFF EMIT-LOAD sum swap F-ADD!
   col step R-ADD!  l1 BRA
   e1 CG-LDEF
   sum EMIT-BLOCK-SUM EMIT-UN EMIT-U/ {: mu:n :}
   F-ZERO {: ss:n :}
   LANE {: col2:n :}
   CG-NEXT-L {: l2:n :}  CG-NEXT-L {: e2:n :}
   l2 CG-LDEF
   col2 1 e2 BGE
   xs col2 OFF EMIT-LOAD mu EMIT-B- {: d:n :}
   d d EMIT-MUL ss swap F-ADD!
   col2 step R-ADD!  l2 BRA
   e2 CG-LDEF
   ss EMIT-BLOCK-SUM EMIT-UN EMIT-U/ EMIT-RMS-EPS+ EMIT-USQRT {: std:n :}
   LANE {: col3:n :}
   CG-NEXT-L {: l3:n :}  CG-NEXT-L {: e3:n :}
   l3 CG-LDEF
   col3 1 e3 BGE
   col3 OFF {: off:n :}
   xs off EMIT-LOAD mu EMIT-B- std EMIT-B/
   gamma off EMIT-LOAD EMIT-MUL
   beta off EMIT-LOAD EMIT-ADD
   ys off EMIT-STORE
   col3 step R-ADD!  l3 BRA
   e3 CG-LDEF ;

: LN. ( matrix<space-global,f32,r,c> span<space-global,f32,c> span<space-global,f32,c> matrix<space-global,f32,r,c> -- )
   [: EMIT-LN ;] PTXREP:SINK4 ;

KERNEL: LN-K ( matrix<space-global,f32,r,c> span<space-global,f32,c> span<space-global,f32,c> matrix<space-global,f32,r,c> -- ) GRID: extent-r
   LN. ;

: EMIT-LINEAR ( n n n n -- ) {: x:n w:n bias:n out:n :}
   TID {: i:n :}
   1 3 R-MUL {: n:n :}
   i n BGE-DONE
   i 3 R-DIV {: row:n :}
   i 3 R-REM {: col:n :}
   bias col OFF EMIT-LOAD {: acc:n :}
   CG-NEXT-R {: k:n :}
   SB-RESET s" mov.u32 " CG-S k CG-R s" , 0;" CG-S CG-LINE
   CG-NEXT-L {: loop:n :}  CG-NEXT-L {: done:n :}
   loop CG-LDEF
   k 2 done BGE
   x row 2 k R-MAD OFF EMIT-LOAD
   w k 3 col R-MAD OFF EMIT-LOAD
   acc -rot FMA!
   k R-INC!  loop BRA
   done CG-LDEF
   acc out i OFF EMIT-STORE ;

: LINEAR. ( matrix<space-global,f32,r,k> matrix<space-global,f32,k,c> span<space-global,f32,c> matrix<space-global,f32,r,c> -- )
   [: EMIT-LINEAR ;] PTXREP:SINK4 ;

KERNEL: LINEAR-K ( matrix<space-global,f32,r,k> matrix<space-global,f32,k,c> span<space-global,f32,c> matrix<space-global,f32,r,c> -- ) GRID: ceil-n-256
   LINEAR. ;

: EMIT-UNEMBED ( n n n -- ) {: x:n wte:n out:n :}
   TID {: row:n :}
   row 2 BGE-DONE
   F-ZERO {: acc:n :}
   CG-NEXT-R {: k:n :}
   SB-RESET s" mov.u32 " CG-S k CG-R s" , 0;" CG-S CG-LINE
   CG-NEXT-L {: loop:n :}  CG-NEXT-L {: done:n :}
   loop CG-LDEF
   k 1 done BGE
   x k OFF EMIT-LOAD
   wte row 1 k R-MAD OFF EMIT-LOAD
   acc -rot FMA!
   k R-INC!  loop BRA
   done CG-LDEF
   acc out row OFF EMIT-STORE ;

: UNEMBED. ( span<space-global,f32,h> matrix<space-global,f32,v,h> span<space-global,f32,v> -- )
   [: EMIT-UNEMBED ;] PTXREP:SINK3 ;

KERNEL: UNEMBED-K ( span<space-global,f32,h> matrix<space-global,f32,v,h> span<space-global,f32,v> -- ) GRID: ceil-n-256
   UNEMBED. ;

: GELU. ( tile<f32,b,m> -- tile<f32,b,m> )
   [: PTX-ACT:EMIT-GELU ;] PTXREP:REP1 ;

KERNEL: GELU-K ( span<space-global,f32,e> -- ) GRID: ceil-n-256
   {: x :} \ typed-local-lint: allow-bare-local - inferred span carries comma-separated phantom args.
   x GRID-CTX {: c :} \ typed-local-lint: allow-bare-local - inferred grid context carries comma-separated phantom args.
   x c LOAD GELU.  x c STORE ;

KERNEL: RESIDUAL-K ( span<space-global,f32,e> span<space-global,f32,e> span<space-global,f32,e> -- ) GRID: ceil-n-256
   {: x res out :} \ typed-local-lint: allow-bare-local - inferred spans carry comma-separated phantom args.
   x GRID-CTX {: c :} \ typed-local-lint: allow-bare-local - inferred grid context carries comma-separated phantom args.
   x c LOAD  res c LOAD  +.  out c STORE ;

: EMBED ( -- )
   s" .visible .entry GPT2_EMBED(.param .u64 p_ids, .param .u64 p_wte, .param .u64 p_wpe, .param .u64 p_out, .param .u32 p_tokens, .param .u32 p_hidden)" PTX-L
   OPEN
   s" ld.param.u64 %rd1, [p_ids];" PTX-L  s" ld.param.u64 %rd2, [p_wte];" PTX-L
   s" ld.param.u64 %rd3, [p_wpe];" PTX-L  s" ld.param.u64 %rd4, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_tokens];" PTX-L  s" ld.param.u32 %r2, [p_hidden];" PTX-L
   1 CG-NF !  5 CG-NRD !  3 CG-NR !  1 CG-NP !  0 CG-NL !
   EMBED-ABI EMBED-K
   CLOSE ;

: LAYERNORM ( -- )
   s" .visible .entry GPT2_LAYERNORM(.param .u64 p_x, .param .u64 p_gamma, .param .u64 p_beta, .param .u64 p_out, .param .u32 p_rows, .param .u32 p_cols)" PTX-L
   1 CG-NF !  5 CG-NRD !  3 CG-NR !  1 CG-NP !  0 CG-NL !
   CG-SM-OPEN
   s" ld.param.u64 %rd1, [p_x];" PTX-L  s" ld.param.u64 %rd2, [p_gamma];" PTX-L
   s" ld.param.u64 %rd3, [p_beta];" PTX-L  s" ld.param.u64 %rd4, [p_out];" PTX-L
   s" ld.param.u32 %r2, [p_rows];" PTX-L  s" ld.param.u32 %r1, [p_cols];" PTX-L
   LN-ABI LN-K
   CLOSE ;

: LINEAR ( -- )
   s" .visible .entry GPT2_LINEAR(.param .u64 p_x, .param .u64 p_w, .param .u64 p_bias, .param .u64 p_out, .param .u32 p_rows, .param .u32 p_in, .param .u32 p_out_cols)" PTX-L
   OPEN
   s" ld.param.u64 %rd1, [p_x];" PTX-L  s" ld.param.u64 %rd2, [p_w];" PTX-L
   s" ld.param.u64 %rd3, [p_bias];" PTX-L  s" ld.param.u64 %rd4, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_rows];" PTX-L  s" ld.param.u32 %r2, [p_in];" PTX-L  s" ld.param.u32 %r3, [p_out_cols];" PTX-L
   1 CG-NF !  5 CG-NRD !  4 CG-NR !  1 CG-NP !  0 CG-NL !
   LINEAR-ABI LINEAR-K
   CLOSE ;

: UNEMBED ( -- )
   s" .visible .entry GPT2_UNEMBED(.param .u64 p_x, .param .u64 p_wte, .param .u64 p_logits, .param .u32 p_hidden, .param .u32 p_vocab)" PTX-L
   OPEN
   s" ld.param.u64 %rd1, [p_x];" PTX-L  s" ld.param.u64 %rd2, [p_wte];" PTX-L
   s" ld.param.u64 %rd3, [p_logits];" PTX-L  s" ld.param.u32 %r1, [p_hidden];" PTX-L  s" ld.param.u32 %r2, [p_vocab];" PTX-L
   1 CG-NF !  4 CG-NRD !  3 CG-NR !  1 CG-NP !  0 CG-NL !
   UNEMBED-ABI UNEMBED-K
   CLOSE ;

: GELU ( -- )
   s" .visible .entry GPT2_GELU(.param .u64 p_x, .param .u32 p_n)" PTX-L
   OPEN
   s" ld.param.u64 %rd1, [p_x];" PTX-L  s" ld.param.u32 %r1, [p_n];" PTX-L
   1 CG-NF !  2 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL !
   1 SPAN-REG GELU-K
   CLOSE ;

: RESIDUAL ( -- )
   s" .visible .entry GPT2_RESIDUAL(.param .u64 p_x, .param .u64 p_residual, .param .u64 p_out, .param .u32 p_n)" PTX-L
   OPEN
   s" ld.param.u64 %rd1, [p_x];" PTX-L  s" ld.param.u64 %rd2, [p_residual];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L  s" ld.param.u32 %r1, [p_n];" PTX-L
   1 CG-NF !  4 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL !
   1 SPAN-REG  2 SPAN-REG  3 SPAN-REG  RESIDUAL-K
   CLOSE ;

public

: EMIT ( -- )
   CG-HEADER
   EMBED LAYERNORM LINEAR UNEMBED GELU RESIDUAL ;

;package
