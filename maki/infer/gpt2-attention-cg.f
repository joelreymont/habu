\ gpt2-attention-cg.f - GPT-2 one-row causal decode attention.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f

package GPT2-ATTN

private

$FFFFFFFF constant U32-MAX
4 constant F32-BYTES
8 constant ROLE-BITS
$FF constant ROLE-MASK

: U32? ( n -- bool ) {: x:n :}
   x 0 >= x U32-MAX <= and ;

: POS-U32? ( n -- bool )
   dup U32? swap 0 > and ;

: PACK ( n n n n n n -- n ) {: q:n k:n v:n kc:n vc:n out:n :}
   q
   k  ROLE-BITS     lshift or
   v  ROLE-BITS 2 * lshift or
   kc ROLE-BITS 3 * lshift or
   vc ROLE-BITS 4 * lshift or
   out ROLE-BITS 5 * lshift or ;

: ROLE@ ( n n -- n ) {: roles:n lane:n :}
   roles lane ROLE-BITS * rshift ROLE-MASK and ;

: QROLE   ( n -- n ) 0 ROLE@ ;
: KROLE   ( n -- n ) 1 ROLE@ ;
: VROLE   ( n -- n ) 2 ROLE@ ;
: KCROLE  ( n -- n ) 3 ROLE@ ;
: VCROLE  ( n -- n ) 4 ROLE@ ;
: OROLE   ( n -- n ) 5 ROLE@ ;

: BASE+OFF ( n n n -- ) {: dst:n base:n off:n :}
   SB-RESET
   s" add.u64 " SB-APPEND dst CG-RD s" ," SB-APPEND
   base CG-RD s" ," SB-APPEND off CG-RD s" ;" SB-APPEND
   SB$ PTX-L ;

: ENTRY ( -- )
   s" .visible .entry GPT2_ATTN(" PTX-L
   s" .param .u64 p_q,.param .u64 p_k,.param .u64 p_v," PTX-L
   s" .param .u64 p_kc,.param .u64 p_vc,.param .u64 p_out," PTX-L
   s" .param .u32 p_pos,.param .u32 p_heads,.param .u32 p_hd,.param .u32 p_cap)" PTX-L ;

: REGS ( -- )
   s" {" PTX-L
   s" .reg .pred %p<8>;" PTX-L
   s" .reg .f32 %f<12>;" PTX-L
   s" .reg .b32 %r<24>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L ;

: PARAMS ( -- )
   s" ld.param.u64 %rd1,[p_q];" PTX-L
   s" ld.param.u64 %rd2,[p_k];" PTX-L
   s" ld.param.u64 %rd3,[p_v];" PTX-L
   s" ld.param.u64 %rd4,[p_kc];" PTX-L
   s" ld.param.u64 %rd5,[p_vc];" PTX-L
   s" ld.param.u64 %rd6,[p_out];" PTX-L
   s" ld.param.u32 %r1,[p_pos];" PTX-L
   s" ld.param.u32 %r2,[p_heads];" PTX-L
   s" ld.param.u32 %r3,[p_hd];" PTX-L
   s" ld.param.u32 %r4,[p_cap];" PTX-L
   s" cvta.to.global.u64 %rd1,%rd1;" PTX-L
   s" cvta.to.global.u64 %rd2,%rd2;" PTX-L
   s" cvta.to.global.u64 %rd3,%rd3;" PTX-L
   s" cvta.to.global.u64 %rd4,%rd4;" PTX-L
   s" cvta.to.global.u64 %rd5,%rd5;" PTX-L
   s" cvta.to.global.u64 %rd6,%rd6;" PTX-L ;

: SETUP-EMIT ( -- )
   s" mov.u32 %r5,%ctaid.x;" PTX-L
   s" mov.u32 %r6,%tid.x;" PTX-L
   s" mov.u32 %r7,%ntid.x;" PTX-L
   s" setp.ge.u32 %p1,%r5,%r2;" PTX-L
   s" @%p1 bra $DONE;" PTX-L
   s" setp.ge.u32 %p1,%r1,%r4;" PTX-L
   s" @%p1 bra $DONE;" PTX-L
   s" setp.eq.u32 %p1,%r2,0;" PTX-L
   s" @%p1 bra $DONE;" PTX-L
   s" setp.eq.u32 %p1,%r3,0;" PTX-L
   s" @%p1 bra $DONE;" PTX-L
   s" cvt.u64.u32 %rd7,%r5;" PTX-L
   s" cvt.u64.u32 %rd8,%r3;" PTX-L
   s" cvt.u64.u32 %rd11,%r1;" PTX-L
   s" cvt.u64.u32 %rd12,%r2;" PTX-L
   s" mul.lo.u64 %rd9,%rd7,%rd8;" PTX-L
   s" shl.b64 %rd10,%rd9,2;" PTX-L
   s" mad.lo.u64 %rd13,%rd11,%rd12,%rd7;" PTX-L
   s" mul.lo.u64 %rd13,%rd13,%rd8;" PTX-L
   s" shl.b64 %rd14,%rd13,2;" PTX-L
   s" cvt.rn.f32.u32 %f10,%r3;" PTX-L
   s" rsqrt.approx.f32 %f10,%f10;" PTX-L
   s" mov.u32 %r10,GPT2_ATTN_SM;" PTX-L ;

: APPEND-EMIT ( n -- ) {: roles:n :}
   s" mov.u32 %r8,%r6;" PTX-L
   s" $COPY:" PTX-L
   s" setp.ge.u32 %p1,%r8,%r3;" PTX-L
   s" @%p1 bra $COPY_DONE;" PTX-L
   s" mul.wide.u32 %rd15,%r8,4;" PTX-L
   16 roles KROLE 10 BASE+OFF
   s" add.u64 %rd16,%rd16,%rd15;" PTX-L
   s" ld.global.f32 %f1,[%rd16];" PTX-L
   17 roles KCROLE 14 BASE+OFF
   s" add.u64 %rd17,%rd17,%rd15;" PTX-L
   s" st.global.f32 [%rd17],%f1;" PTX-L
   16 roles VROLE 10 BASE+OFF
   s" add.u64 %rd16,%rd16,%rd15;" PTX-L
   s" ld.global.f32 %f1,[%rd16];" PTX-L
   17 roles VCROLE 14 BASE+OFF
   s" add.u64 %rd17,%rd17,%rd15;" PTX-L
   s" st.global.f32 [%rd17],%f1;" PTX-L
   s" add.u32 %r8,%r8,%r7;" PTX-L
   s" bra $COPY;" PTX-L
   s" $COPY_DONE:" PTX-L
   s" bar.sync 0;" PTX-L ;

: SCORE-EMIT ( n -- ) {: roles:n :}
   s" mov.u32 %r9,%r6;" PTX-L
   s" $SCORE:" PTX-L
   s" setp.gt.u32 %p1,%r9,%r1;" PTX-L
   s" @%p1 bra $SCORE_DONE;" PTX-L
   s" cvt.u64.u32 %rd18,%r9;" PTX-L
   s" mad.lo.u64 %rd19,%rd18,%rd12,%rd7;" PTX-L
   s" mul.lo.u64 %rd19,%rd19,%rd8;" PTX-L
   s" shl.b64 %rd19,%rd19,2;" PTX-L
   s" mov.f32 %f1,0f00000000;" PTX-L
   s" mov.u32 %r8,0;" PTX-L
   s" $DOT:" PTX-L
   s" setp.ge.u32 %p2,%r8,%r3;" PTX-L
   s" @%p2 bra $DOT_DONE;" PTX-L
   s" mul.wide.u32 %rd15,%r8,4;" PTX-L
   16 roles QROLE 10 BASE+OFF
   s" add.u64 %rd16,%rd16,%rd15;" PTX-L
   s" ld.global.f32 %f2,[%rd16];" PTX-L
   17 roles KCROLE 19 BASE+OFF
   s" add.u64 %rd17,%rd17,%rd15;" PTX-L
   s" ld.global.f32 %f3,[%rd17];" PTX-L
   s" fma.rn.f32 %f1,%f2,%f3,%f1;" PTX-L
   s" add.u32 %r8,%r8,1;" PTX-L
   s" bra $DOT;" PTX-L
   s" $DOT_DONE:" PTX-L
   s" mul.rn.f32 %f1,%f1,%f10;" PTX-L
   s" shl.b32 %r11,%r9,2;" PTX-L
   s" add.u32 %r11,%r10,%r11;" PTX-L
   s" st.shared.f32 [%r11],%f1;" PTX-L
   s" add.u32 %r9,%r9,%r7;" PTX-L
   s" bra $SCORE;" PTX-L
   s" $SCORE_DONE:" PTX-L
   s" bar.sync 0;" PTX-L ;

: SOFTMAX-EMIT ( -- )
   s" setp.ne.u32 %p1,%r6,0;" PTX-L
   s" @%p1 bra $SOFT_DONE;" PTX-L
   s" mov.f32 %f4,0fFF800000;" PTX-L
   s" mov.u32 %r9,0;" PTX-L
   s" $MAX:" PTX-L
   s" setp.gt.u32 %p2,%r9,%r1;" PTX-L
   s" @%p2 bra $MAX_DONE;" PTX-L
   s" shl.b32 %r11,%r9,2;" PTX-L
   s" add.u32 %r11,%r10,%r11;" PTX-L
   s" ld.shared.f32 %f1,[%r11];" PTX-L
   s" max.f32 %f4,%f4,%f1;" PTX-L
   s" add.u32 %r9,%r9,1;" PTX-L
   s" bra $MAX;" PTX-L
   s" $MAX_DONE:" PTX-L
   s" mov.f32 %f5,0f00000000;" PTX-L
   s" mov.u32 %r9,0;" PTX-L
   s" $EXP:" PTX-L
   s" setp.gt.u32 %p2,%r9,%r1;" PTX-L
   s" @%p2 bra $EXP_DONE;" PTX-L
   s" shl.b32 %r11,%r9,2;" PTX-L
   s" add.u32 %r11,%r10,%r11;" PTX-L
   s" ld.shared.f32 %f1,[%r11];" PTX-L
   s" sub.rn.f32 %f1,%f1,%f4;" PTX-L
   s" mul.rn.f32 %f1,%f1,0f3FB8AA3B;" PTX-L
   s" ex2.approx.f32 %f1,%f1;" PTX-L
   s" st.shared.f32 [%r11],%f1;" PTX-L
   s" add.rn.f32 %f5,%f5,%f1;" PTX-L
   s" add.u32 %r9,%r9,1;" PTX-L
   s" bra $EXP;" PTX-L
   s" $EXP_DONE:" PTX-L
   s" shl.b32 %r12,%r4,2;" PTX-L
   s" add.u32 %r12,%r10,%r12;" PTX-L
   s" st.shared.f32 [%r12],%f5;" PTX-L
   s" $SOFT_DONE:" PTX-L
   s" bar.sync 0;" PTX-L ;

: OUTPUT-EMIT ( n -- ) {: roles:n :}
   s" mov.u32 %r8,%r6;" PTX-L
   s" $OUT_DIM:" PTX-L
   s" setp.ge.u32 %p1,%r8,%r3;" PTX-L
   s" @%p1 bra $DONE;" PTX-L
   s" mov.f32 %f1,0f00000000;" PTX-L
   s" mov.u32 %r9,0;" PTX-L
   s" $OUT_TOKEN:" PTX-L
   s" setp.gt.u32 %p2,%r9,%r1;" PTX-L
   s" @%p2 bra $OUT_STORE;" PTX-L
   s" shl.b32 %r11,%r9,2;" PTX-L
   s" add.u32 %r11,%r10,%r11;" PTX-L
   s" ld.shared.f32 %f2,[%r11];" PTX-L
   s" cvt.u64.u32 %rd18,%r9;" PTX-L
   s" mad.lo.u64 %rd19,%rd18,%rd12,%rd7;" PTX-L
   s" mul.lo.u64 %rd19,%rd19,%rd8;" PTX-L
   s" cvt.u64.u32 %rd20,%r8;" PTX-L
   s" add.u64 %rd19,%rd19,%rd20;" PTX-L
   s" shl.b64 %rd19,%rd19,2;" PTX-L
   17 roles VCROLE 19 BASE+OFF
   s" ld.global.f32 %f3,[%rd17];" PTX-L
   s" fma.rn.f32 %f1,%f2,%f3,%f1;" PTX-L
   s" add.u32 %r9,%r9,1;" PTX-L
   s" bra $OUT_TOKEN;" PTX-L
   s" $OUT_STORE:" PTX-L
   s" shl.b32 %r12,%r4,2;" PTX-L
   s" add.u32 %r12,%r10,%r12;" PTX-L
   s" ld.shared.f32 %f5,[%r12];" PTX-L
   s" div.rn.f32 %f1,%f1,%f5;" PTX-L
   s" cvt.u64.u32 %rd20,%r8;" PTX-L
   s" add.u64 %rd20,%rd9,%rd20;" PTX-L
   s" shl.b64 %rd20,%rd20,2;" PTX-L
   20 roles OROLE 20 BASE+OFF
   s" st.global.f32 [%rd20],%f1;" PTX-L
   s" add.u32 %r8,%r8,%r7;" PTX-L
   s" bra $OUT_DIM;" PTX-L
   ;

TRUSTED: ROW-REG ( n -- matrix<space-global,f32,extent-h,extent-d> ) ;
TRUSTED: CACHE-REG ( n -- matrix<space-global,f32,extent-c,extent-d> ) ;

TRUSTED: STATE ( matrix<space-global,f32,h,d> matrix<space-global,f32,h,d> matrix<space-global,f32,h,d> matrix<space-global,f32,c,d> matrix<space-global,f32,c,d> matrix<space-global,f32,h,d> -- attnctx<h,d,attn-stage-q> attnacc<f32,block-128,mask-live> )
   PACK 0 ;

TRUSTED: APPEND ( attnctx<h,d,attn-stage-q> attnacc<f32,b,m> -- attnctx<h,d,attn-stage-score> attnacc<f32,b,m> )
   over APPEND-EMIT ;

TRUSTED: SCORE ( attnctx<h,d,attn-stage-score> attnacc<f32,b,m> -- attnctx<h,d,attn-stage-softmax> attnacc<f32,b,m> )
   over SCORE-EMIT ;

TRUSTED: SOFTMAX ( attnctx<h,d,attn-stage-softmax> attnacc<f32,b,m> -- attnctx<h,d,attn-stage-output> attnacc<f32,b,m> )
   SOFTMAX-EMIT ;

TRUSTED: OUTPUT ( attnctx<h,d,attn-stage-output> attnacc<f32,b,m> -- attnctx<h,d,attn-stage-done> attnacc<f32,b,m> )
   over OUTPUT-EMIT ;

: START ( matrix<space-global,f32,h,d> matrix<space-global,f32,h,d> matrix<space-global,f32,h,d> matrix<space-global,f32,c,d> matrix<space-global,f32,c,d> matrix<space-global,f32,h,d> -- attnctx<h,d,attn-stage-q> attnacc<f32,block-128,mask-live> )
   PARAMS SETUP-EMIT STATE ;

: FINISH ( attnctx<h,d,attn-stage-done> attnacc<f32,b,m> -- )
   2drop ;

KERNEL: CHECKED ( matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-c,extent-d> matrix<space-global,f32,extent-c,extent-d> matrix<space-global,f32,extent-h,extent-d> -- ) GRID: extent-h
   START APPEND SCORE SOFTMAX OUTPUT FINISH ;

: AUTHOR-OPEN ( -- matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-h,extent-d> matrix<space-global,f32,extent-c,extent-d> matrix<space-global,f32,extent-c,extent-d> matrix<space-global,f32,extent-h,extent-d> )
   PTX-HEADER PTX-NL
   s" .extern .shared .align 4 .b8 GPT2_ATTN_SM[];" PTX-L
   ENTRY REGS
   1 ROW-REG 2 ROW-REG 3 ROW-REG 4 CACHE-REG 5 CACHE-REG 6 ROW-REG ;

: AUTHOR-CLOSE ( -- )
   s" $DONE:" PTX-L
   s" ret;" PTX-L
   s" }" PTX-L ;

public

: LAUNCH-CHECK ( n n n n -- n ) {: pos:n heads:n width:n cap:n :}
   pos U32? 0= if E-PTX-BLOCK throw then
   heads POS-U32? 0= if E-PTX-BLOCK throw then
   width POS-U32? 0= if E-PTX-BLOCK throw then
   cap POS-U32? 0= if E-PTX-BLOCK throw then
   pos cap >= if E-PTX-BLOCK throw then
   cap 1+ F32-BYTES * ;

: EMIT ( -- )
   AUTHOR-OPEN CHECKED AUTHOR-CLOSE ;

;package
