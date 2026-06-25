\ saxpy.f — M3 spike: Habu emits an sm_87 saxpy PTX kernel to stdout.
\ saxpy: y[i] = a*x[i] + y[i] for i < n, one thread per element.
\ Two roundings (mul.rn + add.rn) so the GPU result matches a CPU golden that
\ also does separate multiply-then-add (no FMA contraction). This is the
\ toolchain spike: prove Habu -> valid PTX -> ptxas -> GPU. The parametric
\ tile DSL that *generates* this from typed words is later milestones; here the
\ structure is fixed for saxpy.
\
\ Run (host):  bin/hb --load lib/errors.f lib/string.f tools/ptx/saxpy.f > saxpy.ptx
\ Then (Orin): ptxas -arch=sm_87 saxpy.ptx -o saxpy.cubin

: L ( ptr u8 n -- ) type cr ;        \ emit one PTX line
: NL ( -- ) cr ;                     \ blank line

: EMIT-SAXPY ( -- )
   s" .version 8.3" L
   s" .target sm_87" L
   s" .address_size 64" L
   NL
   s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" L
   s" {" L
   s" .reg .pred %p<2>;  .reg .f32 %f<5>;  .reg .b32 %r<6>;  .reg .b64 %rd<6>;" L
   s" ld.param.u64 %rd1, [p_x];  ld.param.u64 %rd2, [p_y];" L
   s" ld.param.f32 %f1, [p_a];   ld.param.u32 %r1, [p_n];" L
   s" mov.u32 %r2, %ctaid.x;  mov.u32 %r3, %ntid.x;  mov.u32 %r4, %tid.x;" L
   s" mad.lo.u32 %r5, %r2, %r3, %r4;" L
   s" setp.ge.u32 %p1, %r5, %r1;  @%p1 bra DONE;" L
   s" mul.wide.u32 %rd3, %r5, 4;" L
   s" cvta.to.global.u64 %rd4, %rd1;  add.u64 %rd4, %rd4, %rd3;  ld.global.f32 %f2, [%rd4];" L
   s" cvta.to.global.u64 %rd5, %rd2;  add.u64 %rd5, %rd5, %rd3;  ld.global.f32 %f3, [%rd5];" L
   s" mul.rn.f32 %f4, %f1, %f2;  add.rn.f32 %f4, %f4, %f3;" L
   s" st.global.f32 [%rd5], %f4;" L
   s" DONE: ret;" L
   s" }" L ;

EMIT-SAXPY
