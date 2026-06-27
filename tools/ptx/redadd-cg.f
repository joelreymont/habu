\ redadd-cg.f - emit a raw PTX kernel using red.global.add.f32 (atomic float add) to verify
\ the instruction assembles + runs on sm_87 (the scatter-add building block for fan-in
\ adjoints, habu-ptx-ad-verify). Each in-bounds thread atomically adds 1.0 to out[0], so a
\ 256-thread launch yields out[0] = 256.0. Raw PTX (the instruction is not yet in the checked
\ codegen; this is a verification, per docs/autograd.md). Load after lib/errors.f,
\ lib/string.f, src/arch/ptx/emit.f; emits to stdout.

: EMIT-REDADD ( -- )
   PTX-HEADER-SM87  PTX-NL
   s" .visible .entry REDADD(.param .u64 p_out, .param .u32 p_n)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<2>;" PTX-L
   s" .reg .b32 %r<8>;" PTX-L
   s" .reg .b64 %rd<4>;" PTX-L
   s" ld.param.u64 %rd1, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L
   s" mov.u32 %r2, %ctaid.x;" PTX-L
   s" mov.u32 %r3, %ntid.x;" PTX-L
   s" mov.u32 %r4, %tid.x;" PTX-L
   s" mad.lo.u32 %r5, %r2, %r3, %r4;" PTX-L
   s" setp.ge.u32 %p1, %r5, %r1;" PTX-L
   s" @%p1 bra DONE;" PTX-L
   s" cvta.to.global.u64 %rd2, %rd1;" PTX-L
   s" red.global.add.f32 [%rd2], 0f3F800000;" PTX-L
   s" DONE:" PTX-L
   s" ret;" PTX-L
   s" }" PTX-L ;

EMIT-REDADD
