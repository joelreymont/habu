\ emit.f - checked PTX text encoder.
\
\ Load after lib/errors.f and lib/string.f.
\
\ PTX-L is the single line sink. By default it types the line to stdout (the emit
\ drivers run under `bin/hb ... > kernel.ptx`). A capture mode routes each line into
\ an in-process buffer instead, so a host-side test can inspect the generated PTX
\ text without spawning a child or shelling out to a file (fail closed on overflow).

$8000 constant PTX-CAP-CAP
create PTX-CAP-BUF PTX-CAP-CAP allot
variable PTX-CAP-U
variable PTX-CAPTURE?                       \ 0 = stdout ; nonzero = append into PTX-CAP-BUF

: PTX-CAP+ ( ptr u8 n -- ) {: a:ptr u:n :}   \ append one line + newline, bounds-checked
   PTX-CAP-U @ u + 1 + PTX-CAP-CAP > if E-PTX-CAP throw then
   a  PTX-CAP-BUF PTX-CAP-U @ +  u BYTE-COPY
   PTX-CAP-U @ u + PTX-CAP-U !
   $0A PTX-CAP-BUF PTX-CAP-U @ + c!
   PTX-CAP-U @ 1+ PTX-CAP-U ! ;

: PTX-L ( ptr u8 n -- )
   PTX-CAPTURE? @ if PTX-CAP+ else type cr then ;

: PTX-CAPTURE-ON ( -- )   0 PTX-CAP-U !  -1 PTX-CAPTURE? ! ;
: PTX-CAPTURE-OFF ( -- )  0 PTX-CAPTURE? ! ;
: PTX-CAPTURE$ ( -- ptr u8 n )  PTX-CAP-BUF PTX-CAP-U @ ;

: PTX-NL ( -- )
   PTX-CAPTURE? @ if s" " PTX-CAP+ else cr then ;

: PTX-HEADER-SM87 ( -- )
   s" .version 8.3" PTX-L
   s" .target sm_87" PTX-L
   s" .address_size 64" PTX-L ;

: PTX-SAXPY-ENTRY ( -- )
   s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" PTX-L ;

: PTX-SAXPY-REGS ( -- )
   s" .reg .pred %p<2>;" PTX-L
   s" .reg .f32 %f<5>;" PTX-L
   s" .reg .b32 %r<6>;" PTX-L
   s" .reg .b64 %rd<6>;" PTX-L ;

: PTX-SAXPY-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_y];" PTX-L
   s" ld.param.f32 %f1, [p_a];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L ;

: PTX-SAXPY-TID ( -- )
   s" mov.u32 %r2, %ctaid.x;" PTX-L
   s" mov.u32 %r3, %ntid.x;" PTX-L
   s" mov.u32 %r4, %tid.x;" PTX-L
   s" mad.lo.u32 %r5, %r2, %r3, %r4;" PTX-L ;

: PTX-SAXPY-BOUNDS ( -- )
   s" setp.ge.u32 %p1, %r5, %r1;" PTX-L
   s" @%p1 bra DONE;" PTX-L ;

: PTX-SAXPY-LOADS ( -- )
   s" mul.wide.u32 %rd3, %r5, 4;" PTX-L
   s" cvta.to.global.u64 %rd4, %rd1;" PTX-L
   s" add.u64 %rd4, %rd4, %rd3;" PTX-L
   s" ld.global.f32 %f2, [%rd4];" PTX-L
   s" cvta.to.global.u64 %rd5, %rd2;" PTX-L
   s" add.u64 %rd5, %rd5, %rd3;" PTX-L
   s" ld.global.f32 %f3, [%rd5];" PTX-L ;

: PTX-SAXPY-MATH ( -- )
   s" mul.rn.f32 %f4, %f1, %f2;" PTX-L
   s" add.rn.f32 %f4, %f4, %f3;" PTX-L
   s" st.global.f32 [%rd5], %f4;" PTX-L ;

: PTX-SAXPY-RET ( -- )
   s" DONE:" PTX-L
   s" ret;" PTX-L ;

: PTX-SAXPY-BODY ( -- )
   s" {" PTX-L
   PTX-SAXPY-REGS
   PTX-SAXPY-PARAMS
   PTX-SAXPY-TID
   PTX-SAXPY-BOUNDS
   PTX-SAXPY-LOADS
   PTX-SAXPY-MATH
   PTX-SAXPY-RET
   s" }" PTX-L ;

: PTX-EMIT-SAXPY ( -- )
   PTX-HEADER-SM87
   PTX-NL
   PTX-SAXPY-ENTRY
   PTX-SAXPY-BODY ;
