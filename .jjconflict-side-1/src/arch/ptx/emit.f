\ emit.f - checked PTX text encoder.
\
\ Load after lib/errors.f and lib/string.f.
\
\ PTX-L is the single line sink. By default it types the line to stdout (the emit
\ drivers run under `bin/hb ... > kernel.ptx`). A capture mode routes each line into
\ an in-process buffer instead, so a host-side test can inspect the generated PTX
\ text without spawning a child or shelling out to a file (fail closed on overflow).

$20000 constant PTX-CAP-CAP     \ 128 KB: bk=32 register-blocked kernels with a fused epilogue
                                \ (e.g. LINEAR->GELU 64x64) emit ~40 KB; the N-stage wide MMA tiles
                                \ (lib/ptx/cg-mma.f MMA-PIPE-KLOOP-MULTI: steady body + N-1 unrolled
                                \ epilogue tiles, MFRAGS=4 scalar/B-ldmatrix) emit up to ~90 KB, so
                                \ the capture buffer is sized past the deepest committed sweep config.
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

\ module-target facts as queryable accessors (manifest/export consumers read
\ these instead of scraping the header text). The .target arch and .version are
\ a settable pair, not a literal: an emit driver that targets a specific GPU sets
\ them with PTX-TARGET! before emitting the header, so the .target directive, the
\ .version, and the ptxas -arch all trace to one target. The default is the sm_87
\ Orin fixture, which keeps every host-side unit test host-independent.
32 constant PTX-TGT-CAP
create PTX-ARCH-BUF PTX-TGT-CAP allot
create PTX-VER-BUF  PTX-TGT-CAP allot
variable PTX-ARCH-U
variable PTX-VER-U

: PTX-TGT-STR! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u PTX-TGT-CAP > if E-PTX-CAP throw then
   a dst u BYTE-COPY  u lenp ! ;

: PTX-ARCH! ( ptr u8 n -- )   PTX-ARCH-BUF PTX-ARCH-U PTX-TGT-STR! ;
: PTX-VER!  ( ptr u8 n -- )   PTX-VER-BUF  PTX-VER-U  PTX-TGT-STR! ;

: PTX-TARGET-DEFAULT ( -- )   s" sm_87" PTX-ARCH!  s" 8.3" PTX-VER! ;
PTX-TARGET-DEFAULT

\ parse-and-store the module target: PTX-TARGET! <arch> <version> (two tokens).
\ Used at the top of an emit driver so the spawned child's header names the GPU
\ the parent probed; the tokens are consumed by parse-name, never interpreted.
: PTX-TARGET! ( -- )
   parse-name PTX-ARCH!  parse-name PTX-VER! ;

: PTX-VERSION$ ( -- ptr u8 n )
   PTX-VER-BUF PTX-VER-U @ ;

: PTX-SM-TARGET$ ( -- ptr u8 n )
   PTX-ARCH-BUF PTX-ARCH-U @ ;

: PTX-ADDRESS-SIZE$ ( -- ptr u8 n )
   s" 64" ;

\ Emit the module header from the configured target (default sm_87). The word
\ keeps its historical name so its many callers stay untouched; its output now
\ follows PTX-TARGET!.
: PTX-HEADER ( -- )
   SB-RESET s" .version " SB-APPEND PTX-VERSION$ SB-APPEND SB$ PTX-L
   SB-RESET s" .target " SB-APPEND PTX-SM-TARGET$ SB-APPEND SB$ PTX-L
   SB-RESET s" .address_size " SB-APPEND PTX-ADDRESS-SIZE$ SB-APPEND SB$ PTX-L ;

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
   PTX-HEADER
   PTX-NL
   PTX-SAXPY-ENTRY
   PTX-SAXPY-BODY ;
