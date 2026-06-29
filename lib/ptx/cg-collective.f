\ cg-collective.f - PTX codegen: emit-mode lowering for the M6 row/collective ops.
\
\ The emit dual of lib/ptx/collective.f, exactly as lib/ptx/cg.f is for the M4
\ tile ops: each op's runtime value is a PTX register NUMBER, and running a
\ checked KERNEL: body in emit mode produces its PTX. One block per row; the
\ block reduction (BLOCK-MAX / BLOCK-SUM) is a shared-memory + bar.sync pass with
\ a thread-0 sequential fold. Each collective applies its own inactive-lane
\ identity before writing shared memory, so direct sums and backward cotangents
\ do not inherit ROW-LOAD's max-friendly -inf seed. Param registers (CG-SM-PARAMS):
\ in=%rd1, out=%rd2, k=%r1. Load after lib/errors.f, lib/string.f, lib/fmt.f,
\ src/arch/ptx/emit.f, lib/ptx/header.f, and lib/ptx/cg.f. Checked Habu.

: CG-BLOCK ( -- n )
   PTX-BLOCK@ dup PTX-BLOCK-CHECK ;

: SMEM-BYTES ( -- n )
   CG-BLOCK 4 * ;

: CG-REDUCE-OP$ ( n -- ptr u8 n )
   case
      0 of s" max.f32 " endof
      1 of s" add.f32 " endof
      2 of s" min.f32 " endof
      drop s" cg: unknown reduce op" 76 die
   endcase ;

: CG-REDUCE-ID$ ( n -- ptr u8 n )
   case
      0 of s" 0fFF800000" endof
      1 of s" 0f00000000" endof
      2 of s" 0f7F800000" endof
      drop s" cg: unknown reduce op" 76 die
   endcase ;

\ A kernel arg is a register number; the thin from-register casts are the
\ codegen analogue of MK-SPAN's from_raw_parts boundary (see lib/ptx/cg.f).
TRUSTED: MATRIX-REG ( n -- matrix<space-global,f32,extent-r,extent-c> ) ;

\ --- softmax entry scaffolding (distinct from cg.f's SAXPY scaffolding) ---
: CG-SM-RESET ( -- )  1 CG-NF !  3 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;
: CG-SM-ENTRY ( -- )
   s" .visible .entry SOFTMAX_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;
: CG-SM-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<16>;" PTX-L
   s" .reg .f32 %f<64>;" PTX-L
   s" .reg .b32 %r<64>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L
   SB-RESET s" .shared .align 4 .b8 SMEM[" CG-S SMEM-BYTES SB-U s" ];" CG-S CG-LINE ;
: CG-SM-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_in];" PTX-L
   s" ld.param.u64 %rd2, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;
: CG-SM-RET ( -- )    s" ret;" PTX-L ;
: CG-SM-CLOSE ( -- )  s" }" PTX-L ;

\ backward entry: x (input), dy (incoming cotangent), out (dx); x=%rd1 dy=%rd2
\ out=%rd3 k=%r1.  Reuses CG-SM-OPEN/RET/CLOSE.
: CG-BW-RESET ( -- )  1 CG-NF !  4 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;
: CG-BW-ENTRY ( -- )
   s" .visible .entry SOFTMAX_BWD(.param .u64 p_x, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;
: CG-BW-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dy];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

\ --- per-op emitters (register numbers) ---
\ ROW: r = blockIdx.x (one block per row).
: EMIT-ROW ( -- n )
   CG-NEXT-R {: r :}
   SB-RESET s" mov.u32 " CG-S r CG-R s" , %ctaid.x;" CG-S CG-LINE
   r ;

\ ROW-SPAN: rowbase = cvta.global(base) + row*k*4   (k=%r1).
: EMIT-ROW-SPAN ( n n -- n ) {: base row :}
   CG-NEXT-R {: rm :}
   SB-RESET s" mul.lo.s32 " CG-S rm CG-R s" , " CG-S row CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-RD {: off :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S rm CG-R s" , 4;" CG-S CG-LINE
   CG-NEXT-RD {: g :}
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S base CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-RD {: rb :}
   SB-RESET s" add.u64 " CG-S rb CG-RD s" , " CG-S g CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   rb ;

\ ROW-CTX: per-thread column byte offset = tid*4 (span unused; bounds recomputed
\ at load/store from %tid.x and %r1).
: EMIT-ROW-CTX ( n -- n ) {: span :}
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-RD {: c :}
   SB-RESET s" mul.wide.u32 " CG-S c CG-RD s" , " CG-S rt CG-R s" , 4;" CG-S CG-LINE
   c ;

\ ROW-LOAD: masked load from rowbase+coloff; inactive lanes seed -inf.
: EMIT-ROW-LOAD ( n n -- n ) {: span ctx :}
   CG-NEXT-RD {: a :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S span CG-RD s" , " CG-S ctx CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: p :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-F {: t :}
   SB-RESET s" mov.f32 " CG-S t CG-F s" , 0fFF800000;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  ld.global.f32 " CG-S t CG-F s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   t ;

\ Block reduction: smem[tid]=active?tile:identity; bar; thread0 folds
\ 0..PTX-BLOCK@-1; bar; bcast. op: 0=max 1=add 2=min.
: EMIT-REDUCE ( n n -- n ) {: tile op :}        \ op: 0=max 1=add 2=min
   CG-NEXT-R {: rt :}  CG-NEXT-R {: roff :}  CG-NEXT-R {: rsm :}  CG-NEXT-R {: ra :}
   CG-NEXT-L {: lloop :}  CG-NEXT-L {: lend :}  CG-NEXT-L {: lskip :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: pact:n :}
   SB-RESET s" setp.lt.u32 " CG-S pact CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-F {: tval:n :}
   SB-RESET s" mov.f32 " CG-S tval CG-F s" , " CG-S op CG-REDUCE-ID$ CG-S s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S pact CG-P s"  mov.f32 " CG-S tval CG-F s" , " CG-S tile CG-F s" ;" CG-S CG-LINE
   SB-RESET s" mul.lo.s32 " CG-S roff CG-R s" , " CG-S rt CG-R s" , 4;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rsm CG-R s" , SMEM;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S ra CG-R s" , " CG-S rsm CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   SB-RESET s" st.shared.f32 [" CG-S ra CG-R s" ], " CG-S tval CG-F s" ;" CG-S CG-LINE
   SB-RESET s" bar.sync 0;" CG-S CG-LINE
   CG-NEXT-P {: p0 :}
   SB-RESET s" setp.ne.u32 " CG-S p0 CG-P s" , " CG-S rt CG-R s" , 0;" CG-S CG-LINE
   SB-RESET s" @" CG-S p0 CG-P s"  bra " CG-S lskip CG-L s" ;" CG-S CG-LINE
   CG-NEXT-F {: facc :}
   SB-RESET s" ld.shared.f32 " CG-S facc CG-F s" , [SMEM];" CG-S CG-LINE
   CG-NEXT-R {: ri :}
   SB-RESET s" mov.u32 " CG-S ri CG-R s" , 1;" CG-S CG-LINE
   lloop CG-LDEF
   CG-NEXT-P {: pd :}
   SB-RESET s" setp.ge.u32 " CG-S pd CG-P s" , " CG-S ri CG-R s" , " CG-S CG-BLOCK SB-U s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S pd CG-P s"  bra " CG-S lend CG-L s" ;" CG-S CG-LINE
   CG-NEXT-R {: ro2 :}  CG-NEXT-R {: ra2 :}  CG-NEXT-F {: fe :}
   SB-RESET s" mul.lo.s32 " CG-S ro2 CG-R s" , " CG-S ri CG-R s" , 4;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S ra2 CG-R s" , " CG-S rsm CG-R s" , " CG-S ro2 CG-R s" ;" CG-S CG-LINE
   SB-RESET s" ld.shared.f32 " CG-S fe CG-F s" , [" CG-S ra2 CG-R s" ];" CG-S CG-LINE
   SB-RESET op CG-REDUCE-OP$ CG-S facc CG-F s" , " CG-S facc CG-F s" , " CG-S fe CG-F s" ;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S ri CG-R s" , " CG-S ri CG-R s" , 1;" CG-S CG-LINE
   SB-RESET s" bra " CG-S lloop CG-L s" ;" CG-S CG-LINE
   lend CG-LDEF
   SB-RESET s" st.shared.f32 [SMEM], " CG-S facc CG-F s" ;" CG-S CG-LINE
   lskip CG-LDEF
   SB-RESET s" bar.sync 0;" CG-S CG-LINE
   CG-NEXT-F {: u :}
   SB-RESET s" ld.shared.f32 " CG-S u CG-F s" , [SMEM];" CG-S CG-LINE
   u ;

: EMIT-BLOCK-MAX ( n -- n )  0 EMIT-REDUCE ;
: EMIT-BLOCK-SUM ( n -- n )  1 EMIT-REDUCE ;
: EMIT-BLOCK-MIN ( n -- n )  2 EMIT-REDUCE ;          \ used by BLOCK-MAX-SELECT

\ BLOCK-MAX-SELECT (the BLOCK-MAX adjoint): route the cotangent ds to the LOWEST
\ lane where x==mx and 0 elsewhere. candidate = (x==mx) ? float(tid) : +inf; the
\ arg-max lane is block-min(candidate); dx = (tid==argmax) ? ds : 0.
: EMIT-BLOCK-MAX-SELECT ( n n n -- n ) {: ds x mx :}
   CG-NEXT-R {: rt :}  CG-NEXT-F {: ftid :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   SB-RESET s" cvt.rn.f32.u32 " CG-S ftid CG-F s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: peq :}  CG-NEXT-F {: fcand :}
   SB-RESET s" setp.eq.f32 " CG-S peq CG-P s" , " CG-S x CG-F s" , " CG-S mx CG-F s" ;" CG-S CG-LINE
   SB-RESET s" mov.f32 " CG-S fcand CG-F s" , 0f7F800000;" CG-S CG-LINE          \ +inf
   SB-RESET s" selp.f32 " CG-S fcand CG-F s" , " CG-S ftid CG-F s" , " CG-S fcand CG-F s" , " CG-S peq CG-P s" ;" CG-S CG-LINE
   fcand EMIT-BLOCK-MIN {: fargmax :}
   CG-NEXT-P {: psel :}  CG-NEXT-F {: fdx :}
   SB-RESET s" setp.eq.f32 " CG-S psel CG-P s" , " CG-S ftid CG-F s" , " CG-S fargmax CG-F s" ;" CG-S CG-LINE
   SB-RESET s" mov.f32 " CG-S fdx CG-F s" , 0f00000000;" CG-S CG-LINE             \ 0
   SB-RESET s" selp.f32 " CG-S fdx CG-F s" , " CG-S ds CG-F s" , " CG-S fdx CG-F s" , " CG-S psel CG-P s" ;" CG-S CG-LINE
   fdx ;

\ B- : tile - uniform (broadcast subtract)
: EMIT-B- ( n n -- n ) {: tile unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" sub.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ EXP. : ex2.approx(z * log2e)
: EMIT-EXP ( n -- n ) {: tile :}
   CG-NEXT-F {: r :}
   SB-RESET s" mul.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , 0f3FB8AA3B;" CG-S CG-LINE
   SB-RESET s" ex2.approx.f32 " CG-S r CG-F s" , " CG-S r CG-F s" ;" CG-S CG-LINE
   r ;

\ B/ : tile / uniform (broadcast divide, IEEE round-to-nearest)
: EMIT-B/ ( n n -- n ) {: tile unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" div.rn.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ U/ : uniform / uniform - the scalar divide the B/ adjoint needs (ds = -Sum(dz*z)/s)
: EMIT-U/ ( n n -- n ) {: a b :}
   CG-NEXT-F {: r :}
   SB-RESET s" div.rn.f32 " CG-S r CG-F s" , " CG-S a CG-F s" , " CG-S b CG-F s" ;" CG-S CG-LINE
   r ;

\ BROADCAST : uniform -> tile (every lane already holds the reduced scalar)
: EMIT-BROADCAST ( n -- n ) {: unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" mov.f32 " CG-S r CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ NEG : tile -> -tile (used by the B-/B/ adjoints in the AD pass)
: EMIT-NEG ( n -- n ) {: tile :}
   CG-NEXT-F {: r :}
   SB-RESET s" neg.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" ;" CG-S CG-LINE
   r ;

\ ROW-STORE : masked store to rowbase+coloff (active lanes only)
: EMIT-ROW-STORE ( n n n -- ) {: tile span ctx :}
   CG-NEXT-RD {: a :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S span CG-RD s" , " CG-S ctx CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: p :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  st.global.f32 [" CG-S a CG-RD s" ], " CG-S tile CG-F s" ;" CG-S CG-LINE ;
