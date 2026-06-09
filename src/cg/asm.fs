\ asm.fs — ARM64 encoders: ICode instruction -> machine-code u32, then little-
\ endian emit. Pure bit-field recipes (ported from habu src/ir/arm64.zig +
\ src/jit/stencils.zig). One encoder per op, uniform ( idx -- u32 ), dispatched by
\ a comptime table indexed by op tag (no if-chains). TRUSTED:.

require icode.fs

$D2800000 constant MOVZ-BASE     \ MOVZ Xd,#imm16{,LSL#hw*16}
$F2800000 constant MOVK-BASE     \ MOVK Xd,#imm16{,LSL#hw*16}
$92800000 constant MOVN-BASE     \ MOVN Xd,#imm16
$8B000000 constant ADD-BASE      \ ADD  Xd,Xn,Xm
$91000000 constant ADDI-BASE     \ ADD  Xd,Xn,#imm12
$CB000000 constant SUB-BASE      \ SUB  Xd,Xn,Xm
$9B007C00 constant MUL-BASE      \ MUL  Xd,Xn,Xm  (MADD Xd,Xn,Xm,XZR)
$D65F03C0 constant RET-INSN      \ RET  (X30)
$D4000001 constant SVC-BASE      \ SVC  #imm16  -> imm16<<5

\ field placements: Rd[4:0], Rn[9:5], Rm[20:16], imm16[20:5], imm12[21:10], hw[22:21]
: enc-movz ( idx -- u32 )  >r  r@ i.a  r> i.b 5 lshift or  MOVZ-BASE or ;
: enc-movk ( idx -- u32 )  >r  r@ i.a  r@ i.b 5 lshift or  r> i.c 16 / 21 lshift or  MOVK-BASE or ;
: enc-movn ( idx -- u32 )  >r  r@ i.a  r> i.b 5 lshift or  MOVN-BASE or ;
: enc-add  ( idx -- u32 )  >r  r@ i.a  r@ i.b 5 lshift or  r> i.c 16 lshift or  ADD-BASE or ;
: enc-addi ( idx -- u32 )  >r  r@ i.a  r@ i.b 5 lshift or  r> i.c 10 lshift or  ADDI-BASE or ;
: enc-sub  ( idx -- u32 )  >r  r@ i.a  r@ i.b 5 lshift or  r> i.c 16 lshift or  SUB-BASE or ;
: enc-mul  ( idx -- u32 )  >r  r@ i.a  r@ i.b 5 lshift or  r> i.c 16 lshift or  MUL-BASE or ;
: enc-ret  ( idx -- u32 )  drop RET-INSN ;
: enc-svc  ( idx -- u32 )  i.a 5 lshift SVC-BASE or ;

create ENCODERS
  ' enc-movz , ' enc-movk , ' enc-movn , ' enc-add , ' enc-addi ,
  ' enc-sub ,  ' enc-mul ,  ' enc-ret , ' enc-svc ,

: enc1 ( idx -- u32 )  dup i.op cells ENCODERS + @ execute ;

\ Encode the whole IR into buf (little-endian, host = LE = ARM64). Returns #bytes.
: ASSEMBLE ( buf -- nbytes )
   dup >r
   #IC @ 0 ?do  i enc1  over l!  4 +  loop
   r> - ;
