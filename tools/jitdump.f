\ jitdump.f — disassemble a word's JIT-compiled code, in habu. Usage (with
\ src/arch/arm64/disasm.f loaded first):  <program>  ' WORD JD
\ Walks from the xt to the first RET (inclusive), capped at 512 instructions.
: W32@ {: p :} p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
variable JDP  variable JDN
: JD {: xt :}
   xt JDP !  0 JDN !
   BEGIN
     JDP @ W32@ DIS1
     JDN @ 1 + JDN !
     JDP @ W32@ $D65F03C0 =  JDN @ 511 > or
     JDP @ 4 + JDP !
   UNTIL ;
