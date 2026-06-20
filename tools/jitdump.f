\ jitdump.f — disassemble a word's JIT-compiled code, in habu.
\ Run: bin/hb --load src/arch/arm64/disasm.f tools/jitdump.f -- '<program>' WORD
\ Inline usage when disasm.f is already loaded: <program> ' WORD JD
\ Walks from the xt to the first RET (inclusive), capped at 512 instructions.
: W32@ {: p :} p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
variable JDP  variable JDN

TRUSTED: JDP@ ( -- ptr u8 )
   JDP @ ;

: JD ( n -- ) {: xt :}
   xt JDP !  0 JDN !
   BEGIN
     JDP@ W32@ DIS1
     JDN @ 1 + JDN !
     JDP@ W32@ $D65F03C0 =  JDN @ 511 > or
     JDP@ 4 + JDP !
   UNTIL ;

: JIT-USAGE ( -- )
   s" usage: bin/hb --load src/arch/arm64/disasm.f tools/jitdump.f -- '<program>' WORD" 64 die ;

TRUSTED: JIT-FIND ( ptr u8 n -- n )
   get-current search-wl dup 0= if s" jitdump: target word not found" 74 die then ;

TRUSTED: JIT-EVALUATE ( ptr u8 n -- )
   evaluate ;

: JIT-MAIN ( -- )
   SCRIPT-ARGC 2 <> if JIT-USAGE then
   0 SCRIPT-ARGV$ JIT-EVALUATE
   1 SCRIPT-ARGV$ JIT-FIND JD ;

: JIT-AUTO ( -- )
   SCRIPT-ARGC 0 > if JIT-MAIN then ;

JIT-AUTO
