\ code-dump-arm.f - one recorded ARM instruction for the shared code walker.

require src/arch/arm64/disasm.f

package DUMP-DECODE
public

: STEP ( ptr u8 n n -- n )
   drop {: a:ptr u:n :}
   u 4 < if s" code-dump: partial ARM instruction at recorded end" 74 die then
   a c@ a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or DIS1
   4 ;

;package
