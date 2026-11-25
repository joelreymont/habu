\ code-dump-arm.f - one recorded ARM instruction for the shared code walker.

require src/arch/arm64/disasm.f

package DUMP-DECODE
public

: STEP ( str n -- n )
   drop STR:DATA {: a:^ :}
   a c@ a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or A64DIS:DIS1
   4 ;

;package
