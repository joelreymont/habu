split-str
\ code-dump-arm.f - one recorded ARM instruction for the shared code walker.

require src/arch/arm64/disasm.f
require src/core/exit-status.f

package DUMP-DECODE
public

: STEP ( str n -- n )
   drop {: a:^ u:n :}
   u 4 < if s" code-dump: partial ARM instruction at recorded end" EXIT-STATUS:INTERNAL die then
   a c@ a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or DIS1
   4 ;

;package
