\ code-dump-x64.f - one recorded Intel instruction for the shared code walker.

require src/arch/x86-64/disasm.f

package DUMP-DECODE
public

: STEP ( str n -- n )
   X64DIS:DIS1 ;

;package
