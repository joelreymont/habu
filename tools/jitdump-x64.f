\ jitdump-x64.f - one recorded Intel instruction for the shared JIT walker.

require src/arch/x86-64/disasm.f

package JIT-DIS
public

: STEP ( ptr u8 n n -- n )
   X64DIS:DIS1 ;

;package
