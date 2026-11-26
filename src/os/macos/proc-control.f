\ proc-control.f -- macOS exact process-control primitive emitters.
\
\ Both report success as 0 and failure as the NEGATED errno, so the supervisor
\ can tell ESRCH (-3) from EPERM (-1) instead of a bare -1. The macOS (BSD)
\ syscall ABI signals failure with the carry flag and leaves the POSITIVE errno
\ in x0, so each word negates x0 into -errno when the carry is set. macOS kill(2)
\ takes a third `posix` argument; passing 1 selects POSIX.1 errno semantics
\ (matching libc), so a process-group signal to a negative pid reports
\ ESRCH/EPERM the same way Linux does.

require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/arch/arm64/asm.f
require src/habu/rt.f
require src/os/macos/sys.f

package ENGINE-EMIT
: BKILLERRNO ( -- )                \ ( pid sig -- rc )
   1 RT:G-POP  0 RT:G-POP
   2 1 A64ICODE:MOVZ,
   SYSCALL:NR-KILL SYSCALL:SYS,
   A64ICODE:LBL {: ok:label :}
   9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,          \ carry clear -> success, x0 already holds 0
      10 0 A64ICODE:MOVZ,  0 10 0 A64ICODE:SUB,
   ok A64ICODE:LBL,
   0 RT:G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   SYSCALL:NR-EXECVE SYSCALL:SYS,
   A64ICODE:LBL {: ok:label :}
   9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,          \ carry clear -> would not return; carry set -> failure
      10 0 A64ICODE:MOVZ,  0 10 0 A64ICODE:SUB,
   ok A64ICODE:LBL,
   0 RT:G-PUSH ;
;package
