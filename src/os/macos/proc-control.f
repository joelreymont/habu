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
using A64ASM
using A64ICODE
using RT
using SYSCALL

package ENGINE-EMIT
: BKILLERRNO ( -- )                \ ( pid sig -- rc )
   1 G-POP  0 G-POP
   2 1 MOVZ,
   NR-KILL SYS,
   LBL {: ok:label :}
   9 C-CS CSET,  9 ok CBZ,          \ carry clear -> success, x0 already holds 0
      10 0 MOVZ,  0 10 0 SUB,
   ok LBL,
   0 G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 G-POP  1 G-POP  0 G-POP
   NR-EXECVE SYS,
   LBL {: ok:label :}
   9 C-CS CSET,  9 ok CBZ,          \ carry clear -> would not return; carry set -> failure
      10 0 MOVZ,  0 10 0 SUB,
   ok LBL,
   0 G-PUSH ;
;package

;using
;using
;using
;using
