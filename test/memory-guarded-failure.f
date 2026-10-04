\ Deny only a writable mmap after MEM-ALLOC-GUARDED reserves its inaccessible
\ span. A failed fixed remap must return all virtual pages in that span.
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/ffi-abi.f

package MEM-GUARD-FAIL-TEST

38 constant PR-SET-NO-NEW-PRIVS
22 constant PR-SET-SECCOMP
2 constant SECCOMP-FILTER
9 constant NR-MMAP
3 constant PROT-RW
$5000C constant SECCOMP-ENOMEM
$7FFF0000 constant SECCOMP-ALLOW
128 constant STATM-CAP

create FILTER 48 allot
create PROGRAM 16 allot
create STATM STATM-CAP allot
variable START-VM

PROCESS-SYMBOLS
FUNCTION: PRCTL-NO-PRIV prctl ( n n n n n -- i32 ) 1 VARIADIC ;FUNCTION
FUNCTION: PRCTL-SECCOMP prctl ( n n ptr u8 n n -- i32 ) 1 VARIADIC ;FUNCTION
FUNCTION: PAGE-SIZE getpagesize ( -- i32 ) ;FUNCTION

\ The foreign sock_fprog embeds a raw pointer at byte 8.
TRUSTED: FILTER-IN-PROG ( -- ) FILTER PROGRAM 8 + ! ;

: U32! ( n ptr u8 -- ) {: value:n at:ptr :}
   4 0 do
      value i 8 * rshift $FF and at i + c!
   loop ;

\ sock_filter is {u16 code, u8 jt, u8 jf, u32 k}.
: INSN ( n n n n n -- ) {: idx:n code:n jt:n jf:n k:n :}
   FILTER idx 8 * + {: at:ptr :}
   code $FF and at c!
   code 8 rshift $FF and at 1 + c!
   jt at 2 + c!
   jf at 3 + c!
   k at 4 + U32! ;

: INSTALL-FILTER ( -- )
   \ Load nr; allow everything except mmap(PROT_READ|PROT_WRITE).
   0 $20 0 0 0 INSN
   1 $15 0 3 NR-MMAP INSN
   2 $20 0 0 32 INSN
   3 $15 0 1 PROT-RW INSN
   4 $06 0 0 SECCOMP-ENOMEM INSN
   5 $06 0 0 SECCOMP-ALLOW INSN
   6 PROGRAM c!
   FILTER-IN-PROG
   PR-SET-NO-NEW-PRIVS 1 0 0 0 PRCTL-NO-PRIV 0 <> if
      s" memory-guarded-failure: no_new_privs refused" 78 die
   then
   PR-SET-SECCOMP SECCOMP-FILTER PROGRAM 0 0 PRCTL-SECCOMP 0 <> if
      s" memory-guarded-failure: seccomp filter refused" 78 die
   then ;

: STATM-PAGES ( -- n )
   s" /proc/self/statm" STATM STATM-CAP READ-ALL {: u:n :}
   0 u 0 do
      STATM i + c@ dup $20 = if drop unloop exit then
      $30 - swap 10 * +
   loop
   E-FS-IO throw ;

: VM-BYTES ( -- n )
   STATM-PAGES PAGE-SIZE * ;

: FAIL-REMAP ( -- )
   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED MEM-RELEASE-GUARDED ;

: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if
      T-RESET
      VM-BYTES START-VM !
      INSTALL-FILTER
      ['] FAIL-REMAP catch {: rc:n :}
      s" fixed writable mmap is refused" T-LABEL
      rc E-MEM-MAP T=
      s" failed guarded allocation releases its reserved span" T-LABEL
      VM-BYTES START-VM @ T=
      T-REPORT
   else
      s" test: ok" type cr
   then ;

RUN
;package
