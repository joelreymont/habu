\ x86-64-kernel-syscalls.f - the syscall rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer. Each image seals the friend latch and guards a heap span with
\ PROT-SPAN-CALL,: the span meets no band, so the guard must pass it with the
\ latch sealed, which a guard that trapped every sealed span would not. It then
\ guards the span of the protected cell TIER-PROV:N-CELL and checks that the
\ call came back balanced with the data stack empty. hb-x64-kernel-syscalls
\ opens the latch again first, as the boot leaves it, so the guard passes and
\ the image exits 0; hb-x64-kernel-syscalls-negative expects the wrong balance
\ and exits 21. hb-x64-kernel-syscalls-armed keeps the latch sealed, so the
\ guard exits 83, ENGINE-ERROR:SEAL-VIOLATION, as a raw store to that cell does
\ on ARM64 (test/tier.f). The host checks each image's ELF header; running them
\ is the peer's.
require test/x86-64-boot-harness.f

package X64K-SYSCALLS
using X64ASM
using X64CODE

\ A heap cell above every band and below the transaction blob's reach.
DATA-START $10000 + constant HEAP-OFF

\ Guard the one-cell span at DATA offset off.
: GUARD-CELL, ( n -- ) {: off:n :}
   R8 ENGINE-GPR:X64-RBASE >R64 off MEM-OFF ASM-SINK ENC-LEA
   R9 CELL >IMM32 ASM-SINK ENC-MOV-RI32
   R8 R9 X64KERNEL:PROT-SPAN-CALL, ;

: BUILD ( bool bool ptr u8 n -- ) {: negative:bool armed:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!,
   HEAP-OFF GUARD-CELL,
   armed 0= if 0 FRIEND-LATCH-CELL X64HARNESS:CELL!, then
   TIER-PROV:N-CELL GUARD-CELL,
   X64HARNESS:EXPECT-BALANCED,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false false s" hb-x64-kernel-syscalls" TMP-PATH BUILD
   true false s" hb-x64-kernel-syscalls-negative" TMP-PATH BUILD
   false true s" hb-x64-kernel-syscalls-armed" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;package

X64K-SYSCALLS:RUN
