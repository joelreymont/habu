\ x86-64-kernel-atomics.f - the atomics and publication rows of the x86-64
\ kernel (src/habu/kernel-x64.f) in the booted harness, cross-built for an
\ x86-64 peer. hb-x64-kernel-atomics pushes a value and the address of a
\ scratch cell, stores the one through the other, and checks the scratch cell
\ holds the value and the data stack is empty, then exits 0;
\ hb-x64-kernel-atomics-negative expects the wrong value and exits 21. The host
\ checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-ATOMICS
using X64ASM
using X64CODE
using X64RT

77 constant VALUE

\ ( n ptr -- ) the shape `atomic!` takes: pop the address, then the value, and
\ store it.
: STORE, ( -- )
   1 G-POP  0 G-POP
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   VALUE X64HARNESS:PUSH,
   0 X64HARNESS:PUSH-SCRATCH,
   STORE,
   VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-atomics" TMP-PATH BUILD
   true s" hb-x64-kernel-atomics-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-ATOMICS:RUN
