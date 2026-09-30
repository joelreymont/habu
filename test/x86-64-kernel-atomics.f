\ x86-64-kernel-atomics.f - the atomics rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer. Every image seals the friend latch first, so each guard a row calls
\ walks its whole span test and clobbers what it may.
\
\ hb-x64-kernel-atomics runs the rows on one heap scratch cell: atomic! stores
\ a value, atomic@ reads it back, fence runs, atomic-add answers the old value
\ and leaves the sum, and atomic-cas answers the value it found, swapping in
\ the new one when that matches the expected value and writing nothing when it
\ is stale. With the data stack empty and the machine stack balanced after, it
\ exits 0; hb-x64-kernel-atomics-negative expects the wrong stored value and
\ exits 21. hb-x64-kernel-atomics-store-armed, -add-armed and -cas-armed aim
\ atomic!, atomic-add and atomic-cas at the band cell TIER-PROV:N-CELL, so
\ each row's guard exits 83, ENGINE-ERROR:SEAL-VIOLATION. The host checks each
\ image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-ATOMICS
using X64ASM
using X64CODE
using X64RT

77 constant VALUE
5 constant DELTA
1000 constant NEW
2000 constant OTHER

: SUM ( -- n ) VALUE DELTA + ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

\ Push the address of the DATA cell at an offset.
: PUSH-CELL, ( n -- ) {: off:n :}
   RAX ENGINE-GPR:X64-RBASE >R64 off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

: STORE-FETCH, ( -- )
   VALUE X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic!" X64HARNESS:CALL-ROW,
   VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   0 X64HARNESS:PUSH-SCRATCH,
   s" atomic@" X64HARNESS:CALL-ROW,
   VALUE X64HARNESS:EXPECT-POP,
   s" fence" X64HARNESS:CALL-ROW, ;

: ADD, ( -- )
   DELTA X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic-add" X64HARNESS:CALL-ROW,
   VALUE X64HARNESS:EXPECT-POP,
   SUM 0 X64HARNESS:EXPECT-SCRATCH, ;

\ atomic-cas with an expected and a new value: check the value it answers and
\ the cell after.
: CAS, ( n n n n -- ) {: expected:n new:n found:n after:n :}
   expected X64HARNESS:PUSH,  new X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic-cas" X64HARNESS:CALL-ROW,
   found X64HARNESS:EXPECT-POP,
   after 0 X64HARNESS:EXPECT-SCRATCH, ;

\ Ten checks, the harness's whole budget.
: ROWS, ( -- )
   STORE-FETCH,  ADD,
   SUM NEW SUM NEW CAS,
   VALUE OTHER NEW NEW CAS,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   SEAL,  ROWS,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Push n zero operands and then the band cell's address, and call the row.
: BUILD-ARMED ( ptr u8 n n ptr u8 n -- ) {: row:ptr rowu:n args:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   args 0 ?do 0 X64HARNESS:PUSH, loop
   TIER-PROV:N-CELL PUSH-CELL,
   row rowu X64HARNESS:CALL-ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-atomics" TMP-PATH BUILD
   true s" hb-x64-kernel-atomics-negative" TMP-PATH BUILD
   s" atomic!" 1 s" hb-x64-kernel-atomics-store-armed" TMP-PATH BUILD-ARMED
   s" atomic-add" 1 s" hb-x64-kernel-atomics-add-armed" TMP-PATH BUILD-ARMED
   s" atomic-cas" 2 s" hb-x64-kernel-atomics-cas-armed" TMP-PATH BUILD-ARMED
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-ATOMICS:RUN
