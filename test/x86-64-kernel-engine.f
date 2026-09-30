\ x86-64-kernel-engine.f - the engine-state rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer. Each image runs TASK-LIVE-GUARD, and then checks the data stack is
\ empty. hb-x64-kernel-engine runs with no task live, as the boot leaves
\ TASKS-LIVE-CELL, so the guard passes and the image exits 0;
\ hb-x64-kernel-engine-negative expects the wrong depth and exits 21.
\ hb-x64-kernel-engine-armed marks a task live first, so the guard exits 79.
\ hb-x64-kernel-window carries text past one PROT-PAGE-MAX behind the kernel,
\ as the whole kernel will, so its read-write segment lands a page further
\ on; it pushes the text's span and exits 0.
\ The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-ENGINE

PROT-PAGE-MAX constant FILL-BYTES
create FILL FILL-BYTES allot

: BUILD ( bool bool ptr u8 n -- ) {: negative:bool armed:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   armed if 1 TASKS-LIVE-CELL X64HARNESS:CELL!, then
   X64KERNEL:TASK-LIVE-GUARD,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-WINDOW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   FILL FILL-BYTES X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   FILL-BYTES X64HARNESS:EXPECT-POP,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false false s" hb-x64-kernel-engine" TMP-PATH BUILD
   true false s" hb-x64-kernel-engine-negative" TMP-PATH BUILD
   false true s" hb-x64-kernel-engine-armed" TMP-PATH BUILD
   s" hb-x64-kernel-window" TMP-PATH BUILD-WINDOW
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-ENGINE:RUN
