\ x86-64-kernel-control.f - the control rows of the x86-64 kernel
\ (src/habu/kernel-x64.f CONTROL,) in the booted harness, cross-built for an
\ x86-64 peer. hb-x64-kernel-control stages the string `evaluate` takes, checks
\ the data stack holds it and calls the row, which writes `hb: evaluate is not
\ in the x86-64 kernel` on fd 2 and exits 76. hb-x64-kernel-control-negative
\ expects the wrong depth and exits 21. The host checks each image's ELF
\ header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-CONTROL

: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   s" 1 2 +" X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   s" evaluate" X64HARNESS:CALL-ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-control" TMP-PATH BUILD
   true s" hb-x64-kernel-control-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-CONTROL:RUN
