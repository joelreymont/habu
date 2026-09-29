\ x86-64-skel-image.f - cross-build the x86-64 boot, X64BOOT's `_start`, into
\ two executables for an x86-64 peer. After the boot, hb-x64-skel pushes the
\ boot data stack full (STACK-ABI:BOOT-BYTES of cells), writes one cell through
\ every address the boot published (the return and DO/LOOP stack bases, DP, the
\ code region and its code pointer) and exits with argc read back through the
\ user area from ARGC-CELL, so `hb-x64-skel a b` exits 3. hb-x64-skel-negative
\ pushes one cell more, onto the guard page, and dies SIGSEGV (a shell reports
\ 139). The host checks each image's ELF header; running them is the peer's.
\
\ boot-x64.f loads the x86-64 seam, so it comes before the harness, which would
\ otherwise load the seam into its own private wordlist.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require test/x86-64-peer-harness.f

package X64SKEL
using X64ASM
using X64CODE
using X64RT

: REG ( n -- r64 ) >R64 ;

\ Push `n` cells onto the data stack, rcx counting them down.
: FILL, ( n -- ) {: n:n :}
   LBL {: top:label :}
   RCX n >IMM64 ASM-SINK ENC-MOV-RI64
   top LBL,
   0 G-PUSH
   RCX 1 >IMM8 ASM-SINK ENC-SUB-RI8
   C-NE top JCC, ;

\ Write one cell at the address a register holds.
: TOUCH, ( r64 -- ) {: r:r64 :}  r r MEM-AT ASM-SINK ENC-MOV-MR ;

\ Write one cell at the address the DATA cell at `off` holds.
: TOUCH-CELL, ( n -- ) {: off:n :}
   RCX ENGINE-GPR:X64-RBASE REG off MEM-OFF ASM-SINK ENC-MOV-RM
   RCX TOUCH, ;

: BUILD ( n ptr u8 n -- ) {: pushes:n path:ptr pathu:n :}
   ASM-RESET
   X64BOOT:START,
   pushes FILL,
   STACK-ABI:RETURN-BASE-CELL TOUCH-CELL,
   STACK-ABI:LOOP-BASE-CELL TOUCH-CELL,
   DP-CELL TOUCH-CELL,
   ENGINE-GPR:X64-DBASE REG TOUCH,
   ENGINE-GPR:X64-CP REG TOUCH,
   RDI ENGINE-GPR:X64-RBASE REG ARGC-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   NR-EXIT-GROUP SYS,
   path pathu X64HARNESS:WRITE ;

public

: RUN ( -- )
   STACK-ABI:BOOT-BYTES CELL / {: full:n :}
   T-RESET
   X64HARNESS:INIT
   full s" hb-x64-skel" TMP-PATH BUILD
   full 1+ s" hb-x64-skel-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64SKEL:RUN
