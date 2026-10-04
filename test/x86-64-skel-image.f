\ x86-64-skel-image.f - cross-build the x86-64 boot, X64BOOT's `_start`, into
\ bare executables for an x86-64 peer. After the boot, hb-x64-skel pushes the
\ boot data stack full (STACK-ABI:BOOT-BYTES of cells), writes one cell through
\ every address the boot published (the return and DO/LOOP stack bases, DP, the
\ code region and its code pointer) and exits with argc read back through the
\ user area from ARGC-CELL, so `hb-x64-skel a b` exits 3. hb-x64-skel-negative
\ pushes one cell more, onto the guard page, and the crash handler the boot
\ installs writes `hb: stack bounds exceeded (data)` on fd 2 and exits 102. The
\ underflow image reads below an empty data stack; the boot's floor recovery
\ resets its cursor and exits 70 (it has no catch handler).
\ The guard-fetch image jumps into the data stack's low guard page. An
\ instruction fetch there is a bounds fault (102), never an underdepth throw.
\ The foreign-fetch image faults outside every owned stack with an aligned but
\ unmapped RBP; the handler must dump it (134) without dereferencing RBP.
\ The gap-fetch image faults between text and REGION with the same untrusted
\ RBP; that unmapped gap cannot authorize a DATA descriptor either.
\ The host checks each image's ELF header; running them is the peer's.
\
\ boot-x64.f loads the x86-64 seam, so it comes before the harness, which would
\ otherwise load the seam into its own private wordlist.
require src/habu/boot-x64.f
require src/habu/task-abi.f
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

\ An owned task descriptor remains in fixed DATA while its private region may
\ be unmapped concurrently. Build the same chain/region shape TASK publishes,
\ then fetch through a separate guarded stack with an unusable saved RBP.
DATA-START $80 + constant CHAIN-HEAD-OFF
DATA-START $100 + constant TCB-OFF

: ANON-MAP, ( n n n -- ) {: size:n prot:n flags:n :}
   RDI ZERO-REG,
   RSI size >IMM32 ASM-SINK ENC-MOV-RI32
   RDX prot >IMM32 ASM-SINK ENC-MOV-RI32
   R10 flags >IMM32 ASM-SINK ENC-MOV-RI32
   R8 -1 >IMM64 ASM-SINK ENC-MOV-RI64
   R9 ZERO-REG,
   NR-MMAP SYS, ;

: TASK-FETCH, ( n -- ) {: mode:n :}
   ENGINE-GPR:X64-RBASE REG {: root:r64 :}
   RCX root CHAIN-HEAD-OFF MEM-OFF ASM-SINK ENC-LEA
   RCX root TASK-CHAIN-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX root TCB-OFF MEM-OFF ASM-SINK ENC-LEA
   RCX root CHAIN-HEAD-OFF MEM-OFF ASM-SINK ENC-MOV-MR
   STACK-ABI:PAGE-BYTES 3 $22 ANON-MAP,
   RAX root TCB-OFF TASK-ABI:REGION-OFF + MEM-OFF ASM-SINK ENC-MOV-MR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-MOV-RI32
   RCX root TCB-OFF TASK-ABI:REGION-U-OFF + MEM-OFF ASM-SINK ENC-MOV-MR
   STACK-ABI:PAGE-BYTES 4 * 0 $22 ANON-MAP,
   RDI RAX STACK-ABI:PAGE-BYTES 2 * 1- MEM-OFF ASM-SINK ENC-LEA
   RDI STACK-ABI:PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   RSI STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-MOV-RI32
   RDX 3 >IMM32 ASM-SINK ENC-MOV-RI32
   R10 $32 >IMM32 ASM-SINK ENC-MOV-RI32
   R8 -1 >IMM64 ASM-SINK ENC-MOV-RI64
   R9 ZERO-REG,
   NR-MMAP SYS,
   RBX RAX ASM-SINK ENC-MOV-RR
   RCX root TCB-OFF TASK-ABI:REGION-OFF + MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RAX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-MOV-RI32
   RAX RCX STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   mode -7 = if
      RBX root STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
      RAX root STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   then
   mode -6 = if
      RDI RCX ASM-SINK ENC-MOV-RR
      RSI STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-MOV-RI32
      NR-MUNMAP SYS,
   then
   RBP $DEAD0000 >IMM64 ASM-SINK ENC-MOV-RI64
   RAX RBX ASM-SINK ENC-MOV-RR
   mode -5 = if RAX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
      else RAX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-SUB-RI32 then
   RAX ASM-SINK ENC-JMP-REG ;

: BUILD ( n ptr u8 n -- ) {: pushes:n path:ptr pathu:n :}
   ASM-RESET
   LBL LBL {: floor:label code-end:label :}
   floor code-end X64BOOT:START,
   pushes -2 = if
      RAX ENGINE-GPR:X64-RBASE REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
      RAX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-SUB-RI32
      RAX ASM-SINK ENC-JMP-REG
   else
      pushes -8 = if
         RBP $DEAD0000 >IMM64 ASM-SINK ENC-MOV-RI64
         RAX $500000 >IMM64 ASM-SINK ENC-MOV-RI64
         RAX ASM-SINK ENC-JMP-REG
      else pushes -4 <= if
         pushes TASK-FETCH,
      else pushes -3 = if
         RBP $DEAD0000 >IMM64 ASM-SINK ENC-MOV-RI64
         RAX $DEAD1000 >IMM64 ASM-SINK ENC-MOV-RI64
         RAX ASM-SINK ENC-JMP-REG
      else pushes -1 = if
         0 G-POP
         RDI 99 >IMM32 ASM-SINK ENC-MOV-RI32
         NR-EXIT-GROUP SYS,
      else
         pushes FILL,
         STACK-ABI:RETURN-BASE-CELL TOUCH-CELL,
         STACK-ABI:LOOP-BASE-CELL TOUCH-CELL,
         DP-CELL TOUCH-CELL,
         ENGINE-GPR:X64-DBASE REG TOUCH,
         ENGINE-GPR:X64-CP REG TOUCH,
         RDI ENGINE-GPR:X64-RBASE REG ARGC-CELL MEM-OFF ASM-SINK ENC-MOV-RM
         NR-EXIT-GROUP SYS,
   then then then then
   then
   floor LBL,
   ENGINE-GPR:X64-DSTACK REG
   ENGINE-GPR:X64-RBASE REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RDI 70 >IMM32 ASM-SINK ENC-MOV-RI32
   NR-EXIT-GROUP SYS,
   code-end LBL,
   path pathu X64HARNESS:WRITE ;

public

: RUN ( -- )
   STACK-ABI:BOOT-BYTES CELL / {: full:n :}
   T-RESET
   X64HARNESS:INIT
   full s" hb-x64-skel" TMP-PATH BUILD
   full 1+ s" hb-x64-skel-negative" TMP-PATH BUILD
   -1 s" hb-x64-skel-underflow" TMP-PATH BUILD
   -2 s" hb-x64-skel-guard-fetch" TMP-PATH BUILD
   -3 s" hb-x64-skel-foreign-fetch" TMP-PATH BUILD
   -8 s" hb-x64-skel-gap-fetch" TMP-PATH BUILD
   -4 s" hb-x64-skel-task-low-fetch" TMP-PATH BUILD
   -5 s" hb-x64-skel-task-high-fetch" TMP-PATH BUILD
   -6 s" hb-x64-skel-released-fetch" TMP-PATH BUILD
   -7 s" hb-x64-skel-switched-fetch" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64SKEL:RUN
