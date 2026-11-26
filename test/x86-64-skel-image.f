\ Cross-builds the x86-64 boot, X64BOOT's `_start`, into bare executables for an
\ x86-64 peer.

\ boot-x64.f loads the x86-64 seam, so it comes before the harness, which would
\ otherwise load the seam into its own private wordlist.
require src/habu/boot-x64.f
require src/habu/task-abi.f
require src/core/exit-status.f
require src/arch/x86-64/rt.f
require test/x86-64-peer-harness.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/lib/process-command.f

package X64SKEL

11 constant SIGSEGV
3 constant ARGC                                 \ the image's path, a and b
70 constant FLOOR-EXIT

: REG ( n -- X64ASM:r64 ) X64ASM:>R64 ;

\ Push `n` cells onto the data stack, rcx counting them down.
: FILL, ( n -- ) {: n:n :}
   X64CODE:LBL {: top:label :}
   X64ASM:RCX n X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   top X64CODE:LBL,
   0 X64RT:G-PUSH
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:C-NE top X64CODE:JCC, ;

\ Write one cell at the address a register holds.
: TOUCH, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}  r r X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ Write one cell at the region address a register holds, through its RW alias.
: TOUCH-REGION, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64ASM:RCX r X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX ENGINE-GPR:X64-RBASE REG REGION-ALIAS:DELTA-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RCX TOUCH, ;

\ Write one cell at the address the DATA cell at `off` holds.
: TOUCH-CELL, ( n -- ) {: off:n :}
   X64ASM:RCX ENGINE-GPR:X64-RBASE REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX TOUCH, ;

\ An owned task descriptor remains in fixed DATA while its private region may
\ be unmapped concurrently. Build the same chain/region shape TASK publishes,
\ then fetch through a separate guarded stack with an unusable saved RBP.
DATA-START $80 + constant CHAIN-HEAD-OFF
DATA-START $100 + constant TCB-OFF

: ANON-MAP, ( n n n -- ) {: size:n prot:n flags:n :}
   X64ASM:RDI ZERO-REG,
   X64ASM:RSI size X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RDX prot X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:R10 flags X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:R8 -1 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:R9 ZERO-REG,
   NR-MMAP SYS, ;

: TASK-FETCH, ( n -- ) {: mode:n :}
   ENGINE-GPR:X64-RBASE REG {: root:X64ASM:r64 :}
   X64ASM:RCX root CHAIN-HEAD-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX root TASK-CHAIN-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX root TCB-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX root CHAIN-HEAD-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   STACK-ABI:PAGE-BYTES 3 $22 ANON-MAP,
   X64ASM:RAX root TCB-OFF TASK-ABI:REGION-OFF + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RCX root TCB-OFF TASK-ABI:REGION-U-OFF + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   STACK-ABI:PAGE-BYTES 4 * 0 $22 ANON-MAP,
   X64ASM:RDI X64ASM:RAX STACK-ABI:PAGE-BYTES 2 * 1- X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI STACK-ABI:PAGE-BYTES negate X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32
   X64ASM:RSI STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RDX 3 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:R10 $32 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:R8 -1 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,
   X64ASM:RBX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX root TCB-OFF TASK-ABI:REGION-OFF + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RCX STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RAX X64ASM:RCX STACK-ABI:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   mode -7 = if
      X64ASM:RBX root STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:RAX root STACK-ABI:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   then
   mode -6 = if
      X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:RSI STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
      NR-MUNMAP SYS,
   then
   X64ASM:RBP $DEAD0000 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:RAX X64ASM:RBX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   mode -5 = if X64ASM:RAX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
      else X64ASM:RAX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32 then
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-JMP-REG ;

: BUILD ( n str [ CMD:command -- CMD:command ] -- ) {: pushes:n path:str args :}
   X64CODE:ASM-RESET
   X64CODE:LBL X64CODE:LBL {: floor:label code-end:label :}
   floor code-end false X64BOOT:START,
   pushes -2 = if
      X64ASM:RAX ENGINE-GPR:X64-RBASE REG STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-JMP-REG
   else
      pushes -8 = if
         X64ASM:RBP $DEAD0000 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
         X64ASM:RAX $500000 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
         X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-JMP-REG
      else pushes -4 <= if
         pushes TASK-FETCH,
      else pushes -3 = if
         X64ASM:RBP $DEAD0000 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
         X64ASM:RAX $DEAD1000 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
         X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-JMP-REG
      else pushes -1 = if
         0 X64RT:G-POP
         X64ASM:RDI 99 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
         NR-EXIT-GROUP SYS,
      else
         pushes FILL,
         STACK-ABI:RETURN-BASE-CELL TOUCH-CELL,
         STACK-ABI:LOOP-BASE-CELL TOUCH-CELL,
         DP-CELL TOUCH-CELL,
         ENGINE-GPR:X64-DBASE REG TOUCH-REGION,
         ENGINE-GPR:X64-CP REG TOUCH-REGION,
         X64ASM:RDI ENGINE-GPR:X64-RBASE REG ARGC-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
         NR-EXIT-GROUP SYS,
   then then then then
   then
   floor X64CODE:LBL,
   ENGINE-GPR:X64-DSTACK REG
   ENGINE-GPR:X64-RBASE REG STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI FLOOR-EXIT X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   NR-EXIT-GROUP SYS,
   code-end X64CODE:LBL,
   path args X64HARNESS:WRITE-WITH ;


public

\ hb-x64-skel pushes the boot data stack full (STACK-ABI:BOOT-BYTES of cells),
\ writes one cell through every address the boot published (the return and
\ DO/LOOP stack bases, DP, and through the region's alias the code region and
\ its code pointer) and exits with argc read through the user area from
\ ARGC-CELL.
\ -negative pushes one cell more, onto the guard page: the E-STACK-BOUNDS
\ record on fd 2. -underflow reads below an empty data stack; the floor
\ recovery resets its cursor and exits (no catch handler). The fetch
\ images jump outside the image: a fetch's rip says nothing of the code that
\ ran, so the handler dumps, reading through no RBP, and dies by the signal.
\ -guard-fetch jumps into the data stack's low guard page.
\ -foreign-fetch faults outside every owned stack with an aligned, unmapped
\ RBP, -gap-fetch between text and REGION with the same RBP. The task images
\ jump into the guard page below or above a task's data stack, the same with
\ the task's region unmapped (-released-), or with the task's stack in the
\ main DATA's cells too (-switched-).
: RUN ( -- )
   STACK-ABI:BOOT-BYTES CELL / {: full:n :}
   T-RESET
   X64HARNESS:INIT
   ARGC X64HARNESS:EXPECT-EXIT
   full s" hb-x64-skel" TMP-PATH [: s" a" CMD:ARG+ s" b" CMD:ARG+ ;] BUILD
   EXIT-STATUS:STACK-BOUNDS X64HARNESS:EXPECT-EXIT
   full 1+ s" hb-x64-skel-negative" TMP-PATH [: ;] BUILD
   FLOOR-EXIT X64HARNESS:EXPECT-EXIT
   -1 s" hb-x64-skel-underflow" TMP-PATH [: ;] BUILD
   SIGSEGV X64HARNESS:EXPECT-SIGNAL
   -2 s" hb-x64-skel-guard-fetch" TMP-PATH [: ;] BUILD
   -3 s" hb-x64-skel-foreign-fetch" TMP-PATH [: ;] BUILD
   -8 s" hb-x64-skel-gap-fetch" TMP-PATH [: ;] BUILD
   -4 s" hb-x64-skel-task-low-fetch" TMP-PATH [: ;] BUILD
   -5 s" hb-x64-skel-task-high-fetch" TMP-PATH [: ;] BUILD
   -6 s" hb-x64-skel-released-fetch" TMP-PATH [: ;] BUILD
   -7 s" hb-x64-skel-switched-fetch" TMP-PATH [: ;] BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64SKEL:RUN
