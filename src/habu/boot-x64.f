\ boot-x64.f - the x86-64 engine's process entry, package X64BOOT. START, emits
\ `_start`, the twin of src/habu/habu2.f EM-STARTUP up to its first run-time
\ state: it maps the guarded VM stacks, the code region and the DATA region,
\ loads the six VM registers (layout.f ENGINE-GPR) and fills the DATA cells the
\ ARM64 boot fills, then falls through into whatever the stream emits next.
\ docs/x86-64.md "Kernel inventory" lists each register and cell beside its
\ ARM64 twin.
\
\ It is straight-line code that never calls or pushes, so rsp stays where the
\ kernel left it, at argc, until the DATA cells take the argument vector.
\
\ The syscall numbers and SYS, are the x86-64 seam's, loaded below the way
\ tools/native-emit.f loads a target's seam before the files that emit against
\ it. A file that loads the seam into a private wordlist first (the peer
\ harness, test/x86-64-peer-harness.f) must come after this one: the require
\ below would then load nothing and the names here would be undefined.
require lib/byte-buffer.f
require lib/string.f
require src/core/cell.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f

package X64BOOT
using X64ASM
using X64CODE

\ The host's layout names its own DATA. Replay the Linux target's layout here
\ so cross-built boot instructions use the target addresses and sizes.
s" src/os/linux-x86-64/layout.f" included

0 constant PROT-NONE
3 constant PROT-RW                  \ PROT_READ|PROT_WRITE
\ The rc every boot mapping failure exits with: STACK-GUARD's MAP-FAIL-RC
\ (src/habu/rt.f) and habu2.f's two fixed-region mappings.
78 constant MAP-FAIL-RC
2 constant STDERR

\ The three failures the boot names, one label each per image.
variable STACK-BAD
variable REGION-BAD
variable DATA-BAD

: RBASE-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSTACK-REG ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;

: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

\ Store a register into the DATA cell at `off`, through rbp once it is DATA.
: CELL! ( r64 n -- ) {: r:r64 off:n :}
   r RBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ mmap(rdi, len, prot, flags, -1, 0) with the address already in rdi, which the
\ syscall keeps. SYS, leaves CF set when the kernel refused.
: MMAP, ( n n n -- ) {: len:n prot:n flags:n :}
   RSI len IMM,  RDX prot IMM,  R10 flags IMM,  R8 -1 IMM,  R9 ZERO-REG,
   NR-MMAP SYS, ;

\ Map `len` read/write bytes exactly at rdi. A refusal returns -errno and a
\ moved mapping another address, so one comparison catches both.
: MAP-FIXED, ( n label -- ) {: len:n bad:label :}
   len PROT-RW MAP-ANON-PRIVATE-FIXED MMAP,
   RAX RDI ASM-SINK ENC-CMP-RR
   C-NE bad JCC, ;

\ The twin of STACK-GUARD:EMIT-MAP (src/habu/rt.f): map one guarded VM stack of
\ `cap` bytes and leave its base in `dst`. The first mapping reserves cap plus
\ three guard pages PROT_NONE; the second reopens cap bytes read/write at the
\ first PAGE-BYTES boundary at least one page in, so at least one inaccessible
\ page lies below the base and one above base + cap, and a push past the
\ capacity faults.
: MAP-STACK, ( n r64 -- ) {: cap:n dst:r64 :}
   STACK-BAD @ >LABEL {: bad:label :}
   RDI ZERO-REG,
   cap STACK-ABI:PAGE-BYTES 3 * +  PROT-NONE  MAP-ANON-PRIVATE MMAP,
   C-B bad JCC,
   RDI RAX STACK-ABI:PAGE-BYTES 2 * 1- MEM-OFF ASM-SINK ENC-LEA
   RDI STACK-ABI:PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   cap PROT-RW MAP-ANON-PRIVATE-FIXED MMAP,
   RAX RDI ASM-SINK ENC-CMP-RR
   C-NE bad JCC,
   dst RDI ASM-SINK ENC-MOV-RR ;

\ rbp = the runtime address of the stream's byte 0, the text content base: rip
\ after the lea is that base plus the lea's end offset. habu2.f EM-STARTUP takes
\ the same base into XREG-RBASE with an ADR.
: TEXT-BASE, ( -- )
   RBASE-REG 0 MEM-RIP ASM-SINK ENC-LEA
   RBASE-REG ASM-LEN >IMM32 ASM-SINK ENC-SUB-RI32 ;

\ r13 = the code region, REGION bytes at the image base plus REGION-OFF, where
\ the linker's PT_LOAD will place it; r15 = its code area past the records;
\ r14 = no records; rbx = 0. The twin of EM-MMAP-CODE-REGION and EM-SEED-DICT.
: CODE-REGION, ( -- )
   RDI RBASE-REG REGION-OFF CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   REGION REGION-BAD @ >LABEL MAP-FIXED,
   DBASE-REG RDI ASM-SINK ENC-MOV-RR
   ENGINE-GPR:X64-CP >R64 DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   ENGINE-GPR:X64-NDICT >R64 ZERO-REG,
   ENGINE-GPR:X64-INTERP >R64 ZERO-REG, ;

\ rax = DATA, DATA-SIZE bytes at DATA-VA, as EM-MMAP-DATA-REGION maps it.
: DATA-REGION, ( -- )
   RDI DATA-VA VA>N IMM,
   DATA-SIZE DATA-BAD @ >LABEL MAP-FIXED, ;

\ The twin of EM-DATA-INIT: publish the text base, then rbp becomes DATA; then
\ the data stack's extent, the argument vector ([rsp] = argc, argv at rsp + 8,
\ envp past argv's null) and the heap floor with the DP that starts at it.
: DATA-INIT, ( -- )
   RBASE-REG RAX RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RBASE-REG RAX ASM-SINK ENC-MOV-RR
   DSTACK-REG STACK-ABI:BASE-CELL CELL!
   RAX STACK-ABI:BOOT-BYTES IMM,  RAX STACK-ABI:CAP-CELL CELL!
   RAX RSP MEM-AT ASM-SINK ENC-MOV-RM  RAX ARGC-CELL CELL!
   RCX RSP CELL MEM-OFF ASM-SINK ENC-LEA  RCX ARGV-CELL CELL!
   RCX RCX RAX CELL CELL MEM-IDX ASM-SINK ENC-LEA  RCX ENVP-CELL CELL!
   RAX DATA-START IMM,  RAX BOOT-LAYOUT:HEAP-START-CELL CELL!
   RAX RBASE-REG DATA-START MEM-OFF ASM-SINK ENC-LEA  RAX DP-CELL CELL! ;

\ The twin of EM-FRAME-STACKS: the return and DO/LOOP frame stacks, published
\ in their DATA cells.
: FRAME-STACKS, ( -- )
   STACK-ABI:RETURN-BYTES RAX MAP-STACK,  RAX STACK-ABI:RETURN-BASE-CELL CELL!
   STACK-ABI:LOOP-BYTES RAX MAP-STACK,  RAX STACK-ABI:LOOP-BASE-CELL CELL! ;

\ Name the failure on fd 2 and exit MAP-FAIL-RC. The message follows the exit
\ syscall inside the loaded text, so it is readable before any region exists.
: FAIL, ( label ptr u8 n -- ) {: at:label a:ptr u:n :}
   LBL {: msg:label :}
   at LBL,
   RDI STDERR IMM,  RSI msg MOVABS,  RDX u 1+ IMM,  NR-WRITE SYS,
   RDI MAP-FAIL-RC IMM,  NR-EXIT-GROUP SYS,
   msg LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   STR-LF ASM-SINK BUF:APPEND-BYTE ;

public

\ Emit `_start`. The ELF entry is the text's byte 0, so it begins the stream.
: START, ( -- )
   LBL STACK-BAD !  LBL REGION-BAD !  LBL DATA-BAD !
   LBL {: booted:label :}
   TEXT-BASE,
   STACK-ABI:BOOT-BYTES DSTACK-REG MAP-STACK,
   CODE-REGION,
   DATA-REGION,
   DATA-INIT,
   FRAME-STACKS,
   booted JMP,
   STACK-BAD @ >LABEL s" hb: cannot map guarded VM stack" FAIL,
   REGION-BAD @ >LABEL s" hb: cannot map fixed code region" FAIL,
   DATA-BAD @ >LABEL s" hb: cannot map fixed data region" FAIL,
   booted LBL, ;

;using
;using
;package
