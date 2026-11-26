split-str
\ boot-x64.f - the x86-64 process entry, package X64BOOT. START, emits
\ `_start` up to its first run-time state, and falls through into whatever the
\ stream emits next. It is straight-line code that never calls or pushes, and
\ whatever moves rsp before DATA-INIT, gives it back: rsp is at argc when the
\ DATA cells take the argument vector.
\ The signal and crash words are the x86-64 side of src/habu/crash.f. A file that loads the x86-64
\ seam into a private wordlist first (test/x86-64-peer-harness.f) must load after this one.
require src/core/exit-status.f
require src/lib/vector.f
require src/lib/string.f
require src/core/cell.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/stack-abi.f
require src/habu/address-cells.f
require src/habu/fault-record.f
require src/habu/crash-x64.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f
require src/os/linux/layout.f

package X64BOOT

0 constant PROT-NONE
3 constant PROT-RW                  \ PROT_READ|PROT_WRITE
5 constant PROT-RX                  \ PROT_READ|PROT_EXEC
2 constant STDERR

\ The four failures the boot names, one label each per image.
variable STACK-BAD
variable REGION-BAD
variable DATA-BAD
variable OCC-BAD
variable TEXT0                       \ the stream's byte 0, which OPEN, binds

: RBASE-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: DSTACK-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;
: DBASE-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DBASE X64ASM:>R64 ;

\ Boot keeps the region's alias - base in the loop index until ALIAS-DELTA,
\ stores it; no code between writes that register.
: DELTA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-INDEX X64ASM:>R64 ;

: IMM, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;

: CELL! ( X64ASM:r64 n -- ) {: r:X64ASM:r64 off:n :}
   r RBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ mmap(rdi, len, prot, flags, -1, 0) with the address already in rdi, which the
\ syscall keeps. SYS, leaves CF set when the call failed.
: MMAP, ( n n n -- ) {: len:n prot:n flags:n :}
   X64ASM:RSI len IMM,  X64ASM:RDX prot IMM,  X64ASM:R10 flags IMM,  X64ASM:R8 -1 IMM,  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS, ;

\ Map `len` read/write bytes exactly at rdi with `flags`. A failure returns
\ -errno (NOREPLACE's EEXIST among them) and a moved mapping another address,
\ so one comparison catches both.
: MAP-FIXED, ( n n label -- ) {: len:n flags:n bad:label :}
   len PROT-RW flags MMAP,
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR
   X64ASM:C-NE bad X64CODE:JCC, ;

public

\ A fresh executable, close-on-exec memfd of REGION bytes, returned in rax.
\ Only EINVAL retries without MFD_EXEC, for Linux before 6.3. On failure no
\ descriptor remains open and rsp is restored. Clobbers the syscall scratch
\ registers and r8; the twin of LIBC-OS:REGION-MEMFD.
: REGION-MEMFD, ( label -- ) {: bad:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: made:label sized:label failed:label :}
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX $6F696765722D6268 IMM,  X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX $6E IMM,  X64ASM:RAX X64ASM:RSP 8 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI $11 IMM,
   NR-MEMFD-CREATE SYS,  X64ASM:C-AE made X64CODE:JCC,
   X64ASM:RAX -22 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE failed X64CODE:JCC,
   X64ASM:RDI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI 1 IMM,
   NR-MEMFD-CREATE SYS,  X64ASM:C-B failed X64CODE:JCC,
   made X64CODE:LBL,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI REGION IMM,
   NR-FTRUNCATE SYS,  X64ASM:C-AE sized X64CODE:JCC,
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  NR-CLOSE SYS,
   failed X64CODE:LBL,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  bad X64CODE:JMP,
   sized X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

private

\ The twin of STACK-GUARD:EMIT-MAP (src/habu/rt.f): map one guarded VM stack of
\ `cap` bytes and leave its base in `dst`. The first mapping reserves cap plus
\ three guard pages PROT_NONE; the second reopens cap bytes read/write at the
\ first PAGE-BYTES boundary at least one page in, so at least one inaccessible
\ page lies below the base and one above base + cap, and a push past the
\ capacity faults.
: MAP-STACK, ( n X64ASM:r64 label -- ) {: cap:n dst:X64ASM:r64 bad:label :}
   X64ASM:RDI ZERO-REG,
   cap STACK-ABI:PAGE-BYTES 3 * +  PROT-NONE  MAP-ANON-PRIVATE MMAP,
   X64ASM:C-B bad X64CODE:JCC,
   X64ASM:RDI X64ASM:RAX STACK-ABI:PAGE-BYTES 2 * 1- X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI STACK-ABI:PAGE-BYTES negate X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32
   cap PROT-RW MAP-ANON-PRIVATE-FIXED MMAP,
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR
   X64ASM:C-NE bad X64CODE:JCC,
   dst X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ r = the runtime address of the stream's byte 0, the text content base: a
\ rip-relative lea of TEXT0 reads no code offset, so placement moves it freely.
: TEXT-BASE, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r TEXT0 @ >LABEL X64CODE:LEA-REL, ;

\ Map the descriptor in r8, REGION bytes shared, at rdi.
: VIEW, ( n n -- ) {: prot:n flags:n :}
   X64ASM:RSI REGION IMM,  X64ASM:RDX prot IMM,  X64ASM:R10 flags IMM,  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,  X64ASM:C-B REGION-BAD @ >LABEL X64CODE:JCC, ;

\ r13 = the code region, REGION-OFF past the image base, and r15 past its
\ records, of which r14 counts none; r12 = its RW alias - r13.
: CODE-REGION, ( -- )
   DBASE-REG RBASE-REG REGION-OFF LINUX-LAYOUT:CODE-OFF - X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   REGION-BAD @ >LABEL REGION-MEMFD,  X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   \ The RX view over the image's own load, the RW alias where the kernel puts it.
   X64ASM:RDI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  PROT-RX MAP-SHARED-FIXED VIEW,
   X64ASM:RDI ZERO-REG,  PROT-RW MAP-SHARED VIEW,
   DELTA-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  DELTA-REG DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  NR-CLOSE SYS,  X64ASM:C-B REGION-BAD @ >LABEL X64CODE:JCC,
   ENGINE-GPR:X64-CP X64ASM:>R64 DBASE-REG DICT-SIZE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   ENGINE-GPR:X64-NDICT X64ASM:>R64 ZERO-REG, ;

\ rax = rdi = DATA, the DATA-SIZE reservation at DATA-VA, as
\ EM-MMAP-DATA-REGION maps it.
: DATA-REGION, ( -- )
   X64ASM:RDI LINUX-LAYOUT:DATA-VA VA>N IMM,
   LINUX-LAYOUT:DATA-SIZE MAP-DATA-RESERVE DATA-BAD @ >LABEL MAP-FIXED, ;

\ The twin of EM-DATA-INIT: publish the text base, then rbp becomes DATA; then
\ the global and package wid cells, the data stack's extent, the argument
\ vector ([rsp] = argc, argv at rsp + 8, envp past argv's null) and the heap
\ floor with the DP that starts at it.
: DATA-INIT, ( bool -- ) {: heap:bool :}
   RBASE-REG X64ASM:RAX RBASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   RBASE-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   \ The text content size the source text-extent words read (layout.f
   \ TEXT-SIZE-CELL), from the header as kernel-x64.f DLSYM, reads it. Every
   \ entry passes here.
   X64ASM:RAX RBASE-REG RBASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX LINUX-LAYOUT:CODE-OFF X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RAX X64ASM:RAX LINUX-LAYOUT:IMAGE-TEXT-SIZE-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX LINUX-LAYOUT:IMAGE-TEXT-CONTENT-ADJ X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RAX TEXT-SIZE-CELL CELL!
   \ A saved image carries DATA, but no process owns its old mutex or index.
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX ADDRESS-CELLS:LOCK-CELL CELL!
   X64ASM:RAX ADDRESS-CELLS:INDEX-CELL CELL!
   \ The lookups' wids, 0 and WL:NAMESPACE at every boot.
   X64ASM:RAX BOOT:GLOBAL-WL-CELL CELL!
   X64ASM:RAX WL:NAMESPACE IMM,  X64ASM:RAX BOOT:NAMESPACE-WL-CELL CELL!
   DSTACK-REG STACK-ABI:BASE-CELL CELL!
   X64ASM:RAX STACK-ABI:BOOT-BYTES IMM,  X64ASM:RAX STACK-ABI:CAP-CELL CELL!
   X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX ARGC-CELL CELL!
   X64ASM:RCX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RCX ARGV-CELL CELL!
   X64ASM:RCX X64ASM:RCX X64ASM:RAX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RCX ENVP-CELL CELL!
   heap if
      X64ASM:RAX DATA-START IMM,  X64ASM:RAX BOOT:HEAP-START-CELL CELL!
      X64ASM:RAX RBASE-REG DATA-START X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RAX DP-CELL CELL!
   then
   \ Every process starts with no definition open, whatever DATA carried.
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX NCOMP-DISPATCH:COMP-LATCH-CELL CELL! ;

: ALIAS-DELTA, ( -- ) DELTA-REG REGION-ALIAS:DELTA-CELL CELL! ;

\ This process's definition occurrence allocation (layout.f DEF-OCC): a fresh
\ mapping whatever DATA the image carried, whose pointer DATA-INIT's caller
\ could not have restored. The records already counted, [0,NDICT), take the
\ first numbers in order, as ARM64's EM-DEF-OCC LINIT issues them: slot i+1
\ holds i+1, and the first cell, the last number issued, holds NDICT. Without
\ them a linked record has no selectable occurrence.
: OCC-INIT, ( -- )
   X64CODE:LBL X64CODE:LBL {: issue:label issued:label :}
   X64ASM:RDI ZERO-REG,
   DEF-OCC:STATE-BYTES PROT-RW MAP-ANON-PRIVATE MMAP,
   X64ASM:C-B OCC-BAD @ >LABEL X64CODE:JCC,
   X64ASM:RAX DEF-OCC:PTR-CELL CELL!
   X64ASM:RCX ZERO-REG,
   issue X64CODE:LBL,
      X64ASM:RCX ENGINE-GPR:X64-NDICT X64ASM:>R64 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE issued X64CODE:JCC,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
      X64ASM:RCX X64ASM:RAX X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      issue X64CODE:JMP,
   issued X64CODE:LBL,
   X64ASM:RCX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ The twin of EM-FRAME-STACKS: the return and DO/LOOP frame stacks, published
\ in their DATA cells.
: FRAME-STACKS, ( -- )
   STACK-ABI:RETURN-BYTES X64ASM:RAX STACK-BAD @ >LABEL MAP-STACK,
   X64ASM:RAX STACK-ABI:RETURN-BASE-CELL CELL!
   STACK-ABI:LOOP-BYTES X64ASM:RAX STACK-BAD @ >LABEL MAP-STACK,
   X64ASM:RAX STACK-ABI:LOOP-BASE-CELL CELL!
   X64ASM:RAX LOOPSP-CELL CELL! ;

\ Bind a failure at the label: its line, newline included, to fd 2 in one
\ write, then exit with the status. The line follows the exit syscall inside
\ the loaded text, so it is readable before any region exists.
: FAIL, ( label str n -- ) {: at:label a:^ u:n rc:n :}
   X64CODE:LBL {: msg:label :}
   at X64CODE:LBL,
   X64ASM:RDI STDERR IMM,  X64ASM:RSI msg X64CODE:MOVABS,  X64ASM:RDX u IMM,  NR-WRITE SYS,
   X64ASM:RDI rc IMM,  NR-EXIT-GROUP SYS,
   msg X64CODE:LBL,
   a u X64CODE:BYTES, ;

\ The width of the stub's one write: under PIPE_BUF, so a pipe takes it whole.
4 constant SIGNO-BYTES

\ The fd word's absolute address. DATA is mapped at DATA-VA and DATA-REGION,
\ rejects a boot the kernel answered elsewhere, so this one address names the
\ same word for the life of the process, whatever task is running.
: FD-WORD-VA ( -- n ) LINUX-LAYOUT:DATA-VA VA>N SIGNAL-ABI:FD-CELL + ;

\ Bind the signal stub at the label: the twin of src/habu/crash.f
\ EMIT-SIGNAL-HANDLER (LSIGH), whose comment gives the contract. It is a
\ `void (int)` sa_handler, so the number arrives in edi; its four low bytes,
\ pushed, are what the fd receives in one write whose result it ignores. It
\ reads the fd word by its absolute address, never through rbp, which is the
\ interrupted thread's own DATA; a word of zero absorbs the signal. It costs
\ rax rcx rdx rsi r11, the flags and a cell below its own rsp, all of which
\ rt_sigreturn restores, and touches no Forth state. Its `ret`,
\ at the second label, enters the restorer the installer named: src/lib/signal.f
\ installs through libc sigaction, which supplies one.
: SIGNAL-STUB, ( label label -- ) {: at:label last:label :}
   at X64CODE:LBL,
   X64CODE:LBL {: done:label :}
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX FD-WORD-VA IMM,
   X64ASM:RDI X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR
   X64ASM:C-E done X64CODE:JCC,
   X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX X64ASM:R64>N X64ASM:>R32 SIGNO-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 NR-WRITE X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32
   X64CODE:ASM-SINK X64ASM:ENC-SYSCALL
   done X64CODE:LBL,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP
   last X64CODE:LBL,  X64CODE:RET, ;

\ Publish the stub bound at the label and the address of the word it reads,
\ so a program installs and arms the stub without spelling either (layout.f
\ package SIGNAL-ABI). The word itself needs no clear: DATA is a fresh mapping
\ here.
: PUBLISH-STUB, ( label -- ) {: stub:label :}
   X64ASM:RAX stub X64CODE:MOVABS,  X64ASM:RAX SIGNAL-ABI:STUB-CELL CELL!
   X64ASM:RAX FD-WORD-VA IMM,  X64ASM:RAX SIGNAL-ABI:FD-PTR-CELL CELL! ;

: PUBLISH-FLOOR, ( label -- )
   X64ASM:RAX swap X64CODE:MOVABS,  X64ASM:RAX FLOORREC-CELL CELL! ;

\ The kernel's own struct sigaction on x86-64, which is not glibc's: the
\ handler, the flags, the restorer, then the blocked mask, SIGSET-BYTES wide,
\ the size rt_sigaction also takes as its fourth argument.
0 constant SA-HANDLER
8 constant SA-FLAGS
$10 constant SA-RESTORER-AT
$18 constant SA-MASK
$20 constant SA-BYTES
8 constant SIGSET-BYTES

\ The action names its restorer. x86-64 has no default one: the kernel will not
\ build a handler's frame for an action without it and kills the process with
\ SIGSEGV instead.
$04000000 constant SA-RESTORER

\ struct sigcontext's slot for each register, by register number: rax rcx rdx
\ rbx rsp rbp rsi rdi, then r8-r15. The kernel lays the slots out r8-r15, rdi,
\ rsi, rbp, rbx, rdx, rax, rcx, rsp, and rip after them.
create GREG-SLOTS
   13 c, 14 c, 12 c, 11 c, 15 c, 10 c, 9 c, 8 c,
   0 c, 1 c, 2 c, 3 c, 4 c, 5 c, 6 c, 7 c,

public

\ The flag that enters a handler with the signal number in rdi, the siginfo in
\ rsi and the ucontext in rdx, and has the kernel fill the siginfo.
4 constant SA-SIGINFO

\ The ucontext's struct sigcontext: where its first slot, r8's, lies, and where
\ rip's lies past the sixteen general registers.
$28 constant UC-GREGS
UC-GREGS 16 CELL * + constant UC-RIP

\ The ucontext offset of the slot that holds a register as the signal
\ interrupted it. The kernel restores every register from its slot when the
\ handler returns, so writing a slot changes the context that resumes.
: UC-GREG ( X64ASM:r64 -- n ) X64ASM:R64>N GREG-SLOTS + c@ CELL * UC-GREGS + ;

\ Bind the restorer at the label: rt_sigreturn, which resumes the interrupted
\ context from the ucontext and never returns. A handler's `ret` enters it, so
\ it goes where no other control falls in.
: RESTORER, ( label -- )
   X64CODE:LBL,
   0 X64ASM:>R32 NR-SIGRETURN X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32
   X64CODE:ASM-SINK X64ASM:ENC-SYSCALL ;

\ Install the handler whose address a register holds for signal n with the
\ given flags, returning through the restorer bound at the label; SA-RESTORER
\ joins the flags. The action is built on the machine stack and rsp comes back
\ where it was. The handler is stored before anything is clobbered, so any
\ register but rsp, and rdi under SIGACTION-AT,, may hold it. It clobbers rax
\ rcx rdx rsi rdi r10 r11 and leaves CF set when the call failed, as SYS,
\ does: the lea that gives the frame back keeps the flags. SIGACTION-RDI,
\ installs it for the signal rdi holds.
: SIGACTION-RDI, ( n X64ASM:r64 label -- ) {: flags:n handler:X64ASM:r64 rest:label :}
   X64ASM:RSP SA-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   handler X64ASM:RSP SA-HANDLER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX flags SA-RESTORER or IMM,  X64ASM:RAX X64ASM:RSP SA-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX rest X64CODE:MOVABS,  X64ASM:RAX X64ASM:RSP SA-RESTORER-AT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RSP SA-MASK X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX ZERO-REG,
   X64ASM:R10 SIGSET-BYTES IMM,
   NR-SIGACTION SYS,
   X64ASM:RSP X64ASM:RSP SA-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA ;

: SIGACTION-AT, ( n n X64ASM:r64 label -- ) {: sig:n flags:n handler:X64ASM:r64 rest:label :}
   X64ASM:RDI sig IMM,  flags handler rest SIGACTION-RDI, ;

\ The same for the handler at a label, through rax.
: SIGACTION, ( n n label label -- ) {: sig:n flags:n handler:label rest:label :}
   X64ASM:RAX handler X64CODE:MOVABS,
   sig flags X64ASM:RAX rest SIGACTION-AT, ;

$08000000 constant SA-ONSTACK

\ The kernel's stack_t, which sigaltstack reads and writes: the base, the int
\ flags, the size; and the flag that ends a registration.
0 constant SS-SP
8 constant SS-FLAGS
16 constant SS-SIZE
24 constant SS-BYTES
2 constant SS-DISABLE

\ The twin of crash.f C-SIGNAL-STACK: rsp drops STACK-ABI:SIGNAL-BYTES and the
\ band above it is registered, so the crash handler, installed SA_ONSTACK, has
\ a stack when a machine-stack overflow has left rsp in the kernel's guard. It
\ clobbers rax rcx rdi rsi r11.
: SIGNAL-STACK, ( -- )
   X64CODE:LBL X64CODE:LBL {: ok:label bad:label :}
   X64ASM:RSP STACK-ABI:SIGNAL-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RAX X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR                                   \ the band's base
   X64ASM:RSP SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX X64ASM:RSP SS-SP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RSP SS-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX STACK-ABI:SIGNAL-BYTES IMM,  X64ASM:RAX X64ASM:RSP SS-SIZE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   X64ASM:RSP X64ASM:RSP SS-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA                     \ the frame back, CF kept
   X64ASM:C-AE ok X64CODE:JCC,
   bad S\" hb: cannot install the signal stack\n" EXIT-STATUS:SYSTEM FAIL,
   ok X64CODE:LBL, ;

\ The way out of the code that registered: SS_DISABLE, then the caller gives
\ the band back. Its result goes unchecked because it cannot fail off the
\ alternate stack: the one failure of a well-formed disable is a thread running
\ on its alternate stack, and rsp is below the band. It clobbers rax rcx rdi
\ rsi r11.
: SIGNAL-STACK-OFF, ( -- )
   X64ASM:RSP SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX SS-DISABLE IMM,  X64ASM:RAX X64ASM:RSP SS-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   X64ASM:RSP SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ The fault entry's label, the twin of crash.f LFAULTREC: OPEN, makes it;
\ X64KERNEL:CONTROL, binds it to a THROW,, or START, with no kernel to an exit.
variable LFAULTREC

private

\ The twin of crash.f EMIT-CRASH-HANDLER. The kernel enters it with rdi = the
\ signal, rsi = the siginfo and rdx = the ucontext. It returns only through
\ the restorer, to resume the thread at another rip, so it owns every
\ register: it keeps those three and the text base where neither the printer
\ nor `syscall` writes.
\
\ It gives a signal the first of crash.f's rows that holds, rows 1 to 6 only to
\ a SIGSEGV or SIGBUS:
\ 0. a saved rip in the crash handler's own code: death by the signal, no dump;
\ 1. the fault address within STACK-ABI:MACHINE-REACH of the saved rsp, either
\    side, whatever code faulted: crash.f's machine-stack record, `at` the
\    saved rip, and exit EXIT-STATUS:STACK-BOUNDS;
\ 2. not Habu code -- a saved rbp of 0, code interrupted on this thread's
\    signal stack, a fetch (rip = fault address), a rip in the signal handlers
\    STUBS, lays out, or a rip outside the image's text and the code region's
\    code: the dump, read through no saved register;
\ 3. FAULT-MARK-CELL set in the saved rbp's DATA, a fault while one is
\    delivered: the dump;
\ 4. the data stack's low guard page and FLOORREC-CELL set: the thread resumes
\    at FLOORREC-CELL's entry, the underdepth throw;
\ 5. the page below a data, return or loop stack's base or the page at its
\    capacity: that stack's record, `in` the word at the saved rip, and exit
\    EXIT-STATUS:STACK-BOUNDS; a base that is zero or not PAGE-BYTES aligned
\    is no stack;
\ 6. anything else: it sets FAULT-MARK-CELL, writes the E-MEMORY-FAULT record
\    and resumes the thread at LFAULTREC with rax = EXIT-STATUS:MEMORY-FAULT
\    and every other register as it faulted, so rbp is DATA, where the
\    kernel's THROW, throws the code.
\
\ The dump, which every other signal takes too, writes to fd 2, one write per
\ line: HEAD$, then sixteen lowercase hex digits per line for the signal, the
\ sixteen registers by number and rip, then the 24 bytes from rip - 8 as three
\ cells in memory order, each 0 unless its eight bytes lie in the code region.
\ The region lies a fixed distance past the text, so its base comes
\ rip-relative, never from the saved r13, and the handler reads no address it
\ cannot prove mapped. Then the process dies by the signal itself, as crash.f
\ C-CRASH-RERAISE does and as row 0 does without the dump: SIG_DFL for it,
\ tgkill to this thread, where it stays pending while its handler runs, and
\ rt_sigreturn, which restores the interrupted context and mask, so it is
\ delivered at the faulting rip.
4 constant SIGILL
5 constant SIGTRAP
7 constant SIGBUS
8 constant SIGFPE
11 constant SIGSEGV
$10 constant SI-ADDR                    \ siginfo_t._sifields._sigfault.si_addr
REGION-OFF LINUX-LAYOUT:CODE-OFF - constant REGION-FROM-TEXT

\ Where the handler keeps the signal, the siginfo, the ucontext, the text base
\ and the code region's base.
: SIG-REG ( -- X64ASM:r64 ) X64ASM:R12 ;
: INFO-REG ( -- X64ASM:r64 ) X64ASM:R13 ;
: UC-REG ( -- X64ASM:r64 ) X64ASM:RBX ;
: TEXT-REG ( -- X64ASM:r64 ) X64ASM:R14 ;
: REGION-REG ( -- X64ASM:r64 ) X64ASM:R15 ;

\ A line is sixteen digits and a newline, built in LINE-FRAME bytes below rsp.
\ Digit n takes the value's nibble n, counted from the least significant, to
\ place n xor the order mask: VALUE-ORDER puts the most significant nibble
\ first, MEMORY-ORDER each byte's two digits in address order.
16 constant DIGITS
DIGITS 1+ constant LINE-BYTES
3 CELL * constant LINE-FRAME
15 constant VALUE-ORDER
1 constant MEMORY-ORDER
$F constant NIBBLE

: HEX$ ( -- str ) s" 0123456789abcdef" ;
: HEAD$ ( -- str )
   S\" habu-crash regs [sig rax rcx rdx rbx rsp rbp rsi rdi r8..r15 rip] code [rip-8 rip rip+8], hex one-per-line:\n" ;

\ Bind the printer at the label: called with rax = the value and rdx = the
\ order mask, it writes one line to fd 2 in one write. It clobbers rax rcx rdx
\ rsi rdi r8 r11.
: HEX-LINE, ( label -- ) {: at:label :}
   X64CODE:LBL X64CODE:LBL {: digits:label next:label :}
   X64ASM:RDI X64ASM:R64>N X64ASM:>R8 {: low:X64ASM:r8 :}
   at X64CODE:LBL,
   X64ASM:RSP LINE-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:R8 digits X64CODE:MOVABS,
   X64ASM:RCX ZERO-REG,
   next X64CODE:LBL,
      X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
      X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI NIBBLE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
      X64ASM:RDI X64ASM:R8 X64ASM:RDI 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      low X64ASM:RSP X64ASM:RSI 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
      X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
      X64ASM:RCX DIGITS X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:RDI STR:LF IMM,  low X64ASM:RSP DIGITS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RDI STDERR IMM,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX LINE-BYTES IMM,
   NR-WRITE SYS,
   X64ASM:RSP LINE-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64CODE:RET,
   digits X64CODE:LBL,
   HEX$ X64CODE:BYTES, ;

: PRINT, ( label n -- ) {: hex:label order:n :}
   X64ASM:RDX order IMM,  hex X64CODE:CALL, ;

\ One VM stack's guard case, with rdi = DATA and rsi = the fault address: rax
\ = its base from the DATA cell `base`, skipped when that is zero or not
\ PAGE-BYTES aligned; a fault in the page below the base branches to `hit`,
\ then what the quotation emits adds the capacity to rax and a fault in the
\ page there branches to `hit` too. It clobbers rax and rcx.
: GUARD-CASE, ( [ -- ] n label -- ) {: base:n hit:label :}
   X64CODE:LBL {: next:label :}
   X64ASM:RAX X64ASM:RDI base X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E next X64CODE:JCC,
   X64ASM:RAX STACK-ABI:PAGE-BYTES 1- X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B hit X64CODE:JCC,
   execute
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B hit X64CODE:JCC,
   next X64CODE:LBL, ;

\ Row 4, rdi = DATA and rsi = the fault address: a fault in the page below the
\ data stack's base, FLOORREC-CELL set, resumes the thread at that entry, the
\ kernel's underdepth throw.
: DATA-RECOVER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label resume:label done:label :}
   X64ASM:RCX X64ASM:RDI FLOORREC-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E next X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E next X64CODE:JCC,
   X64ASM:RAX STACK-ABI:PAGE-BYTES 1- X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   X64ASM:RCX STACK-ABI:PAGE-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B resume X64CODE:JCC,
   next X64CODE:LBL,  done X64CODE:JMP,
   resume X64CODE:LBL,
   X64ASM:RAX X64ASM:RDI FLOORREC-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64CODE:RET,
   done X64CODE:LBL, ;

\ Row 5: the three guarded VM stacks of the DATA at rdi, in crash.f's order.
: STACK-GUARDS, ( label label label -- ) {: dhit:label rhit:label lhit:label :}
   [: X64ASM:RAX X64ASM:RDI STACK-ABI:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM ;]
   STACK-ABI:BASE-CELL dhit GUARD-CASE,
   [: X64ASM:RAX STACK-ABI:RETURN-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32 ;]
   STACK-ABI:RETURN-BASE-CELL rhit GUARD-CASE,
   [: X64ASM:RAX STACK-ABI:LOOP-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32 ;]
   STACK-ABI:LOOP-BASE-CELL lhit GUARD-CASE, ;

\ The dictionary base a stack record reads: the code region's when Habu code
\ faulted, 0 otherwise. The interrupted rbp stays in the ucontext.
: DICT-REG ( -- X64ASM:r64 ) X64ASM:RBP ;

\ A VM stack's record's entry at the label: Habu code faulted, so DICT-REG is
\ the code region's.
: HIT, ( label -- )
   X64CODE:LBL,  DICT-REG REGION-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ A stack's overflow record: `in` the word at the saved rip, read from the
\ records at DICT-REG, which the saved r14 counts; `at` the rip when DICT-REG
\ is 0 or no word holds it.
: STACK-RECORD, ( str -- ) {: a:^ u:n :}
   s" E-STACK-BOUNDS" a u X64CRASH:FAULT-OPEN,
   X64ASM:RDI UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI DICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX UC-REG ENGINE-GPR:X64-NDICT X64ASM:>R64 UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64CRASH:FAULT-IN,
   FAULT-RECORD:CRS-HELP$ EXIT-STATUS:STACK-BOUNDS X64CRASH:FAULT-STOP, ;

\ Row 6, rdi = DATA and rsp at the handler's entry: the mark, the record, then
\ the saved rax = the code and the saved rip = LFAULTREC, and a `ret` into the
\ restorer resumes the thread there.
: FAULT-THROW, ( -- )
   X64ASM:RAX 1 IMM,  X64ASM:RAX X64ASM:RDI FAULT-MARK-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   s" E-MEMORY-FAULT" s" a read or write faulted" X64CRASH:FAULT-OPEN,
   X64ASM:RSI INFO-REG SI-ADDR X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   s" address" X64ASM:RSI X64CRASH:FAULT-ADDRESS,
   X64ASM:RDI UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI REGION-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX UC-REG ENGINE-GPR:X64-NDICT X64ASM:>R64 UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64CRASH:FAULT-IN,
   s" " X64CRASH:FAULT-KEEP,
   X64ASM:RAX EXIT-STATUS:MEMORY-FAULT IMM,
   X64ASM:RAX UC-REG X64ASM:RAX UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX LFAULTREC @ >LABEL X64CODE:MOVABS,
   X64ASM:RAX UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64CODE:RET, ;

\ Branch to the label when the saved rsp lies in the signal stack this thread
\ registered, as sigaltstack(NULL, old) answers it; a disabled one holds none.
\ The twin of crash.f C-CRASH-ONSTACK>R9, which says why not the ucontext's
\ flags. Only bit SS-DISABLE of the flags' cell is read, so the bytes past the
\ int are never looked at. rsp comes back where it was; it clobbers rax rcx rdx
\ rdi rsi r11.
: ON-SIGNAL-STACK, ( label -- ) {: hit:label :}
   X64CODE:LBL {: off:label :}
   X64ASM:RSP SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI ZERO-REG,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  NR-SIGALTSTACK SYS,
   X64ASM:RAX X64ASM:RSP SS-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX UC-REG X64ASM:RSP UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RSP SS-SP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC                                          \ rsp - 1 - ss_sp, unsigned
   X64ASM:RDX X64ASM:RSP SS-SIZE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSP SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX SS-DISABLE X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE off X64CODE:JCC,
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B hit X64CODE:JCC,
   off X64CODE:LBL, ;

\ Rows 0 to 6 of the handler's list. The crash handler's own code is [at, rest)
\ and the signal handlers STUBS, lays out run from at to the instruction at last;
\ only a fault that passes row 2 has the saved rbp read, as its DATA.
: GUARDS, ( label label label label label -- )
   {: dump:label raise:label at:label rest:label last:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
      {: other:label fault:label text:label region:label owned:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
      {: dhit:label rhit:label lhit:label mhit:label :}
   X64ASM:RAX UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX at X64CODE:MOVABS,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B other X64CODE:JCC,
   X64ASM:RCX rest X64CODE:MOVABS,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B raise X64CODE:JCC,
   other X64CODE:LBL,
   SIG-REG SIGSEGV X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E fault X64CODE:JCC,
   SIG-REG SIGBUS X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE dump X64CODE:JCC,
   fault X64CODE:LBL,
   \ The machine stack by the saved rsp alone: it needs no DATA.
   X64ASM:RAX INFO-REG SI-ADDR X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX UC-REG X64ASM:RSP UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
   X64ASM:RAX STACK-ABI:MACHINE-REACH X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32       \ fault - rsp + reach
   X64ASM:RAX STACK-ABI:MACHINE-REACH 2 * X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B mhit X64CODE:JCC,
   dump ON-SIGNAL-STACK,
   X64ASM:RAX UC-REG X64ASM:RBP UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E dump X64CODE:JCC,
   \ A fetch's rip is the jump's target and says nothing of the code that ran.
   X64ASM:RAX UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX INFO-REG SI-ADDR X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-E dump X64CODE:JCC,
   X64ASM:RCX at X64CODE:MOVABS,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B text X64CODE:JCC,
   X64ASM:RCX last X64CODE:MOVABS,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE dump X64CODE:JCC,
   text X64CODE:LBL,
   X64ASM:RAX TEXT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B region X64CODE:JCC,
   X64ASM:RDI LINUX-LAYOUT:DATA-VA VA>N IMM,
   X64ASM:RCX X64ASM:RDI TEXT-SIZE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX TEXT-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B owned X64CODE:JCC,
   region X64CODE:LBL,
   X64ASM:RCX REGION-REG DICT-SIZE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B dump X64CODE:JCC,
   X64ASM:RCX REGION-REG REGION X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE dump X64CODE:JCC,
   owned X64CODE:LBL,
   X64ASM:RDI UC-REG X64ASM:RBP UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RDI FAULT-MARK-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE dump X64CODE:JCC,
   X64ASM:RSI INFO-REG SI-ADDR X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   DATA-RECOVER,
   dhit rhit lhit STACK-GUARDS,
   FAULT-THROW,
   dhit HIT,  FAULT-RECORD:CRS-DATA$ STACK-RECORD,
   rhit HIT,  FAULT-RECORD:CRS-RET$ STACK-RECORD,
   lhit HIT,  FAULT-RECORD:CRS-LOOP$ STACK-RECORD,
   mhit X64CODE:LBL,  DICT-REG ZERO-REG,  FAULT-RECORD:CRS-MACHINE$ STACK-RECORD, ;

\ One code line: the cell at the saved rip plus `off`, in memory order, when
\ all eight of its bytes lie in the code region at REGION-REG; 0 otherwise.
: CODE-LINE, ( n label -- ) {: off:n hex:label :}
   X64CODE:LBL {: print:label :}
   X64ASM:RSI UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI X64ASM:RSI off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX REGION-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX ZERO-REG,
   X64ASM:RCX REGION CELL - X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-A print X64CODE:JCC,
   X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   print X64CODE:LBL,
   hex MEMORY-ORDER PRINT, ;

\ The dump: HEAD$ from the label, the signal, the registers, rip and the three
\ code lines, then, from the raise label, the signal re-raised, returning
\ through the restorer label for the default action's frame.
: DUMP, ( label label label label -- ) {: head:label hex:label rest:label raise:label :}
   X64ASM:RDI STDERR IMM,  X64ASM:RSI head X64CODE:MOVABS,  X64ASM:RDX HEAD$ nip IMM,  NR-WRITE SYS,
   X64ASM:RAX SIG-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  hex VALUE-ORDER PRINT,
   16 0 ?do
      X64ASM:RAX UC-REG i X64ASM:>R64 UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  hex VALUE-ORDER PRINT,
   loop
   X64ASM:RAX UC-REG UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  hex VALUE-ORDER PRINT,
   REGION-REG TEXT-REG REGION-FROM-TEXT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   CELL negate hex CODE-LINE,  0 hex CODE-LINE,  CELL hex CODE-LINE,
   raise X64CODE:LBL,
   X64ASM:RDI SIG-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R8 ZERO-REG,  0 X64ASM:R8 rest SIGACTION-RDI,
   NR-GETPID SYS,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   NR-GETTID SYS,  X64ASM:RSI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX SIG-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  NR-TGKILL SYS,
   \ rsp is back at entry, at the restorer address the kernel pushed; the
   \ restorer's own `ret` would pop it, so rt_sigreturn finds its frame at
   \ rsp + 8.
   X64ASM:RSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  NR-SIGRETURN SYS,
   \ Reached only if rt_sigreturn fails.
   X64ASM:RDI EXIT-STATUS:SYSTEM IMM,  NR-EXIT-GROUP SYS, ;

\ The crash handler's printer, which the breakpoint handler calls too.
variable HEX-AT

\ Bind the handler at the first label, then the printer and the header it
\ writes, where no control falls in. The second is the restorer RESTORER,
\ binds right after, which the default action the handler installs names; the
\ third is the last instruction of the signal handlers STUBS, lays out.
: CRASH-HANDLER, ( label label label -- ) {: at:label rest:label last:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
      {: dump:label raise:label hex:label head:label :}
   hex HEX-AT !
   at X64CODE:LBL,
   SIG-REG X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   INFO-REG X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   UC-REG X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   TEXT-REG TEXT-BASE,
   REGION-REG TEXT-REG REGION-FROM-TEXT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   dump raise at rest last GUARDS,
   dump X64CODE:LBL,
   head hex rest raise DUMP,
   hex HEX-LINE,
   head X64CODE:LBL,
   HEAD$ X64CODE:BYTES, ;

\ The breakpoint handler, the twin of src/habu/habu2.f EMIT-TRAPH, entered on
\ SIGTRAP with rdi = the signal, rsi = the siginfo and rdx = the ucontext; int3
\ leaves the saved rip one past itself. A trap outside the code region, where
\ the saved rbp is not DATA, or at an address no BPTAB slot holds, is no
\ breakpoint: it jumps to the crash handler with rdi, rsi and rdx as the kernel
\ gave them. A hit adds one to the slot's hits and, past its skip, writes
\ habu-bp: and the address, the data stack's top, habu-bp-lr: and the cell at
\ the saved rsp, the caller's return address at a word's entry, habu-bp-stack:
\ and the stack from its base, then habu-bp-watch: and each watch cell's
\ address and value when the watch table holds any. A persistent slot resumes
\ at its trampoline; a one-shot one gives the entry its first byte back
\ through the region's RW alias, empties the slot and resumes on the entry.
\ From the hit on it keeps the ucontext in rbx, the slot in r12, DATA in r13
\ and the entry in r14, which neither the printer nor `syscall` writes; its
\ `ret` enters the restorer.
32 constant BP-SLOT                     \ address, saved word, hits, ctrl
5 constant BP-SLOT-SHIFT
8 constant BP-WORD
16 constant BP-HITS
24 constant BP-CTRL                     \ skip << 1 | persistent

: BP-HEAD$ ( -- str ) S\" habu-bp:\n" ;
: BP-LR$ ( -- str ) S\" habu-bp-lr:\n" ;
: BP-STACK$ ( -- str ) S\" habu-bp-stack:\n" ;
: BP-WATCH$ ( -- str ) S\" habu-bp-watch:\n" ;

\ One line from the text at the label, n bytes, to fd 2.
: BP-LINE, ( label n -- ) {: at:label u:n :}
   X64ASM:RDI STDERR IMM,  X64ASM:RSI at X64CODE:MOVABS,  X64ASM:RDX u IMM,  NR-WRITE SYS, ;

: BP-HEX, ( -- ) HEX-AT @ >LABEL VALUE-ORDER PRINT, ;

\ r10 = the slot holding the address in rax, else on to `miss`.
: BP-SCAN, ( label -- ) {: miss:label :}
   X64CODE:LBL X64CODE:LBL {: scan:label hit:label :}
   X64ASM:R9 X64ASM:RDX RBASE-REG UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R9 BPTAB-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R11 X64ASM:R10 BREAKPOINT:SLOTS BP-SLOT * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   scan X64CODE:LBL,
      X64ASM:R10 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE miss X64CODE:JCC,
      X64ASM:RCX X64ASM:R10 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E hit X64CODE:JCC,
      X64ASM:R10 BP-SLOT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      scan X64CODE:JMP,
   hit X64CODE:LBL,
   X64ASM:RBX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R12 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R13 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R14 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ The address, the stack's top when the stack holds one, and the return address.
: BP-PRINT-HIT, ( label label -- ) {: head:label lr:label :}
   X64CODE:LBL {: empty:label :}
   head BP-HEAD$ nip BP-LINE,
   X64ASM:RAX X64ASM:R14 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  BP-HEX,
   X64ASM:R15 X64ASM:R13 STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RBX DSTACK-REG UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:R15 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE empty X64CODE:JCC,
   X64ASM:RAX X64ASM:RCX CELL negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  BP-HEX,
   empty X64CODE:LBL,
   lr BP-LR$ nip BP-LINE,
   X64ASM:RCX X64ASM:RBX X64ASM:RSP UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  BP-HEX, ;

\ The data stack from its base, r15, up to the saved stack pointer.
: BP-PRINT-STACK, ( label -- ) {: head:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   head BP-STACK$ nip BP-LINE,
   next X64CODE:LBL,
      X64ASM:RCX X64ASM:RBX DSTACK-REG UC-GREG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:R15 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
      X64ASM:RAX X64ASM:R15 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  BP-HEX,
      X64ASM:R15 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      next X64CODE:JMP,
   done X64CODE:LBL, ;

\ Each watch cell's address and value, r15 walking the table to r9.
: BP-PRINT-WATCH, ( label -- ) {: head:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   X64ASM:RCX X64ASM:R13 BPWN-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R15 X64ASM:R13 BPWBASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R15 X64ASM:R15 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R9 X64ASM:R15 X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   head BP-WATCH$ nip BP-LINE,
   next X64CODE:LBL,
      X64ASM:R15 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
      X64ASM:R10 X64ASM:R15 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  BP-HEX,
      X64ASM:RAX X64ASM:R10 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  BP-HEX,
      X64ASM:R15 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      next X64CODE:JMP,
   done X64CODE:LBL, ;

: BP-RESUME, ( -- )
   X64CODE:LBL X64CODE:LBL {: tramp:label done:label :}
   X64ASM:RAX X64ASM:R12 BP-CTRL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX 1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE tramp X64CODE:JCC,
   X64ASM:RAX X64ASM:R12 BP-WORD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:R14 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:R13 REGION-ALIAS:DELTA-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX X64ASM:R64>N X64ASM:>R8 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:R12 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R14 X64ASM:RBX UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   done X64CODE:JMP,
   tramp X64CODE:LBL,
   X64ASM:RAX X64ASM:R12 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RAX X64ASM:R13 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX BPTAB-OFF X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RAX BP-SLOT-SHIFT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RAX X64ASM:RAX BREAKPOINT:TRAMP X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX X64ASM:R13 BPTRAMP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX X64ASM:RBX UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   done X64CODE:LBL, ;

\ Bind the handler at the first label, then its lines, where no control falls
\ in; the second is the crash handler's.
: BP-HANDLER, ( label label -- ) {: at:label crash:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: resume:label head:label lr:label stack:label watch:label :}
   at X64CODE:LBL,
   X64ASM:RAX X64ASM:RDX UC-RIP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:R8 TEXT-BASE,  X64ASM:R8 X64ASM:R8 REGION-FROM-TEXT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX REGION X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-AE crash X64CODE:JCC,
   crash BP-SCAN,
   X64ASM:RAX X64ASM:R12 BP-HITS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RAX X64ASM:R12 BP-HITS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX X64ASM:R12 BP-CTRL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE resume X64CODE:JCC,      \ hits <= skip: silent
   head lr BP-PRINT-HIT,
   stack BP-PRINT-STACK,
   watch BP-PRINT-WATCH,
   resume X64CODE:LBL,
   BP-RESUME,
   X64CODE:RET,
   head X64CODE:LBL,  BP-HEAD$ X64CODE:BYTES,
   lr X64CODE:LBL,  BP-LR$ X64CODE:BYTES,
   stack X64CODE:LBL,  BP-STACK$ X64CODE:BYTES,
   watch X64CODE:LBL,  BP-WATCH$ X64CODE:BYTES, ;

\ Install the handler at a label for crash.f's signals, each returning through
\ the restorer at the other, on the alternate stack SIGNAL-STACK, registered.
\ Unchecked, as crash.f's install is: a rejected install leaves that signal's
\ default action.
: INSTALL-CRASH, ( label label -- ) {: at:label rest:label :}
   SA-SIGINFO SA-ONSTACK or {: flags:n :}
   SIGILL flags at rest SIGACTION,
   SIGTRAP flags at rest SIGACTION,
   SIGBUS flags at rest SIGACTION,
   SIGFPE flags at rest SIGACTION,
   SIGSEGV flags at rest SIGACTION, ;

\ SIGTRAP repointed from the crash handler at the breakpoint handler at the
\ first label; an engine's only (a program has no breakpoints).
: TRAP, ( label label -- ) {: trap:label rest:label :}
   SIGTRAP SA-SIGINFO SA-ONSTACK or trap rest SIGACTION, ;

\ The persistent breakpoints' trampolines, int3 at the live code's start, their
\ address in BPTRAMP-CELL; CP starts after them.
: TRAMPOLINES, ( -- )
   ENGINE-GPR:X64-CP X64ASM:>R64 {: cp:X64ASM:r64 :}
   cp BPTRAMP-CELL CELL!
   X64ASM:RDI cp X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDI RBASE-REG REGION-ALIAS:DELTA-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX $CCCCCCCCCCCCCCCC IMM,
   BREAKPOINT:BYTES 0 ?do X64ASM:RAX X64ASM:RDI i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR CELL +loop
   cp cp BREAKPOINT:BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA ;

: OPEN, ( -- )
   X64CODE:LBL TEXT0 !  TEXT0 @ >LABEL X64CODE:LBL,
   X64CODE:LBL STACK-BAD !  X64CODE:LBL REGION-BAD !  X64CODE:LBL DATA-BAD !  X64CODE:LBL OCC-BAD !
   X64CODE:LBL LFAULTREC !
   RBASE-REG TEXT-BASE,
   STACK-ABI:BOOT-BYTES DSTACK-REG STACK-BAD @ >LABEL MAP-STACK, ;

: SETTLE, ( label label label -- ) {: crash:label rest:label stub:label :}
   SIGNAL-STACK,                 \ once DATA-INIT, has read argc and argv off rsp
   OCC-INIT,
   stub PUBLISH-STUB,
   FRAME-STACKS,
   crash rest INSTALL-CRASH, ;

\ The signal handlers, the crash handler, its restorer and the signal stub, lie
\ together after the routines Habu code calls, so GUARDS, tells them by one
\ band.
: STUBS, ( label label label -- ) {: crash:label rest:label stub:label :}
   X64CODE:LBL {: last:label :}
   X64CRASH:EMIT-FAULT-RECORD
   STACK-BAD @ >LABEL S\" hb: cannot map guarded VM stack\n" EXIT-STATUS:SYSTEM FAIL,
   OCC-BAD @ >LABEL S\" hb: cannot map definition occurrences\n" EXIT-STATUS:SYSTEM FAIL,
   DATA-BAD @ >LABEL S\" hb: cannot map fixed data region\n" EXIT-STATUS:SYSTEM FAIL,
   REGION-BAD @ >LABEL
   S\" hb: cannot map the code region; raise the open-file limit (ulimit -n) or the address-space limit (ulimit -v)\n"
   EXIT-STATUS:SYSTEM FAIL,
   crash rest last CRASH-HANDLER,
   rest RESTORER,
   stub last SIGNAL-STUB, ;

: CLOSE, ( label label label label -- ) {: booted:label crash:label rest:label stub:label :}
   crash rest stub STUBS,
   booted X64CODE:LBL, ;

public

\ rax = this thread's pointer: mov rax, qword ptr fs:0, the segment form the
\ assembler does not expose. glibc's fs:0 is the value pthread_self answers.
: CB-SELF, ( -- )
   $64 X64CODE:ASM-SINK VEC:PUSH c!  $48 X64CODE:ASM-SINK VEC:PUSH c!
   $8B X64CODE:ASM-SINK VEC:PUSH c!  $04 X64CODE:ASM-SINK VEC:PUSH c!
   $25 X64CODE:ASM-SINK VEC:PUSH c!
   4 0 do  0 X64CODE:ASM-SINK VEC:PUSH c!  loop ;

\ The label of the routine that runs the exit actions, called with the exit
\ code in rdi; X64KERNEL:HELPERS, makes it and emits the routine.
variable LEXITHOOK

\ kernel is true when the caller emits X64KERNEL:KERNEL, after it, which
\ binds the fault entry.
: START, ( label label bool -- ) {: floor:label code-end:label kernel:bool :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: booted:label crash:label rest:label stub:label :}
   OPEN,
   CODE-REGION,
   DATA-REGION,
   true DATA-INIT,  ALIAS-DELTA,
   floor PUBLISH-FLOOR,
   X64ASM:RAX code-end X64CODE:MOVABS,  X64ASM:RAX CODE-END-CELL CELL!
   crash rest stub SETTLE,
   booted X64CODE:JMP,
   kernel 0= if
      \ No kernel follows to throw from: FAULT-THROW, has written the record, so
      \ exit as an uncaught memory fault.
      LFAULTREC @ >LABEL X64CODE:LBL,  X64ASM:RDI EXIT-STATUS:MEMORY-FAULT IMM,  NR-EXIT-GROUP SYS,
   then
   booted crash rest stub CLOSE, ;

\ A full engine's MAIN runs a saved application before routing stdin or the
\ terminal. An image without MAIN can run APP-ENTRY directly. A word that
\ returns exits successfully, while an image with neither entry is broken.
: ENTRY, ( -- )
   X64CODE:LBL X64CODE:LBL {: go:label none:label :}
   X64ASM:RAX RBASE-REG ENGINE-MAIN:XT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE go X64CODE:JCC,
   X64ASM:RAX RBASE-REG APP-ENTRY:XT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   go X64CODE:LBL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RDI ZERO-REG,  LEXITHOOK @ >LABEL X64CODE:CALL,
   NR-EXIT-GROUP SYS,
   none S\" hb: no entry\n" EXIT-STATUS:DISPATCH-UNSET FAIL, ;

;package
