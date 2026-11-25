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
using X64ASM
using X64CODE
using LINUX-LAYOUT   \ the guard

0 constant PROT-NONE
3 constant PROT-RW                  \ PROT_READ|PROT_WRITE
5 constant PROT-RX                  \ PROT_READ|PROT_EXEC
2 constant STDERR

\ The four failures the boot names, one label each per image.
variable STACK-BAD
variable REGION-BAD
variable DATA-BAD
variable OCC-BAD

: RBASE-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSTACK-REG ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;

\ Boot keeps the region's alias - base in the loop index until ALIAS-DELTA,
\ stores it; no code between writes that register.
: DELTA-REG ( -- r64 ) ENGINE-GPR:X64-INDEX >R64 ;

: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

: CELL! ( r64 n -- ) {: r:r64 off:n :}
   r RBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ mmap(rdi, len, prot, flags, -1, 0) with the address already in rdi, which the
\ syscall keeps. SYS, leaves CF set when the kernel refused.
: MMAP, ( n n n -- ) {: len:n prot:n flags:n :}
   RSI len IMM,  RDX prot IMM,  R10 flags IMM,  R8 -1 IMM,  R9 ZERO-REG,
   NR-MMAP SYS, ;

\ Map `len` read/write bytes exactly at rdi with `flags`. A refusal returns
\ -errno (NOREPLACE's EEXIST among them) and a moved mapping another address,
\ so one comparison catches both.
: MAP-FIXED, ( n n label -- ) {: len:n flags:n bad:label :}
   len PROT-RW flags MMAP,
   RAX RDI ASM-SINK ENC-CMP-RR
   C-NE bad JCC, ;

public

\ A fresh executable, close-on-exec memfd of REGION bytes, returned in rax.
\ Only EINVAL retries without MFD_EXEC, for Linux before 6.3. On failure no
\ descriptor remains open and rsp is restored. Clobbers the syscall scratch
\ registers and r8; the twin of LIBC-OS:REGION-MEMFD.
: REGION-MEMFD, ( label -- ) {: bad:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: made:label sized:label failed:label :}
   RSP 16 >IMM8 ASM-SINK ENC-SUB-RI8
   RAX $6F696765722D6268 IMM,  RAX RSP MEM-AT ASM-SINK ENC-MOV-MR
   RAX $6E IMM,  RAX RSP 8 MEM-OFF ASM-SINK ENC-MOV-MR
   RDI RSP ASM-SINK ENC-MOV-RR  RSI $11 IMM,
   NR-MEMFD-CREATE SYS,  C-AE made JCC,
   RAX -22 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE failed JCC,
   RDI RSP ASM-SINK ENC-MOV-RR  RSI 1 IMM,
   NR-MEMFD-CREATE SYS,  C-B failed JCC,
   made X64CODE:LBL,
   R8 RAX ASM-SINK ENC-MOV-RR
   RDI R8 ASM-SINK ENC-MOV-RR  RSI REGION IMM,
   NR-FTRUNCATE SYS,  C-AE sized JCC,
   RDI R8 ASM-SINK ENC-MOV-RR  NR-CLOSE SYS,
   failed X64CODE:LBL,
   RSP 16 >IMM8 ASM-SINK ENC-ADD-RI8  bad JMP,
   sized X64CODE:LBL,
   RAX R8 ASM-SINK ENC-MOV-RR
   RSP 16 >IMM8 ASM-SINK ENC-ADD-RI8 ;

private

\ The twin of STACK-GUARD:EMIT-MAP (src/habu/rt.f): map one guarded VM stack of
\ `cap` bytes and leave its base in `dst`. The first mapping reserves cap plus
\ three guard pages PROT_NONE; the second reopens cap bytes read/write at the
\ first PAGE-BYTES boundary at least one page in, so at least one inaccessible
\ page lies below the base and one above base + cap, and a push past the
\ capacity faults.
: MAP-STACK, ( n r64 label -- ) {: cap:n dst:r64 bad:label :}
   RDI ZERO-REG,
   cap STACK-ABI:PAGE-BYTES 3 * +  PROT-NONE  MAP-ANON-PRIVATE MMAP,
   C-B bad JCC,
   RDI RAX STACK-ABI:PAGE-BYTES 2 * 1- MEM-OFF ASM-SINK ENC-LEA
   RDI STACK-ABI:PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   cap PROT-RW MAP-ANON-PRIVATE-FIXED MMAP,
   RAX RDI ASM-SINK ENC-CMP-RR
   C-NE bad JCC,
   dst RDI ASM-SINK ENC-MOV-RR ;

\ r = the runtime address of the stream's byte 0, the text content base: rip
\ after the lea is that base plus the lea's end offset.
: TEXT-BASE, ( r64 -- ) {: r:r64 :}
   r 0 MEM-RIP ASM-SINK ENC-LEA
   r X64CODE:ASM-LEN >IMM32 ASM-SINK ENC-SUB-RI32 ;

\ Map the descriptor in r8, REGION bytes shared, at rdi.
: VIEW, ( n n -- ) {: prot:n flags:n :}
   RSI REGION IMM,  RDX prot IMM,  R10 flags IMM,  R9 ZERO-REG,
   NR-MMAP SYS,  C-B REGION-BAD @ >LABEL JCC, ;

\ r13 = the code region, REGION-OFF past the image base, and r15 past its
\ records, of which r14 counts none; r12 = its RW alias - r13.
: CODE-REGION, ( -- )
   DBASE-REG RBASE-REG REGION-OFF LINUX-LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   REGION-BAD @ >LABEL REGION-MEMFD,  R8 RAX ASM-SINK ENC-MOV-RR
   \ The RX view over the image's own load, the RW alias where the kernel puts it.
   RDI DBASE-REG ASM-SINK ENC-MOV-RR  PROT-RX MAP-SHARED-FIXED VIEW,
   RDI ZERO-REG,  PROT-RW MAP-SHARED VIEW,
   DELTA-REG RAX ASM-SINK ENC-MOV-RR  DELTA-REG DBASE-REG ASM-SINK ENC-SUB-RR
   RDI R8 ASM-SINK ENC-MOV-RR  NR-CLOSE SYS,  C-B REGION-BAD @ >LABEL JCC,
   ENGINE-GPR:X64-CP >R64 DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   ENGINE-GPR:X64-NDICT >R64 ZERO-REG, ;

\ rax = rdi = DATA, the DATA-SIZE reservation at DATA-VA, as
\ EM-MMAP-DATA-REGION maps it.
: DATA-REGION, ( -- )
   RDI LINUX-LAYOUT:DATA-VA VA>N IMM,
   LINUX-LAYOUT:DATA-SIZE MAP-DATA-RESERVE DATA-BAD @ >LABEL MAP-FIXED, ;

\ The twin of EM-DATA-INIT: publish the text base, then rbp becomes DATA; then
\ the global and package wid cells, the data stack's extent, the argument
\ vector ([rsp] = argc, argv at rsp + 8, envp past argv's null) and the heap
\ floor with the DP that starts at it.
: DATA-INIT, ( bool -- ) {: heap:bool :}
   RBASE-REG RAX RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RBASE-REG RAX ASM-SINK ENC-MOV-RR
   \ The text content size the source text-extent words read (layout.f
   \ TEXT-SIZE-CELL), from the header as kernel-x64.f DLSYM, reads it. Every
   \ entry passes here.
   RAX RBASE-REG RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX LINUX-LAYOUT:CODE-OFF >IMM32 ASM-SINK ENC-SUB-RI32
   RAX RAX LINUX-LAYOUT:IMAGE-TEXT-SIZE-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX LINUX-LAYOUT:IMAGE-TEXT-CONTENT-ADJ >IMM32 ASM-SINK ENC-SUB-RI32
   RAX TEXT-SIZE-CELL CELL!
   \ A saved image carries DATA, but no process owns its old mutex or index.
   RAX ZERO-REG,
   RAX ADDRESS-CELLS:LOCK-CELL CELL!
   RAX ADDRESS-CELLS:INDEX-CELL CELL!
   \ The lookups' wids, 0 and WL:NAMESPACE at every boot.
   RAX BOOT:GLOBAL-WL-CELL CELL!
   RAX WL:NAMESPACE IMM,  RAX BOOT:NAMESPACE-WL-CELL CELL!
   DSTACK-REG STACK-ABI:BASE-CELL CELL!
   RAX STACK-ABI:BOOT-BYTES IMM,  RAX STACK-ABI:CAP-CELL CELL!
   RAX RSP MEM-AT ASM-SINK ENC-MOV-RM  RAX ARGC-CELL CELL!
   RCX RSP CELL MEM-OFF ASM-SINK ENC-LEA  RCX ARGV-CELL CELL!
   RCX RCX RAX CELL CELL MEM-IDX ASM-SINK ENC-LEA  RCX ENVP-CELL CELL!
   heap if
      RAX DATA-START IMM,  RAX BOOT:HEAP-START-CELL CELL!
      RAX RBASE-REG DATA-START MEM-OFF ASM-SINK ENC-LEA  RAX DP-CELL CELL!
   then
   \ Every process starts with no definition open, whatever DATA carried.
   RAX ZERO-REG,
   RAX NCOMP-DISPATCH:COMP-LATCH-CELL CELL! ;

: ALIAS-DELTA, ( -- ) DELTA-REG REGION-ALIAS:DELTA-CELL CELL! ;

\ This process's definition occurrence allocation (layout.f DEF-OCC): a fresh
\ mapping whatever DATA the image carried, whose pointer DATA-INIT's caller
\ could not have restored. The records already counted, [0,NDICT), take the
\ first numbers in order, as ARM64's EM-DEF-OCC LINIT issues them: slot i+1
\ holds i+1, and the first cell, the last number issued, holds NDICT. Without
\ them a linked record has no selectable occurrence.
: OCC-INIT, ( -- )
   X64CODE:LBL X64CODE:LBL {: issue:label issued:label :}
   RDI ZERO-REG,
   DEF-OCC:STATE-BYTES PROT-RW MAP-ANON-PRIVATE MMAP,
   C-B OCC-BAD @ >LABEL JCC,
   RAX DEF-OCC:PTR-CELL CELL!
   RCX ZERO-REG,
   issue X64CODE:LBL,
      RCX ENGINE-GPR:X64-NDICT >R64 ASM-SINK ENC-CMP-RR  C-AE issued JCC,
      RCX ASM-SINK ENC-INC
      RCX RAX RCX CELL 0 MEM-IDX ASM-SINK ENC-MOV-MR
      issue JMP,
   issued X64CODE:LBL,
   RCX RAX MEM-AT ASM-SINK ENC-MOV-MR ;

\ The twin of EM-FRAME-STACKS: the return and DO/LOOP frame stacks, published
\ in their DATA cells.
: FRAME-STACKS, ( -- )
   STACK-ABI:RETURN-BYTES RAX STACK-BAD @ >LABEL MAP-STACK,
   RAX STACK-ABI:RETURN-BASE-CELL CELL!
   STACK-ABI:LOOP-BYTES RAX STACK-BAD @ >LABEL MAP-STACK,
   RAX STACK-ABI:LOOP-BASE-CELL CELL!
   RAX LOOPSP-CELL CELL! ;

\ Bind a failure at the label: its line, newline included, to fd 2 in one
\ write, then exit with the status. The line follows the exit syscall inside
\ the loaded text, so it is readable before any region exists.
: FAIL, ( label str n -- ) {: at:label a:^ u:n rc:n :}
   X64CODE:LBL {: msg:label :}
   at X64CODE:LBL,
   RDI STDERR IMM,  RSI msg MOVABS,  RDX u IMM,  NR-WRITE SYS,
   RDI rc IMM,  NR-EXIT-GROUP SYS,
   msg X64CODE:LBL,
   a u BYTES, ;

\ The width of the stub's one write: under PIPE_BUF, so a pipe takes it whole.
4 constant SIGNO-BYTES

\ The fd word's absolute address. DATA is mapped at DATA-VA and DATA-REGION,
\ refuses a boot the kernel answered elsewhere, so this one address names the
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
   RDI ASM-SINK ENC-PUSH
   RAX FD-WORD-VA IMM,
   RDI RAX MEM-AT ASM-SINK ENC-MOV-RM
   RDI RDI ASM-SINK ENC-TEST-RR
   C-E done JCC8,
   RSI RSP ASM-SINK ENC-MOV-RR
   RDX R64>N >R32 SIGNO-BYTES >IMM32 ASM-SINK ENC-MOV32-RI32
   RAX R64>N >R32 NR-WRITE >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL
   done X64CODE:LBL,
   RDI ASM-SINK ENC-POP
   last X64CODE:LBL,  RET, ;

\ Publish the stub bound at the label and the address of the word it reads,
\ so a program installs and arms the stub without spelling either (layout.f
\ package SIGNAL-ABI). The word itself needs no clear: DATA is a fresh mapping
\ here.
: PUBLISH-STUB, ( label -- ) {: stub:label :}
   RAX stub MOVABS,  RAX SIGNAL-ABI:STUB-CELL CELL!
   RAX FD-WORD-VA IMM,  RAX SIGNAL-ABI:FD-PTR-CELL CELL! ;

: PUBLISH-FLOOR, ( label -- )
   RAX swap MOVABS,  RAX FLOORREC-CELL CELL! ;

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
: UC-GREG ( r64 -- n ) R64>N GREG-SLOTS + c@ CELL * UC-GREGS + ;

\ Bind the restorer at the label: rt_sigreturn, which resumes the interrupted
\ context from the ucontext and never returns. A handler's `ret` enters it, so
\ it goes where no other control falls in.
: RESTORER, ( label -- )
   X64CODE:LBL,
   0 >R32 NR-SIGRETURN >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL ;

\ Install the handler whose address a register holds for signal n with the
\ given flags, returning through the restorer bound at the label; SA-RESTORER
\ joins the flags. The action is built on the machine stack and rsp comes back
\ where it was. The handler is stored before anything is clobbered, so any
\ register but rsp, and rdi under SIGACTION-AT,, may hold it. It clobbers rax
\ rcx rdx rsi rdi r10 r11 and leaves CF set when the kernel refused, as SYS,
\ does: the lea that gives the frame back keeps the flags. SIGACTION-RDI,
\ installs it for the signal rdi holds.
: SIGACTION-RDI, ( n r64 label -- ) {: flags:n handler:r64 rest:label :}
   RSP SA-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   handler RSP SA-HANDLER MEM-OFF ASM-SINK ENC-MOV-MR
   RAX flags SA-RESTORER or IMM,  RAX RSP SA-FLAGS MEM-OFF ASM-SINK ENC-MOV-MR
   RAX rest MOVABS,  RAX RSP SA-RESTORER-AT MEM-OFF ASM-SINK ENC-MOV-MR
   RAX ZERO-REG,  RAX RSP SA-MASK MEM-OFF ASM-SINK ENC-MOV-MR
   RSI RSP ASM-SINK ENC-MOV-RR  RDX ZERO-REG,
   R10 SIGSET-BYTES IMM,
   NR-SIGACTION SYS,
   RSP RSP SA-BYTES MEM-OFF ASM-SINK ENC-LEA ;

: SIGACTION-AT, ( n n r64 label -- ) {: sig:n flags:n handler:r64 rest:label :}
   RDI sig IMM,  flags handler rest SIGACTION-RDI, ;

\ The same for the handler at a label, through rax.
: SIGACTION, ( n n label label -- ) {: sig:n flags:n handler:label rest:label :}
   RAX handler MOVABS,
   sig flags RAX rest SIGACTION-AT, ;

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
   RSP STACK-ABI:SIGNAL-BYTES >IMM32 ASM-SINK ENC-SUB-RI32
   RAX RSP ASM-SINK ENC-MOV-RR                                   \ the band's base
   RSP SS-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP SS-SP MEM-OFF ASM-SINK ENC-MOV-MR
   RAX ZERO-REG,  RAX RSP SS-FLAGS MEM-OFF ASM-SINK ENC-MOV-MR
   RAX STACK-ABI:SIGNAL-BYTES IMM,  RAX RSP SS-SIZE MEM-OFF ASM-SINK ENC-MOV-MR
   RDI RSP ASM-SINK ENC-MOV-RR  RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   RSP RSP SS-BYTES MEM-OFF ASM-SINK ENC-LEA                     \ the frame back, CF kept
   C-AE ok JCC,
   bad S\" hb: cannot install the signal stack\n" EXIT-STATUS:SYSTEM FAIL,
   ok X64CODE:LBL, ;

\ The way out of the code that registered: SS_DISABLE, then the caller gives
\ the band back. Its result goes unchecked because it cannot fail off the
\ alternate stack: the one refusal of a well-formed disable is a thread running
\ on its alternate stack, and rsp is below the band. It clobbers rax rcx rdi
\ rsi r11.
: SIGNAL-STACK-OFF, ( -- )
   RSP SS-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RAX SS-DISABLE IMM,  RAX RSP SS-FLAGS MEM-OFF ASM-SINK ENC-MOV-MR
   RDI RSP ASM-SINK ENC-MOV-RR  RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   RSP SS-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

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
: SIG-REG ( -- r64 ) R12 ;
: INFO-REG ( -- r64 ) R13 ;
: UC-REG ( -- r64 ) RBX ;
: TEXT-REG ( -- r64 ) R14 ;
: REGION-REG ( -- r64 ) R15 ;

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
   RDI R64>N >R8 {: low:r8 :}
   at X64CODE:LBL,
   RSP LINE-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   R8 digits MOVABS,
   RCX ZERO-REG,
   next X64CODE:LBL,
      RSI RCX ASM-SINK ENC-MOV-RR  RSI RDX ASM-SINK ENC-XOR-RR
      RDI RAX ASM-SINK ENC-MOV-RR  RDI NIBBLE >IMM8 ASM-SINK ENC-AND-RI8
      RDI R8 RDI 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
      low RSP RSI 1 0 MEM-IDX ASM-SINK ENC-MOV8-MR
      RAX 4 >IMM8 ASM-SINK ENC-SHR-RI8
      RCX ASM-SINK ENC-INC
      RCX DIGITS >IMM8 ASM-SINK ENC-CMP-RI8  C-NE next JCC,
   RDI STR:LF IMM,  low RSP DIGITS MEM-OFF ASM-SINK ENC-MOV8-MR
   RDI STDERR IMM,  RSI RSP ASM-SINK ENC-MOV-RR  RDX LINE-BYTES IMM,
   NR-WRITE SYS,
   RSP LINE-FRAME >IMM8 ASM-SINK ENC-ADD-RI8
   RET,
   digits X64CODE:LBL,
   HEX$ BYTES, ;

: PRINT, ( label n -- ) {: hex:label order:n :}
   RDX order IMM,  hex CALL, ;

\ One VM stack's guard case, with rdi = DATA and rsi = the fault address: rax
\ = its base from the DATA cell `base`, skipped when that is zero or not
\ PAGE-BYTES aligned; a fault in the page below the base branches to `hit`,
\ then what the quotation emits adds the capacity to rax and a fault in the
\ page there branches to `hit` too. It clobbers rax and rcx.
: GUARD-CASE, ( [ -- ] n label -- ) {: base:n hit:label :}
   X64CODE:LBL {: next:label :}
   RAX RDI base MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E next JCC,
   RAX STACK-ABI:PAGE-BYTES 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE next JCC,
   RCX RSI ASM-SINK ENC-MOV-RR  RCX RAX ASM-SINK ENC-SUB-RR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B hit JCC,
   execute
   RCX RSI ASM-SINK ENC-MOV-RR  RCX RAX ASM-SINK ENC-SUB-RR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B hit JCC,
   next X64CODE:LBL, ;

\ Row 4, rdi = DATA and rsi = the fault address: a fault in the page below the
\ data stack's base, FLOORREC-CELL set, resumes the thread at that entry, the
\ kernel's underdepth throw.
: DATA-RECOVER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label resume:label done:label :}
   RCX RDI FLOORREC-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RCX ASM-SINK ENC-TEST-RR  C-E next JCC,
   RAX RDI STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E next JCC,
   RAX STACK-ABI:PAGE-BYTES 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE next JCC,
   RCX RSI ASM-SINK ENC-MOV-RR  RCX RAX ASM-SINK ENC-SUB-RR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B resume JCC,
   next X64CODE:LBL,  done JMP,
   resume X64CODE:LBL,
   RAX RDI FLOORREC-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-MR
   RET,
   done X64CODE:LBL, ;

\ Row 5: the three guarded VM stacks of the DATA at rdi, in crash.f's order.
: STACK-GUARDS, ( label label label -- ) {: dhit:label rhit:label lhit:label :}
   [: RAX RDI STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM ;]
   STACK-ABI:BASE-CELL dhit GUARD-CASE,
   [: RAX STACK-ABI:RETURN-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:RETURN-BASE-CELL rhit GUARD-CASE,
   [: RAX STACK-ABI:LOOP-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:LOOP-BASE-CELL lhit GUARD-CASE, ;

\ The dictionary base a stack record reads: the code region's when Habu code
\ faulted, 0 otherwise. The interrupted rbp stays in the ucontext.
: DICT-REG ( -- r64 ) RBP ;

\ A VM stack's record's entry at the label: Habu code faulted, so DICT-REG is
\ the code region's.
: HIT, ( label -- )
   X64CODE:LBL,  DICT-REG REGION-REG ASM-SINK ENC-MOV-RR ;

\ A stack's overflow record: `in` the word at the saved rip, read from the
\ records at DICT-REG, which the saved r14 counts; `at` the rip when DICT-REG
\ is 0 or no word holds it.
: STACK-RECORD, ( str -- ) {: a:^ u:n :}
   s" E-STACK-BOUNDS" a u X64CRASH:FAULT-OPEN,
   RDI UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RSI DICT-REG ASM-SINK ENC-MOV-RR
   RDX UC-REG ENGINE-GPR:X64-NDICT >R64 UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   X64CRASH:FAULT-IN,
   FAULT-RECORD:CRS-HELP$ EXIT-STATUS:STACK-BOUNDS X64CRASH:FAULT-STOP, ;

\ Row 6, rdi = DATA and rsp at the handler's entry: the mark, the record, then
\ the saved rax = the code and the saved rip = LFAULTREC, and a `ret` into the
\ restorer resumes the thread there.
: FAULT-THROW, ( -- )
   RAX 1 IMM,  RAX RDI FAULT-MARK-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   s" E-MEMORY-FAULT" s" a read or write faulted" X64CRASH:FAULT-OPEN,
   RSI INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-MOV-RM
   s" address" RSI X64CRASH:FAULT-ADDRESS,
   RDI UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RSI REGION-REG ASM-SINK ENC-MOV-RR
   RDX UC-REG ENGINE-GPR:X64-NDICT >R64 UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   X64CRASH:FAULT-IN,
   s" " X64CRASH:FAULT-KEEP,
   RAX EXIT-STATUS:MEMORY-FAULT IMM,
   RAX UC-REG RAX UC-GREG MEM-OFF ASM-SINK ENC-MOV-MR
   RAX LFAULTREC @ >LABEL MOVABS,
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-MR
   RET, ;

\ Branch to the label when the saved rsp lies in the signal stack this thread
\ registered, as sigaltstack(NULL, old) answers it; a disabled one holds none.
\ The twin of crash.f C-CRASH-ONSTACK>R9, which says why not the ucontext's
\ flags. Only bit SS-DISABLE of the flags' cell is read, so the bytes past the
\ int are never looked at. rsp comes back where it was; it clobbers rax rcx rdx
\ rdi rsi r11.
: ON-SIGNAL-STACK, ( label -- ) {: hit:label :}
   X64CODE:LBL {: off:label :}
   RSP SS-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RDI ZERO-REG,  RSI RSP ASM-SINK ENC-MOV-RR  NR-SIGALTSTACK SYS,
   RAX RSP SS-FLAGS MEM-OFF ASM-SINK ENC-MOV-RM
   RCX UC-REG RSP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RSP SS-SP MEM-OFF ASM-SINK ENC-SUB-RM
   RCX ASM-SINK ENC-DEC                                          \ rsp - 1 - ss_sp, unsigned
   RDX RSP SS-SIZE MEM-OFF ASM-SINK ENC-MOV-RM
   RSP SS-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   RAX SS-DISABLE >IMM32 ASM-SINK ENC-TEST-RI32  C-NE off JCC,
   RCX RDX ASM-SINK ENC-CMP-RR  C-B hit JCC,
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
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RCX at MOVABS,  RAX RCX ASM-SINK ENC-CMP-RR  C-B other JCC,
   RCX rest MOVABS,  RAX RCX ASM-SINK ENC-CMP-RR  C-B raise JCC,
   other X64CODE:LBL,
   SIG-REG SIGSEGV >IMM8 ASM-SINK ENC-CMP-RI8  C-E fault JCC,
   SIG-REG SIGBUS >IMM8 ASM-SINK ENC-CMP-RI8  C-NE dump JCC,
   fault X64CODE:LBL,
   \ The machine stack by the saved rsp alone: it needs no DATA.
   RAX INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-MOV-RM
   RAX UC-REG RSP UC-GREG MEM-OFF ASM-SINK ENC-SUB-RM
   RAX STACK-ABI:MACHINE-REACH >IMM32 ASM-SINK ENC-ADD-RI32       \ fault - rsp + reach
   RAX STACK-ABI:MACHINE-REACH 2 * >IMM32 ASM-SINK ENC-CMP-RI32  C-B mhit JCC,
   dump ON-SIGNAL-STACK,
   RAX UC-REG RBP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E dump JCC,
   \ A fetch's rip is the jump's target and says nothing of the code that ran.
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RAX INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-CMP-RM  C-E dump JCC,
   RCX at MOVABS,  RAX RCX ASM-SINK ENC-CMP-RR  C-B text JCC,
   RCX last MOVABS,  RAX RCX ASM-SINK ENC-CMP-RR  C-BE dump JCC,
   text X64CODE:LBL,
   RAX TEXT-REG ASM-SINK ENC-CMP-RR  C-B region JCC,
   RDI LINUX-LAYOUT:DATA-VA VA>N IMM,
   RCX RDI TEXT-SIZE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX TEXT-REG ASM-SINK ENC-ADD-RR
   RAX RCX ASM-SINK ENC-CMP-RR  C-B owned JCC,
   region X64CODE:LBL,
   RCX REGION-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   RAX RCX ASM-SINK ENC-CMP-RR  C-B dump JCC,
   RCX REGION-REG REGION MEM-OFF ASM-SINK ENC-LEA
   RAX RCX ASM-SINK ENC-CMP-RR  C-AE dump JCC,
   owned X64CODE:LBL,
   RDI UC-REG RBP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RDI FAULT-MARK-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE dump JCC,
   RSI INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-MOV-RM
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
   RSI UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RSI RSI off MEM-OFF ASM-SINK ENC-LEA
   RCX RSI ASM-SINK ENC-MOV-RR  RCX REGION-REG ASM-SINK ENC-SUB-RR
   RAX ZERO-REG,
   RCX REGION CELL - >IMM32 ASM-SINK ENC-CMP-RI32  C-A print JCC,
   RAX RSI MEM-AT ASM-SINK ENC-MOV-RM
   print X64CODE:LBL,
   hex MEMORY-ORDER PRINT, ;

\ The dump: HEAD$ from the label, the signal, the registers, rip and the three
\ code lines, then, from the raise label, the signal re-raised, returning
\ through the restorer label for the default action's frame.
: DUMP, ( label label label label -- ) {: head:label hex:label rest:label raise:label :}
   RDI STDERR IMM,  RSI head MOVABS,  RDX HEAD$ nip IMM,  NR-WRITE SYS,
   RAX SIG-REG ASM-SINK ENC-MOV-RR  hex VALUE-ORDER PRINT,
   16 0 ?do
      RAX UC-REG i >R64 UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM  hex VALUE-ORDER PRINT,
   loop
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM  hex VALUE-ORDER PRINT,
   REGION-REG TEXT-REG REGION-FROM-TEXT MEM-OFF ASM-SINK ENC-LEA
   CELL negate hex CODE-LINE,  0 hex CODE-LINE,  CELL hex CODE-LINE,
   raise X64CODE:LBL,
   RDI SIG-REG ASM-SINK ENC-MOV-RR  R8 ZERO-REG,  0 R8 rest SIGACTION-RDI,
   NR-GETPID SYS,  RDI RAX ASM-SINK ENC-MOV-RR
   NR-GETTID SYS,  RSI RAX ASM-SINK ENC-MOV-RR
   RDX SIG-REG ASM-SINK ENC-MOV-RR  NR-TGKILL SYS,
   \ rsp is back at entry, at the restorer address the kernel pushed; the
   \ restorer's own `ret` would pop it, so rt_sigreturn finds its frame at
   \ rsp + 8.
   RSP CELL >IMM8 ASM-SINK ENC-ADD-RI8  NR-SIGRETURN SYS,
   \ Reached only if rt_sigreturn fails.
   RDI EXIT-STATUS:SYSTEM IMM,  NR-EXIT-GROUP SYS, ;

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
   SIG-REG RDI ASM-SINK ENC-MOV-RR
   INFO-REG RSI ASM-SINK ENC-MOV-RR
   UC-REG RDX ASM-SINK ENC-MOV-RR
   TEXT-REG TEXT-BASE,
   REGION-REG TEXT-REG REGION-FROM-TEXT MEM-OFF ASM-SINK ENC-LEA
   dump raise at rest last GUARDS,
   dump X64CODE:LBL,
   head hex rest raise DUMP,
   hex HEX-LINE,
   head X64CODE:LBL,
   HEAD$ BYTES, ;

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
   RDI STDERR IMM,  RSI at MOVABS,  RDX u IMM,  NR-WRITE SYS, ;

: BP-HEX, ( -- ) HEX-AT @ >LABEL VALUE-ORDER PRINT, ;

\ r10 = the slot holding the address in rax, else on to `miss`.
: BP-SCAN, ( label -- ) {: miss:label :}
   X64CODE:LBL X64CODE:LBL {: scan:label hit:label :}
   R9 RDX RBASE-REG UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   R10 R9 BPTAB-OFF MEM-OFF ASM-SINK ENC-LEA
   R11 R10 BREAKPOINT:SLOTS BP-SLOT * MEM-OFF ASM-SINK ENC-LEA
   scan X64CODE:LBL,
      R10 R11 ASM-SINK ENC-CMP-RR  C-AE miss JCC,
      RCX R10 MEM-AT ASM-SINK ENC-MOV-RM
      RCX RAX ASM-SINK ENC-CMP-RR  C-E hit JCC,
      R10 BP-SLOT >IMM8 ASM-SINK ENC-ADD-RI8
      scan JMP,
   hit X64CODE:LBL,
   RBX RDX ASM-SINK ENC-MOV-RR  R12 R10 ASM-SINK ENC-MOV-RR
   R13 R9 ASM-SINK ENC-MOV-RR  R14 RAX ASM-SINK ENC-MOV-RR ;

\ The address, the stack's top when the stack holds one, and the return address.
: BP-PRINT-HIT, ( label label -- ) {: head:label lr:label :}
   X64CODE:LBL {: empty:label :}
   head BP-HEAD$ nip BP-LINE,
   RAX R14 ASM-SINK ENC-MOV-RR  BP-HEX,
   R15 R13 STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RBX DSTACK-REG UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RCX R15 ASM-SINK ENC-CMP-RR  C-BE empty JCC,
   RAX RCX CELL negate MEM-OFF ASM-SINK ENC-MOV-RM  BP-HEX,
   empty X64CODE:LBL,
   lr BP-LR$ nip BP-LINE,
   RCX RBX RSP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX MEM-AT ASM-SINK ENC-MOV-RM  BP-HEX, ;

\ The data stack from its base, r15, up to the saved stack pointer.
: BP-PRINT-STACK, ( label -- ) {: head:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   head BP-STACK$ nip BP-LINE,
   next X64CODE:LBL,
      RCX RBX DSTACK-REG UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
      R15 RCX ASM-SINK ENC-CMP-RR  C-AE done JCC,
      RAX R15 MEM-AT ASM-SINK ENC-MOV-RM  BP-HEX,
      R15 CELL >IMM8 ASM-SINK ENC-ADD-RI8
      next JMP,
   done X64CODE:LBL, ;

\ Each watch cell's address and value, r15 walking the table to r9.
: BP-PRINT-WATCH, ( label -- ) {: head:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   RCX R13 BPWN-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   R15 R13 BPWBASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   R15 R15 ASM-SINK ENC-TEST-RR  C-E done JCC,
   R9 R15 RCX CELL 0 MEM-IDX ASM-SINK ENC-LEA
   head BP-WATCH$ nip BP-LINE,
   next X64CODE:LBL,
      R15 R9 ASM-SINK ENC-CMP-RR  C-AE done JCC,
      R10 R15 MEM-AT ASM-SINK ENC-MOV-RM
      RAX R10 ASM-SINK ENC-MOV-RR  BP-HEX,
      RAX R10 MEM-AT ASM-SINK ENC-MOV-RM  BP-HEX,
      R15 CELL >IMM8 ASM-SINK ENC-ADD-RI8
      next JMP,
   done X64CODE:LBL, ;

: BP-RESUME, ( -- )
   X64CODE:LBL X64CODE:LBL {: tramp:label done:label :}
   RAX R12 BP-CTRL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX 1 >IMM32 ASM-SINK ENC-TEST-RI32  C-NE tramp JCC,
   RAX R12 BP-WORD MEM-OFF ASM-SINK ENC-MOV-RM
   RCX R14 ASM-SINK ENC-MOV-RR  RCX R13 REGION-ALIAS:DELTA-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   RAX R64>N >R8 RCX MEM-AT ASM-SINK ENC-MOV8-MR
   RAX ZERO-REG,  RAX R12 MEM-AT ASM-SINK ENC-MOV-MR
   R14 RBX UC-RIP MEM-OFF ASM-SINK ENC-MOV-MR
   done JMP,
   tramp X64CODE:LBL,
   RAX R12 ASM-SINK ENC-MOV-RR  RAX R13 ASM-SINK ENC-SUB-RR
   RAX BPTAB-OFF >IMM32 ASM-SINK ENC-SUB-RI32
   RAX BP-SLOT-SHIFT >IMM8 ASM-SINK ENC-SHR-RI8
   RAX RAX BREAKPOINT:TRAMP >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX R13 BPTRAMP-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   RAX RBX UC-RIP MEM-OFF ASM-SINK ENC-MOV-MR
   done X64CODE:LBL, ;

\ Bind the handler at the first label, then its lines, where no control falls
\ in; the second is the crash handler's.
: BP-HANDLER, ( label label -- ) {: at:label crash:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: resume:label head:label lr:label stack:label watch:label :}
   at X64CODE:LBL,
   RAX RDX UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM  RAX ASM-SINK ENC-DEC
   R8 TEXT-BASE,  R8 R8 REGION-FROM-TEXT MEM-OFF ASM-SINK ENC-LEA
   RCX RAX ASM-SINK ENC-MOV-RR  RCX R8 ASM-SINK ENC-SUB-RR
   RCX REGION >IMM32 ASM-SINK ENC-CMP-RI32  C-AE crash JCC,
   crash BP-SCAN,
   RAX R12 BP-HITS MEM-OFF ASM-SINK ENC-MOV-RM  RAX ASM-SINK ENC-INC
   RAX R12 BP-HITS MEM-OFF ASM-SINK ENC-MOV-MR
   RCX R12 BP-CTRL MEM-OFF ASM-SINK ENC-MOV-RM  RCX 1 >IMM8 ASM-SINK ENC-SHR-RI8
   RAX RCX ASM-SINK ENC-CMP-RR  C-BE resume JCC,      \ hits <= skip: silent
   head lr BP-PRINT-HIT,
   stack BP-PRINT-STACK,
   watch BP-PRINT-WATCH,
   resume X64CODE:LBL,
   BP-RESUME,
   RET,
   head X64CODE:LBL,  BP-HEAD$ BYTES,
   lr X64CODE:LBL,  BP-LR$ BYTES,
   stack X64CODE:LBL,  BP-STACK$ BYTES,
   watch X64CODE:LBL,  BP-WATCH$ BYTES, ;

\ Install the handler at a label for crash.f's signals, each returning through
\ the restorer at the other, on the alternate stack SIGNAL-STACK, registered.
\ Unchecked, as crash.f's install is: a refused install leaves that signal's
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
   ENGINE-GPR:X64-CP >R64 {: cp:r64 :}
   cp BPTRAMP-CELL CELL!
   RDI cp ASM-SINK ENC-MOV-RR
   RDI RBASE-REG REGION-ALIAS:DELTA-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   RAX $CCCCCCCCCCCCCCCC IMM,
   BREAKPOINT:BYTES 0 ?do RAX RDI i MEM-OFF ASM-SINK ENC-MOV-MR CELL +loop
   cp cp BREAKPOINT:BYTES MEM-OFF ASM-SINK ENC-LEA ;

: OPEN, ( -- )
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
   RAX code-end MOVABS,  RAX CODE-END-CELL CELL!
   crash rest stub SETTLE,
   booted JMP,
   kernel 0= if
      \ No kernel follows to throw from: FAULT-THROW, has written the record, so
      \ exit as an uncaught memory fault.
      LFAULTREC @ >LABEL X64CODE:LBL,  RDI EXIT-STATUS:MEMORY-FAULT IMM,  NR-EXIT-GROUP SYS,
   then
   booted crash rest stub CLOSE, ;

\ A full engine's MAIN runs a saved application before routing stdin or the
\ terminal. An image without MAIN can run APP-ENTRY directly. A word that
\ returns exits successfully, while an image with neither entry is broken.
: ENTRY, ( -- )
   X64CODE:LBL X64CODE:LBL {: go:label none:label :}
   RAX RBASE-REG ENGINE-MAIN:XT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE go JCC,
   RAX RBASE-REG APP-ENTRY:XT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E none JCC,
   go X64CODE:LBL,
   RAX ASM-SINK ENC-CALL-REG
   RDI ZERO-REG,  LEXITHOOK @ >LABEL CALL,
   NR-EXIT-GROUP SYS,
   none S\" hb: no entry\n" EXIT-STATUS:DISPATCH-UNSET FAIL, ;

;using   \ LINUX-LAYOUT
;using
;using
;package
