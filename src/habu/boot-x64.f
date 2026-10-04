\ boot-x64.f - the x86-64 engine's process entry, package X64BOOT. START, emits
\ `_start`, the twin of src/habu/habu2.f EM-STARTUP up to its first run-time
\ state: it maps the guarded VM stacks and initializes the code and DATA regions,
\ loads the six VM registers (layout.f ENGINE-GPR), fills the DATA cells the
\ ARM64 boot fills, publishes the signal stub it carries out of line, registers
\ a separate crash signal stack and installs the crash handler, then falls
\ through into whatever the stream emits
\ next. docs/x86-64.md "Kernel inventory" lists each register and cell beside
\ its ARM64 twin.
\
\ It is straight-line code that never calls or pushes, so rsp stays where the
\ kernel left it, at argc, until the DATA cells take the argument vector. Each
\ install builds its action below rsp and gives rsp back.
\
\ The signal words below are the x86-64 side of what src/habu/crash.f emits for
\ ARM64: SIGNAL-STUB, is its LSIGH, SIGACTION, and SIGACTION-AT, install a
\ handler through rt_sigaction, RESTORER, emits the rt_sigreturn stub every
\ handler returns through, and UC-GREG and UC-RIP name where the ucontext a
\ SA_SIGINFO handler receives keeps the interrupted registers. The crash handler START, installs through them is the twin of
\ crash.f EMIT-CRASH-HANDLER: it names a VM stack whose guard page faulted and
\ exits 102, or dumps the registers on fd 2 and exits 134.
\
\ The syscall numbers and SYS, are the x86-64 seam's, loaded below the way
\ tools/native-emit.f loads a target's seam before the files that emit against
\ it. A file that loads the seam into a private wordlist first (the peer
\ harness, test/x86-64-peer-harness.f) must come after this one: the require
\ below would then load nothing and the names here would be undefined.
require lib/byte-buffer.f
require lib/string.f
require src/core/cell.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/habu/task-abi.f
require src/habu/snapshot-format.f
require src/habu/snap-decode-x64.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f
require src/os/linux-x86-64/target-layout.f

package X64BOOT
using X64ASM
using X64CODE
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

0 constant PROT-NONE
3 constant PROT-RW                  \ PROT_READ|PROT_WRITE
5 constant PROT-RX                  \ PROT_READ|PROT_EXEC
\ The rc every boot mapping failure exits with: STACK-GUARD's MAP-FAIL-RC
\ (src/habu/rt.f) and habu2.f's two fixed-region mappings.
78 constant MAP-FAIL-RC
2 constant STDERR
16 constant CODE-SLOT

\ The four failures the boot names, one label each per image.
variable STACK-BAD
variable ALT-BAD
variable REGION-BAD
variable DATA-BAD
variable SNAP-END

: RBASE-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSTACK-REG ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;

: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

\ Append a string's bytes to the stream.
: TEXT-BYTES, ( ptr u8 n -- ) BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN ;

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
\ after the lea is that base plus the lea's end offset. habu2.f EM-STARTUP takes
\ the same base into XREG-RBASE with an ADR.
: TEXT-BASE, ( r64 -- ) {: r:r64 :}
   r 0 MEM-RIP ASM-SINK ENC-LEA
   r X64CODE:ASM-LEN >IMM32 ASM-SINK ENC-SUB-RI32 ;

\ r13 = the code region, REGION bytes at the image base plus REGION-OFF, where
\ the writer's PT_LOAD places it; r15 = its code area past the records;
\ r14 = no records; rbx = 0. The twin of EM-MMAP-CODE-REGION and EM-SEED-DICT.
: CODE-REGION, ( -- )
   RDI RBASE-REG REGION-OFF X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   REGION REGION-BAD @ >LABEL MAP-FIXED,
   DBASE-REG RDI ASM-SINK ENC-MOV-RR
   ENGINE-GPR:X64-CP >R64 DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   ENGINE-GPR:X64-NDICT >R64 ZERO-REG,
   ENGINE-GPR:X64-INTERP >R64 ZERO-REG, ;

\ rax = DATA, DATA-SIZE bytes at DATA-VA, as EM-MMAP-DATA-REGION maps it.
: DATA-REGION, ( -- )
   RDI X64LAYOUT:DATA-VA VA>N IMM,
   X64LAYOUT:DATA-SIZE DATA-BAD @ >LABEL MAP-FIXED, ;

\ A linked image carries its region and DATA in PT_LOADs. The records begin
\ at r13 and the code pointer and record count are known at write time.
: LINKED-REGION, ( n n -- ) {: records:n cp:n :}
   DBASE-REG RBASE-REG REGION-OFF X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   ENGINE-GPR:X64-CP >R64 cp IMM,
   ENGINE-GPR:X64-NDICT >R64 records IMM,
   ENGINE-GPR:X64-INTERP >R64 ZERO-REG,
   RDI DBASE-REG ASM-SINK ENC-MOV-RR  RSI REGION IMM,  RDX PROT-RX IMM,
   NR-MPROTECT SYS,
   C-B REGION-BAD @ >LABEL JCC, ;

\ The snapshot's REGION and DATA are already mapped at their fixed VAs.
\ Its trailer supplies the exact live dictionary count and code pointer.
: SNAP-REGION, ( -- )
   DBASE-REG RBASE-REG REGION-OFF X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   R9 RAX SNAP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   ENGINE-GPR:X64-NDICT >R64 R9 SNAP-TRL-NDICT MEM-OFF ASM-SINK ENC-MOV-RM
   RCX R9 SNAP-TRL-REGLEN MEM-OFF ASM-SINK ENC-MOV-RM
   ENGINE-GPR:X64-CP >R64 DBASE-REG ASM-SINK ENC-MOV-RR
   ENGINE-GPR:X64-CP >R64 RCX ASM-SINK ENC-ADD-RR
   ENGINE-GPR:X64-INTERP >R64 ZERO-REG,
   RDI DBASE-REG ASM-SINK ENC-MOV-RR  RSI REGION IMM,  RDX PROT-RX IMM,
   NR-MPROTECT SYS,
   C-B REGION-BAD @ >LABEL JCC, ;

\ Reject a malformed appended frame before the decoder reads any RX payload.
\ R8 is the immutable cold text end and R9 points to the final 48-byte
\ trailer; R10/R11 are the exact heap extent and wire form for the decoder.
: SNAP-FRAME, ( label label -- ) {: bad:label badver:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: framed:label legacy:label versioned:label :}
   R8 SNAP-END @ >LABEL MOVABS,
   RCX RBASE-REG $60 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RAX R8 ASM-SINK ENC-MOV-RR  RAX RBASE-REG ASM-SINK ENC-SUB-RR
   RAX X64LAYOUT:CODE-OFF >IMM32 ASM-SINK ENC-ADD-RI32
   RAX SNAP-TRL-LEGACY-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   RCX RAX ASM-SINK ENC-CMP-RR  C-B bad JCC,
   RAX SNAP-TRL-BYTES SNAP-TRL-LEGACY-BYTES - >IMM8 ASM-SINK ENC-ADD-RI8
   RCX RAX ASM-SINK ENC-CMP-RR  C-B legacy JCC,
   R9 RBASE-REG SNAP-TRL-BYTES X64LAYOUT:CODE-OFF + negate MEM-OFF ASM-SINK ENC-LEA
   R9 RCX ASM-SINK ENC-ADD-RR
   RAX R9 MEM-AT ASM-SINK ENC-MOV-RM
   RDX SNAP-MAGIC IMM,
   RAX RDX ASM-SINK ENC-CMP-RR  C-E versioned JCC,
   legacy X64CODE:LBL,
   R9 RBASE-REG SNAP-TRL-LEGACY-BYTES X64LAYOUT:CODE-OFF + negate MEM-OFF ASM-SINK ENC-LEA
   R9 RCX ASM-SINK ENC-ADD-RR
   RAX R9 MEM-AT ASM-SINK ENC-MOV-RM
   RDX SNAP-MAGIC IMM,
   RAX RDX ASM-SINK ENC-CMP-RR  C-E badver JCC,
   bad JMP,
   versioned X64CODE:LBL,
   RAX R9 SNAP-TRL-VERSION MEM-OFF ASM-SINK ENC-MOV-RM
   RAX SNAPSHOT-FORMAT:VERSION >IMM32 ASM-SINK ENC-CMP-RI32  C-NE badver JCC,
   R11 R9 SNAPSHOT-FORMAT:HEAP-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   R11 SNAPSHOT-FORMAT:HEAP-GRID >IMM8 ASM-SINK ENC-CMP-RI8  C-E framed JCC,
   R11 SNAPSHOT-FORMAT:HEAP-RAW >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   framed X64CODE:LBL,
   RAX R9 SNAP-TRL-NDICT MEM-OFF ASM-SINK ENC-MOV-RM
   RAX DICT-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-A bad JCC,
   RAX R9 SNAP-TRL-REGLEN MEM-OFF ASM-SINK ENC-MOV-RM
   RAX DICT-SIZE >IMM32 ASM-SINK ENC-CMP-RI32  C-B bad JCC,
   RAX X64LAYOUT:CODE-CEILING >IMM32 ASM-SINK ENC-CMP-RI32  C-A bad JCC,
   RAX CODE-SLOT 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE bad JCC,
   \ ELF64's fifth/sixth headers must carry exactly the saved fixed spans.
   RCX RBASE-REG $130 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RDX RBASE-REG REGION-OFF X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-LEA
   RCX RDX ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RCX RBASE-REG $140 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RCX RBASE-REG $148 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RCX REGION >IMM32 ASM-SINK ENC-CMP-RI32  C-NE bad JCC,
   RCX RBASE-REG $168 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RDX X64LAYOUT:DATA-VA VA>N IMM,
   RCX RDX ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RCX RBASE-REG $178 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RCX DATA-START >IMM32 ASM-SINK ENC-CMP-RI32  C-NE bad JCC,
   RCX RBASE-REG $180 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RCX X64LAYOUT:DATA-SIZE >IMM32 ASM-SINK ENC-CMP-RI32  C-NE bad JCC,
   RAX R9 SNAP-TRL-DATALEN MEM-OFF ASM-SINK ENC-MOV-RM
   RAX DATA-START >IMM32 ASM-SINK ENC-CMP-RI32  C-B bad JCC,
   RAX X64LAYOUT:DATA-SIZE >IMM32 ASM-SINK ENC-CMP-RI32  C-A bad JCC,
   RCX R9 ASM-SINK ENC-MOV-RR  RCX R8 ASM-SINK ENC-SUB-RR
   RAX DATA-START >IMM32 ASM-SINK ENC-SUB-RI32
   RAX RCX ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   R10 RAX DP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX X64LAYOUT:DATA-VA VA>N DATA-START + IMM,
   R10 RCX ASM-SINK ENC-CMP-RR  C-B bad JCC,
   RCX X64LAYOUT:DATA-VA VA>N X64LAYOUT:DATA-SIZE PROF-CNT-BYTES - + IMM,
   R10 RCX ASM-SINK ENC-CMP-RR  C-A bad JCC,
   R9 RAX SNAP-CELL MEM-OFF ASM-SINK ENC-MOV-MR ;

\ The twin of EM-DATA-INIT: publish the text base, then rbp becomes DATA; then
\ the data stack's extent, the argument vector ([rsp] = argc, argv at rsp + 8,
\ envp past argv's null) and the heap floor with the DP that starts at it.
: DATA-INIT, ( bool -- ) {: heap:bool :}
   RBASE-REG RAX RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RBASE-REG RAX ASM-SINK ENC-MOV-RR
   DSTACK-REG STACK-ABI:BASE-CELL CELL!
   RAX STACK-ABI:BOOT-BYTES IMM,  RAX STACK-ABI:CAP-CELL CELL!
   RAX RSP MEM-AT ASM-SINK ENC-MOV-RM  RAX ARGC-CELL CELL!
   RCX RSP CELL MEM-OFF ASM-SINK ENC-LEA  RCX ARGV-CELL CELL!
   RCX RCX RAX CELL CELL MEM-IDX ASM-SINK ENC-LEA  RCX ENVP-CELL CELL!
   heap if
      RAX DATA-START IMM,  RAX BOOT-LAYOUT:HEAP-START-CELL CELL!
      RAX RBASE-REG DATA-START MEM-OFF ASM-SINK ENC-LEA  RAX DP-CELL CELL!
   then
   RAX 1 IMM,  RAX NCOMP-DISPATCH:TIER-CELL CELL! ;

\ The twin of EM-FRAME-STACKS: the return and DO/LOOP frame stacks, published
\ in their DATA cells.
: FRAME-STACKS, ( -- )
   STACK-ABI:RETURN-BYTES RAX STACK-BAD @ >LABEL MAP-STACK,
   RAX STACK-ABI:RETURN-BASE-CELL CELL!
   STACK-ABI:LOOP-BYTES RAX STACK-BAD @ >LABEL MAP-STACK,
   RAX STACK-ABI:LOOP-BASE-CELL CELL! ;

\ Bind a failure at the label: its line, newline included, to fd 2 in one
\ write, then exit with the status. The line follows the exit syscall inside
\ the loaded text, so it is readable before any region exists.
: FAIL, ( label ptr u8 n n -- ) {: at:label a:ptr u:n rc:n :}
   X64CODE:LBL {: msg:label :}
   at X64CODE:LBL,
   RDI STDERR IMM,  RSI msg MOVABS,  RDX u IMM,  NR-WRITE SYS,
   RDI rc IMM,  NR-EXIT-GROUP SYS,
   msg X64CODE:LBL,
   a u TEXT-BYTES, ;

\ The width of the stub's one write: under PIPE_BUF, so a pipe takes it whole.
4 constant SIGNO-BYTES

\ The fd word's absolute address. DATA is MAP_FIXED at DATA-VA and DATA-REGION,
\ refuses a boot the kernel answered elsewhere, so this one address names the
\ same word for the life of the process, whatever task is running.
: FD-WORD-VA ( -- n ) X64LAYOUT:DATA-VA VA>N SIGNAL-ABI:FD-CELL + ;

\ Bind the signal stub at the label: the twin of src/habu/crash.f
\ EMIT-SIGNAL-HANDLER (LSIGH), whose comment gives the contract. It is a
\ `void (int)` sa_handler, so the number arrives in edi; its four low bytes,
\ pushed, are what the fd receives in one write whose result it ignores. It
\ reads the fd word by its absolute address, never through rbp, which is the
\ interrupted thread's own DATA; a word of zero absorbs the signal. It costs
\ rax rcx rdx rsi r11, the flags and a cell below its own rsp, all of which
\ rt_sigreturn restores, and touches no Forth state. Its `ret` enters the
\ restorer the installer named: lib/signal.f installs through libc sigaction,
\ which supplies one.
: SIGNAL-STUB, ( label -- )
   X64CODE:LBL,
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
   ASM-SINK ENC-RET ;

\ Publish the stub bound at the label and the address of the word it reads,
\ as habu2.f EM-STARTUP-RUNTIME-STATE does, so a program installs and arms the
\ stub without spelling either (layout.f package SIGNAL-ABI). The word itself
\ needs no clear: DATA is a fresh mapping here, where the ARM64 boot clears it
\ after a restore that carried DATA bytes.
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
$08000000 constant SA-ONSTACK

0 constant SS-SP
8 constant SS-FLAGS
16 constant SS-SIZE
24 constant SS-BYTES

\ A machine CALL can exhaust its own stack before the VM return-stack guard.
\ The crash handler needs a different stack for the kernel's signal frame.
: CRASH-ALTSTACK, ( -- )
   STACK-ABI:PAGE-BYTES RAX ALT-BAD @ >LABEL MAP-STACK,
   RSP SS-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP SS-SP MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ZERO-REG,  RCX RSP SS-FLAGS MEM-OFF ASM-SINK ENC-MOV-MR
   RCX STACK-ABI:PAGE-BYTES IMM,
   RCX RSP SS-SIZE MEM-OFF ASM-SINK ENC-MOV-MR
   RDI RSP ASM-SINK ENC-MOV-RR  RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   RSP RSP SS-BYTES MEM-OFF ASM-SINK ENC-LEA
   C-B ALT-BAD @ >LABEL JCC, ;

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
\ register but rsp may hold it. It clobbers rax rcx rdx rsi rdi r10 r11 and
\ leaves CF set when the kernel refused, as SYS, does: the lea that gives the
\ frame back keeps the flags.
: SIGACTION-AT, ( n n r64 label -- ) {: sig:n flags:n handler:r64 rest:label :}
   RSP SA-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   handler RSP SA-HANDLER MEM-OFF ASM-SINK ENC-MOV-MR
   RAX flags SA-RESTORER or IMM,  RAX RSP SA-FLAGS MEM-OFF ASM-SINK ENC-MOV-MR
   RAX rest MOVABS,  RAX RSP SA-RESTORER-AT MEM-OFF ASM-SINK ENC-MOV-MR
   RAX ZERO-REG,  RAX RSP SA-MASK MEM-OFF ASM-SINK ENC-MOV-MR
   RDI sig IMM,  RSI RSP ASM-SINK ENC-MOV-RR  RDX ZERO-REG,
   R10 SIGSET-BYTES IMM,
   NR-SIGACTION SYS,
   RSP RSP SA-BYTES MEM-OFF ASM-SINK ENC-LEA ;

\ The same for the handler at a label, through rax.
: SIGACTION, ( n n label label -- ) {: sig:n flags:n handler:label rest:label :}
   RAX handler MOVABS,
   sig flags RAX rest SIGACTION-AT, ;

private

\ ---- the crash handler -------------------------------------------------------
\ The twin of crash.f EMIT-CRASH-HANDLER. The kernel enters it with rdi = the
\ signal, rsi = the siginfo and rdx = the ucontext. It never returns, so it
\ owns every register: it keeps those three and the text base where neither
\ the printer nor `syscall` writes.
\
\ A SIGSEGV or SIGBUS whose saved rip lies in Habu's own code, from the text
\ base to the code region's end, is classified first, as crash.f classifies
\ one. Only there is the saved rbp DATA; foreign SysV code keeps a frame
\ pointer in it. A fault address in the page below a VM stack's base, or in
\ the page at its capacity, writes that stack's line and exits
\ ENGINE-ERROR:STACK-BOUNDS; a base that is zero or not PAGE-BYTES aligned is
\ no stack and is skipped.
\
\ Anything else is dumped to fd 2, one write per line: HEAD$, then sixteen
\ lowercase hex digits per line for the signal, the sixteen registers by
\ number and rip, then the 24 bytes from rip - 8 as three cells in memory
\ order, each 0 unless its eight bytes lie in the code region. The region lies
\ a fixed distance past the text, so its base comes rip-relative, never from
\ the saved r13, and the handler reads no address it cannot prove mapped. It
\ then exits CRASH-RC, as crash.f does.
4 constant SIGILL
5 constant SIGTRAP
7 constant SIGBUS
8 constant SIGFPE
11 constant SIGSEGV
$10 constant SI-ADDR                    \ siginfo_t._sifields._sigfault.si_addr
134 constant CRASH-RC
\ The code region's base less the text base.
REGION-OFF X64LAYOUT:CODE-OFF - constant REGION-FROM-TEXT

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

: HEX$ ( -- ptr u8 n ) s" 0123456789abcdef" ;
\ crash.f's CRS-DATA$, CRS-RET$ and CRS-LOOP$, byte for byte. crash.f itself
\ is not loaded here: only the ARM64 build driver loads it, beside A64ASM.
: DATA-HIT$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (data)\n" ;
: RETURN-HIT$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (return)\n" ;
: LOOP-HIT$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (loop)\n" ;
: HEAD$ ( -- ptr u8 n )
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
   RDI STR-LF IMM,  low RSP DIGITS MEM-OFF ASM-SINK ENC-MOV8-MR
   RDI STDERR IMM,  RSI RSP ASM-SINK ENC-MOV-RR  RDX LINE-BYTES IMM,
   NR-WRITE SYS,
   RSP LINE-FRAME >IMM8 ASM-SINK ENC-ADD-RI8
   ASM-SINK ENC-RET
   digits X64CODE:LBL,
   HEX$ TEXT-BYTES, ;

\ Print rax through the printer at the label, in the given order.
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

\ A data access through the page below an owned stack can resume at the
\ kernel's underdepth throw entry. GUARDS, admits the saved RIP as owned code
\ before this routine reads the saved DATA descriptor.
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
   ASM-SINK ENC-RET
   done X64CODE:LBL, ;

\ Classify a context's three guarded VM stacks in crash.f's order.
: STACK-GUARDS, ( label label label -- ) {: dhit:label rhit:label lhit:label :}
   [: RAX RDI STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM ;]
   STACK-ABI:BASE-CELL dhit GUARD-CASE,
   [: RAX STACK-ABI:RETURN-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:RETURN-BASE-CELL rhit GUARD-CASE,
   [: RAX STACK-ABI:LOOP-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:LOOP-BASE-CELL lhit GUARD-CASE, ;

\ A task's REGION can be released while the signal handler is inspecting its
\ persistent TCB. process_vm_readv copies only the four stack descriptor
\ cells; a partial copy leaves no descriptor to classify. The two iovec arrays,
\ their four result cells and the next-chain pointer live in this handler's
\ machine stack frame. No task-region address is dereferenced in the handler.
$40 constant TASK-REMOTE-AT
$80 constant TASK-COPY-AT
$A0 constant TASK-NEXT-AT
$B0 constant TASK-FRAME-BYTES
STACK-ABI:LOOP-BASE-CELL CELL + constant TASK-REGION-MIN

: TASK-IOV, ( n n -- ) {: index:n off:n :}
   RAX RSP TASK-COPY-AT index cells + MEM-OFF ASM-SINK ENC-LEA
   RAX RSP index 16 * MEM-OFF ASM-SINK ENC-MOV-MR
   RAX CELL IMM,
   RAX RSP index 16 * CELL + MEM-OFF ASM-SINK ENC-MOV-MR
   RAX R9 off MEM-OFF ASM-SINK ENC-LEA
   RAX RSP TASK-REMOTE-AT index 16 * + MEM-OFF ASM-SINK ENC-MOV-MR
   RAX CELL IMM,
   RAX RSP TASK-REMOTE-AT index 16 * + CELL + MEM-OFF ASM-SINK ENC-MOV-MR ;

: TASK-GUARDS, ( label label label -- ) {: dhit:label rhit:label lhit:label :}
   [: RAX RDI CELL MEM-OFF ASM-SINK ENC-ADD-RM ;]
   0 dhit GUARD-CASE,
   [: RAX STACK-ABI:RETURN-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   2 cells rhit GUARD-CASE,
   [: RAX STACK-ABI:LOOP-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   3 cells lhit GUARD-CASE, ;

: TASK-CHAIN-GUARDS, ( label label label label -- )
   {: dump:label dhit:label rhit:label lhit:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: loop:label next:label done:label :}
   RDI X64LAYOUT:DATA-VA VA>N IMM,
   RAX RDI TASK-CHAIN-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E dump JCC,
   RCX RAX ASM-SINK ENC-MOV-RR  RCX RDI ASM-SINK ENC-SUB-RR
   RCX CELL 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE dump JCC,
   RCX X64LAYOUT:DATA-SIZE CELL - >IMM32 ASM-SINK ENC-CMP-RI32
      C-A dump JCC,
   R8 RAX MEM-AT ASM-SINK ENC-MOV-RM
   RBP RSI ASM-SINK ENC-MOV-RR
   RSP TASK-FRAME-BYTES >IMM32 ASM-SINK ENC-SUB-RI32
   R8 RSP TASK-NEXT-AT MEM-OFF ASM-SINK ENC-MOV-MR
   loop X64CODE:LBL,
   R8 RSP TASK-NEXT-AT MEM-OFF ASM-SINK ENC-MOV-RM
   R8 R8 ASM-SINK ENC-TEST-RR  C-E done JCC,
   RDI X64LAYOUT:DATA-VA VA>N IMM,
   RAX R8 ASM-SINK ENC-MOV-RR  RAX RDI ASM-SINK ENC-SUB-RR
   RAX X64LAYOUT:DATA-SIZE TASK-ABI:TCB-BYTES CELL + - >IMM32
      ASM-SINK ENC-CMP-RI32  C-A done JCC,
   R8 CELL 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE done JCC,
   RAX R8 TASK-ABI:TCB-BYTES MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSP TASK-NEXT-AT MEM-OFF ASM-SINK ENC-MOV-MR
   R9 R8 TASK-ABI:REGION-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX R8 TASK-ABI:REGION-U-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX TASK-REGION-MIN >IMM32 ASM-SINK ENC-CMP-RI32  C-B next JCC,
   0 STACK-ABI:BASE-CELL TASK-IOV,
   1 STACK-ABI:CAP-CELL TASK-IOV,
   2 STACK-ABI:RETURN-BASE-CELL TASK-IOV,
   3 STACK-ABI:LOOP-BASE-CELL TASK-IOV,
   NR-GETPID SYS,
   RDI RAX ASM-SINK ENC-MOV-RR
   RSI RSP ASM-SINK ENC-MOV-RR
   RDX 4 IMM,
   R10 RSP TASK-REMOTE-AT MEM-OFF ASM-SINK ENC-LEA
   R8 4 IMM,  R9 ZERO-REG,
   NR-PROCESS-VM-READV SYS,
   RAX 4 cells >IMM8 ASM-SINK ENC-CMP-RI8  C-NE next JCC,
   RDI RSP TASK-COPY-AT MEM-OFF ASM-SINK ENC-LEA
   RSI RBP ASM-SINK ENC-MOV-RR
   dhit rhit lhit TASK-GUARDS,
   next X64CODE:LBL,  loop JMP,
   done X64CODE:LBL,
   RSP TASK-FRAME-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   dump JMP, ;

\ Saved RBP is a DATA descriptor only when the saved instruction was Habu
\ code. An instruction fetch in a guard has RIP outside every code interval,
\ so inspect the fixed root DATA directly there and never dereference RBP.
: GUARDS, ( label -- ) {: dump:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
      {: fault:label region:label owned:label foreign:label dhit:label rhit:label lhit:label :}
   SIG-REG SIGSEGV >IMM8 ASM-SINK ENC-CMP-RI8  C-E fault JCC,
   SIG-REG SIGBUS >IMM8 ASM-SINK ENC-CMP-RI8  C-NE dump JCC,
   fault X64CODE:LBL,
   RSI INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-MOV-RM
   RDI X64LAYOUT:DATA-VA VA>N IMM,
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RAX TEXT-REG ASM-SINK ENC-CMP-RR  C-B region JCC,
   RCX RDI CODE-END-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX ASM-SINK ENC-CMP-RR  C-B owned JCC,
   region X64CODE:LBL,
   RCX REGION-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   RAX RCX ASM-SINK ENC-CMP-RR  C-B foreign JCC,
   RCX REGION-REG REGION MEM-OFF ASM-SINK ENC-LEA
   RAX RCX ASM-SINK ENC-CMP-RR  C-AE foreign JCC,
   owned X64CODE:LBL,
   RDI UC-REG RBP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   DATA-RECOVER,
   dhit rhit lhit STACK-GUARDS,
   dump JMP,
   foreign X64CODE:LBL,
   RDI X64LAYOUT:DATA-VA VA>N IMM,
   dhit rhit lhit STACK-GUARDS,
   dump dhit rhit lhit TASK-CHAIN-GUARDS,
   dhit DATA-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL,
   rhit RETURN-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL,
   lhit LOOP-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL, ;

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
\ code lines, then exit CRASH-RC.
: DUMP, ( label label -- ) {: head:label hex:label :}
   RDI STDERR IMM,  RSI head MOVABS,  RDX HEAD$ nip IMM,  NR-WRITE SYS,
   RAX SIG-REG ASM-SINK ENC-MOV-RR  hex VALUE-ORDER PRINT,
   16 0 ?do
      RAX UC-REG i >R64 UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM  hex VALUE-ORDER PRINT,
   loop
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM  hex VALUE-ORDER PRINT,
   REGION-REG TEXT-REG REGION-FROM-TEXT MEM-OFF ASM-SINK ENC-LEA
   CELL negate hex CODE-LINE,  0 hex CODE-LINE,  CELL hex CODE-LINE,
   RDI CRASH-RC IMM,  NR-EXIT-GROUP SYS, ;

\ Bind the handler at the label, then the printer and the header it writes,
\ where no control falls in.
: CRASH-HANDLER, ( label -- ) {: at:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: dump:label hex:label head:label :}
   at X64CODE:LBL,
   SIG-REG RDI ASM-SINK ENC-MOV-RR
   INFO-REG RSI ASM-SINK ENC-MOV-RR
   UC-REG RDX ASM-SINK ENC-MOV-RR
   TEXT-REG TEXT-BASE,
   REGION-REG TEXT-REG REGION-FROM-TEXT MEM-OFF ASM-SINK ENC-LEA
   dump GUARDS,
   dump X64CODE:LBL,
   head hex DUMP,
   hex HEX-LINE,
   head X64CODE:LBL,
   HEAD$ TEXT-BYTES, ;

\ Install the handler at a label for crash.f's signals, each returning through
\ the restorer at the other. Unchecked, as crash.f's install is: a refused
\ install leaves that signal's default action.
: INSTALL-CRASH, ( label label -- ) {: at:label rest:label :}
   SIGILL SA-SIGINFO SA-ONSTACK or at rest SIGACTION,
   SIGTRAP SA-SIGINFO SA-ONSTACK or at rest SIGACTION,
   SIGBUS SA-SIGINFO SA-ONSTACK or at rest SIGACTION,
   SIGFPE SA-SIGINFO SA-ONSTACK or at rest SIGACTION,
   SIGSEGV SA-SIGINFO SA-ONSTACK or at rest SIGACTION, ;

\ Shared open and close of both startup paths.
: OPEN, ( -- )
   X64CODE:LBL STACK-BAD !  X64CODE:LBL ALT-BAD !  X64CODE:LBL REGION-BAD !  X64CODE:LBL DATA-BAD !
   RBASE-REG TEXT-BASE,
   STACK-ABI:BOOT-BYTES DSTACK-REG STACK-BAD @ >LABEL MAP-STACK, ;

: SETTLE, ( label label label -- ) {: crash:label rest:label stub:label :}
   stub PUBLISH-STUB,
   FRAME-STACKS,
   CRASH-ALTSTACK,
   crash rest INSTALL-CRASH, ;

: CLOSE, ( label label label label -- ) {: booted:label crash:label rest:label stub:label :}
   STACK-BAD @ >LABEL S\" hb: cannot map guarded VM stack\n" MAP-FAIL-RC FAIL,
   ALT-BAD @ >LABEL S\" hb: cannot install crash handler stack\n" MAP-FAIL-RC FAIL,
   crash CRASH-HANDLER,
   rest RESTORER,
   stub SIGNAL-STUB,
   booted X64CODE:LBL, ;

public

\ The caller binds this after the immutable text-site footer.
: TEXT-END, ( -- ) SNAP-END @ >LABEL X64CODE:LBL, ;

: START, ( label label -- ) {: floor:label code-end:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: booted:label crash:label rest:label stub:label :}
   OPEN,
   CODE-REGION,
   DATA-REGION,
   true DATA-INIT,
   floor PUBLISH-FLOOR,
   RAX code-end MOVABS,  RAX CODE-END-CELL CELL!
   crash rest stub SETTLE,
   booted JMP,
   REGION-BAD @ >LABEL S\" hb: cannot map fixed code region\n" MAP-FAIL-RC FAIL,
   DATA-BAD @ >LABEL S\" hb: cannot map fixed data region\n" MAP-FAIL-RC FAIL,
   booted crash rest stub CLOSE, ;

: LINKED-START, ( n n -- ) {: records:n cp:n :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: booted:label crash:label rest:label stub:label :}
   OPEN,
   records cp LINKED-REGION,
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,
   crash rest stub SETTLE,
   booted JMP,
   REGION-BAD @ >LABEL S\" hb: cannot protect the code region\n" MAP-FAIL-RC FAIL,
   booted crash rest stub CLOSE, ;

\ Full-engine entry accepts either its cold RX extent or an appended snapshot
\ frame. The snapshot's REGION and DATA prefixes are already loaded at their
\ fixed addresses; only the heap needs materialization before ENGINE-MAIN.
: SNAP-START, ( n n label [ -- ] -- ) {: records:n cp:n floor:label hidx :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: booted:label crash:label rest:label stub:label
      cold:label ready:label bad:label badver:label :}
   X64CODE:LBL SNAP-END !
   OPEN,
   RAX RBASE-REG $60 X64LAYOUT:CODE-OFF - MEM-OFF ASM-SINK ENC-MOV-RM
   RAX REGION-OFF >IMM32 ASM-SINK ENC-CMP-RI32  C-AE bad JCC,
   RCX SNAP-END @ >LABEL MOVABS,
   RCX RBASE-REG ASM-SINK ENC-SUB-RR
   RCX X64LAYOUT:CODE-OFF >IMM32 ASM-SINK ENC-ADD-RI32
   RAX RCX ASM-SINK ENC-CMP-RR  C-E cold JCC,
   C-B bad JCC,
   bad badver SNAP-FRAME,
   bad X64SNAP:DECODE,
   SNAP-REGION,
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,
   hidx execute
   ready JMP,
   cold X64CODE:LBL,
   records cp LINKED-REGION,
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,
   ready X64CODE:LBL,
   floor PUBLISH-FLOOR,
   crash rest stub SETTLE,
   booted JMP,
   bad S\" hb: malformed snapshot\n" 79 FAIL,
   badver S\" hb: unsupported snapshot version\n" 80 FAIL,
   REGION-BAD @ >LABEL S\" hb: cannot protect the code region\n" MAP-FAIL-RC FAIL,
   booted crash rest stub CLOSE, ;

\ A full engine's MAIN runs a saved application before routing stdin or the
\ terminal. An image without MAIN can run APP-ENTRY directly. A word that
\ returns exits successfully, while an image with neither entry is broken.
: ENTRY, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: go:label none:label exit:label :}
   RAX RBASE-REG ENGINE-MAIN:XT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE go JCC,
   RAX RBASE-REG APP-ENTRY:XT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E none JCC,
   go X64CODE:LBL,
   RAX ASM-SINK ENC-CALL-REG
   RAX RBASE-REG EXIT-HOOK-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E exit JCC,
   RCX ZERO-REG,  RCX EXIT-HOOK-CELL CELL!
   RAX ASM-SINK ENC-CALL-REG
   exit X64CODE:LBL,
   RDI ZERO-REG,  NR-EXIT-GROUP SYS,
   none S\" hb: no entry\n" ENGINE-ERROR:AOT-SEED FAIL, ;

;using   \ X64LAYOUT
;using
;using
;package
