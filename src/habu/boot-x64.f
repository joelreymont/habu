\ boot-x64.f - the x86-64 engine's process entry, package X64BOOT. START, emits
\ `_start`, the twin of src/habu/habu2.f EM-STARTUP up to its first run-time
\ state: it maps the guarded VM stacks, the code region and the DATA region,
\ loads the six VM registers (layout.f ENGINE-GPR), fills the DATA cells the
\ ARM64 boot fills, publishes the signal stub it carries out of line and
\ installs the crash handler, then falls through into whatever the stream emits
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

\ r = the runtime address of the stream's byte 0, the text content base: rip
\ after the lea is that base plus the lea's end offset. habu2.f EM-STARTUP takes
\ the same base into XREG-RBASE with an ADR.
: TEXT-BASE, ( r64 -- ) {: r:r64 :}
   r 0 MEM-RIP ASM-SINK ENC-LEA
   r ASM-LEN >IMM32 ASM-SINK ENC-SUB-RI32 ;

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

\ The twin of EM-DATA-INIT: publish the text base, then rbp becomes DATA; then
\ the data stack's extent, the argument vector ([rsp] = argc, argv at rsp + 8,
\ envp past argv's null) and the heap floor with the DP that starts at it.
\ The tier is 1, the one x86-64 has: the mapping's zero would select tier 0,
\ whose JIT rows the kernel refuses.
: DATA-INIT, ( -- )
   RBASE-REG RAX RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RBASE-REG RAX ASM-SINK ENC-MOV-RR
   DSTACK-REG STACK-ABI:BASE-CELL CELL!
   RAX STACK-ABI:BOOT-BYTES IMM,  RAX STACK-ABI:CAP-CELL CELL!
   RAX RSP MEM-AT ASM-SINK ENC-MOV-RM  RAX ARGC-CELL CELL!
   RCX RSP CELL MEM-OFF ASM-SINK ENC-LEA  RCX ARGV-CELL CELL!
   RCX RCX RAX CELL CELL MEM-IDX ASM-SINK ENC-LEA  RCX ENVP-CELL CELL!
   RAX DATA-START IMM,  RAX BOOT-LAYOUT:HEAP-START-CELL CELL!
   RAX RBASE-REG DATA-START MEM-OFF ASM-SINK ENC-LEA  RAX DP-CELL CELL!
   RAX 1 IMM,  RAX NCOMP-DISPATCH:TIER-CELL CELL! ;

\ The twin of EM-FRAME-STACKS: the return and DO/LOOP frame stacks, published
\ in their DATA cells.
: FRAME-STACKS, ( -- )
   STACK-ABI:RETURN-BYTES RAX MAP-STACK,  RAX STACK-ABI:RETURN-BASE-CELL CELL!
   STACK-ABI:LOOP-BYTES RAX MAP-STACK,  RAX STACK-ABI:LOOP-BASE-CELL CELL! ;

\ Bind a failure at the label: its line, newline included, to fd 2 in one
\ write, then exit with the status. The line follows the exit syscall inside
\ the loaded text, so it is readable before any region exists.
: FAIL, ( label ptr u8 n n -- ) {: at:label a:ptr u:n rc:n :}
   LBL {: msg:label :}
   at LBL,
   RDI STDERR IMM,  RSI msg MOVABS,  RDX u IMM,  NR-WRITE SYS,
   RDI rc IMM,  NR-EXIT-GROUP SYS,
   msg LBL,
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
   LBL,
   LBL {: done:label :}
   RDI ASM-SINK ENC-PUSH
   RAX FD-WORD-VA IMM,
   RDI RAX MEM-AT ASM-SINK ENC-MOV-RM
   RDI RDI ASM-SINK ENC-TEST-RR
   C-E done JCC8,
   RSI RSP ASM-SINK ENC-MOV-RR
   RDX R64>N >R32 SIGNO-BYTES >IMM32 ASM-SINK ENC-MOV32-RI32
   RAX R64>N >R32 NR-WRITE >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL
   done LBL,
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
   LBL,
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
\ The code region's base less the text base, and Habu's own code's length.
REGION-OFF X64LAYOUT:CODE-OFF - constant REGION-FROM-TEXT
REGION-FROM-TEXT REGION + constant HABU-SPAN

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
   LBL LBL {: digits:label next:label :}
   RDI R64>N >R8 {: low:r8 :}
   at LBL,
   RSP LINE-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   R8 digits MOVABS,
   RCX ZERO-REG,
   next LBL,
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
   digits LBL,
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
   LBL {: next:label :}
   RAX RDI base MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E next JCC,
   RAX STACK-ABI:PAGE-BYTES 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-NE next JCC,
   RCX RSI ASM-SINK ENC-MOV-RR  RCX RAX ASM-SINK ENC-SUB-RR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B hit JCC,
   execute
   RCX RSI ASM-SINK ENC-MOV-RR  RCX RAX ASM-SINK ENC-SUB-RR
   RCX STACK-ABI:PAGE-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B hit JCC,
   next LBL, ;

\ Classify a SIGSEGV or SIGBUS from Habu's own code by the three VM stacks,
\ crash.f's order; any other signal or address goes on to `dump`.
: GUARDS, ( label -- ) {: dump:label :}
   LBL LBL LBL LBL {: fault:label dhit:label rhit:label lhit:label :}
   SIG-REG SIGSEGV >IMM8 ASM-SINK ENC-CMP-RI8  C-E fault JCC,
   SIG-REG SIGBUS >IMM8 ASM-SINK ENC-CMP-RI8  C-NE dump JCC,
   fault LBL,
   RAX UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RAX TEXT-REG ASM-SINK ENC-SUB-RR
   RAX HABU-SPAN >IMM32 ASM-SINK ENC-CMP-RI32  C-AE dump JCC,
   RDI UC-REG RBP UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM
   RSI INFO-REG SI-ADDR MEM-OFF ASM-SINK ENC-MOV-RM
   [: RAX RDI STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM ;]
   STACK-ABI:BASE-CELL dhit GUARD-CASE,
   [: RAX STACK-ABI:RETURN-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:RETURN-BASE-CELL rhit GUARD-CASE,
   [: RAX STACK-ABI:LOOP-BYTES >IMM32 ASM-SINK ENC-ADD-RI32 ;]
   STACK-ABI:LOOP-BASE-CELL lhit GUARD-CASE,
   dump JMP,
   dhit DATA-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL,
   rhit RETURN-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL,
   lhit LOOP-HIT$ ENGINE-ERROR:STACK-BOUNDS FAIL, ;

\ One code line: the cell at the saved rip plus `off`, in memory order, when
\ all eight of its bytes lie in the code region at REGION-REG; 0 otherwise.
: CODE-LINE, ( n label -- ) {: off:n hex:label :}
   LBL {: print:label :}
   RSI UC-REG UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM
   RSI RSI off MEM-OFF ASM-SINK ENC-LEA
   RCX RSI ASM-SINK ENC-MOV-RR  RCX REGION-REG ASM-SINK ENC-SUB-RR
   RAX ZERO-REG,
   RCX REGION CELL - >IMM32 ASM-SINK ENC-CMP-RI32  C-A print JCC,
   RAX RSI MEM-AT ASM-SINK ENC-MOV-RM
   print LBL,
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
   LBL LBL LBL {: dump:label hex:label head:label :}
   at LBL,
   SIG-REG RDI ASM-SINK ENC-MOV-RR
   INFO-REG RSI ASM-SINK ENC-MOV-RR
   UC-REG RDX ASM-SINK ENC-MOV-RR
   TEXT-REG TEXT-BASE,
   dump GUARDS,
   dump LBL,
   head hex DUMP,
   hex HEX-LINE,
   head LBL,
   HEAD$ TEXT-BYTES, ;

\ Install the handler at a label for crash.f's signals, each returning through
\ the restorer at the other. Unchecked, as crash.f's install is: a refused
\ install leaves that signal's default action.
: INSTALL-CRASH, ( label label -- ) {: at:label rest:label :}
   SIGILL SA-SIGINFO at rest SIGACTION,
   SIGTRAP SA-SIGINFO at rest SIGACTION,
   SIGBUS SA-SIGINFO at rest SIGACTION,
   SIGFPE SA-SIGINFO at rest SIGACTION,
   SIGSEGV SA-SIGINFO at rest SIGACTION, ;

public

\ Emit `_start`. The ELF entry is the text's byte 0, so it begins the stream.
\ The crash handler goes in once the stacks it classifies are published; it,
\ its restorer and the signal stub sit behind the jump with the failures.
: START, ( -- )
   LBL STACK-BAD !  LBL REGION-BAD !  LBL DATA-BAD !
   LBL LBL LBL LBL {: booted:label crash:label rest:label stub:label :}
   RBASE-REG TEXT-BASE,
   STACK-ABI:BOOT-BYTES DSTACK-REG MAP-STACK,
   CODE-REGION,
   DATA-REGION,
   DATA-INIT,
   stub PUBLISH-STUB,
   FRAME-STACKS,
   crash rest INSTALL-CRASH,
   booted JMP,
   STACK-BAD @ >LABEL S\" hb: cannot map guarded VM stack\n" MAP-FAIL-RC FAIL,
   REGION-BAD @ >LABEL S\" hb: cannot map fixed code region\n" MAP-FAIL-RC FAIL,
   DATA-BAD @ >LABEL S\" hb: cannot map fixed data region\n" MAP-FAIL-RC FAIL,
   crash CRASH-HANDLER,
   rest RESTORER,
   stub SIGNAL-STUB,
   booted LBL, ;

;using   \ X64LAYOUT
;using
;using
;package
