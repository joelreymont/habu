\ x86-64-boot-signal.f - the signal stub src/habu/boot-x64.f START, emits and
\ publishes, in the booted harness, cross-built for an x86-64 peer. Each image
\ installs the stub from SIGNAL-ABI:STUB-CELL for SIGUSR1 through
\ SIGACTION-AT,, opens a non-blocking pipe and raises the signal with kill on
\ its own pid, and the peer that runs it must see its status:
\
\    hb-x64-boot-signal           0  FD-PTR-CELL names DATA-VA + FD-CELL; with
\                                    the pipe's write end in the fd word the
\                                    read end yields the four bytes of 10, then
\                                    nothing
\    hb-x64-boot-signal-idle      0  the fd word reads 0 at boot; with fd 0
\                                    a second write end, the image survives
\                                    the signal and the read end yields
\                                    nothing
\    hb-x64-boot-signal-negative 21  the first, expecting the wrong fd-word
\                                    address
\
\ The raise runs with rbp moved to a zeroed window of DATA, so a stub that read
\ the fd word through the thread's DATA register instead of its absolute
\ address would write nothing and fail the read. Without the stub STUB-CELL
\ reads 0, sigaction installs SIG_DFL and the image dies of SIGUSR1 (138 in a
\ shell). The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-SIGNAL
using X64ASM
using X64CODE
using X64RT
using X64BOOT

10 constant SIGUSR1
0 constant NO-FLAGS                     \ a plain `void (int)` handler
$800 constant O-NONBLOCK
-11 constant EAGAIN-RC                  \ read on an empty non-blocking pipe
4 constant SIGNO-BYTES                  \ the one write the stub makes

\ Scratch the harness hands out (PUSH-SCRATCH,): pipe2's two four-byte
\ descriptors, the cell a read fills, and the window rbp names during the
\ raise, whose FD-CELL slot nothing writes.
0 constant FDS
8 constant BUF
$1000 constant WINDOW

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;

\ Load the address of the scratch cell n bytes in into a register.
: SCRATCH>, ( n r64 -- ) {: off:n r:r64 :}
   off X64HARNESS:PUSH-SCRATCH,  r R64>N G-POP ;

\ Check the syscall's answer in rax.
: ANSWER, ( n -- ) {: want:n :}  RAX R64>N G-PUSH  want WANT ;

\ Install the stub the boot published for SIGUSR1, returning through a
\ restorer the image carries behind a jump.
: INSTALL, ( -- )
   LBL LBL {: rest:label past:label :}
   past JMP,
   rest RESTORER,
   past LBL,
   R8 DATA-REG SIGNAL-ABI:STUB-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   SIGUSR1 NO-FLAGS R8 rest SIGACTION-AT,
   0 ANSWER, ;

\ pipe2 with O_NONBLOCK into the FDS scratch, so a read of the empty pipe
\ answers at once.
: PIPE, ( -- )
   FDS RDI SCRATCH>,
   RSI O-NONBLOCK >IMM64 ASM-SINK ENC-MOV-RI64
   NR-PIPE SYS,
   0 ANSWER, ;

\ Make fd 0 the pipe's write end too, with dup3, so a stub that wrote to the
\ zero in the fd word instead of absorbing the signal would fill the pipe.
: STDIN-PIPE, ( -- )
   FDS RCX SCRATCH>,
   RDI R64>N >R32 RCX 4 MEM-OFF ASM-SINK ENC-MOV32-RM
   RSI ZERO-REG,  RDX ZERO-REG,
   NR-DUP2 SYS,
   0 ANSWER, ;

\ Store the pipe's write end in the fd word, through the address the boot
\ published.
: ARM, ( -- )
   FDS RCX SCRATCH>,
   RAX R64>N >R32 RCX 4 MEM-OFF ASM-SINK ENC-MOV32-RM
   RDX DATA-REG SIGNAL-ABI:FD-PTR-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RDX MEM-AT ASM-SINK ENC-MOV-MR ;

\ kill(getpid(), SIGUSR1) with rbp on the window; the handler has run when the
\ syscall returns. r8 keeps DATA across the call.
: RAISE, ( -- )
   NR-GETPID SYS,  RDI RAX ASM-SINK ENC-MOV-RR
   RSI SIGUSR1 >IMM64 ASM-SINK ENC-MOV-RI64
   R8 DATA-REG ASM-SINK ENC-MOV-RR
   WINDOW DATA-REG SCRATCH>,
   NR-KILL SYS,
   DATA-REG R8 ASM-SINK ENC-MOV-RR
   0 ANSWER, ;

\ Read up to a cell from the pipe's read end into BUF and check the count.
: DRAIN, ( n -- ) {: want:n :}
   FDS RCX SCRATCH>,
   RDI R64>N >R32 RCX MEM-AT ASM-SINK ENC-MOV32-RM
   BUF RSI SCRATCH>,
   RDX CELL >IMM64 ASM-SINK ENC-MOV-RI64
   NR-READ SYS,
   want ANSWER, ;

: ARMED-CASE ( -- )
   X64LAYOUT:DATA-VA SIGNAL-ABI:FD-CELL +  SIGNAL-ABI:FD-PTR-CELL
   X64HARNESS:EXPECT-CELL,
   INSTALL,  PIPE,  ARM,  RAISE,
   SIGNO-BYTES DRAIN,
   SIGUSR1 BUF X64HARNESS:EXPECT-SCRATCH,
   EAGAIN-RC DRAIN, ;

: IDLE-CASE ( -- )
   0 SIGNAL-ABI:FD-CELL X64HARNESS:EXPECT-CELL,
   INSTALL,  PIPE,  STDIN-PIPE,  RAISE,
   EAGAIN-RC DRAIN, ;

\ An image: the case, then the stack checks every case ends with.
: BUILD ( [ -- ] bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: ARMED-CASE ;] false s" hb-x64-boot-signal" TMP-PATH BUILD
   [: IDLE-CASE ;] false s" hb-x64-boot-signal-idle" TMP-PATH BUILD
   [: ARMED-CASE ;] true s" hb-x64-boot-signal-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;using
;package

X64K-SIGNAL:RUN
