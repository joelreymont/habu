\ x86-64-boot-signal.f - the SIGUSR1 stub src/habu/boot-x64.f START emits and
\ publishes, cross-built for an x86-64 peer. Each image installs it from
\ SIGNAL-ABI:STUB-CELL through SIGACTION-AT,, marks it the disposition in the
\ stubbed word and raises the signal on its own pid with rbp on a zeroed DATA
\ window: a stub reading its words through the DATA register instead of their
\ absolute addresses sends the signal on and the armed image dies of it.
require test/x86-64-boot-harness.f
require src/os/linux/layout.f
require src/arch/x86-64/rt.f
require src/habu/boot-x64.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-SIGNAL

10 constant SIGUSR1
$80000000 constant SA-RESETHAND          \ SIG_DFL goes back as the stub enters
$800 constant O-NONBLOCK
-11 constant EAGAIN-RC                  \ read on an empty non-blocking pipe
4 constant SIGNO-BYTES                  \ the one write the stub makes

\ Scratch the harness hands out (PUSH-SCRATCH,): pipe2's two four-byte
\ descriptors, the cell a read fills, and the window rbp names during the
\ raise, whose FD-CELL slot nothing writes.
0 constant FDS
8 constant BUF
$1000 constant WINDOW

: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;

: SCRATCH>, ( n X64ASM:r64 -- ) {: off:n r:X64ASM:r64 :}
   off X64HARNESS:PUSH-SCRATCH,  r X64ASM:R64>N X64RT:G-POP ;

: ANSWER, ( n -- ) {: want:n :}  X64ASM:RAX X64ASM:R64>N X64RT:G-PUSH  want WANT ;

\ The restorer the handler returns through sits behind a jump in the image.
: INSTALL, ( -- )
   X64CODE:LBL X64CODE:LBL {: rest:label past:label :}
   past X64CODE:JMP,
   rest X64BOOT:RESTORER,
   past X64CODE:LBL,
   X64ASM:R8 DATA-REG SIGNAL-ABI:STUB-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   SIGUSR1 SA-RESETHAND X64ASM:R8 rest X64BOOT:SIGACTION-AT,
   0 ANSWER, ;

\ O_NONBLOCK: a read of the empty pipe answers at once.
: PIPE, ( -- )
   FDS X64ASM:RDI SCRATCH>,
   X64ASM:RSI O-NONBLOCK X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   NR-PIPE SYS,
   0 ANSWER, ;

: ARM, ( -- )
   FDS X64ASM:RCX SCRATCH>,
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:RCX 4 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RDX DATA-REG SIGNAL-ABI:FD-PTR-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ Set bit SIGUSR1-1 of the stubbed word: the stub is still the disposition.
: STUBBED, ( -- )
   X64ASM:RDX DATA-REG SIGNAL-ABI:FD-PTR-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX 1 SIGUSR1 1- lshift X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:RAX X64ASM:RDX SIGNAL-ABI:STUBBED-CELL SIGNAL-ABI:FD-CELL - X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ The handler has run when kill returns; r8 keeps DATA across the call.
: RAISE, ( -- )
   NR-GETPID SYS,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI SIGUSR1 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:R8 DATA-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   WINDOW DATA-REG SCRATCH>,
   NR-KILL SYS,
   DATA-REG X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   0 ANSWER, ;

: DRAIN, ( n -- ) {: want:n :}
   FDS X64ASM:RCX SCRATCH>,
   X64ASM:RDI X64ASM:R64>N X64ASM:>R32 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   BUF X64ASM:RSI SCRATCH>,
   X64ASM:RDX CELL X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   NR-READ SYS,
   want ANSWER, ;

: ARMED-CASE ( -- )
   LINUX-LAYOUT:DATA-VA SIGNAL-ABI:FD-CELL +  SIGNAL-ABI:FD-PTR-CELL
   X64HARNESS:EXPECT-CELL,
   INSTALL,  PIPE,  ARM,  STUBBED,  RAISE,
   SIGNO-BYTES DRAIN,
   SIGUSR1 BUF X64HARNESS:EXPECT-SCRATCH,
   EAGAIN-RC DRAIN, ;

: IDLE-CASE ( -- )
   SIGUSR1 X64HARNESS:EXPECT-SIGNAL
   0 SIGNAL-ABI:FD-CELL X64HARNESS:EXPECT-CELL,
   INSTALL,  STUBBED,  RAISE, ;

: BUILD ( [ -- ] bool str -- ) {: negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

\ The armed image reads the signal number from the pipe the fd word names. The
\ idle image's fd word is 0, so the stub sends the signal on and the image dies
\ of it; a stub that wrote to fd 0 instead would leave it alive. The negative
\ image is the armed one failing its first check.
: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: ARMED-CASE ;] false s" hb-x64-boot-signal" TMP-PATH BUILD
   [: IDLE-CASE ;] false s" hb-x64-boot-signal-idle" TMP-PATH BUILD
   [: ARMED-CASE ;] true s" hb-x64-boot-signal-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-SIGNAL:RUN
