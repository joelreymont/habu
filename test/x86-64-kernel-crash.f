\ x86-64-kernel-crash.f - the crash handler src/habu/boot-x64.f START,
\ installs, in the booted harness, cross-built for an x86-64 peer. Each image
\ forks. The child moves a pipe's write end onto fd 2 and faults. The parent
\ closes its own write end, waits, reads the pipe to EOF and checks the wait
\ status, the byte count and every byte the handler wrote but the sixteen
\ digits of rsp; it exits 0 when each held. The peer that runs them must see:
\
\    hb-x64-kernel-crash             0  every register but rsp distinct, rbp
\                                       not canonical, a jump to $1000, which
\                                       is never mapped: the dump and exit 134,
\                                       its three code lines 0
\    hb-x64-kernel-crash-negative   21  the same, expecting the wrong status
\    hb-x64-kernel-crash-region      0  rbp DATA and a jump to CP in the
\                                       read-write code region: rip CP and the
\                                       24 bytes the child wrote around it
\    hb-x64-kernel-crash-region-end  0  the same at the region's last cell:
\                                       the cell past the region reads 0
\    hb-x64-kernel-crash-ill         0  ud2 with rbp not canonical: SIGILL,
\                                       which no guard case reads rbp for
\    hb-x64-kernel-crash-data        0  a push past the data stack's capacity:
\                                       exit 102 and the line naming it
\    hb-x64-kernel-crash-return      0  a store at the return stack's end
\    hb-x64-kernel-crash-loop        0  a store below the loop stack's base
\    hb-x64-kernel-crash-machine     0  a call to itself, forever: exit 102
\                                       and the machine stack's line
\    hb-x64-kernel-crash-past       0  rbp not canonical and a jump to the
\                                       first byte past the region, text base
\                                       + HABU-SPAN, never mapped: the dump,
\                                       its three code lines 0
\    hb-x64-kernel-crash-region-guard
\                                    0  code at CP, made read-execute, storing
\                                       below the loop stack's base: 102 and
\                                       the loop line
\    hb-x64-kernel-crash-zero-base   0  the loop stack's base cell zeroed and
\                                       a store at LOOP-BYTES: no stack, so
\                                       the dump
\
\ Without the handler each child dies of its signal and every image exits 21.
\ The host checks each image's ELF header; running them is the peer's.
require lib/string.f
require src/core/engine-error.f
require src/habu/stack-abi.f
require test/x86-64-boot-harness.f
require src/os/linux-x86-64/target-layout.f

package X64K-CRASH
using X64ASM
using X64CODE
using X64RT
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

\ Scratch the harness hands out (PUSH-SCRATCH,): the pipe's two ends and the
\ expected text's address, then the bytes the parent reads.
0 constant RFD
8 constant WFD
16 constant TEXT-AT
$40 constant GOT
$400 constant GOT-CAP

2 constant STDERR
134 8 lshift constant DUMPED                    \ wait4's status for exit 134
ENGINE-ERROR:STACK-BOUNDS 8 lshift constant NAMED
11 constant SIGSEGV
4 constant SIGILL
$1000 constant UNMAPPED                         \ below vm.mmap_min_addr
5 constant JMP-BYTES                            \ e9 and its rel32
7 constant RIP-STORE-BYTES                      \ 48 89 05 and its disp32
16 constant DIGITS
VMBASE REGION-OFF + constant REGION-VA
REGION-VA DICT-SIZE + constant CP-VA            \ where the boot leaves r15
REGION-VA REGION + CELL - constant LAST-VA      \ the region's last cell
REGION-VA REGION + constant PAST-VA             \ text base + HABU-SPAN
$0B0F008948 constant STORE-CODE                 \ 48 89 00 0f 0b: mov [rax], rax; ud2
5 constant PROT-RX                              \ PROT_READ|PROT_EXEC

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

\ ( x -- ) into the scratch cell at an offset, and ( -- x ) back out of it.
: KEEP, ( n -- ) AT,  1 G-POP  0 G-POP  RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;
: RECALL, ( n -- ) AT,  0 G-POP  RAX RAX MEM-AT ASM-SINK ENC-MOV-RM  0 G-PUSH ;

\ ---- the expected text -------------------------------------------------------
\ What the child's fd 2 must carry, built while the child is emitted. SKIP-AT
\ is where the sixteen bytes no check reads start, rsp's digits, or the text's
\ end when it has none.
512 constant WANT-CAP
WANT-CAP BUFFER: WANT-BUF
variable WANT-N
variable SKIP-AT

: WANT-BYTE ( n -- ) {: b:n :}
   WANT-N @ WANT-CAP >= if E-BUF-BOUNDS throw then
   b WANT-BUF WANT-N @ + c!
   WANT-N @ 1+ WANT-N ! ;

: WANT-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}  u 0 ?do  a i + c@ WANT-BYTE  loop ;

: HEX-CHAR ( n -- n ) {: d:n :}  s" 0123456789abcdef" drop d + c@ ;

\ A value: sixteen lowercase digits, the most significant first.
: VALUE-LINE ( n -- ) {: v:n :}
   DIGITS 0 ?do  v DIGITS 1- i - 4 * rshift 15 and HEX-CHAR WANT-BYTE  loop
   STR-LF WANT-BYTE ;

\ A code cell: its eight bytes in address order, two digits each.
: MEMORY-LINE ( n -- ) {: v:n :}
   CELL 0 ?do
      v i 8 * rshift  dup 4 rshift 15 and HEX-CHAR WANT-BYTE  15 and HEX-CHAR WANT-BYTE
   loop
   STR-LF WANT-BYTE ;

\ rsp's line, whose digits no check reads.
: RSP-LINE ( -- )
   WANT-N @ SKIP-AT !
   DIGITS 0 ?do  [char] ? WANT-BYTE  loop
   STR-LF WANT-BYTE ;

: HEAD$ ( -- ptr u8 n )
   S\" habu-crash regs [sig rax rcx rdx rbx rsp rbp rsi rdi r8..r15 rip] code [rip-8 rip rip+8], hex one-per-line:\n" ;

\ The value a dump image gives register n, rbp the one given: the others hold
\ $0123456789abcdef turned left n nibbles. Its nibbles all differ, so no two
\ registers agree and a digit out of place changes a line, and none is
\ canonical: the top sixteen bits are never all zero or all one.
$0123456789ABCDEF constant NIBBLES

: TURNED ( n -- n ) {: k:n :}
   NIBBLES  k 0 ?do  dup 4 lshift  swap 60 rshift  or  loop ;

: REG-VALUE ( n n -- n ) {: bp:n r:n :}
   r RBP R64>N = if bp exit then
   r TURNED ;

RBP R64>N TURNED constant WILD-RBP

\ The dump for a signal, rbp's value and the rip, then the three code cells
\ from rip - 8.
: DUMP-TEXT ( n n n n n n -- ) {: sig:n bp:n rip:n lo:n mid:n hi:n :}
   0 WANT-N !
   HEAD$ WANT-TEXT
   sig VALUE-LINE
   16 0 ?do
      i RSP R64>N = if RSP-LINE else bp i REG-VALUE VALUE-LINE then
   loop
   rip VALUE-LINE
   lo MEMORY-LINE  mid MEMORY-LINE  hi MEMORY-LINE ;

\ A guard hit's one line, compared whole.
: LINE-TEXT ( ptr u8 n -- )
   0 WANT-N !  WANT-TEXT  WANT-N @ SKIP-AT ! ;

\ ---- the children ------------------------------------------------------------
\ Each emits what a child does once fd 2 is the pipe, which never comes back,
\ and builds the text its handler must write.

\ Give every register but rsp its value, rbp the one given.
: REGS, ( n -- ) {: bp:n :}
   16 0 ?do
      i RSP R64>N <> if  i >R64  bp i REG-VALUE >IMM64 ASM-SINK ENC-MOV-RI64  then
   loop ;

\ jmp rel32 to an absolute address.
: JUMP, ( n -- ) {: to:n :}
   to  X64HARNESS:POSITION JMP-BYTES +  -  >REL ASM-SINK ENC-JMP-REL32 ;

\ Store a cell at an absolute address.
: POKE, ( n n -- ) {: v:n at:n :}
   RCX at >IMM64 ASM-SINK ENC-MOV-RI64
   RAX v >IMM64 ASM-SINK ENC-MOV-RI64
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ The code cells a region image writes: bytes 00 to 17 in address order, so
\ each line counts up and a byte out of place shows.
$0706050403020100 constant CELL-A
$0F0E0D0C0B0A0908 constant CELL-B
$1716151413121110 constant CELL-C

: CRASH-CHILD ( -- )
   WILD-RBP REGS,  UNMAPPED JUMP,
   SIGSEGV WILD-RBP UNMAPPED 0 0 0 DUMP-TEXT ;

: ILL-CHILD ( -- )
   WILD-RBP REGS,
   X64HARNESS:POSITION {: rip:n :}
   ASM-SINK ENC-UD2
   SIGILL WILD-RBP rip 0 0 0 DUMP-TEXT ;

\ The region is mapped read-write, never executable, so the jump faults with
\ rip at its target.
: REGION-CHILD ( -- )
   CELL-A CP-VA CELL - POKE,  CELL-B CP-VA POKE,  CELL-C CP-VA CELL + POKE,
   X64LAYOUT:DATA-VA REGS,  CP-VA JUMP,
   SIGSEGV X64LAYOUT:DATA-VA CP-VA CELL-A CELL-B CELL-C DUMP-TEXT ;

: REGION-END-CHILD ( -- )
   CELL-A LAST-VA CELL - POKE,  CELL-B LAST-VA POKE,
   X64LAYOUT:DATA-VA REGS,  LAST-VA JUMP,
   SIGSEGV X64LAYOUT:DATA-VA LAST-VA CELL-A CELL-B 0 DUMP-TEXT ;

\ A push with r12 at the data stack's base plus the capacity the boot
\ published: the first byte of its high guard page.
: DATA-CHILD ( -- )
   ENGINE-GPR:X64-DSTACK >R64 {: dsp:r64 :}
   dsp DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   dsp DATA-REG STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   0 G-PUSH
   S\" hb: stack bounds exceeded (data)\n" LINE-TEXT ;

\ A store at the return stack's end, its high guard page.
: RETURN-CHILD ( -- )
   RAX DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX STACK-ABI:RETURN-BYTES MEM-OFF ASM-SINK ENC-MOV-MR
   S\" hb: stack bounds exceeded (return)\n" LINE-TEXT ;

\ A store one cell below the loop stack's base, its low guard page.
: LOOP-CHILD ( -- )
   RAX DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX CELL negate MEM-OFF ASM-SINK ENC-MOV-MR
   S\" hb: stack bounds exceeded (loop)\n" LINE-TEXT ;

\ A call to itself, forever: the return addresses walk rsp down the machine
\ stack until a push lands in the kernel's guard below it, where no signal
\ frame fits; the handler runs on the alternate stack START, registered.
: MACHINE-CHILD ( -- )
   LBL {: self:label :}
   self LBL,  self CALL,
   S\" hb: stack bounds exceeded (machine)\n" LINE-TEXT ;

\ A jump to the first byte past the region, which nothing maps: rip one past
\ the span whose rbp the guard cases read, so rbp, not canonical, is never
\ read. The cell before rip is the region's last, never written.
: PAST-CHILD ( -- )
   WILD-RBP REGS,  PAST-VA JUMP,
   SIGSEGV WILD-RBP PAST-VA 0 0 0 DUMP-TEXT ;

\ The loop stack's guard hit from code in the region: the cell at CP holds
\ mov [rax], rax and then ud2, which dumps should the store land, made
\ read-execute, and runs with rax one cell below the loop stack's base.
\ mprotect widens the cell to its page; should it fail, the fetch at CP
\ faults instead and the dump fails the status check.
: REGION-GUARD-CHILD ( -- )
   STORE-CODE CP-VA POKE,
   RDI CP-VA >IMM64 ASM-SINK ENC-MOV-RI64
   RSI CELL >IMM32 ASM-SINK ENC-MOV-RI32
   RDX PROT-RX >IMM32 ASM-SINK ENC-MOV-RI32
   NR-MPROTECT SYS,
   RAX DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX CELL negate >IMM32 ASM-SINK ENC-ADD-RI32
   CP-VA JUMP,
   S\" hb: stack bounds exceeded (loop)\n" LINE-TEXT ;

\ The loop stack's base cell zeroed and a store at LOOP-BYTES, where its high
\ guard page would stand were zero a base. A zero base is no stack: the dump.
: ZERO-BASE-CHILD ( -- )
   0 STACK-ABI:LOOP-BASE-CELL X64HARNESS:CELL!,
   X64LAYOUT:DATA-VA REGS,
   X64HARNESS:POSITION {: rip:n :}
   RAX  STACK-ABI:LOOP-BYTES rip RIP-STORE-BYTES + -  MEM-RIP ASM-SINK ENC-MOV-MR
   SIGSEGV X64LAYOUT:DATA-VA rip 0 0 0 DUMP-TEXT ;

\ ---- the image ---------------------------------------------------------------
\ After fork: the child, whose answer is 0, takes the pipe's write end as fd 2
\ and runs what the quotation emits; the parent goes on with the pid.
: CHILD, ( [ -- ] -- )
   LBL {: parent:label :}
   0 G-POP  RAX RAX ASM-SINK ENC-TEST-RR  C-NE parent JCC,
   WFD RECALL,  STDERR N,  s" dup2" ROW  0 G-POP
   execute
   parent LBL,
   0 G-PUSH ;

\ ( -- diff ): the OR of the XORs of the n bytes from an offset in the
\ expected text and in what the parent read, 0 when they agree.
: SAME, ( n n -- ) {: off:n len:n :}
   LBL LBL {: top:label done:label :}
   TEXT-AT RECALL,  0 G-POP  RDI RAX ASM-SINK ENC-MOV-RR
   RDI off >IMM32 ASM-SINK ENC-ADD-RI32
   GOT off + AT,  0 G-POP  RSI RAX ASM-SINK ENC-MOV-RR
   RCX len >IMM32 ASM-SINK ENC-MOV-RI32
   RAX ZERO-REG,
   top LBL,
      RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
      RDX RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
      R8 RDI MEM-AT ASM-SINK ENC-MOVZX-8-RM
      RDX R8 ASM-SINK ENC-XOR-RR  RAX RDX ASM-SINK ENC-OR-RR
      RSI ASM-SINK ENC-INC  RDI ASM-SINK ENC-INC  RCX ASM-SINK ENC-DEC
      top JMP,
   done LBL,
   0 G-PUSH ;

\ The parent, holding the pid: close the write end, wait, read the pipe to
\ EOF, then check the status, the count, the EOF and the bytes on each side of
\ the sixteen at SKIP-AT.
: PARENT, ( n -- ) {: status:n :}
   WFD RECALL,  s" close" ROW
   s" wait-status" ROW  status WANT
   RFD RECALL,  GOT AT,  GOT-CAP N,  s" read" ROW  WANT-N @ WANT
   RFD RECALL,  GOT AT,  GOT-CAP N,  s" read" ROW  0 WANT
   WANT-BUF WANT-N @ X64HARNESS:PUSH-TEXT,  0 G-POP  TEXT-AT KEEP,
   0 SKIP-AT @ SAME,  0 WANT
   SKIP-AT @ WANT-N @ < if
      SKIP-AT @ DIGITS +  WANT-N @ over -  SAME,  0 WANT
   then ;

\ An image: the pipe, the fork, the child the quotation emits and the
\ parent's checks, then the stack checks every image ends with.
: IMAGE ( [ -- ] n bool ptr u8 n -- ) {: status:n negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   s" pipe" ROW  0 G-POP  WFD KEEP,  RFD KEEP,
   s" fork" ROW  CHILD,
   status PARENT,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: CRASH-CHILD ;] DUMPED false s" hb-x64-kernel-crash" TMP-PATH IMAGE
   [: CRASH-CHILD ;] DUMPED true s" hb-x64-kernel-crash-negative" TMP-PATH IMAGE
   [: REGION-CHILD ;] DUMPED false s" hb-x64-kernel-crash-region" TMP-PATH IMAGE
   [: REGION-END-CHILD ;] DUMPED false s" hb-x64-kernel-crash-region-end" TMP-PATH IMAGE
   [: ILL-CHILD ;] DUMPED false s" hb-x64-kernel-crash-ill" TMP-PATH IMAGE
   [: DATA-CHILD ;] NAMED false s" hb-x64-kernel-crash-data" TMP-PATH IMAGE
   [: RETURN-CHILD ;] NAMED false s" hb-x64-kernel-crash-return" TMP-PATH IMAGE
   [: LOOP-CHILD ;] NAMED false s" hb-x64-kernel-crash-loop" TMP-PATH IMAGE
   [: MACHINE-CHILD ;] NAMED false s" hb-x64-kernel-crash-machine" TMP-PATH IMAGE
   [: PAST-CHILD ;] DUMPED false s" hb-x64-kernel-crash-past" TMP-PATH IMAGE
   [: REGION-GUARD-CHILD ;] NAMED false s" hb-x64-kernel-crash-region-guard" TMP-PATH IMAGE
   [: ZERO-BASE-CHILD ;] DUMPED false s" hb-x64-kernel-crash-zero-base" TMP-PATH IMAGE
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ X64LAYOUT
;using
;using
;using
;package

X64K-CRASH:RUN
