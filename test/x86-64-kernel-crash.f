\ x86-64-kernel-crash.f - the crash handler src/habu/boot-x64.f START,
\ installs, in the booted harness, cross-built for an x86-64 peer. Each image
\ forks a child that faults with a pipe on fd 2; the parent checks the wait
\ status and every byte the handler wrote but rsp's sixteen digits, and exits 0
\ when each held. Without the handler none exits 0.
require src/lib/string.f
require src/core/exit-status.f
require src/habu/stack-abi.f
require src/habu/fault-record.f
require test/x86-64-boot-harness.f
require src/os/linux/layout.f
require src/lib/fd.f

package X64K-CRASH
using X64ASM
using X64CODE
using X64RT
using LINUX-LAYOUT   \ the guard

\ Scratch the harness hands out (PUSH-SCRATCH,): the pipe's two ends and the
\ expected text's address, then the bytes the parent reads.
0 constant RFD
8 constant WFD
16 constant TEXT-AT
$40 constant GOT
$400 constant GOT-CAP

$80 constant WCOREFLAG                          \ wait4's core bit: the peer's RLIMIT_CORE
102 8 lshift constant NAMED
EXIT-STATUS:MEMORY-FAULT 8 lshift constant THROWN
11 constant SIGSEGV
4 constant SIGILL
$1000 constant UNMAPPED                         \ below vm.mmap_min_addr
5 constant JMP-BYTES                            \ e9 and its rel32
7 constant RIP-STORE-BYTES                      \ 48 89 05 and its disp32
16 constant DIGITS
LINUX-LAYOUT:IMAGE-VA REGION-OFF + constant REGION-VA
REGION-VA DICT-SIZE + constant CP-VA            \ where the boot leaves r15
REGION-VA REGION + CELL - constant LAST-VA      \ the region's last cell
REGION-VA REGION + constant PAST-VA             \ never mapped
$0B0F008948 constant STORE-CODE                 \ 48 89 00 0f 0b: mov [rax], rax; ud2

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: ROW ( str -- ) X64HARNESS:CALL-ROW, ;
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

: WANT-TEXT ( str -- ) {: s:str :}  s STR:LEN 0 ?do  s STR:DATA i + c@ WANT-BYTE  loop ;

: HEX-CHAR ( n -- n ) {: d:n :}  s" 0123456789abcdef" STR:DATA d + c@ ;

\ A value: sixteen lowercase digits, the most significant first.
: VALUE-LINE ( n -- ) {: v:n :}
   DIGITS 0 ?do  v DIGITS 1- i - 4 * rshift 15 and HEX-CHAR WANT-BYTE  loop
   STR:LF WANT-BYTE ;

\ A code cell: its eight bytes in address order, two digits each.
: MEMORY-LINE ( n -- ) {: v:n :}
   CELL 0 ?do
      v i 8 * rshift  dup 4 rshift 15 and HEX-CHAR WANT-BYTE  15 and HEX-CHAR WANT-BYTE
   loop
   STR:LF WANT-BYTE ;

\ rsp's line, whose digits no check reads.
: RSP-LINE ( -- )
   WANT-N @ SKIP-AT !
   DIGITS 0 ?do  [char] ? WANT-BYTE  loop
   STR:LF WANT-BYTE ;

: HEAD$ ( -- str )
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

\ The records, compared whole: no word's code holds the rip of an image's
\ stream, so each has an `at` note.
: RECORD-TEXT ( str -- )  0 WANT-N !  WANT-TEXT ;
: AT-LINE ( n -- )  s"   = at: 0x" WANT-TEXT  VALUE-LINE ;
: RECORD-END ( -- )  STR:LF WANT-BYTE  WANT-N @ SKIP-AT ! ;

: BOUNDS-TEXT ( str n -- ) {: msg:str rip:n :}
   s" error[E-STACK-BOUNDS]: " RECORD-TEXT
   msg WANT-TEXT  STR:LF WANT-BYTE
   rip AT-LINE
   s"   = help: " WANT-TEXT  FAULT-RECORD:CRS-HELP$ WANT-TEXT
   STR:LF WANT-BYTE  RECORD-END ;

: FAULT-TEXT ( n n -- ) {: addr:n rip:n :}
   S\" error[E-MEMORY-FAULT]: a read or write faulted\n" RECORD-TEXT
   s"   = address: 0x" WANT-TEXT  addr VALUE-LINE
   rip AT-LINE  RECORD-END ;

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

\ Store a cell at a region address, through the region's RW alias.
: POKE, ( n n -- ) {: v:n at:n :}
   RCX at >IMM64 ASM-SINK ENC-MOV-RI64  RCX X64KERNEL:WRITE-ALIAS,
   RAX v >IMM64 ASM-SINK ENC-MOV-RI64
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ The code cells a region image writes: each byte from 00 to 17 once, so a byte
\ out of place shows. CELL-B begins with ud2, 0f 0b.
$0706050403020100 constant CELL-A
$0E0D0C0A09080B0F constant CELL-B
$1716151413121110 constant CELL-C

\ A jump to a page that is never mapped, with every register but rsp distinct
\ and rbp not canonical.
: CRASH-CHILD ( -- )
   WILD-RBP REGS,  UNMAPPED JUMP,
   SIGSEGV WILD-RBP UNMAPPED 0 0 0 DUMP-TEXT ;

\ rbp is not canonical: no guard case reads it for a SIGILL.
: ILL-CHILD ( -- )
   WILD-RBP REGS,
   X64HARNESS:POSITION {: rip:n :}
   ASM-SINK ENC-UD2
   SIGILL WILD-RBP rip 0 0 0 DUMP-TEXT ;

\ The region is executable, and read-only through its own address, so the jump
\ runs CELL-B's ud2.
: REGION-CHILD ( -- )
   CELL-A CP-VA CELL - POKE,  CELL-B CP-VA POKE,  CELL-C CP-VA CELL + POKE,
   LINUX-LAYOUT:DATA-VA REGS,  CP-VA JUMP,
   SIGILL LINUX-LAYOUT:DATA-VA CP-VA CELL-A CELL-B CELL-C DUMP-TEXT ;

\ The cell past the region reads 0.
: REGION-END-CHILD ( -- )
   CELL-A LAST-VA CELL - POKE,  CELL-B LAST-VA POKE,
   LINUX-LAYOUT:DATA-VA REGS,  LAST-VA JUMP,
   SIGILL LINUX-LAYOUT:DATA-VA LAST-VA CELL-A CELL-B 0 DUMP-TEXT ;

\ A push with rbx at the data stack's base plus the capacity the boot
\ published: the first byte of its high guard page.
: DATA-CHILD ( -- )
   ENGINE-GPR:X64-DSTACK >R64 {: dsp:r64 :}
   dsp DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   dsp DATA-REG STACK-ABI:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   X64HARNESS:POSITION {: rip:n :}
   0 G-PUSH
   FAULT-RECORD:CRS-DATA$ rip BOUNDS-TEXT ;

\ A store at the return stack's end, its high guard page.
: RETURN-CHILD ( -- )
   RAX DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   X64HARNESS:POSITION {: rip:n :}
   RAX RAX STACK-ABI:RETURN-BYTES MEM-OFF ASM-SINK ENC-MOV-MR
   FAULT-RECORD:CRS-RET$ rip BOUNDS-TEXT ;

\ A store one cell below the loop stack's base, its low guard page.
: LOOP-CHILD ( -- )
   RAX DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   X64HARNESS:POSITION {: rip:n :}
   RAX RAX CELL negate MEM-OFF ASM-SINK ENC-MOV-MR
   FAULT-RECORD:CRS-LOOP$ rip BOUNDS-TEXT ;

\ A call to itself, forever: the return addresses walk rsp down the machine
\ stack until a push lands in the kernel's guard below it, where no signal
\ frame fits; the handler runs on the alternate stack START, registered.
: MACHINE-CHILD ( -- )
   LBL {: self:label :}
   X64HARNESS:POSITION {: rip:n :}
   self LBL,  self CALL,
   FAULT-RECORD:CRS-MACHINE$ rip BOUNDS-TEXT ;

\ A jump to the first byte past the region, which nothing maps: rip one past
\ the span whose rbp the guard cases read, so rbp, not canonical, is never
\ read. The cell before rip is the region's last, never written.
: PAST-CHILD ( -- )
   WILD-RBP REGS,  PAST-VA JUMP,
   SIGSEGV WILD-RBP PAST-VA 0 0 0 DUMP-TEXT ;

\ The loop stack's guard hit from code in the region: the cell at CP holds
\ mov [rax], rax and then ud2, which dumps should the store land, and runs with
\ rax one cell below the loop stack's base.
: REGION-GUARD-CHILD ( -- )
   STORE-CODE CP-VA POKE,
   RAX DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX CELL negate >IMM32 ASM-SINK ENC-ADD-RI32
   CP-VA JUMP,
   FAULT-RECORD:CRS-LOOP$ CP-VA BOUNDS-TEXT ;

\ The loop stack's base cell zeroed and a store at LOOP-BYTES, where its high
\ guard page would stand were zero a base. A zero base is no stack: the memory
\ fault, thrown.
: ZERO-BASE-CHILD ( -- )
   0 STACK-ABI:LOOP-BASE-CELL X64HARNESS:CELL!,
   LINUX-LAYOUT:DATA-VA REGS,
   X64HARNESS:POSITION {: rip:n :}
   RAX  STACK-ABI:LOOP-BYTES rip RIP-STORE-BYTES + -  MEM-RIP ASM-SINK ENC-MOV-MR
   STACK-ABI:LOOP-BYTES rip FAULT-TEXT ;

\ ---- the image ---------------------------------------------------------------
\ After fork: the child, whose answer is 0, takes the pipe's write end as fd 2
\ and runs what the quotation emits; the parent goes on with the pid.
: CHILD, ( [ -- ] -- )
   LBL {: parent:label :}
   0 G-POP  RAX RAX ASM-SINK ENC-TEST-RR  C-NE parent JCC,
   WFD RECALL,  FD:STDERR FD>N N,  s" dup2" ROW  0 G-POP
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
\ EOF, then check the status without its core bit (a dumped child dies of its
\ signal and dumps core when the peer allows), the count, the EOF and the
\ bytes on each side of the sixteen at SKIP-AT.
: PARENT, ( n -- ) {: status:n :}
   WFD RECALL,  s" close" ROW
   s" wait-status" ROW
   0 G-POP  RAX WCOREFLAG invert >IMM32 ASM-SINK ENC-AND-RI32  0 G-PUSH
   status WANT
   RFD RECALL,  GOT AT,  GOT-CAP N,  s" read" ROW  WANT-N @ WANT
   RFD RECALL,  GOT AT,  GOT-CAP N,  s" read" ROW  0 WANT
   WANT-BUF WANT-N @ >str X64HARNESS:PUSH-TEXT,  0 G-POP  TEXT-AT KEEP,
   0 SKIP-AT @ SAME,  0 WANT
   SKIP-AT @ WANT-N @ < if
      SKIP-AT @ DIGITS +  WANT-N @ over -  SAME,  0 WANT
   then ;

\ An image: the pipe, the fork, the child the quotation emits and the
\ parent's checks, then the stack checks every image ends with.
: IMAGE ( [ -- ] n bool str -- ) {: status:n negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   s" pipe" ROW  0 G-POP  WFD KEEP,  RFD KEEP,
   s" fork" ROW  CHILD,
   status PARENT,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: CRASH-CHILD ;] SIGSEGV false s" hb-x64-kernel-crash" TMP-PATH IMAGE
   [: CRASH-CHILD ;] SIGSEGV true s" hb-x64-kernel-crash-negative" TMP-PATH IMAGE
   [: REGION-CHILD ;] SIGILL false s" hb-x64-kernel-crash-region" TMP-PATH IMAGE
   [: REGION-END-CHILD ;] SIGILL false s" hb-x64-kernel-crash-region-end" TMP-PATH IMAGE
   [: ILL-CHILD ;] SIGILL false s" hb-x64-kernel-crash-ill" TMP-PATH IMAGE
   [: DATA-CHILD ;] NAMED false s" hb-x64-kernel-crash-data" TMP-PATH IMAGE
   [: RETURN-CHILD ;] NAMED false s" hb-x64-kernel-crash-return" TMP-PATH IMAGE
   [: LOOP-CHILD ;] NAMED false s" hb-x64-kernel-crash-loop" TMP-PATH IMAGE
   [: MACHINE-CHILD ;] NAMED false s" hb-x64-kernel-crash-machine" TMP-PATH IMAGE
   [: PAST-CHILD ;] SIGSEGV false s" hb-x64-kernel-crash-past" TMP-PATH IMAGE
   [: REGION-GUARD-CHILD ;] NAMED false s" hb-x64-kernel-crash-region-guard" TMP-PATH IMAGE
   [: ZERO-BASE-CHILD ;] THROWN false s" hb-x64-kernel-crash-zero-base" TMP-PATH IMAGE
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ LINUX-LAYOUT
;using
;using
;using
;package

X64K-CRASH:RUN
