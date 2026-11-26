\ x86-64-boot-harness.f - an x86-64 image that boots as the engine does, carries
\ the whole kernel and runs one case against the DATA the boot mapped:
\ BOOT-OPEN, stages the boot and the kernel, the case follows, and BOOT-CLOSE,
\ exits 0 and writes the image.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/vector.f
\ boot-x64.f and kernel-x64.f load the x86-64 seam globally, so they come
\ before the peer harness, which would otherwise load it into its own private
\ wordlist.
require test/x86-64-peer-harness.f
require src/habu/primitive-registry.f
require src/os/linux-x86-64/sys.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64HARNESS

\ Scratch cells past the heap floor, where nothing a booted image runs
\ allocates.
DATA-START $10000 + constant BOOT-SCRATCH-OFF
variable ENTRY-CELL
variable BOOT-END-CELL

: ENTRY-LBL ( -- label ) ENTRY-CELL @ >LABEL ;
: BOOT-END-LBL ( -- label ) BOOT-END-CELL @ >LABEL ;
: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: DSP ( -- X64ASM:r64 ) ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;

public

: SCRATCH-OFF ( -- n ) BOOT-SCRATCH-OFF ;

\ Checks exit FIRST-CASE upward in staging order, as the peer harness numbers
\ its checks; a negative image (BOOT-OPEN, given true) expects a wrong answer
\ from its first check.
\ Fresh labels and a fresh registry per image: labels die with the stream the
\ last image linked; registry rows do not.
: BOOT-OPEN, ( bool -- ) {: negative:bool :}
   X64CODE:ASM-RESET
   X64CODE:LBL EXIT-CELL !  X64CODE:LBL ENTRY-CELL !  X64CODE:LBL BOOT-END-CELL !
   FIRST-CASE CASE-NEXT !
   negative if FIRST-CASE else 0 then dup WRONG-AT !  EXPECT-EXIT
   X64KERNEL:FLOORREC-NEW BOOT-END-LBL true X64BOOT:START,
   ENTRY-LBL X64CODE:JMP,
   ENGINE-PRIMS:RESET
   X64KERNEL:KERNEL,
   X64KERNEL:SCOPE-OBJECT,
   ENTRY-LBL X64CODE:LBL, ;

: PUSH, ( n -- ) {: v:n :}  X64ASM:RAX v IMM  0 X64RT:G-PUSH ;

\ The string's bytes sit in the text behind a jump.
: PUSH-TEXT, ( str -- ) {: s:str :}
   X64CODE:LBL X64CODE:LBL {: text:label past:label :}
   past X64CODE:JMP,
   text X64CODE:LBL,
   s X64CODE:ASM-SINK VEC:APPEND
   past X64CODE:LBL,
   X64ASM:RAX text X64CODE:MOVABS,  0 X64RT:G-PUSH
   s STR:LEN PUSH, ;

: PUSH-SCRATCH, ( n -- ) {: off:n :}
   X64ASM:RAX DATA-REG BOOT-SCRATCH-OFF off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   0 X64RT:G-PUSH ;

: CELL!, ( n n -- ) {: v:n off:n :}
   X64ASM:RAX v IMM
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: CALL-ROW, ( str -- ) X64KERNEL:ENTRY-LABEL X64CODE:CALL, ;

: EXPECT-POP, ( n -- ) {: want:n :}  0 X64RT:G-POP  want EXPECT, ;

: EXPECT-DEPTH, ( n -- ) {: want:n :}
   X64ASM:RAX DSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX DATA-REG STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
   want CELL * EXPECT, ;

\ The boot leaves rsp below argc, one cell under the argument vector it
\ published, by the signal band boot-x64.f SIGNAL-STACK, carves off the machine
\ stack for the life of the thread.
: EXPECT-BALANCED, ( -- )
   X64ASM:RAX DATA-REG ARGV-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   CELL STACK-ABI:SIGNAL-BYTES + EXPECT, ;

: EXPECT-SCRATCH, ( n n -- ) {: want:n off:n :}
   X64ASM:RAX DATA-REG BOOT-SCRATCH-OFF off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   want EXPECT, ;

\ A row that ends the process ends the image with its own status, which the
\ test states.
: BOOT-CLOSE, ( str -- )
   X64ASM:RDI ZERO-REG,
   EXIT,
   BOOT-END-LBL X64CODE:LBL,
   WRITE ;

private

: DBASE-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DBASE X64ASM:>R64 ;
: NDICT-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-NDICT X64ASM:>R64 ;

\ Up to eight of the name's bytes from `at`, little-endian, zero past its end.
: NAME-CELL ( str n -- n ) {: s:str at:n :}
   0  CELL 0 ?do
      at i + s STR:LEN < if  s STR:DATA at i + + c@  i 8 * lshift  or  then
   loop ;

\ Store n into the cell at an offset in rax's record, through rcx.
: ROW!, ( n n -- ) {: v:n off:n :}
   X64ASM:RCX v IMM
   X64ASM:RCX X64ASM:RAX off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: NAME-TEXT, ( str -- label ) {: s:str :}
   X64CODE:LBL X64CODE:LBL {: text:label past:label :}
   past X64CODE:JMP,
   text X64CODE:LBL,
   s X64CODE:ASM-SINK VEC:APPEND
   past X64CODE:LBL,
   text ;

public

\ Seed record r14 of the dictionary at r13 + r14 * DREC as X64KERNEL's record
\ cells name them: the flags or'd with the name's length, the name inline when
\ it fits DNAME-INL bytes else a pointer with DNAME-EXT set, and the wid. The
\ code cell holds the ordinal (index plus one), so a found record's code is
\ never 0, the answer for an absent name. Stores use the booted RW alias.
\ Clobbers rax and rcx.
: RECORD, ( str n n -- ) {: s:str wid:n flags:n :}
   X64ASM:RAX NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX X64KERNEL:WRITE-ALIAS,
   X64ASM:RCX NDICT-REG 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RAX X64KERNEL:REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   s STR:LEN DNAME-INL > if
      flags s STR:LEN or DNAME-EXT or X64KERNEL:REC-FLAGS ROW!,
      X64ASM:RCX s NAME-TEXT, X64CODE:MOVABS,
      X64ASM:RCX X64ASM:RAX X64KERNEL:REC-NAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   else
      flags s STR:LEN or X64KERNEL:REC-FLAGS ROW!,
      s 0 NAME-CELL X64KERNEL:REC-NAME ROW!,
      s CELL NAME-CELL X64KERNEL:REC-NAME CELL + ROW!,
   then
   wid X64KERNEL:REC-WID ROW!,
   NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-INC ;

: EXPECT-ROW, ( n -- ) {: ix:n :}
   0 X64RT:G-POP
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   ix DREC * EXPECT, ;

;package

\ The engine-state cases' words: addresses the boot fixes only at run time
\ (DATA, the code region).
package X64HARNESS

public

\ Push the address n bytes into the code region: an xt the hook rows admit.
: PUSH-REGION, ( n -- ) {: off:n :}
   X64ASM:RAX DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   0 X64RT:G-PUSH ;

: EXPECT-POP-DATA, ( n -- ) {: want:n :}
   0 X64RT:G-POP  X64ASM:RAX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  want EXPECT, ;

: EXPECT-POP-REGION, ( n -- ) {: want:n :}
   0 X64RT:G-POP  X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  want EXPECT, ;

: EXPECT-CELL, ( n n -- ) {: want:n off:n :}
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   want EXPECT, ;

: LABEL-CELL!, ( label n -- ) {: at:label off:n :}
   X64ASM:RAX at X64CODE:MOVABS,
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: ROUTINE, ( [ -- ] -- label )
   X64CODE:LBL X64CODE:LBL {: entry:label past:label :}
   past X64CODE:JMP,
   entry X64CODE:LBL,
   execute
   X64CODE:ASM-SINK X64ASM:ENC-RET
   past X64CODE:LBL,
   entry ;

;package

\ The dictionary cases' words: DATA addresses the boot fixes only at run time.
package X64HARNESS

public

\ DATA spans 2^38 bytes, past a disp32, so the offset is a 64-bit immediate
\ added to DATA.
: PUSH-DATA, ( n -- ) {: off:n :}
   X64ASM:RAX off IMM  X64ASM:RAX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   0 X64RT:G-PUSH ;

: DATA-ADDR!, ( n n -- ) {: at:n off:n :}
   X64ASM:RAX DATA-REG at X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: REGION-ADDR!, ( n n -- ) {: at:n off:n :}
   X64ASM:RAX DBASE-REG at X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: REGION!, ( n n -- ) {: v:n off:n :}
   X64ASM:RAX v IMM
   X64ASM:RCX DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64KERNEL:WRITE-ALIAS,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: TEXT!, ( str n -- ) {: s:str off:n :}
   s STR:LEN 0 ?do  s i NAME-CELL off i + CELL!,  CELL +loop ;

: EXPECT-RECORD, ( n n n -- ) {: want:n ix:n off:n :}
   X64ASM:RAX DBASE-REG ix DREC * off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   want EXPECT, ;

\ Force the selected record's pages to the protection the fixture requests.
: PROT-RECORD, ( n n -- ) {: ix:n prot:n :}
   X64ASM:RDI DBASE-REG ix DREC * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI STACK-ABI:PAGE-BYTES negate X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32
   X64ASM:RSI STACK-ABI:PAGE-BYTES 2 * IMM
   X64ASM:RDX prot IMM
   NR-MPROTECT SYS, ;

;package

\ An address in the image's text, which the image fixes only at link time.
package X64HARNESS

public

: PUSH-LABEL, ( label n -- ) {: at:label off:n :}
   X64ASM:RAX at X64CODE:MOVABS,
   X64ASM:RAX X64ASM:RAX off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   0 X64RT:G-PUSH ;

;package

\ The pure rows' check: their case sets outnumber the statuses the checks
\ number from FIRST-CASE, so each check names itself in text instead.
package X64HARNESS

public

\ Compare rax with rcx; on a mismatch the kernel's `die` row writes the text and
\ LF on fd 2 and exits FIRST-CASE. Staged before any numbered check, so a
\ negative image expects one less, as EXPECT-RCX, does.
: CHECK-RCX, ( str -- ) {: s:str :}
   X64CODE:LBL {: ok:label :}
   WRONG-AT @ CASE-NEXT @ = if
      X64ASM:RCX 1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
      0 WRONG-AT !
   then
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E ok X64CODE:JCC,
   s PUSH-TEXT,  FIRST-CASE PUSH,  s" die" CALL-ROW,
   ok X64CODE:LBL, ;

;package

\ The profiler cases' word: a record whose code is a span of text the image
\ fixes only at link time.
package X64HARNESS

public

\ Seed record r14 as RECORD, does, in wordlist 0 with no flags: the code cell
\ holds the first label's address and the length cell after it the span's byte
\ count (src/habu/code-span.f). Clobbers rax, rcx and rdx.
: CODE-RECORD, ( str label label -- ) {: s:str first:label last:label :}
   s 0 0 RECORD,
   X64ASM:RAX NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR                     \ one record past it
   X64ASM:RAX X64KERNEL:WRITE-ALIAS,
   X64ASM:RCX first X64CODE:MOVABS,
   X64ASM:RCX X64ASM:RAX X64KERNEL:REC-CODE DREC - X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX last X64CODE:MOVABS,
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDX X64ASM:RAX X64KERNEL:REC-CODE CELL + DREC - X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

;package

\ The alias cases' words. RX addresses stay read-execute throughout; only
\ the separate alias accepts writes.
package X64HARNESS

private

: CP-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-CP X64ASM:>R64 ;

public

\ What PROBE, pushes for a page the kernel cannot write: -EFAULT.
-14 constant FAULTED

EXPORT NAME-CELL

\ Push clock_gettime's answer for a timespec n bytes into the region: 0 when
\ the kernel can write the page, FAULTED when it cannot.
: PROBE, ( n -- ) {: off:n :}
   X64ASM:RDI CLOCK-MONOTONIC X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RSI DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   NR-CLOCK-GETTIME SYS,
   0 X64RT:G-PUSH ;

: PROBE-ALIAS, ( n -- ) {: off:n :}
   X64ASM:RDI CLOCK-MONOTONIC X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RSI DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RSI X64KERNEL:WRITE-ALIAS,
   NR-CLOCK-GETTIME SYS,
   0 X64RT:G-PUSH ;

: POKE, ( n -- ) {: off:n :}
   X64ASM:RAX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RCX DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64KERNEL:WRITE-ALIAS,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: PUSH-REGION-CELL, ( n -- ) {: off:n :}
   X64ASM:RAX DBASE-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   0 X64RT:G-PUSH ;

: PUSH-DATA-CELL, ( n -- ) {: off:n :}
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   0 X64RT:G-PUSH ;

: PUSH-CP, ( -- ) X64ASM:RAX CP-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  0 X64RT:G-PUSH ;

;package
