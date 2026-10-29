\ x86-64-boot-harness.f - an x86-64 image that boots as the engine does, carries
\ the whole kernel and runs one case against the DATA the boot mapped:
\ BOOT-OPEN, stages the boot and the kernel, the case follows, and BOOT-CLOSE,
\ exits 0 and writes the image.
require src/os/linux-x86-64/elf.f
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/habu/code-span.f
\ boot-x64.f and kernel-x64.f load the x86-64 seam globally, so they come
\ before the peer harness, which would otherwise load it into its own private
\ wordlist.
require test/x86-64-peer-harness.f

package X64HARNESS
using X64ASM
using X64CODE
using X64RT

\ Scratch cells past the heap floor, where nothing a booted image runs
\ allocates.
DATA-START $10000 + constant BOOT-SCRATCH-OFF
variable ENTRY-CELL
variable BOOT-END-CELL

: ENTRY-LBL ( -- label ) ENTRY-CELL @ >LABEL ;
: BOOT-END-LBL ( -- label ) BOOT-END-CELL @ >LABEL ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;

public

: SCRATCH-OFF ( -- n ) BOOT-SCRATCH-OFF ;

\ test/x86-64-peer-harness.f OPEN, cannot run a kernel body: its rbp is a 1 KiB
\ window of the machine stack, CASE1, resets r12 to rbp and CLOSE, pins r13-r15,
\ where a body reads DATA through rbp and moves those registers.
\ Checks exit FIRST-CASE upward in staging order, as the peer harness numbers
\ its checks; a negative image (BOOT-OPEN, given true) expects a wrong answer
\ from its first check.
\ Fresh labels and a fresh registry per image: labels die with the stream the
\ last image linked; registry rows do not.
: BOOT-OPEN, ( bool -- ) {: negative:bool :}
   ASM-RESET
   X64CODE:LBL EXIT-CELL !  X64CODE:LBL ENTRY-CELL !  X64CODE:LBL BOOT-END-CELL !
   FIRST-CASE CASE-NEXT !
   negative if FIRST-CASE else 0 then WRONG-AT !
   X64KERNEL:FLOORREC-NEW BOOT-END-LBL X64BOOT:START,
   ENTRY-LBL JMP,
   ENGINE-PRIMS:RESET
   X64KERNEL:KERNEL,
   0 0 0 0 0 0 0 X64KERNEL:SCOPE-KIND-HELPER,
   ENTRY-LBL X64CODE:LBL, ;

: PUSH, ( n -- ) {: v:n :}  RAX v IMM  0 G-PUSH ;

\ The string's bytes sit in the text behind a jump.
: PUSH-TEXT, ( ptr u8 n -- ) {: a:ptr u:n :}
   X64CODE:LBL X64CODE:LBL {: text:label past:label :}
   past JMP,
   text X64CODE:LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past X64CODE:LBL,
   RAX text MOVABS,  0 G-PUSH
   u PUSH, ;

: PUSH-SCRATCH, ( n -- ) {: off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

: CELL!, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: CALL-ROW, ( ptr u8 n -- ) X64KERNEL:ENTRY-LABEL CALL, ;

: EXPECT-POP, ( n -- ) {: want:n :}  0 G-POP  want EXPECT, ;

: EXPECT-DEPTH, ( n -- ) {: want:n :}
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   want CELL * EXPECT, ;

\ The boot leaves rsp below argc, one cell under the argument vector it
\ published, by the signal band boot-x64.f SIGNAL-STACK, carves off the machine
\ stack for the life of the thread.
: EXPECT-BALANCED, ( -- )
   RAX DATA-REG ARGV-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSP ASM-SINK ENC-SUB-RR
   CELL STACK-ABI:SIGNAL-BYTES + EXPECT, ;

: EXPECT-SCRATCH, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

\ A row that ends the process ends the image with its own status.
: BOOT-CLOSE, ( ptr u8 n -- )
   RDI ZERO-REG,
   EXIT,
   BOOT-END-LBL X64CODE:LBL,
   WRITE ;

private

: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: NDICT-REG ( -- r64 ) ENGINE-GPR:X64-NDICT >R64 ;

\ Up to eight of the name's bytes from `at`, little-endian, zero past its end.
: NAME-CELL ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   0  CELL 0 ?do
      at i + u < if  a at i + + c@  i 8 * lshift  or  then
   loop ;

\ Store n into the cell at an offset in rax's record, through rcx.
: ROW!, ( n n -- ) {: v:n off:n :}
   RCX v IMM
   RCX RAX off MEM-OFF ASM-SINK ENC-MOV-MR ;

: NAME-TEXT, ( ptr u8 n -- label ) {: a:ptr u:n :}
   X64CODE:LBL X64CODE:LBL {: text:label past:label :}
   past JMP,
   text X64CODE:LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past X64CODE:LBL,
   text ;

public

\ Seed record r14 of the dictionary at r13 + r14 * DREC as X64KERNEL's record
\ cells name them: the flags or'd with the name's length, the name inline when
\ it fits DNAME-INL bytes else a pointer with DNAME-EXT set, and the wid. The
\ code cell holds the ordinal (index plus one), so a found record's code is
\ never 0, the answer for an absent name. The booted region is read-write.
\ Clobbers rax and rcx.
: RECORD, ( ptr u8 n n n -- ) {: a:ptr u:n wid:n flags:n :}
   RAX NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX DBASE-REG ASM-SINK ENC-ADD-RR
   RCX NDICT-REG 1 MEM-OFF ASM-SINK ENC-LEA
   RCX RAX X64KERNEL:REC-CODE MEM-OFF ASM-SINK ENC-MOV-MR
   u DNAME-INL > if
      flags u or DNAME-EXT or X64KERNEL:REC-FLAGS ROW!,
      RCX a u NAME-TEXT, MOVABS,
      RCX RAX X64KERNEL:REC-NAME MEM-OFF ASM-SINK ENC-MOV-MR
   else
      flags u or X64KERNEL:REC-FLAGS ROW!,
      a u 0 NAME-CELL X64KERNEL:REC-NAME ROW!,
      a u CELL NAME-CELL X64KERNEL:REC-NAME CELL + ROW!,
   then
   wid X64KERNEL:REC-WID ROW!,
   NDICT-REG ASM-SINK ENC-INC ;

: EXPECT-ROW, ( n -- ) {: ix:n :}
   0 G-POP
   RAX DBASE-REG ASM-SINK ENC-SUB-RR
   ix DREC * EXPECT, ;

;using
;using
;using
;package

\ The engine-state cases' words: addresses the boot fixes only at run time
\ (DATA, the code region).
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Push the address n bytes into the code region: an xt the hook rows admit.
: PUSH-REGION, ( n -- ) {: off:n :}
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

: EXPECT-POP-DATA, ( n -- ) {: want:n :}
   0 G-POP  RAX DATA-REG ASM-SINK ENC-SUB-RR  want EXPECT, ;

: EXPECT-POP-REGION, ( n -- ) {: want:n :}
   0 G-POP  RAX DBASE-REG ASM-SINK ENC-SUB-RR  want EXPECT, ;

: EXPECT-CELL, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

: LABEL-CELL!, ( label n -- ) {: at:label off:n :}
   RAX at MOVABS,
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: ROUTINE, ( [ -- ] -- label )
   X64CODE:LBL X64CODE:LBL {: entry:label past:label :}
   past JMP,
   entry X64CODE:LBL,
   execute
   ASM-SINK ENC-RET
   past X64CODE:LBL,
   entry ;

;using
;using
;using
;package

\ The dictionary cases' words: DATA addresses the boot fixes only at run time.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ DATA spans 2^38 bytes, past a disp32, so the offset is a 64-bit immediate
\ added to DATA.
: PUSH-DATA, ( n -- ) {: off:n :}
   RAX off IMM  RAX DATA-REG ASM-SINK ENC-ADD-RR
   0 G-PUSH ;

: DATA-ADDR!, ( n n -- ) {: at:n off:n :}
   RAX DATA-REG at MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: REGION-ADDR!, ( n n -- ) {: at:n off:n :}
   RAX DBASE-REG at MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: REGION!, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: TEXT!, ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   u 0 ?do  a u i NAME-CELL off i + CELL!,  CELL +loop ;

: EXPECT-RECORD, ( n n n -- ) {: want:n ix:n off:n :}
   RAX DBASE-REG ix DREC * off + MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

\ Set record n's pages to the protection prot, as the kernel's own flips do.
: PROT-RECORD, ( n n -- ) {: ix:n prot:n :}
   R8 DBASE-REG ix DREC * MEM-OFF ASM-SINK ENC-LEA
   R8 prot X64KERNEL:PROT-REC, ;

;using
;using
;using
;package

\ An address in the image's text, which the image fixes only at link time.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

: PUSH-LABEL, ( label n -- ) {: at:label off:n :}
   RAX at MOVABS,
   RAX RAX off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

;using
;using
;using
;package

\ The pure rows' check: their case sets outnumber the statuses the checks
\ number from FIRST-CASE, so each check names itself in text instead.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Compare rax with rcx; on a mismatch the kernel's `die` row writes the text and
\ LF on fd 2 and exits FIRST-CASE. Staged before any numbered check, so a
\ negative image expects one less, as EXPECT-RCX, does.
: CHECK-RCX, ( ptr u8 n -- ) {: a:ptr u:n :}
   X64CODE:LBL {: ok:label :}
   WRONG-AT @ CASE-NEXT @ = if
      RCX 1 >IMM32 ASM-SINK ENC-SUB-RI32
      0 WRONG-AT !
   then
   RAX RCX ASM-SINK ENC-CMP-RR  C-E ok JCC,
   a u PUSH-TEXT,  FIRST-CASE PUSH,  s" die" CALL-ROW,
   ok X64CODE:LBL, ;

;using
;using
;using
;package

\ The profiler cases' word: a record whose code is a span of text the image
\ fixes only at link time.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Seed record r14 as RECORD, does, in wordlist 0 with no flags: the code cell
\ holds the first label's address and the length cell after it the span's byte
\ count (src/habu/code-span.f). Clobbers rax, rcx and rdx.
: CODE-RECORD, ( ptr u8 n label label -- ) {: a:ptr u:n first:label last:label :}
   a u 0 0 RECORD,
   RAX NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX DBASE-REG ASM-SINK ENC-ADD-RR                     \ one record past it
   RCX first MOVABS,
   RCX RAX X64KERNEL:REC-CODE DREC - MEM-OFF ASM-SINK ENC-MOV-MR
   RDX last MOVABS,
   RDX RCX ASM-SINK ENC-SUB-RR
   RCX R64>N >R32 CODE-SPAN:FULL >IMM32 ASM-SINK ENC-MOV32-RI32
   RDX RCX ASM-SINK ENC-OR-RR
   RDX RAX X64KERNEL:REC-CODE CELL + DREC - MEM-OFF ASM-SINK ENC-MOV-MR ;

;using
;using
;using
;package

\ The window cases' words. At rest the code region is read-execute throughout,
\ as the ARM64 boot leaves it, so a row that writes it outside its window faults
\ into the crash handler's dump and dies of that signal.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

private

5 constant PROT-RX                     \ PROT_READ|PROT_EXEC
: CP-REG ( -- r64 ) ENGINE-GPR:X64-CP >R64 ;

public

\ What PROBE, pushes for a page the kernel cannot write: -EFAULT.
-14 constant FAULTED

EXPORT NAME-CELL

\ The region at rest, as habu2.f EM-SNAPSHOT-RX-FLUSH leaves it: read-execute
\ with every band closed. The boot maps it read-write and opens no band.
: REST, ( -- )
   RDI DBASE-REG ASM-SINK ENC-MOV-RR
   RSI REGION >IMM32 ASM-SINK ENC-MOV-RI32
   RDX PROT-RX >IMM32 ASM-SINK ENC-MOV-RI32
   NR-MPROTECT SYS, ;

\ Push clock_gettime's answer for a timespec n bytes into the region: 0 when
\ the kernel can write the page, FAULTED when it cannot.
: PROBE, ( n -- ) {: off:n :}
   RDI CLOCK-MONOTONIC >IMM32 ASM-SINK ENC-MOV-RI32
   RSI DBASE-REG off MEM-OFF ASM-SINK ENC-LEA
   NR-CLOCK-GETTIME SYS,
   0 G-PUSH ;

: POKE, ( n -- ) {: off:n :}
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: PUSH-REGION-CELL, ( n -- ) {: off:n :}
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-RM
   0 G-PUSH ;

: PUSH-DATA-CELL, ( n -- ) {: off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM
   0 G-PUSH ;

\ Push the five band cells or'd together: 0 when every band is closed.
: PUSH-BANDS, ( -- )
   RAX DATA-REG PROT:WINDOW MEM-OFF ASM-SINK ENC-MOV-RM
   RAX DATA-REG PROT:WLO MEM-OFF ASM-SINK ENC-OR-RM
   RAX DATA-REG PROT:RLO MEM-OFF ASM-SINK ENC-OR-RM
   RAX DATA-REG PROT:RHI MEM-OFF ASM-SINK ENC-OR-RM
   RAX DATA-REG PROT:CF MEM-OFF ASM-SINK ENC-OR-RM
   0 G-PUSH ;

: PUSH-CP, ( -- ) RAX CP-REG ASM-SINK ENC-MOV-RR  0 G-PUSH ;

;using
;using
;using
;package
