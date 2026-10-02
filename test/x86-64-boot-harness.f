\ x86-64-boot-harness.f - the booted harness: an x86-64 image that boots as the
\ engine does, carries the whole kernel and runs one case against the DATA the
\ boot mapped. An image is staged in this order:
\
\    BOOT-OPEN,  case  BOOT-CLOSE,
\
\ BOOT-OPEN, emits X64BOOT's `_start`, a jump over the kernel X64KERNEL:KERNEL,
\ emits next, and the case's entry past it, which marks the kernel's text
\ native as the engine's boot marks its own; BOOT-CLOSE, exits 0 and writes the
\ image. A case stages cells, arms DATA cells, calls kernel rows by name and
\ checks a popped cell, the data stack's depth, the machine stack's balance
\ and a scratch cell. The checks exit FIRST-CASE upward in the order they are
\ staged, as the peer harness numbers its case checks; a negative image
\ (BOOT-OPEN, given true) expects a wrong answer from its first check, so it
\ exits FIRST-CASE. A row that ends the process ends the image with its own
\ status.
\
\ The peer harness's own entry (test/x86-64-peer-harness.f OPEN,) cannot run a
\ kernel body: its rbp is a 1 KiB window of the machine stack, CASE1, resets
\ r12 to rbp and CLOSE, pins r13-r15, where a body reads DATA through rbp and
\ moves those registers.
\
\ boot-x64.f and kernel-x64.f load the x86-64 seam globally, so they come
\ before the peer harness, which would otherwise load it into its own private
\ wordlist.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/habu/code-origin-x64.f
require src/habu/code-span.f
require test/x86-64-peer-harness.f

package X64HARNESS
using X64ASM
using X64CODE
using X64RT

\ Scratch cells past the heap floor, where nothing a booted image runs
\ allocates.
DATA-START $10000 + constant BOOT-SCRATCH-OFF
variable ENTRY-CELL

: ENTRY-LBL ( -- label ) ENTRY-CELL @ >LABEL ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;

public

\ Begin an image: the boot, then the kernel with fresh labels and a fresh
\ registry, since labels die with the stream the last image linked and
\ registry rows do not.
: BOOT-OPEN, ( bool -- ) {: negative:bool :}
   ASM-RESET
   LBL EXIT-CELL !  LBL ENTRY-CELL !
   FIRST-CASE CASE-NEXT !
   negative if FIRST-CASE else 0 then WRONG-AT !
   X64BOOT:START,
   ENTRY-LBL JMP,
   ENGINE-PRIMS:RESET
   LBL {: text:label :}  text LBL,
   X64KERNEL:KERNEL,
   ENTRY-LBL LBL,
   text ENTRY-LBL X64PROV:TEXT-NATIVE, ;

: PUSH, ( n -- ) {: v:n :}  RAX v IMM  0 G-PUSH ;

\ Push a string's address and length. Its bytes sit in the text behind a jump.
: PUSH-TEXT, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past LBL,
   RAX text MOVABS,  0 G-PUSH
   u PUSH, ;

\ Push the address of the scratch cell n bytes in.
: PUSH-SCRATCH, ( n -- ) {: off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

\ Store n into the DATA cell at an offset: arm the friend latch, a live task or
\ the exit hook before a row reads it.
: CELL!, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Call the kernel row registered under the name.
: CALL-ROW, ( ptr u8 n -- ) X64KERNEL:ENTRY-LABEL CALL, ;

\ Pop a cell and check it.
: EXPECT-POP, ( n -- ) {: want:n :}  0 G-POP  want EXPECT, ;

\ Check the data stack holds n cells above the base the boot published.
: EXPECT-DEPTH, ( n -- ) {: want:n :}
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   want CELL * EXPECT, ;

\ Check rsp stands where the kernel left it, at argc: one cell below the
\ argument vector the boot published.
: EXPECT-BALANCED, ( -- )
   RAX DATA-REG ARGV-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSP ASM-SINK ENC-SUB-RR
   CELL EXPECT, ;

\ Check the scratch cell at an offset holds n.
: EXPECT-SCRATCH, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

\ End an image: exit 0 when every check held, then write it.
: BOOT-CLOSE, ( ptr u8 n -- )
   RDI ZERO-REG,
   WRITE-ELF ;

private

: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: NDICT-REG ( -- r64 ) ENGINE-GPR:X64-NDICT >R64 ;

\ One cell of an inline name: up to eight of its bytes from byte `at`,
\ little-endian, zero past its end.
: NAME-CELL ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   0  CELL 0 ?do
      at i + u < if  a at i + + c@  i 8 * lshift  or  then
   loop ;

\ Store n into the cell at an offset in rax's record, through rcx.
: ROW!, ( n n -- ) {: v:n off:n :}
   RCX v IMM
   RCX RAX off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ The address of a name's bytes, which sit in the text behind a jump.
: NAME-TEXT, ( ptr u8 n -- label ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past LBL,
   text ;

public

\ Seed record r14 of the dictionary at r13 + r14 * DREC with a name, a wid and
\ flags, and count it, laid out as X64KERNEL's record cells name them: the
\ flags or'd with the name's length, the name inline when it fits DNAME-INL
\ bytes and otherwise a pointer to its bytes with DNAME-EXT set, and the wid.
\ The code cell holds the record's ordinal, its index plus one, so a found
\ record's code is never 0, the answer for an absent name. The booted region is
\ read-write. It clobbers rax and rcx.
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

\ Pop a cell and check it is the address of record n.
: EXPECT-ROW, ( n -- ) {: ix:n :}
   0 G-POP
   RAX DBASE-REG ASM-SINK ENC-SUB-RR
   ix DREC * EXPECT, ;

;using
;using
;using
;package

\ The engine-state cases' words: addresses the boot fixes only at run time
\ (DATA, the code region), whole DATA cells, and a routine a case installs
\ where a row calls through a DATA cell.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Push the address n bytes into the code region: an xt the hook rows admit.
: PUSH-REGION, ( n -- ) {: off:n :}
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

\ Pop an address and check it lies n bytes into DATA.
: EXPECT-POP-DATA, ( n -- ) {: want:n :}
   0 G-POP  RAX DATA-REG ASM-SINK ENC-SUB-RR  want EXPECT, ;

\ Pop an address and check it lies n bytes into the code region.
: EXPECT-POP-REGION, ( n -- ) {: want:n :}
   0 G-POP  RAX DBASE-REG ASM-SINK ENC-SUB-RR  want EXPECT, ;

\ Check the DATA cell at an offset holds n.
: EXPECT-CELL, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

\ Store a label's address into the DATA cell at an offset: install a routine
\ where a row calls through the cell.
: LABEL-CELL!, ( label n -- ) {: at:label off:n :}
   RAX at MOVABS,
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Emit a routine behind a jump, its body what the quotation emits and then
\ `ret`, and answer its entry label.
: ROUTINE, ( [ -- ] -- label )
   LBL LBL {: entry:label past:label :}
   past JMP,
   entry LBL,
   execute
   ASM-SINK ENC-RET
   past LBL,
   entry ;

;using
;using
;using
;package

\ The dictionary and engine-state cases' words: DATA cells that hold an
\ address the boot fixes only at run time, text staged in DATA, and a
\ record's cell.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Push the address of the DATA cell at an offset.
: PUSH-DATA, ( n -- ) {: off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

\ Store the address n bytes into DATA into the DATA cell at an offset.
: DATA-ADDR!, ( n n -- ) {: at:n off:n :}
   RAX DATA-REG at MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Store the address n bytes into the code region into the DATA cell at an
\ offset.
: REGION-ADDR!, ( n n -- ) {: at:n off:n :}
   RAX DBASE-REG at MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Store n into the cell at an offset in the code region.
: REGION!, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Store a string's bytes into DATA from an offset, eight to a cell, the last
\ cell zero past its end.
: TEXT!, ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   u 0 ?do  a u i NAME-CELL off i + CELL!,  CELL +loop ;

\ Check the cell at an offset in record n holds n.
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

\ The provenance cases' word: an address in the image's text, which the image
\ fixes only at link time.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Push the address n bytes past a label.
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

\ Compare rax with rcx. On a mismatch, the kernel's `die` row writes the text
\ and LF on fd 2 and exits FIRST-CASE. A negative image expects a wrong answer
\ from its first check of either kind, so this check, staged before any
\ numbered one, expects one less, as EXPECT-RCX, does.
: CHECK-RCX, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: ok:label :}
   WRONG-AT @ CASE-NEXT @ = if
      RCX 1 >IMM32 ASM-SINK ENC-SUB-RI32
      0 WRONG-AT !
   then
   RAX RCX ASM-SINK ENC-CMP-RR  C-E ok JCC,
   a u PUSH-TEXT,  FIRST-CASE PUSH,  s" die" CALL-ROW,
   ok LBL, ;

;using
;using
;using
;package

\ The profiler cases' word: a record whose code is a span of the image's text,
\ which the image fixes only at link time.
package X64HARNESS
using X64ASM
using X64CODE
using X64RT

public

\ Seed record r14 as RECORD, does, in wordlist 0 with no flags, its code the
\ text from the first label to the second: the code cell holds the first
\ label's address and the length cell after it the span's byte count as an
\ exact span (src/habu/code-span.f). It clobbers rax, rcx and rdx.
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

\ The window cases' words: the code region put at rest, a probe that proves a
\ page closed, a poisoned cell, and the cells a case reads back. At rest the
\ region is read-execute throughout, as the ARM64 boot leaves it, so a row
\ that writes it outside its window faults into the crash handler's dump
\ (134).
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
\ throughout with every band closed. The boot maps it read-write and opens no
\ band.
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

\ Store -1 n bytes into the region: a store its page must admit.
: POKE, ( n -- ) {: off:n :}
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Push the cell n bytes into the region.
: PUSH-REGION-CELL, ( n -- ) {: off:n :}
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-MOV-RM
   0 G-PUSH ;

\ Push the DATA cell at an offset.
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

\ Pop a cell into the scratch cell at an offset.
: POP-SCRATCH, ( n -- ) {: off:n :}
   0 G-POP
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Add n to the scratch cell at an offset: a routine a row calls counts its
\ calls there.
: ADD-SCRATCH, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-ADD-MR ;

;using
;using
;using
;package
