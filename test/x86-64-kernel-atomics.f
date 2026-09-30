\ x86-64-kernel-atomics.f - the atomics and publication rows of the x86-64
\ kernel (src/habu/kernel-x64.f) in the booted harness, cross-built for an
\ x86-64 peer. Every image seals the friend latch first, so each guard a row
\ calls walks its whole span test and clobbers what it may.
\
\ hb-x64-kernel-atomics runs the rows on one heap scratch cell: atomic! stores
\ a value, atomic@ reads it back, fence runs, atomic-add answers the old value
\ and leaves the sum, and atomic-cas answers the value it found, swapping in
\ the new one when that matches the expected value and writing nothing when it
\ is stale. With the data stack empty and the machine stack balanced after, it
\ exits 0; hb-x64-kernel-atomics-negative expects the wrong stored value and
\ exits 21. hb-x64-kernel-atomics-store-armed, -add-armed and -cas-armed aim
\ atomic!, atomic-add and atomic-cas at the band cell TIER-PROV:N-CELL, so
\ each row's guard exits 83, ENGINE-ERROR:SEAL-VIOLATION;
\ hb-x64-kernel-patch32-armed aims patch32 there and exits 83 too.
\
\ The publication images put the code region at rest first, read-execute
\ throughout as the ARM64 boot leaves it, so a row that writes the region
\ outside its window faults into the crash handler's dump (134). A probe
\ proves a window closed: clock_gettime answers -EFAULT for a timespec the
\ kernel cannot write. Each of these exits 0:
\ - hb-x64-kernel-prot-window declares a span across the record band's top and
\   into the control-flow band, opens the code band at CP with an end below it
\   and widens it two pages on, writes each, checks the five band cells and
\   finds the page between the code band's two declarations writable.
\ - hb-x64-kernel-prot-window-closed opens the same window, closes it, and
\   probes each band and the page just past the window's recorded end.
\ - hb-x64-kernel-publish records five sites around a six-byte routine,
\   publishes it at CP, finds CP on the next code slot, calls it, probes its
\   page and finds the three sites in the span and its int3 fill gone; patch32
\   then rewrites its immediate and the call answers the new value.
\ - hb-x64-kernel-publish-fill publishes a 17-byte routine at CP and finds CP
\   32 bytes on, bytes 17-31 int3 over poisoned cells and the cell past them
\   untouched.
\ - hb-x64-kernel-sites records sites out of order, one twice and one at the
\   region's last byte, clears a middle span, a span below the region and one
\   reaching into it, and checks the rows left.
\ - hb-x64-kernel-retarget points a live record and the pending one at new
\   code, marks one record internal and another with a minimum input depth, and
\   probes the records' page.
\ - hb-x64-kernel-does gives an inline-named parent the clause record past the
\   pending one, finds its window closed and checks the record and its name.
\ - hb-x64-kernel-does-long then gives a long-named parent its clause, finds
\   the window closed and checks the record, the name zero-padded to a code
\   slot and CP on the next slot.
\ The armed images exit a refusal: code-publish one byte past CP
\ (-publish-armed) or past the region (-publish-past-armed), a site outside the
\ region (-sites-outside-armed) and xref-retarget at NDICT + 1
\ (-retarget-armed), with the empty exact length (-retarget-empty-armed) or
\ past CODE-SPAN:RAW-MAX (-retarget-raw-armed) exit 83; a site of the other kind
\ at a recorded offset (-sites-kind-armed) and a site into a full band
\ (-sites-full-armed) exit 103, SNAP-RELOC:SITE-RC. The host checks each
\ image's ELF header; running them is the peer's.
require src/habu/code-span.f
require test/x86-64-boot-harness.f

package X64K-ATOMICS
using X64ASM
using X64CODE
using X64RT

77 constant VALUE
5 constant DELTA
1000 constant NEW
2000 constant OTHER

: SUM ( -- n ) VALUE DELTA + ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

\ Push the address of the DATA cell at an offset.
: PUSH-CELL, ( n -- ) {: off:n :}
   RAX ENGINE-GPR:X64-RBASE >R64 off MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

: STORE-FETCH, ( -- )
   VALUE X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic!" X64HARNESS:CALL-ROW,
   VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   0 X64HARNESS:PUSH-SCRATCH,
   s" atomic@" X64HARNESS:CALL-ROW,
   VALUE X64HARNESS:EXPECT-POP,
   s" fence" X64HARNESS:CALL-ROW, ;

: ADD, ( -- )
   DELTA X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic-add" X64HARNESS:CALL-ROW,
   VALUE X64HARNESS:EXPECT-POP,
   SUM 0 X64HARNESS:EXPECT-SCRATCH, ;

\ atomic-cas with an expected and a new value: check the value it answers and
\ the cell after.
: CAS, ( n n n n -- ) {: expected:n new:n found:n after:n :}
   expected X64HARNESS:PUSH,  new X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,
   s" atomic-cas" X64HARNESS:CALL-ROW,
   found X64HARNESS:EXPECT-POP,
   after 0 X64HARNESS:EXPECT-SCRATCH, ;

\ Ten checks, the harness's whole budget.
: ROWS, ( -- )
   STORE-FETCH,  ADD,
   SUM NEW SUM NEW CAS,
   VALUE OTHER NEW NEW CAS,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   SEAL,  ROWS,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Push n zero operands and then the band cell's address, and call the row.
: BUILD-ARMED ( ptr u8 n n ptr u8 n -- ) {: row:ptr rowu:n args:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   args 0 ?do 0 X64HARNESS:PUSH, loop
   TIER-PROV:N-CELL PUSH-CELL,
   row rowu X64HARNESS:CALL-ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the publication rows ----------------------------------------------------
$1000 constant PAGE                    \ the x86-64 page a window flips
5 constant PROT-RX                     \ PROT_READ|PROT_EXEC
-14 constant FAULTED                   \ -EFAULT: the kernel could not write
42 constant ANSWER                     \ what the published routine answers
99 constant PATCHED                    \ what it answers once patch32 rewrites it
1 constant IMM-AT                      \ mov eax, imm32: the immediate follows the opcode
32 constant KIND-SHIFT                 \ a checked site row: the kind above the offset
52 constant MIN-IN-SHIFT               \ layout.f DNAME-MIN-IN-MASK: flag bits 52-59

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: CP-REG ( -- r64 ) ENGINE-GPR:X64-CP >R64 ;

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

\ Call the routine n bytes into the region and push what it answers.
: CALL-REGION, ( n -- ) {: off:n :}
   RAX DBASE-REG off MEM-OFF ASM-SINK ENC-LEA
   RAX ASM-SINK ENC-CALL-REG
   0 G-PUSH ;

\ Push the address of the code the quotation emits, which sits in the text
\ behind a jump, and answer its length.
: PUSH-CODE, ( [ -- ] -- n )
   LBL LBL {: code:label past:label :}
   past JMP,
   code LBL,
   ASM-LEN {: start:n :}
   execute
   ASM-LEN start - {: len:n :}
   past LBL,
   RAX code MOVABS,  0 G-PUSH
   len ;

\ mov eax, ANSWER; ret: six bytes, a length no instruction-word guard admits.
: ANSWER-CODE, ( -- )
   0 >R32 ANSWER >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-RET ;

\ mov eax, ANSWER, eleven nops and ret: 17 bytes, one past a code slot.
: LONG-CODE, ( -- )
   0 >R32 ANSWER >IMM32 ASM-SINK ENC-MOV32-RI32
   11 0 ?do ASM-SINK ENC-NOP loop
   ASM-SINK ENC-RET ;

\ The code slots past DICT-SIZE, and the 17-byte routine's second slot as
\ whole cells: its ret, then the int3 fill.
DICT-SIZE 16 + constant SECOND-SLOT
DICT-SIZE 32 + constant THIRD-SLOT
$CCCCCCCCCCCCCCC3 constant RET-INT3S
$CCCCCCCCCCCCCCCC constant INT3S

\ Call the row on the address n bytes into the region.
: AT-ROW, ( n ptr u8 n -- ) {: off:n row:ptr rowu:n :}
   off X64HARNESS:PUSH-REGION,  row rowu X64HARNESS:CALL-ROW, ;

\ reloc-maps-clear over n bytes from the offset into the region.
: CLEAR, ( n n -- ) {: off:n len:n :}
   off X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" reloc-maps-clear" X64HARNESS:CALL-ROW, ;

\ Check site row k holds the offset and the kind.
: EXPECT-SITE, ( n n n -- ) {: k:n off:n kind:n :}
   SNAP-RELOC:SITE-ROWS-OFF k SNAP-RELOC:SITE-ROW-BYTES * + {: at:n :}
   0 >R32 DATA-REG at MEM-OFF ASM-SINK ENC-MOV32-RM
   RCX DATA-REG at SNAP-RELOC:SITE-KIND-OFF + MEM-OFF ASM-SINK ENC-MOVZX-8-RM
   RCX KIND-SHIFT >IMM8 ASM-SINK ENC-SHL-RI8
   RAX RCX ASM-SINK ENC-OR-RR
   0 G-PUSH
   kind KIND-SHIFT lshift off or X64HARNESS:EXPECT-POP, ;

\ The offset of cell `off` of record k.
: REC ( n n -- n ) {: k:n off:n :} k DREC * off + ;

\ A record's CODE-SPAN length: the cell after its code cell.
X64KERNEL:REC-CODE CELL + constant REC-LEN

\ Store record k's address in PEND-CELL: the pending record does-record reads.
: PEND!, ( n -- ) {: k:n :}
   RAX DBASE-REG k DREC * MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG PEND-CELL MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Up to eight bytes of a name as one little-endian cell, zero past its end.
: LE-CELL ( ptr u8 n -- n ) {: a:ptr u:n :}
   0  u 0 ?do  a i + c@  i 8 * lshift  or  loop ;

\ A span from a cell below the control-flow band into it, and a cell two pages
\ past CP.
CFSTK-OFF CELL - constant STRADDLE
DICT-SIZE PAGE 2 * + CELL + constant WIDEN

\ Declare n bytes from the offset into the region with the window's LSPAN.
: DECLARE, ( n n -- ) {: off:n len:n :}
   RDI DBASE-REG off MEM-OFF ASM-SINK ENC-LEA
   RSI len >IMM32 ASM-SINK ENC-MOV-RI32
   X64KERNEL:WINDOW-SPAN, ;

\ The window both window images open: a span across the record band's top and
\ into the control-flow band, the code band at CP with an end below it, and a
\ span two pages on that widens the code band, each written.
: WINDOW-SETUP, ( -- )
   REST,
   STRADDLE CELL 2 * DECLARE,
   STRADDLE POKE,  CFSTK-OFF POKE,
   RDI ZERO-REG,  X64KERNEL:WINDOW-OPEN,              \ an end below CP: CP's byte
   WIDEN CELL DECLARE,
   DICT-SIZE POKE,  WIDEN POKE, ;

: WINDOW-CASE, ( -- )
   WINDOW-SETUP,
   PROT:RLO PUSH-DATA-CELL,  CFSTK-OFF PAGE - X64HARNESS:EXPECT-POP-REGION,
   PROT:RHI PUSH-DATA-CELL,  CFSTK-OFF X64HARNESS:EXPECT-POP-REGION,
   1 PROT:CF X64HARNESS:EXPECT-CELL,
   PROT:WLO PUSH-DATA-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   PROT:WINDOW PUSH-DATA-CELL,  DICT-SIZE PAGE 3 * + X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE PAGE + PROBE,  0 X64HARNESS:EXPECT-POP,  \ the page only the union opened
   X64KERNEL:WINDOW-CLOSE, ;

\ Closed, each band is read-execute again, and so is the page just past the
\ window's recorded end, which no flip may have reached.
: WINDOW-CLOSED-CASE, ( -- )
   WINDOW-SETUP,
   X64KERNEL:WINDOW-CLOSE,
   PUSH-BANDS,  0 X64HARNESS:EXPECT-POP,
   STRADDLE CELL - PROBE,  FAULTED X64HARNESS:EXPECT-POP,
   CFSTK-OFF PROBE,  FAULTED X64HARNESS:EXPECT-POP,
   WIDEN PROBE,  FAULTED X64HARNESS:EXPECT-POP,
   DICT-SIZE PAGE 3 * + PROBE,  FAULTED X64HARNESS:EXPECT-POP, ;

\ The routine's sites: below its span and on the slot past its fill stay, and
\ the two inside it and the one at the fill's last byte go with the
\ publication.
: PUBLISH-CASE, ( -- )
   REST,
   [: ANSWER-CODE, ;] PUSH-CODE, {: len:n :}
   SECOND-SLOT s" callmap-set" AT-ROW,               \ into the empty band
   SECOND-SLOT 1- s" callmap-set" AT-ROW,            \ the fill's last byte, below it
   DICT-SIZE 1- s" callmap-set" AT-ROW,              \ below every row
   DICT-SIZE 2 + s" addrmap-set" AT-ROW,             \ between the two
   DICT-SIZE len + 1- s" callmap-set" AT-ROW,        \ the span's last byte
   DICT-SIZE X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" code-publish" X64HARNESS:CALL-ROW,
   PUSH-CP,  SECOND-SLOT X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE CALL-REGION,  ANSWER X64HARNESS:EXPECT-POP,
   DICT-SIZE CELL + PROBE,  FAULTED X64HARNESS:EXPECT-POP,
   PUSH-BANDS,  0 X64HARNESS:EXPECT-POP,
   2 SNAP-RELOC:SITE-N-CELL X64HARNESS:EXPECT-CELL,
   0 DICT-SIZE 1- SNAP-RELOC:SITE-CALL EXPECT-SITE,
   1 SECOND-SLOT SNAP-RELOC:SITE-CALL EXPECT-SITE,
   PATCHED X64HARNESS:PUSH,  DICT-SIZE IMM-AT + X64HARNESS:PUSH-REGION,
   s" patch32" X64HARNESS:CALL-ROW,
   DICT-SIZE CALL-REGION,  PATCHED X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

\ The fill overwrites the two poisoned cells of the routine's second slot and
\ stops short of the poisoned cell past it, where CP lands.
: PUBLISH-FILL-CASE, ( -- )
   SECOND-SLOT POKE,  SECOND-SLOT CELL + POKE,  THIRD-SLOT POKE,
   REST,
   [: LONG-CODE, ;] PUSH-CODE, {: len:n :}
   DICT-SIZE X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" code-publish" X64HARNESS:CALL-ROW,
   PUSH-CP,  THIRD-SLOT X64HARNESS:EXPECT-POP-REGION,
   SECOND-SLOT PUSH-REGION-CELL,  RET-INT3S X64HARNESS:EXPECT-POP,
   SECOND-SLOT CELL + PUSH-REGION-CELL,  INT3S X64HARNESS:EXPECT-POP,
   THIRD-SLOT PUSH-REGION-CELL,  -1 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: SITES-CASE, ( -- )
   100 s" callmap-set" AT-ROW,                       \ into the empty band
   300 s" addrmap-set" AT-ROW,                       \ an append
   200 s" callmap-set" AT-ROW,                       \ below the last row, which moves up
   50 s" addrmap-set" AT-ROW,                        \ below every row
   100 s" callmap-set" AT-ROW,                       \ the same row again: no change
   REGION 1- s" addrmap-set" AT-ROW,                 \ the region's last byte
   150 100 CLEAR,                                    \ drops 200
   PAGE negate PAGE 2 / CLEAR,                       \ below the region: drops none
   CELL negate 76 CLEAR,                             \ reaches into it: drops 50
   3 SNAP-RELOC:SITE-N-CELL X64HARNESS:EXPECT-CELL,
   0 100 SNAP-RELOC:SITE-CALL EXPECT-SITE,
   1 300 SNAP-RELOC:SITE-ADDR EXPECT-SITE,
   2 REGION 1- SNAP-RELOC:SITE-ADDR EXPECT-SITE,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

\ Where the retargets point, and a length that is no whole instruction word.
DICT-SIZE $40 + constant ENTRY-A
DICT-SIZE $80 + constant ENTRY-B
7 constant LEN-A

: RETARGET, ( n n n -- ) {: entry:n len:n idx:n :}
   entry X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,  idx X64HARNESS:PUSH,
   s" xref-retarget" X64HARNESS:CALL-ROW, ;

: RETARGET-CASE, ( -- )
   s" alpha" 3 0 X64HARNESS:RECORD,
   s" beta" 3 0 X64HARNESS:RECORD,
   s" gamma" 3 0 X64HARNESS:RECORD,
   REST,
   ENTRY-A LEN-A 1 RETARGET,
   ENTRY-B CODE-SPAN:RAW-MAX 3 RETARGET,             \ record NDICT, the pending one
   0 X64HARNESS:PUSH,  s" int-mark" X64HARNESS:CALL-ROW,
   2 X64HARNESS:PUSH,  $1FF X64HARNESS:PUSH,  s" min-in-mark" X64HARNESS:CALL-ROW,
   1 X64KERNEL:REC-CODE REC PUSH-REGION-CELL,  ENTRY-A X64HARNESS:EXPECT-POP-REGION,
   1 REC-LEN REC PUSH-REGION-CELL,  LEN-A X64HARNESS:EXPECT-POP,
   3 X64KERNEL:REC-CODE REC PUSH-REGION-CELL,  ENTRY-B X64HARNESS:EXPECT-POP-REGION,
   3 REC-LEN REC PUSH-REGION-CELL,  CODE-SPAN:RAW-MAX X64HARNESS:EXPECT-POP,
   0 X64KERNEL:REC-FLAGS REC PUSH-REGION-CELL,
   s" alpha" nip DNAME-INT or X64HARNESS:EXPECT-POP,
   2 X64KERNEL:REC-FLAGS REC PUSH-REGION-CELL,
   s" gamma" nip $FF MIN-IN-SHIFT lshift or X64HARNESS:EXPECT-POP,
   0 X64KERNEL:REC-FLAGS REC PROBE,  FAULTED X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

\ The two clauses: record 3, past the pending record 2, twice. The short name,
\ 8 bytes, fills a 16-byte code slot at CP; the long name, 22 bytes, lands on
\ the next slot and fills two, and its pad lands in its third and fourth cells.
DICT-SIZE $100 + constant CLAUSE-A
DICT-SIZE $200 + constant CLAUSE-B
9 constant CLAUSE-LEN-A
11 constant CLAUSE-LEN-B
: SHORT$ ( -- ptr u8 n ) s" abc;does" ;
: LONG$ ( -- ptr u8 n ) s" abcdefghijklmnopq;does" ;
DICT-SIZE 16 + constant LONG-AT
LONG-AT CELL 2 * + constant LONG-TAIL
LONG-TAIL CELL + constant LONG-PAD
32 constant LONG-PADDED

: DOES, ( n n -- ) {: entry:n len:n :}
   entry X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" does-record" X64HARNESS:CALL-ROW, ;

\ The two parents, the poisoned cells the long name's pad lands in, and the
\ region at rest.
: DOES-SETUP, ( -- )
   s" abc" 5 0 X64HARNESS:RECORD,
   s" abcdefghijklmnopq" 6 0 X64HARNESS:RECORD,     \ past DNAME-INL: out of line
   LONG-TAIL POKE,  LONG-PAD POKE,
   0 PEND!,
   REST, ;

\ After each clause the row has closed its window: no band is open and the
\ clause record's page cannot be written.
: DOES-CLOSED, ( -- )
   PUSH-BANDS,  0 X64HARNESS:EXPECT-POP,
   3 X64KERNEL:REC-FLAGS REC PROBE,  FAULTED X64HARNESS:EXPECT-POP, ;

: DOES-CASE, ( -- )
   DOES-SETUP,
   CLAUSE-A CLAUSE-LEN-A DOES,
   DOES-CLOSED,
   3 X64KERNEL:REC-CODE REC PUSH-REGION-CELL,  CLAUSE-A X64HARNESS:EXPECT-POP-REGION,
   3 REC-LEN REC PUSH-REGION-CELL,  CLAUSE-LEN-A X64HARNESS:EXPECT-POP,
   3 X64KERNEL:REC-FLAGS REC PUSH-REGION-CELL,
   SHORT$ nip DNAME-EXT or X64HARNESS:EXPECT-POP,
   3 X64KERNEL:REC-NAME REC PUSH-REGION-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE PUSH-REGION-CELL,  SHORT$ LE-CELL X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

\ The long-named parent's clause, published after the short one's, so its name
\ lands at LONG-AT.
: DOES-LONG-CASE, ( -- )
   DOES-SETUP,
   CLAUSE-A CLAUSE-LEN-A DOES,
   1 PEND!,
   CLAUSE-B CLAUSE-LEN-B DOES,
   DOES-CLOSED,
   3 X64KERNEL:REC-FLAGS REC PUSH-REGION-CELL,
   LONG$ nip DNAME-EXT or X64HARNESS:EXPECT-POP,
   3 X64KERNEL:REC-WID REC PUSH-REGION-CELL,  6 X64HARNESS:EXPECT-POP,
   LONG-TAIL PUSH-REGION-CELL,  s" q;does" LE-CELL X64HARNESS:EXPECT-POP,
   LONG-PAD PUSH-REGION-CELL,  0 X64HARNESS:EXPECT-POP,
   PUSH-CP,  LONG-AT LONG-PADDED + X64HARNESS:EXPECT-POP-REGION,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: PUBLISH-ARMED, ( n n -- ) {: dst:n len:n :}
   0 X64HARNESS:PUSH,  dst X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" code-publish" X64HARNESS:CALL-ROW, ;

\ One image: the boot, the sealed latch, the case the quotation emits and the
\ exit.
: IMAGE ( [ -- ] ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   execute
   path pathu X64HARNESS:BOOT-CLOSE, ;

: PUBLICATION ( -- )
   [: WINDOW-CASE, ;] s" hb-x64-kernel-prot-window" TMP-PATH IMAGE
   [: WINDOW-CLOSED-CASE, ;] s" hb-x64-kernel-prot-window-closed" TMP-PATH IMAGE
   [: PUBLISH-CASE, ;] s" hb-x64-kernel-publish" TMP-PATH IMAGE
   [: PUBLISH-FILL-CASE, ;] s" hb-x64-kernel-publish-fill" TMP-PATH IMAGE
   [: SITES-CASE, ;] s" hb-x64-kernel-sites" TMP-PATH IMAGE
   [: RETARGET-CASE, ;] s" hb-x64-kernel-retarget" TMP-PATH IMAGE
   [: DOES-CASE, ;] s" hb-x64-kernel-does" TMP-PATH IMAGE
   [: DOES-LONG-CASE, ;] s" hb-x64-kernel-does-long" TMP-PATH IMAGE
   [: DICT-SIZE 1+ 1 PUBLISH-ARMED, ;] s" hb-x64-kernel-publish-armed" TMP-PATH IMAGE
   [: DICT-SIZE REGION DICT-SIZE - 1+ PUBLISH-ARMED, ;]
   s" hb-x64-kernel-publish-past-armed" TMP-PATH IMAGE
   [: 100 s" callmap-set" AT-ROW,  100 s" addrmap-set" AT-ROW, ;]
   s" hb-x64-kernel-sites-kind-armed" TMP-PATH IMAGE
   [: SNAP-RELOC:SITE-CAP SNAP-RELOC:SITE-N-CELL X64HARNESS:CELL!,
      100 s" callmap-set" AT-ROW, ;]
   s" hb-x64-kernel-sites-full-armed" TMP-PATH IMAGE
   [: REGION s" callmap-set" AT-ROW, ;]
   s" hb-x64-kernel-sites-outside-armed" TMP-PATH IMAGE
   [: 0 1 1 RETARGET, ;] s" hb-x64-kernel-retarget-armed" TMP-PATH IMAGE
   [: 0 CODE-SPAN:FULL 0 RETARGET, ;]
   s" hb-x64-kernel-retarget-empty-armed" TMP-PATH IMAGE
   [: 0 CODE-SPAN:RAW-MAX 1+ 0 RETARGET, ;]
   s" hb-x64-kernel-retarget-raw-armed" TMP-PATH IMAGE ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-atomics" TMP-PATH BUILD
   true s" hb-x64-kernel-atomics-negative" TMP-PATH BUILD
   s" atomic!" 1 s" hb-x64-kernel-atomics-store-armed" TMP-PATH BUILD-ARMED
   s" atomic-add" 1 s" hb-x64-kernel-atomics-add-armed" TMP-PATH BUILD-ARMED
   s" atomic-cas" 2 s" hb-x64-kernel-atomics-cas-armed" TMP-PATH BUILD-ARMED
   s" patch32" 1 s" hb-x64-kernel-patch32-armed" TMP-PATH BUILD-ARMED
   PUBLICATION
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-ATOMICS:RUN
