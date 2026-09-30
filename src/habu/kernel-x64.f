\ kernel-x64.f - the x86-64 engine's primitive bodies, package X64KERNEL.
\
\ The twins of src/habu/habu1.f's bodies for the rows of src/habu/prims.f, hand
\ written through X64ASM. PRIM registers a body in the shared registry
\ (src/habu/primitive-registry.f) as habu1.f FPRIM does, so the specification
\ gates hold on both targets, and the helpers the rows share are emitted once:
\ the span guard (PROT-SPAN), the narrow page flip LPROTREC, the task-live
\ exit LTASKLIVE, the dictionary index with the one-wordlist search, the DP
\ refusal LDPBAD and the output device arm (GENIO-OUT).
\
\ A BODY'S CONTRACT (docs/x86-64.md "Kernel inventory"). rbp is DATA, r12 the
\ data stack, rbx and r13-r15 the other VM registers; rax rcx rdx rsi rdi and
\ r8-r11 are scratch. A body is entered by `call` and leaves by the `ret` PRIM
\ appends, so the return address stays on the machine stack and a body calls a
\ helper without a frame: one definer serves where habu1.f needs FPRIM and
\ FPRIM-L.
\
\ The x86-64 seam (src/os/linux-x86-64/sys.f) loads here globally, so this file
\ loads before any file that loads the seam into a private wordlist, as
\ src/habu/boot-x64.f does.
\
\ One section per group of rows. KERNEL, emits the helpers and every section;
\ a group's rows land in its section word, which KERNEL, calls.
require lib/byte-buffer.f
require lib/string.f
require src/core/util.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/habu/primitive-registry.f
require src/habu/data-bands.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/rt.f
require src/os/linux-x86-64/sys.f

package X64KERNEL
using X64ASM
using X64CODE
using X64RT

\ The host's layout names its own DATA. Replay the Linux target's layout here,
\ as src/habu/boot-x64.f does, so the heap rows bound DP by the target's
\ DATA-SIZE.
s" src/os/linux-x86-64/layout.f" included

public

\ The rc a primitive body this kernel lacks dies with: ENTRY-LABEL's at build
\ time and a REFUSE row's at run time. It is the rc habu1.f
\ ENGINE-EMIT:TARGET-UNKNOWN dies with when a target has no body at all.
76 constant REFUSE-RC

\ The highest DP, as an offset from DATA, that the heap rows admit: the top of
\ DATA less the profiler's counter band, as habu1.f DP-CHECK bounds it.
DATA-SIZE PROF-CNT-BYTES - constant DP-CEILING

private

$1000 constant PAGE-BYTES              \ the x86-64 Linux base page
79 constant TASK-LIVE-RC               \ habu1.f B-TASK-LIVE-GUARD's $4F
2 constant STDERR

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

\ mov r32, imm32, which zero-extends: every constant here is a small
\ non-negative count, descriptor, prot or status.
: IMM32, ( r64 n -- ) {: r:r64 v:n :}
   r R64>N >R32 v >IMM32 ASM-SINK ENC-MOV32-RI32 ;

: TEXT, ( ptr u8 n -- ) BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN ;

: EXIT-GROUP, ( n -- ) {: rc:n :} RDI rc IMM32,  NR-EXIT-GROUP SYS, ;

: SAME? ( r64 r64 -- bool ) R64>N swap R64>N = ;

\ The helpers' labels, made by HELPERS, in the stream it emits them into.
variable SPAN-CELL
variable REC-CELL
variable LIVE-CELL
variable DPBAD-CELL
: SPAN-LBL ( -- label ) SPAN-CELL @ >LABEL ;
: REC-LBL ( -- label ) REC-CELL @ >LABEL ;
: LIVE-LBL ( -- label ) LIVE-CELL @ >LABEL ;
: DPBAD-LBL ( -- label ) DPBAD-CELL @ >LABEL ;

\ ---- the definer -------------------------------------------------------------
\ The row being registered. Every body registers under a name src/habu/prims.f
\ specifies: SPEC-CHECK runs before KEEP?, so a subset build cannot hide an
\ unspecified primitive, and ENGINE-PRIMS:COMPLETE refuses a kept row that no
\ body answered.
defer BODY ( -- )
PTR-VARIABLE ROW-A
variable ROW-U
: ROW$ ( -- ptr u8 n ) ROW-A @ ROW-U @ ;

: ARGS ( ptr u8 n [ -- ] -- )
   is BODY  ROW-U !  ROW-A !
   ROW$ ENGINE-PRIMS:SPEC-CHECK ;

\ The record spans [first, last): the body and the ret after it.
: EMIT-ROW ( -- n )
   LBL LBL {: first:label last:label :}
   ROW$ first last ENGINE-PRIMS:ADD {: row:n :}
   first LBL,  BODY  ASM-SINK ENC-RET  last LBL,
   row ;

: REFUSE-HEAD$ ( -- ptr u8 n ) s" hb: " ;
: REFUSE-TAIL$ ( -- ptr u8 n ) s"  is not in the x86-64 kernel" ;

\ Name the row on fd 2 and exit REFUSE-RC. The text follows the exit, inside
\ the record, as src/habu/boot-x64.f FAIL, places its own.
: REFUSE-BODY ( -- )
   LBL {: msg:label :}
   REFUSE-HEAD$ nip ROW-U @ + REFUSE-TAIL$ nip + 1+ {: len:n :}
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX len IMM32,  NR-WRITE SYS,
   REFUSE-RC EXIT-GROUP,
   msg LBL,
   REFUSE-HEAD$ TEXT,  ROW$ TEXT,  REFUSE-TAIL$ TEXT,
   STR-LF ASM-SINK BUF:APPEND-BYTE ;

public

\ Register the body a quotation emits under the row's name, skipped when the
\ tree shaker drops the row.
: PRIM ( ptr u8 n [ -- ] -- )
   ARGS
   ROW$ KEEP? 0= if exit then
   EMIT-ROW drop ;

\ PRIM whose record carries wid n: the twin of habu1.f FPRIM-WID.
: PRIM-WID ( ptr u8 n [ -- ] n -- ) {: wid:n :}
   ARGS
   ROW$ KEEP? 0= if exit then
   wid EMIT-ROW ENGINE-PRIMS:WID! ;

\ A row the x86-64 kernel does not carry: its body writes `hb: <name> is not
\ in the x86-64 kernel` on fd 2 and exits REFUSE-RC.
: REFUSE ( ptr u8 n -- ) [: REFUSE-BODY ;] PRIM ;

\ The first label of the body registered under the name: the address a `call`
\ enters it at.
: ENTRY-LABEL ( ptr u8 n -- label ) {: a:ptr u:n :}
   ENGINE-PRIMS:COUNT 0 ?do
      i ENGINE-PRIMS:NAME$ a u CORE-STR= if
         i ENGINE-PRIMS:FIRST-LABEL unloop exit
      then
   loop
   s" x64kernel: no registered body named " type a u type cr
   s" x64kernel: ENTRY-LABEL names no registered body" REFUSE-RC die ;

\ The twin of habu1.f B-TASK-LIVE-GUARD, for a body that moves engine state:
\ while a task is live it exits TASK-LIVE-RC. It clobbers rax and the flags, as
\ the ARM64 guard clobbers x9; X64ASM has no compare of memory with an
\ immediate.
: TASK-LIVE-GUARD, ( -- )
   RAX DATA-REG TASKS-LIVE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR
   C-NE LIVE-LBL JCC, ;

\ Guard the span the registers hold: move it into the (PROT-SPAN) ABI, rdi the
\ address and rsi the length, whichever registers hold them, then call it.
\ The twin of habu1.f PROT-GUARD:CALL. The call clobbers rax rcx rdx rsi rdi
\ and r8-r11, and the VM registers survive it.
: PROT-SPAN-CALL, ( r64 r64 -- ) {: addr:r64 len:r64 :}
   len RDI SAME? if
      addr RSI SAME? if
         RDI RSI ASM-SINK ENC-XCHG-RR                  \ crossed: swap them
      else
         RSI RDI ASM-SINK ENC-MOV-RR                   \ the length first
         addr RDI SAME? 0= if RDI addr ASM-SINK ENC-MOV-RR then
      then
   else
      addr RDI SAME? 0= if RDI addr ASM-SINK ENC-MOV-RR then
      len RSI SAME? 0= if RSI len ASM-SINK ENC-MOV-RR then
   then
   SPAN-LBL CALL, ;

private

: BIT ( r64 -- n ) R64>N 1 swap lshift ;

\ PROT-REC, and LPROTREC write rdi, rdx and rsi, and the syscall clobbers rax,
\ rcx and r11.
: REC-CLOBBERS? ( r64 -- bool )
   BIT
   RAX BIT RCX BIT or RDX BIT or RSI BIT or RDI BIT or R11 BIT or
   and 0<> ;

public

\ The twin of a habu1.f LPROTREC call site: mprotect the two target pages at
\ the register's page-aligned address to prot n, and record nothing. The
\ register survives, so a writer between an opening and a closing flip
\ addresses the same pages; a register LPROTREC clobbers is refused with
\ E-OPERAND. CF reports the kernel's refusal, as SYS, leaves it.
: PROT-REC, ( r64 n -- ) {: at:r64 prot:n :}
   at REC-CLOBBERS? if E-OPERAND throw then
   RDI at ASM-SINK ENC-MOV-RR
   RDX prot IMM32,
   REC-LBL CALL, ;

private

\ ---- the helpers -------------------------------------------------------------
\ One band of DATA-BANDS: the twin of habu1.f GUARD-BAND. rdi is the span's
\ start and rdx its end; a span that starts at or past the band's end misses
\ it, and one that ends past the band's start then overlaps it.
: BAND, ( n label -- ) {: ix:n trap:label :}
   LBL {: skip:label :}
   RAX DATA-REG ix DATA-BANDS:OFF ix DATA-BANDS:LEN + MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-AE skip JCC,
   RAX DATA-REG ix DATA-BANDS:OFF MEM-OFF ASM-SINK ENC-LEA
   RDX RAX ASM-SINK ENC-CMP-RR  C-A trap JCC,
   skip LBL, ;

: BANDS, ( label -- ) {: trap:label :}
   0 BEGIN dup DATA-BANDS:LEN 0 <> WHILE  dup trap BAND,  1+  REPEAT drop ;

\ The open transaction's blob: the twin of habu1.f GUARD:SPAN. With a blob
\ open, a span that ends past its start and starts below its end overlaps it.
: BLOB, ( label -- ) {: trap:label :}
   LBL {: skip:label :}
   RAX DATA-REG TXN-BLOB-A-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E skip JCC,
   RDX RAX ASM-SINK ENC-CMP-RR  C-BE skip JCC,
   RAX DATA-REG TXN-BLOB-CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   RDI RAX ASM-SINK ENC-CMP-RR  C-B trap JCC,
   skip LBL, ;

\ (PROT-SPAN) ( rdi = address, rsi = byte length ): the twin of habu1.f
\ GUARD-SPAN. It passes while the friend latch is open and for an empty span;
\ a span that wraps, meets a band or meets the open blob exits SEAL-VIOLATION.
\ The hull test skips the band walk for a span outside [DATA-BANDS:LO,
\ DATA-BANDS:HI), which is every DP heap address. Registered as an engine
\ helper, as habu1.f EMIT-PROT-SPAN registers the ARM64 body, so a compiled
\ word that reaches it by a direct call is carried and relocated with it.
: SPAN-HELPER, ( -- )
   SPAN-LBL {: start:label :}
   LBL LBL LBL {: end:label trap:label past:label :}
   LBL {: ok:label :}
   s" (PROT-SPAN)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   RAX DATA-REG FRIEND-LATCH-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   RSI RSI ASM-SINK ENC-TEST-RR  C-E ok JCC,
   RDX RDI RSI 1 0 MEM-IDX ASM-SINK ENC-LEA            \ the span's end
   RDX RDI ASM-SINK ENC-CMP-RR  C-B trap JCC,          \ unsigned wrap
   RAX DATA-REG DATA-BANDS:HI MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-AE past JCC,         \ start >= hull end
   RAX DATA-REG DATA-BANDS:LO MEM-OFF ASM-SINK ENC-LEA
   RDX RAX ASM-SINK ENC-CMP-RR  C-BE past JCC,         \ end <= hull start
   trap BANDS,
   past LBL,
   trap BLOB,
   ok LBL,
   ASM-SINK ENC-RET
   trap LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   end LBL, ;

\ LPROTREC ( rdi = an address in the first page, rdx = prot ): mprotect the two
\ PAGE-BYTES pages from rdi's page, as habu1.f LPROTREC flips two host pages.
: REC-HELPER, ( -- )
   REC-LBL LBL,
   RDI PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   RSI PAGE-BYTES 2 * IMM32,
   NR-MPROTECT SYS,
   ASM-SINK ENC-RET ;

: LIVE-HELPER, ( -- )
   LIVE-LBL LBL,
   TASK-LIVE-RC EXIT-GROUP, ;

\ Write the n bytes at the label on fd 2.
: STDERR-WRITE, ( label n -- ) {: msg:label len:n :}
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX len IMM32,  NR-WRITE SYS, ;

\ The machine-stack frame DIAG-U, writes digits into: room for any cell's.
CELL 4 * constant DIAG-BYTES

\ rax as unsigned decimal on fd 2, no newline: the twin of habu2.f LDIAGU,
\ which a refusal with a count to state calls instead of `u.`, since a
\ diagnostic never leaves through the output device. It clobbers rax rcx rdx
\ rsi rdi r11 and keeps r8.
: DIAG-U, ( -- )
   RSP DIAG-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RSI RSP DIAG-BYTES MEM-OFF ASM-SINK ENC-LEA
   DIGITS,
   RDX RSP DIAG-BYTES MEM-OFF ASM-SINK ENC-LEA
   RDX RSI ASM-SINK ENC-SUB-RR
   RDI STDERR IMM32,  NR-WRITE SYS,
   RSP DIAG-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ The refusal line of habu2.f LDPBAD, before and between its two numbers.
: DPBAD-HEAD$ ( -- ptr u8 n ) s" hb: data space out of range: DP " ;
: DPBAD-OF$ ( -- ptr u8 n ) s"  of " ;
: DPBAD-UNIT$ ( -- ptr u8 n ) s"  bytes" ;

\ The status a refused DP exits with: habu2.f LDPBAD's.
76 constant DPBAD-RC

\ LDPBAD ( rdi = the refused DP ): the twin of habu2.f LDPBAD. It writes
\ `hb: data space out of range: DP <dp> of <ceiling> bytes` on fd 2, both
\ numbers offsets from DATA, and exits DPBAD-RC. On ARM64 it continues into
\ LCOMPILEDIE, whose throw inside `evaluate` this engine has no interpreter
\ to catch yet (CONTROL,), so the refusal is always the top level's exit.
\ r8 carries the DP across the writes: a syscall preserves it.
: DPBAD-HELPER, ( -- )
   LBL LBL LBL {: head:label of:label unit:label :}
   DPBAD-LBL LBL,
   R8 RDI ASM-SINK ENC-MOV-RR  R8 DATA-REG ASM-SINK ENC-SUB-RR
   head DPBAD-HEAD$ nip STDERR-WRITE,
   RAX R8 ASM-SINK ENC-MOV-RR  DIAG-U,
   of DPBAD-OF$ nip STDERR-WRITE,
   RAX DP-CEILING IMM32,  DIAG-U,
   unit DPBAD-UNIT$ nip 1+ STDERR-WRITE,
   DPBAD-RC EXIT-GROUP,
   head LBL,  DPBAD-HEAD$ TEXT,
   of LBL,  DPBAD-OF$ TEXT,
   unit LBL,  DPBAD-UNIT$ TEXT,  STR-LF ASM-SINK BUF:APPEND-BYTE ;

\ (GENIO-OUT) ( rdi = device index, rsi = span, rdx = length ): the output
\ funnel's device arm at X64RT's LGENIOOUT, the twin of habu2.f
\ EMIT-GENIO-OUT. It writes fd 1 while GENIO-ABI:BUSY-CELL is set, for an
\ index past DEVICES (unsigned, so a negative one too) and for an empty row.
\ Otherwise it saves ACTIVE-CELL on the machine stack, marks the write as the
\ device's and the funnel busy, calls row index-1's xt with the span and
\ length on the data stack, and clears BUSY-CELL and restores ACTIVE-CELL.
\ A device's own output meanwhile finds the funnel busy and reaches fd 1.
\ Registered as an engine helper, as EMIT-GENIO-OUT registers the ARM64 arm.
: GENIO-HELPER, ( -- )
   LGENIOOUT @ >LABEL {: start:label :}
   LBL LBL {: term:label end:label :}
   s" (GENIO-OUT)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   RAX DATA-REG GENIO-ABI:BUSY-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE term JCC,
   RDI GENIO-ABI:DEVICES >IMM8 ASM-SINK ENC-CMP-RI8  C-A term JCC,
   RAX DATA-REG RDI CELL GENIO-ABI:WRITE-OFF CELL - MEM-IDX ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E term JCC,
   RCX DATA-REG GENIO-ABI:ACTIVE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX ASM-SINK ENC-PUSH
   RDI DATA-REG GENIO-ABI:ACTIVE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX 1 IMM32,  RCX DATA-REG GENIO-ABI:BUSY-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RSI R64>N G-PUSH  RDX R64>N G-PUSH
   RAX ASM-SINK ENC-CALL-REG
   RCX ZERO-REG,  RCX DATA-REG GENIO-ABI:BUSY-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ASM-SINK ENC-POP
   RCX DATA-REG GENIO-ABI:ACTIVE-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   ASM-SINK ENC-RET
   term LBL,
   RDI 1 IMM32,  NR-WRITE SYS,
   ASM-SINK ENC-RET
   end LBL, ;

\ ---- the dictionary index and the one-wordlist search -------------------------
\ The twins of habu1.f EMIT-HIDX and WLFIND:EMIT. The index at HIDXP-CELL is
\ HIDX-SLOTS u32 slots, each 0 (empty) or a record index plus one, keyed on
\ C-HIDX-HASH's hash of the folded name XOR the record's wid. It is
\ insert-once: the definer refuses a second live row for one folded name in
\ one wordlist, so an empty slot on a key's chain means absent.

public

\ A dictionary record's cells, from its start at r13 + index * DREC: the code
\ cell, the flags or'd with the name's length (DNAME-LEN-MASK), the name
\ inline up to DNAME-INL bytes or, with DNAME-EXT, a pointer to its bytes, and
\ the wid.
0 constant REC-CODE
16 constant REC-FLAGS
24 constant REC-NAME
40 constant REC-WID

private

$CBF29CE484222325 constant FNV-BASIS
$100000001B3 constant FNV-PRIME
$41 constant FOLD-FIRST                \ A
$5A constant FOLD-LAST                 \ Z
$20 constant FOLD-BIT
3 constant PROT-RW                     \ PROT_READ|PROT_WRITE
74 constant INDEX-RC                   \ habu1.f's exit for an index it cannot keep

: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: NDICT-REG ( -- r64 ) ENGINE-GPR:X64-NDICT >R64 ;

variable FIND-CELL
variable BUILD-CELL
variable ADD-CELL
variable REBUILD-CELL
variable FULL-CELL
: FIND-LBL ( -- label ) FIND-CELL @ >LABEL ;
: BUILD-LBL ( -- label ) BUILD-CELL @ >LABEL ;
: ADD-LBL ( -- label ) ADD-CELL @ >LABEL ;
: REBUILD-LBL ( -- label ) REBUILD-CELL @ >LABEL ;
: FULL-LBL ( -- label ) FULL-CELL @ >LABEL ;

: IMM64, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

\ Fold the byte in the register A-Z to a-z, as every name compare does.
: FOLD, ( r64 -- ) {: r:r64 :}
   LBL {: done:label :}
   r FOLD-FIRST >IMM8 ASM-SINK ENC-CMP-RI8  C-B done JCC,
   r FOLD-LAST >IMM8 ASM-SINK ENC-CMP-RI8  C-A done JCC,
   r FOLD-BIT >IMM8 ASM-SINK ENC-OR-RI8
   done LBL, ;

\ The twin of C-HIDX-HASH: h = the FNV-1a hash of the folded name at `name`,
\ `len` bytes, through the cursor, byte and prime registers.
: HASH, ( r64 r64 r64 r64 r64 r64 -- )
   {: name:r64 len:r64 h:r64 cur:r64 byte:r64 prime:r64 :}
   LBL LBL {: next:label done:label :}
   h FNV-BASIS IMM64,
   prime FNV-PRIME IMM64,
   cur ZERO-REG,
   next LBL,
   cur len ASM-SINK ENC-CMP-RR  C-GE done JCC,
   byte name cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   byte FOLD,
   h byte ASM-SINK ENC-XOR-RR
   h prime ASM-SINK ENC-IMUL-RR
   cur ASM-SINK ENC-INC
   next JMP,
   done LBL, ;

\ The hash's first slot: XOR the wid, masked to the table.
: SLOT, ( r64 r64 -- ) {: h:r64 wid:r64 :}
   h wid ASM-SINK ENC-XOR-RR
   h HIDX-SLOTS 1- >IMM32 ASM-SINK ENC-AND-RI32 ;

\ The next slot on the chain.
: NEXT-SLOT, ( r64 -- ) {: slot:r64 :}
   slot ASM-SINK ENC-INC
   slot HIDX-SLOTS 1- >IMM32 ASM-SINK ENC-AND-RI32 ;

\ The table's slot value, a record index plus one or 0, through rax.
: SLOT@, ( r64 r64 -- ) {: dst:r64 slot:r64 :}
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   dst R64>N >R32 RAX slot 4 0 MEM-IDX ASM-SINK ENC-MOV32-RM ;

\ A record index becomes its record's address in place.
: ROW, ( r64 -- ) {: r:r64 :}
   r r DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   r DBASE-REG ASM-SINK ENC-ADD-RR ;

\ The length of the record's name, through a scratch register.
: NAME-LEN, ( r64 r64 r64 -- ) {: row:r64 dst:r64 tmp:r64 :}
   dst row REC-FLAGS MEM-OFF ASM-SINK ENC-MOV-RM
   tmp DNAME-LEN-MASK IMM64,
   dst tmp ASM-SINK ENC-AND-RR ;

\ The address of the record's name bytes, through a scratch register.
: NAME-AT, ( r64 r64 r64 -- ) {: row:r64 dst:r64 tmp:r64 :}
   LBL {: inline:label :}
   dst row REC-NAME MEM-OFF ASM-SINK ENC-LEA
   tmp DNAME-EXT IMM64,
   tmp row REC-FLAGS MEM-OFF ASM-SINK ENC-TEST-MR  C-E inline JCC,
   dst row REC-NAME MEM-OFF ASM-SINK ENC-MOV-RM
   inline LBL, ;

\ Compare `len` bytes at a and b, folded, through the cursor and two byte
\ registers: a mismatch jumps to `miss`, a match falls through.
: SAME-NAME, ( r64 r64 r64 r64 r64 r64 label -- )
   {: a:r64 b:r64 len:r64 cur:r64 x:r64 y:r64 miss:label :}
   LBL LBL {: next:label done:label :}
   cur ZERO-REG,
   next LBL,
   cur len ASM-SINK ENC-CMP-RR  C-GE done JCC,
   x a cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM  x FOLD,
   y b cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM  y FOLD,
   x y ASM-SINK ENC-CMP-RR  C-NE miss JCC,
   cur ASM-SINK ENC-INC
   next JMP,
   done LBL, ;

\ Name the failure on fd 2 and exit INDEX-RC, with habu1.f's bytes. The text
\ follows the exit.
: INDEX-FAIL, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: msg:label :}
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX u IMM32,  NR-WRITE SYS,
   INDEX-RC EXIT-GROUP,
   msg LBL,
   a u TEXT, ;

\ WLFIND:LENTRY's twin ( rdi = name, rsi = length, rdx = wid ): rax = the
\ name's record in that one wordlist, or 0; the record's code cell is its first.
\ It keeps rdi, rsi and rdx and clobbers rcx and r8-r11. With a table it probes
\ the key's chain; with none, for DICT-WL:RETIRED (stamped onto rows already
\ indexed under another wid, so no chain holds them), and after a chain walked
\ through every slot, it scans. The scan's answer is the LAST matching record,
\ which is the probe's one record wherever a wid holds one row per name.
: FIND-HELPER, ( -- )
   LBL LBL LBL LBL {: probe:label miss:label next:label absent:label :}
   LBL LBL LBL {: scan:label row:label skip:label :}
   LBL {: done:label :}
   FIND-LBL LBL,
   RDX DICT-WL:RETIRED >IMM8 ASM-SINK ENC-CMP-RI8  C-E scan JCC,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E scan JCC,
   RDI RSI R8 R9 R10 R11 HASH,
   R8 RDX SLOT,                                        \ r8 = the slot
   R9 HIDX-SLOTS IMM32,                                \ r9 = slots left to walk
   probe LBL,
   R10 R8 SLOT@,
   R10 R10 ASM-SINK ENC-TEST-RR  C-E absent JCC,       \ an empty slot
   R10 ASM-SINK ENC-DEC
   R10 NDICT-REG ASM-SINK ENC-CMP-RR  C-GE next JCC,   \ a stale, rolled back one
   R10 ROW,
   RDX R10 REC-WID MEM-OFF ASM-SINK ENC-CMP-RM  C-NE next JCC,
   R10 R11 RAX NAME-LEN,
   R11 RSI ASM-SINK ENC-CMP-RR  C-NE next JCC,
   R10 R11 RAX NAME-AT,
   R10 ASM-SINK ENC-PUSH                               \ the compare needs r10
   RDI R11 RSI RCX RAX R10 miss SAME-NAME,
   RAX ASM-SINK ENC-POP
   ASM-SINK ENC-RET
   miss LBL,
   RAX ASM-SINK ENC-POP
   next LBL,
   R9 ASM-SINK ENC-DEC  C-E scan JCC,
   R8 NEXT-SLOT,
   probe JMP,
   absent LBL,
   RAX ZERO-REG,
   ASM-SINK ENC-RET
   scan LBL,
   R8 ZERO-REG,                                        \ r8 = the last match
   R9 DBASE-REG ASM-SINK ENC-MOV-RR                    \ r9 = the record
   row LBL,
   RAX NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX DBASE-REG ASM-SINK ENC-ADD-RR
   R9 RAX ASM-SINK ENC-CMP-RR  C-AE done JCC,          \ past the last record
   RDX R9 REC-WID MEM-OFF ASM-SINK ENC-CMP-RM  C-NE skip JCC,
   R9 R11 RAX NAME-LEN,
   R11 RSI ASM-SINK ENC-CMP-RR  C-NE skip JCC,
   R9 R11 RAX NAME-AT,
   RDI R11 RSI RCX RAX R10 skip SAME-NAME,
   R8 R9 ASM-SINK ENC-MOV-RR
   skip LBL,
   R9 DREC >IMM8 ASM-SINK ENC-ADD-RI8
   row JMP,
   done LBL,
   RAX R8 ASM-SINK ENC-MOV-RR
   ASM-SINK ENC-RET ;

\ The twin of C-HIDX-INS: index record rdi, which it keeps, at the first empty
\ or stale slot of its key's chain. Claiming an empty slot counts one more
\ HIDX:CLAIMS; a chain walked through every slot is HIDX:LFULL's. It clobbers
\ rax rcx rdx rsi and r8-r11.
: INSERT, ( -- )
   LBL LBL LBL {: probe:label empty:label put:label :}
   RAX RDI ASM-SINK ENC-MOV-RR  RAX ROW,
   RCX RAX REC-WID MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RDX R8 NAME-LEN,
   RAX RSI R8 NAME-AT,
   RSI RDX R8 R9 R10 R11 HASH,
   R8 RCX SLOT,                                        \ r8 = the slot
   R9 HIDX-SLOTS IMM32,                                \ r9 = slots left to walk
   probe LBL,
   R10 R8 SLOT@,
   R10 R10 ASM-SINK ENC-TEST-RR  C-E empty JCC,
   R10 ASM-SINK ENC-DEC
   R10 NDICT-REG ASM-SINK ENC-CMP-RR  C-GE put JCC,    \ stale: reuse its claim
   R9 ASM-SINK ENC-DEC  C-E FULL-LBL JCC,
   R8 NEXT-SLOT,
   probe JMP,
   empty LBL,
   R10 DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-RM
   R10 ASM-SINK ENC-INC
   R10 DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   put LBL,
   R10 RDI 1 MEM-OFF ASM-SINK ENC-LEA
   R10 R64>N >R32 RAX R8 4 0 MEM-IDX ASM-SINK ENC-MOV32-MR ;

\ HIDX:LREBUILD's twin: zero the table and HIDX:CLAIMS, then index the live
\ records [0, r14). With no table it returns at once; a dictionary at
\ HIDX:LOAD-MAX, which the compaction could not bring under the bound, is
\ HIDX:LFULL's.
: REBUILD-HELPER, ( -- )
   LBL LBL LBL {: zero:label fill:label done:label :}
   REBUILD-LBL LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NDICT-REG HIDX:LOAD-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-GE FULL-LBL JCC,
   RCX ZERO-REG,                                       \ rcx = the byte offset
   RDX ZERO-REG,
   zero LBL,
   RDX RAX RCX 1 0 MEM-IDX ASM-SINK ENC-MOV-MR
   RCX CELL >IMM8 ASM-SINK ENC-ADD-RI8
   RCX HIDX-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B zero JCC,
   RDX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   RDI ZERO-REG,                                       \ rdi = the record
   fill LBL,
   RDI NDICT-REG ASM-SINK ENC-CMP-RR  C-GE done JCC,
   INSERT,
   RDI ASM-SINK ENC-INC
   fill JMP,
   done LBL,
   ASM-SINK ENC-RET ;

\ LHIDXADD's twin: index record r14 - 1, the one just published, and compact
\ the table once HIDX:CLAIMS reaches HIDX:LOAD-MAX. With no table it returns at
\ once.
: ADD-HELPER, ( -- )
   LBL {: done:label :}
   ADD-LBL LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RDI NDICT-REG -1 MEM-OFF ASM-SINK ENC-LEA
   INSERT,
   RAX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-RM
   RAX HIDX:LOAD-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-L done JCC,
   REBUILD-LBL CALL,
   done LBL,
   ASM-SINK ENC-RET ;

\ LHIDXBUILD's twin: map the table once, HIDX-BYTES of fresh zero pages with no
\ claims, then rebuild it; a table already mapped is refilled in place.
: BUILD-HELPER, ( -- )
   LBL LBL {: have:label fail:label :}
   BUILD-LBL LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE have JCC,
   RDI ZERO-REG,  RSI HIDX-BYTES IMM32,  RDX PROT-RW IMM32,
   R10 MAP-ANON-PRIVATE IMM32,  R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B fail JCC,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ZERO-REG,
   RCX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   have LBL,
   REBUILD-LBL JMP,                                    \ its ret is this one's
   fail LBL,
   s" hb: dictionary index alloc failed" INDEX-FAIL, ;

\ HIDX:LFULL's twin: the index cannot be kept, which is loud, never a quiet
\ fall back to the scan.
: FULL-HELPER, ( -- )
   FULL-LBL LBL,
   s" hb: dictionary index exhausted" INDEX-FAIL, ;

: INDEX-HELPERS, ( -- )
   LBL FIND-CELL !  LBL BUILD-CELL !  LBL ADD-CELL !  LBL REBUILD-CELL !
   LBL FULL-CELL !
   FIND-HELPER,  BUILD-HELPER,  ADD-HELPER,  REBUILD-HELPER,  FULL-HELPER, ;

public

\ The index's call sites, the twins of habu1.f's LHIDXBUILD, LHIDXADD and
\ HIDX:LREBUILD calls, for the rows that move r14: build the table (a refused
\ mapping exits 74 with `hb: dictionary index alloc failed`), index the record
\ just published, and rebuild the table over [0, r14). Each call keeps every VM
\ register and clobbers rax rcx rdx rsi rdi and r8-r11; one that cannot keep
\ the index exits 74 with `hb: dictionary index exhausted`.
: HIDX-BUILD, ( -- ) BUILD-LBL CALL, ;
: HIDX-ADD, ( -- ) ADD-LBL CALL, ;
: HIDX-REBUILD, ( -- ) REBUILD-LBL CALL, ;

\ Emit the helpers, making their labels in this stream first: every section
\ that calls one follows.
: HELPERS, ( -- )
   LBL SPAN-CELL !  LBL REC-CELL !  LBL LIVE-CELL !
   LBL DPBAD-CELL !  LBL LGENIOOUT !
   SPAN-HELPER,  REC-HELPER,  LIVE-HELPER,  INDEX-HELPERS,
   DPBAD-HELPER,  GENIO-HELPER, ;

\ Each section's rows are tabled in docs/x86-64.md under the heading of its
\ name.

\ ---- syscall rows ------------------------------------------------------------
\ The twins of habu1.f's file, memory, time and identity bodies. A row pops its
\ arguments into the syscall registers, rdi rsi rdx r10 r8 r9 (the order
\ src/os/linux-x86-64/sys.f maps x0..x5 onto), stages the *at* family's
\ AT_FDCWD and flags, traps through SYS, and pushes through X64RT:SYS-PUSH: the
\ result, or -1 when the kernel refused.
\
\ A row whose kernel call writes through a caller's span guards it first. The
\ guard clobbers every argument register, so it reads the span from the data
\ stack before the pops.

private

$10 constant MAP-FIXED              \ the same bit in the BSD and Linux words
$90 constant STAT-BYTES             \ the x86-64 struct stat newfstatat writes
$5401 constant TCGETS
$5402 constant TCSETS
36 constant TERMIOS-BYTES           \ the struct termios TCGETS writes
1000000000 constant NS-PER-S

: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: POP, ( r64 -- ) R64>N G-POP ;
: PUSH, ( r64 -- ) R64>N G-PUSH ;
: DROP, ( -- ) DSP CELL >IMM8 ASM-SINK ENC-SUB-RI8 ;
: AT-FDCWD, ( r64 -- ) AT-FDCWD >IMM32 ASM-SINK ENC-MOV-RI32 ;
: SYS-PUSH, ( n -- ) SYS, SYS-PUSH ;

\ Load the cell n below the top of the data stack, which stays where it is.
: PEEK, ( r64 n -- ) {: r:r64 depth:n :}
   r DSP depth 1+ CELL * negate MEM-OFF ASM-SINK ENC-MOV-RM ;

\ Guard the span whose address and length are the cells a and l below the top.
: SPAN-GUARD, ( n n -- ) {: a:n l:n :}
   RDI a PEEK,  RSI l PEEK,  RDI RSI PROT-SPAN-CALL, ;

\ Guard n bytes at the address the cell a below the top holds.
: SIZED-GUARD, ( n n -- ) {: a:n bytes:n :}
   RDI a PEEK,  RSI bytes IMM32,  RDI RSI PROT-SPAN-CALL, ;

: OPEN-ROW ( -- )                   \ ( path flags mode -- fd|-1 )
   R10 POP,  RSI POP,  RDI POP,
   OS-OPEN-FLAGS                    \ rsi's BSD-shaped flags into rdx
   RSI RDI ASM-SINK ENC-MOV-RR  RDI AT-FDCWD,
   NR-OPEN SYS-PUSH, ;

\ The twin of habu1.f GUARD-IOCTL, over the request and the argument still on
\ the stack. TCGETS writes a termios and TCSETS only reads one; an _IOC request
\ the kernel writes through (_IOC_READ in bits 30-31) names its size in bits
\ 16-29, and one it only reads needs no guard. Any other request has no size
\ the guard could check, so it fails closed.
: IOCTL-GUARD, ( -- )
   LBL LBL LBL {: legacy:label write:label span:label :}
   LBL LBL {: trap:label done:label :}
   RAX 1 PEEK,  RDI 0 PEEK,
   RAX TCGETS >IMM32 ASM-SINK ENC-CMP-RI32  C-E legacy JCC,
   RAX TCSETS >IMM32 ASM-SINK ENC-CMP-RI32  C-E done JCC,
   RCX RAX ASM-SINK ENC-MOV-RR
   RCX 30 >IMM8 ASM-SINK ENC-SHR-RI8  RCX 3 >IMM8 ASM-SINK ENC-AND-RI8
   RCX 2 >IMM32 ASM-SINK ENC-TEST-RI32  C-NE write JCC,
   RCX RCX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   trap JMP,
   legacy LBL,
   RSI TERMIOS-BYTES IMM32,  span JMP,
   write LBL,
   RSI RAX ASM-SINK ENC-MOV-RR
   RSI 16 >IMM8 ASM-SINK ENC-SHR-RI8  RSI $3FFF >IMM32 ASM-SINK ENC-AND-RI32
   C-E done JCC,
   span LBL,
   RDI RSI PROT-SPAN-CALL,  done JMP,
   trap LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   done LBL, ;

\ Fresh anonymous storage: ( bytes -- ptr ior ), a null pointer and -1 for a
\ length that is not positive or a mapping the kernel refuses.
: MAP-ANON-ROW ( -- )
   LBL LBL {: failed:label done:label :}
   RSI POP,
   RSI RSI ASM-SINK ENC-TEST-RR  C-LE failed JCC,
   RDI ZERO-REG,  RDX PROT-RW IMM32,  R10 MAP-ANON-PRIVATE IMM32,
   R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B failed JCC,
   RCX ZERO-REG,  done JMP,
   failed LBL,
   RAX ZERO-REG,  RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   done LBL,
   0 G-PUSH  1 G-PUSH ;

\ ( addr len prot flags fd off -- addr|-1 ). Only MAP_FIXED replaces a mapping
\ that stands, so only it guards the span.
: MMAP-ROW ( -- )
   LBL {: placed:label :}
   RAX 2 PEEK,  RAX MAP-FIXED >IMM32 ASM-SINK ENC-TEST-RI32  C-E placed JCC,
   5 4 SPAN-GUARD,
   placed LBL,
   R9 POP,  R8 POP,  R10 POP,  RDX POP,  RSI POP,  RDI POP,
   OS-MMAP-FLAGS
   NR-MMAP SYS-PUSH, ;

\ newfstatat writes the x86-64 struct stat, whose st_mode sits at 24 behind
\ three u64s where aarch64's sits at 16; the size and the two times sit where
\ aarch64's do. The fix rewrites the layout lib/fs.f reads, as habu1.f
\ LINUX-STAT-FIX does: the mode at 4, the modification time at 48 and 56, the
\ change time at 64 and 72 and the size at 96. Every field is loaded before
\ one is stored, since the size and the modification time trade places.
: STAT-FIX, ( r64 -- ) {: buf:r64 :}
   1 >R32 buf 24 MEM-OFF ASM-SINK ENC-MOV32-RM
   1 >R32 buf 4 MEM-OFF ASM-SINK ENC-MOV32-MR
   RSI buf 48 MEM-OFF ASM-SINK ENC-MOV-RM
   RDI buf 88 MEM-OFF ASM-SINK ENC-MOV-RM
   R8 buf 96 MEM-OFF ASM-SINK ENC-MOV-RM
   R9 buf 104 MEM-OFF ASM-SINK ENC-MOV-RM
   R10 buf 112 MEM-OFF ASM-SINK ENC-MOV-RM
   RSI buf 96 MEM-OFF ASM-SINK ENC-MOV-MR
   RDI buf 48 MEM-OFF ASM-SINK ENC-MOV-MR
   R8 buf 56 MEM-OFF ASM-SINK ENC-MOV-MR
   R9 buf 64 MEM-OFF ASM-SINK ENC-MOV-MR
   R10 buf 72 MEM-OFF ASM-SINK ENC-MOV-MR ;

\ ( path buf -- 0|-1 ) through newfstatat with the flags n. The buffer is still
\ in rdx after the trap, and the fix runs only when the row pushed 0.
: STAT-ROW ( n n -- ) {: nr:n flags:n :}
   LBL {: done:label :}
   0 STAT-BYTES SIZED-GUARD,
   RDX POP,  RSI POP,  RDI AT-FDCWD,  R10 flags IMM32,
   nr SYS-PUSH,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RDX STAT-FIX,
   done LBL, ;

: GTOD ( n -- mem ) {: off:n :} DATA-REG GTOD-SCRATCH off + MEM-OFF ;

\ A time call cannot fail with the arguments these rows give it. One that does
\ stops on ud2, as habu1.f's twins stop on brk.
: TIME-CHECK, ( -- )
   LBL {: ok:label :}
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   ASM-SINK ENC-UD2
   ok LBL, ;

: EPOCH-ROW ( -- )                  \ ( -- seconds ) gettimeofday's seconds
   RDI 0 GTOD ASM-SINK ENC-LEA  RSI ZERO-REG,
   NR-GETTIMEOFDAY SYS,  TIME-CHECK,
   RAX 0 GTOD ASM-SINK ENC-MOV-RM  0 G-PUSH ;

\ ( -- ns ) CLOCK_MONOTONIC as seconds * 10^9 + nanoseconds, where the ARM64
\ twin reads CNTVCT_EL0.
: MONO-ROW ( -- )
   RDI CLOCK-MONOTONIC IMM32,  RSI 0 GTOD ASM-SINK ENC-LEA
   NR-CLOCK-GETTIME SYS,  TIME-CHECK,
   RAX 0 GTOD ASM-SINK ENC-MOV-RM
   RAX RAX NS-PER-S >IMM32 ASM-SINK ENC-IMUL-RRI32
   RAX CELL GTOD ASM-SINK ENC-ADD-RM
   0 G-PUSH ;

public

\ Call the C function at rax under the SysV ABI: align rsp to 16 for the call
\ and restore it after. Both pushed copies are the entry rsp, so [rsp+8] holds
\ it whether the alignment took 0 or 8 more bytes. The VM registers survive by
\ the callee-saved rule; rax rcx rdx rsi rdi and r8-r11 do not.
: C-CALL, ( -- )
   RCX RSP ASM-SINK ENC-MOV-RR
   RCX ASM-SINK ENC-PUSH  RCX ASM-SINK ENC-PUSH
   RSP -16 >IMM8 ASM-SINK ENC-AND-RI8
   RAX ASM-SINK ENC-CALL-REG
   RSP RSP CELL MEM-OFF ASM-SINK ENC-MOV-RM ;

\ rax = dlsym(RTLD_DEFAULT, the NUL-terminated name rsi points at), 0 when the
\ loader has no such symbol: the twin of habu1.f LIBC-OS DLSYM. The loader's
\ slot sits LINUX-DLSYM-SLOT-OFF into the read-write segment, which starts the
\ text's size past the image base; the image base is the text base RBASE-CELL
\ holds less CODE-OFF, and the text size is the first program header's
\ p_filesz, IMAGE-TEXT-SIZE-OFF into the image.
: DLSYM, ( -- )
   RAX DATA-REG RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX CODE-OFF >IMM32 ASM-SINK ENC-SUB-RI32
   RAX RAX IMAGE-TEXT-SIZE-OFF MEM-OFF ASM-SINK ENC-ADD-RM
   RAX RAX LINUX-DLSYM-SLOT-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RDI ZERO-REG,
   C-CALL, ;

private

\ The realpath frame on the machine stack: the arguments, free, the result and
\ its length, then the two names dlsym is asked for, NUL-terminated.
0 constant RP-PATH
8 constant RP-DST
16 constant RP-CAP
24 constant RP-FREE
32 constant RP-RESULT
40 constant RP-LEN
48 constant RP-FREE-NAME            \ "free"
56 constant RP-REALPATH-NAME        \ "realpath", its NUL the next cell
72 constant RP-FRAME
$65657266 constant FREE-NAME
$687461706C616572 constant REALPATH-NAME

: RP ( n -- mem ) RSP swap MEM-OFF ;

\ ( pathz dst capacity -- length | -1 | -2 ): the twin of habu1.f LIBC-OS
\ REALPATH. libc resolves the path, symlinks included, and allocates the
\ result; only a result that fits with its NUL reaches dst, and free runs after
\ a copy and after a result too long. -1 is a resolution or loader failure, -2
\ a capacity with no room for the whole C string.
: REALPATH-ROW ( -- )
   LBL LBL LBL LBL {: badcap:label failed:label short:label release:label :}
   LBL LBL LBL LBL {: count:label counted:label copy:label done:label :}
   RDX POP,  RSI POP,  RDI POP,
   RSP RP-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   RDI RP-PATH RP ASM-SINK ENC-MOV-MR
   RSI RP-DST RP ASM-SINK ENC-MOV-MR
   RDX RP-CAP RP ASM-SINK ENC-MOV-MR
   RDX RDX ASM-SINK ENC-TEST-RR  C-LE badcap JCC,
   RSI RDX PROT-SPAN-CALL,
   RAX FREE-NAME IMM32,  RAX RP-FREE-NAME RP ASM-SINK ENC-MOV-MR
   RAX REALPATH-NAME >IMM64 ASM-SINK ENC-MOV-RI64
   RAX RP-REALPATH-NAME RP ASM-SINK ENC-MOV-MR
   RAX ZERO-REG,  RAX RP-REALPATH-NAME CELL + RP ASM-SINK ENC-MOV-MR
   RSI RP-FREE-NAME RP ASM-SINK ENC-LEA  DLSYM,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E failed JCC,
   RAX RP-FREE RP ASM-SINK ENC-MOV-MR
   RSI RP-REALPATH-NAME RP ASM-SINK ENC-LEA  DLSYM,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E failed JCC,
   RDI RP-PATH RP ASM-SINK ENC-MOV-RM  RSI ZERO-REG,  C-CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E failed JCC,
   RAX RP-RESULT RP ASM-SINK ENC-MOV-MR
   RCX RAX ASM-SINK ENC-MOV-RR  RDX ZERO-REG,
   count LBL,
   R8 RCX MEM-AT ASM-SINK ENC-MOVZX-8-RM
   R8 R8 ASM-SINK ENC-TEST-RR  C-E counted JCC,
   RCX ASM-SINK ENC-INC  RDX ASM-SINK ENC-INC  count JMP,
   counted LBL,
   RDX RP-LEN RP ASM-SINK ENC-MOV-MR
   RDX RP-CAP RP ASM-SINK ENC-CMP-RM  C-AE short JCC,
   RSI RP-RESULT RP ASM-SINK ENC-MOV-RM
   RDI RP-DST RP ASM-SINK ENC-MOV-RM
   RCX RDX ASM-SINK ENC-MOV-RR  RCX ASM-SINK ENC-INC
   copy LBL,
   R8 RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
   8 >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
   RSI ASM-SINK ENC-INC  RDI ASM-SINK ENC-INC
   RCX ASM-SINK ENC-DEC  C-NE copy JCC,
   release LBL,
   RDI RP-RESULT RP ASM-SINK ENC-MOV-RM
   RAX RP-FREE RP ASM-SINK ENC-MOV-RM  C-CALL,
   RAX RP-LEN RP ASM-SINK ENC-MOV-RM  done JMP,
   short LBL,
   RAX -2 >IMM32 ASM-SINK ENC-MOV-RI32  RAX RP-LEN RP ASM-SINK ENC-MOV-MR
   release JMP,
   badcap LBL,
   RAX -2 >IMM32 ASM-SINK ENC-MOV-RI32  done JMP,
   failed LBL,
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   done LBL,
   RSP RP-FRAME >IMM8 ASM-SINK ENC-ADD-RI8
   0 G-PUSH ;

public

\ The rows, one line each, in the order docs/x86-64.md "Syscall rows" tables
\ them.
: SYSCALLS, ( -- )
   s" open" [: OPEN-ROW ;] PRIM
   s" read" [: 1 0 SPAN-GUARD,  RDX POP, RSI POP, RDI POP,  NR-READ SYS-PUSH, ;] PRIM
   s" write" [: RDX POP, RSI POP, RDI POP,  NR-WRITE SYS-PUSH, ;] PRIM
   s" close" [: RDI POP,  NR-CLOSE SYS, ;] PRIM
   s" close-rc" [: RDI POP,  NR-CLOSE SYS-PUSH, ;] PRIM
   s" ioctl" [: IOCTL-GUARD,  RDX POP, RSI POP, RDI POP,  NR-IOCTL SYS-PUSH, ;] PRIM
   s" map-anon" [: MAP-ANON-ROW ;] PRIM
   s" mmap" [: MMAP-ROW ;] PRIM
   s" munmap" [: 1 0 SPAN-GUARD,  RSI POP, RDI POP,  NR-MUNMAP SYS-PUSH, ;] PRIM
   s" open-rd" [: RDI POP,  RDI R64>N OS-OPEN-RD  SYS-PUSH ;] PRIM
   s" access" [: RDX POP, RSI POP, RDI AT-FDCWD, R10 ZERO-REG,  NR-ACCESS SYS-PUSH, ;] PRIM
   s" unlink" [: RSI POP, RDI AT-FDCWD, RDX ZERO-REG,  NR-UNLINK SYS-PUSH, ;] PRIM
   s" rename" [: R10 POP, RSI POP, RDI AT-FDCWD, RDX AT-FDCWD,  NR-RENAME SYS-PUSH, ;] PRIM
   s" chmod" [: RDX POP, RSI POP, RDI AT-FDCWD, R10 ZERO-REG,  NR-CHMOD SYS-PUSH, ;] PRIM
   s" symlink" [: RDX POP, RDI POP, RSI AT-FDCWD,  NR-SYMLINKAT SYS-PUSH, ;] PRIM
   s" readlink" [: 1 0 SPAN-GUARD,  R10 POP, RDX POP, RSI POP, RDI AT-FDCWD,  NR-READLINKAT SYS-PUSH, ;] PRIM
   s" realpath" [: REALPATH-ROW ;] PRIM
   s" mkdir" [: RDX POP, RSI POP, RDI AT-FDCWD,  NR-MKDIR SYS-PUSH, ;] PRIM
   s" rmdir" [: RSI POP, RDI AT-FDCWD, RDX AT-REMOVEDIR IMM32,  NR-RMDIR SYS-PUSH, ;] PRIM
   s" stat64" [: NR-STAT64 0 STAT-ROW ;] PRIM
   s" lstat64" [: NR-LSTAT64 AT-SYMLINK-NOFOLLOW STAT-ROW ;] PRIM
   \ getdents64 keeps no base cookie: the pointer is guarded, as the macOS
   \ call writes through it, and dropped.
   s" getdirentries64" [: 2 1 SPAN-GUARD,  0 CELL SIZED-GUARD,  DROP,  RDX POP, RSI POP, RDI POP,  NR-GETDIRENTRIES64 SYS-PUSH, ;] PRIM
   s" epoch-seconds" [: EPOCH-ROW ;] PRIM
   s" mono-ns" [: MONO-ROW ;] PRIM
   s" getpid" [: NR-GETPID SYS-PUSH, ;] PRIM ;

\ ---- control rows ------------------------------------------------------------
\ The catch frame, STACK-ABI:CATCH-BYTES on the machine stack and chained
\ through HND-CELL, laid out as habu1.f BCATCH lays it: the byte offsets of its
\ cells below STACK-ABI:CATCH-BASE and CATCH-CAP.
0 constant CATCH-PREV       \ the handler this one hides
8 constant CATCH-DSP        \ the data stack pointer once the xt is popped
16 constant CATCH-MSP       \ rsp at CAUGHT,'s entry, above the frame
24 constant CATCH-RESUME    \ where a throw resumes, the code in rax
32 constant CATCH-RET       \ the cell at CATCH-MSP, as BCATCH saves x30
40 constant CATCH-RDEPTH    \ RSP-CELL, the return stack's depth
48 constant CATCH-LDEPTH    \ LOOPSP-CELL, the loop stack's depth
56 constant CATCH-SENTINEL  \ STACK-ABI:CATCH-MAGIC

\ The twins of habu1.f BEXEC, BEXECFLOOR, B2TOR, B2RFROM, B2RFETCH, BCATCH,
\ BTHROW, BFINALLY, BRUNSTACK and BDIE. An xt is a code address a body enters
\ by `call`; the code it runs owns every scratch register, so what a body needs
\ after the call waits on the machine stack.

private

\ mov r, [base + off] and mov [base + off], r.
: MOV-LOAD, ( r64 r64 n -- ) {: r:r64 base:r64 off:n :}
   r base off MEM-OFF ASM-SINK ENC-MOV-RM ;
: MOV-STORE, ( r64 r64 n -- ) {: r:r64 base:r64 off:n :}
   r base off MEM-OFF ASM-SINK ENC-MOV-MR ;

: CORRUPT$ ( -- ptr u8 n ) s" hb: catch frame corrupt" ;

\ run-in-stack's frame: the caller's data stack pointer and descriptor.
0 constant RUN-DSP
8 constant RUN-BASE
16 constant RUN-CAP
24 constant RUN-BYTES

\ Exit with rdi when it is a status in [n, 255], and with UNCAUGHT-RC otherwise:
\ the kernel would keep only the low byte, so a negative code could exit 0.
\ The tails of habu1.f BDIE (n = 0) and BTHROW (n = 1).
: RC-EXIT, ( n -- ) {: lo:n :}
   LBL {: wide:label :}
   RAX RDI lo negate MEM-OFF ASM-SINK ENC-LEA
   RAX 255 lo - >IMM32 ASM-SINK ENC-CMP-RI32  C-A wide JCC,
   NR-EXIT-GROUP SYS,
   wide LBL,
   UNCAUGHT-RC EXIT-GROUP, ;

\ execute-floor ( n -- bool ): call the xt, then answer whether it left the
\ data stack below the base in S0-CELL. Below, the stack is reset to the base
\ before the flag is pushed, so the push lands at the base and not on the low
\ guard page.
: EXECUTE-FLOOR, ( -- )
   LBL {: above:label :}
   RAX POP,  RAX ASM-SINK ENC-CALL-REG
   RCX ZERO-REG,
   RAX DATA-REG S0-CELL MOV-LOAD,
   DSP RAX ASM-SINK ENC-CMP-RR  C-AE above JCC,
   DSP RAX ASM-SINK ENC-MOV-RR
   RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   above LBL,
   RCX PUSH, ;

\ The return stack is RSP-CELL cells deep from the base in
\ STACK-ABI:RETURN-BASE-CELL, as habu1.f RSTK-PUSH and RSTK-POP address it; a
\ push past RETURN-CELLS lands on its guard page. RSTACK, loads the depth into
\ rcx and the base into rsi, and RSLOT is the cell n cells past the depth.
: RSTACK, ( -- )
   RCX DATA-REG RSP-CELL MOV-LOAD,
   RSI DATA-REG STACK-ABI:RETURN-BASE-CELL MOV-LOAD, ;
: RSLOT ( n -- mem ) {: k:n :} RSI RCX CELL k CELL * MEM-IDX ;

\ 2>r ( n n -- ), R: ( -- n n ).
: 2>R, ( -- )
   RDX POP,  RAX POP,  RSTACK,
   RAX 0 RSLOT ASM-SINK ENC-MOV-MR  RDX 1 RSLOT ASM-SINK ENC-MOV-MR
   RCX 2 >IMM8 ASM-SINK ENC-ADD-RI8  RCX DATA-REG RSP-CELL MOV-STORE, ;

\ 2r> ( -- n n ), R: ( n n -- ).
: 2R>, ( -- )
   RSTACK,
   RCX 2 >IMM8 ASM-SINK ENC-SUB-RI8  RCX DATA-REG RSP-CELL MOV-STORE,
   RAX 0 RSLOT ASM-SINK ENC-MOV-RM  RDX 1 RSLOT ASM-SINK ENC-MOV-RM
   RAX PUSH,  RDX PUSH, ;

\ 2r@ ( -- n n ), R: ( n n -- n n ).
: 2R@, ( -- )
   RSTACK,
   RAX -2 RSLOT ASM-SINK ENC-MOV-RM  RDX -1 RSLOT ASM-SINK ENC-MOV-RM
   RAX PUSH,  RDX PUSH, ;

\ Pop an xt and call it under a catch frame, leaving 0 in rax when it returns
\ and its throw's code when it throws: the twin of BCATCH before its push. The
\ frame saves the caller's whole execution state, links in as the handler and
\ is unlinked on the return; a throw restores the state, unlinks the frame and
\ resumes after the unlink with the code in rax.
: CAUGHT, ( -- )
   LBL {: resume:label :}
   RAX POP,
   RSP STACK-ABI:CATCH-BYTES >IMM32 ASM-SINK ENC-SUB-RI32
   RCX DATA-REG HND-CELL MOV-LOAD,  RCX RSP CATCH-PREV MOV-STORE,
   DSP RSP CATCH-DSP MOV-STORE,
   RCX RSP STACK-ABI:CATCH-BYTES MEM-OFF ASM-SINK ENC-LEA
   RCX RSP CATCH-MSP MOV-STORE,
   RDX RCX 0 MOV-LOAD,  RDX RSP CATCH-RET MOV-STORE,
   RCX resume MOVABS,  RCX RSP CATCH-RESUME MOV-STORE,
   RCX DATA-REG RSP-CELL MOV-LOAD,  RCX RSP CATCH-RDEPTH MOV-STORE,
   RCX DATA-REG LOOPSP-CELL MOV-LOAD,  RCX RSP CATCH-LDEPTH MOV-STORE,
   RCX STACK-ABI:CATCH-MAGIC >IMM64 ASM-SINK ENC-MOV-RI64
   RCX RSP CATCH-SENTINEL MOV-STORE,
   RCX DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   RCX RSP STACK-ABI:CATCH-BASE MOV-STORE,
   RCX DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,
   RCX RSP STACK-ABI:CATCH-CAP MOV-STORE,
   RSP DATA-REG HND-CELL MOV-STORE,
   RAX ASM-SINK ENC-CALL-REG
   RCX RSP CATCH-PREV MOV-LOAD,  RCX DATA-REG HND-CELL MOV-STORE,
   RSP STACK-ABI:CATCH-BYTES >IMM32 ASM-SINK ENC-ADD-RI32
   RAX ZERO-REG,
   resume LBL, ;

\ Admit the handler frame in rdx before any store: its sentinel, both depths
\ within their stacks' capacities, and its data stack's descriptor and cursor
\ with room for a cell. A frame that fails branches to the label.
: FRAME-OK, ( label -- ) {: bad:label :}
   RCX STACK-ABI:CATCH-MAGIC >IMM64 ASM-SINK ENC-MOV-RI64
   RCX RDX CATCH-SENTINEL MEM-OFF ASM-SINK ENC-CMP-RM  C-NE bad JCC,
   RCX STACK-ABI:RETURN-CELLS IMM32,
   RCX RDX CATCH-RDEPTH MEM-OFF ASM-SINK ENC-CMP-RM  C-B bad JCC,
   RCX STACK-ABI:LOOP-FRAMES IMM32,
   RCX RDX CATCH-LDEPTH MEM-OFF ASM-SINK ENC-CMP-RM  C-B bad JCC,
   RSI RDX STACK-ABI:CATCH-BASE MOV-LOAD,
   RDI RDX STACK-ABI:CATCH-CAP MOV-LOAD,
   R8 RDX CATCH-DSP MOV-LOAD,
   RSI RDI R8 CELL bad CHECK-CURSOR ;

\ Restore the execution state the frame in rdx saved, unlink it and resume
\ where CAUGHT, left, with the code still in rax.
: RESUME, ( -- )
   DSP RDX CATCH-DSP MOV-LOAD,
   RCX RDX STACK-ABI:CATCH-BASE MOV-LOAD,
   RCX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RCX RDX STACK-ABI:CATCH-CAP MOV-LOAD,
   RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   RCX RDX CATCH-RDEPTH MOV-LOAD,  RCX DATA-REG RSP-CELL MOV-STORE,
   RCX RDX CATCH-LDEPTH MOV-LOAD,  RCX DATA-REG LOOPSP-CELL MOV-STORE,
   RCX RDX CATCH-PREV MOV-LOAD,  RCX DATA-REG HND-CELL MOV-STORE,
   RCX RDX CATCH-RET MOV-LOAD,
   RSI RDX CATCH-RESUME MOV-LOAD,
   RSP RDX CATCH-MSP MOV-LOAD,
   RCX RSP 0 MOV-STORE,
   RSI ASM-SINK ENC-JMP-REG ;

\ No handler: call the reporter xt in UNCGH-CELL, when one is installed, with
\ the code on the data stack, and then exit with the code, which waited on the
\ machine stack.
: UNCAUGHT, ( -- )
   LBL {: bare:label :}
   RAX ASM-SINK ENC-PUSH
   RCX DATA-REG UNCGH-CELL MOV-LOAD,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   RAX PUSH,  RCX ASM-SINK ENC-CALL-REG
   bare LBL,
   RDI ASM-SINK ENC-POP
   1 RC-EXIT, ;

\ Throw the code in rax: BTHROW without its evaluate and REPL arms, which jump
\ into habu2.f routines the x86-64 engine never has. Habu's `evaluate`
\ recovers through `catch`. A handler's frame is admitted, restored and resumed;
\ one that fails its admission writes `hb: catch frame corrupt` on fd 2 and
\ exits ENGINE-ERROR:CATCH-STACK. The text follows the exit.
: THROW, ( -- )
   LBL LBL LBL {: none:label corrupt:label msg:label :}
   RDX DATA-REG HND-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E none JCC,
   corrupt FRAME-OK,
   RESUME,
   none LBL,
   UNCAUGHT,
   corrupt LBL,
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX CORRUPT$ nip IMM32,  NR-WRITE SYS,
   ENGINE-ERROR:CATCH-STACK EXIT-GROUP,
   msg LBL,
   CORRUPT$ TEXT, ;

\ finally ( xt xt -- ): run the body under CAUGHT, and then the cleanup outside
\ it, so the cleanup's throw supersedes the body's; then rethrow the body's
\ code unless it is 0. The cleanup's xt and then the code wait in a two-cell
\ frame, which a throw inside the body leaves in place.
: FINALLY, ( -- )
   LBL {: done:label :}
   RAX POP,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP 0 MOV-STORE,
   CAUGHT,
   RAX RSP CELL MOV-STORE,
   RAX RSP 0 MOV-LOAD,  RAX ASM-SINK ENC-CALL-REG
   RAX RSP CELL MOV-LOAD,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   THROW,
   done LBL, ;

\ Branch to the label unless the registers name an extent a guarded stack
\ mapping could be: habu1.f GUARDED-EXTENT?. The base and the capacity are
\ nonzero and STACK-ABI:PAGE-BYTES aligned, their sum does not wrap, and the
\ base lies outside DATA, where every create, allot and buffer address lies, so
\ no dictionary buffer passes however it is aligned. Clobbers rsi and rdi.
: GUARDED-EXTENT, ( r64 r64 label -- ) {: base:r64 cap:r64 bad:label :}
   STACK-ABI:PAGE-BYTES 1- {: mask:n :}
   base base ASM-SINK ENC-TEST-RR  C-E bad JCC,
   cap cap ASM-SINK ENC-TEST-RR  C-E bad JCC,
   base mask >IMM32 ASM-SINK ENC-TEST-RI32  C-NE bad JCC,
   cap mask >IMM32 ASM-SINK ENC-TEST-RI32  C-NE bad JCC,
   RSI base cap 1 0 MEM-IDX ASM-SINK ENC-LEA
   RSI base ASM-SINK ENC-CMP-RR  C-B bad JCC,
   RDI base ASM-SINK ENC-MOV-RR
   RSI DATA-VA VA>N >IMM64 ASM-SINK ENC-MOV-RI64
   RDI RSI ASM-SINK ENC-SUB-RR
   RSI DATA-SIZE >IMM64 ASM-SINK ENC-MOV-RI64
   RDI RSI ASM-SINK ENC-CMP-RR  C-B bad JCC, ;

\ run-in-stack ( xt ptr u8 n -- ): run the xt on the extent as its data stack.
\ An extent that is not a guarded mapping is the caller's error, so it throws
\ STACK-ABI:E-STACK-UNGUARDED over the three cells, as BRUNSTACK does. The
\ extent becomes the data stack and its descriptor for the call; on the return
\ the caller's saved descriptor and cursor are admitted, since nothing proves
\ what the callback left of them, and restored.
: RUN-IN-STACK, ( -- )
   LBL LBL LBL {: unguarded:label bad:label done:label :}
   RAX DSP -3 CELL * MOV-LOAD,
   RDX DSP -2 CELL * MOV-LOAD,
   RCX DSP CELL negate MOV-LOAD,
   RDX RCX unguarded GUARDED-EXTENT,
   DSP 3 CELL * >IMM8 ASM-SINK ENC-SUB-RI8
   RSP RUN-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   DSP RSP RUN-DSP MOV-STORE,
   RSI DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,  RSI RSP RUN-BASE MOV-STORE,
   RSI DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,  RSI RSP RUN-CAP MOV-STORE,
   RDX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP RDX ASM-SINK ENC-MOV-RR
   RAX ASM-SINK ENC-CALL-REG
   RSI RSP RUN-BASE MOV-LOAD,  RDI RSP RUN-CAP MOV-LOAD,  RDX RSP RUN-DSP MOV-LOAD,
   RSI RDI RDX 0 bad CHECK-CURSOR
   RSI DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RDI RSP RUN-CAP MOV-LOAD,  RDI DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP RSP RUN-DSP MOV-LOAD,
   RSP RUN-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   done JMP,
   unguarded LBL,
   RAX STACK-ABI:E-STACK-UNGUARDED >IMM32 ASM-SINK ENC-MOV-RI32
   THROW,
   bad LBL,
   EXIT-BOUNDS
   done LBL, ;

\ die ( ptr u8 n n -- ): the message and LF on fd 2 when its length is
\ positive, then the exit hook, then the exit. The rc waits on the machine
\ stack, and the LF is a cell pushed there for its write. The hook's cell is
\ cleared before the call (layout.f EXIT-HOOK-CELL, habu2.f EMIT-EXITHOOK), so
\ a hook that dies or throws finds it empty.
: DIE, ( -- )
   LBL LBL {: quiet:label bare:label :}
   RAX POP,  RDX POP,  RSI POP,
   RAX ASM-SINK ENC-PUSH
   RDX RDX ASM-SINK ENC-TEST-RR  C-LE quiet JCC,
   RDI STDERR IMM32,  NR-WRITE SYS,
   RCX STR-LF IMM32,  RCX ASM-SINK ENC-PUSH
   RDI STDERR IMM32,  RSI RSP ASM-SINK ENC-MOV-RR  RDX 1 IMM32,  NR-WRITE SYS,
   RCX ASM-SINK ENC-POP
   quiet LBL,
   RAX DATA-REG EXIT-HOOK-CELL MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   RCX ZERO-REG,  RCX DATA-REG EXIT-HOOK-CELL MOV-STORE,
   RAX ASM-SINK ENC-CALL-REG
   bare LBL,
   RDI ASM-SINK ENC-POP
   0 RC-EXIT, ;

public

\ The interpreter-bound rows jump into habu2.f's interpreter on ARM64, which
\ the x86-64 engine never has. Lane I provides each in Habu; until then each
\ row refuses, and the registration is what ENGINE-PRIMS:COMPLETE needs
\ meanwhile.
: CONTROL, ( -- )
   s" execute" [: RAX POP,  RAX ASM-SINK ENC-CALL-REG ;] PRIM
   s" execute-floor" [: EXECUTE-FLOOR, ;] PRIM
   s" 2>r" [: 2>R, ;] PRIM
   s" 2r>" [: 2R>, ;] PRIM
   s" 2r@" [: 2R@, ;] PRIM
   s" catch" [: CAUGHT,  RAX PUSH, ;] PRIM
   s" throw" [: RAX POP,  THROW, ;] PRIM
   s" finally" [: FINALLY, ;] PRIM
   s" run-in-stack" [: RUN-IN-STACK, ;] PRIM
   s" die" [: DIE, ;] PRIM
   s" evaluate" REFUSE
   s" create" REFUSE
   s" parse-name" REFUSE
   s" num-parse" REFUSE
   s" tok-imm?" REFUSE ;

\ ---- atomics and publication rows --------------------------------------------
private

\ The twins of habu1.f BATFETCH .. BFENCE. x86-64 orders memory as total store
\ order: a plain load already has LDAR's acquire order, but a plain store may
\ pass a later load, which STLR followed by LDAR forbids, so the store is the
\ implicitly locked xchg. A row that writes guards the cell whose address is on
\ top of the data stack, and pops its operands only after the guard, which
\ preserves no scratch register.
: TOP-CELL-GUARD, ( -- )
   RDI ENGINE-GPR:X64-DSTACK >R64 CELL negate MEM-OFF ASM-SINK ENC-MOV-RM
   RSI CELL IMM32,
   RDI RSI PROT-SPAN-CALL, ;

\ atomic@ ( ptr a -- a )
: ATOMIC-FETCH, ( -- )
   0 X64RT:G-POP
   RAX RAX MEM-AT ASM-SINK ENC-MOV-RM
   0 X64RT:G-PUSH ;

\ atomic! ( a ptr a -- )
: ATOMIC-STORE, ( -- )
   TOP-CELL-GUARD,
   1 X64RT:G-POP  0 X64RT:G-POP
   RAX RCX MEM-AT ASM-SINK ENC-XCHG-MR ;

\ atomic-add ( n ptr n -- n ), answering the value the cell held.
: ATOMIC-ADD, ( -- )
   TOP-CELL-GUARD,
   1 X64RT:G-POP  0 X64RT:G-POP
   RAX RCX MEM-AT ASM-SINK ENC-LOCK-XADD-MR
   0 X64RT:G-PUSH ;

\ atomic-cas ( a a ptr a -- a ): the expected value in rax and the new one in
\ rdx. It answers the value the cell held, which cmpxchg leaves in rax whether
\ or not it matched.
: ATOMIC-CAS, ( -- )
   TOP-CELL-GUARD,
   1 X64RT:G-POP  2 X64RT:G-POP  0 X64RT:G-POP
   RDX RCX MEM-AT ASM-SINK ENC-LOCK-CMPXCHG-MR
   0 X64RT:G-PUSH ;

public

: ATOMICS, ( -- )
   s" atomic@" [: ATOMIC-FETCH, ;] PRIM
   s" atomic!" [: ATOMIC-STORE, ;] PRIM
   s" atomic-add" [: ATOMIC-ADD, ;] PRIM
   s" atomic-cas" [: ATOMIC-CAS, ;] PRIM
   s" fence" [: ASM-SINK ENC-MFENCE ;] PRIM ;

\ ---- engine-state rows -------------------------------------------------------

private

\ Pop the name and the wid into FIND-HELPER,'s registers.
: FIND-ARGS, ( -- )
   RDX R64>N X64RT:G-POP  RSI R64>N X64RT:G-POP  RDI R64>N X64RT:G-POP ;

\ `search-wl ( ptr u8 n n -- n )`, the twin of habu1.f BSWL: the code cell of
\ the name's record in one wordlist, or 0. An engine helper's wid
\ (OWNER-API-PRI-WID) is refused before the search and a DNAME-INT record
\ answers 0, so `search-wl execute` never reaches an internal word.
: SEARCH-WL-BODY ( -- )
   LBL LBL {: none:label push:label :}
   FIND-ARGS,
   RDX OWNER-API-PRI-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E none JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E push JCC,
   RCX DNAME-INT IMM64,
   RCX RAX REC-FLAGS MEM-OFF ASM-SINK ENC-TEST-MR  C-NE none JCC,
   RAX RAX REC-CODE MEM-OFF ASM-SINK ENC-MOV-RM
   push JMP,
   none LBL,
   RAX ZERO-REG,
   push LBL,
   RAX R64>N X64RT:G-PUSH ;

\ `xref-search-wl ( ptr u8 n n -- ptr n )`, the twin of habu1.f BCOMPILERSWL:
\ the record itself, or 0, with neither refusal; its checker row admits it
\ only at a trusted boundary.
: XREF-SEARCH-WL-BODY ( -- )
   FIND-ARGS,
   FIND-LBL CALL,
   RAX R64>N X64RT:G-PUSH ;

public

\ The dictionary search, tabled under "Engine-state rows".
: DICT-SEARCH, ( -- )
   s" search-wl" [: SEARCH-WL-BODY ;] PRIM
   s" xref-search-wl" [: XREF-SEARCH-WL-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID ;

private

: CP-REG ( -- r64 ) ENGINE-GPR:X64-CP >R64 ;

\ Load and store the DATA cell at an offset.
: CELL@, ( r64 n -- ) {: r:r64 off:n :}
   r DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM ;
: CELL!, ( r64 n -- ) {: r:r64 off:n :}
   r DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ The twin of habu1.f DP-CHECK, before every store of a new DP: rdi, the new
\ DP, must lie in [DATA + DATA-START, DATA + DP-CEILING], or LDPBAD refuses
\ it with rdi in hand. The compares are signed, as the ARM64 ones are. It
\ clobbers rax.
: DP-CHECK, ( -- )
   RAX DATA-REG DATA-START MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-L DPBAD-LBL JCC,
   RAX DATA-REG DP-CEILING MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-G DPBAD-LBL JCC, ;

\ The heap rows move DP, so each begins with the task-live guard, as the
\ ARM64 bodies do. `,` and c, check the DP past their store before it lands.
: HEAP, ( -- )
   s" here" [: RAX DP-CELL CELL@,  RAX PUSH, ;] PRIM
   s" allot" [:
      TASK-LIVE-GUARD,  RCX POP,  RDI DP-CELL CELL@,
      RDI RCX ASM-SINK ENC-ADD-RR  DP-CHECK,  RDI DP-CELL CELL!, ;] PRIM
   s" align" [:
      TASK-LIVE-GUARD,  RDI DP-CELL CELL@,
      RDI CELL 1- >IMM8 ASM-SINK ENC-ADD-RI8
      RDI CELL negate >IMM8 ASM-SINK ENC-AND-RI8
      DP-CHECK,  RDI DP-CELL CELL!, ;] PRIM
   s" ," [:
      TASK-LIVE-GUARD,  RCX POP,  RDX DP-CELL CELL@,
      RDI RDX CELL MEM-OFF ASM-SINK ENC-LEA  DP-CHECK,
      RCX RDX MEM-AT ASM-SINK ENC-MOV-MR  RDI DP-CELL CELL!, ;] PRIM
   s" c," [:
      TASK-LIVE-GUARD,  RCX POP,  RDX DP-CELL CELL@,
      RDI RDX 1 MEM-OFF ASM-SINK ENC-LEA  DP-CHECK,
      RCX R64>N >R8 RDX MEM-AT ASM-SINK ENC-MOV8-MR  RDI DP-CELL CELL!, ;] PRIM ;

\ .s ( -- ): print each cell from the base up, one per line, and leave them.
\ The cursor lives in SSCR-CELL, as on ARM64: a device write calls an xt that
\ may clobber every scratch register.
: DOT-S-BODY ( -- )
   LBL LBL {: loop:label done:label :}
   RAX S0-CELL CELL@,  RAX SSCR-CELL CELL!,
   loop LBL,
   RAX SSCR-CELL CELL@,  RAX DSP ASM-SINK ENC-CMP-RR  C-AE done JCC,
   RAX RAX MEM-AT ASM-SINK ENC-MOV-RM  G-PRINT9
   RAX SSCR-CELL CELL@,  RAX CELL >IMM8 ASM-SINK ENC-ADD-RI8
   RAX SSCR-CELL CELL!,
   loop JMP,
   done LBL, ;

\ Every printer leaves through X64RT's G-OUT, which honours OUT-CELL.
: PRINTERS, ( -- )
   s" ." [: RAX POP,  G-PRINT9 ;] PRIM
   s" u." [: RAX POP,  G-PRINTU9 ;] PRIM
   s" .s" [: DOT-S-BODY ;] PRIM
   s" depth" [:
      RAX DSP ASM-SINK ENC-MOV-RR
      RAX DATA-REG S0-CELL MEM-OFF ASM-SINK ENC-SUB-RM
      RAX 3 >IMM8 ASM-SINK ENC-SHR-RI8  RAX PUSH, ;] PRIM
   s" emit" [: RAX POP,  G-EMITC ;] PRIM
   s" cr" [: RAX STR-LF IMM32,  G-EMITC ;] PRIM
   s" space" [: RAX STR-SPACE IMM32,  G-EMITC ;] PRIM
   s" type" [: RDX POP,  RSI POP,  G-OUT ;] PRIM ;

\ The status a refused hook exits with: habu1.f BSETCHECK's.
70 constant HOOK-BAD-RC

\ Write the text on fd 2, with no newline, as the ARM64 rows do, and exit
\ HOOK-BAD-RC. The text follows the exit, inside the record.
: HOOK-DIE, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: msg:label :}
   msg u STDERR-WRITE,
   HOOK-BAD-RC EXIT-GROUP,
   msg LBL,  a u TEXT, ;

\ Branch to the label unless rax is a live JIT entry, DBASE <= rax < CP,
\ unsigned: the install window of habu1.f BSETCHECK. It catches a wild
\ install, not a well-formed pointer into live code.
: WINDOW, ( label -- ) {: bad:label :}
   RAX DBASE-REG ASM-SINK ENC-CMP-RR  C-B bad JCC,
   RAX CP-REG ASM-SINK ENC-CMP-RR  C-AE bad JCC, ;

\ set-check ( xt -- ): 0 turns checking off and empties the preflight hook
\ too; any other xt must lie in the window.
: SET-CHECK-BODY ( -- )
   LBL LBL LBL {: bad:label ok:label done:label :}
   RAX POP,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   bad WINDOW,
   ok LBL,
   RAX HOOK-CELL CELL!,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RAX COMPILE-PREFLIGHT-CELL CELL!,
   done JMP,
   bad LBL,  s" set-check: invalid checker xt" HOOK-DIE,
   done LBL, ;

\ set-preflight ( xt -- ): installs once. With the cell set, the same xt is
\ inert and any other refused; with it empty, the xt must lie in the window.
: SET-PREFLIGHT-BODY ( -- )
   LBL LBL LBL {: invalid:label empty:label done:label :}
   RAX POP,
   RCX COMPILE-PREFLIGHT-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E empty JCC,
   RCX RAX ASM-SINK ENC-CMP-RR  C-E done JCC,
   s" set-preflight: invalid or replaced hook" HOOK-DIE,
   empty LBL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E invalid JCC,
   invalid WINDOW,
   RAX COMPILE-PREFLIGHT-CELL CELL!,
   done JMP,
   invalid LBL,  s" set-preflight: invalid hook" HOOK-DIE,
   done LBL, ;

\ set-top-check ( xt -- ): 0 uninstalls; any other xt must lie in the window.
: SET-TOP-CHECK-BODY ( -- )
   LBL LBL LBL {: bad:label ok:label done:label :}
   RAX POP,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   bad WINDOW,
   ok LBL,
   RAX TOP-HOOK-CELL CELL!,
   done JMP,
   bad LBL,  s" set-top-check: invalid top-row hook xt" HOOK-DIE,
   done LBL, ;

\ The checker's hooks live in sealed DATA cells, so a direct store from the
\ row is their only writer once the engine is sealed.
: HOOKS, ( -- )
   s" set-check" [: SET-CHECK-BODY ;] PRIM
   s" check@" [: RAX HOOK-CELL CELL@,  RAX PUSH, ;] PRIM
   s" set-preflight" [: SET-PREFLIGHT-BODY ;] PRIM
   s" set-top-check" [: SET-TOP-CHECK-BODY ;] PRIM
   s" top-check@" [: RAX TOP-HOOK-CELL CELL@,  RAX PUSH, ;] PRIM ;

public

: ENGINE-STATE, ( -- )
   HEAP,  PRINTERS,  HOOKS, ;

\ The whole kernel: the helpers, then every section.
: KERNEL, ( -- )
   HELPERS,
   SYSCALLS,
   CONTROL,
   ATOMICS,
   DICT-SEARCH,
   ENGINE-STATE, ;

;using
;using
;using
;package
