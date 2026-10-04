\ kernel-x64.f - the x86-64 engine's primitive bodies, package X64KERNEL.
\
\ The twins of src/habu/habu1.f's bodies for the rows of src/habu/prims.f, hand
\ written through X64ASM. PRIM registers a body in the shared registry
\ (src/habu/primitive-registry.f) as habu1.f FPRIM does, so the specification
\ gates hold on both targets, and the helpers the rows share are emitted once:
\ the span guard (PROT-SPAN), the narrow page flip LPROTREC, the task-live
\ exit LTASKLIVE, the dictionary index with the one-wordlist search, the DP
\ refusal LDPBAD, the output device arm (GENIO-OUT) and the code-provenance
\ band's set and query (src/habu/code-origin-x64.f).
\
\ A BODY'S CONTRACT (docs/x86-64.md "Kernel inventory"). rbp is DATA, r12 the
\ data stack, rbx and r13-r15 the other VM registers; rax rcx rdx rsi rdi and
\ r8-r11 are scratch. A body is entered by `call` and leaves by the `ret` PRIM
\ appends, so the return address stays on the machine stack and a body calls a
\ helper without a frame: one definer serves where habu1.f needs FPRIM and
\ FPRIM-L. A compiled body (PRIM-HIR) keeps the same contract and ends in the
\ x86-64 chain's own `ret`.
\
\ The x86-64 seam (src/os/linux-x86-64/sys.f) loads here globally, with its
\ process emitters (proc-watch.f, proc-control.f), so this file loads before
\ any file that loads the seam into a private wordlist, as src/habu/boot-x64.f
\ does.
\
\ One section per group of rows. KERNEL, emits the helpers and every section;
\ a group's rows land in its section word, which KERNEL, calls.
require lib/byte-buffer.f
require lib/string.f
require src/habu/prof-x64.f
require src/core/does-clause.f
require src/core/util.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/habu/stack-abi.f
require src/habu/regalloc-abi.f
require src/habu/primitive-registry.f
require src/habu/data-claims.f
require src/habu/data-bands.f
require src/habu/snapshot-format.f
require src/habu/code-span.f
require src/habu/code-origin-x64.f
require src/habu/task-abi.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/rt.f
require src/os/linux-x86-64/sys.f
require src/os/linux-x86-64/proc-watch.f
require src/os/linux-x86-64/proc-control.f
require src/os/linux-x86-64/target-layout.f
require src/habu/kernel-hir-x64.f
require src/habu/aot-decl.f

package X64KERNEL
using X64ASM
using X64CODE
using X64RT
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

public

\ The rc a primitive body this kernel lacks dies with: ENTRY-LABEL's at build
\ time and a REFUSE row's at run time. It is the rc habu1.f
\ ENGINE-EMIT:TARGET-UNKNOWN dies with when a target has no body at all.
76 constant REFUSE-RC

variable FLOORREC-ENTRY
variable FLOORREC-PENDING
: FLOORREC-LBL ( -- label ) FLOORREC-ENTRY @ >LABEL ;
: FLOORREC-NEW ( -- label )
   X64CODE:LBL dup LABEL>N FLOORREC-ENTRY !
   true FLOORREC-PENDING ! ;

\ The highest DP, as an offset from DATA, that the heap rows admit: the top of
\ the target's DATA less the profiler's counter band, as habu1.f DP-CHECK
\ bounds it.
X64LAYOUT:DATA-SIZE PROF-CNT-BYTES - constant DP-CEILING

\ The DATA claims place that band for the host's DATA-SIZE, which differs from
\ the target's on a macOS host: check them with the target's band.
X64LAYOUT:DATA-SIZE DATA-CLAIMS:BAND-ASSERT

\ The code slot: every CP is a multiple of it, and each row that moves CP moves
\ it to a slot. It is X64IR:SP-ALIGN, the only placement X64EMIT:PLACE-AT
\ accepts, and the native driver places each definition at CP
\ (NPUB:NEXT-SLOT). The kernel loads no dialect, so it states the value.
16 constant CODE-SLOT

\ The code ceiling, as an offset into the region: the definition writers
\ refuse a CP, or a long name's slots at CP, that reach it, as habu2.f bounds
\ `:` at REGION - $4000 above DBASE.
X64LAYOUT:CODE-CEILING constant CODE-CEILING

private

$1000 constant PAGE-BYTES              \ the x86-64 Linux base page
79 constant TASK-LIVE-RC               \ habu1.f B-TASK-LIVE-GUARD's $4F
2 constant STDERR

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: SHARED-DATA, ( r64 -- ) X64LAYOUT:DATA-VA VA>N >IMM64 ASM-SINK ENC-MOV-RI64 ;

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
variable LCLOSE-CELL
: SPAN-LBL ( -- label ) SPAN-CELL @ >LABEL ;
: REC-LBL ( -- label ) REC-CELL @ >LABEL ;
: LIVE-LBL ( -- label ) LIVE-CELL @ >LABEL ;
: DPBAD-LBL ( -- label ) DPBAD-CELL @ >LABEL ;
: LCLOSE-LBL ( -- label ) LCLOSE-CELL @ >LABEL ;

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

\ The record spans [first, last): the body, and the ret after it when the
\ flag says so. A compiled body (PRIM-HIR) ends in its own.
: RECORD ( bool -- n ) {: ret:bool :}
   X64CODE:LBL X64CODE:LBL {: first:label last:label :}
   ROW$ first last ENGINE-PRIMS:ADD {: row:n :}
   first X64CODE:LBL,  BODY  ret if ASM-SINK ENC-RET then  last X64CODE:LBL,
   row ;

: EMIT-ROW ( -- n ) true RECORD ;

: REFUSE-HEAD$ ( -- ptr u8 n ) s" hb: " ;
: REFUSE-TAIL$ ( -- ptr u8 n ) s"  is not in the x86-64 kernel" ;

\ Write the n bytes at the label on fd 2.
: STDERR-WRITE, ( label n -- ) {: msg:label len:n :}
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX len IMM32,  NR-WRITE SYS, ;

\ Write the line on fd 2, its newline included, and exit n. The text follows
\ the exit, inside the record.
: STDERR-EXIT, ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   X64CODE:LBL {: msg:label :}
   msg u STDERR-WRITE,
   rc EXIT-GROUP,
   msg X64CODE:LBL,  a u TEXT, ;

\ Name the row on fd 2 and exit REFUSE-RC. The text follows the exit, inside
\ the record, as src/habu/boot-x64.f FAIL, places its own.
: REFUSE-BODY ( -- )
   X64CODE:LBL {: msg:label :}
   REFUSE-HEAD$ nip ROW-U @ + REFUSE-TAIL$ nip + 1+ {: len:n :}
   msg len STDERR-WRITE,
   REFUSE-RC EXIT-GROUP,
   msg X64CODE:LBL,
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

private

variable ROW-CELL

\ Jump through the row's dispatch cell, leaving the caller's return address on
\ the machine stack for the word the cell holds.
: DISPATCH-BODY ( -- )
   RAX DATA-REG ROW-CELL @ MEM-OFF ASM-SINK ENC-MOV-RM
   RAX ASM-SINK ENC-JMP-REG ;

public

\ PRIM for a row the captured runtime provides (src/habu/prims.f
\ EPREFIX-PROVIDED!), registered with its dispatch cell n (layout.f
\ PROVIDED-XT): the twin of habu1.f FPRIM-PROVIDED. In a seeded build the
\ record is a stub that jumps through the cell; otherwise it is the body given.
: PROVIDED ( n ptr u8 n [ -- ] -- ) {: cell:n a:ptr u:n q :}
   a u q ARGS
   ROW$ KEEP? 0= if exit then
   cell ROW-CELL !
   ENGINE-PRIMS:SEEDED? if
      [: DISPATCH-BODY ;] is BODY  false RECORD
   else
      true RECORD
   then
   cell swap ENGINE-PRIMS:DISPATCH! ;

\ A row the x86-64 kernel does not carry: its body writes `hb: <name> is not
\ in the x86-64 kernel` on fd 2 and exits REFUSE-RC.
: REFUSE ( ptr u8 n -- ) [: REFUSE-BODY ;] PRIM ;

\ REFUSE whose record carries wid n, for a row ARM64 registers through
\ FPRIM-WID: the refusal's record is as visible as the body that replaces it.
: REFUSE-WID ( ptr u8 n n -- ) {: wid:n :} [: REFUSE-BODY ;] wid PRIM-WID ;

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
   X64CODE:LBL {: skip:label :}
   RAX SHARED-DATA,
   RAX RAX ix DATA-BANDS:OFF ix DATA-BANDS:LEN + MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-AE skip JCC,
   RAX SHARED-DATA,
   RAX RAX ix DATA-BANDS:OFF MEM-OFF ASM-SINK ENC-LEA
   RDX RAX ASM-SINK ENC-CMP-RR  C-A trap JCC,
   skip X64CODE:LBL, ;

: BANDS, ( label -- ) {: trap:label :}
   0 BEGIN dup DATA-BANDS:LEN 0 <> WHILE  dup trap BAND,  1+  REPEAT drop ;

\ (PROT-SPAN) ( rdi = address, rsi = byte length ): the twin of habu1.f
\ GUARD-SPAN. It passes while the friend latch is open and for an empty span;
\ a span that wraps or meets a band exits SEAL-VIOLATION. The hull test skips
\ the band walk for a span outside [DATA-BANDS:LO, DATA-BANDS:HI), which is
\ every DP heap address. Registered as an engine helper, as habu1.f
\ EMIT-PROT-SPAN registers the ARM64 body, so a compiled word that reaches it
\ by a direct call is carried and relocated with it.
: SPAN-HELPER, ( -- )
   SPAN-LBL {: start:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: end:label trap:label ok:label :}
   s" (PROT-SPAN)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start X64CODE:LBL,
   RAX SHARED-DATA,
   RAX RAX FRIEND-LATCH-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   RSI RSI ASM-SINK ENC-TEST-RR  C-E ok JCC,
   RDX RDI RSI 1 0 MEM-IDX ASM-SINK ENC-LEA            \ the span's end
   RDX RDI ASM-SINK ENC-CMP-RR  C-B trap JCC,          \ unsigned wrap
   RAX SHARED-DATA,
   RAX RAX DATA-BANDS:HI MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-AE ok JCC,           \ start >= hull end
   RAX SHARED-DATA,
   RAX RAX DATA-BANDS:LO MEM-OFF ASM-SINK ENC-LEA
   RDX RAX ASM-SINK ENC-CMP-RR  C-BE ok JCC,           \ end <= hull start
   trap BANDS,
   ok X64CODE:LBL,
   ASM-SINK ENC-RET
   trap X64CODE:LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   end X64CODE:LBL, ;

\ LPROTREC ( rdi = an address in the first page, rdx = prot ): mprotect the two
\ PAGE-BYTES pages from rdi's page, as habu1.f LPROTREC flips two host pages.
: REC-HELPER, ( -- )
   REC-LBL X64CODE:LBL,
   RDI PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   RSI PAGE-BYTES 2 * IMM32,
   NR-MPROTECT SYS,
   ASM-SINK ENC-RET ;

: LIVE-HELPER, ( -- )
   LIVE-LBL X64CODE:LBL,
   TASK-LIVE-RC EXIT-GROUP, ;

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
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: head:label of:label unit:label :}
   DPBAD-LBL X64CODE:LBL,
   R8 RDI ASM-SINK ENC-MOV-RR  R8 DATA-REG ASM-SINK ENC-SUB-RR
   head DPBAD-HEAD$ nip STDERR-WRITE,
   RAX R8 ASM-SINK ENC-MOV-RR  DIAG-U,
   of DPBAD-OF$ nip STDERR-WRITE,
   RAX DP-CEILING IMM32,  DIAG-U,
   unit DPBAD-UNIT$ nip 1+ STDERR-WRITE,
   DPBAD-RC EXIT-GROUP,
   head X64CODE:LBL,  DPBAD-HEAD$ TEXT,
   of X64CODE:LBL,  DPBAD-OF$ TEXT,
   unit X64CODE:LBL,  DPBAD-UNIT$ TEXT,  STR-LF ASM-SINK BUF:APPEND-BYTE ;

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
   X64CODE:LBL X64CODE:LBL {: term:label end:label :}
   s" (GENIO-OUT)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start X64CODE:LBL,
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
   term X64CODE:LBL,
   RDI 1 IMM32,  NR-WRITE SYS,
   ASM-SINK ENC-RET
   end X64CODE:LBL, ;

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
5 constant PROT-RX                     \ PROT_READ|PROT_EXEC
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
   X64CODE:LBL {: done:label :}
   r FOLD-FIRST >IMM8 ASM-SINK ENC-CMP-RI8  C-B done JCC,
   r FOLD-LAST >IMM8 ASM-SINK ENC-CMP-RI8  C-A done JCC,
   r FOLD-BIT >IMM8 ASM-SINK ENC-OR-RI8
   done X64CODE:LBL, ;

\ The twin of C-HIDX-HASH: h = the FNV-1a hash of the folded name at `name`,
\ `len` bytes, through the cursor, byte and prime registers.
: HASH, ( r64 r64 r64 r64 r64 r64 -- )
   {: name:r64 len:r64 h:r64 cur:r64 byte:r64 prime:r64 :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   h FNV-BASIS IMM64,
   prime FNV-PRIME IMM64,
   cur ZERO-REG,
   next X64CODE:LBL,
   cur len ASM-SINK ENC-CMP-RR  C-GE done JCC,
   byte name cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   byte FOLD,
   h byte ASM-SINK ENC-XOR-RR
   h prime ASM-SINK ENC-IMUL-RR
   cur ASM-SINK ENC-INC
   next JMP,
   done X64CODE:LBL, ;

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
   X64CODE:LBL {: inline:label :}
   dst row REC-NAME MEM-OFF ASM-SINK ENC-LEA
   tmp DNAME-EXT IMM64,
   tmp row REC-FLAGS MEM-OFF ASM-SINK ENC-TEST-MR  C-E inline JCC,
   dst row REC-NAME MEM-OFF ASM-SINK ENC-MOV-RM
   inline X64CODE:LBL, ;

\ Compare `len` bytes at a and b, folded, through the cursor and two byte
\ registers: a mismatch jumps to `miss`, a match falls through.
: SAME-NAME, ( r64 r64 r64 r64 r64 r64 label -- )
   {: a:r64 b:r64 len:r64 cur:r64 x:r64 y:r64 miss:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   cur ZERO-REG,
   next X64CODE:LBL,
   cur len ASM-SINK ENC-CMP-RR  C-GE done JCC,
   x a cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM  x FOLD,
   y b cur 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM  y FOLD,
   x y ASM-SINK ENC-CMP-RR  C-NE miss JCC,
   cur ASM-SINK ENC-INC
   next JMP,
   done X64CODE:LBL, ;

\ WLFIND:LENTRY's twin ( rdi = name, rsi = length, rdx = wid ): rax = the
\ name's record in that one wordlist, or 0; the record's code cell is its first.
\ It keeps rdi, rsi and rdx and clobbers rcx and r8-r11. With a table it probes
\ the key's chain; with none, for DICT-WL:RETIRED (stamped onto rows already
\ indexed under another wid, so no chain holds them), and after a chain walked
\ through every slot, it scans. The scan's answer is the LAST matching record,
\ which is the probe's one record wherever a wid holds one row per name.
: FIND-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: probe:label miss:label next:label absent:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label row:label skip:label :}
   X64CODE:LBL {: done:label :}
   FIND-LBL X64CODE:LBL,
   RDX DICT-WL:RETIRED >IMM8 ASM-SINK ENC-CMP-RI8  C-E scan JCC,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E scan JCC,
   RDI RSI R8 R9 R10 R11 HASH,
   R8 RDX SLOT,                                        \ r8 = the slot
   R9 HIDX-SLOTS IMM32,                                \ r9 = slots left to walk
   probe X64CODE:LBL,
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
   miss X64CODE:LBL,
   RAX ASM-SINK ENC-POP
   next X64CODE:LBL,
   R9 ASM-SINK ENC-DEC  C-E scan JCC,
   R8 NEXT-SLOT,
   probe JMP,
   absent X64CODE:LBL,
   RAX ZERO-REG,
   ASM-SINK ENC-RET
   scan X64CODE:LBL,
   R8 ZERO-REG,                                        \ r8 = the last match
   R9 DBASE-REG ASM-SINK ENC-MOV-RR                    \ r9 = the record
   row X64CODE:LBL,
   RAX NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX DBASE-REG ASM-SINK ENC-ADD-RR
   R9 RAX ASM-SINK ENC-CMP-RR  C-AE done JCC,          \ past the last record
   RDX R9 REC-WID MEM-OFF ASM-SINK ENC-CMP-RM  C-NE skip JCC,
   R9 R11 RAX NAME-LEN,
   R11 RSI ASM-SINK ENC-CMP-RR  C-NE skip JCC,
   R9 R11 RAX NAME-AT,
   RDI R11 RSI RCX RAX R10 skip SAME-NAME,
   R8 R9 ASM-SINK ENC-MOV-RR
   skip X64CODE:LBL,
   R9 DREC >IMM8 ASM-SINK ENC-ADD-RI8
   row JMP,
   done X64CODE:LBL,
   RAX R8 ASM-SINK ENC-MOV-RR
   ASM-SINK ENC-RET ;

\ The twin of C-HIDX-INS: index record rdi, which it keeps, at the first empty
\ or stale slot of its key's chain. Claiming an empty slot counts one more
\ HIDX:CLAIMS; a chain walked through every slot is HIDX-EMIT:LFULL's. It clobbers
\ rax rcx rdx rsi and r8-r11.
: INSERT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: probe:label empty:label put:label :}
   RAX RDI ASM-SINK ENC-MOV-RR  RAX ROW,
   RCX RAX REC-WID MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RDX R8 NAME-LEN,
   RAX RSI R8 NAME-AT,
   RSI RDX R8 R9 R10 R11 HASH,
   R8 RCX SLOT,                                        \ r8 = the slot
   R9 HIDX-SLOTS IMM32,                                \ r9 = slots left to walk
   probe X64CODE:LBL,
   R10 R8 SLOT@,
   R10 R10 ASM-SINK ENC-TEST-RR  C-E empty JCC,
   R10 ASM-SINK ENC-DEC
   R10 NDICT-REG ASM-SINK ENC-CMP-RR  C-GE put JCC,    \ stale: reuse its claim
   R9 ASM-SINK ENC-DEC  C-E FULL-LBL JCC,
   R8 NEXT-SLOT,
   probe JMP,
   empty X64CODE:LBL,
   R10 DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-RM
   R10 ASM-SINK ENC-INC
   R10 DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   put X64CODE:LBL,
   R10 RDI 1 MEM-OFF ASM-SINK ENC-LEA
   R10 R64>N >R32 RAX R8 4 0 MEM-IDX ASM-SINK ENC-MOV32-MR ;

\ HIDX-EMIT:LREBUILD's twin: zero the table and HIDX:CLAIMS, then index the live
\ records [0, r14). With no table it returns at once; a dictionary at
\ HIDX:LOAD-MAX, which the compaction could not bring under the bound, is
\ HIDX-EMIT:LFULL's.
: REBUILD-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: zero:label fill:label done:label :}
   REBUILD-LBL X64CODE:LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NDICT-REG HIDX:LOAD-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-GE FULL-LBL JCC,
   RCX ZERO-REG,                                       \ rcx = the byte offset
   RDX ZERO-REG,
   zero X64CODE:LBL,
   RDX RAX RCX 1 0 MEM-IDX ASM-SINK ENC-MOV-MR
   RCX CELL >IMM8 ASM-SINK ENC-ADD-RI8
   RCX HIDX-BYTES >IMM32 ASM-SINK ENC-CMP-RI32  C-B zero JCC,
   RDX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   RDI ZERO-REG,                                       \ rdi = the record
   fill X64CODE:LBL,
   RDI NDICT-REG ASM-SINK ENC-CMP-RR  C-GE done JCC,
   INSERT,
   RDI ASM-SINK ENC-INC
   fill JMP,
   done X64CODE:LBL,
   ASM-SINK ENC-RET ;

\ LHIDXADD's twin: index record r14 - 1, the one just published, and compact
\ the table once HIDX:CLAIMS reaches HIDX:LOAD-MAX. With no table it returns at
\ once.
: ADD-HELPER, ( -- )
   X64CODE:LBL {: done:label :}
   ADD-LBL X64CODE:LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RDI NDICT-REG -1 MEM-OFF ASM-SINK ENC-LEA
   INSERT,
   RAX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-RM
   RAX HIDX:LOAD-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-L done JCC,
   REBUILD-LBL CALL,
   done X64CODE:LBL,
   ASM-SINK ENC-RET ;

\ LHIDXBUILD's twin: map the table once, HIDX-BYTES of fresh zero pages with no
\ claims, then rebuild it; a table already mapped is refilled in place.
: BUILD-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL {: have:label fail:label :}
   BUILD-LBL X64CODE:LBL,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE have JCC,
   RDI ZERO-REG,  RSI HIDX-BYTES IMM32,  RDX PROT-RW IMM32,
   R10 MAP-ANON-PRIVATE IMM32,  R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B fail JCC,
   RAX DATA-REG HIDXP-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ZERO-REG,
   RCX DATA-REG HIDX:CLAIMS MEM-OFF ASM-SINK ENC-MOV-MR
   have X64CODE:LBL,
   REBUILD-LBL JMP,                                    \ its ret is this one's
   fail X64CODE:LBL,
   S\" hb: dictionary index alloc failed\n" INDEX-RC STDERR-EXIT, ;

\ HIDX-EMIT:LFULL's twin: the index cannot be kept, which is loud, never a quiet
\ fall back to the scan.
: FULL-HELPER, ( -- )
   FULL-LBL X64CODE:LBL,
   S\" hb: dictionary index exhausted\n" INDEX-RC STDERR-EXIT, ;

: INDEX-HELPERS, ( -- )
   X64CODE:LBL FIND-CELL !  X64CODE:LBL BUILD-CELL !  X64CODE:LBL ADD-CELL !  X64CODE:LBL REBUILD-CELL !
   X64CODE:LBL FULL-CELL !
   FIND-HELPER,  BUILD-HELPER,  ADD-HELPER,  REBUILD-HELPER,  FULL-HELPER, ;

public

\ The index's call sites, the twins of habu1.f's LHIDXBUILD, LHIDXADD and
\ HIDX-EMIT:LREBUILD calls, for the rows that move r14: build the table (a refused
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
   X64CODE:LBL SPAN-CELL !  X64CODE:LBL REC-CELL !  X64CODE:LBL LIVE-CELL !
   X64CODE:LBL DPBAD-CELL !  X64CODE:LBL LGENIOOUT !
   SPAN-HELPER,  REC-HELPER,  LIVE-HELPER,  INDEX-HELPERS,
   DPBAD-HELPER,  GENIO-HELPER,  X64PROV:EMIT-HELPERS ;

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
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: legacy:label write:label span:label :}
   X64CODE:LBL X64CODE:LBL {: trap:label done:label :}
   RAX 1 PEEK,  RDI 0 PEEK,
   RAX TCGETS >IMM32 ASM-SINK ENC-CMP-RI32  C-E legacy JCC,
   RAX TCSETS >IMM32 ASM-SINK ENC-CMP-RI32  C-E done JCC,
   RCX RAX ASM-SINK ENC-MOV-RR
   RCX 30 >IMM8 ASM-SINK ENC-SHR-RI8  RCX 3 >IMM8 ASM-SINK ENC-AND-RI8
   RCX 2 >IMM32 ASM-SINK ENC-TEST-RI32  C-NE write JCC,
   RCX RCX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   trap JMP,
   legacy X64CODE:LBL,
   RSI TERMIOS-BYTES IMM32,  span JMP,
   write X64CODE:LBL,
   RSI RAX ASM-SINK ENC-MOV-RR
   RSI 16 >IMM8 ASM-SINK ENC-SHR-RI8  RSI $3FFF >IMM32 ASM-SINK ENC-AND-RI32
   C-E done JCC,
   span X64CODE:LBL,
   RDI RSI PROT-SPAN-CALL,  done JMP,
   trap X64CODE:LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   done X64CODE:LBL, ;

\ Fresh anonymous storage: ( bytes -- ptr ior ), a null pointer and -1 for a
\ length that is not positive or a mapping the kernel refuses.
: MAP-ANON-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   RSI POP,
   RSI RSI ASM-SINK ENC-TEST-RR  C-LE failed JCC,
   RDI ZERO-REG,  RDX PROT-RW IMM32,  R10 MAP-ANON-PRIVATE IMM32,
   R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B failed JCC,
   RCX ZERO-REG,  done JMP,
   failed X64CODE:LBL,
   RAX ZERO-REG,  RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   done X64CODE:LBL,
   0 G-PUSH  1 G-PUSH ;

\ ( addr len prot flags fd off -- addr|-1 ). Only MAP_FIXED replaces a mapping
\ that stands, so only it guards the span.
: MMAP-ROW ( -- )
   X64CODE:LBL {: placed:label :}
   RAX 2 PEEK,  RAX MAP-FIXED >IMM32 ASM-SINK ENC-TEST-RI32  C-E placed JCC,
   5 4 SPAN-GUARD,
   placed X64CODE:LBL,
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
   X64CODE:LBL {: done:label :}
   0 STAT-BYTES SIZED-GUARD,
   RDX POP,  RSI POP,  RDI AT-FDCWD,  R10 flags IMM32,
   nr SYS-PUSH,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RDX STAT-FIX,
   done X64CODE:LBL, ;

: GTOD ( n -- mem ) {: off:n :} DATA-REG GTOD-SCRATCH off + MEM-OFF ;

\ A time call cannot fail with the arguments these rows give it. One that does
\ stops on ud2, as habu1.f's twins stop on brk.
: TIME-CHECK, ( -- )
   X64CODE:LBL {: ok:label :}
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   ASM-SINK ENC-UD2
   ok X64CODE:LBL, ;

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

\ Call the C function at r11 under the SysV ABI: the register arguments
\ already in rdi rsi rdx rcx r8 r9 and xmm0..7, and the r10 cells at rax (a
\ count >= 0) as its stack arguments; the answer is in rax or xmm0. The entry
\ rsp waits on the data stack, whose pointer r12 is callee-saved, as habu1.f
\ BFFI-CALL-N-CORE keeps the frame sp in x20: every callee-saved register here
\ is a VM register. The function waits in the machine-stack cell below the
\ entry rsp while r11 carries the copy. rsp drops by the cells and aligns down
\ to 16, the cells land from [rsp] up, and al is set to n, the caller's bound
\ on its vector arguments, which a variadic callee reads to decide whether it
\ saves xmm0..7; after the call rsp is the entry rsp again. The VM registers
\ survive by the callee-saved rule; rax rcx rdx rsi rdi, r8-r11 and the XMM
\ registers do not.
: SYSV-CALL, ( n -- ) {: vecs:n :}
   X64CODE:LBL X64CODE:LBL {: copy:label called:label :}
   RSP PUSH,
   R11 ASM-SINK ENC-PUSH
   R11 R10 ASM-SINK ENC-MOV-RR  R11 3 >IMM8 ASM-SINK ENC-SHL-RI8
   RSP R11 ASM-SINK ENC-SUB-RR
   RSP -16 >IMM8 ASM-SINK ENC-AND-RI8
   R10 R10 ASM-SINK ENC-TEST-RR  C-E called JCC,
   copy X64CODE:LBL,
   R10 ASM-SINK ENC-DEC
   R11 RAX R10 CELL 0 MEM-IDX ASM-SINK ENC-MOV-RM
   R11 RSP R10 CELL 0 MEM-IDX ASM-SINK ENC-MOV-MR
   C-NE copy JCC,
   called X64CODE:LBL,
   R11 DSP CELL negate MEM-OFF ASM-SINK ENC-MOV-RM      \ the entry rsp
   R11 R11 CELL negate MEM-OFF ASM-SINK ENC-MOV-RM      \ the function below it
   RAX vecs IMM32,
   R11 ASM-SINK ENC-CALL-REG
   RSP POP, ;

\ Call the C function at rax with its integer register arguments only:
\ SYSV-CALL,'s zero-cell case, with no vector arguments.
: C-CALL, ( -- )
   R11 RAX ASM-SINK ENC-MOV-RR  R10 ZERO-REG,  0 SYSV-CALL, ;

\ rax = dlsym(RTLD_DEFAULT, the NUL-terminated name rsi points at), 0 when the
\ loader has no such symbol: the twin of habu1.f LIBC-OS DLSYM. The loader's
\ slot sits LINUX-DLSYM-SLOT-OFF into the read-write segment, which starts the
\ text's size past the image base; the image base is the text base RBASE-CELL
\ holds less CODE-OFF, and the text size is the first program header's
\ p_filesz, IMAGE-TEXT-SIZE-OFF into the image.
: DLSYM, ( -- )
   RAX DATA-REG RBASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX X64LAYOUT:CODE-OFF >IMM32 ASM-SINK ENC-SUB-RI32
   RAX RAX X64LAYOUT:IMAGE-TEXT-SIZE-OFF MEM-OFF ASM-SINK ENC-ADD-RM
   RAX RAX X64LAYOUT:LINUX-DLSYM-SLOT-OFF MEM-OFF ASM-SINK ENC-MOV-RM
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
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: badcap:label failed:label short:label release:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: count:label counted:label copy:label done:label :}
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
   count X64CODE:LBL,
   R8 RCX MEM-AT ASM-SINK ENC-MOVZX-8-RM
   R8 R8 ASM-SINK ENC-TEST-RR  C-E counted JCC,
   RCX ASM-SINK ENC-INC  RDX ASM-SINK ENC-INC  count JMP,
   counted X64CODE:LBL,
   RDX RP-LEN RP ASM-SINK ENC-MOV-MR
   RDX RP-CAP RP ASM-SINK ENC-CMP-RM  C-AE short JCC,
   RSI RP-RESULT RP ASM-SINK ENC-MOV-RM
   RDI RP-DST RP ASM-SINK ENC-MOV-RM
   RCX RDX ASM-SINK ENC-MOV-RR  RCX ASM-SINK ENC-INC
   copy X64CODE:LBL,
   R8 RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
   8 >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
   RSI ASM-SINK ENC-INC  RDI ASM-SINK ENC-INC
   RCX ASM-SINK ENC-DEC  C-NE copy JCC,
   release X64CODE:LBL,
   RDI RP-RESULT RP ASM-SINK ENC-MOV-RM
   RAX RP-FREE RP ASM-SINK ENC-MOV-RM  C-CALL,
   RAX RP-LEN RP ASM-SINK ENC-MOV-RM  done JMP,
   short X64CODE:LBL,
   RAX -2 >IMM32 ASM-SINK ENC-MOV-RI32  RAX RP-LEN RP ASM-SINK ENC-MOV-MR
   release JMP,
   badcap X64CODE:LBL,
   RAX -2 >IMM32 ASM-SINK ENC-MOV-RI32  done JMP,
   failed X64CODE:LBL,
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   done X64CODE:LBL,
   RSP RP-FRAME >IMM8 ASM-SINK ENC-ADD-RI8
   0 G-PUSH ;

\ ---- process rows ------------------------------------------------------------
\ The twins of habu1.f's Linux process bodies: LINUX-SPAWN under the four spawn
\ rows and BRUNRC, then BPIPE .. BWAITSTATUS and LIBC-OS FORK. Every failure
\ is -1, as habu1.f's errno rule has it, except poll's -errno.

17 constant CLONE-SIGCHLD           \ SIGCHLD at exit, nothing shared
$80000 constant PIPE-CLOEXEC        \ O_CLOEXEC
1030 constant F-DUPFD-CLOEXEC
3 constant SPAWN-MIN-FD             \ the lowest the pipe's write end may sit
127 constant SPAWN-FAIL-RC
73 constant FCNTL-NOSIGPIPE         \ lib/process.f F-SETNOSIGPIPE
13 constant SIGPIPE
1 constant SIG-IGN
8 constant SIGSET-BYTES
1000 constant MS-PER-S
1000000 constant NS-PER-MS
61 constant NFDS-WRAP-SHIFT         \ an nfds at or past 2^61 wraps nfds * 8

\ The spawn frame on the machine stack. The first seven cells are LINUX-SPAWN's
\ arguments; -1 in the directory or a descriptor cell means none. pipe2 writes
\ two u32 descriptors, and the child's failure byte, the pid and wait4's u32
\ status follow. The default argv (the path, then 0) and envp (0) sit last.
0 constant SPN-PATH
8 constant SPN-ARGV
16 constant SPN-ENV
24 constant SPN-CWD
32 constant SPN-IN
40 constant SPN-OUT
48 constant SPN-ERR
56 constant SPN-PIPE-R
60 constant SPN-PIPE-W
64 constant SPN-BYTE
72 constant SPN-PID
80 constant SPN-STATUS
88 constant SPN-ARGV0
104 constant SPN-ENVP0
112 constant SPN-FRAME

: FRAME@, ( r64 n -- ) RP ASM-SINK ENC-MOV-RM ;
: FRAME!, ( r64 n -- ) RP ASM-SINK ENC-MOV-MR ;
: FRAME32@, ( r64 n -- ) {: r:r64 off:n :} r R64>N >R32 off RP ASM-SINK ENC-MOV32-RM ;
: FRAME32!, ( r64 n -- ) {: r:r64 off:n :} r R64>N >R32 off RP ASM-SINK ENC-MOV32-MR ;
: FRAME-AT, ( r64 n -- ) RP ASM-SINK ENC-LEA ;
: MINUS-1, ( r64 -- ) -1 >IMM32 ASM-SINK ENC-MOV-RI32 ;

\ clone(SIGCHLD, 0, 0, 0, 0): x86-64's order puts child_tid before tls where
\ aarch64's puts tls first, and both are 0.
: CLONE-ARGS, ( -- )
   RDI CLONE-SIGCHLD IMM32,  RSI ZERO-REG,  RDX ZERO-REG,  R10 ZERO-REG,  R8 ZERO-REG, ;

: CLOSE-SLOT, ( n -- ) {: off:n :}  RDI off FRAME32@,  NR-CLOSE SYS, ;

\ Move the pipe's write end to SPAWN-MIN-FD or above, as LINUX-SPAWN-PREP-W
\ does, so the child's dup onto 0, 1 or 2 cannot replace it. syscall keeps r8.
: SPAWN-PREP-W, ( label -- ) {: fail:label :}
   X64CODE:LBL {: high:label :}
   RDI SPN-PIPE-W FRAME32@,
   RDI SPAWN-MIN-FD 1- >IMM8 ASM-SINK ENC-CMP-RI8  C-G high JCC,
   RSI F-DUPFD-CLOEXEC IMM32,  RDX SPAWN-MIN-FD IMM32,  NR-FCNTL SYS,  C-B fail JCC,
   R8 RAX ASM-SINK ENC-MOV-RR
   SPN-PIPE-W CLOSE-SLOT,
   R8 SPN-PIPE-W FRAME32!,
   high X64CODE:LBL, ;

\ Dup the descriptor in the frame cell onto fd n, as LINUX-DUP2-FD: skipped
\ when it is negative or n itself.
: CHILD-DUP, ( n n label -- ) {: off:n fd:n fail:label :}
   X64CODE:LBL {: skip:label :}
   RDI off FRAME@,
   RDI RDI ASM-SINK ENC-TEST-RR  C-S skip JCC,
   RDI fd >IMM8 ASM-SINK ENC-CMP-RI8  C-E skip JCC,
   RSI fd IMM32,  RDX ZERO-REG,  NR-DUP2 SYS,  C-B fail JCC,
   skip X64CODE:LBL, ;

\ LINUX-SPAWN-CHILD: its own process group, the directory, the three
\ descriptors, then execve. A step that fails, execve included, writes one
\ byte to the pipe and exits SPAWN-FAIL-RC; the pipe is O_CLOEXEC, so a
\ successful execve closes it with nothing written.
: SPAWN-CHILD, ( -- )
   X64CODE:LBL X64CODE:LBL {: fail:label nocwd:label :}
   SPN-PIPE-R CLOSE-SLOT,
   RDI ZERO-REG,  RSI ZERO-REG,  NR-SETPGID SYS,  C-B fail JCC,
   RDI SPN-CWD FRAME@,  RDI RDI ASM-SINK ENC-TEST-RR  C-S nocwd JCC,
   NR-CHDIR SYS,  C-B fail JCC,
   nocwd X64CODE:LBL,
   SPN-IN 0 fail CHILD-DUP,  SPN-OUT 1 fail CHILD-DUP,  SPN-ERR 2 fail CHILD-DUP,
   RDI SPN-PATH FRAME@,  RSI SPN-ARGV FRAME@,  RDX SPN-ENV FRAME@,
   NR-EXECVE SYS,
   fail X64CODE:LBL,
   RAX 1 IMM32,  RAX SPN-BYTE FRAME!,
   RDI SPN-PIPE-W FRAME32@,  RSI SPN-BYTE FRAME-AT,  RDX 1 IMM32,  NR-WRITE SYS,
   SPAWN-FAIL-RC EXIT-GROUP, ;

\ LINUX-SPAWN-PARENT, with the child's pid in rax: read the pipe once. End of
\ file is an execve that worked, and rax becomes the pid; a byte or a failed
\ read is a child that failed, which is reaped, and rax becomes -1.
: SPAWN-PARENT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: failed:label ok:label done:label :}
   RAX SPN-PID FRAME!,
   SPN-PIPE-W CLOSE-SLOT,
   RDI SPN-PIPE-R FRAME32@,  RSI SPN-BYTE FRAME-AT,  RDX 1 IMM32,  NR-READ SYS,
   C-B failed JCC,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   failed X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,
   RDI SPN-PID FRAME@,  RSI SPN-STATUS FRAME-AT,  RDX ZERO-REG,  R10 ZERO-REG,
   NR-WAIT4 SYS,
   RAX MINUS-1,  done JMP,
   ok X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,
   RAX SPN-PID FRAME@,
   done X64CODE:LBL, ;

\ The twin of habu1.f LINUX-SPAWN over the frame at rsp: rax becomes the
\ child's pid, or -1 when the pipe, the clone or any step of the child failed.
: SPAWN, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: child:label closefail:label fail:label done:label :}
   RDI SPN-PIPE-R FRAME-AT,  RSI PIPE-CLOEXEC IMM32,  NR-PIPE SYS,  C-B fail JCC,
   closefail SPAWN-PREP-W,
   CLONE-ARGS,  NR-SPAWN SYS,  C-B closefail JCC,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E child JCC,
   SPAWN-PARENT,  done JMP,
   child X64CODE:LBL,
   SPAWN-CHILD,
   closefail X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,  SPN-PIPE-W CLOSE-SLOT,
   fail X64CODE:LBL,
   RAX MINUS-1,
   done X64CODE:LBL, ;

: SPAWN-OPEN, ( -- ) RSP SPN-FRAME >IMM8 ASM-SINK ENC-SUB-RI8 ;
: SPAWN-CLOSE, ( -- ) RSP SPN-FRAME >IMM8 ASM-SINK ENC-ADD-RI8  RAX PUSH, ;
: POP-SLOT, ( n -- ) {: off:n :}  RAX POP,  RAX off FRAME!, ;
: NONE-SLOT, ( n -- ) {: off:n :}  RAX MINUS-1,  RAX off FRAME!, ;
: STDIO-SLOTS, ( -- ) SPN-ERR POP-SLOT,  SPN-OUT POP-SLOT,  SPN-IN POP-SLOT, ;

\ argv = { path, 0 }, as BSPAWNIO and BRUNRC build it.
: DEFAULT-ARGV, ( -- )
   RAX SPN-PATH FRAME@,  RAX SPN-ARGV0 FRAME!,
   RAX ZERO-REG,  RAX SPN-ARGV0 CELL + FRAME!,
   RAX SPN-ARGV0 FRAME-AT,  RAX SPN-ARGV FRAME!, ;

\ envp = { 0 }: no environment.
: DEFAULT-ENV, ( -- )
   RAX ZERO-REG,  RAX SPN-ENVP0 FRAME!,
   RAX SPN-ENVP0 FRAME-AT,  RAX SPN-ENV FRAME!, ;

: SPAWN-IO-ROW ( -- )               \ ( path in out err -- pid|-1 )
   SPAWN-OPEN,  STDIO-SLOTS,  SPN-PATH POP-SLOT,
   SPN-CWD NONE-SLOT,  DEFAULT-ARGV,  DEFAULT-ENV,
   SPAWN,  SPAWN-CLOSE, ;

: SPAWN-ARGV-IO-ROW ( -- )          \ ( path argv in out err -- pid|-1 )
   SPAWN-OPEN,  STDIO-SLOTS,  SPN-ARGV POP-SLOT,  SPN-PATH POP-SLOT,
   SPN-CWD NONE-SLOT,  DEFAULT-ENV,
   SPAWN,  SPAWN-CLOSE, ;

: SPAWN-ENV-IO-ROW ( -- )           \ ( path argv envp in out err -- pid|-1 )
   SPAWN-OPEN,  STDIO-SLOTS,  SPN-ENV POP-SLOT,  SPN-ARGV POP-SLOT,
   SPN-PATH POP-SLOT,  SPN-CWD NONE-SLOT,
   SPAWN,  SPAWN-CLOSE, ;

: SPAWN-CWD-IO-ROW ( -- )           \ ( path argv envp cwd in out err -- pid|-1 )
   SPAWN-OPEN,  STDIO-SLOTS,  SPN-CWD POP-SLOT,  SPN-ENV POP-SLOT,
   SPN-ARGV POP-SLOT,  SPN-PATH POP-SLOT,
   SPAWN,  SPAWN-CLOSE, ;

\ ( path -- status|-1 ): spawn with no arguments, environment or redirection,
\ then wait4 for the exit status (WEXITSTATUS), as BRUNRC does. A spawn or a
\ wait that failed is -1.
: RUN-RC-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   SPAWN-OPEN,  SPN-PATH POP-SLOT,
   SPN-CWD NONE-SLOT,  SPN-IN NONE-SLOT,  SPN-OUT NONE-SLOT,  SPN-ERR NONE-SLOT,
   DEFAULT-ARGV,  DEFAULT-ENV,
   SPAWN,
   RAX RAX ASM-SINK ENC-TEST-RR  C-S done JCC,
   RDI RAX ASM-SINK ENC-MOV-RR  RSI SPN-STATUS FRAME-AT,  RDX ZERO-REG,  R10 ZERO-REG,
   NR-WAIT4 SYS,  C-B failed JCC,
   RAX SPN-STATUS FRAME32@,
   RAX 8 >IMM8 ASM-SINK ENC-SHR-RI8  RAX $FF >IMM32 ASM-SINK ENC-AND-RI32
   done JMP,
   failed X64CODE:LBL,
   RAX MINUS-1,
   done X64CODE:LBL,
   SPAWN-CLOSE, ;

\ ( pid -- status|-1 ): wait4's raw u32 status, as BWAITSTATUS. The load
\ leaves the carry SYS, set for SYS-PUSH.
: WAIT-STATUS-ROW ( -- )
   RDI POP,
   RSP CELL >IMM8 ASM-SINK ENC-SUB-RI8
   RSI RSP ASM-SINK ENC-MOV-RR  RDX ZERO-REG,  R10 ZERO-REG,  NR-WAIT4 SYS,
   RAX 0 FRAME32@,  SYS-PUSH
   RSP CELL >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ ( -- rfd wfd 0 | -1 -1 -1 ): pipe2(fds, 0), as BPIPE.
: PIPE-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   RSP CELL >IMM8 ASM-SINK ENC-SUB-RI8
   RDI RSP ASM-SINK ENC-MOV-RR  RSI ZERO-REG,  NR-PIPE SYS,  C-B failed JCC,
   RAX 0 FRAME32@,  RAX PUSH,  RAX 4 FRAME32@,  RAX PUSH,
   RAX ZERO-REG,  RAX PUSH,  done JMP,
   failed X64CODE:LBL,
   RAX MINUS-1,  RAX PUSH,  RAX PUSH,  RAX PUSH,
   done X64CODE:LBL,
   RSP CELL >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ ( fd cmd arg -- rc ): fcntl, as BFCNTL. FCNTL-NOSIGPIPE, the macOS
\ F_SETNOSIGPIPE, has no Linux command: it ignores SIGPIPE for the process
\ through rt_sigaction, as LINUX-IGNORE-SIGPIPE does. The four cells pushed
\ are the kernel's struct sigaction, the handler SIG_IGN and then zero flags,
\ restorer and mask, and the lea that drops them keeps the carry.
: FCNTL-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: real:label done:label :}
   RDX POP,  RSI POP,  RDI POP,
   RSI FCNTL-NOSIGPIPE >IMM8 ASM-SINK ENC-CMP-RI8  C-NE real JCC,
   RAX ZERO-REG,
   RAX ASM-SINK ENC-PUSH  RAX ASM-SINK ENC-PUSH  RAX ASM-SINK ENC-PUSH
   RAX SIG-IGN IMM32,  RAX ASM-SINK ENC-PUSH
   RDI SIGPIPE IMM32,  RSI RSP ASM-SINK ENC-MOV-RR  RDX ZERO-REG,
   R10 SIGSET-BYTES IMM32,
   NR-SIGACTION SYS,
   RSP 4 CELL * FRAME-AT,  done JMP,
   real X64CODE:LBL,
   NR-FCNTL SYS,
   done X64CODE:LBL,
   SYS-PUSH ;

\ ( fds nfds ms -- n|0|-errno ): ppoll, as BPOLL. The pollfd array's nfds * 8
\ bytes are guarded first, and an nfds whose byte length wraps is the
\ all-address span. ms becomes a timespec, and a negative ms none, which waits
\ without end. rax is pushed raw: poll alone is not restarted after a signal,
\ so its caller needs -EINTR (habu1.f's errno rule).
: POLL-ROW ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scaled:label guard:label call:label :}
   RDI 2 PEEK,  RSI 1 PEEK,
   RAX RSI ASM-SINK ENC-MOV-RR  RAX NFDS-WRAP-SHIFT >IMM8 ASM-SINK ENC-SHR-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E scaled JCC,
   RSI MINUS-1,  guard JMP,
   scaled X64CODE:LBL,
   RSI 3 >IMM8 ASM-SINK ENC-SHL-RI8
   guard X64CODE:LBL,
   RDI RSI PROT-SPAN-CALL,
   RAX POP,  RSI POP,  RDI POP,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-SUB-RI8
   RDX ZERO-REG,
   RAX RAX ASM-SINK ENC-TEST-RR  C-S call JCC,
   RCX MS-PER-S IMM32,  RCX ASM-SINK ENC-DIV
   RDX RDX NS-PER-MS >IMM32 ASM-SINK ENC-IMUL-RRI32
   RAX 0 FRAME!,  RDX CELL FRAME!,
   RDX RSP ASM-SINK ENC-MOV-RR
   call X64CODE:LBL,
   R10 ZERO-REG,  R8 ZERO-REG,  NR-POLL SYS,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-ADD-RI8
   RAX PUSH, ;

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
   s" getpid" [: NR-GETPID SYS-PUSH, ;] PRIM
   s" pipe" [: PIPE-ROW ;] PRIM
   s" dup2" [: RSI POP, RDI POP, RDX ZERO-REG,  NR-DUP2 SYS-PUSH, ;] PRIM
   s" fcntl" [: FCNTL-ROW ;] PRIM
   s" poll" [: POLL-ROW ;] PRIM
   s" kill" [: RSI POP, RDI POP,  NR-KILL SYS-PUSH, ;] PRIM
   s" setpgid" [: RSI POP, RDI POP,  NR-SETPGID SYS-PUSH, ;] PRIM
   s" proc-watch-open" [: BPROCWATCHOPEN ;] PRIM
   s" kill-errno" [: BKILLERRNO ;] PRIM
   s" execve" [: BEXECVE ;] PRIM
   s" fork" [: CLONE-ARGS,  NR-FORK SYS-PUSH, ;] PRIM
   s" wait-status" [: WAIT-STATUS-ROW ;] PRIM
   s" spawn-io" [: SPAWN-IO-ROW ;] PRIM
   s" spawn-argv-io" [: SPAWN-ARGV-IO-ROW ;] PRIM
   s" spawn-argv-env-io" [: SPAWN-ENV-IO-ROW ;] PRIM
   s" spawn-argv-env-cwd-io" [: SPAWN-CWD-IO-ROW ;] PRIM
   s" run-rc" [: RUN-RC-ROW ;] PRIM ;

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

: CORRUPT$ ( -- ptr u8 n ) S\" hb: catch frame corrupt\n" ;

\ Refuse only the empty typed quotation value at a public invocation boundary.
: CALLABLE, ( -- )
   X64CODE:LBL {: live:label :}
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE live JCC,
   S\" hb: unset quotation\n" ENGINE-ERROR:CALLABLE-ABI STDERR-EXIT,
   live X64CODE:LBL, ;

\ run-in-stack's frame: the caller's data stack pointer and descriptor.
0 constant RUN-DSP
8 constant RUN-BASE
16 constant RUN-CAP
24 constant RUN-BYTES

\ Exit with rdi when it is a status in [n, 255], and with UNCAUGHT-RC otherwise:
\ the kernel would keep only the low byte, so a negative code could exit 0.
\ The tails of habu1.f BDIE (n = 0) and BTHROW (n = 1).
: RC-EXIT, ( n -- ) {: lo:n :}
   X64CODE:LBL {: wide:label :}
   RAX RDI lo negate MEM-OFF ASM-SINK ENC-LEA
   RAX 255 lo - >IMM32 ASM-SINK ENC-CMP-RI32  C-A wide JCC,
   NR-EXIT-GROUP SYS,
   wide X64CODE:LBL,
   UNCAUGHT-RC EXIT-GROUP, ;

\ execute-floor ( n -- bool ): call the xt, then answer whether it left the
\ data stack below the base in S0-CELL. Below, the stack is reset to the base
\ before the flag is pushed, so the push lands at the base and not on the low
\ guard page.
: EXECUTE-FLOOR, ( -- )
   X64CODE:LBL {: above:label :}
   RAX POP,  RAX ASM-SINK ENC-CALL-REG
   RCX ZERO-REG,
   RAX DATA-REG S0-CELL MOV-LOAD,
   DSP RAX ASM-SINK ENC-CMP-RR  C-AE above JCC,
   DSP RAX ASM-SINK ENC-MOV-RR
   RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   above X64CODE:LBL,
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
   X64CODE:LBL {: resume:label :}
   RAX POP,  CALLABLE,
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
   resume X64CODE:LBL, ;

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
   X64CODE:LBL {: bare:label :}
   RAX ASM-SINK ENC-PUSH
   RCX DATA-REG UNCGH-CELL MOV-LOAD,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   RAX PUSH,  RCX ASM-SINK ENC-CALL-REG
   bare X64CODE:LBL,
   RDI ASM-SINK ENC-POP
   1 RC-EXIT, ;

\ Throw the code in rax: BTHROW without its evaluate and REPL arms, which jump
\ into habu2.f routines the x86-64 engine never has. Habu's `evaluate`
\ recovers through `catch`. A handler's frame is admitted, restored and resumed;
\ one that fails its admission writes `hb: catch frame corrupt` on fd 2 and
\ exits ENGINE-ERROR:CATCH-STACK. The text follows the exit.
: THROW, ( -- )
   X64CODE:LBL X64CODE:LBL {: none:label corrupt:label :}
   RAX ASM-SINK ENC-PUSH
   LCLOSE-LBL CALL,
   RAX ASM-SINK ENC-POP
   RDX DATA-REG HND-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E none JCC,
   corrupt FRAME-OK,
   RESUME,
   none X64CODE:LBL,
   UNCAUGHT,
   corrupt X64CODE:LBL,
   CORRUPT$ ENGINE-ERROR:CATCH-STACK STDERR-EXIT, ;

\ finally ( xt xt -- ): run the body under CAUGHT, and then the cleanup outside
\ it, so the cleanup's throw supersedes the body's; then rethrow the body's
\ code unless it is 0. The cleanup's xt and then the code wait in a two-cell
\ frame, which a throw inside the body leaves in place.
: FINALLY, ( -- )
   X64CODE:LBL {: done:label :}
   RAX POP,  CALLABLE,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP 0 MOV-STORE,
   CAUGHT,
   RAX RSP CELL MOV-STORE,
   RAX RSP 0 MOV-LOAD,  RAX ASM-SINK ENC-CALL-REG
   RAX RSP CELL MOV-LOAD,
   RSP 2 CELL * >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   THROW,
   done X64CODE:LBL, ;

: C2-INVOKE, ( -- )
   X64CODE:LBL X64CODE:LBL {: clean:label done:label :}
   RAX POP,
   RSP 4 CELL * >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP CELL MOV-STORE,
   RAX POP,  RAX RSP 0 MOV-STORE,
   CAUGHT,
   RAX RSP 2 CELL * MOV-STORE,
   RAX RSP 0 MOV-LOAD,  RAX PUSH,  CAUGHT,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E clean JCC,
   RAX RSP 2 CELL * MOV-STORE,
   clean X64CODE:LBL,
   RAX RSP 2 CELL * MOV-LOAD,  RAX PUSH,
   RAX RSP CELL MOV-LOAD,  RAX ASM-SINK ENC-CALL-REG
   RAX RSP 2 CELL * MOV-LOAD,
   RSP 4 CELL * >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   THROW,
   done X64CODE:LBL, ;

: C2-INIT-STOW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bad:label copy:label done:label :}
   R8 POP,  R9 POP,  R10 POP,  R11 POP,  RDI POP,
   R11 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-LE bad JCC,
   R10 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-LE bad JCC,
   R9 8 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RAX R11 ASM-SINK ENC-MOV-RR
   RAX 3 >IMM8 ASM-SINK ENC-SHL-RI8
   RAX R10 ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX R10 ASM-SINK ENC-SUB-RR
   RAX 16 >IMM8 ASM-SINK ENC-SUB-RI8
   RCX RAX 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RDX RAX 8 MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bad JCC,
   RCX 7 >IMM32 ASM-SINK ENC-TEST-RI32  C-NE bad JCC,
   RDX R10 ASM-SINK ENC-CMP-RR  C-L bad JCC,
   RAX R8 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RCX R8 16 MEM-OFF ASM-SINK ENC-MOV-MR
   R10 R8 24 MEM-OFF ASM-SINK ENC-MOV-MR
   RDX R8 40 MEM-OFF ASM-SINK ENC-MOV-MR
   RAX 2 IMM32,  RAX R8 0 MEM-OFF ASM-SINK ENC-MOV-MR
   copy X64CODE:LBL,
      RSI POP,
      RAX R11 ASM-SINK ENC-MOV-RR
      RAX ASM-SINK ENC-DEC
      RAX 3 >IMM8 ASM-SINK ENC-SHL-RI8
      RAX RCX ASM-SINK ENC-ADD-RR
      RSI RAX 0 MEM-OFF ASM-SINK ENC-MOV-MR
      R11 ASM-SINK ENC-DEC  C-NE copy JCC,
   RSI POP,  R10 PUSH,  RDI PUSH,  done JMP,
   bad X64CODE:LBL,
   RAX -6101 >IMM64 ASM-SINK ENC-MOV-RI64
   THROW,
   done X64CODE:LBL, ;

: C2-RECORDS-STOW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: bad:label discard:label first:label copy:label cell-copy:label done:label exit-code:label :}
   R8 POP,  R9 POP,  R10 POP,  R11 POP,  RDI POP,
   R11 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-LE bad JCC,
   R10 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-LE bad JCC,
   R9 8 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RAX R11 ASM-SINK ENC-MOV-RR
   RAX 3 >IMM8 ASM-SINK ENC-SHL-RI8
   RAX R10 ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX R10 ASM-SINK ENC-SUB-RR
   RAX 24 >IMM8 ASM-SINK ENC-SUB-RI8
   RCX RAX 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RDX RAX 8 MEM-OFF ASM-SINK ENC-MOV-RM
   R9 RAX 16 MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bad JCC,
   RCX 7 >IMM32 ASM-SINK ENC-TEST-RI32  C-NE bad JCC,
   R9 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-L bad JCC,
   RDX 0 >IMM8 ASM-SINK ENC-CMP-RI8  C-L bad JCC,
   RDX R8 40 MEM-OFF ASM-SINK ENC-MOV-MR
   RAX RDX ASM-SINK ENC-MOV-RR
   RDX ZERO-REG,
   R10 ASM-SINK ENC-DIV
   R9 RAX ASM-SINK ENC-CMP-RR  C-A bad JCC,
   RAX R8 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RCX R8 16 MEM-OFF ASM-SINK ENC-MOV-MR
   RAX R9 ASM-SINK ENC-MOV-RR
   RAX R10 ASM-SINK ENC-IMUL-RR
   RAX R8 24 MEM-OFF ASM-SINK ENC-MOV-MR
   RAX 2 IMM32,  RAX R8 0 MEM-OFF ASM-SINK ENC-MOV-MR
   R9 R9 ASM-SINK ENC-TEST-RR  C-NE first JCC,
   discard X64CODE:LBL,
      RSI POP,
      R11 ASM-SINK ENC-DEC  C-NE discard JCC,
      done JMP,
   first X64CODE:LBL,
      RSI POP,
      RAX R11 ASM-SINK ENC-MOV-RR
      RAX ASM-SINK ENC-DEC
      RAX 3 >IMM8 ASM-SINK ENC-SHL-RI8
      RAX RCX ASM-SINK ENC-ADD-RR
      RSI RAX 0 MEM-OFF ASM-SINK ENC-MOV-MR
      R11 ASM-SINK ENC-DEC  C-NE first JCC,
   R9 ASM-SINK ENC-DEC  C-E done JCC,
   RAX RCX ASM-SINK ENC-MOV-RR
   RAX R10 ASM-SINK ENC-ADD-RR
   copy X64CODE:LBL,
      RDX RCX ASM-SINK ENC-MOV-RR
      R11 R10 ASM-SINK ENC-MOV-RR
      R11 3 >IMM8 ASM-SINK ENC-SHR-RI8
   cell-copy X64CODE:LBL,
      RSI RDX 0 MEM-OFF ASM-SINK ENC-MOV-RM
      RSI RAX 0 MEM-OFF ASM-SINK ENC-MOV-MR
      RDX 8 >IMM8 ASM-SINK ENC-ADD-RI8
      RAX 8 >IMM8 ASM-SINK ENC-ADD-RI8
      R11 ASM-SINK ENC-DEC  C-NE cell-copy JCC,
      R9 ASM-SINK ENC-DEC  C-NE copy JCC,
   done X64CODE:LBL,
   RSI POP,  RSI POP,
   RAX R8 24 MEM-OFF ASM-SINK ENC-MOV-RM
   RDX ZERO-REG,
   R10 ASM-SINK ENC-DIV
   RAX PUSH,  RDI PUSH,  exit-code JMP,
   bad X64CODE:LBL,
   RAX -6101 >IMM64 ASM-SINK ENC-MOV-RI64
   THROW,
   exit-code X64CODE:LBL, ;

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
   RSI X64LAYOUT:DATA-VA VA>N >IMM64 ASM-SINK ENC-MOV-RI64
   RDI RSI ASM-SINK ENC-SUB-RR
   RSI X64LAYOUT:DATA-SIZE >IMM64 ASM-SINK ENC-MOV-RI64
   RDI RSI ASM-SINK ENC-CMP-RR  C-B bad JCC, ;

\ run-in-stack ( xt ptr u8 n -- ): run the xt on the extent as its data stack.
\ An extent that is not a guarded mapping is the caller's error, so it throws
\ STACK-ABI:E-STACK-UNGUARDED over the three cells, as BRUNSTACK does. The
\ extent becomes the data stack and its descriptor for the call; on the return
\ the caller's saved descriptor and cursor are admitted, since nothing proves
\ what the callback left of them, and restored.
: RUN-IN-STACK, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: unguarded:label bad:label done:label :}
   RAX DSP -3 CELL * MOV-LOAD,
   RDX DSP -2 CELL * MOV-LOAD,
   RCX DSP CELL negate MOV-LOAD,
   CALLABLE,
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
   unguarded X64CODE:LBL,
   RAX STACK-ABI:E-STACK-UNGUARDED >IMM32 ASM-SINK ENC-MOV-RI32
   THROW,
   bad X64CODE:LBL,
   EXIT-BOUNDS
   done X64CODE:LBL, ;

\ source-unit-run ( xt -- ): the callback's whole execution uses the caller's
\ cursor as its floor. No evaluator frame or SOURCE state changes. The saved
\ cursor is compared before any data-stack push, including when the callback
\ returns with the guarded extent exactly full.
: SOURCE-UNIT-RUN, ( -- )
   X64CODE:LBL X64CODE:LBL {: restore:label done:label :}
   TASK-LIVE-GUARD,
   RAX POP,  CALLABLE,
   RSP RUN-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   DSP RSP RUN-DSP MOV-STORE,
   RCX DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,  RCX RSP RUN-BASE MOV-STORE,
   RDX DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,   RDX RSP RUN-CAP MOV-STORE,
   RSI DSP ASM-SINK ENC-MOV-RR
   RSI RCX ASM-SINK ENC-SUB-RR
   RDX RSI ASM-SINK ENC-SUB-RR
   DSP DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RDX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   RAX ASM-SINK ENC-CALL-REG
   RCX RSP RUN-DSP MOV-LOAD,
   RAX ZERO-REG,
   DSP RCX ASM-SINK ENC-CMP-RR  C-E restore JCC,
   RAX STACK-ABI:E-EVAL-RESIDUE >IMM32 ASM-SINK ENC-MOV-RI32
   C-AE restore JCC,
   RAX 70 >IMM32 ASM-SINK ENC-MOV-RI32
   restore X64CODE:LBL,
   RDX RSP RUN-BASE MOV-LOAD,  RDX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RDX RSP RUN-CAP MOV-LOAD,   RDX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP RCX ASM-SINK ENC-MOV-RR
   RSP RUN-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   THROW,
   done X64CODE:LBL, ;

\ A closed evaluator's catch lives on the caller's stack. Its called arm finds
\ the owned segment and source in the machine frame above that catch; a throw
\ resumes after CAUGHT, with the frame still available for pooling.
0 constant CLOSED-A
8 constant CLOSED-U
16 constant CLOSED-DSP
24 constant CLOSED-BASE
32 constant CLOSED-CAP
40 constant CLOSED-SEG
48 constant CLOSED-INNER-DSP
56 constant CLOSED-CODE
64 constant CLOSED-BYTES
STACK-ABI:CATCH-BYTES CELL + constant CLOSED-ARM-OFF

: CLOSED-ARM, ( -- )
   RDX RSP CLOSED-ARM-OFF CLOSED-SEG + MOV-LOAD,
   RDX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RCX STACK-ABI:PAGE-BYTES IMM32,
   RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP RDX ASM-SINK ENC-MOV-RR
   RAX RSP CLOSED-ARM-OFF CLOSED-A + MOV-LOAD,  RAX PUSH,
   RAX RSP CLOSED-ARM-OFF CLOSED-U + MOV-LOAD,  RAX PUSH,
   RAX DATA-REG PROVIDED-XT:EVALUATE-CELL MOV-LOAD,
   CALLABLE,
   RAX ASM-SINK ENC-CALL-REG
   DSP RSP CLOSED-ARM-OFF CLOSED-INNER-DSP + MOV-STORE,
   ASM-SINK ENC-RET ;

\ The idle stack's first cell links the pool. Mapping follows boot-x64's
\ guarded allocation: an inaccessible page lies on each side of the aligned
\ 64 KiB data extent. The source and caller descriptor wait in a machine frame
\ across mmap and across the guarded call.
: EVAL-CLOSED, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: have:label take:label map-fail:label pool:label done:label :}
   X64CODE:LBL {: arm:label :}
   TASK-LIVE-GUARD,
   RSP CLOSED-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RAX POP,  RAX RSP CLOSED-U MOV-STORE,
   RAX POP,  RAX RSP CLOSED-A MOV-STORE,
   DSP RSP CLOSED-DSP MOV-STORE,
   RAX DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   RAX RSP CLOSED-BASE MOV-STORE,
   RAX DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,
   RAX RSP CLOSED-CAP MOV-STORE,
   RAX DATA-REG CLOSED-FREE-CELL MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE have JCC,
   RDI ZERO-REG,
   RSI STACK-ABI:PAGE-BYTES 4 * IMM32,
   RDX 0 IMM32,
   R10 MAP-ANON-PRIVATE IMM32,
   R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B map-fail JCC,
   RDI RAX STACK-ABI:PAGE-BYTES 2 * 1- MEM-OFF ASM-SINK ENC-LEA
   RDI STACK-ABI:PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   RSI STACK-ABI:PAGE-BYTES IMM32,
   RDX PROT-RW IMM32,
   R10 MAP-ANON-PRIVATE-FIXED IMM32,
   R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,
   RAX RDI ASM-SINK ENC-CMP-RR  C-NE map-fail JCC,
   take JMP,
   have X64CODE:LBL,
   RCX RAX 0 MOV-LOAD,
   RCX DATA-REG CLOSED-FREE-CELL MOV-STORE,
   take X64CODE:LBL,
   RAX RSP CLOSED-SEG MOV-STORE,
   RAX arm MOVABS,  RAX PUSH,
   CAUGHT,
   RAX RSP CLOSED-CODE MOV-STORE,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE pool JCC,
   RDX RSP CLOSED-INNER-DSP MOV-LOAD,
   RCX RSP CLOSED-SEG MOV-LOAD,
   RDX RCX ASM-SINK ENC-CMP-RR  C-E pool JCC,
   RAX STACK-ABI:E-EVAL-RESIDUE >IMM32 ASM-SINK ENC-MOV-RI32
   RAX RSP CLOSED-CODE MOV-STORE,
   C-A pool JCC,
   RAX 70 IMM32,
   RAX RSP CLOSED-CODE MOV-STORE,
   pool X64CODE:LBL,
   RCX RSP CLOSED-BASE MOV-LOAD,
   RCX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   RCX RSP CLOSED-CAP MOV-LOAD,
   RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP RSP CLOSED-DSP MOV-LOAD,
   RDX RSP CLOSED-SEG MOV-LOAD,
   RCX DATA-REG CLOSED-FREE-CELL MOV-LOAD,
   RCX RDX 0 MOV-STORE,
   RDX DATA-REG CLOSED-FREE-CELL MOV-STORE,
   RAX RSP CLOSED-CODE MOV-LOAD,
   RSP CLOSED-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   THROW,
   done JMP,
   map-fail X64CODE:LBL,
   S\" hb: cannot map guarded VM stack\n" 78 STDERR-EXIT,
   arm X64CODE:LBL,
   CLOSED-ARM,
   done X64CODE:LBL, ;

\ die ( ptr u8 n n -- ): the message and LF on fd 2 when its length is
\ positive, then the exit hook, then the exit. The rc waits on the machine
\ stack, and the LF is a cell pushed there for its write. The hook's cell is
\ cleared before the call (layout.f EXIT-HOOK-CELL, habu2.f EMIT-EXITHOOK), so
\ a hook that dies or throws finds it empty.
: DIE, ( -- )
   X64CODE:LBL X64CODE:LBL {: quiet:label bare:label :}
   RAX POP,  RDX POP,  RSI POP,
   RAX ASM-SINK ENC-PUSH
   RDX RDX ASM-SINK ENC-TEST-RR  C-LE quiet JCC,
   RDI STDERR IMM32,  NR-WRITE SYS,
   RCX STR-LF IMM32,  RCX ASM-SINK ENC-PUSH
   RDI STDERR IMM32,  RSI RSP ASM-SINK ENC-MOV-RR  RDX 1 IMM32,  NR-WRITE SYS,
   RCX ASM-SINK ENC-POP
   quiet X64CODE:LBL,
   RAX DATA-REG EXIT-HOOK-CELL MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   RCX ZERO-REG,  RCX DATA-REG EXIT-HOOK-CELL MOV-STORE,
   RAX ASM-SINK ENC-CALL-REG
   bare X64CODE:LBL,
   RDI ASM-SINK ENC-POP
   0 RC-EXIT, ;

\ A source token's scope is a dictionary-record lookup. The one-wordlist
\ FIND-LBL helper also serves the public search rows; using it here keeps
\ package, qualified and used-public names on the same name-folding path.
64 constant SCOPE-FRAME
0 constant SF-TOKEN
8 constant SF-LEN
16 constant SF-BOUND
24 constant SF-USED
32 constant SF-USED2
40 constant SF-COLON
48 constant SF-INDEX

\ scope-find ( ptr u8 n -- bound used used2 flags ). A qualified name searches
\ the namespace record's public wid. Bare names search the open private/public
\ wids, then global, while the used publics are collected independently. Two
\ distinct used records set flag 2 and bind neither when the primary search
\ missed; a bound seeded primitive sets flag 1.
: SCOPE-FIND-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: colon-scan:label colon-hit:label bare:label qual-scan:label
      qual-find:label primary-done:label pub:label global:label
      used-loop:label used-next:label used-first:label used-done:label
      bound:label seed-done:label push-out:label qual-bad:label seed-skip:label :}
   RSI POP,  RDI POP,
   RSP SCOPE-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   RDI RSP SF-TOKEN MOV-STORE,  RSI RSP SF-LEN MOV-STORE,
   RAX ZERO-REG,
   RAX RSP SF-BOUND MOV-STORE,
   RAX RSP SF-USED MOV-STORE,
   RAX RSP SF-USED2 MOV-STORE,
   RAX RSP SF-INDEX MOV-STORE,
   RAX -1 IMM64,  RAX RSP SF-COLON MOV-STORE,
   RCX ZERO-REG,
   colon-scan X64CODE:LBL,
   RCX RSI ASM-SINK ENC-CMP-RR  C-GE bare JCC,
   RAX RDI RCX 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX $3A >IMM8 ASM-SINK ENC-CMP-RI8  C-E colon-hit JCC,
   RCX ASM-SINK ENC-INC  colon-scan JMP,
   colon-hit X64CODE:LBL,
   RCX RSP SF-COLON MOV-STORE,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   R8 RCX 1 MEM-OFF ASM-SINK ENC-LEA
   R8 RSI ASM-SINK ENC-CMP-RR  C-GE bare JCC,
   qual-scan X64CODE:LBL,
   R8 RSI ASM-SINK ENC-CMP-RR  C-GE qual-find JCC,
   RAX RDI R8 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX $3A >IMM8 ASM-SINK ENC-CMP-RI8  C-E qual-bad JCC,
   R8 ASM-SINK ENC-INC  qual-scan JMP,
   qual-find X64CODE:LBL,
   RSI RCX ASM-SINK ENC-MOV-RR
   RDX DICT-WL:NAMESPACE IMM64,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E primary-done JCC,
   RDX RAX REC-CODE MOV-LOAD,
   RCX RSP SF-COLON MOV-LOAD,
   RDI RSP SF-TOKEN MOV-LOAD,
   RDI RDI RCX 1 1 MEM-IDX ASM-SINK ENC-LEA
   RSI RSP SF-LEN MOV-LOAD,
   RSI RCX ASM-SINK ENC-SUB-RR
   RSI ASM-SINK ENC-DEC
   FIND-LBL CALL,
   RAX RSP SF-BOUND MOV-STORE,
   primary-done JMP,
   qual-bad X64CODE:LBL,
   primary-done X64CODE:LBL,
   RAX RSP SF-COLON MOV-LOAD,
   RAX -1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE used-done JCC,
   used-loop JMP,
   bare X64CODE:LBL,
   RDI RSP SF-TOKEN MOV-LOAD,  RSI RSP SF-LEN MOV-LOAD,
   RDX DATA-REG PKG-PRI-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E pub JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE bound JCC,
   pub X64CODE:LBL,
   RDX DATA-REG PKG-PUB-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E global JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE bound JCC,
   global X64CODE:LBL,
   RDX ZERO-REG,
   FIND-LBL CALL,
   bound X64CODE:LBL,
   RAX RSP SF-BOUND MOV-STORE,
   RAX RSP SF-COLON MOV-LOAD,
   RAX -1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE used-done JCC,
   used-loop X64CODE:LBL,
   RCX RSP SF-INDEX MOV-LOAD,
   RAX DATA-REG USE-DEPTH-CELL MOV-LOAD,
   RCX RAX ASM-SINK ENC-CMP-RR  C-GE used-done JCC,
   RDX DATA-REG RCX CELL USE-WIDS-OFF MEM-IDX ASM-SINK ENC-MOV-RM
   RDI RSP SF-TOKEN MOV-LOAD,  RSI RSP SF-LEN MOV-LOAD,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E used-next JCC,
   RCX RSP SF-USED MOV-LOAD,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E used-first JCC,
   RAX RCX ASM-SINK ENC-CMP-RR  C-E used-next JCC,
   RAX RSP SF-USED2 MOV-STORE,
   used-done JMP,
   used-first X64CODE:LBL,
   RAX RSP SF-USED MOV-STORE,
   used-next X64CODE:LBL,
   RCX RSP SF-INDEX MOV-LOAD,
   RCX ASM-SINK ENC-INC
   RCX RSP SF-INDEX MOV-STORE,
   used-loop JMP,
   used-done X64CODE:LBL,
   RAX RSP SF-BOUND MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE push-out JCC,
   RCX RSP SF-USED2 MOV-LOAD,
   RCX RCX ASM-SINK ENC-TEST-RR  C-NE push-out JCC,
   RAX RSP SF-USED MOV-LOAD,
   push-out X64CODE:LBL,
   RCX ZERO-REG,
   RDX RSP SF-USED2 MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E seed-done JCC,
   RCX 2 IMM32,
   seed-done X64CODE:LBL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E seed-skip JCC,
   RDX RAX ASM-SINK ENC-MOV-RR
   RDX DBASE-REG ASM-SINK ENC-SUB-RR
   RDX ENGINE-PRIMS:COUNT DREC * >IMM32 ASM-SINK ENC-CMP-RI32  C-AE seed-skip JCC,
   RCX 1 >IMM8 ASM-SINK ENC-OR-RI8
   seed-skip X64CODE:LBL,
   RAX PUSH,
   RAX RSP SF-USED MOV-LOAD,  RAX PUSH,
   RAX RSP SF-USED2 MOV-LOAD,  RAX PUSH,
   RCX PUSH,
   RSP SCOPE-FRAME >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ parse-name follows the shared source cursor. Whitespace is every byte at
\ or below space; on exhaustion the previous token cells remain untouched.
: PARSE-NAME-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: skip:label scan:label found:label none:label done:label finish:label :}
   RAX DATA-REG INP-CELL MOV-LOAD,
   RDX DATA-REG INE-CELL MOV-LOAD,
   skip X64CODE:LBL,
   RAX RDX ASM-SINK ENC-CMP-RR  C-GE none JCC,
   RCX RAX MEM-AT ASM-SINK ENC-MOVZX-8-RM
   RCX 32 >IMM8 ASM-SINK ENC-CMP-RI8  C-A found JCC,
   RAX ASM-SINK ENC-INC  skip JMP,
   found X64CODE:LBL,
   RSI RAX ASM-SINK ENC-MOV-RR
   RSI DATA-REG TKA-CELL MOV-STORE,
   scan X64CODE:LBL,
   RAX RDX ASM-SINK ENC-CMP-RR  C-GE done JCC,
   RCX RAX MEM-AT ASM-SINK ENC-MOVZX-8-RM
   RCX 32 >IMM8 ASM-SINK ENC-CMP-RI8  C-BE done JCC,
   RAX ASM-SINK ENC-INC  scan JMP,
   done X64CODE:LBL,
   RAX DATA-REG INP-CELL MOV-STORE,
   RCX RAX ASM-SINK ENC-MOV-RR
   RCX RSI ASM-SINK ENC-SUB-RR
   RCX DATA-REG TKL-CELL MOV-STORE,
   RSI PUSH,  RCX PUSH,
   finish JMP,
   none X64CODE:LBL,
   RAX DATA-REG INP-CELL MOV-STORE,
   RAX PUSH,
   RAX ZERO-REG,  RAX PUSH,
   finish X64CODE:LBL, ;

\ tok-imm? asks the primary scope only: a used public does not make a token
\ immediate to a checked body. FIND-LBL supplies the same folded-name record
\ lookup as scope-find; the immediate flag is its DNAME-IMM bit folded to 2.
: TOK-IMM-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: scan:label colon:label bare:label qual-scan:label qual-find:label
      pub:label global:label result:label miss:label done:label :}
   RSI POP,  RDI POP,
   RSP 24 >IMM8 ASM-SINK ENC-SUB-RI8
   RDI RSP 0 MOV-STORE,  RSI RSP 8 MOV-STORE,
   RCX ZERO-REG,
   scan X64CODE:LBL,
   RCX RSI ASM-SINK ENC-CMP-RR  C-GE bare JCC,
   RAX RDI RCX 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX $3A >IMM8 ASM-SINK ENC-CMP-RI8  C-E colon JCC,
   RCX ASM-SINK ENC-INC  scan JMP,
   colon X64CODE:LBL,
   RCX RSP 16 MOV-STORE,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bare JCC,
   R8 RCX 1 MEM-OFF ASM-SINK ENC-LEA
   R8 RSI ASM-SINK ENC-CMP-RR  C-GE bare JCC,
   qual-scan X64CODE:LBL,
   R8 RSI ASM-SINK ENC-CMP-RR  C-GE qual-find JCC,
   RAX RDI R8 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX $3A >IMM8 ASM-SINK ENC-CMP-RI8  C-E miss JCC,
   R8 ASM-SINK ENC-INC  qual-scan JMP,
   qual-find X64CODE:LBL,
   RSI RCX ASM-SINK ENC-MOV-RR
   RDX DICT-WL:NAMESPACE IMM64,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E miss JCC,
   RDX RAX REC-CODE MOV-LOAD,
   RCX RSP 16 MOV-LOAD,
   RDI RSP 0 MOV-LOAD,
   RDI RDI RCX 1 1 MEM-IDX ASM-SINK ENC-LEA
   RSI RSP 8 MOV-LOAD,
   RSI RCX ASM-SINK ENC-SUB-RR
   RSI ASM-SINK ENC-DEC
   FIND-LBL CALL,
   result JMP,
   bare X64CODE:LBL,
   RDI RSP 0 MOV-LOAD,  RSI RSP 8 MOV-LOAD,
   RDX DATA-REG PKG-PRI-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E pub JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE result JCC,
   pub X64CODE:LBL,
   RDX DATA-REG PKG-PUB-CELL MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E global JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE result JCC,
   global X64CODE:LBL,
   RDX ZERO-REG,  FIND-LBL CALL,
   result X64CODE:LBL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E miss JCC,
   RAX RAX REC-FLAGS MOV-LOAD,
   RAX 59 >IMM8 ASM-SINK ENC-SHR-RI8
   RAX 2 >IMM8 ASM-SINK ENC-AND-RI8
   done JMP,
   miss X64CODE:LBL,
   RAX ZERO-REG,
   done X64CODE:LBL,
   RAX PUSH,
   RSP 24 >IMM8 ASM-SINK ENC-ADD-RI8 ;

public

\ Habu supplies the interpreter-bound rows on x86-64. The provided evaluate
\ entry is installed by that evaluator; evaluate-closed gives it a guarded
\ stack. Rows still tied to ARM's interpreter refuse until supplied in Habu.
: CONTROL, ( -- )
   FLOORREC-PENDING @ if
      false FLOORREC-PENDING !
   else
      X64CODE:LBL FLOORREC-ENTRY !
   then
   X64CODE:LBL {: after-floor:label :}
   after-floor JMP,
   FLOORREC-LBL X64CODE:LBL,
   DSP DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   RAX 70 IMM32,
   THROW,
   after-floor X64CODE:LBL,
   s" execute" [: RAX POP,  CALLABLE,  RAX ASM-SINK ENC-CALL-REG ;] PRIM
   s" execute-floor" [: EXECUTE-FLOOR, ;] PRIM
   s" 2>r" [: 2>R, ;] PRIM
   s" 2r>" [: 2R>, ;] PRIM
   s" 2r@" [: 2R@, ;] PRIM
   s" catch" [: CAUGHT,  RAX PUSH, ;] PRIM
   s" throw" [: RAX POP,  THROW, ;] PRIM
   s" finally" [: FINALLY, ;] PRIM
   s" c2-invoke" [: C2-INVOKE, ;] PRIM
   s" c2-init-stow" [: C2-INIT-STOW, ;] PRIM
   s" c2-records-stow" [: C2-RECORDS-STOW, ;] PRIM
   s" run-in-stack" [: RUN-IN-STACK, ;] PRIM
   s" die" [: DIE, ;] PRIM
   s" unit-compile-run" ENGINE-PRIMS:GLOBAL-INT-WID REFUSE-WID
   s" source-unit-run" [: SOURCE-UNIT-RUN, ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   PROVIDED-XT:EVALUATE-CELL s" evaluate" [: REFUSE-BODY ;] PROVIDED
   s" evaluate-closed" [: EVAL-CLOSED, ;] PRIM
   s" create" REFUSE
   s" parse-name" [: PARSE-NAME-BODY ;] PRIM
   s" num-parse" REFUSE
   s" tok-imm?" [: TOK-IMM-BODY ;] PRIM
   s" scope-find" [: SCOPE-FIND-BODY ;] PRIM
   s" scope-kind?" [: RAX POP,  RAX ZERO-REG,  RAX PUSH, ;] PRIM ;

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

\ The publication rows: the twins of habu1.f BPATCH32, BCODEPUBLISH,
\ BCALLMAPSET, BADDRMAPSET, RELOC-EMIT:BCLEAR-MAPS, BXREFRETARGET, BINTMARK
\ and BMININMARK and of habu2.f DOES-REC:NATIVE-PRIM, over the twins of
\ habu1.f EMIT-PROT-WINDOW's PROT-EMIT:LSPAN, LOPEN and LCLOSE. x86-64 keeps its
\ instruction cache coherent, so nothing flushes. An x86-64 span is
\ byte-granular: a routine may end in a one-byte ret, so no guard traps a
\ length or an address that is not a whole four-byte word. The two rows that
\ write at CP leave it on a CODE-SLOT: code-publish fills the gap with int3 and
\ does-record zero-pads its name.

private

4 constant PATCH-BYTES                 \ the word patch32 writes
52 constant MIN-IN-SHIFT               \ layout.f DNAME-MIN-IN-MASK: flag bits 52-59
$CC constant INT3                      \ the byte code-publish fills a slot's gap with

\ The helpers' labels, made by PUBLICATION, in the stream it emits them into.
variable LSPAN-CELL
variable LOPEN-CELL
variable ADD-SITE-CELL
variable DROP-SITES-CELL
variable SEAL-TRAP-CELL
variable SITE-TRAP-CELL
: LSPAN-LBL ( -- label ) LSPAN-CELL @ >LABEL ;
: LOPEN-LBL ( -- label ) LOPEN-CELL @ >LABEL ;
: ADD-SITE-LBL ( -- label ) ADD-SITE-CELL @ >LABEL ;
: DROP-SITES-LBL ( -- label ) DROP-SITES-CELL @ >LABEL ;
: SEAL-TRAP-LBL ( -- label ) SEAL-TRAP-CELL @ >LABEL ;
: SITE-TRAP-LBL ( -- label ) SITE-TRAP-CELL @ >LABEL ;

\ ---- the protection window ---------------------------------------------------
\ The region's write bands, recorded in the PROT cells as habu1.f keeps them:
\ the dictionary-record band [DBASE, DBASE+CFSTK-OFF) in RLO/RHI, the
\ control-flow band [DBASE+CFSTK-OFF, DBASE+DICT-SIZE) as the latch CF, and
\ the code band [DBASE+DICT-SIZE, DBASE+REGION) in WLO/WINDOW. A bracket
\ declares what it writes, a band only grows while it is open, and the close
\ flips back exactly the ranges the cells recorded, so no page stays
\ read-write. Every flip covers whole PAGE-BYTES pages, the one x86-64 page
\ size, where ARM64 rounds to PROT-PAGE-MAX for kernels of 4, 16 and 64 KiB
\ pages: CFSTK-OFF and DICT-SIZE are whole pages, so the control-flow band is
\ one page and no two bands share one.

\ Round the span in rdi (start) and rsi (end) outward to whole pages.
: PAGE-OUT, ( -- )
   RDI PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32
   RSI PAGE-BYTES 1- >IMM32 ASM-SINK ENC-ADD-RI32
   RSI PAGE-BYTES negate >IMM32 ASM-SINK ENC-AND-RI32 ;

\ Clamp the span [r8, r9) to the band [DBASE+lo, DBASE+hi), unsigned, into rdi
\ and rsi, and branch to the label when nothing is left. Clobbers rax.
: CLAMP, ( n n label -- ) {: lo:n hi:n none:label :}
   RDI R8 ASM-SINK ENC-MOV-RR
   RAX DBASE-REG lo MEM-OFF ASM-SINK ENC-LEA
   RDI RAX ASM-SINK ENC-CMP-RR  C-B RDI RAX ASM-SINK ENC-CMOVCC
   RSI R9 ASM-SINK ENC-MOV-RR
   RAX DBASE-REG hi MEM-OFF ASM-SINK ENC-LEA
   RSI RAX ASM-SINK ENC-CMP-RR  C-A RSI RAX ASM-SINK ENC-CMOVCC
   RSI RDI ASM-SINK ENC-CMP-RR  C-BE none JCC, ;

\ Union the page-aligned span in rdi and rsi into the band the two cells record
\ and flip the union read-write; a band that already covers the span costs no
\ syscall. The twin of habu1.f BAND-WIDEN,. Clobbers rax rcx rdx rsi rdi r11.
: BAND-WIDEN, ( n n -- ) {: locell:n hicell:n :}
   X64CODE:LBL X64CODE:LBL {: fresh:label done:label :}
   RAX DATA-REG locell MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E fresh JCC,        \ nothing open: the span opens it
   RDI RAX ASM-SINK ENC-CMP-RR  C-A RDI RAX ASM-SINK ENC-CMOVCC   \ the lower start
   RCX DATA-REG hicell MOV-LOAD,
   RSI RCX ASM-SINK ENC-CMP-RR  C-B RSI RCX ASM-SINK ENC-CMOVCC   \ the higher end
   RDI RAX ASM-SINK ENC-CMP-RR  C-NE fresh JCC,
   RSI RCX ASM-SINK ENC-CMP-RR  C-E done JCC,          \ unchanged: already covered
   fresh X64CODE:LBL,
   RDI DATA-REG locell MOV-STORE,  RSI DATA-REG hicell MOV-STORE,
   RSI RDI ASM-SINK ENC-SUB-RR
   RDX PROT-RW IMM32,
   NR-MPROTECT SYS,
   done X64CODE:LBL, ;

\ Flip the band the two cells record back to read-execute and clear the cells
\ first, so no band is claimed open past the flip: the twin of habu1.f
\ BAND-CLOSE,.
: BAND-CLOSE, ( n n -- ) {: locell:n hicell:n :}
   X64CODE:LBL {: skip:label :}
   RDI DATA-REG locell MOV-LOAD,
   RSI DATA-REG hicell MOV-LOAD,
   RSI RSI ASM-SINK ENC-TEST-RR  C-E skip JCC,
   RAX ZERO-REG,
   RAX DATA-REG locell MOV-STORE,  RAX DATA-REG hicell MOV-STORE,
   RSI RDI ASM-SINK ENC-SUB-RR
   RDX PROT-RX IMM32,
   NR-MPROTECT SYS,
   skip X64CODE:LBL, ;

\ Flip the pages that hold the control-flow band to prot n.
: CF-FLIP, ( n -- ) {: prot:n :}
   RDI DBASE-REG CFSTK-OFF MEM-OFF ASM-SINK ENC-LEA
   RSI DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   PAGE-OUT,
   RSI RDI ASM-SINK ENC-SUB-RR
   RDX prot IMM32,
   NR-MPROTECT SYS, ;

\ LSPAN ( rdi = address, rsi = byte length ): declare a span this bracket will
\ write, intersected with each band, so a span that reaches no band declares
\ nothing. r8 and r9 hold the span across the flips, which a syscall keeps.
\ Clobbers rax rcx rdx rsi rdi r8 r9 r11.
: LSPAN-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: rskip:label fskip:label cskip:label :}
   LSPAN-LBL X64CODE:LBL,
   R8 RDI ASM-SINK ENC-MOV-RR
   R9 RDI RSI 1 0 MEM-IDX ASM-SINK ENC-LEA
   0 CFSTK-OFF rskip CLAMP,                            \ the record band
   PAGE-OUT,
   PROT:RLO PROT:RHI BAND-WIDEN,
   rskip X64CODE:LBL,
   CFSTK-OFF DICT-SIZE fskip CLAMP,                    \ the control-flow band
   RAX DATA-REG PROT:CF MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE fskip JCC,       \ already declared
   RAX 1 IMM32,  RAX DATA-REG PROT:CF MOV-STORE,
   PROT-RW CF-FLIP,
   fskip X64CODE:LBL,
   DICT-SIZE REGION cskip CLAMP,                       \ the code band
   PAGE-OUT,
   PROT:WLO PROT:WINDOW BAND-WIDEN,
   cskip X64CODE:LBL,
   ASM-SINK ENC-RET ;

\ LOPEN ( rdi = one past the last byte to write ): LSPAN over [CP, rdi). An end
\ at or below CP declares the byte at CP.
: LOPEN-HELPER, ( -- )
   ENGINE-GPR:X64-CP >R64 {: cp:r64 :}
   X64CODE:LBL {: ok:label :}
   LOPEN-LBL X64CODE:LBL,
   RDI cp ASM-SINK ENC-CMP-RR  C-A ok JCC,
   RDI cp 1 MEM-OFF ASM-SINK ENC-LEA
   ok X64CODE:LBL,
   RSI RDI ASM-SINK ENC-MOV-RR  RSI cp ASM-SINK ENC-SUB-RR
   RDI cp ASM-SINK ENC-MOV-RR
   LSPAN-LBL JMP, ;                                    \ its ret is this one's

\ LCLOSE: flip every open band back to read-execute and clear its record. With
\ nothing open it flips nothing.
: LCLOSE-HELPER, ( -- )
   X64CODE:LBL {: xcf:label :}
   LCLOSE-LBL X64CODE:LBL,
   RAX DATA-REG PROT:CF MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E xcf JCC,
   RAX ZERO-REG,  RAX DATA-REG PROT:CF MOV-STORE,     \ cleared before the flip
   PROT-RX CF-FLIP,
   xcf X64CODE:LBL,
   PROT:RLO PROT:RHI BAND-CLOSE,
   PROT:WLO PROT:WINDOW BAND-CLOSE,
   ASM-SINK ENC-RET ;

\ ---- the site rows -----------------------------------------------------------
\ SNAP-RELOC's x86-64 band (src/habu/layout.f): a u64 count at SITE-N-CELL, then
\ rows of a u32 region offset and a u8 kind, strictly ascending by offset.

\ r8 = the first row, rcx = the count and r9 = past the last row. A count above
\ SITE-CAP is a corrupt band, SITE-RC's, as src/habu/sites.f refuses it.
: ROWS, ( -- )
   R8 DATA-REG SNAP-RELOC:SITE-ROWS-OFF MEM-OFF ASM-SINK ENC-LEA
   RCX DATA-REG SNAP-RELOC:SITE-N-CELL MOV-LOAD,
   RCX SNAP-RELOC:SITE-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-A SITE-TRAP-LBL JCC,
   R9 RCX SNAP-RELOC:SITE-ROW-BYTES >IMM8 ASM-SINK ENC-IMUL-RRI8
   R9 R8 ASM-SINK ENC-ADD-RR ;

\ Record the site at region offset rdi of kind rdx in its place. The scan runs
\ down from the last row, so a site above every row is an append; the rows
\ above a lower site move up one row, the top byte first. The same row again
\ changes nothing; another kind at a recorded offset, or a row past SITE-CAP,
\ exits SITE-RC. Clobbers rax rcx rsi r8 r9.
: ADD-SITE-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label same:label place:label move:label :}
   X64CODE:LBL {: moved:label :}
   SNAP-RELOC:SITE-ROW-BYTES {: row:n :}
   ADD-SITE-LBL X64CODE:LBL,
   ROWS,
   RSI R9 ASM-SINK ENC-MOV-RR                          \ rsi = the new row's place
   scan X64CODE:LBL,
   RSI R8 ASM-SINK ENC-CMP-RR  C-BE place JCC,         \ below every row
   0 >R32 RSI row negate MEM-OFF ASM-SINK ENC-MOV32-RM \ the offset of the row below
   RAX RDI ASM-SINK ENC-CMP-RR  C-B place JCC,
   C-E same JCC,
   RSI row >IMM8 ASM-SINK ENC-SUB-RI8
   scan JMP,
   same X64CODE:LBL,
   RAX RSI row negate SNAP-RELOC:SITE-KIND-OFF + MEM-OFF ASM-SINK ENC-MOVZX-8-RM
   RAX RDX ASM-SINK ENC-CMP-RR  C-NE SITE-TRAP-LBL JCC,
   ASM-SINK ENC-RET
   place X64CODE:LBL,
   RCX SNAP-RELOC:SITE-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-AE SITE-TRAP-LBL JCC,
   move X64CODE:LBL,
   R9 RSI ASM-SINK ENC-CMP-RR  C-BE moved JCC,
   R9 ASM-SINK ENC-DEC
   RAX R9 MEM-AT ASM-SINK ENC-MOVZX-8-RM
   0 >R8 R9 row MEM-OFF ASM-SINK ENC-MOV8-MR
   move JMP,
   moved X64CODE:LBL,
   RDI R64>N >R32 RSI MEM-AT ASM-SINK ENC-MOV32-MR
   RDX R64>N >R8 RSI SNAP-RELOC:SITE-KIND-OFF MEM-OFF ASM-SINK ENC-MOV8-MR
   RCX ASM-SINK ENC-INC
   RCX DATA-REG SNAP-RELOC:SITE-N-CELL MOV-STORE,
   ASM-SINK ENC-RET ;

\ Remove every row whose site lies in [rdi, rdi+rsi), both kinds: the twin of
\ habu1.f RELOC-EMIT:CLEAR-SPAN over both maps. The span's offsets compare
\ signed, so a span below the region removes nothing. Two scans down from the
\ last row find the first row at or above the span's end, in r10, and the first
\ at or above its start, in r11, counting the rows between in rdx; the rows
\ from r10 up move down over them. Clobbers rax rcx rdx rsi rdi r8-r11.
: DROP-SITES-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: high:label highs:label low:label lows:label :}
   X64CODE:LBL X64CODE:LBL {: move:label done:label :}
   SNAP-RELOC:SITE-ROW-BYTES {: row:n :}
   DROP-SITES-LBL X64CODE:LBL,
   RDI DBASE-REG ASM-SINK ENC-SUB-RR                   \ rdi = the span's first offset
   RSI RDI ASM-SINK ENC-ADD-RR                         \ rsi = past its last
   ROWS,
   R10 R9 ASM-SINK ENC-MOV-RR
   high X64CODE:LBL,
   R10 R8 ASM-SINK ENC-CMP-RR  C-BE highs JCC,
   0 >R32 R10 row negate MEM-OFF ASM-SINK ENC-MOV32-RM
   RAX RSI ASM-SINK ENC-CMP-RR  C-L highs JCC,
   R10 row >IMM8 ASM-SINK ENC-SUB-RI8
   high JMP,
   highs X64CODE:LBL,
   R11 R10 ASM-SINK ENC-MOV-RR
   RDX ZERO-REG,
   low X64CODE:LBL,
   R11 R8 ASM-SINK ENC-CMP-RR  C-BE lows JCC,
   0 >R32 R11 row negate MEM-OFF ASM-SINK ENC-MOV32-RM
   RAX RDI ASM-SINK ENC-CMP-RR  C-L lows JCC,
   R11 row >IMM8 ASM-SINK ENC-SUB-RI8
   RDX ASM-SINK ENC-INC
   low JMP,
   lows X64CODE:LBL,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E done JCC,         \ no row in the span
   RCX RDX ASM-SINK ENC-SUB-RR
   RCX DATA-REG SNAP-RELOC:SITE-N-CELL MOV-STORE,
   move X64CODE:LBL,
   R10 R9 ASM-SINK ENC-CMP-RR  C-AE done JCC,
   RAX R10 MEM-AT ASM-SINK ENC-MOVZX-8-RM
   0 >R8 R11 MEM-AT ASM-SINK ENC-MOV8-MR
   R10 ASM-SINK ENC-INC  R11 ASM-SINK ENC-INC
   move JMP,
   done X64CODE:LBL,
   ASM-SINK ENC-RET ;

\ The refusals the rows share: a span or index a guard refuses, and a site the
\ band cannot take.
: TRAPS, ( -- )
   SEAL-TRAP-LBL X64CODE:LBL,  ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   SITE-TRAP-LBL X64CODE:LBL,  SNAP-RELOC:SITE-RC EXIT-GROUP, ;

\ ---- the rows ----------------------------------------------------------------
\ patch32 ( n n -- ): guard the four bytes at the address, then store the word
\ between two LPROTREC flips of its pages, as BPATCH32 does: the flip is keyed
\ on the target, never on a band another bracket may hold open. A generic
\ instruction write carries no optimizer proof, so the four bytes lose their
\ native evidence before the write can publish.
: PATCH32, ( -- )
   0 PATCH-BYTES SIZED-GUARD,
   RDI 0 PEEK,  RSI RDI PATCH-BYTES MEM-OFF ASM-SINK ENC-LEA
   X64PROV:INVALIDATE,
   R8 POP,  R9 POP,                                    \ the address, the word
   R8 PROT-RW PROT-REC,
   R9 R64>N >R32 R8 MEM-AT ASM-SINK ENC-MOV32-MR
   R8 PROT-RX PROT-REC, ;

\ dst = the first code slot at or past src.
: SLOT-UP, ( r64 r64 -- ) {: dst:r64 src:r64 :}
   dst src CODE-SLOT 1- MEM-OFF ASM-SINK ENC-LEA
   dst CODE-SLOT negate >IMM8 ASM-SINK ENC-AND-RI8 ;

\ code-publish's frame: the source, the destination, the length and the slot
\ past the span.
0 constant PUB-SRC
8 constant PUB-DST
16 constant PUB-LEN
24 constant PUB-SLOT
32 constant PUB-FRAME

\ code-publish ( ptr u8 n n -- ): the twin of BCODEPUBLISH. The span is nonzero,
\ does not wrap and lies in [DBASE+DICT-SIZE, DBASE+REGION), and dst is CP: a
\ publication is an append. The slot is the first CODE-SLOT multiple at or
\ past the span's end, so at most the region's end. The code band opens once
\ over [CP, slot), the bytes copy, int3 fills [dst+len, slot), the band
\ closes, the site rows of both kinds in [dst, slot) go, since the span's
\ sites are recorded after and the fill holds no code, and CP moves to the
\ slot. The origin of [dst, slot) is unknown: only
\ the compiler's successful return, X64PROV:CLOSE, with 1, may certify an
\ emission.
: PUBLISH, ( -- )
   ENGINE-GPR:X64-CP >R64 {: cp:r64 :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: copy:label copied:label fill:label filled:label :}
   RSI POP,  RDI POP,  RDX POP,                        \ len, dst, src
   RSI RSI ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,    \ a window of nothing
   RAX RDI RSI 1 0 MEM-IDX ASM-SINK ENC-LEA            \ rax = the span's end
   RAX RDI ASM-SINK ENC-CMP-RR  C-B SEAL-TRAP-LBL JCC,     \ an unsigned wrap
   RCX DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   RDI RCX ASM-SINK ENC-CMP-RR  C-B SEAL-TRAP-LBL JCC,     \ below the code interval
   RCX DBASE-REG REGION MEM-OFF ASM-SINK ENC-LEA
   RAX RCX ASM-SINK ENC-CMP-RR  C-A SEAL-TRAP-LBL JCC,     \ past the region
   RDI cp ASM-SINK ENC-CMP-RR  C-NE SEAL-TRAP-LBL JCC,     \ not an append at CP
   RSP PUB-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   RDX RSP PUB-SRC MOV-STORE,  RDI RSP PUB-DST MOV-STORE,  RSI RSP PUB-LEN MOV-STORE,
   RDI RAX SLOT-UP,  RDI RSP PUB-SLOT MOV-STORE,  LOPEN-LBL CALL,
   RDX RSP PUB-SRC MOV-LOAD,  RDI RSP PUB-DST MOV-LOAD,  RCX RSP PUB-LEN MOV-LOAD,
   RAX ZERO-REG,
   copy X64CODE:LBL,
   RAX RCX ASM-SINK ENC-CMP-RR  C-AE copied JCC,
   R8 RDX RAX 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   8 >R8 RDI RAX 1 0 MEM-IDX ASM-SINK ENC-MOV8-MR
   RAX ASM-SINK ENC-INC
   copy JMP,
   copied X64CODE:LBL,
   RCX RSP PUB-SLOT MOV-LOAD,  RCX RDI ASM-SINK ENC-SUB-RR  \ rcx = slot - dst
   R8 INT3 IMM32,
   fill X64CODE:LBL,                                           \ int3 to the slot
   RAX RCX ASM-SINK ENC-CMP-RR  C-AE filled JCC,
   8 >R8 RDI RAX 1 0 MEM-IDX ASM-SINK ENC-MOV8-MR
   RAX ASM-SINK ENC-INC
   fill JMP,
   filled X64CODE:LBL,
   LCLOSE-LBL CALL,
   RDI RSP PUB-DST MOV-LOAD,  RSI RSP PUB-SLOT MOV-LOAD,  RSI RDI ASM-SINK ENC-SUB-RR
   DROP-SITES-LBL CALL,
   RDI RSP PUB-DST MOV-LOAD,  cp RSP PUB-SLOT MOV-LOAD,     \ the slot is claimed
   RSI cp ASM-SINK ENC-MOV-RR  X64PROV:UNKNOWN-RANGE,
   RSP PUB-FRAME >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ callmap-set and addrmap-set ( n -- ): the site's region offset into rdi, where
\ an address outside the region exits SEAL-VIOLATION, and the kind into rdx.
: SITE-SET, ( n -- ) {: kind:n :}
   RDI POP,
   RDI DBASE-REG ASM-SINK ENC-SUB-RR                   \ below DBASE is huge, unsigned
   RDI REGION >IMM32 ASM-SINK ENC-CMP-RI32  C-AE SEAL-TRAP-LBL JCC,
   RDX kind IMM32,
   ADD-SITE-LBL CALL, ;

\ r8 = the address of the record whose index the register holds.
: RECORD-AT, ( r64 -- ) {: idx:r64 :}
   R8 idx DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   R8 DBASE-REG ASM-SINK ENC-ADD-RR ;

\ xref-retarget ( n n n -- ): the twin of BXREFRETARGET. The index names a live
\ record or the pending one, NDICT, and none past it; the length is at most
\ CODE-SPAN:RAW-MAX and not CODE-SPAN:FULL, an empty exact span. Between two
\ LPROTREC flips the length, the cell after the code cell, is stored first and
\ the address last: under total store order a plain store publishes after the
\ one before it, where ARM64 needs STLR.
: RETARGET, ( -- )
   RCX POP,  R9 POP,  R10 POP,                         \ the index, the length, the start
   RCX NDICT-REG ASM-SINK ENC-CMP-RR  C-A SEAL-TRAP-LBL JCC,
   RAX CODE-SPAN:RAW-MAX IMM32,
   R9 RAX ASM-SINK ENC-CMP-RR  C-A SEAL-TRAP-LBL JCC,
   RAX CODE-SPAN:FULL IMM32,
   R9 RAX ASM-SINK ENC-CMP-RR  C-E SEAL-TRAP-LBL JCC,
   RCX RECORD-AT,
   R8 PROT-RW PROT-REC,
   R9 R8 REC-CODE CELL + MOV-STORE,
   R10 R8 REC-CODE MOV-STORE,
   R8 PROT-RX PROT-REC, ;

\ int-mark ( n -- ): or DNAME-INT into record n's flags between two LPROTREC
\ flips, the twin of BINTMARK, which checks no index either.
: INT-MARK, ( -- )
   RCX POP,  RCX RECORD-AT,
   R8 PROT-RW PROT-REC,
   RAX DNAME-INT IMM64,
   RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   R8 PROT-RX PROT-REC, ;

\ min-in-mark ( n n -- ): or the minimum input depth into DNAME-MIN-IN of record
\ n's flags, the twin of BMININMARK: only the field's eight bits land.
: MIN-IN-MARK, ( -- )
   R9 POP,  RCX POP,  RCX RECORD-AT,
   R8 PROT-RW PROT-REC,
   R9 MIN-IN-SHIFT >IMM8 ASM-SINK ENC-SHL-RI8
   RAX DNAME-MIN-IN-MASK IMM64,
   R9 RAX ASM-SINK ENC-AND-RR
   R9 R8 REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   R8 PROT-RX PROT-REC, ;

\ Copy rcx bytes from rsi to rdi, stepping both past them. Clobbers rax.
: COPY-BYTES, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   next X64CODE:LBL,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RAX RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
   0 >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
   RSI ASM-SINK ENC-INC  RDI ASM-SINK ENC-INC  RCX ASM-SINK ENC-DEC
   next JMP,
   done X64CODE:LBL, ;

\ Store rcx zero bytes from rdi, stepping it past them. Clobbers rax.
: ZERO-BYTES, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   RAX ZERO-REG,
   next X64CODE:LBL,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   0 >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
   RDI ASM-SINK ENC-INC  RCX ASM-SINK ENC-DEC
   next JMP,
   done X64CODE:LBL, ;

\ does-record's frame.
0 constant DR-ENTRY                    \ the clause's entry
8 constant DR-LEN                      \ its recorded length
16 constant DR-NAME-LEN                \ its name's length
24 constant DR-NAME                    \ the parent's name bytes
32 constant DR-PAD                     \ the name's bytes at CP, CODE-SLOT whole
40 constant DR-REC                     \ the clause's record
48 constant DR-FRAME

\ Store the suffix's bytes at rdi and step past them.
: SUFFIX, ( -- )
   DOES-CLAUSE:SUFFIX$ {: a:ptr u:n :}
   u 0 ?do
      RAX a i + c@ IMM32,
      0 >R8 RDI i MEM-OFF ASM-SINK ENC-MOV8-MR
   loop
   RDI u >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ does-record ( n n -- ): the twin of DOES-REC:NATIVE-PRIM. The parent is the
\ record PEND-CELL points at, and its clause takes the slot past the pending
\ one, NDICT+1: the entry and length given, the parent's name and ";does"
\ out of line at CP, zero-padded to a CODE-SLOT multiple, and the parent's
\ wid. The code band opens over the padded name and the record band over the
\ record, and CP moves past the pad, to a slot, before the close.
\ src/compiler/native/publish.f DOES-NAME-PAD measures the name padded to a
\ four-byte word; CODE-RESERVE absorbs the rest.
: DOES-RECORD, ( -- )
   ENGINE-GPR:X64-CP >R64 {: cp:r64 :}
   RSP DR-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   RAX POP,  RAX RSP DR-LEN MOV-STORE,
   RAX POP,  RAX RSP DR-ENTRY MOV-STORE,
   R11 DATA-REG PEND-CELL MOV-LOAD,                    \ the parent's record
   R11 RCX RAX NAME-LEN,
   R11 RDX RAX NAME-AT,
   RCX DOES-CLAUSE:SUFFIX$ nip >IMM8 ASM-SINK ENC-ADD-RI8
   RCX RSP DR-NAME-LEN MOV-STORE,
   RDX RSP DR-NAME MOV-STORE,
   RAX RCX SLOT-UP,
   RAX RSP DR-PAD MOV-STORE,
   RDI cp RAX 1 0 MEM-IDX ASM-SINK ENC-LEA  LOPEN-LBL CALL,
   RSI RSP DR-NAME MOV-LOAD,
   RDI cp ASM-SINK ENC-MOV-RR
   RCX RSP DR-NAME-LEN MOV-LOAD,  RCX DOES-CLAUSE:SUFFIX$ nip >IMM8 ASM-SINK ENC-SUB-RI8
   COPY-BYTES,                                         \ the parent's name
   SUFFIX,
   RCX RSP DR-PAD MOV-LOAD,
   RCX RSP DR-NAME-LEN MEM-OFF ASM-SINK ENC-SUB-RM
   ZERO-BYTES,                                         \ the zeros to the pad
   RDI NDICT-REG 1 MEM-OFF ASM-SINK ENC-LEA
   RDI RECORD-AT,
   R8 RSP DR-REC MOV-STORE,
   RDI R8 ASM-SINK ENC-MOV-RR  RSI DREC IMM32,  LSPAN-LBL CALL,
   RDI RSP DR-REC MOV-LOAD,
   RAX RSP DR-ENTRY MOV-LOAD,  RAX RDI REC-CODE MOV-STORE,
   RAX RSP DR-LEN MOV-LOAD,  RAX RDI REC-CODE CELL + MOV-STORE,
   RAX DNAME-EXT IMM64,
   RAX RSP DR-NAME-LEN MEM-OFF ASM-SINK ENC-OR-RM
   RAX RDI REC-FLAGS MOV-STORE,
   cp RDI REC-NAME MOV-STORE,
   RAX ZERO-REG,  RAX RDI REC-NAME CELL + MOV-STORE,
   RAX DATA-REG PEND-CELL MOV-LOAD,
   RAX RAX REC-WID MOV-LOAD,  RAX RDI REC-WID MOV-STORE,
   cp RSP DR-PAD MEM-OFF ASM-SINK ENC-ADD-RM
   LCLOSE-LBL CALL,
   RSP DR-FRAME >IMM8 ASM-SINK ENC-ADD-RI8 ;

public

\ The window's call sites, the twins of a habu1.f PROT-EMIT:LSPAN, PROT-EMIT:LOPEN and
\ PROT-EMIT:LCLOSE call: declare the span at rdi, rsi bytes long; open the code band
\ over [CP, rdi), or CP's byte for an end at or below CP; close every band.
\ Each keeps every VM register and clobbers rax rcx rdx rsi rdi and r8-r11. A
\ section that calls one follows PUBLICATION, in KERNEL,.
: WINDOW-SPAN, ( -- ) LSPAN-LBL CALL, ;
: WINDOW-OPEN, ( -- ) LOPEN-LBL CALL, ;
: WINDOW-CLOSE, ( -- ) LCLOSE-LBL CALL, ;

\ The window and site helpers, made in this stream, and then the rows.
\ native-unit-publish refuses until its body lands.
: PUBLICATION, ( -- )
   X64CODE:LBL LSPAN-CELL !  X64CODE:LBL LOPEN-CELL !
   X64CODE:LBL ADD-SITE-CELL !  X64CODE:LBL DROP-SITES-CELL !
   X64CODE:LBL SEAL-TRAP-CELL !  X64CODE:LBL SITE-TRAP-CELL !
   LSPAN-HELPER,  LOPEN-HELPER,  LCLOSE-HELPER,
   ADD-SITE-HELPER,  DROP-SITES-HELPER,  TRAPS,
   s" patch32" [: PATCH32, ;] PRIM
   s" code-publish" [: PUBLISH, ;] PRIM
   s" native-unit-publish" ENGINE-PRIMS:GLOBAL-INT-WID REFUSE-WID
   s" callmap-set" [: SNAP-RELOC:SITE-CALL SITE-SET, ;] PRIM
   s" addrmap-set" [: SNAP-RELOC:SITE-ADDR SITE-SET, ;] PRIM
   s" reloc-maps-clear" [: RSI POP,  RDI POP,  DROP-SITES-LBL CALL, ;] PRIM
   s" xref-retarget" [: RETARGET, ;] PRIM
   s" int-mark" [: INT-MARK, ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" min-in-mark" [: MIN-IN-MARK, ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" does-record" [: DOES-RECORD, ;] PRIM ;

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
   X64CODE:LBL X64CODE:LBL {: none:label push:label :}
   FIND-ARGS,
   RDX OWNER-API-PRI-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E none JCC,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E push JCC,
   RCX DNAME-INT IMM64,
   RCX RAX REC-FLAGS MEM-OFF ASM-SINK ENC-TEST-MR  C-NE none JCC,
   RAX RAX REC-CODE MEM-OFF ASM-SINK ENC-MOV-RM
   push JMP,
   none X64CODE:LBL,
   RAX ZERO-REG,
   push X64CODE:LBL,
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
   X64CODE:LBL X64CODE:LBL {: loop:label done:label :}
   RAX S0-CELL CELL@,  RAX SSCR-CELL CELL!,
   loop X64CODE:LBL,
   RAX SSCR-CELL CELL@,  RAX DSP ASM-SINK ENC-CMP-RR  C-AE done JCC,
   RAX RAX MEM-AT ASM-SINK ENC-MOV-RM  G-PRINT9
   RAX SSCR-CELL CELL@,  RAX CELL >IMM8 ASM-SINK ENC-ADD-RI8
   RAX SSCR-CELL CELL!,
   loop JMP,
   done X64CODE:LBL, ;

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

\ Branch to the label unless rax is a live JIT entry, DBASE <= rax < CP,
\ unsigned: the install window of habu1.f BSETCHECK. It catches a wild
\ install, not a well-formed pointer into live code.
: WINDOW, ( label -- ) {: bad:label :}
   RAX DBASE-REG ASM-SINK ENC-CMP-RR  C-B bad JCC,
   RAX CP-REG ASM-SINK ENC-CMP-RR  C-AE bad JCC, ;

\ set-check ( xt -- ): 0 turns checking off and empties the preflight hook
\ too; any other xt must lie in the window.
: SET-CHECK-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bad:label ok:label done:label :}
   RAX POP,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   bad WINDOW,
   ok X64CODE:LBL,
   RAX HOOK-CELL CELL!,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RAX COMPILE-PREFLIGHT-CELL CELL!,
   done JMP,
   bad X64CODE:LBL,  S\" set-check: invalid checker xt\n" HOOK-BAD-RC STDERR-EXIT,
   done X64CODE:LBL, ;

\ set-preflight ( xt -- ): installs once. With the cell set, the same xt is
\ inert and any other refused; with it empty, the xt must lie in the window.
: SET-PREFLIGHT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: invalid:label empty:label done:label :}
   RAX POP,
   RCX COMPILE-PREFLIGHT-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E empty JCC,
   RCX RAX ASM-SINK ENC-CMP-RR  C-E done JCC,
   S\" set-preflight: invalid or replaced hook\n" HOOK-BAD-RC STDERR-EXIT,
   empty X64CODE:LBL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E invalid JCC,
   invalid WINDOW,
   RAX COMPILE-PREFLIGHT-CELL CELL!,
   done JMP,
   invalid X64CODE:LBL,  S\" set-preflight: invalid hook\n" HOOK-BAD-RC STDERR-EXIT,
   done X64CODE:LBL, ;

\ set-top-check ( xt -- ): 0 uninstalls; any other xt must lie in the window.
: SET-TOP-CHECK-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bad:label ok:label done:label :}
   RAX POP,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   bad WINDOW,
   ok X64CODE:LBL,
   RAX TOP-HOOK-CELL CELL!,
   done JMP,
   bad X64CODE:LBL,  S\" set-top-check: invalid top-row hook xt\n" HOOK-BAD-RC STDERR-EXIT,
   done X64CODE:LBL, ;

\ The checker's hooks live in sealed DATA cells, so a direct store from the
\ row is their only writer once the engine is sealed.
: HOOKS, ( -- )
   s" set-check" [: SET-CHECK-BODY ;] PRIM
   s" check@" [: RAX HOOK-CELL CELL@,  RAX PUSH, ;] PRIM
   s" set-preflight" [: SET-PREFLIGHT-BODY ;] PRIM
   s" set-top-check" [: SET-TOP-CHECK-BODY ;] PRIM
   s" top-check@" [: RAX TOP-HOOK-CELL CELL@,  RAX PUSH, ;] PRIM ;

\ ---- the registers and the dictionary count ----------------------------------
\ The twins of habu1.f BCPFETCH .. BDATAFETCH, BRBASE, BCPSET, BNDSET,
\ BSEEDNDICTSET and BNDAPPEND. A row that moves r14 keeps the dictionary index
\ authoritative through the HIDX calls.

74 constant COUNT-RC                   \ BNDSET's and BSEEDNDICTSET's refusal

\ The twin of habu1.f GUARD-CODE-WORD, over code slots where ARM64 takes
\ instruction words: a new CP must be a CODE-SLOT multiple in
\ [DBASE + DICT-SIZE, DBASE + REGION - CODE-SLOT], unsigned, or the row exits
\ SEAL-VIOLATION, so cp! never aims later emission outside the code area or
\ at an address X64EMIT:PLACE-AT refuses. It clobbers rax.
: CODE-SLOT-GUARD, ( r64 -- ) {: at:r64 :}
   X64CODE:LBL X64CODE:LBL {: ok:label trap:label :}
   RAX DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   at RAX ASM-SINK ENC-CMP-RR  C-B trap JCC,
   RAX DBASE-REG REGION CODE-SLOT - MEM-OFF ASM-SINK ENC-LEA
   at RAX ASM-SINK ENC-CMP-RR  C-A trap JCC,
   at CODE-SLOT 1- >IMM32 ASM-SINK ENC-TEST-RI32  C-E ok JCC,
   trap X64CODE:LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   ok X64CODE:LBL, ;

\ Guard the DREC bytes of record n, n the top of the data stack, which stays:
\ the record a new count points the next write at, as habu1.f BNDSET guards
\ it. It clobbers what PROT-SPAN-CALL, clobbers.
: RECORD-GUARD, ( -- )
   RAX 0 PEEK,
   RDI RAX DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RDI DBASE-REG ASM-SINK ENC-ADD-RR
   RSI DREC IMM32,
   RDI RSI PROT-SPAN-CALL, ;

\ Exit SEAL-VIOLATION when rax, a count, lies below the floor SEAL-CAPTURE
\ recorded; 0 is no floor. It clobbers rcx.
: FLOOR-GUARD, ( -- )
   X64CODE:LBL {: ok:label :}
   RCX SEAL-NDICT-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E ok JCC,
   RAX RCX ASM-SINK ENC-CMP-RR  C-AE ok JCC,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   ok X64CODE:LBL, ;

\ Once a count lowers, a create record at or beyond its new end is retired.
\ The next definition may reuse that address, so does-patch must forget it.
: LASTC-TRIM, ( -- )
   X64CODE:LBL {: live:label :}
   RDX NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RDX DBASE-REG ASM-SINK ENC-ADD-RR
   RCX LASTC-CELL CELL@,
   RCX RDX ASM-SINK ENC-CMP-RR  C-B live JCC,
   RCX ZERO-REG,  RCX LASTC-CELL CELL!,
   live X64CODE:LBL, ;

\ ndict! ( n -- ): a count past DICT-CAP, unsigned, writes `hb: dictionary
\ count out of range` and exits COUNT-RC; one below the seal floor exits
\ SEAL-VIOLATION. A lowered count keeps the index, whose probe skips a record
\ at or past the count; a raised one re-exposes records whose slots a later
\ insert may have reused, so the index is rebuilt over the raised count.
: NDICT-SET-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bounded:label lower:label done:label :}
   TASK-LIVE-GUARD,
   RAX 0 PEEK,
   RAX DICT-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-BE bounded JCC,
   S\" hb: dictionary count out of range\n" COUNT-RC STDERR-EXIT,
   bounded X64CODE:LBL,
   RECORD-GUARD,
   RAX POP,
   FLOOR-GUARD,
   RAX NDICT-REG ASM-SINK ENC-CMP-RR
   NDICT-REG RAX ASM-SINK ENC-MOV-RR                   \ mov keeps the flags
   C-L lower JCC,
   C-E done JCC,
   HIDX-REBUILD,
   done JMP,
   lower X64CODE:LBL,
   LASTC-TRIM,
   done X64CODE:LBL, ;

\ seed-ndict! ( n -- ): lower the count below the live one, rebuild the index
\ and clear the seal floor, which the native builder's trusted reset opens.
\ A negative count or one that does not lower exits COUNT-RC.
: SEED-NDICT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: lower:label bad:label :}
   TASK-LIVE-GUARD,
   RAX 0 PEEK,
   RAX RAX ASM-SINK ENC-TEST-RR  C-L bad JCC,
   RAX NDICT-REG ASM-SINK ENC-CMP-RR  C-L lower JCC,
   bad X64CODE:LBL,
   COUNT-RC EXIT-GROUP,
   lower X64CODE:LBL,
   RECORD-GUARD,
   NDICT-REG POP,
   LASTC-TRIM,
   HIDX-REBUILD,
   RAX ZERO-REG,  RAX SEAL-NDICT-CELL CELL!, ;

\ ndict-append ( n -- ): count the native tier's pending record, which must
\ be record n at the count, or its does> companion one record past it while
\ a does> body is pending, and index it. Anything else, or a count below the
\ seal floor, exits SEAL-VIOLATION.
: NDICT-APPEND-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: owner:label bad:label done:label :}
   TASK-LIVE-GUARD,
   RAX 0 PEEK,
   RAX NDICT-REG ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RAX DICT-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-AE bad JCC,
   RCX NCOMP-DISPATCH:DEF-TIER-CELL CELL@,
   RCX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RDX RAX DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   RDX DBASE-REG ASM-SINK ENC-ADD-RR                   \ rdx = record n
   RCX PEND-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E bad JCC,
   RCX RDX ASM-SINK ENC-CMP-RR  C-E owner JCC,
   RCX DREC >IMM8 ASM-SINK ENC-ADD-RI8
   RCX RDX ASM-SINK ENC-CMP-RR  C-NE bad JCC,
   RCX DOESB-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-LE bad JCC,
   owner X64CODE:LBL,
   FLOOR-GUARD,
   RECORD-GUARD,
   DROP,
   NDICT-REG ASM-SINK ENC-INC
   HIDX-ADD,
   done JMP,
   bad X64CODE:LBL,
   ENGINE-ERROR:SEAL-VIOLATION EXIT-GROUP,
   done X64CODE:LBL, ;

: DICT-COUNT, ( -- )
   s" cp@" [: CP-REG PUSH, ;] PRIM
   s" dbase@" [: DBASE-REG PUSH, ;] PRIM
   s" data-base" [: DATA-REG PUSH, ;] PRIM
   s" rbase" [: RAX RBASE-CELL CELL@,  RAX PUSH, ;] PRIM
   s" ndict@" [: NDICT-REG PUSH, ;] PRIM
   s" cp!" [:
      TASK-LIVE-GUARD,  RCX POP,  RCX CODE-SLOT-GUARD,
      CP-REG RCX ASM-SINK ENC-MOV-RR ;] PRIM
   s" ndict!" [: NDICT-SET-BODY ;] PRIM
   s" seed-ndict!" [: SEED-NDICT-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" ndict-append" [: NDICT-APPEND-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID ;

\ ---- the seal ----------------------------------------------------------------
\ The twins of habu1.f BSEALCAP, BSEALCAPQ and BSEALFRIEND and habu2.f
\ BDRAINPRETRUST. The pending pre-trust defer table at PD-TABLE-OFF is a count
\ and PD-SLOT byte slots, each a name and a signature with their lengths.

73 constant UNDRAINED-RC               \ habu1.f BSEALCAP's
70 constant REGISTRAR-RC               \ habu2.f DEF-TRUST:FIND's

: PD-COUNT ( -- mem ) DATA-REG PD-TABLE-OFF MEM-OFF ;

\ r8 = the base of the pending slot whose index rcx holds.
: SLOT-BASE, ( -- )
   R8 RCX PD-SLOT >IMM32 ASM-SINK ENC-IMUL-RRI32
   R8 DATA-REG R8 1 PD-TABLE-OFF PD-SLOTS-REL + MEM-IDX ASM-SINK ENC-LEA ;

: UNDRAINED$ ( -- ptr u8 n ) s" hb: undrained pre-trust defer: " ;

\ SEAL-CAPTURE ( -- ): record the count as the seal floor. The pending table
\ must be empty by then, since checker.f drains it after `: TRUST`; a pending
\ defer means the drain never ran, so each is named on fd 2, one per line, and
\ the row exits UNDRAINED-RC. The walk lives in r8-r10, which a syscall keeps.
: SEAL-CAPTURE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label stop:label drained:label :}
   X64CODE:LBL X64CODE:LBL {: head:label nl:label :}
   R9 PD-COUNT ASM-SINK ENC-MOV-RM                     \ r9 = the count
   R9 R9 ASM-SINK ENC-TEST-RR  C-E drained JCC,
   R10 ZERO-REG,                                       \ r10 = the slot
   next X64CODE:LBL,
   R10 R9 ASM-SINK ENC-CMP-RR  C-GE stop JCC,
   RCX R10 ASM-SINK ENC-MOV-RR  SLOT-BASE,
   head UNDRAINED$ nip STDERR-WRITE,
   RDI STDERR IMM32,
   RSI R8 PD-NAME-OFF MEM-OFF ASM-SINK ENC-LEA
   RDX R8 PD-NLEN-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   NR-WRITE SYS,
   nl 1 STDERR-WRITE,
   R10 ASM-SINK ENC-INC
   next JMP,
   stop X64CODE:LBL,
   UNDRAINED-RC EXIT-GROUP,
   head X64CODE:LBL,  UNDRAINED$ TEXT,
   nl X64CODE:LBL,  STR-LF ASM-SINK BUF:APPEND-BYTE
   drained X64CODE:LBL,
   NDICT-REG SEAL-NDICT-CELL CELL!, ;

\ rax = the target checker's operation at offset n of its record, the twin of
\ habu2.f DECL-OWNER:TARGET: with no record or no operation it branches to the
\ label.
: DECL-TARGET, ( n label -- ) {: off:n absent:label :}
   RAX NCOMP-DISPATCH:TARGET-DECL-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E absent JCC,
   RAX RAX off MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E absent JCC, ;

\ r8 = the top pending slot's base. The count is read again from DATA each
\ time, since a checker call keeps no scratch register. It clobbers rcx.
: TOP-SLOT, ( -- )
   RCX PD-COUNT ASM-SINK ENC-MOV-RM
   RCX ASM-SINK ENC-DEC
   SLOT-BASE, ;

\ Push a string of the slot at r8: the address of its bytes, at the first
\ offset, and its length, the cell at the second.
: PUSH-SLOT, ( n n -- ) {: at:n len:n :}
   RCX R8 at MEM-OFF ASM-SINK ENC-LEA  RCX PUSH,
   RCX R8 len MEM-OFF ASM-SINK ENC-MOV-RM  RCX PUSH, ;

\ DRAIN-PRETRUST ( -- ): replay the table from the top: the target checker's
\ trust-decl with the slot's name and signature, then its checker-defer with
\ the name, then drop the slot. No trust-decl exits REGISTRAR-RC naming it on
\ fd 2; no checker-defer ends the drain with the slot still pending, as the
\ ARM64 twin does.
: DRAIN-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label absent:label done:label :}
   next X64CODE:LBL,
   RCX PD-COUNT ASM-SINK ENC-MOV-RM
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NCOMP-DISPATCH:DECL-EFFECT-OFF absent DECL-TARGET,
   TOP-SLOT,
   PD-NAME-OFF PD-NLEN-OFF PUSH-SLOT,
   PD-SIG-OFF PD-SLEN-OFF PUSH-SLOT,
   RAX ASM-SINK ENC-CALL-REG
   NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-TARGET,
   TOP-SLOT,
   PD-NAME-OFF PD-NLEN-OFF PUSH-SLOT,
   RAX ASM-SINK ENC-CALL-REG
   RCX PD-COUNT ASM-SINK ENC-MOV-RM
   RCX ASM-SINK ENC-DEC
   RCX PD-COUNT ASM-SINK ENC-MOV-MR
   next JMP,
   absent X64CODE:LBL,
   S\" trust-decl\n" REGISTRAR-RC STDERR-EXIT,
   done X64CODE:LBL, ;

: SEAL-ROWS, ( -- )
   s" SEAL-CAPTURE" [: SEAL-CAPTURE-BODY ;] PRIM
   \ -1 when the floor is set: neg sets CF for a nonzero rax, sbb spreads it.
   s" seal-captured?" [:
      RAX SEAL-NDICT-CELL CELL@,
      RAX ASM-SINK ENC-NEG  RAX RAX ASM-SINK ENC-SBB-RR  RAX PUSH, ;] PRIM
   s" SEAL-FRIEND" [: RAX FRIEND-ARENA-LEN IMM32,  RAX FRIEND-LATCH-CELL CELL!, ;] PRIM
   s" DRAIN-PRETRUST" [: DRAIN-BODY ;] PRIM ;

\ ---- wordlists and record marks ----------------------------------------------
\ The twins of habu1.f BWORDLIST, BGETCUR, BSETCUR, BPROTWIDADD, BPROTWIDROOM
\ and BWIDEMARK.

\ The twin of habu1.f PROT-BITS-ADDR, over the wid in rdi, which must lie
\ below PROT-WID-MAX: rsi = the protected-WID bitmap's cell holding its bit,
\ rdx = the bit. It clobbers rcx; shl takes its count mod 64, the wid's low
\ six bits.
: PROT-BITS, ( -- )
   RSI RDI ASM-SINK ENC-MOV-RR
   RSI 6 >IMM8 ASM-SINK ENC-SHR-RI8
   RSI DATA-REG RSI CELL PROT-BITS-OFF MEM-IDX ASM-SINK ENC-LEA
   RCX RDI ASM-SINK ENC-MOV-RR
   RDX 1 IMM32,  RDX ASM-SINK ENC-SHL-CL ;

\ prot-wid-add ( n -- ): protect the wid, once. The two engine-reserved wids
\ are protected already, as habu1.f LPROTWIDQ pins them, and a set bit is
\ left alone; a wid at or above PROT-WID-MAX has no bit, so it writes `hb:
\ protected-WID id above the bound` and exits SEAL-PACKAGE rather than write
\ past the band. The set bit is published by a plain store, which x86-64 total
\ store order releases as habu1.f's STLR does.
: PROT-WID-ADD-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: bounded:label done:label :}
   RDI POP,
   RDI OWNER-API-PUB-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E done JCC,
   RDI OWNER-API-PRI-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E done JCC,
   RDI PROT-WID-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-B bounded JCC,
   S\" hb: protected-WID id above the bound\n" ENGINE-ERROR:SEAL-PACKAGE STDERR-EXIT,
   bounded X64CODE:LBL,
   PROT-BITS,
   RAX RSI MEM-AT ASM-SINK ENC-MOV-RM
   RAX RDX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RAX RDX ASM-SINK ENC-OR-RR
   RAX RSI MEM-AT ASM-SINK ENC-MOV-MR
   done X64CODE:LBL, ;

\ prot-wid-room ( -- n ): PROT-WID-MAX less WIDN-CELL, or 0 once WIDN-CELL
\ reaches the bound.
: PROT-WID-ROOM-BODY ( -- )
   RCX ZERO-REG,
   RAX PROT-WID-MAX IMM32,
   RAX DATA-REG WIDN-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   RAX RAX ASM-SINK ENC-TEST-RR
   C-L RAX RCX ASM-SINK ENC-CMOVCC
   RAX PUSH, ;

\ Or bits into the newest record's flags between writable page flips.
: NEWEST-MARK, ( n -- ) {: bits:n :}
   R8 NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   R8 DBASE-REG R8 1 DREC negate MEM-IDX ASM-SINK ENC-LEA
   R8 PROT-RW PROT-REC,
   RAX bits IMM64,
   RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   R8 PROT-RX PROT-REC, ;

: WIDE-MARK-BODY ( -- ) DNAME-WIDE NEWEST-MARK, ;

\ policy-admit and policy-seal refuse: the design seal is read by habu2.f's
\ token loops (LPOLICYREC, LKWCMP), which this kernel does not have, so a seal
\ stored here would confine nothing.
: WORDLIST-ROWS, ( -- )
   s" wordlist" [:
      RAX WIDN-CELL CELL@,  RAX PUSH,
      RAX ASM-SINK ENC-INC  RAX WIDN-CELL CELL!, ;] PRIM
   s" get-current" [: RAX CUR-CELL CELL@,  RAX PUSH, ;] PRIM
   s" set-current" [: RAX POP,  RAX CUR-CELL CELL!, ;] PRIM
   s" prot-wid-add" [: PROT-WID-ADD-BODY ;] PRIM
   s" prot-wid-room" [: PROT-WID-ROOM-BODY ;] PRIM
   s" policy-admit" REFUSE
   s" policy-seal" REFUSE
   s" wide-mark" [: WIDE-MARK-BODY ;] PRIM ;

\ ---- persisted cells, the tier and the build scope ---------------------------
\ The x86-64 image has fixed addresses, but capture still needs declarations
\ for cells holding code and DATA addresses. snap-rebase, which moves a
\ restored snapshot's cells, refuses. The twins of habu2.f
\ RELOC-EMIT:BXTSTORE, BPTRCELLMARK, BVERSION and BSNAPSHOTFORMAT and habu1.f
\ BBUILDENTER, BBUILDLEAVE, BSETTIER, BTIERFETCH and BCODEORIGIN.

: TIER-OFF ( -- n ) NCOMP-DISPATCH:TIER-CELL ;
: DEPTH-OFF ( -- n ) NCOMP-DISPATCH:BUILD-DEPTH-CELL ;
: SAVED-OFF ( -- n ) NCOMP-DISPATCH:BUILD-TIER-CELL ;

\ executable-build-enter ( -- ): open a build scope. The outermost saves the
\ caller's tier; every scope selects tier 1.
: BUILD-ENTER-BODY ( -- )
   X64CODE:LBL {: nested:label :}
   RAX DEPTH-OFF CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE nested JCC,
   RCX TIER-OFF CELL@,  RCX SAVED-OFF CELL!,
   nested X64CODE:LBL,
   RAX ASM-SINK ENC-INC  RAX DEPTH-OFF CELL!,
   RAX 1 IMM32,  RAX TIER-OFF CELL!, ;

\ executable-build-leave ( -- ): close a scope. The outermost restores the
\ saved tier and clears the copy; with no scope open it does nothing.
: BUILD-LEAVE-BODY ( -- )
   X64CODE:LBL {: done:label :}
   RAX DEPTH-OFF CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RAX ASM-SINK ENC-DEC
   RAX DEPTH-OFF CELL!,                               \ mov keeps dec's flags
   C-NE done JCC,
   RCX SAVED-OFF CELL@,  RCX TIER-OFF CELL!,
   RAX SAVED-OFF CELL!,
   done X64CODE:LBL, ;

\ set-tier ( n -- ): x86-64 has no tier-0 compiler, so 1 is the one tier it
\ selects; any other writes `set-tier: x86-64 runs tier 1 only` and exits
\ HOOK-BAD-RC, as habu1.f BSETTIER refuses a tier past 1.
: SET-TIER-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: bad:label done:label :}
   RAX POP,
   RAX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE bad JCC,
   RAX TIER-OFF CELL!,
   done JMP,
   bad X64CODE:LBL,
   S\" set-tier: x86-64 runs tier 1 only\n" HOOK-BAD-RC STDERR-EXIT,
   done X64CODE:LBL, ;

\ (MARK) takes rdi = the cell and rsi = its kind tag. Its declaration happens
\ before xt! writes, so a refused cell cannot leave an untracked address.
\ The row vector is authoritative; a linear search keeps the original ordinal
\ and needs no process-local index. The fixed DATA lock protects lookup,
\ growth, and publication across tasks.
variable MARK-CELL
: MARK-LBL ( -- label ) MARK-CELL @ >LABEL ;
: MARK-BODY, ( -- )
   X64CODE:LBL {: start:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: finish:label lock:label shape:label mapped:label scan:label found:label
      append:label grow:label copied:label copy:label publish:label release:label
      full:label band:label kind:label next:label discard:label :}
   start LABEL>N MARK-CELL !
   s" (MARK)" start LABEL>N finish LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start X64CODE:LBL,
   RSP 72 >IMM8 ASM-SINK ENC-SUB-RI8
   RAX SHARED-DATA,  RDI RAX ASM-SINK ENC-SUB-RR
   RAX X64LAYOUT:DATA-SIZE CELL - IMM64,
   RDI RAX ASM-SINK ENC-CMP-RR  C-A band JCC,
   RDI RSP 0 MEM-OFF ASM-SINK ENC-MOV-MR
   RSI RSP 8 MEM-OFF ASM-SINK ENC-MOV-MR
   lock X64CODE:LBL,
      RAX 1 IMM32,
      R11 SHARED-DATA,
      RAX R11 ADDRESS-CELLS:LOCK-CELL MEM-OFF ASM-SINK ENC-XCHG-MR
      RAX RAX ASM-SINK ENC-TEST-RR  C-NE lock JCC,
   R11 SHARED-DATA,
   R11 R11 SNAP-RELOC:XTCELL-N-CELL MEM-OFF ASM-SINK ENC-LEA
   RAX R11 ADDRESS-CELLS:MAGIC-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   RCX ADDRESS-CELLS:MAGIC IMM64,
   RAX RCX ASM-SINK ENC-CMP-RR  C-NE shape JCC,
   R8 R11 MEM-AT ASM-SINK ENC-MOV-RM
   R9 R11 ADDRESS-CELLS:CAP-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   R9 R9 ASM-SINK ENC-TEST-RR  C-LE shape JCC,
   RAX ADDRESS-CELLS:MAX-ROWS IMM64,
   R9 RAX ASM-SINK ENC-CMP-RR  C-A shape JCC,
   R8 R9 ASM-SINK ENC-CMP-RR  C-A shape JCC,
   RDX R11 ADDRESS-CELLS:BASE-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   R10 R11 ADDRESS-CELLS:MODE-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   R10 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-E mapped JCC,
   R10 R10 ASM-SINK ENC-TEST-RR  C-NE shape JCC,
   RAX X64LAYOUT:DATA-SIZE IMM64,
   RDX RAX ASM-SINK ENC-CMP-RR  C-A shape JCC,
   RAX RDX ASM-SINK ENC-SUB-RR
   RAX 3 >IMM8 ASM-SINK ENC-SHR-RI8
   R9 RAX ASM-SINK ENC-CMP-RR  C-A shape JCC,
   RAX SHARED-DATA,  RDX RAX ASM-SINK ENC-ADD-RR
   scan JMP,
   mapped X64CODE:LBL,
   RDX RDX ASM-SINK ENC-TEST-RR  C-LE shape JCC,
   RAX RDX ASM-SINK ENC-MOV-RR
   RAX 7 >IMM8 ASM-SINK ENC-AND-RI8  C-NE shape JCC,
   RAX $7FFFFFFFFFFFFFFF IMM64,
   RAX RDX ASM-SINK ENC-SUB-RR
   RAX 3 >IMM8 ASM-SINK ENC-SHR-RI8
   R9 RAX ASM-SINK ENC-CMP-RR  C-A shape JCC,
   scan X64CODE:LBL,
   R8 RSP 16 MEM-OFF ASM-SINK ENC-MOV-MR
   R9 RSP 24 MEM-OFF ASM-SINK ENC-MOV-MR
   RDX RSP 32 MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ZERO-REG,
   found X64CODE:LBL,
      RCX R8 ASM-SINK ENC-CMP-RR  C-AE append JCC,
      RAX RDX RCX 8 0 MEM-IDX ASM-SINK ENC-MOV-RM
      R10 RAX ASM-SINK ENC-MOV-RR
      R10 1 >IMM8 ASM-SINK ENC-SHL-RI8
      R10 1 >IMM8 ASM-SINK ENC-SHR-RI8
      RDI RSP 0 MEM-OFF ASM-SINK ENC-MOV-RM
      R10 RDI ASM-SINK ENC-CMP-RR  C-NE next JCC,
      RSI RSP 8 MEM-OFF ASM-SINK ENC-MOV-RM
      RDI RSI ASM-SINK ENC-OR-RR
      RAX RDI ASM-SINK ENC-CMP-RR  C-NE kind JCC,
      release JMP,
   next X64CODE:LBL,
      RCX ASM-SINK ENC-INC  found JMP,
   append X64CODE:LBL,
   R8 R9 ASM-SINK ENC-CMP-RR  C-E grow JCC,
   RDI RSP 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RSI RSP 8 MEM-OFF ASM-SINK ENC-MOV-RM
   RDI RSI ASM-SINK ENC-OR-RR
   RDI RDX R8 8 0 MEM-IDX ASM-SINK ENC-MOV-MR
   R8 ASM-SINK ENC-INC
   R11 SHARED-DATA,
   R8 R11 SNAP-RELOC:XTCELL-N-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   release JMP,
   grow X64CODE:LBL,
   RAX ADDRESS-CELLS:MAX-ROWS 2 / IMM64,
   R9 RAX ASM-SINK ENC-CMP-RR  C-A full JCC,
   R9 1 >IMM8 ASM-SINK ENC-SHL-RI8
   R9 RSP 40 MEM-OFF ASM-SINK ENC-MOV-MR
   RDI ZERO-REG,  RSI R9 ASM-SINK ENC-MOV-RR
   RSI 3 >IMM8 ASM-SINK ENC-SHL-RI8
   RDX PROT-RW IMM32,  R10 MAP-ANON-PRIVATE IMM32,
   R8 -1 >IMM32 ASM-SINK ENC-MOV-RI32  R9 ZERO-REG,
   NR-MMAP SYS,  C-B full JCC,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E full JCC,
   RAX RSP 48 MEM-OFF ASM-SINK ENC-MOV-MR
   RCX ZERO-REG,
   copy X64CODE:LBL,
      R8 RSP 16 MEM-OFF ASM-SINK ENC-MOV-RM
      RCX R8 ASM-SINK ENC-CMP-RR  C-AE copied JCC,
      RDX RSP 32 MEM-OFF ASM-SINK ENC-MOV-RM
      RAX RDX RCX 8 0 MEM-IDX ASM-SINK ENC-MOV-RM
      RDI RSP 48 MEM-OFF ASM-SINK ENC-MOV-RM
      RAX RDI RCX 8 0 MEM-IDX ASM-SINK ENC-MOV-MR
      RCX ASM-SINK ENC-INC  copy JMP,
   copied X64CODE:LBL,
   R11 SHARED-DATA,
   R11 R11 SNAP-RELOC:XTCELL-N-CELL MEM-OFF ASM-SINK ENC-LEA
   RAX R11 ADDRESS-CELLS:MODE-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E publish JCC,
   RDI R11 ADDRESS-CELLS:BASE-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   RSI R11 ADDRESS-CELLS:CAP-FIELD MEM-OFF ASM-SINK ENC-MOV-RM
   RSI 3 >IMM8 ASM-SINK ENC-SHL-RI8
   NR-MUNMAP SYS,  C-B discard JCC,
   publish X64CODE:LBL,
   R11 SHARED-DATA,
   R11 R11 SNAP-RELOC:XTCELL-N-CELL MEM-OFF ASM-SINK ENC-LEA
   RDI RSP 48 MEM-OFF ASM-SINK ENC-MOV-RM
   R8 RSP 16 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSP 0 MEM-OFF ASM-SINK ENC-MOV-RM
   RSI RSP 8 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSI ASM-SINK ENC-OR-RR
   RAX RDI R8 8 0 MEM-IDX ASM-SINK ENC-MOV-MR
   RDI R11 ADDRESS-CELLS:BASE-FIELD MEM-OFF ASM-SINK ENC-MOV-MR
   RAX RSP 40 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX R11 ADDRESS-CELLS:CAP-FIELD MEM-OFF ASM-SINK ENC-MOV-MR
   RAX 1 IMM32,
   RAX R11 ADDRESS-CELLS:MODE-FIELD MEM-OFF ASM-SINK ENC-MOV-MR
   R8 ASM-SINK ENC-INC
   R8 R11 MEM-AT ASM-SINK ENC-MOV-MR
   release X64CODE:LBL,
   RAX ZERO-REG,
   R11 SHARED-DATA,
   RAX R11 ADDRESS-CELLS:LOCK-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RSP 72 >IMM8 ASM-SINK ENC-ADD-RI8
   ASM-SINK ENC-RET
   discard X64CODE:LBL,
   RDI RSP 48 MEM-OFF ASM-SINK ENC-MOV-RM
   RSI RSP 40 MEM-OFF ASM-SINK ENC-MOV-RM
   RSI 3 >IMM8 ASM-SINK ENC-SHL-RI8
   NR-MUNMAP SYS,  full JMP,
   full X64CODE:LBL,
   S\" hb: address-cell storage allocation failed\n" SNAP-RELOC:XTCELL-RC STDERR-EXIT,
   shape X64CODE:LBL,
   S\" hb: invalid address-cell storage header\n" SNAP-RELOC:XTCELL-RC STDERR-EXIT,
   band X64CODE:LBL,
   S\" hb: snapshot address cell out of range\n" SNAP-RELOC:XTBAND-RC STDERR-EXIT,
   kind X64CODE:LBL,
   S\" hb: snapshot address cell kind mismatch\n" SNAP-RELOC:XTKIND-RC STDERR-EXIT,
   finish X64CODE:LBL, ;

: SCOPE-ROWS, ( -- )
   MARK-BODY,
   s" xt!" [:
      0 CELL SIZED-GUARD,  RCX POP,  RAX POP,
      RAX ASM-SINK ENC-PUSH  RCX ASM-SINK ENC-PUSH
      RDI RCX ASM-SINK ENC-MOV-RR  RSI ZERO-REG,  MARK-LBL CALL,
      RCX ASM-SINK ENC-POP  RAX ASM-SINK ENC-POP
      RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;] PRIM
   s" ptr-cell-mark" [:
      0 CELL SIZED-GUARD,  RDI POP,
      RSI SNAP-RELOC:XTCELL-DATA-TAG IMM64,  MARK-LBL CALL, ;] PRIM
   s" addr-cells-abi" [:
      RAX ADDRESS-CELLS:ABI-VERSION IMM32,  RAX PUSH, ;] PRIM
   s" snapshot-format" [: RAX SNAPSHOT-FORMAT:VERSION IMM32,  RAX PUSH, ;] PRIM
   s" snap-rebase" REFUSE
   s" executable-build-enter" [: BUILD-ENTER-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" executable-build-leave" [: BUILD-LEAVE-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" set-tier" [: SET-TIER-BODY ;] PRIM
   s" tier@" [: RAX TIER-OFF CELL@,  RAX PUSH, ;] PRIM
   s" code-origin" [: RSI POP,  RDI POP,  X64PROV:QUERY,  RAX PUSH, ;] PRIM ;

public

: ENGINE-STATE, ( -- )
   HEAP,  PRINTERS,  HOOKS,
   DICT-COUNT,  SEAL-ROWS,  WORDLIST-ROWS,  SCOPE-ROWS, ;

\ ---- FFI rows ----------------------------------------------------------------
\ The twins of habu1.f BFFI-CALL, BFFI-CALL-N and BFFI-CALL-BOUNDED: integer
\ SysV calls through SYSV-CALL,. argbuf holds at least eight cells, the
\ contract ARM64's unconditional load of x0..x7 sets; SysV takes six in
\ registers, so the other two, and every cell past eight that ffi-call-n and
\ ffi-call-bounded pass, go on the stack.
\
\ And of BFFI-CALL-ABI(-R) and BFFI-CALL-ABI(-R)-BOUNDED, the rows lib/ffi-abi.f
\ plans a call for: the integer arguments in argbuf, the doubles in fpbuf and
\ the stack arguments prepacked in stackbuf. rdi..r9 take argbuf[0..6),
\ xmm0..7 always take fpbuf[0..8), and the stack takes max(nstack, 0)
\ stackbuf cells and nothing else. argbuf[6..9) pass nowhere, but the twins'
\ guards over them stand. No argument carries a count of doubles, so al is
\ VEC-REGS, the bound SysV admits.

private

6 constant REG-CELLS                    \ rdi rsi rdx rcx r8 r9
8 constant BUF-CELLS                    \ x0..x7
8 constant SRET-SLOT                    \ x8, ARM64's indirect-result register
SRET-SLOT 1+ constant ABI-SLOTS         \ x0..x8, the slots the twin guards
8 constant VEC-REGS                     \ xmm0..7

\ Guard argbuf[i] for each i below the count in rcx, argbuf the cell n below
\ the top, before the pops, as BFFI-GUARD-ARGS and BFFI-GUARD-BOUNDS do with
\ the count in x14. The quotation loads the span's length into rsi with
\ rax = i. The guard clobbers every scratch register, so i and the count wait
\ on the machine stack across it and argbuf and the extents are read again
\ each turn. The compare is signed, so a count <= 0 guards nothing.
: GUARD-ARGS, ( [ -- ] n -- ) {: buf:n :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   RAX ZERO-REG,
   next X64CODE:LBL,
   RAX RCX ASM-SINK ENC-CMP-RR  C-GE done JCC,
   RCX ASM-SINK ENC-PUSH  RAX ASM-SINK ENC-PUSH
   RCX buf PEEK,  RDI RCX RAX CELL 0 MEM-IDX ASM-SINK ENC-MOV-RM
   execute
   RDI RSI PROT-SPAN-CALL,
   RAX ASM-SINK ENC-POP  RCX ASM-SINK ENC-POP  RAX ASM-SINK ENC-INC
   next JMP,
   done X64CODE:LBL, ;

\ The raw rows' guard: one byte at each live argument, a band pointer's test.
: BYTE-LEN, ( -- ) RSI 1 IMM32, ;

\ The bounded rows': extents[i] bytes, extents the cell n below the top.
: EXTENT-LEN, ( n -- ) {: ext:n :}
   RCX ext PEEK,  RSI RCX RAX CELL 0 MEM-IDX ASM-SINK ENC-MOV-RM ;

\ r10 = max(nargs, BUF-CELLS) - REG-CELLS, the stack cells, from nargs in r10;
\ signed, as BFFI-CALL-N-CORE compares.
: STACK-CELLS, ( -- )
   X64CODE:LBL {: wide:label :}
   R10 BUF-CELLS >IMM8 ASM-SINK ENC-CMP-RI8  C-GE wide JCC,
   R10 BUF-CELLS IMM32,
   wide X64CODE:LBL,
   R10 REG-CELLS >IMM8 ASM-SINK ENC-SUB-RI8 ;

\ Load rdi rsi rdx rcx r8 r9 from the six cells a register points at, rcx
\ last, so the register may be rcx.
: REG-ARGS, ( r64 -- ) {: base:r64 :}
   RDI base 0 MOV-LOAD,  RSI base CELL MOV-LOAD,  RDX base 2 CELL * MOV-LOAD,
   R8 base 4 CELL * MOV-LOAD,  R9 base 5 CELL * MOV-LOAD,
   RCX base 3 CELL * MOV-LOAD, ;

\ With rax = argbuf, r10 = the stack cells and r11 = the function: load the
\ register arguments, point rax at the first stack cell, call, push the answer.
: BUF-CALL, ( -- )
   RAX REG-ARGS,
   RAX REG-CELLS CELL * >IMM8 ASM-SINK ENC-ADD-RI8
   0 SYSV-CALL,
   RAX PUSH, ;

\ ffi-call ( argbuf nargs fn -- ret ): eight cells, whatever nargs is.
: FFI-CALL-BODY ( -- )
   RCX 1 PEEK,  [: BYTE-LEN, ;] 2 GUARD-ARGS,
   R11 POP,  DROP,  RAX POP,
   R10 BUF-CELLS REG-CELLS - IMM32,
   BUF-CALL, ;

\ ffi-call-n ( argbuf nargs fn -- ret ): max(nargs, 8) cells, unguarded as
\ BFFI-CALL-N is.
: FFI-CALL-N-BODY ( -- )
   R11 POP,  R10 POP,  RAX POP,
   STACK-CELLS,
   BUF-CALL, ;

\ ffi-call-bounded ( argbuf extents nargs fn -- ret ): max(nargs, 8) cells,
\ each live argument guarded over its extent.
: FFI-CALL-BOUNDED-BODY ( -- )
   RCX 1 PEEK,  [: 2 EXTENT-LEN, ;] 3 GUARD-ARGS,
   R11 POP,  R10 POP,  DROP,  RAX POP,
   STACK-CELLS,
   BUF-CALL, ;

\ With r11 = the function, r10 = nstack and argbuf fpbuf stackbuf the top three
\ cells: pop them, load xmm0..7 from fpbuf and rdi..r9 from argbuf, and call
\ with the stackbuf cells. nstack is the caller's data, and SYSV-CALL, copies
\ r10 cells, so a negative count copies none, as BFFI-COPY-ABI-STACK skips.
: ABI-CALL, ( -- )
   X64CODE:LBL {: counted:label :}
   R10 R10 ASM-SINK ENC-TEST-RR  C-GE counted JCC,
   R10 ZERO-REG,
   counted X64CODE:LBL,
   RAX POP,
   RCX POP,
   VEC-REGS 0 ?do  i >XMM RCX i CELL * MEM-OFF ASM-SINK ENC-MOVSD-RM  loop
   RCX POP,  RCX REG-ARGS,
   VEC-REGS SYSV-CALL, ;

\ ffi-call-abi(-r) ( argbuf fpbuf stackbuf nstack nint sret fn -- n|r ): one
\ byte guarded at argbuf[i] for each i < nint, then at argbuf[8] when sret is
\ nonzero, as BFFI-GUARD-ARGS guards x8.
: FFI-CALL-ABI-BODY ( -- )
   X64CODE:LBL {: direct:label :}
   RCX 2 PEEK,  [: BYTE-LEN, ;] 6 GUARD-ARGS,
   RAX 1 PEEK,  RAX RAX ASM-SINK ENC-TEST-RR  C-E direct JCC,
   RDI 6 PEEK,  RDI RDI SRET-SLOT CELL * MOV-LOAD,  BYTE-LEN,
   RDI RSI PROT-SPAN-CALL,
   direct X64CODE:LBL,
   R11 POP,  DROP,  DROP,  R10 POP,
   ABI-CALL, ;

\ ffi-call-abi(-r)-bounded ( argbuf fpbuf stackbuf regext stkext nstack fn --
\ n|r ): regext[i] bytes at argbuf[i] for each of the ABI-SLOTS, then
\ stkext[i] bytes at stackbuf[i] for each i < nstack, as
\ BFFI-CALL-ABI-BOUNDED-CORE guards.
: FFI-CALL-ABI-BOUNDED-BODY ( -- )
   RCX ABI-SLOTS IMM32,  [: 3 EXTENT-LEN, ;] 6 GUARD-ARGS,
   RCX 1 PEEK,  [: 2 EXTENT-LEN, ;] 4 GUARD-ARGS,
   R11 POP,  R10 POP,  DROP,  DROP,
   ABI-CALL, ;

\ The -r rows' answer: xmm0's bits.
: XMM0-PUSH, ( -- ) RAX XMM0 ASM-SINK ENC-MOVQ-RX  RAX PUSH, ;

public

: FFI, ( -- )
   s" ffi-call" [: FFI-CALL-BODY ;] PRIM
   s" ffi-call-n" [: FFI-CALL-N-BODY ;] PRIM
   s" ffi-call-bounded" [: FFI-CALL-BOUNDED-BODY ;] PRIM
   s" ffi-call-abi" [: FFI-CALL-ABI-BODY  RAX PUSH, ;] PRIM
   s" ffi-call-abi-r" [: FFI-CALL-ABI-BODY  XMM0-PUSH, ;] PRIM
   s" ffi-call-abi-bounded" [: FFI-CALL-ABI-BOUNDED-BODY  RAX PUSH, ;] PRIM
   s" ffi-call-abi-r-bounded" [: FFI-CALL-ABI-BOUNDED-BODY  XMM0-PUSH, ;] PRIM ;

\ ---- task entry --------------------------------------------------------------
\ The twin of habu1.f BTASK-ENTRY. `task-entry` answers an immutable pthread
\ entry, not a Habu execution token: a SysV function whose one argument, in
\ rdi, is the TASK-ABI descriptor, and which enters that task's VM state. It
\ saves rbx rbp r12-r15, the SysV callee-saved set, which is exactly the VM's
\ registers (docs/x86-64.md "Machine model"), so no vector register needs
\ BTASK-ENTRY's d8..d15 save. It writes two cells of the task's own: the TCB
\ address into the task's region, and DONE into the status once the body
\ returns. It never writes RUNNING: lib/task.f ACTIVATE stores that before
\ pthread_create, and a store here could put RUNNING back over the HALT-REQ a
\ TASK:HALT in the create window leaves. DONE is the last write the task makes
\ to anything another thread reads, so it is published with atomic!'s xchg,
\ the release BTASK-ENTRY gets from STLR (ATOMICS, above).

private

: INTERP-REG ( -- r64 ) ENGINE-GPR:X64-INTERP >R64 ;

\ The VM's registers, pushed and popped in reverse.
: SAVE-VM, ( -- )
   INTERP-REG ASM-SINK ENC-PUSH  DATA-REG ASM-SINK ENC-PUSH  DSP ASM-SINK ENC-PUSH
   DBASE-REG ASM-SINK ENC-PUSH  NDICT-REG ASM-SINK ENC-PUSH  CP-REG ASM-SINK ENC-PUSH ;
: RESTORE-VM, ( -- )
   CP-REG ASM-SINK ENC-POP  NDICT-REG ASM-SINK ENC-POP  DBASE-REG ASM-SINK ENC-POP
   DSP ASM-SINK ENC-POP  DATA-REG ASM-SINK ENC-POP  INTERP-REG ASM-SINK ENC-POP ;

\ The entry. The xt is called as execute calls one, and the body keeps rbp, so
\ the TCB is read back through the region. The six pushes leave rsp 8 off the
\ 16-byte alignment at that call, which Habu code does not need: SYSV-CALL,
\ aligns rsp itself before any C call.
: TASK-ENTRY, ( -- )
   SAVE-VM,
   DATA-REG RDI TASK-ABI:REGION-OFF MOV-LOAD,
   DSP RDI TASK-ABI:STACK-OFF MOV-LOAD,
   DBASE-REG RDI TASK-ABI:DBASE-OFF MOV-LOAD,
   NDICT-REG RDI TASK-ABI:NDICT-OFF MOV-LOAD,
   CP-REG RDI TASK-ABI:CP-OFF MOV-LOAD,
   INTERP-REG ZERO-REG,
   RDI DATA-REG TASK-TCB-CELL MOV-STORE,
   RAX RDI TASK-ABI:XT-OFF MOV-LOAD,  RAX ASM-SINK ENC-CALL-REG
   RCX DATA-REG TASK-TCB-CELL MOV-LOAD,
   RAX TASK-ABI:DONE IMM32,
   RAX RCX TASK-ABI:STATUS-OFF MEM-OFF ASM-SINK ENC-XCHG-MR
   RAX ZERO-REG,
   RESTORE-VM,
   ASM-SINK ENC-RET ;

\ task-entry ( -- n ): push the entry's address and jump over it, so the entry
\ lies inside the row's record, as BTASK-ENTRY's does.
: TASK-ENTRY-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: entry:label done:label :}
   RAX entry MOVABS,  RAX PUSH,  done JMP,
   entry X64CODE:LBL,  TASK-ENTRY,
   done X64CODE:LBL, ;

public

\ callback-entry waits for the SysV twin of habu1.f BCALLBACK-THUNK
\ (habu-port-the-ffi-676f745d).
: TASK, ( -- )
   s" task-entry" [: TASK-ENTRY-BODY ;] PRIM
   s" callback-entry" REFUSE ;

\ ---- definition writers ------------------------------------------------------
\ The nine rows an interpreter written in Habu publishes definitions, namespace
\ rows, aliases and package scope through (src/habu/prims.f, "the definition
\ writers"): the twins of habu2.f DEFWRITE's bodies, in each twin's check
\ order. Every refusal comes before the first store and writes nothing on fd
\ 2: a live task exits TASK-LIVE-RC in the four dictionary rows, a
\ protected wid after the seal ENGINE-ERROR:SEAL-PACKAGE, and every other
\ refusal ENGINE-ERROR:SEAL-VIOLATION. Past the checks two helper exits can
\ still end a row: 74 when the index cannot be kept, and
\ ENGINE-ERROR:CODE-ORIGIN-FULL. Each record carries
\ ENGINE-PRIMS:GLOBAL-INT-WID, as on ARM64.

private

\ The record writers' frame, where the arguments survive the helper calls.
0 constant DW-NAME                     \ the name's address
8 constant DW-LEN                      \ its length
16 constant DW-WID                     \ the wid
24 constant DW-ARG                     \ the flag, the source's index or the kind
32 constant DW-REC                     \ record NDICT, the one the row writes
40 constant DW-SLOT                    \ the code slot past a long name
48 constant DW-FRAME
$3A constant NAME-COLON                \ a qualified name's separator

: FRAME-OPEN, ( -- ) RSP DW-FRAME >IMM8 ASM-SINK ENC-SUB-RI8 ;
: FRAME-CLOSE, ( -- ) RSP DW-FRAME >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ Pop the data stack's top into the frame cell.
: POP-TO, ( n -- ) {: off:n :} RAX POP,  RAX RSP off MOV-STORE, ;

\ A pending definition's record is slot NDICT, the slot a record writer fills.
: NOT-PENDING, ( -- )
   RAX PEND-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE SEAL-TRAP-LBL JCC, ;

\ Slot DICT-CAP is the control-flow band, not a record.
: DICT-ROOM, ( -- )
   NDICT-REG DICT-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-AE SEAL-TRAP-LBL JCC, ;

\ The frame's name: refuse an empty one, and a long one whose slots at CP
\ would reach the code ceiling. The compares are unsigned and never wrap: a CP
\ already at the ceiling, and a length no region holds, a negative one among
\ them, are refused rather than added. Clobbers rax rcx rsi.
: NAME-SIZE, ( -- )
   X64CODE:LBL {: fits:label :}
   RSI RSP DW-LEN MOV-LOAD,
   RSI RSI ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   RSI DNAME-INL >IMM8 ASM-SINK ENC-CMP-RI8  C-BE fits JCC,
   RAX DBASE-REG CODE-CEILING MEM-OFF ASM-SINK ENC-LEA
   CP-REG RAX ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   RAX CP-REG ASM-SINK ENC-SUB-RR                     \ rax = the room below it
   RSI RAX ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,   \ the bytes alone reach it
   RCX RSI SLOT-UP,
   RCX RAX ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,   \ so do their slots
   fits X64CODE:LBL, ;

\ Refuse a live record of the frame's folded name in the frame's wid: the
\ one-wordlist search's probe, FIND-LBL, answers it in rax.
: FRESH, ( -- )
   RDI RSP DW-NAME MOV-LOAD,  RSI RSP DW-LEN MOV-LOAD,  RDX RSP DW-WID MOV-LOAD,
   FIND-LBL CALL,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE SEAL-TRAP-LBL JCC, ;

\ Wid rdx is a namespace row's or a retired record's, not a wordlist.
: REAL-WID, ( -- )
   RDX DICT-WL:NAMESPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-E SEAL-TRAP-LBL JCC,
   RDX DICT-WL:RETIRED >IMM8 ASM-SINK ENC-CMP-RI8  C-E SEAL-TRAP-LBL JCC, ;

\ After the seal, branch to the label for a protected wid rdx, judged as
\ habu1.f LPROTWIDQ judges one: the two engine-reserved wids always, one at or
\ above PROT-WID-MAX, unsigned, never, and any other by its PROT-BITS, bit.
\ Clobbers rax rcx rdx rsi rdi.
: OPEN-WID, ( label -- ) {: prot:label :}
   X64CODE:LBL {: open:label :}
   RAX SEAL-NDICT-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E open JCC,
   RDX OWNER-API-PUB-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E prot JCC,
   RDX OWNER-API-PRI-WID >IMM8 ASM-SINK ENC-CMP-RI8  C-E prot JCC,
   RDX PROT-WID-MAX >IMM32 ASM-SINK ENC-CMP-RI32  C-AE open JCC,
   RDI RDX ASM-SINK ENC-MOV-RR
   PROT-BITS,
   RAX RSI MEM-AT ASM-SINK ENC-MOV-RM
   RAX RDX ASM-SINK ENC-TEST-RR  C-NE prot JCC,
   open X64CODE:LBL, ;

\ Store the frame's name in record NDICT, whose address DW-REC takes: [16]
\ its length, with DNAME-EXT for a long one; [24] and [32] the bytes inline,
\ zero past them, or [24] the address of a copy at CP, zero-padded to the
\ next code slot and marked native, with CP moved to that slot. The code band
\ opens over a long name's slots and the record band over the record; the
\ caller closes both. The checks have admitted the name.
: NAME-STORE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: banded:label long:label done:label :}
   NDICT-REG RECORD-AT,
   R8 RSP DW-REC MOV-STORE,
   RDI RSP DW-LEN MOV-LOAD,
   RDI DNAME-INL >IMM8 ASM-SINK ENC-CMP-RI8  C-BE banded JCC,
   RDI CP-REG RDI 1 0 MEM-IDX ASM-SINK ENC-LEA
   RDI RDI SLOT-UP,
   RDI RSP DW-SLOT MOV-STORE,
   WINDOW-OPEN,                                       \ over [CP, slot)
   banded X64CODE:LBL,
   RDI RSP DW-REC MOV-LOAD,  RSI DREC IMM32,  WINDOW-SPAN,
   R8 RSP DW-REC MOV-LOAD,
   RSI RSP DW-NAME MOV-LOAD,  RCX RSP DW-LEN MOV-LOAD,
   RAX ZERO-REG,
   RAX R8 REC-NAME MOV-STORE,  RAX R8 REC-NAME CELL + MOV-STORE,
   RCX DNAME-INL >IMM8 ASM-SINK ENC-CMP-RI8  C-A long JCC,
   RCX R8 REC-FLAGS MOV-STORE,
   RDI R8 REC-NAME MEM-OFF ASM-SINK ENC-LEA
   COPY-BYTES,
   done JMP,
   long X64CODE:LBL,
   RAX DNAME-EXT IMM64,  RAX RCX ASM-SINK ENC-OR-RR
   RAX R8 REC-FLAGS MOV-STORE,
   CP-REG R8 REC-NAME MOV-STORE,
   RDI CP-REG ASM-SINK ENC-MOV-RR
   COPY-BYTES,
   RCX RSP DW-SLOT MOV-LOAD,  RCX RDI ASM-SINK ENC-SUB-RR
   ZERO-BYTES,
   RDI CP-REG ASM-SINK ENC-MOV-RR  RSI RSP DW-SLOT MOV-LOAD,
   X64PROV:NATIVE-RANGE,
   CP-REG RSP DW-SLOT MOV-LOAD,
   done X64CODE:LBL, ;

\ Count record NDICT, index it and close the window.
: PUBLISH-RECORD, ( -- )
   NDICT-REG ASM-SINK ENC-INC
   HIDX-ADD,
   WINDOW-CLOSE, ;

\ namespace-record ( ptr u8 n bool -- n ): record NDICT, [0] a fresh wid, [8]
\ a second when the flag is set and else 0, [40] DICT-WL:NAMESPACE; a colon
\ in the name is refused. It answers the row's index.
: NAMESPACE-RECORD-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label clean:label one:label :}
   TASK-LIVE-GUARD,
   FRAME-OPEN,
   DW-ARG POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   NOT-PENDING,
   DICT-ROOM,
   NAME-SIZE,
   RDI RSP DW-NAME MOV-LOAD,  RSI RSP DW-LEN MOV-LOAD,  RCX ZERO-REG,
   scan X64CODE:LBL,
   RCX RSI ASM-SINK ENC-CMP-RR  C-AE clean JCC,
   RAX RDI RCX 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX NAME-COLON >IMM8 ASM-SINK ENC-CMP-RI8  C-E SEAL-TRAP-LBL JCC,
   RCX ASM-SINK ENC-INC
   scan JMP,
   clean X64CODE:LBL,
   RAX DICT-WL:NAMESPACE >IMM32 ASM-SINK ENC-MOV-RI32
   RAX RSP DW-WID MOV-STORE,
   FRESH,
   NAME-STORE,
   R8 RSP DW-REC MOV-LOAD,
   RAX WIDN-CELL CELL@,
   RAX R8 REC-CODE MOV-STORE,
   RAX ASM-SINK ENC-INC
   RCX ZERO-REG,
   RDX RSP DW-ARG MOV-LOAD,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E one JCC,
   RCX RAX ASM-SINK ENC-MOV-RR  RAX ASM-SINK ENC-INC
   one X64CODE:LBL,
   RCX R8 REC-CODE CELL + MOV-STORE,
   RAX WIDN-CELL CELL!,
   RAX RSP DW-WID MOV-LOAD,  RAX R8 REC-WID MOV-STORE,
   PUBLISH-RECORD,
   FRAME-CLOSE,
   RAX NDICT-REG -1 MEM-OFF ASM-SINK ENC-LEA  RAX PUSH, ;

\ namespace-private ( n -- ): namespace row n, below NDICT, unsigned, whose [8]
\ is 0, takes a fresh private wid.
: NAMESPACE-PRIVATE-BODY ( -- )
   TASK-LIVE-GUARD,
   RCX POP,
   RCX NDICT-REG ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   RCX RECORD-AT,
   RAX R8 REC-WID MOV-LOAD,
   RAX DICT-WL:NAMESPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-NE SEAL-TRAP-LBL JCC,
   RAX R8 REC-CODE CELL + MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE SEAL-TRAP-LBL JCC,
   R8 ASM-SINK ENC-PUSH                               \ the record, past the span call
   RDI R8 ASM-SINK ENC-MOV-RR  RSI DREC IMM32,  WINDOW-SPAN,
   R8 ASM-SINK ENC-POP
   RAX WIDN-CELL CELL@,
   RAX R8 REC-CODE CELL + MOV-STORE,
   RAX ASM-SINK ENC-INC
   RAX WIDN-CELL CELL!,
   WINDOW-CLOSE, ;

\ alias-record ( ptr u8 n n n -- ) name, source index, wid: record NDICT with
\ the source's [0] and [8] and exactly its DNAME-IMM, DNAME-WIDE and
\ DNAME-MIN-IN bits. A namespace, retired or DNAME-INT source is refused: an
\ alias without the bit would run an internal body from the interpret loop.
: ALIAS-RECORD-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: prot:label done:label :}
   TASK-LIVE-GUARD,
   FRAME-OPEN,
   DW-WID POP-TO,  DW-ARG POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   RDX RSP DW-WID MOV-LOAD,
   REAL-WID,
   prot OPEN-WID,
   NOT-PENDING,
   RCX RSP DW-ARG MOV-LOAD,
   RCX NDICT-REG ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   RCX RECORD-AT,
   RAX R8 REC-WID MOV-LOAD,
   RAX DICT-WL:NAMESPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-E SEAL-TRAP-LBL JCC,
   RAX DICT-WL:RETIRED >IMM8 ASM-SINK ENC-CMP-RI8  C-E SEAL-TRAP-LBL JCC,
   RAX DNAME-INT IMM64,
   RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-TEST-MR  C-NE SEAL-TRAP-LBL JCC,
   DICT-ROOM,
   NAME-SIZE,
   FRESH,
   NAME-STORE,
   RDI RSP DW-REC MOV-LOAD,
   RCX RSP DW-ARG MOV-LOAD,  RCX RECORD-AT,           \ r8 = the source
   RAX R8 REC-CODE MOV-LOAD,  RAX RDI REC-CODE MOV-STORE,
   RAX R8 REC-CODE CELL + MOV-LOAD,  RAX RDI REC-CODE CELL + MOV-STORE,
   RAX DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or IMM64,
   RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-AND-RM
   RAX RDI REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   RAX RSP DW-WID MOV-LOAD,  RAX RDI REC-WID MOV-STORE,
   PUBLISH-RECORD,
   FRAME-CLOSE,
   done JMP,
   prot X64CODE:LBL,  ENGINE-ERROR:SEAL-PACKAGE EXIT-GROUP,
   done X64CODE:LBL, ;

\ package-scope! ( n n -- ) namespace index, parent wid: PKG-PUB and PKG-PRI
\ from the row's [0] and [8], PKG-PARENT the wid and PKG-REC the row; `-1 0`
\ clears all four. It stores while a task is live, as habu2.f
\ DEFWRITE:PACKAGE-SCOPE does: the keywords guard, and recovery restores the
\ scope through it.
: PACKAGE-SCOPE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: set:label done:label :}
   RDX POP,  RCX POP,                                 \ the parent wid, the row
   RCX -1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE set JCC,
   RDX RDX ASM-SINK ENC-TEST-RR  C-NE SEAL-TRAP-LBL JCC,  \ only `-1 0` clears
   RAX ZERO-REG,
   RAX PKG-PUB-CELL CELL!,  RAX PKG-PRI-CELL CELL!,
   RAX PKG-PARENT-CELL CELL!,  RAX PKG-REC-CELL CELL!,
   done JMP,
   set X64CODE:LBL,
   RCX NDICT-REG ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   RCX RECORD-AT,
   RAX R8 REC-WID MOV-LOAD,
   RAX DICT-WL:NAMESPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-NE SEAL-TRAP-LBL JCC,
   RCX R8 REC-CODE CELL + MOV-LOAD,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   RAX R8 REC-CODE MOV-LOAD,
   RAX PKG-PUB-CELL CELL!,  RCX PKG-PRI-CELL CELL!,
   RDX PKG-PARENT-CELL CELL!,  R8 PKG-REC-CELL CELL!,
   done X64CODE:LBL, ;

\ def-open ( ptr u8 n n n -- ) name, wid, kind: record NDICT unpublished, [0]
\ CP past the name, [8] 0, the kind beside the length and the wid in [40];
\ PEND-CELL the record, and LASTC-CELL too for DKIND:VAL or DKIND:ADDR;
\ TSIG, TCSIG, DOESB and TRUSTED clear; DEF-TIER-CELL takes TIER-CELL and the
\ provenance window opens at that CP. At tier 0 it opens the record only.
: DEF-OPEN-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: prot:label done:label nolast:label :}
   TASK-LIVE-GUARD,
   FRAME-OPEN,
   DW-ARG POP-TO,  DW-WID POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   RAX DKIND:MASK invert IMM64,
   RAX RSP DW-ARG MEM-OFF ASM-SINK ENC-TEST-MR  C-NE SEAL-TRAP-LBL JCC,
   RDX RSP DW-WID MOV-LOAD,
   REAL-WID,
   prot OPEN-WID,
   NOT-PENDING,
   RAX DBASE-REG CODE-CEILING MEM-OFF ASM-SINK ENC-LEA
   CP-REG RAX ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   DICT-ROOM,
   NAME-SIZE,
   FRESH,
   NAME-STORE,
   R8 RSP DW-REC MOV-LOAD,
   CP-REG R8 REC-CODE MOV-STORE,
   RAX ZERO-REG,  RAX R8 REC-CODE CELL + MOV-STORE,
   RAX RSP DW-ARG MOV-LOAD,  RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   RAX RSP DW-WID MOV-LOAD,  RAX R8 REC-WID MOV-STORE,
   R8 PEND-CELL CELL!,
   RAX RSP DW-ARG MOV-LOAD,                           \ a body that pushes a cell is the slot `does>` patches
   RAX RAX ASM-SINK ENC-TEST-RR  C-E nolast JCC,
   RCX DKIND:CAST IMM64,
   RAX RCX ASM-SINK ENC-CMP-RR  C-E nolast JCC,
   R8 LASTC-CELL CELL!,
   nolast X64CODE:LBL,
   RAX ZERO-REG,
   RAX TSIG-A-CELL CELL!,  RAX TSIG-U-CELL CELL!,
   RAX TCSIG-A-CELL CELL!,  RAX TCSIG-U-CELL CELL!,
   RAX DOESB-CELL CELL!,  RAX TRUSTED-CELL CELL!,
   RAX NCOMP-DISPATCH:TIER-CELL CELL@,  RAX NCOMP-DISPATCH:DEF-TIER-CELL CELL!,
   X64PROV:OPEN,
   WINDOW-CLOSE,
   FRAME-CLOSE,
   done JMP,
   prot X64CODE:LBL,  ENGINE-ERROR:SEAL-PACKAGE EXIT-GROUP,
   done X64CODE:LBL, ;

\ body-append ( ptr u8 n -- ): append the bytes and a space to BODYBUF, as the
\ capture `:` appends through does. BODYLEN past BODYBUF-CAP, and u at or past
\ the room left, both unsigned, are refused; pass 2 re-runs a captured body
\ and stores nothing.
: BODY-APPEND-BODY ( -- )
   X64CODE:LBL {: done:label :}
   RCX POP,  RSI POP,                                 \ the count, the bytes
   RAX BODYLEN-CELL CELL@,
   RAX BODYBUF-CAP >IMM32 ASM-SINK ENC-CMP-RI32  C-A SEAL-TRAP-LBL JCC,
   RDX BODYBUF-CAP IMM32,
   RDX RAX ASM-SINK ENC-SUB-RR                        \ rdx = the room left
   RCX RDX ASM-SINK ENC-CMP-RR  C-AE SEAL-TRAP-LBL JCC,
   RDX P2-CELL CELL@,
   RDX RDX ASM-SINK ENC-TEST-RR  C-NE done JCC,
   RDX RAX RCX 1 1 MEM-IDX ASM-SINK ENC-LEA           \ the length past the space
   RDI DATA-REG RAX 1 BODYBUF-OFF MEM-IDX ASM-SINK ENC-LEA
   COPY-BYTES,
   RAX STR-SPACE IMM32,
   0 >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
   RDX BODYLEN-CELL CELL!,
   done X64CODE:LBL, ;

\ ( ptr u8 n -- ): the span into the friend-arena cells aoff and uoff while a
\ definition is pending.
: PENDING-SPAN-BODY ( n n -- ) {: aoff:n uoff:n :}
   RCX POP,  RAX POP,
   RDX PEND-CELL CELL@,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   RAX aoff CELL!,  RCX uoff CELL!, ;

\ trust-sig! ( ptr u8 n -- ): the pending definition's signature span into
\ TSIG-A and TSIG-U.
: TRUST-SIG-BODY ( -- )
   TSIG-A-CELL TSIG-U-CELL PENDING-SPAN-BODY ;

\ created-sig! ( ptr u8 n -- ): the pending definition's `does>` signature
\ span into TCSIG-A and TCSIG-U.
: CREATED-SIG-BODY ( -- )
   TCSIG-A-CELL TCSIG-U-CELL PENDING-SPAN-BODY ;

\ def-close ( -- ): the pending definition of the native tier ends, as the
\ engine's tier-1 `;` ends one (habu2.f DEFWRITE:NATIVE-CLOSE): the provenance window
\ closes native, then DEF-TIER-CELL, TSIG, TCSIG, DOESB, TRUSTED and PEND-CELL
\ clear. Nothing pending, and DEF-TIER-CELL other than 1, are refused.
: DEF-CLOSE-BODY ( -- )
   RDX PEND-CELL CELL@,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   RCX NCOMP-DISPATCH:DEF-TIER-CELL CELL@,
   RCX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-NE SEAL-TRAP-LBL JCC,
   1 X64PROV:CLOSE,
   RAX ZERO-REG,
   RAX NCOMP-DISPATCH:DEF-TIER-CELL CELL!,
   RAX TSIG-A-CELL CELL!,  RAX TSIG-U-CELL CELL!,
   RAX TCSIG-A-CELL CELL!,  RAX TCSIG-U-CELL CELL!,
   RAX DOESB-CELL CELL!,  RAX TRUSTED-CELL CELL!,
   RAX PEND-CELL CELL!, ;

\ A caught compiler failure leaves its handler, return and loop state to the
\ catch frame. Only definition-local state is abandoned before its cursors
\ are lowered, and the code window closes before Habu recovery executes.
: DEF-ABORT-BODY ( -- )
   WINDOW-CLOSE,
   -1 X64PROV:CLOSE,
   RAX ZERO-REG,
   RAX NCOMP-DISPATCH:DEF-TIER-CELL CELL!,
   RAX LVD-CELL CELL!,  RAX VSP-CELL CELL!,
   RAX QPATCH-CELL CELL!,  RAX FRAME-CELL CELL!,  RAX QFRAME-CELL CELL!,
   RAX JIT-SNAP:SP-CELL CELL!,  RAX JIT-QUOT:SP-CELL CELL!,
   RAX LOCN-CELL CELL!,  RAX BODYLEN-CELL CELL!,  RAX EXITH-CELL CELL!,
   RAX PEND-CELL CELL!,  RAX CMM-CELL CELL!,
   RAX CMFRD-CELL CELL!,  RAX CMBK-CELL CELL!,
   RAX TSIG-A-CELL CELL!,  RAX TSIG-U-CELL CELL!,
   RAX TCSIG-A-CELL CELL!,  RAX TCSIG-U-CELL CELL!,
   RAX CRSIG-A-CELL CELL!,  RAX CRSIG-U-CELL CELL!,
   RAX DOESB-CELL CELL!,  RAX TRUSTED-CELL CELL!,
   RAX 8191 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX REGALLOC-ABI:VRFREE-CELL CELL!, ;

\ Trusted recovery drops a rejected REPL line's stack without looping through
\ dynamic depth, which the native compiler cannot certify as a fixed effect.
: STACK-CLEAR-BODY ( -- )
   DSP DATA-REG STACK-ABI:BASE-CELL MOV-LOAD, ;

: IMM-MARK-BODY ( -- ) DNAME-IMM NEWEST-MARK, ;

$C3 constant RET-OP
INT3 $0101010101010101 * constant INT3-CELL
INT3-CELL $FF invert and RET-OP or constant CAST-CELL

\ Publish a cast's identity slot at CP and the pending record that names it.
: DEF-CAST-BODY ( -- )
   TASK-LIVE-GUARD,
   RDX PEND-CELL CELL@,
   RDX RDX ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   NDICT-REG RECORD-AT,
   RDX R8 ASM-SINK ENC-CMP-RR  C-NE SEAL-TRAP-LBL JCC,
   RCX R8 REC-FLAGS MOV-LOAD,
   RAX DKIND:MASK IMM64,  RCX RAX ASM-SINK ENC-AND-RR
   RAX DKIND:CAST IMM64,  RCX RAX ASM-SINK ENC-CMP-RR  C-NE SEAL-TRAP-LBL JCC,
   RAX DOESB-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE SEAL-TRAP-LBL JCC,
   RDI CP-REG CODE-SLOT MEM-OFF ASM-SINK ENC-LEA
   WINDOW-OPEN,
   RDI PEND-CELL CELL@,  RSI DREC IMM32,  WINDOW-SPAN,
   RAX CAST-CELL IMM64,  RAX CP-REG 0 MOV-STORE,
   RAX INT3-CELL IMM64,  RAX CP-REG CELL MOV-STORE,
   R8 PEND-CELL CELL@,
   RAX 1 CODE-SPAN:EXACT IMM64,  RAX R8 REC-CODE CELL + MOV-STORE,
   CP-REG CP-REG CODE-SLOT MEM-OFF ASM-SINK ENC-LEA
   1 X64PROV:CLOSE,
   PUBLISH-RECORD,
   RAX ZERO-REG,
   RAX NCOMP-DISPATCH:DEF-TIER-CELL CELL!,
   RAX TSIG-A-CELL CELL!,  RAX TSIG-U-CELL CELL!,
   RAX TCSIG-A-CELL CELL!,  RAX TCSIG-U-CELL CELL!,
   RAX DOESB-CELL CELL!,  RAX TRUSTED-CELL CELL!,
   RAX PEND-CELL CELL!, ;

\ ---- does-patch -----------------------------------------------------------------
\ A created word's routine ends `jmp rel32; ret`, its last six bytes
\ (src/compiler/native/emit-x64.f PUT-RET): the jump falls through to the
\ return while its displacement is 0, and does-patch aims it at a clause,
\ where ARM64 rewrites the routine's final RET word (habu2.f LDOESPATCH). The
\ slot is found by its position in the record's exact span. The clause lies in
\ the code region or the kernel's text, as every routine a linked call reaches
\ does, so its displacement fits rel32.

$E9 constant JMP-REL32                 \ the slot's jump
CALL-REL32-OFF 4 + constant JMP-BYTES  \ the jump, to its displacement's end
JMP-BYTES 1+ constant PATCH-SLOT       \ the jump and the return

\ does-patch's frame.
0 constant DP-ENTRY                    \ the clause's entry, 0 for the bare body
8 constant DP-SLOT                     \ the created routine's `jmp rel32`
16 constant DP-XT                      \ the registrar being called
32 constant DP-FRAME

\ Push the name the body capture starts with: its bytes up to the first space
\ or NUL, or all BODYLEN of them. The twin of habu2.f C-PUSH-DREC-NAME.
\ Clobbers rax rcx rdx rsi.
: PUSH-DREC-NAME, ( -- )
   X64CODE:LBL X64CODE:LBL {: scan:label done:label :}
   RSI DATA-REG BODYBUF-OFF MEM-OFF ASM-SINK ENC-LEA
   RCX ZERO-REG,
   RDX BODYLEN-CELL CELL@,
   scan X64CODE:LBL,
   RCX RDX ASM-SINK ENC-CMP-RR  C-GE done JCC,
   RAX RSI RCX 1 0 MEM-IDX ASM-SINK ENC-MOVZX-8-RM
   RAX STR-SPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-E done JCC,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RCX ASM-SINK ENC-INC
   scan JMP,
   done X64CODE:LBL,
   RSI PUSH,  RCX PUSH, ;

\ Call the registrar in rax with the captured name and CRSIG, the effect the
\ clause declared. A registrar keeps no scratch register.
: RAW-CALL, ( -- )
   RAX RSP DP-XT MOV-STORE,
   PUSH-DREC-NAME,
   RAX CRSIG-A-CELL CELL@,  RAX PUSH,
   RAX CRSIG-U-CELL CELL@,  RAX PUSH,
   RAX RSP DP-XT MOV-LOAD,
   RAX ASM-SINK ENC-CALL-REG ;

\ The twin of habu2.f LASTC-TRUST:PUBLISH: the active checker's trust-raw;
\ without one, with the check hook armed, the target checker's, or the process
\ ends naming it; then the target checker's too unless the active one holds
\ the same operation. With neither a checker nor the hook nothing registers.
: RAW-PUBLISH, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: inactive:label ready:label other:label absent:label done:label :}
   RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E inactive JCC,
   RAX RAX NCOMP-DISPATCH:DECL-RAW-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE ready JCC,
   inactive X64CODE:LBL,
   RCX HOOK-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-TARGET,
   ready X64CODE:LBL,
   RAW-CALL,
   RCX NCOMP-DISPATCH:DECL-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NCOMP-DISPATCH:DECL-RAW-OFF done DECL-TARGET,
   RCX NCOMP-DISPATCH:DECL-CELL CELL@,
   RCX RCX ASM-SINK ENC-TEST-RR  C-E other JCC,
   RCX RCX NCOMP-DISPATCH:DECL-RAW-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX ASM-SINK ENC-CMP-RR  C-E done JCC,
   other X64CODE:LBL,
   RAW-CALL,
   done JMP,
   absent X64CODE:LBL,
   s" trust-raw" REGISTRAR-RC STDERR-EXIT,
   done X64CODE:LBL, ;

\ rax = the active checker's operation at offset n of its record; with no
\ record or no operation the process ends naming it.
: OWNER-OP, ( n ptr u8 n -- ) {: off:n a:ptr u:n :}
   X64CODE:LBL X64CODE:LBL {: absent:label found:label :}
   RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E absent JCC,
   RAX RAX off MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE found JCC,
   absent X64CODE:LBL,
   a u REGISTRAR-RC STDERR-EXIT,
   found X64CODE:LBL, ;

\ With the check hook armed, the checker's tail for the record a definition
\ published (habu2.f EM-REC-WIDE-PUBLISH): its rec-wide-publish, then its
\ rec-min-in@, whose nonzero answer lands in the DNAME-MIN-IN bits of record
\ NDICT-1 between two flips, as min-in-mark stores it. ARM64 finds the two by
\ name; this kernel reads them from the active checker's record, and a missing
\ one ends the process naming it, as a missing word does there.
: WIDE-PUBLISH, ( -- )
   X64CODE:LBL {: done:label :}
   RAX HOOK-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF s" rec-wide-publish" OWNER-OP,
   RAX ASM-SINK ENC-CALL-REG
   NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF s" rec-min-in@" OWNER-OP,
   RAX ASM-SINK ENC-CALL-REG
   R9 POP,
   R9 R9 ASM-SINK ENC-TEST-RR  C-E done JCC,
   R8 NDICT-REG DREC >IMM8 ASM-SINK ENC-IMUL-RRI8
   R8 DBASE-REG R8 1 DREC negate MEM-IDX ASM-SINK ENC-LEA
   R8 PROT-RW PROT-REC,
   R9 MIN-IN-SHIFT >IMM8 ASM-SINK ENC-SHL-RI8
   RAX DNAME-MIN-IN-MASK IMM64,
   R9 RAX ASM-SINK ENC-AND-RR
   R9 R8 REC-FLAGS MEM-OFF ASM-SINK ENC-OR-MR
   R8 PROT-RX PROT-REC,
   done X64CODE:LBL, ;

\ does-patch ( n ptr u8 n -- ) clause entry, created signature: the twin of
\ habu2.f DOESPATCH:PRIM and LDOESPATCH over the record LASTC-CELL names. Its
\ routine must end in the slot, E9 at slot and C3 at slot+5, else the row
\ exits 83 before it writes. Between the record's and the displacement's
\ windows the displacement becomes the entry less the slot's end, and the
\ record's DKIND clears: the body is a clause now, not a push. Entry 0 writes
\ displacement 0, the bare body, and leaves the kind as it is; with the
\ displacement already 0 it writes nothing. A signature then registers as
\ the created word's raw effect, the record's DNAME-WIDE and DNAME-MIN-IN
\ clear for the checker's tail to set again, and CRSIG clears.
: DOES-PATCH-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: patch:label write:label declared:label nocr:label :}
   RCX POP,  RCX CRSIG-U-CELL CELL!,
   RCX POP,  RCX CRSIG-A-CELL CELL!,
   RSP DP-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
   DP-ENTRY POP-TO,
   R8 LASTC-CELL CELL@,
   R8 R8 ASM-SINK ENC-TEST-RR  C-E SEAL-TRAP-LBL JCC,
   RDX R8 REC-CODE CELL + MOV-LOAD,
   RDX CODE-SPAN:MASK >IMM32 ASM-SINK ENC-AND-RI32
   RDX R8 REC-CODE MEM-OFF ASM-SINK ENC-ADD-RM
   RDX RDX PATCH-SLOT negate MEM-OFF ASM-SINK ENC-LEA
   RAX RDX MEM-AT ASM-SINK ENC-MOVZX-8-RM
   RAX JMP-REL32 >IMM32 ASM-SINK ENC-CMP-RI32  C-NE SEAL-TRAP-LBL JCC,
   RAX RDX JMP-BYTES MEM-OFF ASM-SINK ENC-MOVZX-8-RM
   RAX RET-OP >IMM32 ASM-SINK ENC-CMP-RI32  C-NE SEAL-TRAP-LBL JCC,
   RDX RSP DP-SLOT MOV-STORE,
   RAX RSP DP-ENTRY MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE patch JCC,
   0 >R32 RDX CALL-REL32-OFF MEM-OFF ASM-SINK ENC-MOV32-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-E declared JCC,
   patch X64CODE:LBL,
   RDI R8 ASM-SINK ENC-MOV-RR  RSI DREC IMM32,  WINDOW-SPAN,
   RDI RSP DP-SLOT MOV-LOAD,  RDI CALL-REL32-OFF >IMM8 ASM-SINK ENC-ADD-RI8
   RSI 4 IMM32,  WINDOW-SPAN,
   RCX RSP DP-SLOT MOV-LOAD,
   RDX ZERO-REG,
   RAX RSP DP-ENTRY MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E write JCC,
   RDX RAX ASM-SINK ENC-MOV-RR
   RDX RCX ASM-SINK ENC-SUB-RR
   RDX JMP-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   R8 LASTC-CELL CELL@,
   RSI DKIND:MASK invert IMM64,
   RSI R8 REC-FLAGS MEM-OFF ASM-SINK ENC-AND-MR
   write X64CODE:LBL,
   2 >R32 RCX CALL-REL32-OFF MEM-OFF ASM-SINK ENC-MOV32-MR
   WINDOW-CLOSE,
   declared X64CODE:LBL,
   RAX CRSIG-U-CELL CELL@,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E nocr JCC,
   RAW-PUBLISH,
   R8 LASTC-CELL CELL@,
   R8 PROT-RW PROT-REC,
   RAX DNAME-WIDE DNAME-MIN-IN-MASK or invert IMM64,
   RAX R8 REC-FLAGS MEM-OFF ASM-SINK ENC-AND-MR
   R8 PROT-RX PROT-REC,
   WIDE-PUBLISH,
   RAX ZERO-REG,  RAX CRSIG-A-CELL CELL!,  RAX CRSIG-U-CELL CELL!,
   nocr X64CODE:LBL,
   RSP DP-FRAME >IMM8 ASM-SINK ENC-ADD-RI8 ;

public

: DEFINITION, ( -- )
   s" namespace-record" [: NAMESPACE-RECORD-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" namespace-private" [: NAMESPACE-PRIVATE-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" alias-record" [: ALIAS-RECORD-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" package-scope!" [: PACKAGE-SCOPE-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" def-open" [: DEF-OPEN-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" body-append" [: BODY-APPEND-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" trust-sig!" [: TRUST-SIG-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" created-sig!" [: CREATED-SIG-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" def-close" [: DEF-CLOSE-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" def-abort" [: DEF-ABORT-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" stack-clear" [: STACK-CLEAR-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" imm-mark" [: IMM-MARK-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" def-cast" [: DEF-CAST-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID
   s" jit-open" ENGINE-PRIMS:GLOBAL-INT-WID REFUSE-WID
   s" jit-token" ENGINE-PRIMS:GLOBAL-INT-WID REFUSE-WID
   s" does-patch" [: DOES-PATCH-BODY ;] PRIM ;

\ ---- pure rows ---------------------------------------------------------------
\ The arithmetic, comparison, shuffle, memory and float rows. Every row but
\ five is the compiler's own lowering: PRIM-HIR stages the row as one HIR
\ function through X64KHIR (src/habu/kernel-hir-x64.f), the operations the
\ word model gives the word (src/compiler/native/hir-word.f DEF-ARITH to
\ DEF-BYTE-VIEW, src/compiler/native/elaborate.f EXPAND-CELL-INDEX to
\ EXPAND-MAXIMUM), and lays the routine the x86-64 chain emits into the record
\ whole, its own ret included. A word the model has no row for is staged here
\ from operations it has. The five hand-written rows are the guarded stores,
\ the twins of habu1.f BSTORE, BCSTORE and BPLUSSTORE, which compiled stores
\ call rather than inline (elaborate.f DO-STORE), ?dup, whose depth depends on
\ its value, and f., the twin of habu1.f BFDOT, which writes its own line.
\
\ A compiled body calls out only where a divide refuses a zero divisor: to the
\ entry X64SEL:THROW-ENTRY names, which is the kernel's `throw` row here. Its
\ field becomes a site of that row's label; any other call or an address site
\ ends the build with REFUSE-RC.

private

variable HIR-IN
variable HIR-OUT
TYPED-VARIABLE HIR-STAGE [ -- ]

\ The emission laid at the stream's end, its throws linked.
: HIR-USE ( NART:emission -- )
   {: e:NART:emission :}
   e NART:ADDR-SITES 0<> if
      s" x64kernel: " type ROW$ type s"  compiles to an address site" type cr
      s" x64kernel: a compiled row takes an address" REFUSE-RC die
   then
   X64CODE:ASM-LEN {: at:n :}
   e NART:BYTES e NART:SIZE TEXT,
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-KIND@ NEMIT:CALL <>
      e i NART:CALL-TARGET@ X64SEL:THROW-ENTRY <> or if
         s" x64kernel: " type ROW$ type s"  compiles to a call other than throw" type cr
         s" x64kernel: a compiled row calls out" REFUSE-RC die
      then
      at e i NART:CALL-SITE@ + CALL-REL32-OFF + s" throw" ENTRY-LABEL REL32-SITE
   loop ;

: HIR-BODY ( -- )
   ROW$ HIR-IN @ HIR-OUT @ HIR-STAGE @ [: HIR-USE ;] X64KHIR:COMPILE ;

public

\ Register the body the x86-64 chain compiles from the function the stager
\ builds, of n cells in and n out, under the row's name: PRIM's twin for a
\ compiled body, skipped when the tree shaker drops the row.
: PRIM-HIR ( ptr u8 n n n [ -- ] -- )
   HIR-STAGE !  HIR-OUT !  HIR-IN !
   [: HIR-BODY ;] ARGS
   ROW$ KEEP? 0= if exit then
   false RECORD drop ;

private

\ ---- the stagers ----
: PASS ( n -- ) X64KHIR:ARG X64KHIR:RESULT ;

\ ( x -- x o n ): the argument against a literal.
: WITH ( n HIR:opcode -- ) {: k:n o:HIR:opcode :}
   0 X64KHIR:ARG  k X64KHIR:LIT  o X64KHIR:OP2  X64KHIR:RESULT ;

: BIN ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 X64KHIR:ARG  1 X64KHIR:ARG  o X64KHIR:OP2  X64KHIR:RESULT ;

\ ( x -- 0 x sub ).
: NEGATE-HIR ( -- )
   0 X64KHIR:LIT  0 X64KHIR:ARG  HIR-OPCODE:SUB X64KHIR:OP2  X64KHIR:RESULT ;

\ ( x -- (x xor m) - m ), m the mask x 0 lt answers.
: ABS-HIR ( -- )
   0 X64KHIR:ARG {: x:IR-ID:ir-value-id :}
   x  0 X64KHIR:LIT  HIR-OPCODE:LT X64KHIR:OP2 {: m:IR-ID:ir-value-id :}
   x m HIR-OPCODE:XOR X64KHIR:OP2  m HIR-OPCODE:SUB X64KHIR:OP2  X64KHIR:RESULT ;

\ elaborate.f EXPAND-MAXIMUM, `b xor ((a xor b) and (a rel b))`: gt answers the
\ larger, lt the smaller.
: PICK-HIR ( HIR:opcode -- ) {: rel:HIR:opcode :}
   0 X64KHIR:ARG  1 X64KHIR:ARG {: a:IR-ID:ir-value-id b:IR-ID:ir-value-id :}
   a b HIR-OPCODE:XOR X64KHIR:OP2
   a b rel X64KHIR:OP2
   HIR-OPCODE:AND X64KHIR:OP2
   b HIR-OPCODE:XOR X64KHIR:OP2
   X64KHIR:RESULT ;

\ elaborate.f EXPAND-MODULO, a - (a / b) * b, over one division whose quotient
\ /mod also answers.
: QUOTIENT ( -- IR-ID:ir-value-id IR-ID:ir-value-id )
   0 X64KHIR:ARG  1 X64KHIR:ARG {: a:IR-ID:ir-value-id b:IR-ID:ir-value-id :}
   a b HIR-OPCODE:DIV X64KHIR:OP2 {: q:IR-ID:ir-value-id :}
   a  q b HIR-OPCODE:MUL X64KHIR:OP2  HIR-OPCODE:SUB X64KHIR:OP2
   q ;

: MOD-HIR ( -- ) QUOTIENT drop X64KHIR:RESULT ;
: DIVMOD-HIR ( -- ) QUOTIENT {: q:IR-ID:ir-value-id :} X64KHIR:RESULT q X64KHIR:RESULT ;

\ elaborate.f EXPAND-CELL-INDEX: ( base n -- base + n cells ).
: PTR-FIELD-HIR ( -- )
   0 X64KHIR:ARG
   1 X64KHIR:ARG  HIR:CELL-BYTES X64KHIR:LIT  HIR-OPCODE:MUL X64KHIR:OP2
   HIR-OPCODE:ADD X64KHIR:OP2  X64KHIR:RESULT ;

: FETCH-HIR ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 X64KHIR:ARG  o X64KHIR:FETCH  X64KHIR:RESULT ;

\ ( addr -- addr+1 byte ).
: COUNT-HIR ( -- )
   0 X64KHIR:ARG {: x:IR-ID:ir-value-id :}
   x  1 X64KHIR:LIT  HIR-OPCODE:ADD X64KHIR:OP2  X64KHIR:RESULT
   x HIR-OPCODE:BLOAD X64KHIR:FETCH  X64KHIR:RESULT ;

\ A float row's cells are a double's bits. Each argument crosses into a real
\ with bitsreal and a real answer back to a cell with realbits, as a compiled
\ word's values cross (elaborate.f COERCE1 and CELL-CROSS).
: REAL-ARG ( n -- IR-ID:ir-value-id )
   X64KHIR:ARG HIR-OPCODE:BITSREAL X64KHIR:OP1 ;

: REAL-RESULT ( IR-ID:ir-value-id -- )
   HIR-OPCODE:REALBITS X64KHIR:OP1 X64KHIR:RESULT ;

: FBIN ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 REAL-ARG  1 REAL-ARG  o X64KHIR:OP2  REAL-RESULT ;

: FUNARY ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 REAL-ARG  o X64KHIR:OP1  REAL-RESULT ;

\ A comparison answers a flag, which is a cell.
: FREL ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 REAL-ARG  1 REAL-ARG  o X64KHIR:OP2  X64KHIR:RESULT ;

: FREL0 ( HIR:opcode -- ) {: o:HIR:opcode :}
   0 REAL-ARG  o X64KHIR:OP1  X64KHIR:RESULT ;

\ s>f and f>s cross between a cell's number and a real, and so take or answer
\ the cell as it stands.
: S>F-HIR ( -- )
   0 X64KHIR:ARG  HIR-OPCODE:INTREAL X64KHIR:OP1  REAL-RESULT ;

: F>S-HIR ( -- )
   0 REAL-ARG  HIR-OPCODE:REALINT X64KHIR:OP1  X64KHIR:RESULT ;

\ ---- the hand-written rows ----
: STORE-BODY ( -- )
   0 CELL SIZED-GUARD,  RCX POP,  RAX POP,
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

: CSTORE-BODY ( -- )
   0 1 SIZED-GUARD,  RCX POP,  RAX POP,
   RAX R64>N >R8 RCX MEM-AT ASM-SINK ENC-MOV8-MR ;

: ADDSTORE-BODY ( -- )
   0 CELL SIZED-GUARD,  RCX POP,  RAX POP,
   RAX RCX MEM-AT ASM-SINK ENC-ADD-MR ;

: QDUP-BODY ( -- )
   X64CODE:LBL {: done:label :}
   RAX 0 PEEK,
   RAX RAX ASM-SINK ENC-TEST-RR  C-E done JCC,
   RAX PUSH,
   done X64CODE:LBL, ;

\ f.'s frame, written downward from its top: a sign, 19 integer digits, the
\ point, six fraction digits and the newline are 28 bytes.
32 constant FDOT-BYTES
1000000 constant FRAC-SCALE             \ six fraction digits
6 constant FRAC-DIGITS

\ Put the low byte of a register one below rsi, which moves down to it.
: PUT, ( r64 -- ) {: r:r64 :}
   RSI ASM-SINK ENC-DEC
   r R64>N >R8 RSI MEM-AT ASM-SINK ENC-MOV8-MR ;

: CHAR, ( n -- ) {: c:n :}  RCX c IMM32,  RCX PUT, ;

\ rax = ARM64 fcvtzs of a double that is a NaN or not negative, which is all
\ f. converts. cvttsd2si answers MIN-N for a NaN and past a cell, where fcvtzs
\ answers 0 and saturates to MAX-N: MIN-N's sign smeared over rcx flips it to
\ MAX-N, and a NaN, unordered with itself, then takes zero. It clobbers rcx.
: TRUNC-MAG, ( xmm -- ) {: x:xmm :}
   RAX x ASM-SINK ENC-CVTTSD2SI-RR
   RCX RAX ASM-SINK ENC-MOV-RR  RCX 63 >IMM8 ASM-SINK ENC-SAR-RI8
   RAX RCX ASM-SINK ENC-XOR-RR
   RCX ZERO-REG,
   x x ASM-SINK ENC-UCOMISD-RR  C-P RAX RCX ASM-SINK ENC-CMOVCC ;

\ The twin of habu1.f BFDOT: one write through the output device of `-`
\ when bit 63 is set, the decimal of I = fcvtzs(|x|), `.`, the six low digits,
\ zero-padded, of fcvtzs((|x| - I) * 1e6) and a newline. |x| clears bit 63, so
\ an infinity prints MAX-N and MAX-N's six low digits, and a NaN 0.000000
\ after its sign.
: FDOT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: frac:label pos:label :}
   R8 POP,                                          \ the bits
   RSP FDOT-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RSI RSP FDOT-BYTES MEM-OFF ASM-SINK ENC-LEA
   STR-LF CHAR,
   RAX R8 ASM-SINK ENC-MOV-RR
   RAX 1 >IMM8 ASM-SINK ENC-SHL-RI8  RAX 1 >IMM8 ASM-SINK ENC-SHR-RI8
   XMM1 RAX ASM-SINK ENC-MOVQ-XR                    \ |x|
   XMM1 TRUNC-MAG,  R9 RAX ASM-SINK ENC-MOV-RR      \ r9 = I
   XMM2 R9 ASM-SINK ENC-CVTSI2SD-RR
   XMM1 XMM2 ASM-SINK ENC-SUBSD-RR
   RCX FRAC-SCALE IMM32,  XMM2 RCX ASM-SINK ENC-CVTSI2SD-RR
   XMM1 XMM2 ASM-SINK ENC-MULSD-RR
   XMM1 TRUNC-MAG,                                  \ rax = the fraction
   R10 FRAC-DIGITS IMM32,  RCX 10 IMM32,
   frac X64CODE:LBL,
      RDX ZERO-REG,  RCX ASM-SINK ENC-DIV
      RDX [char] 0 >IMM8 ASM-SINK ENC-ADD-RI8  RDX PUT,
      R10 ASM-SINK ENC-DEC  C-NE frac JCC,
   [char] . CHAR,
   RAX R9 ASM-SINK ENC-MOV-RR  DIGITS,
   R8 R8 ASM-SINK ENC-TEST-RR  C-NS pos JCC,
   [char] - CHAR,
   pos X64CODE:LBL,
   RDX RSP FDOT-BYTES MEM-OFF ASM-SINK ENC-LEA  RDX RSI ASM-SINK ENC-SUB-RR
   G-OUT
   RSP FDOT-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

: ARITH-ROWS, ( -- )
   s" +" 2 1 [: HIR-OPCODE:ADD BIN ;] PRIM-HIR
   s" -" 2 1 [: HIR-OPCODE:SUB BIN ;] PRIM-HIR
   s" *" 2 1 [: HIR-OPCODE:MUL BIN ;] PRIM-HIR
   s" /" 2 1 [: HIR-OPCODE:DIV BIN ;] PRIM-HIR
   s" mod" 2 1 [: MOD-HIR ;] PRIM-HIR
   s" /mod" 2 2 [: DIVMOD-HIR ;] PRIM-HIR
   s" abs" 1 1 [: ABS-HIR ;] PRIM-HIR
   s" min" 2 1 [: HIR-OPCODE:LT PICK-HIR ;] PRIM-HIR
   s" max" 2 1 [: HIR-OPCODE:GT PICK-HIR ;] PRIM-HIR ;

: COMPARE-ROWS, ( -- )
   s" =" 2 1 [: HIR-OPCODE:EQUAL BIN ;] PRIM-HIR
   s" <>" 2 1 [: HIR-OPCODE:NE BIN ;] PRIM-HIR
   s" <" 2 1 [: HIR-OPCODE:LT BIN ;] PRIM-HIR
   s" >" 2 1 [: HIR-OPCODE:GT BIN ;] PRIM-HIR
   s" <=" 2 1 [: HIR-OPCODE:LE BIN ;] PRIM-HIR
   s" >=" 2 1 [: HIR-OPCODE:GE BIN ;] PRIM-HIR
   s" 0=" 1 1 [: 0 HIR-OPCODE:EQUAL WITH ;] PRIM-HIR
   s" 0<" 1 1 [: 0 HIR-OPCODE:LT WITH ;] PRIM-HIR
   s" 1+" 1 1 [: 1 HIR-OPCODE:ADD WITH ;] PRIM-HIR
   s" 1-" 1 1 [: 1 HIR-OPCODE:SUB WITH ;] PRIM-HIR
   s" and" 2 1 [: HIR-OPCODE:AND BIN ;] PRIM-HIR
   s" or" 2 1 [: HIR-OPCODE:OR BIN ;] PRIM-HIR
   s" xor" 2 1 [: HIR-OPCODE:XOR BIN ;] PRIM-HIR
   s" invert" 1 1 [: 0 X64KHIR:ARG HIR-OPCODE:INVERT X64KHIR:OP1 X64KHIR:RESULT ;] PRIM-HIR
   s" negate" 1 1 [: NEGATE-HIR ;] PRIM-HIR
   s" lshift" 2 1 [: HIR-OPCODE:LSHIFT BIN ;] PRIM-HIR
   s" rshift" 2 1 [: HIR-OPCODE:RSHIFT BIN ;] PRIM-HIR ;

: STACK-ROWS, ( -- )
   s" dup" 1 2 [: 0 PASS 0 PASS ;] PRIM-HIR
   s" drop" 1 0 [: ;] PRIM-HIR
   s" swap" 2 2 [: 1 PASS 0 PASS ;] PRIM-HIR
   s" nip" 2 1 [: 1 PASS ;] PRIM-HIR
   s" over" 2 3 [: 0 PASS 1 PASS 0 PASS ;] PRIM-HIR
   s" tuck" 2 3 [: 1 PASS 0 PASS 1 PASS ;] PRIM-HIR
   s" rot" 3 3 [: 1 PASS 2 PASS 0 PASS ;] PRIM-HIR
   s" -rot" 3 3 [: 2 PASS 0 PASS 1 PASS ;] PRIM-HIR
   s" 2dup" 2 4 [: 0 PASS 1 PASS 0 PASS 1 PASS ;] PRIM-HIR
   s" 2drop" 2 0 [: ;] PRIM-HIR
   s" 2swap" 4 4 [: 2 PASS 3 PASS 0 PASS 1 PASS ;] PRIM-HIR
   s" 2over" 4 6 [: 0 PASS 1 PASS 2 PASS 3 PASS 0 PASS 1 PASS ;] PRIM-HIR
   s" ?dup" [: QDUP-BODY ;] PRIM ;

: MEMORY-ROWS, ( -- )
   s" @" 1 1 [: HIR-OPCODE:LOAD FETCH-HIR ;] PRIM-HIR
   s" !" [: STORE-BODY ;] PRIM
   s" ptr-field" 2 1 [: PTR-FIELD-HIR ;] PRIM-HIR
   s" byte-view" 1 1 [: 0 PASS ;] PRIM-HIR
   s" cell-view" 1 1 [: 0 PASS ;] PRIM-HIR
   s" +!" [: ADDSTORE-BODY ;] PRIM
   s" c@" 1 1 [: HIR-OPCODE:BLOAD FETCH-HIR ;] PRIM-HIR
   s" c!" [: CSTORE-BODY ;] PRIM
   s" cells" 1 1 [: HIR:CELL-BYTES HIR-OPCODE:MUL WITH ;] PRIM-HIR
   s" cell+" 1 1 [: HIR:CELL-BYTES HIR-OPCODE:ADD WITH ;] PRIM-HIR
   s" chars" 1 1 [: 0 PASS ;] PRIM-HIR
   s" char+" 1 1 [: 1 HIR-OPCODE:ADD WITH ;] PRIM-HIR
   s" count" 1 2 [: COUNT-HIR ;] PRIM-HIR ;

\ The word model's float operations (hir-word.f DEF-FLOAT and DEF-FCOMPARE),
\ and f.
: FLOAT-ROWS, ( -- )
   s" f+" 2 1 [: HIR-OPCODE:FADD FBIN ;] PRIM-HIR
   s" f-" 2 1 [: HIR-OPCODE:FSUB FBIN ;] PRIM-HIR
   s" f*" 2 1 [: HIR-OPCODE:FMUL FBIN ;] PRIM-HIR
   s" f/" 2 1 [: HIR-OPCODE:FDIV FBIN ;] PRIM-HIR
   s" f<" 2 1 [: HIR-OPCODE:FLT FREL ;] PRIM-HIR
   s" f=" 2 1 [: HIR-OPCODE:FEQ FREL ;] PRIM-HIR
   s" f>" 2 1 [: HIR-OPCODE:FGT FREL ;] PRIM-HIR
   s" f0<" 1 1 [: HIR-OPCODE:FLTZ FREL0 ;] PRIM-HIR
   s" f0=" 1 1 [: HIR-OPCODE:FEQZ FREL0 ;] PRIM-HIR
   s" fabs" 1 1 [: HIR-OPCODE:FABS FUNARY ;] PRIM-HIR
   s" fnegate" 1 1 [: HIR-OPCODE:FNEG FUNARY ;] PRIM-HIR
   s" fsqrt" 1 1 [: HIR-OPCODE:FSQRT FUNARY ;] PRIM-HIR
   s" s>f" 1 1 [: S>F-HIR ;] PRIM-HIR
   s" f>s" 1 1 [: F>S-HIR ;] PRIM-HIR
   s" f." [: FDOT-BODY ;] PRIM ;

public

: PURE, ( -- )
   ARITH-ROWS,  COMPARE-ROWS,  STACK-ROWS,  MEMORY-ROWS,  FLOAT-ROWS, ;
\ ---- profiler rows -----------------------------------------------------------
\ habu1.f's profiler rows: src/habu/prof-x64.f emits the SIGALRM handler, its
\ restorer, the index helpers, the sync, the printers and the reports once,
\ then each body (docs/x86-64.md "Profiler rows").

private

\ prof-rate's guard, the twin of prof.f BPROF-RATE's: an interval below 0
\ throws PROF-ABI:E-PROF-RATE before X64PROF:RATE-BODY maps the arena or
\ stores the rate, so a caller that catches it keeps the rate it had.
: RATE-GUARD, ( -- )
   X64CODE:LBL {: ok:label :}
   RAX DSP CELL negate MOV-LOAD,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NS ok JCC,
   RAX PROF-ABI:E-PROF-RATE >IMM32 ASM-SINK ENC-MOV-RI32
   THROW,
   ok X64CODE:LBL, ;

public

: PROFILER, ( -- )
   X64PROF:HELPERS,
   s" prof-on" [: X64PROF:ON-BODY ;] PRIM
   s" prof-off" [: X64PROF:OFF-BODY ;] PRIM
   s" prof-reset" [: X64PROF:RESET-BODY ;] PRIM
   s" prof-rate" [: RATE-GUARD,  X64PROF:RATE-BODY ;] PRIM
   s" prof-pc>rec" [: X64PROF:PCREC-BODY ;] PRIM
   s" prof-report" [: X64PROF:REPORT-BODY ;] PRIM
   s" prof-json" [: X64PROF:JSON-BODY ;] PRIM
   s" prof-row" [: X64PROF:ROW-BODY ;] PRIM ;

\ A seeded provider needs a non-null code xt at its dispatch cell.
: PROVIDED-FILLED? ( n -- bool ) {: cell:n :}
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + CELL-VIEW @ {: pair:n :}
      pair 32 rshift {: meta:n :}
      pair $FFFFFFFF and cell =
      meta AOT-WINDOW:XTOFF-KIND-MASK and 0= and
      meta AOT-WINDOW:XTOFF-VALUE-MASK and 0<> and if unloop true exit then
   loop
   false ;

\ The whole kernel: the helpers, then every section, then the specification's
\ other half. ENGINE-PRIMS:COMPLETE dies 76 naming the first row of
\ src/habu/prims.f that KEEP? keeps and no section registered, as habu2.f
\ ENGINE-EMIT:EMIT-PRIMITIVE-SECTIONS does for ARM64, so a row this kernel
\ does not carry is a REFUSE, never an absence a captured program meets.
: KERNEL, ( -- )
   HELPERS,
   X64CODE:LBL LCLOSE-CELL !
   SYSCALLS,
   CONTROL,
   ATOMICS,
   PUBLICATION,
   DICT-SEARCH,
   ENGINE-STATE,
   FFI,
   TASK,
   DEFINITION,
   PURE,
   PROFILER,
   [: PROVIDED-FILLED? ;] ENGINE-PRIMS:COMPLETE ;

;using   \ X64LAYOUT
;using
;using
;using
;package
