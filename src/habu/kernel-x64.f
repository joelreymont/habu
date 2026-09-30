\ kernel-x64.f - the x86-64 engine's primitive bodies, package X64KERNEL.
\
\ The twins of src/habu/habu1.f's bodies for the rows of src/habu/prims.f, hand
\ written through X64ASM. PRIM registers a body in the shared registry
\ (src/habu/primitive-registry.f) as habu1.f FPRIM does, so the specification
\ gates hold on both targets, and the helpers the rows share are emitted once:
\ the span guard (PROT-SPAN), the narrow page flip LPROTREC and the task-live
\ exit LTASKLIVE.
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
public

\ The rc a primitive body this kernel lacks dies with: ENTRY-LABEL's at build
\ time and a REFUSE row's at run time. It is the rc habu1.f
\ ENGINE-EMIT:TARGET-UNKNOWN dies with when a target has no body at all.
76 constant REFUSE-RC

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
: SPAN-LBL ( -- label ) SPAN-CELL @ >LABEL ;
: REC-LBL ( -- label ) REC-CELL @ >LABEL ;
: LIVE-LBL ( -- label ) LIVE-CELL @ >LABEL ;

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

public

\ Emit the helpers, making their labels in this stream first: every section
\ that calls one follows.
: HELPERS, ( -- )
   LBL SPAN-CELL !  LBL REC-CELL !  LBL LIVE-CELL !
   SPAN-HELPER,  REC-HELPER,  LIVE-HELPER, ;

\ Each section's rows are tabled in docs/x86-64.md under the heading of its
\ name.

\ ---- syscall rows ------------------------------------------------------------

\ ---- control rows ------------------------------------------------------------
\ `evaluate` jumps into habu2.f's interpreter on ARM64, which the x86-64 engine
\ never has. Lane I provides it in Habu; until then its row refuses, and the
\ registration is what ENGINE-PRIMS:COMPLETE needs meanwhile.
: CONTROL, ( -- )
   s" evaluate" REFUSE ;

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

\ The whole kernel: the helpers, then every section.
: KERNEL, ( -- )
   HELPERS,
   CONTROL,
   ATOMICS, ;

;using
;using
;package
