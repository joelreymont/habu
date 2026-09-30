\ kernel-x64.f - the x86-64 engine's primitive bodies, package X64KERNEL.
\
\ The twins of src/habu/habu1.f's bodies for the rows of src/habu/prims.f, hand
\ written through X64ASM. PRIM registers a body in the shared registry
\ (src/habu/primitive-registry.f) as habu1.f FPRIM does, so the specification
\ gates hold on both targets, and the helpers the rows share are emitted once:
\ the span guard (PROT-SPAN), the narrow page flip LPROTREC, the task-live
\ exit LTASKLIVE, and the dictionary index with the one-wordlist search.
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
   SPAN-HELPER,  REC-HELPER,  LIVE-HELPER,  INDEX-HELPERS, ;

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

\ The whole kernel: the helpers, then every section.
: KERNEL, ( -- )
   HELPERS,
   CONTROL,
   ATOMICS,
   DICT-SEARCH, ;

;using
;using
;package
