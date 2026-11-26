\ kernel-x64.f - the x86-64 engine's primitive bodies (package X64KERNEL): the
\ twins of habu1.f's bodies for the rows of src/habu/prims.f, written through
\ X64ASM and registered in src/habu/primitive-registry.f as habu1.f FPRIM does.
\ The kernel object contains startup, helpers and primitive rows; the common
\ linker places it beside compiled Habu code in ELF text. REGION holds the
\ runtime dictionary and newly compiled code. Tests: test/x86-64-kernel-*.f
\ (the boot harness, over the same bodies) and test/compiler/x86-64-link-run.f.
\
\ spawn-fds's child blocks BLOCKED's signals, then setpgid(0, PGROUP) puts it in
\ the request's group. Every descriptor is marked close-on-exec, each source is
\ lifted to a fresh fd at or above the floor in the cell past the request, in a
\ mapped scratch array, before any slot is placed, and dup3 with no flags
\ places each one.

\ The x86-64 seam (src/os/linux-x86-64/sys.f) and its process emitters
\ (proc-watch.f, proc-control.f) load here globally, so this file loads before
\ any file that loads the seam into a private wordlist, as boot-x64.f does.
require src/core/exit-status.f
require src/lib/vector.f
require src/lib/string.f
require src/habu/prof-x64.f
require src/core/does-clause.f
require src/core/util.f
require src/core/sha256.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/address-cells.f
require src/habu/stack-abi.f
require src/habu/fnv.f
require src/habu/primitive-registry.f
require src/habu/boot-x64.f
require src/habu/crash-x64.f
require src/habu/data-claims.f
require src/habu/code-span.f
require src/habu/task-abi.f
require src/habu/spawn-abi.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/rt.f
require src/os/linux-x86-64/sys.f
require src/os/linux-x86-64/proc-watch.f
require src/os/linux-x86-64/proc-control.f
require src/os/linux/layout.f
require src/os/linux/elf.f
require src/compiler/fixed-body.f
require src/habu/fault-record.f
require src/habu/prof-abi.f

package X64KERNEL

public

variable FLOORREC-ENTRY
variable FLOORREC-PENDING
variable UNCAUGHT-ENTRY
: UNCAUGHT-LBL ( -- label ) UNCAUGHT-ENTRY @ >LABEL ;
: FLOORREC-LBL ( -- label ) FLOORREC-ENTRY @ >LABEL ;
: FLOORREC-NEW ( -- label )
   X64CODE:LBL dup LABEL>N FLOORREC-ENTRY !
   true FLOORREC-PENDING ! ;

\ The highest DP offset the heap rows admit, as habu1.f DP-CHECK bounds it.
LINUX-LAYOUT:DATA-SIZE PROF-CNT-BYTES - constant DP-CEILING

\ The DATA claims place that band for the host's DATA-SIZE, which differs from
\ the target's on a macOS host: check them with the target's band.
LINUX-LAYOUT:DATA-SIZE DATA-CLAIMS:BAND-ASSERT

\ Every CP is a multiple of the code slot.
16 constant CODE-SLOT

private

2 constant STDERR

: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: SHARED-DATA, ( X64ASM:r64 -- ) LINUX-LAYOUT:DATA-VA VA>N X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;

: CELL@, ( X64ASM:r64 n -- )
   {: r:X64ASM:r64 off:n :}
   r DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;
: CELL!, ( X64ASM:r64 n -- )
   {: r:X64ASM:r64 off:n :}
   r DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ mov r32, imm32, which zero-extends: every constant here is a small
\ non-negative count, descriptor, prot or status.
: IMM32, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 v:n :}
   r X64ASM:R64>N X64ASM:>R32 v X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32 ;

: EXIT-GROUP, ( n -- ) {: rc:n :} X64ASM:RDI rc IMM32,  NR-EXIT-GROUP SYS, ;

: SAME? ( X64ASM:r64 X64ASM:r64 -- bool ) X64ASM:R64>N swap X64ASM:R64>N = ;

\ The helpers' labels, set by HELPERS, in the stream it emits them into.
variable LIVE-CELL
variable DPBAD-CELL
variable OVERLAY-DIE-CELL
variable SCOPE-SEED-CELL
variable SCOPE-SEED-END-CELL
variable SCOPE-KIND-CELL
: LIVE-LBL ( -- label ) LIVE-CELL @ >LABEL ;
: DPBAD-LBL ( -- label ) DPBAD-CELL @ >LABEL ;
: OVERLAY-DIE-LBL ( -- label ) OVERLAY-DIE-CELL @ >LABEL ;
: SCOPE-SEED-LBL ( -- label ) SCOPE-SEED-CELL @ >LABEL ;
: SCOPE-SEED-END-LBL ( -- label ) SCOPE-SEED-END-CELL @ >LABEL ;
: SCOPE-KIND-LBL ( -- label ) SCOPE-KIND-CELL @ >LABEL ;
: EXIT-HOOK-LBL ( -- label ) X64BOOT:LEXITHOOK @ >LABEL ;

\ A BODY'S CONTRACT. rbp is DATA, rbx the data stack, r13-r15 the other VM
\ registers and r12 the loop index, which any callee may write; rax rcx rdx rsi
\ rdi and r8-r11 are scratch. A body is entered by `call` and leaves by the
\ `ret` PRIM appends, so it calls a helper without a frame: one definer serves
\ where habu1.f needs FPRIM and FPRIM-L.
\
\ The row being registered. Every body registers under a name src/habu/prims.f
\ specifies: SPEC-CHECK checks each body, and ENGINE-PRIMS:COMPLETE rejects a
\ specified row that no body answered.
defer BODY ( -- )
PTR-VARIABLE ROW-A
variable ROW-U
: ROW$ ( -- str ) ROW-A @ ROW-U @ >str ;

: ARGS ( str [ -- ] -- )
   is BODY  {: name:str :}
   name STR:DATA ROW-A !  name STR:LEN ROW-U !
   ROW$ ENGINE-PRIMS:SPEC-CHECK ;

\ The record spans [first, last): the body and the ret after it.
: EMIT-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: first:label last:label :}
   ROW$ first last ENGINE-PRIMS:ADD drop
   first X64CODE:LBL,  BODY  X64CODE:RET,  last X64CODE:LBL,  X64CODE:DATA-AFTER, ;

: STDERR-WRITE, ( label n -- ) {: msg:label len:n :}
   X64ASM:RDI STDERR IMM32,  X64ASM:RSI msg X64CODE:MOVABS,  X64ASM:RDX len IMM32,  NR-WRITE SYS, ;

: STDERR-EXIT, ( str n -- ) {: t:str rc:n :}
   X64CODE:LBL {: msg:label :}
   msg t STR:LEN STDERR-WRITE,
   rc EXIT-GROUP,
   msg t X64CODE:DATA-AFTER ;

public

: PRIM ( str [ -- ] -- )
   ARGS
   EMIT-ROW ;

: ENTRY-LABEL ( str -- label ) {: name:str :}
   ENGINE-PRIMS:COUNT 0 ?do
      i ENGINE-PRIMS:NAME$ name STR:= if
         i ENGINE-PRIMS:FIRST-LABEL unloop exit
      then
   loop
   s" x64kernel: no registered body named " type name type cr
   s" x64kernel: ENTRY-LABEL names no registered body" EXIT-STATUS:INTERNAL die ;

\ The live-task retreat guard, also exercised by the bare kernel fixture.
\ Ordinary writers check thread ownership instead. Clobbers rax and flags.
: TASK-LIVE-GUARD, ( -- )
   X64ASM:RAX DATA-REG TASKS-LIVE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR
   X64ASM:C-NE LIVE-LBL X64CODE:JCC, ;

\ An RX address becomes its RW alias for an engine store. Addresses
\ stored in records, code and DATA remain RX identities. No scratch register
\ is needed and no page changes protection while a task can execute it.
: WRITE-ALIAS, ( X64ASM:r64 -- )
   DATA-REG REGION-ALIAS:DELTA-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM ;

private

\ The twin of habu1.f OVERLAY-EMIT:GUARD,, which states the rule. It clobbers
\ rax and the flags.
: OVERLAY-GUARD, ( -- )
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE OVERLAY-DIE-LBL X64CODE:JCC, ;

: LIVE-HELPER, ( -- )
   LIVE-LBL X64CODE:LBL,
   EXIT-STATUS:TASK-COMPILE EXIT-GROUP, ;

\ The machine-stack frame DIAG-U, writes digits into: room for any cell's.
CELL 4 * constant DIAG-BYTES

\ rax as unsigned decimal on fd 2, no newline: the twin of habu2.f LDIAGU,
\ which a rejection with a count to state calls instead of `u.`, since a
\ diagnostic never leaves through the output device. It clobbers rax rcx rdx
\ rsi rdi r11 and keeps r8.
: DIAG-U, ( -- )
   X64ASM:RSP DIAG-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RSI X64ASM:RSP DIAG-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64RT:DIGITS,
   X64ASM:RDX X64ASM:RSP DIAG-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDI STDERR IMM32,  NR-WRITE SYS,
   X64ASM:RSP DIAG-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ A code address is always sixteen hexadecimal digits on the error output.
: DIAG-HEX, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label digit:label :}
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RSI X64ASM:RSP 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RCX 16 IMM32,
   next X64CODE:LBL,
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX 15 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:RDX [char] 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RDX [char] 9 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-BE digit X64CODE:JCC,
   X64ASM:RDX 39 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   digit X64CODE:LBL,
   X64ASM:RDX X64ASM:R64>N X64ASM:>R8 X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:RDI STDERR IMM32,  X64ASM:RDX 16 IMM32,  NR-WRITE SYS,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ Only an input buffer inside the file's owned bytes has a file line.
: SOURCE-LINE, ( label label -- )
   {: noline:label colon:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: scan:label next:label line:label :}
   X64ASM:R9 SRCLOC:OWNED-CELL CELL@,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E noline X64CODE:JCC,
   X64ASM:R10 SRCLOC:INB-CELL CELL@,
   X64ASM:R10 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B noline X64CODE:JCC,
   X64ASM:R10 SRCLOC:OWNEDLEN-CELL CELL@,
   X64ASM:R10 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX INE-CELL CELL@,
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A noline X64CODE:JCC,
   X64ASM:R10 INP-CELL CELL@,  X64ASM:R8 1 IMM32,
   scan X64CODE:LBL,
   X64ASM:R9 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE line X64CODE:JCC,
   X64ASM:RAX X64ASM:R9 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX STR:LF X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC
   next X64CODE:LBL,  X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-INC  scan X64CODE:JMP,
   line X64CODE:LBL,
   colon 1 STDERR-WRITE,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  DIAG-U, ;

: SOURCE-PLACE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: done:label noline:label colon:label :}
   X64ASM:RDX SRCLOC:PATHLEN-CELL CELL@,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RSI SRCLOC:PATH-CELL CELL@,
   X64ASM:RDI STDERR IMM32,  NR-WRITE SYS,
   noline colon SOURCE-LINE,
   noline X64CODE:LBL,  colon 2 STDERR-WRITE,
   colon s" : " X64CODE:DATA-AFTER
   done X64CODE:LBL, ;

\ (GENIO-OUT) ( rdi = device index, rsi = span, rdx = length ): the output
\ funnel's device arm at X64RT's LGENIOOUT, the twin of habu2.f EMIT-GENIO-OUT.
\ It writes fd 1 while GENIO-ABI:BUSY-CELL is set, for an index past DEVICES
\ (unsigned, so a negative one too) and for an empty row; otherwise it calls row
\ index-1's xt with the funnel busy, so a device's own output reaches fd 1.
\ Registered as an engine helper, as EMIT-GENIO-OUT registers the ARM64 arm.
: GENIO-HELPER, ( -- )
   X64RT:LGENIOOUT @ >LABEL {: start:label :}
   X64CODE:LBL X64CODE:LBL {: term:label end:label :}
   s" (GENIO-OUT)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start X64CODE:LBL,
   X64ASM:RAX DATA-REG GENIO-ABI:BUSY-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE term X64CODE:JCC,
   X64ASM:RDI GENIO-ABI:DEVICES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A term X64CODE:JCC,
   X64ASM:RAX DATA-REG X64ASM:RDI CELL GENIO-ABI:WRITE-OFF CELL - X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E term X64CODE:JCC,
   X64ASM:RCX DATA-REG GENIO-ABI:ACTIVE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDI DATA-REG GENIO-ABI:ACTIVE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX 1 IMM32,  X64ASM:RCX DATA-REG GENIO-ABI:BUSY-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI X64ASM:R64>N X64RT:G-PUSH  X64ASM:RDX X64ASM:R64>N X64RT:G-PUSH
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RCX ZERO-REG,  X64ASM:RCX DATA-REG GENIO-ABI:BUSY-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP
   X64ASM:RCX DATA-REG GENIO-ABI:ACTIVE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64CODE:RET,
   term X64CODE:LBL,
   X64ASM:RDI 1 IMM32,  NR-WRITE SYS,
   X64CODE:RET,
   end X64CODE:LBL, ;

\ The index at HIDXP-CELL: HIDX-SLOTS u32 slots, each 0 or a record index + 1,
\ keyed on the folded name's hash and the wid. A re-key leaves the old slot.

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

$41 constant FOLD-FIRST                \ A
$5A constant FOLD-LAST                 \ Z
$20 constant FOLD-BIT
3 constant PROT-RW                     \ PROT_READ|PROT_WRITE
5 constant PROT-RX                     \ PROT_READ|PROT_EXEC

: DBASE-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DBASE X64ASM:>R64 ;
: NDICT-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-NDICT X64ASM:>R64 ;
: CP-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-CP X64ASM:>R64 ;

\ When the cell is nonzero, an E-TASK-COMPILE record naming the row, its note
\ `in` the word whose code holds [rsp], and the exit; else only rax and flags
\ change. It runs before the row's first call or machine-stack push, so [rsp]
\ is the caller's return address.
: MUTATION-GUARD, ( str str str n -- )
   {: p:str r:str h:str cell:n :}
   X64CODE:LBL {: ok:label :}
   X64ASM:RAX cell CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E ok X64CODE:JCC,
   s" E-TASK-COMPILE"  p r FAULT-RECORD:GUARD-MSG$  X64CRASH:FAULT-OPEN,
   X64ASM:RDI X64ASM:RSP FAULT-RECORD:FAULT-FRAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64CRASH:FAULT-IN,
   h EXIT-STATUS:TASK-COMPILE X64CRASH:FAULT-STOP,
   ok X64CODE:LBL, ;

\ The main thread alone defines and allots: a task's or callback's DATA holds
\ a TCB in TASK-TCB-CELL.
: TASK-GUARD, ( str -- )
   FAULT-RECORD:TASK-GUARD$ FAULT-RECORD:TASK-GUARD-HELP$
   TASK-TCB-CELL MUTATION-GUARD, ;

variable FIND-CELL
variable FIND-PAIR-CELL
variable FIND-SCAN-CELL
variable BUILD-CELL
variable ADD-CELL
variable REKEY-CELL
variable REBUILD-CELL
variable FULL-CELL
variable HEAD-ROW-CELL
: FIND-LBL ( -- label ) FIND-CELL @ >LABEL ;
: FIND-PAIR-LBL ( -- label ) FIND-PAIR-CELL @ >LABEL ;
: FIND-SCAN-LBL ( -- label ) FIND-SCAN-CELL @ >LABEL ;
: BUILD-LBL ( -- label ) BUILD-CELL @ >LABEL ;
: ADD-LBL ( -- label ) ADD-CELL @ >LABEL ;
: REKEY-LBL ( -- label ) REKEY-CELL @ >LABEL ;
: REBUILD-LBL ( -- label ) REBUILD-CELL @ >LABEL ;
: FULL-LBL ( -- label ) FULL-CELL @ >LABEL ;
: HEAD-ROW-LBL ( -- label ) HEAD-ROW-CELL @ >LABEL ;

: IMM64, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;

: FOLD, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64CODE:LBL {: done:label :}
   r FOLD-FIRST X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-B done X64CODE:JCC,
   r FOLD-LAST X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A done X64CODE:JCC,
   r FOLD-BIT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-OR-RI8
   done X64CODE:LBL, ;

: HASH-START, ( X64ASM:r64 X64ASM:r64 -- ) {: h:X64ASM:r64 prime:X64ASM:r64 :}
   h FNV:BASIS IMM64,
   prime FNV:PRIME IMM64, ;

: HASH-STEP, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 -- ) {: h:X64ASM:r64 byte:X64ASM:r64 prime:X64ASM:r64 :}
   byte FOLD,
   h byte X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   h prime X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR ;

: HASH-SPAN, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 -- )
   {: name:X64ASM:r64 len:X64ASM:r64 h:X64ASM:r64 cur:X64ASM:r64 byte:X64ASM:r64 prime:X64ASM:r64 :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   cur ZERO-REG,
   next X64CODE:LBL,
   cur len X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE done X64CODE:JCC,
   byte name cur 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   h byte prime HASH-STEP,
   cur X64CODE:ASM-SINK X64ASM:ENC-INC
   next X64CODE:JMP,
   done X64CODE:LBL, ;

\ h = the FNV-1a hash of the folded name.
: HASH, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 -- )
   {: name:X64ASM:r64 len:X64ASM:r64 h:X64ASM:r64 cur:X64ASM:r64 byte:X64ASM:r64 prime:X64ASM:r64 :}
   h prime HASH-START,
   name len h cur byte prime HASH-SPAN, ;

\ h = the hash of `first:name`, of name alone when flen is 0.
: HASH-PAIR, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 -- )
   {: first:X64ASM:r64 flen:X64ASM:r64 name:X64ASM:r64 len:X64ASM:r64 h:X64ASM:r64 cur:X64ASM:r64 byte:X64ASM:r64 prime:X64ASM:r64 :}
   X64CODE:LBL {: second:label :}
   h prime HASH-START,
   first flen h cur byte prime HASH-SPAN,
   flen flen X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E second X64CODE:JCC,
   byte $3A IMM32,  h byte prime HASH-STEP,
   second X64CODE:LBL,
   name len h cur byte prime HASH-SPAN, ;

\ h = the first slot of hash h's chain in wid; clobbers tmp.
: SLOT, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 -- ) {: h:X64ASM:r64 wid:X64ASM:r64 tmp:X64ASM:r64 :}
   h wid X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   tmp HIDX-MIX IMM64,
   h tmp X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR
   h 64 HIDX-BITS - X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8 ;

: NEXT-SLOT, ( X64ASM:r64 -- ) {: slot:X64ASM:r64 :}
   slot X64CODE:ASM-SINK X64ASM:ENC-INC
   slot HIDX-SLOTS 1- X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32 ;

: SLOT@, ( X64ASM:r64 X64ASM:r64 -- ) {: dst:X64ASM:r64 slot:X64ASM:r64 :}
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   dst X64ASM:R64>N X64ASM:>R32 X64ASM:RAX slot 4 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM ;

: ROW, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r r DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   r DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR ;

: NAME-LEN, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 -- ) {: row:X64ASM:r64 dst:X64ASM:r64 tmp:X64ASM:r64 :}
   dst row REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   tmp DNAME-LEN-MASK IMM64,
   dst tmp X64CODE:ASM-SINK X64ASM:ENC-AND-RR ;

: NAME-AT, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 -- ) {: row:X64ASM:r64 dst:X64ASM:r64 tmp:X64ASM:r64 :}
   X64CODE:LBL {: inline:label :}
   dst row REC-NAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   tmp DNAME-EXT IMM64,
   tmp row REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-TEST-MR  X64ASM:C-E inline X64CODE:JCC,
   dst row REC-NAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   inline X64CODE:LBL, ;

\ A folded compare of `len` bytes: a mismatch jumps to `miss`, a match falls
\ through.
: SAME-NAME, ( X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 X64ASM:r64 label -- )
   {: a:X64ASM:r64 b:X64ASM:r64 len:X64ASM:r64 cur:X64ASM:r64 x:X64ASM:r64 y:X64ASM:r64 miss:label :}
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   cur ZERO-REG,
   next X64CODE:LBL,
   cur len X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE done X64CODE:JCC,
   x a cur 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM  x FOLD,
   y b cur 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM  y FOLD,
   x y X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE miss X64CODE:JCC,
   cur X64CODE:ASM-SINK X64ASM:ENC-INC
   next X64CODE:JMP,
   done X64CODE:LBL, ;

\ An entry's end ( rax = answer ): a global miss asks WL:PRIMITIVES by `entry`.
: RETRY, ( label -- ) {: entry:label :}
   X64CODE:LBL {: done:label :}
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE done X64CODE:JCC,
   X64ASM:RDX DATA-REG BOOT:GLOBAL-WL-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE done X64CODE:JCC,
   X64ASM:RDX WL:PRIMITIVES IMM32,
   entry X64CODE:CALL,
   X64ASM:RDX BOOT:GLOBAL-WL-CELL CELL@,
   done X64CODE:LBL,
   X64CODE:RET, ;

\ Jumps to `miss` unless record rcx's name is r9 + 1 + rsi bytes long, rsi
\ when r9 is 0. Clobbers rax and r10.
: NAME-LEN-SAME, ( label -- ) {: miss:label :}
   X64CODE:LBL {: whole:label :}
   X64ASM:RCX X64ASM:RAX X64ASM:R10 NAME-LEN,
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E whole X64CODE:JCC,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC
   whole X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE miss X64CODE:JCC, ;

\ The name at r11 against r8's first r9 bytes, `:`, then rdi/rsi (rdi/rsi alone
\ when r9 is 0); its length is already checked. Clobbers rax, rcx, r10 and r11.
: SAME-PAIR, ( label -- ) {: miss:label :}
   X64CODE:LBL {: second:label :}
   X64ASM:R8 X64ASM:R11 X64ASM:R9 X64ASM:RCX X64ASM:RAX X64ASM:R10 miss SAME-NAME,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E second X64CODE:JCC,
   X64ASM:RAX X64ASM:R11 X64ASM:R9 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX $3A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE miss X64CODE:JCC,
   X64ASM:R11 X64ASM:R11 X64ASM:R9 1 1 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   second X64CODE:LBL,
   X64ASM:RDI X64ASM:R11 X64ASM:RSI X64ASM:RCX X64ASM:RAX X64ASM:R10 miss SAME-NAME, ;

\ Record rcx against the name sought (r8/r9, rdi/rsi as SAME-PAIR, reads
\ them): a match falls through, a mismatch jumps to `miss`, rcx kept either
\ way. Clobbers rax, r10 and r11.
: SAME-ROW, ( label -- ) {: miss:label :}
   X64CODE:LBL X64CODE:LBL {: bad:label hit:label :}
   miss NAME-LEN-SAME,
   X64ASM:RCX X64ASM:R11 X64ASM:RAX NAME-AT,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   bad SAME-PAIR,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  hit X64CODE:JMP,
   bad X64CODE:LBL,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  miss X64CODE:JMP,
   hit X64CODE:LBL, ;

\ The hash probe: the first live row in the name's chain; an empty slot
\ answers absent and a chain with none takes the scan.
: PROBE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: probe:label miss:label next:label absent:label :}
   X64ASM:R8 X64ASM:R9 X64ASM:RDI X64ASM:RSI X64ASM:R10 X64ASM:RCX X64ASM:RAX X64ASM:R11 HASH-PAIR,
   X64ASM:R10 X64ASM:RDX X64ASM:RAX SLOT,                                   \ r10 = the slot
   X64ASM:R11 HIDX-SLOTS IMM32,                               \ r11 = slots left to walk
   probe X64CODE:LBL,
   X64ASM:RCX X64ASM:R10 SLOT@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E absent X64CODE:JCC,       \ an empty slot
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE next X64CODE:JCC,   \ a stale, rolled back one
   X64ASM:RCX ROW,
   X64ASM:RDX X64ASM:RCX REC-WID X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-PUSH        \ SAME-ROW, clobbers them
   miss SAME-ROW,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64CODE:RET,
   miss X64CODE:LBL,
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-POP
   next X64CODE:LBL,
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-E FIND-SCAN-LBL X64CODE:JCC,
   X64ASM:R10 NEXT-SLOT,
   probe X64CODE:JMP,
   absent X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   FIND-PAIR-LBL RETRY, ;

\ The scan: the last matching row, kept at [rsp] while rcx walks the records.
: SCAN, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: row:label skip:label done:label :}
   FIND-SCAN-LBL X64CODE:LBL,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RCX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   row X64CODE:LBL,
   X64ASM:RAX NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,         \ past the last record
   X64ASM:RDX X64ASM:RCX REC-WID X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE skip X64CODE:JCC,
   skip SAME-ROW,
   X64ASM:RCX X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   skip X64CODE:LBL,
   X64ASM:RCX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   row X64CODE:JMP,
   done X64CODE:LBL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP
   FIND-SCAN-LBL RETRY, ;

\ WLFIND:LENTRY's twin, by its rules: in rdi = name, rsi = length, rdx = wid;
\ out rax = the record, code cell first, or 0. FIND-PAIR-LBL is
\ WLFIND:LPAIR's twin: the name `first:second` without composing it, r8 =
\ first, r9 = its length (not 0), rdi/rsi = second. Both keep rdi, rsi, rdx
\ and r8, and clobber rcx, r10 and r11; FIND-LBL zeroes r9, the pair keeps it.
: FIND-HELPER, ( -- )
   FIND-LBL X64CODE:LBL,
   X64ASM:R9 ZERO-REG,
   FIND-PAIR-LBL X64CODE:LBL,
   X64ASM:RDX WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E FIND-SCAN-LBL X64CODE:JCC,
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E FIND-SCAN-LBL X64CODE:JCC,
   PROBE,  SCAN, ;

\ LHEADROW's twin ( rdi = head, rsi = its length ): rax = the namespace row
\ the head names, or 0. Inside package A, or a part of it, that is A's part
\ `A:head` first, then a top-level package. It keeps rdi and rsi and clobbers
\ rcx, rdx and r8-r11.
: HEAD-ROW-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: top:label bare:label done:label :}
   HEAD-ROW-LBL X64CODE:LBL,
   X64ASM:RDX BOOT:NAMESPACE-WL-CELL CELL@,
   X64ASM:RAX DATA-REG ENCLOSING:REC X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE top X64CODE:JCC,
   X64ASM:RAX DATA-REG PKG-REC-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bare X64CODE:JCC,
   top X64CODE:LBL,
   X64ASM:RAX X64ASM:R9 X64ASM:RCX NAME-LEN,                                \ r9 = A's name length
   X64ASM:RAX X64ASM:R8 X64ASM:RCX NAME-AT,                                 \ r8 = A's name bytes
   FIND-PAIR-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE done X64CODE:JCC,
   bare X64CODE:LBL,
   FIND-LBL X64CODE:JMP,                                       \ its ret is this one's
   done X64CODE:LBL,
   X64CODE:RET, ;

\ r8 = the first slot of record rdi's chain, r9 = the most slots a walk of it
\ visits. It keeps rdi and clobbers rax rcx rdx rsi r10 r11.
: CHAIN, ( -- )
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RAX ROW,
   X64ASM:RCX X64ASM:RAX REC-WID X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RDX X64ASM:R8 NAME-LEN,
   X64ASM:RAX X64ASM:RSI X64ASM:R8 NAME-AT,
   X64ASM:RSI X64ASM:RDX X64ASM:R8 X64ASM:R9 X64ASM:R10 X64ASM:R11 HASH,
   X64ASM:R8 X64ASM:RCX X64ASM:R10 SLOT,
   X64ASM:R9 HIDX-SLOTS IMM32, ;

\ Index record rdi at the first empty or stale slot of its chain; meeting rdi
\ before either writes nothing. Keeps rdi, clobbers rax rcx rdx rsi r8-r11.
: INSERT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: probe:label empty:label put:label done:label :}
   CHAIN,
   probe X64CODE:LBL,
   X64ASM:R10 X64ASM:R8 SLOT@,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E empty X64CODE:JCC,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:R10 X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R10 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE put X64CODE:JCC,    \ stale: reuse its claim
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-E FULL-LBL X64CODE:JCC,
   X64ASM:R8 NEXT-SLOT,
   probe X64CODE:JMP,
   empty X64CODE:LBL,
   X64ASM:R10 DATA-REG HIDX:CLAIMS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:R10 DATA-REG HIDX:CLAIMS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   put X64CODE:LBL,
   X64ASM:R10 X64ASM:RDI 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R10 X64ASM:R64>N X64ASM:>R32 X64ASM:RAX X64ASM:R8 4 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR
   done X64CODE:LBL, ;

\ Branch to `held` when rdi's chain holds rdi before its first empty slot, as a
\ probe walks it; else fall through. Keeps rdi, clobbers rax rcx rdx rsi r8-r11.
: HELD, ( label -- ) {: held:label :}
   X64CODE:LBL X64CODE:LBL {: probe:label none:label :}
   CHAIN,
   probe X64CODE:LBL,
   X64ASM:R10 X64ASM:R8 SLOT@,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:R10 X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E held X64CODE:JCC,
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-E none X64CODE:JCC,
   X64ASM:R8 NEXT-SLOT,
   probe X64CODE:JMP,
   none X64CODE:LBL, ;

\ HIDX-EMIT:LREBUILD's twin: zero the table and HIDX:CLAIMS, then index the live
\ records [0, r14). With no table it returns at once. NDICT <= DICT-CAP <
\ HIDX:LOAD-MAX, so the rebuilt table stays under the bound.
: REBUILD-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: zero:label fill:label done:label :}
   REBUILD-LBL X64CODE:LBL,
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RCX ZERO-REG,                                       \ rcx = the byte offset
   X64ASM:RDX ZERO-REG,
   zero X64CODE:LBL,
   X64ASM:RDX X64ASM:RAX X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RCX HIDX-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B zero X64CODE:JCC,
   X64ASM:RDX DATA-REG HIDX:CLAIMS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI ZERO-REG,                                       \ rdi = the record
   fill X64CODE:LBL,
   X64ASM:RDI NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE done X64CODE:JCC,
   INSERT,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC
   fill X64CODE:JMP,
   done X64CODE:LBL,
   X64CODE:RET, ;

\ ADD-LBL indexes the record just published, r14 - 1; REKEY-LBL indexes record
\ rdi after a store to its wid cell.
: ADD-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL {: insert:label done:label :}
   ADD-LBL X64CODE:LBL,
   X64ASM:RDI NDICT-REG -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   insert X64CODE:LBL,
   INSERT,
   X64ASM:RAX DATA-REG HIDX:CLAIMS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX HIDX:LOAD-MAX X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-L done X64CODE:JCC,
   REBUILD-LBL X64CODE:CALL,
   done X64CODE:LBL,
   X64CODE:RET,
   REKEY-LBL X64CODE:LBL,
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   done HELD,
   insert X64CODE:JMP, ;

\ rax = rsi fresh bytes, or carry set.
: MAP-ANON, ( -- )
   X64ASM:RDI ZERO-REG,  X64ASM:RDX PROT-RW IMM32,  X64ASM:R10 MAP-ANON-PRIVATE IMM32,
   X64ASM:R8 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS, ;

\ LHIDXBUILD's twin: map the table once, HIDX-BYTES of fresh zero pages with no
\ claims, then rebuild it; a table already mapped is refilled in place.
: BUILD-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL {: have:label fail:label :}
   BUILD-LBL X64CODE:LBL,
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE have X64CODE:JCC,
   X64ASM:RSI HIDX-BYTES IMM32,  MAP-ANON,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RAX DATA-REG HIDXP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX ZERO-REG,
   X64ASM:RCX DATA-REG HIDX:CLAIMS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   have X64CODE:LBL,
   REBUILD-LBL X64CODE:JMP,                                    \ its ret is this one's
   fail X64CODE:LBL,
   s" E-DICT-INDEX habu: dictionary index alloc failed" EXIT-STATUS:SYSTEM
   X64CRASH:DIE-LINE,  X64CODE:DATA-AFTER, ;

\ HIDX-EMIT:LFULL's twin: the index cannot be kept, which is loud, never a quiet
\ fall back to the scan.
: FULL-HELPER, ( -- )
   FULL-LBL X64CODE:LBL,
   s" E-DICT-INDEX habu: dictionary index exhausted" EXIT-STATUS:INTERNAL X64CRASH:DIE-LINE,
   X64CODE:DATA-AFTER, ;

: INDEX-HELPERS, ( -- )
   X64CODE:LBL FIND-CELL !  X64CODE:LBL FIND-PAIR-CELL !
   X64CODE:LBL FIND-SCAN-CELL !
   X64CODE:LBL BUILD-CELL !  X64CODE:LBL ADD-CELL !  X64CODE:LBL REBUILD-CELL !
   X64CODE:LBL REKEY-CELL !
   X64CODE:LBL FULL-CELL !  X64CODE:LBL HEAD-ROW-CELL !
   FIND-HELPER,  HEAD-ROW-HELPER,  BUILD-HELPER,  ADD-HELPER,  REBUILD-HELPER,  FULL-HELPER, ;

public

\ The index's call sites, the twins of habu1.f's LHIDXBUILD, LHIDXADD and
\ HIDX-EMIT:LREBUILD calls, for the rows that move r14: build the table (a rejected
\ mapping exits 74 with `E-DICT-INDEX habu: dictionary index alloc failed`),
\ index the record just published, and rebuild the table over [0, r14). Each
\ call keeps every VM register and clobbers rax rcx rdx rsi rdi and r8-r11; one
\ that cannot keep the index exits 89 with `E-DICT-INDEX habu: dictionary index
\ exhausted`.
: HIDX-BUILD, ( -- ) BUILD-LBL X64CODE:CALL, ;
: HIDX-ADD, ( -- ) ADD-LBL X64CODE:CALL, ;
: HIDX-REBUILD, ( -- ) REBUILD-LBL X64CODE:CALL, ;

\ The twins of habu1.f's file, memory, time and identity bodies. A row pops its
\ arguments into the syscall registers, rdi rsi rdx r10 r8 r9 (the order
\ src/os/linux-x86-64/sys.f maps x0..x5 onto), stages the *at* family's
\ AT_FDCWD and flags, traps through SYS, and pushes through X64RT:SYS-PUSH: the
\ result, or -1 when the call failed.

private

1000000000 constant NS-PER-S

: DSP ( -- X64ASM:r64 ) ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;
: INDEX-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-INDEX X64ASM:>R64 ;
: POP, ( X64ASM:r64 -- ) X64ASM:R64>N X64RT:G-POP ;
: PUSH, ( X64ASM:r64 -- ) X64ASM:R64>N X64RT:G-PUSH ;
: DROP, ( -- ) DSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8 ;
: AT-FDCWD, ( X64ASM:r64 -- ) AT-FDCWD X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32 ;
: SYS-PUSH, ( n -- ) SYS, X64RT:SYS-PUSH ;
: FS-SYS-PUSH, ( n -- ) SYS, X64RT:FS-SYS-PUSH ;

: PEEK, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 depth:n :}
   r DSP depth 1+ CELL * negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;

: OPEN-ROW ( -- )                   \ ( path flags mode -- fd|-1 )
   X64ASM:R10 POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   OS-OPEN-FLAGS                    \ rsi's BSD-shaped flags into rdx
   X64ASM:RSI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI AT-FDCWD,
   NR-OPEN FS-SYS-PUSH, ;

\ Fresh anonymous storage: ( bytes -- ptr ior ), a null pointer and -1 for a
\ length that is not positive or a mapping that fails.
: MAP-ANON-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   X64ASM:RSI POP,
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE failed X64CODE:JCC,
   MAP-ANON,  X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RCX ZERO-REG,  done X64CODE:JMP,
   failed X64CODE:LBL,
   X64ASM:RAX ZERO-REG,  X64ASM:RCX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   done X64CODE:LBL,
   0 X64RT:G-PUSH  1 X64RT:G-PUSH ;

\ ( addr len prot flags fd off -- addr|-1 ).
: MMAP-ROW ( -- )
   X64ASM:R9 POP,  X64ASM:R8 POP,  X64ASM:R10 POP,  X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   OS-MMAP-FLAGS
   NR-MMAP SYS-PUSH, ;

\ newfstatat writes the x86-64 struct stat, whose st_mode sits at 24 behind
\ three u64s where aarch64's sits at 16; the size and the two times sit where
\ aarch64's do. The fix rewrites the layout src/lib/fs.f reads, as habu1.f
\ LINUX-STAT-FIX does: the mode at 4, the modification time at 48 and 56, the
\ change time at 64 and 72 and the size at 96. Every field is loaded before
\ one is stored, since the size and the modification time trade places.
: STAT-FIX, ( X64ASM:r64 -- ) {: buf:X64ASM:r64 :}
   1 X64ASM:>R32 buf 24 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   1 X64ASM:>R32 buf 4 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR
   X64ASM:RSI buf 48 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI buf 88 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 buf 96 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 buf 104 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 buf 112 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI buf 96 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI buf 48 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 buf 56 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R9 buf 64 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R10 buf 72 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ ( path buf -- 0|-1 ) through newfstatat with the flags n. The buffer is still
\ in rdx after the trap, and the fix runs only when the row pushed 0.
: STAT-ROW ( n n -- ) {: nr:n flags:n :}
   X64CODE:LBL {: done:label :}
   X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI AT-FDCWD,  X64ASM:R10 flags IMM32,
   nr FS-SYS-PUSH,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE done X64CODE:JCC,
   X64ASM:RDX STAT-FIX,
   done X64CODE:LBL, ;

: GTOD ( n -- X64ASM:mem ) {: off:n :} DATA-REG GTOD-SCRATCH off + X64ASM:MEM-OFF ;

\ A time call cannot fail with the arguments these rows give it. One that does
\ stops on ud2, as habu1.f's twins stop on brk.
: TIME-CHECK, ( -- )
   X64CODE:LBL {: ok:label :}
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E ok X64CODE:JCC,
   X64CODE:UD2,
   ok X64CODE:LBL, ;

: EPOCH-ROW ( -- )                  \ ( -- seconds ) gettimeofday's seconds
   X64ASM:RDI 0 GTOD X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RSI ZERO-REG,
   NR-GETTIMEOFDAY SYS,  TIME-CHECK,
   X64ASM:RAX 0 GTOD X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  0 X64RT:G-PUSH ;

\ RAX receives the named import's address from this run's GOT.
\ The read-write segment starts the text's size past the image base.
: LIBC-ENTRY, ( str -- )
   ELF:IMPORT-ROW ELF:SLOT-OFF {: slot:n :}
   X64ASM:RAX DATA-REG RBASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX LINUX-LAYOUT:CODE-OFF X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RAX X64ASM:RAX LINUX-LAYOUT:IMAGE-TEXT-SIZE-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX X64ASM:RAX slot X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;

public

\ Call the C function at r11 under the SysV ABI: the register arguments already
\ in rdi rsi rdx rcx r8 r9 and xmm0..7, and the r10 cells at rax (a count >= 0)
\ as its stack arguments; the answer is in rax/rdx or xmm0. al is set to n, the
\ bound on vector arguments a variadic callee reads to decide whether it saves
\ xmm0..7. The entry rsp waits in r12, so a fault in the copy throws with the
\ data stack pointer in its register. The other VM registers survive by the
\ callee-saved rule; rax rcx rdx rsi rdi, r8-r12 and the XMM registers do not.
\ No private cell occupies the published Habu data stack while C calls back.
: SYSV-CALL, ( n -- ) {: vecs:n :}
   X64CODE:LBL X64CODE:LBL {: copy:label called:label :}
   INDEX-REG X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:R11 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R11 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RSP X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RSP -16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E called X64CODE:JCC,
   copy X64CODE:LBL,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:R11 X64ASM:RAX X64ASM:R10 CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R11 X64ASM:RSP X64ASM:R10 CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:C-NE copy X64CODE:JCC,
   called X64CODE:LBL,
   X64ASM:R11 INDEX-REG CELL negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  \ the function below the entry rsp
   X64ASM:RAX vecs IMM32,
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RSP INDEX-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ Call the C function at rax with its integer register arguments only:
\ SYSV-CALL,'s zero-cell case, with no vector arguments.
: C-CALL, ( -- )
   X64ASM:R11 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 ZERO-REG,  0 SYSV-CALL, ;

\ rax = dlsym(RTLD_DEFAULT, the NUL-terminated name at rsi),
\ or 0 if the loader has no such name.
: DLSYM, ( -- )
   s" dlsym" LIBC-ENTRY,
   X64ASM:RDI ZERO-REG,
   C-CALL, ;

private

\ ( -- ns ) CLOCK_MONOTONIC, the timers' clock, by libc's clock_gettime, which
\ reads the vDSO, no system call. It answers an int, so eax alone is tested.
: MONO-ROW ( -- )
   s" clock_gettime" LIBC-ENTRY,
   X64ASM:RDI CLOCK-MONOTONIC IMM32,  X64ASM:RSI 0 GTOD X64CODE:ASM-SINK X64ASM:ENC-LEA
   C-CALL,
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RR  TIME-CHECK,
   X64ASM:RAX 0 GTOD X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX NS-PER-S X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI32
   X64ASM:RAX CELL GTOD X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   0 X64RT:G-PUSH ;

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

: RP ( n -- X64ASM:mem ) X64ASM:RSP swap X64ASM:MEM-OFF ;

\ ( pathz dst capacity -- length | -1 | -2 ): the twin of habu1.f LIBC-OS
\ REALPATH. libc resolves the path, symlinks included, and allocates the
\ result; only a result that fits with its NUL reaches dst, and free runs after
\ a copy and after a result too long. -1 is a resolution or loader failure, -2
\ a capacity with no room for the whole C string.
: REALPATH-ROW ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: badcap:label failed:label short:label release:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: count:label counted:label copy:label done:label :}
   X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSP RP-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI RP-PATH RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI RP-DST RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX RP-CAP RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE badcap X64CODE:JCC,
   X64ASM:RAX FREE-NAME IMM32,  X64ASM:RAX RP-FREE-NAME RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX REALPATH-NAME X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:RAX RP-REALPATH-NAME RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX ZERO-REG,  X64ASM:RAX RP-REALPATH-NAME CELL + RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI RP-FREE-NAME RP X64CODE:ASM-SINK X64ASM:ENC-LEA  DLSYM,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E failed X64CODE:JCC,
   X64ASM:RAX RP-FREE RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI RP-REALPATH-NAME RP X64CODE:ASM-SINK X64ASM:ENC-LEA  DLSYM,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E failed X64CODE:JCC,
   X64ASM:RDI RP-PATH RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RSI ZERO-REG,  C-CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E failed X64CODE:JCC,
   X64ASM:RAX RP-RESULT RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX ZERO-REG,
   count X64CODE:LBL,
   X64ASM:R8 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E counted X64CODE:JCC,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-INC  count X64CODE:JMP,
   counted X64CODE:LBL,
   X64ASM:RDX RP-LEN RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX RP-CAP RP X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-AE short X64CODE:JCC,
   X64ASM:RSI RP-RESULT RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI RP-DST RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   copy X64CODE:LBL,
   X64ASM:R8 X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   8 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE copy X64CODE:JCC,
   release X64CODE:LBL,
   X64ASM:RDI RP-RESULT RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX RP-FREE RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  C-CALL,
   X64ASM:RAX RP-LEN RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  done X64CODE:JMP,
   short X64CODE:LBL,
   X64ASM:RAX -2 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  X64ASM:RAX RP-LEN RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   release X64CODE:JMP,
   badcap X64CODE:LBL,
   X64ASM:RAX -2 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  done X64CODE:JMP,
   failed X64CODE:LBL,
   X64ASM:RAX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   done X64CODE:LBL,
   X64ASM:RSP RP-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   0 X64RT:G-PUSH ;

\ The twins of habu1.f's Linux process bodies. A failure answers -1, except
\ pipe's, fcntl's, poll's and spawn-fds's -errno.

17 constant CLONE-SIGCHLD           \ SIGCHLD at exit, nothing shared
$80000 constant PIPE-CLOEXEC        \ O_CLOEXEC
1030 constant F-DUPFD-CLOEXEC
2 constant F-SETFD
3 constant SPAWN-MIN-FD             \ the lowest the pipe's write end may sit
73 constant FCNTL-NOSIGPIPE         \ src/lib/fd.f FD:F-SETNOSIGPIPE
13 constant SIGPIPE
1 constant SIG-IGN
8 constant SIGSET-BYTES
1000 constant MS-PER-S
1000000 constant NS-PER-MS

\ The spawn frame on the machine stack: SPAWN,'s seven arguments, -1 in the
\ directory or a descriptor cell for none, or spawn-fds's request.
0 constant SPN-PATH
8 constant SPN-ARGV
16 constant SPN-ENV
24 constant SPN-CWD
32 constant SPN-IN
40 constant SPN-OUT
48 constant SPN-ERR
SPAWN-ABI:BYTES CELL + constant SPN-PIPE-R  \ past spawn-fds's lift floor cell
SPN-PIPE-R 4 + constant SPN-PIPE-W          \ pipe2's two u32 descriptors
SPN-PIPE-R CELL + constant SPN-BYTE         \ the child's failure byte
SPN-BYTE CELL + constant SPN-PID
SPN-PID CELL + constant SPN-STATUS          \ wait4's u32 status
SPN-STATUS CELL + constant SPN-ARGV0        \ the default argv: the path, then 0
SPN-ARGV0 2 CELL * + constant SPN-ENVP0     \ the default envp: 0
SPN-ENVP0 CELL + constant SPN-FRAME

: FRAME@, ( X64ASM:r64 n -- ) RP X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;
: FRAME!, ( X64ASM:r64 n -- ) RP X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;
: FRAME32@, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 off:n :} r X64ASM:R64>N X64ASM:>R32 off RP X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM ;
: FRAME32!, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 off:n :} r X64ASM:R64>N X64ASM:>R32 off RP X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR ;
: FRAME-AT, ( X64ASM:r64 n -- ) RP X64CODE:ASM-SINK X64ASM:ENC-LEA ;
: MINUS-1, ( X64ASM:r64 -- ) -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32 ;

\ clone(SIGCHLD, 0, 0, 0, 0): x86-64's order puts child_tid before tls where
\ aarch64's puts tls first, and both are 0.
: CLONE-ARGS, ( -- )
   X64ASM:RDI CLONE-SIGCHLD IMM32,  X64ASM:RSI ZERO-REG,  X64ASM:RDX ZERO-REG,  X64ASM:R10 ZERO-REG,  X64ASM:R8 ZERO-REG, ;

\ Copy the used region to the fresh view at frame[8], before clone. Include
\ the pending record, the records a lowering retired, and code above CP
\ still named by those records. A task reads the high-water from main DATA,
\ not its own snapshot. This is LIBC-OS:FORK's Linux extent rule.
: FORK-COPY, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: scan:label next:label span:label scanned:label :}
   X64ASM:R10 SHARED-DATA,
   X64ASM:RCX X64ASM:R10 NDICT-HW-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX NDICT-REG 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RAX DICT-CAP IMM32,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RCX X64ASM:RCX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RSI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI 8 FRAME@,
   X64CODE:ASM-SINK X64ASM:ENC-REP-MOVSB
   X64ASM:R9 CP-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R10 X64ASM:R10 NDICT-HW-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B X64ASM:R10 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RCX ZERO-REG,
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE scanned X64CODE:JCC,
   X64ASM:R8 X64ASM:RCX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R8 DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RDX X64ASM:R8 REC-WID X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E span X64CODE:JCC,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S next X64CODE:JCC,
   span X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:R8 REC-CODE CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   next X64CODE:LBL,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  scan X64CODE:JMP,
   scanned X64CODE:LBL,
   X64ASM:R9 7 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R9 -8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:RSI DBASE-REG CFSTK-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDI 8 FRAME@,  X64ASM:RDI CFSTK-OFF X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   X64CODE:ASM-SINK X64ASM:ENC-REP-MOVSB ;

\ The child replaces both shared mappings with its private backing before
\ returning to Habu. Copying before clone prevents later parent writes from
\ reaching it. Every inherited task DATA keeps the same alias delta.
: FORK, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: unfd:label failed:label parent:label dead:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: plain:label done:label out:label :}
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8            \ [0] fd, [8] temporary view
   X64ASM:RAX REGION-ALIAS:DELTA-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E plain X64CODE:JCC,
   failed X64BOOT:REGION-MEMFD,  X64ASM:RAX 0 FRAME!,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI ZERO-REG,
   X64ASM:RSI REGION IMM32,  X64ASM:RDX PROT-RW IMM32,  X64ASM:R10 MAP-SHARED IMM32,  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,  X64ASM:C-B unfd X64CODE:JCC,
   X64ASM:RAX 8 FRAME!,
   FORK-COPY,
   X64ASM:RDI 8 FRAME@,  X64ASM:RSI REGION IMM32,
   NR-MUNMAP SYS,  X64ASM:C-B unfd X64CODE:JCC,
   CLONE-ARGS,  NR-FORK SYS,  X64ASM:C-B unfd X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE parent X64CODE:JCC,
   X64ASM:RDI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RSI REGION IMM32,  X64ASM:RDX PROT-RW IMM32,  X64ASM:R10 MAP-SHARED-FIXED IMM32,
   X64ASM:R8 0 FRAME@,  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,  X64ASM:C-B dead X64CODE:JCC,
   X64ASM:RDI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX PROT-RX IMM32,
   NR-MMAP SYS,  X64ASM:C-B dead X64CODE:JCC,
   X64ASM:RDI 0 FRAME@,  NR-CLOSE SYS,  X64ASM:C-B dead X64CODE:JCC,
   X64ASM:RAX ZERO-REG,  done X64CODE:JMP,
   dead X64CODE:LBL,
   S\" hb: fork: cannot map the child's own region\n" EXIT-STATUS:SYSTEM STDERR-EXIT,
   parent X64CODE:LBL,
   X64ASM:R10 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDI 0 FRAME@,  NR-CLOSE SYS,
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  done X64CODE:JMP,
   plain X64CODE:LBL,
   CLONE-ARGS,  NR-FORK SYS,  X64ASM:C-AE done X64CODE:JCC,
   failed X64CODE:JMP,
   unfd X64CODE:LBL,
   X64ASM:RDI 0 FRAME@,  NR-CLOSE SYS,
   failed X64CODE:LBL,
   X64ASM:RAX MINUS-1,
   done X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE out X64CODE:JCC,
   X64ASM:RDX SHARED-DATA,
   X64ASM:RAX X64ASM:RDX EXIT-OWNER-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   out X64CODE:LBL,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX PUSH, ;

: CLOSE-SLOT, ( n -- ) {: off:n :}  X64ASM:RDI off FRAME32@,  NR-CLOSE SYS, ;

\ Move the pipe's write end to the fd in rdx or above, as LINUX-SPAWN-LIFT-W
\ does, so no descriptor the child places can replace it. syscall keeps r8.
: SPAWN-LIFT-W, ( label -- ) {: fail:label :}
   X64CODE:LBL {: high:label :}
   X64ASM:RDI SPN-PIPE-W FRAME32@,
   X64ASM:RDI X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE high X64CODE:JCC,
   X64ASM:RSI F-DUPFD-CLOEXEC IMM32,  NR-FCNTL SYS,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   SPN-PIPE-W CLOSE-SLOT,
   X64ASM:R8 SPN-PIPE-W FRAME32!,
   high X64CODE:LBL, ;

\ The four stdio spawns lift it above the three stdio descriptors.
: SPAWN-PREP-W, ( label -- )  X64ASM:RDX SPAWN-MIN-FD IMM32,  SPAWN-LIFT-W, ;

\ Put the descriptor in the frame cell on fd n, as LINUX-DUP2-FD: dup3 it
\ there, or, when it is n itself, clear the close-on-exec flag a dup3 would
\ have cleared. A negative descriptor leaves n alone.
: CHILD-DUP, ( n n label -- ) {: off:n fd:n fail:label :}
   X64CODE:LBL X64CODE:LBL {: skip:label keep:label :}
   X64ASM:RDI off FRAME@,
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S skip X64CODE:JCC,
   X64ASM:RDI fd X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E keep X64CODE:JCC,
   X64ASM:RSI fd IMM32,  X64ASM:RDX ZERO-REG,  NR-DUP2 SYS,  X64ASM:C-B fail X64CODE:JCC,
   skip X64CODE:JMP,
   keep X64CODE:LBL,
   X64ASM:RSI F-SETFD IMM32,  X64ASM:RDX ZERO-REG,  NR-FCNTL SYS,  X64ASM:C-B fail X64CODE:JCC,
   skip X64CODE:LBL, ;

\ Close the child's copy of the source in the stdio cell at off once all three
\ are on 0, 1 and 2, as LINUX-CLOSE-SOURCE: not at or below 2, not when an
\ earlier stdio cell holds it, and never the exec-failure pipe.
: CLOSE-SOURCE, ( n label -- ) {: off:n fail:label :}
   X64CODE:LBL {: skip:label :}
   X64ASM:RDI off FRAME@,
   X64ASM:RDI 2 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-LE skip X64CODE:JCC,
   off SPN-IN ?do
      X64ASM:RAX i FRAME@,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E skip X64CODE:JCC,
   CELL +loop
   X64ASM:RAX SPN-PIPE-W FRAME32@,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E skip X64CODE:JCC,
   NR-CLOSE SYS,  X64ASM:C-B fail X64CODE:JCC,
   skip X64CODE:LBL, ;

\ LINUX-SPAWN-CHILD: its own process group, the directory, the three
\ descriptors and their sources' close, then execve. A step that fails, execve
\ included, writes one byte to the pipe and exits EXIT-STATUS:EXEC; the pipe is
\ O_CLOEXEC, so a successful execve closes it with nothing written.
: SPAWN-CHILD, ( -- )
   X64CODE:LBL X64CODE:LBL {: fail:label nocwd:label :}
   SPN-PIPE-R CLOSE-SLOT,
   X64ASM:RDI ZERO-REG,  X64ASM:RSI ZERO-REG,  NR-SETPGID SYS,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RDI SPN-CWD FRAME@,  X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S nocwd X64CODE:JCC,
   NR-CHDIR SYS,  X64ASM:C-B fail X64CODE:JCC,
   nocwd X64CODE:LBL,
   SPN-IN 0 fail CHILD-DUP,  SPN-OUT 1 fail CHILD-DUP,  SPN-ERR 2 fail CHILD-DUP,
   SPN-IN fail CLOSE-SOURCE,  SPN-OUT fail CLOSE-SOURCE,  SPN-ERR fail CLOSE-SOURCE,
   X64ASM:RDI SPN-PATH FRAME@,  X64ASM:RSI SPN-ARGV FRAME@,  X64ASM:RDX SPN-ENV FRAME@,
   NR-EXECVE SYS,
   fail X64CODE:LBL,
   X64ASM:RAX 1 IMM32,  X64ASM:RAX SPN-BYTE FRAME!,
   X64ASM:RDI SPN-PIPE-W FRAME32@,  X64ASM:RSI SPN-BYTE FRAME-AT,  X64ASM:RDX 1 IMM32,  NR-WRITE SYS,
   EXIT-STATUS:EXEC EXIT-GROUP, ;

\ LINUX-SPAWN-PARENT, with the child's pid in rax: read the pipe once. End of
\ file is an execve that worked, and rax becomes the pid; a byte or a failed
\ read is a child that failed, which is reaped, and rax becomes -1.
: SPAWN-PARENT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: failed:label ok:label done:label :}
   X64ASM:RAX SPN-PID FRAME!,
   SPN-PIPE-W CLOSE-SLOT,
   X64ASM:RDI SPN-PIPE-R FRAME32@,  X64ASM:RSI SPN-BYTE FRAME-AT,  X64ASM:RDX 1 IMM32,  NR-READ SYS,
   X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E ok X64CODE:JCC,
   failed X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,
   X64ASM:RDI SPN-PID FRAME@,  X64ASM:RSI SPN-STATUS FRAME-AT,  X64ASM:RDX ZERO-REG,  X64ASM:R10 ZERO-REG,
   NR-WAIT4 SYS,
   X64ASM:RAX MINUS-1,  done X64CODE:JMP,
   ok X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,
   X64ASM:RAX SPN-PID FRAME@,
   done X64CODE:LBL, ;

\ The twin of habu1.f LINUX-SPAWN over the frame at rsp: rax becomes the
\ child's pid, or -1 when the pipe, the clone or any step of the child failed.
: SPAWN, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: child:label closefail:label fail:label done:label :}
   X64ASM:RDI SPN-PIPE-R FRAME-AT,  X64ASM:RSI PIPE-CLOEXEC IMM32,  NR-PIPE SYS,  X64ASM:C-B fail X64CODE:JCC,
   closefail SPAWN-PREP-W,
   CLONE-ARGS,  NR-SPAWN SYS,  X64ASM:C-B closefail X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E child X64CODE:JCC,
   SPAWN-PARENT,  done X64CODE:JMP,
   child X64CODE:LBL,
   SPAWN-CHILD,
   closefail X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,  SPN-PIPE-W CLOSE-SLOT,
   fail X64CODE:LBL,
   X64ASM:RAX MINUS-1,
   done X64CODE:LBL, ;

\ spawn-fds over the same frame: SPN-BYTE's cell holds the child's errno (four
\ bytes), SPN-ARGV0 the address of its mapped array of lifted descriptors.
4 constant CLOSE-RANGE-CLOEXEC
4 constant SPAWN-EINTR
22 constant SPAWN-EINVAL
4 constant SPAWN-ERRNO-BYTES
SPN-ARGV0 constant SPN-LIFTED

: NFDS@, ( X64ASM:r64 -- ) SPAWN-ABI:NFDS FRAME@, ;

\ spawn-fds's child, through the header's steps. A failing step writes its
\ errno to the pipe and exits EXIT-STATUS:EXEC.
: SPAWN-FDS-CHILD, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: fail:label nocwd:label lift:label keep:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: lifted:label place:label skip:label placed:label :}
   X64CODE:LBL {: unmasked:label :}
   SPN-PIPE-R CLOSE-SLOT,
   X64ASM:RAX SPAWN-ABI:BLOCKED FRAME@,  X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E unmasked X64CODE:JCC,
   X64ASM:RDI ZERO-REG,  X64ASM:RSI SPAWN-ABI:BLOCKED FRAME-AT,  X64ASM:RDX ZERO-REG,
   X64ASM:R10 SIGSET-BYTES IMM32,  NR-RT-SIGPROCMASK SYS,  X64ASM:C-B fail X64CODE:JCC,
   unmasked X64CODE:LBL,
   X64ASM:RDI ZERO-REG,  X64ASM:RSI SPAWN-ABI:PGROUP FRAME@,  NR-SETPGID SYS,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RDI SPAWN-ABI:CWD FRAME@,  X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E nocwd X64CODE:JCC,
   NR-CHDIR SYS,  X64ASM:C-B fail X64CODE:JCC,
   nocwd X64CODE:LBL,
   X64ASM:RDI ZERO-REG,  X64ASM:RSI MINUS-1,  X64ASM:RDX CLOSE-RANGE-CLOEXEC IMM32,
   NR-CLOSE-RANGE SYS,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RSI NFDS@,  X64ASM:RSI 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RSI CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   MAP-ANON,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RAX SPN-LIFTED FRAME!,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R8 SPAWN-ABI:FDS FRAME@,  X64ASM:R10 NFDS@,
   lift X64CODE:LBL,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E lifted X64CODE:JCC,
   X64ASM:RDI X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S keep X64CODE:JCC,
   X64ASM:RSI F-DUPFD-CLOEXEC IMM32,  X64ASM:RDX SPAWN-ABI:BYTES FRAME@,  NR-FCNTL SYS,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   keep X64CODE:LBL,
   X64ASM:RDI X64ASM:R9 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:R9 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC  lift X64CODE:JMP,
   lifted X64CODE:LBL,
   X64ASM:R9 SPN-LIFTED FRAME@,  X64ASM:R10 ZERO-REG,
   place X64CODE:LBL,
   X64ASM:R10 SPAWN-ABI:NFDS RP X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-GE placed X64CODE:JCC,
   X64ASM:RDI X64ASM:R9 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S skip X64CODE:JCC,
   X64ASM:RSI X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX ZERO-REG,  NR-DUP2 SYS,  X64ASM:C-B fail X64CODE:JCC,
   skip X64CODE:LBL,
   X64ASM:R9 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-INC  place X64CODE:JMP,
   placed X64CODE:LBL,
   X64ASM:RDI SPAWN-ABI:PATH FRAME@,  X64ASM:RSI SPAWN-ABI:ARGV FRAME@,  X64ASM:RDX SPAWN-ABI:ENVP FRAME@,
   NR-EXECVE SYS,
   fail X64CODE:LBL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NEG  X64ASM:RAX SPN-BYTE FRAME32!,
   X64ASM:RDI SPN-PIPE-W FRAME32@,  X64ASM:RSI SPN-BYTE FRAME-AT,  X64ASM:RDX SPAWN-ERRNO-BYTES IMM32,
   NR-WRITE SYS,
   EXIT-STATUS:EXEC EXIT-GROUP, ;

\ The twin of habu1.f LINUX-SPAWN-FDS-PARENT, with the child's pid in rax:
\ end of file leaves the pid in rax; the child's errno makes rax -errno and
\ reaps it. An interrupted read is retried; any other failed read is its
\ -errno, and the child is reaped.
: SPAWN-FDS-PARENT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: retry:label got:label reap:label done:label :}
   X64ASM:RAX SPN-PID FRAME!,
   SPN-PIPE-W CLOSE-SLOT,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX SPN-BYTE FRAME32!,
   retry X64CODE:LBL,
   X64ASM:RDI SPN-PIPE-R FRAME32@,  X64ASM:RSI SPN-BYTE FRAME-AT,  X64ASM:RDX SPAWN-ERRNO-BYTES IMM32,
   NR-READ SYS,  X64ASM:C-AE got X64CODE:JCC,
   X64ASM:RAX SPAWN-EINTR negate X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E retry X64CODE:JCC,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  reap X64CODE:JMP,
   got X64CODE:LBL,
   X64ASM:R8 SPN-PID FRAME@,  X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R8 SPN-BYTE FRAME32@,  X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-NEG
   reap X64CODE:LBL,
   X64ASM:RDI SPN-PID FRAME@,  X64ASM:RSI SPN-STATUS FRAME-AT,  X64ASM:RDX ZERO-REG,  X64ASM:R10 ZERO-REG,
   NR-WAIT4 SYS,
   done X64CODE:LBL,
   SPN-PIPE-R CLOSE-SLOT,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ The twin of habu1.f LINUX-SPAWN-FDS over the frame at rsp: rax becomes the
\ child's pid or -errno. The floor, max(NFDS, highest source + 1), goes in the
\ cell past the request, and the pipe's write end is lifted to it or above, so
\ no slot targets it and no source names it.
: SPAWN-FDS, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: child:label closefail:label done:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: top:label below:label topped:label :}
   X64ASM:RDI SPN-PIPE-R FRAME-AT,  X64ASM:RSI PIPE-CLOEXEC IMM32,  NR-PIPE SYS,  X64ASM:C-B done X64CODE:JCC,
   X64ASM:RDX NFDS@,  X64ASM:R10 NFDS@,  X64ASM:R8 SPAWN-ABI:FDS FRAME@,
   top X64CODE:LBL,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E topped X64CODE:JCC,
   X64ASM:RCX X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-L below X64CODE:JCC,
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-INC
   below X64CODE:LBL,
   X64ASM:R8 CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC  top X64CODE:JMP,
   topped X64CODE:LBL,
   X64ASM:RDX SPAWN-ABI:BYTES FRAME!,  closefail SPAWN-LIFT-W,
   CLONE-ARGS,  NR-SPAWN SYS,  X64ASM:C-B closefail X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E child X64CODE:JCC,
   SPAWN-FDS-PARENT,  done X64CODE:JMP,
   child X64CODE:LBL,
   SPAWN-FDS-CHILD,
   closefail X64CODE:LBL,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   SPN-PIPE-R CLOSE-SLOT,  SPN-PIPE-W CLOSE-SLOT,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   done X64CODE:LBL, ;

: SPAWN-OPEN, ( -- ) X64ASM:RSP SPN-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32 ;
: SPAWN-CLOSE, ( -- ) X64ASM:RSP SPN-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32  X64ASM:RAX PUSH, ;
: POP-SLOT, ( n -- ) {: off:n :}  X64ASM:RAX POP,  X64ASM:RAX off FRAME!, ;
: NONE-SLOT, ( n -- ) {: off:n :}  X64ASM:RAX MINUS-1,  X64ASM:RAX off FRAME!, ;
: STDIO-SLOTS, ( -- ) SPN-ERR POP-SLOT,  SPN-OUT POP-SLOT,  SPN-IN POP-SLOT, ;

\ argv = { path, 0 }.
: DEFAULT-ARGV, ( -- )
   X64ASM:RAX SPN-PATH FRAME@,  X64ASM:RAX SPN-ARGV0 FRAME!,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX SPN-ARGV0 CELL + FRAME!,
   X64ASM:RAX SPN-ARGV0 FRAME-AT,  X64ASM:RAX SPN-ARGV FRAME!, ;

: DEFAULT-ENV, ( -- )
   X64ASM:RAX ZERO-REG,  X64ASM:RAX SPN-ENVP0 FRAME!,
   X64ASM:RAX SPN-ENVP0 FRAME-AT,  X64ASM:RAX SPN-ENV FRAME!, ;

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

\ ( request -- pid|-errno ): src/habu/spawn-abi.f lays out the request, as
\ habu1.f BSPAWNFDS reads it. A negative NFDS is -EINVAL.
: SPAWN-FDS-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: valid:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RCX X64ASM:RAX SPAWN-ABI:NFDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NS valid X64CODE:JCC,
   X64ASM:RAX SPAWN-EINVAL negate X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  done X64CODE:JMP,
   valid X64CODE:LBL,
   SPAWN-OPEN,
   SPAWN-ABI:BYTES 0 ?do
      X64ASM:RCX X64ASM:RAX i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RCX i FRAME!,
   CELL +loop
   SPAWN-FDS,
   X64ASM:RSP SPN-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   done X64CODE:LBL,
   X64ASM:RAX PUSH, ;

\ ( pid -- status|-1 ): wait4's raw u32 status, as BWAITSTATUS. The load
\ leaves the carry SYS, set for SYS-PUSH.
: WAIT-STATUS-ROW ( -- )
   X64ASM:RDI POP,
   X64ASM:RSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX ZERO-REG,  X64ASM:R10 ZERO-REG,  NR-WAIT4 SYS,
   X64ASM:RAX 0 FRAME32@,  X64RT:SYS-PUSH
   X64ASM:RSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ ( -- rfd wfd 0 | -errno thrice ): pipe2(fds, 0).
: PIPE-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   X64ASM:RSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI ZERO-REG,  NR-PIPE SYS,  X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RAX 0 FRAME32@,  X64ASM:RAX PUSH,  X64ASM:RAX 4 FRAME32@,  X64ASM:RAX PUSH,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX PUSH,  done X64CODE:JMP,
   failed X64CODE:LBL,
   X64ASM:RAX PUSH,  X64ASM:RAX PUSH,  X64ASM:RAX PUSH,
   done X64CODE:LBL,
   X64ASM:RSP CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ ( fd cmd arg -- rc|-errno ): fcntl. FCNTL-NOSIGPIPE, macOS's F_SETNOSIGPIPE,
\ has no Linux command, so it ignores SIGPIPE process-wide via rt_sigaction.
: FCNTL-ROW ( -- )
   X64CODE:LBL X64CODE:LBL {: real:label done:label :}
   X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSI FCNTL-NOSIGPIPE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE real X64CODE:JCC,
   X64ASM:RAX ZERO-REG,
   \ The kernel's struct sigaction: SIG_IGN, zero flags, restorer and mask.
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX SIG-IGN IMM32,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDI SIGPIPE IMM32,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX ZERO-REG,
   X64ASM:R10 SIGSET-BYTES IMM32,
   NR-SIGACTION SYS,
   X64ASM:RSP 4 CELL * FRAME-AT,  done X64CODE:JMP,
   real X64CODE:LBL,
   NR-FCNTL SYS,
   done X64CODE:LBL,
   X64ASM:RAX PUSH, ;

\ ( fds nfds ms -- n|0|-errno ): ppoll, as BPOLL. ms becomes a timespec, and a negative ms none, which waits
\ without end. rax is pushed raw: poll alone is not restarted after a signal,
\ so its caller needs -EINTR (habu1.f's errno rule).
: POLL-ROW ( -- )
   X64CODE:LBL {: call:label :}
   X64ASM:RAX POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSP 2 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDX ZERO-REG,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S call X64CODE:JCC,
   X64ASM:RCX MS-PER-S IMM32,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:RDX X64ASM:RDX NS-PER-MS X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI32
   X64ASM:RAX 0 FRAME!,  X64ASM:RDX CELL FRAME!,
   X64ASM:RDX X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   call X64CODE:LBL,
   X64ASM:R10 ZERO-REG,  X64ASM:R8 ZERO-REG,  NR-POLL SYS,
   X64ASM:RSP 2 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX PUSH, ;

public

: SYSCALLS, ( -- )
   s" open" [: OPEN-ROW ;] PRIM
   s" read" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI POP,  NR-READ FS-SYS-PUSH, ;] PRIM
   s" write" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI POP,  NR-WRITE FS-SYS-PUSH, ;] PRIM
   s" close" [: X64ASM:RDI POP,  NR-CLOSE SYS, ;] PRIM
   s" close-rc" [: X64ASM:RDI POP,  NR-CLOSE SYS-PUSH, ;] PRIM
   s" ioctl" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI POP,  NR-IOCTL SYS-PUSH, ;] PRIM
   s" map-anon" [: MAP-ANON-ROW ;] PRIM
   s" mmap" [: MMAP-ROW ;] PRIM
   s" munmap" [: X64ASM:RSI POP, X64ASM:RDI POP,  NR-MUNMAP SYS-PUSH, ;] PRIM
   s" open-rd" [: X64ASM:RDI POP,  X64ASM:RDI X64ASM:R64>N OS-OPEN-RD  X64RT:FS-SYS-PUSH ;] PRIM
   s" access" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI AT-FDCWD, X64ASM:R10 ZERO-REG,  NR-ACCESS FS-SYS-PUSH, ;] PRIM
   s" unlink" [: X64ASM:RSI POP, X64ASM:RDI AT-FDCWD, X64ASM:RDX ZERO-REG,  NR-UNLINK FS-SYS-PUSH, ;] PRIM
   s" rename" [: X64ASM:R10 POP, X64ASM:RSI POP, X64ASM:RDI AT-FDCWD, X64ASM:RDX AT-FDCWD,  NR-RENAME FS-SYS-PUSH, ;] PRIM
   s" chmod" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI AT-FDCWD, X64ASM:R10 ZERO-REG,  NR-CHMOD FS-SYS-PUSH, ;] PRIM
   s" symlink" [: X64ASM:RDX POP, X64ASM:RDI POP, X64ASM:RSI AT-FDCWD,  NR-SYMLINKAT FS-SYS-PUSH, ;] PRIM
   s" readlink" [: X64ASM:R10 POP, X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI AT-FDCWD,  NR-READLINKAT FS-SYS-PUSH, ;] PRIM
   s" realpath" [: REALPATH-ROW ;] PRIM
   s" mkdir" [: X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI AT-FDCWD,  NR-MKDIR FS-SYS-PUSH, ;] PRIM
   s" rmdir" [: X64ASM:RSI POP, X64ASM:RDI AT-FDCWD, X64ASM:RDX AT-REMOVEDIR IMM32,  NR-RMDIR FS-SYS-PUSH, ;] PRIM
   s" stat64" [: NR-STAT64 0 STAT-ROW ;] PRIM
   s" lstat64" [: NR-LSTAT64 AT-SYMLINK-NOFOLLOW STAT-ROW ;] PRIM
   \ getdents64 keeps no base cookie: the pointer is dropped.
   s" getdirentries64" [: DROP,  X64ASM:RDX POP, X64ASM:RSI POP, X64ASM:RDI POP,  NR-GETDIRENTRIES64 FS-SYS-PUSH, ;] PRIM
   s" epoch-seconds" [: EPOCH-ROW ;] PRIM
   s" mono-ns" [: MONO-ROW ;] PRIM
   s" getpid" [: NR-GETPID SYS-PUSH, ;] PRIM
   s" pipe" [: PIPE-ROW ;] PRIM
   s" dup2" [: X64ASM:RSI POP, X64ASM:RDI POP, X64ASM:RDX ZERO-REG,  NR-DUP2 SYS-PUSH, ;] PRIM
   s" fcntl" [: FCNTL-ROW ;] PRIM
   s" poll" [: POLL-ROW ;] PRIM
   s" kill" [: X64ASM:RSI POP, X64ASM:RDI POP,  NR-KILL SYS-PUSH, ;] PRIM
   s" setpgid" [: X64ASM:RSI POP, X64ASM:RDI POP,  NR-SETPGID SYS-PUSH, ;] PRIM
   s" proc-watch-open" [: BPROCWATCHOPEN ;] PRIM
   s" kill-errno" [: BKILLERRNO ;] PRIM
   s" execve" [: BEXECVE ;] PRIM
   s" fork" [: FORK, ;] PRIM
   s" wait-status" [: WAIT-STATUS-ROW ;] PRIM
   s" spawn-io" [: SPAWN-IO-ROW ;] PRIM
   s" spawn-argv-io" [: SPAWN-ARGV-IO-ROW ;] PRIM
   s" spawn-argv-env-io" [: SPAWN-ENV-IO-ROW ;] PRIM
   s" spawn-argv-env-cwd-io" [: SPAWN-CWD-IO-ROW ;] PRIM
   s" spawn-fds" [: SPAWN-FDS-ROW ;] PRIM ;

\ The catch frame, STACK-ABI:CATCH-BYTES on the machine stack and chained
\ through HND-CELL, laid out as habu1.f BCATCH lays it. The shared ABI names
\ its saved pending definition, stack extent and output-device cells;
\ the remaining byte offsets are below.
0 constant CATCH-PREV       \ the handler this one hides
8 constant CATCH-DSP        \ the data stack pointer once the xt is popped
16 constant CATCH-MSP       \ rsp at CAUGHT,'s entry, above the frame
24 constant CATCH-RESUME    \ where a throw resumes, the code in rax
32 constant CATCH-RET       \ the cell at CATCH-MSP, as BCATCH saves x30
40 constant CATCH-RDEPTH    \ RSP-CELL, the return stack's depth
48 constant CATCH-LTOP      \ LOOPSP-CELL, the loop stack's top

\ An xt is a code address a body enters by `call`; the code it runs owns every
\ scratch register, so what a body needs after the call waits on the machine stack.

private

: MOV-LOAD, ( X64ASM:r64 X64ASM:r64 n -- ) {: r:X64ASM:r64 base:X64ASM:r64 off:n :}
   r base off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;
: MOV-STORE, ( X64ASM:r64 X64ASM:r64 n -- ) {: r:X64ASM:r64 base:X64ASM:r64 off:n :}
   r base off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ Serialize, clobbering nothing, before running code another thread published or
\ this one wrote through the alias (SDM 3A, self/cross-modifying); CPUID exits a VM.
: CODE-SYNC, ( -- )
   X64CODE:LBL {: back:label :}
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:SS X64CODE:ASM-SINK X64ASM:ENC-MOV32-RS
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA    \ rsp before the ss push
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64CODE:ASM-SINK X64ASM:ENC-PUSHFQ
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:CS X64CODE:ASM-SINK X64ASM:ENC-MOV32-RS
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX back X64CODE:LEA-REL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64CODE:ASM-SINK X64ASM:ENC-IRETQ
   back X64CODE:LBL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP ;

\ An ordered xt at or above this thread's CP can name code
\ published after the thread started.
: CALLABLE, ( -- )
   X64CODE:LBL {: old:label :}
   X64ASM:RAX CP-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B old X64CODE:JCC,
   CODE-SYNC,
   old X64CODE:LBL, ;

\ Bit 63 marks a stack record: rdi receives its address and rax its code.
\ Invocation setup preserves rdi until the quotation saves it in its frame.
: QUOTATION-RECORD, ( -- )
   X64CODE:LBL {: plain:label :}
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR X64ASM:C-NS plain X64CODE:JCC,
   X64ASM:RDI $7FFFFFFFFFFFFFFF X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-AND-RR
   X64ASM:RAX X64ASM:RDI 0 MOV-LOAD,
   plain X64CODE:LBL, ;

: INVOKE, ( -- )
   QUOTATION-RECORD,  CALLABLE,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG ;

\ Exit with rdi when it is a status in [n, 255], and with EXIT-STATUS:UNCAUGHT otherwise:
\ the kernel would keep only the low byte, so a negative code could exit 0.
\ The tails of habu1.f BDIE (n = 0) and BTHROW (n = 1).
: RC-EXIT, ( n -- ) {: lo:n :}
   X64CODE:LBL {: wide:label :}
   X64ASM:RAX X64ASM:RDI lo negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX 255 lo - X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-A wide X64CODE:JCC,
   NR-EXIT-GROUP SYS,
   wide X64CODE:LBL,
   EXIT-STATUS:UNCAUGHT EXIT-GROUP, ;

\ execute-floor takes an interpreter-owned code address, never a local record.
\ Call it, then answer whether it left the
\ data stack below the base in S0-CELL. Below, the stack is reset to the base
\ before the flag is pushed, so the push lands at the base and not on the low
\ guard page.
: EXECUTE-FLOOR, ( -- )
   X64CODE:LBL {: above:label :}
   X64ASM:RAX POP,  CALLABLE,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RCX ZERO-REG,
   X64ASM:RAX DATA-REG S0-CELL MOV-LOAD,
   DSP X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE above X64CODE:JCC,
   DSP X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   above X64CODE:LBL,
   X64ASM:RCX PUSH, ;

\ The return stack is RSP-CELL cells deep from the base in
\ STACK-ABI:RETURN-BASE-CELL, as habu1.f RSTK-PUSH and RSTK-POP address it; a
\ push past RETURN-CELLS lands on its guard page. RSTACK, loads the depth into
\ rcx and the base into rsi, and RSLOT is the cell n cells past the depth.
: RSTACK, ( -- )
   X64ASM:RCX DATA-REG RSP-CELL MOV-LOAD,
   X64ASM:RSI DATA-REG STACK-ABI:RETURN-BASE-CELL MOV-LOAD, ;
: RSLOT ( n -- X64ASM:mem ) {: k:n :} X64ASM:RSI X64ASM:RCX CELL k CELL * X64ASM:MEM-IDX ;

: 2>R, ( -- )
   X64ASM:RDX POP,  X64ASM:RAX POP,  RSTACK,
   X64ASM:RAX 0 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR  X64ASM:RDX 1 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX 2 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:RCX DATA-REG RSP-CELL MOV-STORE, ;

: 2R>, ( -- )
   RSTACK,
   X64ASM:RCX 2 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  X64ASM:RCX DATA-REG RSP-CELL MOV-STORE,
   X64ASM:RAX 0 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RDX 1 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX PUSH,  X64ASM:RDX PUSH, ;

: 2R@, ( -- )
   RSTACK,
   X64ASM:RAX -2 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RDX -1 RSLOT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX PUSH,  X64ASM:RDX PUSH, ;

\ Completed or abandoned definitions leave no local or token-mark state.
: CLEAR-LOCALS, ( -- )
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX LOCN-CELL CELL!,  X64ASM:RAX CAPDEPTH-CELL CELL!,  X64ASM:RAX RAWNEXT-CELL CELL!,
   X64ASM:RAX TOKLOCN-CELL CELL!,  X64ASM:RAX TOKDEPTH-CELL CELL!,  X64ASM:RAX TOKRAW-CELL CELL!, ;

\ A caught compiler failure leaves its handler, return and loop state to the catch
\ frame; definition-local state is abandoned before the frame's recovery retires its records.
: DEF-ABORT-BODY ( -- )
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX NCOMP-DISPATCH:COMP-LATCH-CELL CELL!,
   X64ASM:RAX VSP-CELL CELL!,
   X64ASM:RAX BODYLEN-CELL CELL!,  X64ASM:RAX EXITH-CELL CELL!,
   X64ASM:RAX PEND-CELL CELL!,  X64ASM:RAX CMM-CELL CELL!,
   X64ASM:RAX TSIG-A-CELL CELL!,  X64ASM:RAX TSIG-U-CELL CELL!,
   X64ASM:RAX TCSIG-A-CELL CELL!,  X64ASM:RAX TCSIG-U-CELL CELL!,
   X64ASM:RAX CRSIG-A-CELL CELL!,  X64ASM:RAX CRSIG-U-CELL CELL!,
   X64ASM:RAX DOESB-CELL CELL!,  X64ASM:RAX TRUSTED-CELL CELL!,  X64ASM:RAX CLAUSE-CELL CELL!,
   CLEAR-LOCALS, ;

\ Pop an xt and call it under a catch frame: the twin of habu1.f BCATCH-RUN.
\ The frame saves the caller's whole execution state, links in as the handler
\ and is unlinked on the return, which leaves 0 in rax; a throw restores the
\ state, unlinks the frame and resumes at the label this answers, with its code
\ in rax. The caller places that label after the return path.
: CATCH-RUN, ( -- label )
   X64CODE:LBL {: resume:label :}
   X64ASM:RAX POP, QUOTATION-RECORD,
   X64ASM:RSP STACK-ABI:CATCH-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RCX DATA-REG HND-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP CATCH-PREV MOV-STORE,
   DSP X64ASM:RSP CATCH-DSP MOV-STORE,
   X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RSP CATCH-MSP MOV-STORE,
   X64ASM:RDX X64ASM:RCX 0 MOV-LOAD,  X64ASM:RDX X64ASM:RSP CATCH-RET MOV-STORE,
   X64ASM:RCX resume X64CODE:MOVABS,  X64ASM:RCX X64ASM:RSP CATCH-RESUME MOV-STORE,
   X64ASM:RCX DATA-REG RSP-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP CATCH-RDEPTH MOV-STORE,
   X64ASM:RCX DATA-REG LOOPSP-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP CATCH-LTOP MOV-STORE,
   X64ASM:RCX DATA-REG PEND-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-PEND MOV-STORE,
   X64ASM:RCX DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-BASE MOV-STORE,
   X64ASM:RCX DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-CAP MOV-STORE,
   X64ASM:RCX DATA-REG GENIO-ABI:BUSY-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-BUSY MOV-STORE,
   X64ASM:RCX DATA-REG GENIO-ABI:ACTIVE-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RSP STACK-ABI:CATCH-ACTIVE MOV-STORE,
   X64ASM:RSP DATA-REG HND-CELL MOV-STORE,
   CALLABLE,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RCX X64ASM:RSP CATCH-PREV MOV-LOAD,  X64ASM:RCX DATA-REG HND-CELL MOV-STORE,
   X64ASM:RSP STACK-ABI:CATCH-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   X64ASM:RAX ZERO-REG,
   resume ;

\ Restore the execution state the frame in rdx saved, unlink it and resume
\ where CAUGHT, left, with the code still in rax. A delivered fault ends here;
\ abandon only a definition opened since the catch began, as BCATCH-RUN does.
: RESUME, ( -- )
   X64CODE:LBL {: kept:label :}
   X64ASM:RCX ZERO-REG,  X64ASM:RCX DATA-REG FAULT-MARK-CELL MOV-STORE,
   X64ASM:RCX DATA-REG PEND-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E kept X64CODE:JCC,
   X64ASM:R8 X64ASM:RDX STACK-ABI:CATCH-PEND MOV-LOAD,
   X64ASM:RCX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E kept X64CODE:JCC,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   DEF-ABORT-BODY
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   kept X64CODE:LBL,
   DSP X64ASM:RDX CATCH-DSP MOV-LOAD,
   X64ASM:RCX X64ASM:RDX STACK-ABI:CATCH-BASE MOV-LOAD,
   X64ASM:RCX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX STACK-ABI:CATCH-CAP MOV-LOAD,
   X64ASM:RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX STACK-ABI:CATCH-BUSY MOV-LOAD,
   X64ASM:RCX DATA-REG GENIO-ABI:BUSY-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX STACK-ABI:CATCH-ACTIVE MOV-LOAD,
   X64ASM:RCX DATA-REG GENIO-ABI:ACTIVE-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX CATCH-RDEPTH MOV-LOAD,  X64ASM:RCX DATA-REG RSP-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX CATCH-LTOP MOV-LOAD,  X64ASM:RCX DATA-REG LOOPSP-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX CATCH-PREV MOV-LOAD,  X64ASM:RCX DATA-REG HND-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RDX CATCH-RET MOV-LOAD,
   X64ASM:RSI X64ASM:RDX CATCH-RESUME MOV-LOAD,
   X64ASM:RSP X64ASM:RDX CATCH-MSP MOV-LOAD,
   X64ASM:RCX X64ASM:RSP 0 MOV-STORE,
   X64ASM:RSI X64CODE:JMP-REG, ;

\ No handler: run the exit actions, call the reporter xt in UNCGH-CELL, when
\ one is installed, with the code on the data stack, and then exit with the
\ code, which waited on the machine stack.
: UNCAUGHT, ( -- )
   X64CODE:LBL {: bare:label :}
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  EXIT-HOOK-LBL X64CODE:CALL,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RCX DATA-REG UNCGH-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bare X64CODE:JCC,
   X64ASM:RDI PUSH,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   bare X64CODE:LBL,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP
   1 RC-EXIT, ;

\ The default UNCGH hook consumes the code and returns. Ordinary exit codes
\ are quiet; a wider code is printed intact before UNCAUGHT, maps its status.
: UNCAUGHT-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: done:label number:label head:label minus:label nl:label :}
   X64CODE:LBL dup LABEL>N UNCAUGHT-ENTRY ! X64CODE:LBL,
   X64ASM:RAX POP,
   X64ASM:RCX X64ASM:RAX -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX 254 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-BE done X64CODE:JCC,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   head s" hb: uncaught throw code " STR:LEN STDERR-WRITE,
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NS number X64CODE:JCC,
   minus 1 STDERR-WRITE,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-NEG
   number X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  DIAG-U,
   nl 1 STDERR-WRITE,
   done X64CODE:LBL,  X64CODE:RET,
   head s" hb: uncaught throw code " X64CODE:DATA-AFTER
   minus s" -" X64CODE:DATA-AFTER  nl s\" \n" X64CODE:DATA-AFTER
   X64CODE:DATA-AFTER, ;

\ Forward the code in rax without beginning another throw. Habu's `evaluate`
\ recovers through `catch`; the handler's frame is restored and resumed.
: RETHROW, ( -- )
   X64CODE:LBL {: none:label :}
   X64ASM:RDX DATA-REG HND-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   RESUME,
   none X64CODE:LBL,
   UNCAUGHT, ;

\ Begin one throw, as habu1.f BTHROW-COUNT does, before forwarding its code.
: THROW, ( -- )
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RCX DATA-REG THROWS-CELL MOV-STORE,
   RETHROW, ;

\ The `throw` primitive's code in rax: 0 becomes -9854, E-THROW-ZERO.
: NONZERO-CODE, ( -- )
   X64CODE:LBL {: nonzero:label :}
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE nonzero X64CODE:JCC,
   X64ASM:RAX -9854 IMM64,
   nonzero X64CODE:LBL, ;

\ At a throw's resume, with the code in rax: the twin of habu1.f
\ BCATCH-FORWARD. With HALT-UNWIND-CELL clear the code is delivered as always.
\ Set, a TASK:HALT is unwinding the task: while a frame remains below this one
\ (HND nonzero) this is an inner catch, which rethrows the code at once and
\ never delivers it; the task's root catch (HND 0) clears the cell and delivers
\ the code, so the runner ends the task.
: CATCH-FORWARD, ( -- )
   X64CODE:LBL X64CODE:LBL {: inner:label keep:label :}
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E keep X64CODE:JCC,
   X64ASM:RCX DATA-REG HND-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE inner X64CODE:JCC,
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,      \ the root: rcx = HND = 0 clears it
   keep X64CODE:JMP,
   inner X64CODE:LBL,  RETHROW,
   keep X64CODE:LBL, ;

\ Pop an xt and call it under a catch frame, leaving 0 in rax when it returns
\ and its throw's code when it throws: the twin of habu1.f BCATCH, which
\ forwards a halt. CAUGHT-HELD, is BCATCH-HELD's twin, which does not, for the
\ body and cleanup of finally and c2-invoke: they take the halt into their own
\ frame and resume it after their cleanup.
: CAUGHT, ( -- )
   X64CODE:LBL {: done:label :}
   CATCH-RUN, {: resume:label :}
   done X64CODE:JMP,
   resume X64CODE:LBL,  CATCH-FORWARD,
   done X64CODE:LBL, ;

: CAUGHT-HELD, ( -- )  CATCH-RUN, X64CODE:LBL, ;

\ The record writers reject before their first store. Literal codes let a
\ host without src/lib/error.f's words build these entries.
: DPBAD-HELPER, ( -- )
   OVERLAY-DIE-LBL X64CODE:LBL,
   X64ASM:RAX -9852 IMM64,  THROW,                      \ E-OVERLAY-OPEN
   DPBAD-LBL X64CODE:LBL,
   X64ASM:RAX -9850 IMM64,  THROW, ;                    \ E-DATA-SPACE

\ (LEXITHOOK): the exit actions, called with the exit code in rdi; it keeps rdi
\ and the VM registers only. The vector and the owner cell are the main region's,
\ at DATA-VA; the handler chain it empties is this thread's. The thread that
\ claims EXIT-OWNER-CELL runs the vector, again on an action's `die`; any other
\ blocks for good, in a ppoll of no descriptors with no timeout.
: EXIT-HOOK-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: wait:label owned:label none:label end:label :}
   s" (LEXITHOOK)" EXIT-HOOK-LBL LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   EXIT-HOOK-LBL X64CODE:LBL,
   X64ASM:R11 SHARED-DATA,
   X64ASM:RAX ZERO-REG,
   DATA-REG X64ASM:R11 EXIT-OWNER-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E owned X64CODE:JCC,
   X64ASM:RAX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E owned X64CODE:JCC,
   wait X64CODE:LBL,
   X64ASM:RDI ZERO-REG,  X64ASM:RSI ZERO-REG,  X64ASM:RDX ZERO-REG,  X64ASM:R10 ZERO-REG,  X64ASM:R8 ZERO-REG,
   NR-POLL SYS,  wait X64CODE:JMP,
   owned X64CODE:LBL,
   X64ASM:RAX X64ASM:R11 EXIT-HOOK-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   X64ASM:RCX ZERO-REG,  X64ASM:RCX X64ASM:R11 EXIT-HOOK-CELL MOV-STORE,
   X64ASM:RCX DATA-REG HND-CELL MOV-STORE,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP
   none X64CODE:LBL,
   X64CODE:RET,
   end X64CODE:LBL, ;

\ Make helper labels before the sections that branch to them.
: HELPERS, ( -- )
   X64CODE:LBL LIVE-CELL !
   X64CODE:LBL DPBAD-CELL !  X64CODE:LBL X64RT:LGENIOOUT !
   X64CODE:LBL OVERLAY-DIE-CELL !
   X64CODE:LBL X64BOOT:LEXITHOOK !
   LIVE-HELPER,  INDEX-HELPERS,
   DPBAD-HELPER,  GENIO-HELPER,  EXIT-HOOK-HELPER,
   X64PROF:HELPERS, ;

\ Begin the selected code's new throw. RDX is the count when it landed;
\ move its kept record to the new count only when that record was its own.
\ RAX keeps the code. Writes RCX and R8, as habu1.f BFINALLY-COUNT does.
: FINALLY-COUNT, ( -- )
   X64CODE:LBL {: kept:label :}
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RCX DATA-REG THROWS-CELL MOV-STORE,
   X64ASM:R8 DATA-REG STAMP-CELL MOV-LOAD,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE kept X64CODE:JCC,
   X64ASM:RCX DATA-REG STAMP-CELL MOV-STORE,
   kept X64CODE:LBL, ;

\ finally ( xt xt -- ): run the body under CAUGHT-HELD, and then the cleanup
\ under another outside it; the cleanup's nonzero code replaces the body's only
\ when that is 0; then rethrow the code unless it is 0. Frame: [0] cleanup |
\ [CELL] code | [2*CELL] halt, the HALT-UNWIND-CELL value the body's throw
\ carried: the frame takes the cell and clears it while the cleanup runs, a
\ halt that begins inside the cleanup lands in the frame keeping the body's
\ code, and a halt taken here is set again and the code thrown on, even 0.
\ [3*CELL] keeps the throw count when the selected code landed.
: FINALLY, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: keep:label calm:label throw:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RSP 4 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX X64ASM:RSP 0 MOV-STORE,
   CAUGHT-HELD,
   X64ASM:RAX X64ASM:RSP CELL MOV-STORE,
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 3 CELL * MOV-STORE,
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 2 CELL * MOV-STORE,
   X64ASM:RCX ZERO-REG,  X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,
   X64ASM:RAX X64ASM:RSP 0 MOV-LOAD,  X64ASM:RAX PUSH,  CAUGHT-HELD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E keep X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE keep X64CODE:JCC,
   X64ASM:RAX X64ASM:RSP CELL MOV-STORE,                       \ the body's code was 0
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 3 CELL * MOV-STORE,
   keep X64CODE:LBL,
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E calm X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP 2 CELL * MOV-STORE,                   \ a halt begun in the cleanup
   calm X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 2 CELL * MOV-LOAD,
   X64ASM:RDX X64ASM:RSP 3 CELL * MOV-LOAD,
   X64ASM:RSP 4 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,      \ the halt taken, 0 when none
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE throw X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   throw X64CODE:LBL,  FINALLY-COUNT,  RETHROW,
   done X64CODE:LBL, ;

\ c2-invoke ( body cleanup finish -- ): habu1.f BC2-INVOKE's twin. Frame:
\ [0] cleanup | [CELL] finisher | [2*CELL] code | [3*CELL] halt |
\ [4*CELL] the selected code's throw count | [5*CELL] unused. The cleanup's
\ nonzero code replaces the body's only when that is 0. A halt is taken and
\ resumed as FINALLY, does, one begun inside the cleanup included, and the
\ finisher runs with the cell clear.
: C2-INVOKE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: clean:label calm:label throw:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RSP 6 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX X64ASM:RSP CELL MOV-STORE,
   X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP 0 MOV-STORE,
   CAUGHT-HELD,
   X64ASM:RAX X64ASM:RSP 2 CELL * MOV-STORE,
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 4 CELL * MOV-STORE,
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 3 CELL * MOV-STORE,
   X64ASM:RCX ZERO-REG,  X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,
   X64ASM:RAX X64ASM:RSP 0 MOV-LOAD,  X64ASM:RAX PUSH,  CAUGHT-HELD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E clean X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP 2 CELL * MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE clean X64CODE:JCC,
   X64ASM:RAX X64ASM:RSP 2 CELL * MOV-STORE,                   \ the body's code was 0
   X64ASM:RCX DATA-REG THROWS-CELL MOV-LOAD,  X64ASM:RCX X64ASM:RSP 4 CELL * MOV-STORE,
   clean X64CODE:LBL,
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E calm X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP 3 CELL * MOV-STORE,                   \ a halt begun in the cleanup
   X64ASM:RCX ZERO-REG,  X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,
   calm X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP 2 CELL * MOV-LOAD,  X64ASM:RAX PUSH,
   X64ASM:RAX X64ASM:RSP CELL MOV-LOAD, INVOKE,
   X64ASM:RAX X64ASM:RSP 2 CELL * MOV-LOAD,  X64ASM:RCX X64ASM:RSP 3 CELL * MOV-LOAD,
   X64ASM:RDX X64ASM:RSP 4 CELL * MOV-LOAD,
   X64ASM:RSP 6 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RCX DATA-REG HALT-UNWIND-CELL MOV-STORE,      \ the halt taken, 0 when none
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE throw X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   throw X64CODE:LBL,  FINALLY-COUNT,  RETHROW,
   done X64CODE:LBL, ;

: C2-INIT-STOW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bad:label copy:label done:label :}
   X64ASM:R8 POP,  X64ASM:R9 POP,  X64ASM:R10 POP,  X64ASM:R11 POP,  X64ASM:RDI POP,
   X64ASM:R11 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-LE bad X64CODE:JCC,
   X64ASM:R10 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-LE bad X64CODE:JCC,
   X64ASM:R9 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX DSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RCX X64ASM:RAX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RAX 8 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RCX 7 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RDX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-L bad X64CODE:JCC,
   X64ASM:RAX X64ASM:R8 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RCX X64ASM:R8 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R10 X64ASM:R8 24 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX X64ASM:R8 40 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX 2 IMM32,  X64ASM:RAX X64ASM:R8 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   copy X64CODE:LBL,
      X64ASM:RSI POP,
      X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC
      X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
      X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
      X64ASM:RSI X64ASM:RAX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE copy X64CODE:JCC,
   X64ASM:RSI POP,  X64ASM:R10 PUSH,  X64ASM:RDI PUSH,  done X64CODE:JMP,
   bad X64CODE:LBL,
   X64ASM:RAX -6101 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   THROW,
   done X64CODE:LBL, ;

: C2-RECORDS-STOW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: bad:label discard:label first:label copy:label cell-copy:label done:label exit-code:label :}
   X64ASM:R8 POP,  X64ASM:R9 POP,  X64ASM:R10 POP,  X64ASM:R11 POP,  X64ASM:RDI POP,
   X64ASM:R11 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-LE bad X64CODE:JCC,
   X64ASM:R10 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-LE bad X64CODE:JCC,
   X64ASM:R9 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX DSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX 24 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RCX X64ASM:RAX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RAX 8 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 X64ASM:RAX 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RCX 7 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-TEST-RI32  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:R9 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-L bad X64CODE:JCC,
   X64ASM:RDX 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-L bad X64CODE:JCC,
   X64ASM:RDX X64ASM:R8 40 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX ZERO-REG,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A bad X64CODE:JCC,
   X64ASM:RAX X64ASM:R8 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RCX X64ASM:R8 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR
   X64ASM:RAX X64ASM:R8 24 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX 2 IMM32,  X64ASM:RAX X64ASM:R8 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE first X64CODE:JCC,
   discard X64CODE:LBL,
      X64ASM:RSI POP,
      X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE discard X64CODE:JCC,
      done X64CODE:JMP,
   first X64CODE:LBL,
      X64ASM:RSI POP,
      X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC
      X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
      X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
      X64ASM:RSI X64ASM:RAX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE first X64CODE:JCC,
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   copy X64CODE:LBL,
      X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:R11 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:R11 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   cell-copy X64CODE:LBL,
      X64ASM:RSI X64ASM:RDX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RSI X64ASM:RAX 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:RDX 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      X64ASM:RAX 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE cell-copy X64CODE:JCC,
      X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE copy X64CODE:JCC,
   done X64CODE:LBL,
   X64ASM:RSI POP,  X64ASM:RSI POP,
   X64ASM:RAX X64ASM:R8 24 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX ZERO-REG,
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:RAX PUSH,  X64ASM:RDI PUSH,  exit-code X64CODE:JMP,
   bad X64CODE:LBL,
   X64ASM:RAX -6101 X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   THROW,
   exit-code X64CODE:LBL, ;

\ A file or closed text owns a pool stack. Its catch lives on the caller's
\ stack, with this frame above it; A holds the file xt or the text's address.
0 constant CLOSED-A
8 constant CLOSED-U
16 constant CLOSED-DSP
24 constant CLOSED-BASE
32 constant CLOSED-CAP
40 constant CLOSED-SEG
48 constant CLOSED-BYTES
STACK-ABI:CATCH-BYTES CELL + constant CLOSED-ARM-OFF

: CLOSED-ARM, ( bool -- )
   {: text:bool :}
   X64ASM:RDX X64ASM:RSP CLOSED-ARM-OFF CLOSED-SEG + MOV-LOAD,
   X64ASM:RDX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   X64ASM:RCX STACK-ABI:PAGE-BYTES IMM32,
   X64ASM:RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX X64ASM:RSP CLOSED-ARM-OFF CLOSED-A + MOV-LOAD,
   text if
      X64ASM:RAX PUSH,
      X64ASM:RAX X64ASM:RSP CLOSED-ARM-OFF CLOSED-U + MOV-LOAD,  X64ASM:RAX PUSH,
      X64ASM:RAX DATA-REG PROVIDED-XT:EVALUATE-CLOSED-CELL MOV-LOAD,
   then
   INVOKE,
   X64CODE:RET, ;

\ The idle stack's first cell links the pool. Mapping follows boot-x64's
\ guarded allocation: an inaccessible page lies on each side of the aligned
\ 64 KiB data extent. The source and caller descriptor wait in a machine frame
\ across mmap and across the guarded call. The loader or interpreter checks
\ the child stack's result; this emitter restores the caller and rethrows.
: CLOSED-CALL, ( bool -- )
   {: text:bool :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: have:label take:label map-fail:label done:label :}
   X64CODE:LBL {: arm:label :}
   text if s" evaluate-closed" else s" source-file-run" then TASK-GUARD,
   X64ASM:RSP CLOSED-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   text if X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP CLOSED-U MOV-STORE, then
   X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP CLOSED-A MOV-STORE,
   DSP X64ASM:RSP CLOSED-DSP MOV-STORE,
   X64ASM:RAX DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RSP CLOSED-BASE MOV-STORE,
   X64ASM:RAX DATA-REG STACK-ABI:CAP-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RSP CLOSED-CAP MOV-STORE,
   X64ASM:RAX DATA-REG CLOSED-FREE-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE have X64CODE:JCC,
   X64ASM:RDI ZERO-REG,
   X64ASM:RSI STACK-ABI:PAGE-BYTES 4 * IMM32,
   X64ASM:RDX 0 IMM32,
   X64ASM:R10 MAP-ANON-PRIVATE IMM32,
   X64ASM:R8 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,  X64ASM:C-B map-fail X64CODE:JCC,
   X64ASM:RDI X64ASM:RAX STACK-ABI:PAGE-BYTES 2 * 1- X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI STACK-ABI:PAGE-BYTES negate X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32
   X64ASM:RSI STACK-ABI:PAGE-BYTES IMM32,
   X64ASM:RDX PROT-RW IMM32,
   X64ASM:R10 MAP-ANON-PRIVATE-FIXED IMM32,
   X64ASM:R8 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  X64ASM:R9 ZERO-REG,
   NR-MMAP SYS,
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE map-fail X64CODE:JCC,
   take X64CODE:JMP,
   have X64CODE:LBL,
   X64ASM:RCX X64ASM:RAX 0 MOV-LOAD,
   X64ASM:RCX DATA-REG CLOSED-FREE-CELL MOV-STORE,
   take X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP CLOSED-SEG MOV-STORE,
   X64ASM:RAX arm X64CODE:MOVABS,  X64ASM:RAX PUSH,
   CAUGHT-HELD,
   X64ASM:RCX X64ASM:RSP CLOSED-BASE MOV-LOAD,
   X64ASM:RCX DATA-REG STACK-ABI:BASE-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RSP CLOSED-CAP MOV-LOAD,
   X64ASM:RCX DATA-REG STACK-ABI:CAP-CELL MOV-STORE,
   DSP X64ASM:RSP CLOSED-DSP MOV-LOAD,
   X64ASM:RDX X64ASM:RSP CLOSED-SEG MOV-LOAD,
   X64ASM:RCX DATA-REG CLOSED-FREE-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RDX 0 MOV-STORE,
   X64ASM:RDX DATA-REG CLOSED-FREE-CELL MOV-STORE,
   X64ASM:RSP CLOSED-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   RETHROW,
   done X64CODE:JMP,
   map-fail X64CODE:LBL,
   S\" hb: cannot map guarded VM stack\n" EXIT-STATUS:SYSTEM STDERR-EXIT,
   arm X64CODE:LBL,
   text CLOSED-ARM,
   done X64CODE:LBL, ;

\ The message and LF on fd 2 when its length is positive. The rc waits on the
\ machine stack, and the LF is a cell pushed there for its write.
: DIE-TEXT, ( -- )
   X64CODE:LBL {: quiet:label :}
   X64ASM:RAX POP,  X64ASM:RDX POP,  X64ASM:RSI POP,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE quiet X64CODE:JCC,
   X64ASM:RDI STDERR IMM32,  NR-WRITE SYS,
   X64ASM:RCX STR:LF IMM32,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDI STDERR IMM32,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX 1 IMM32,  NR-WRITE SYS,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP
   quiet X64CODE:LBL, ;

\ die ( ^u8 n n -- ): the text, then the exit actions, then the exit.
: DIE, ( -- )
   DIE-TEXT,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP  EXIT-HOOK-LBL X64CODE:CALL,
   0 RC-EXIT, ;

\ die-closed ( ^u8 n n -- ): the text, then the exit; no exit hook runs.
: DIE-CLOSED, ( -- )
   DIE-TEXT,  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP  0 RC-EXIT, ;

\ A defining word's CREATE calls the captured source-owned writer. The outer
\ keyword has its own hook/registrar tail; this primitive shares its row work.
: CREATE-BODY ( -- )
   s" create" TASK-GUARD,
   X64ASM:RAX DATA-REG CREATEP-CELL MOV-LOAD,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG ;

\ The scope lookup searches each wordlist with FIND-LBL, as the public search rows
\ do, so package and qualified names fold the same way.
32 constant SCOPE-FRAME
0 constant SF-TOKEN
8 constant SF-LEN
16 constant SF-COLON
24 constant SF-QUAL-PUB

\ bare-scope-bind ( ^u8 n -- ^n n ): the record a token binds, null for none,
\ and flags (1 = a seeded primitive). RAX holds the record from search to push.
: SCOPE-FIND-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: colon-scan:label colon-hit:label bare:label qual-scan:label
      qual-find:label pub:label enclosing-pri:label enclosing-pub:label
      global:label bound:label qual-bad:label seed-skip:label :}
   X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSP SCOPE-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI X64ASM:RSP SF-TOKEN MOV-STORE,  X64ASM:RSI X64ASM:RSP SF-LEN MOV-STORE,
   X64ASM:RAX -1 IMM64,  X64ASM:RAX X64ASM:RSP SF-COLON MOV-STORE,
   X64ASM:RCX ZERO-REG,
   colon-scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE bare X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX $3A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E colon-hit X64CODE:JCC,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  colon-scan X64CODE:JMP,
   colon-hit X64CODE:LBL,
   X64ASM:RCX X64ASM:RSP SF-COLON MOV-STORE,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bare X64CODE:JCC,
   X64ASM:R8 X64ASM:RCX 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R8 X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE bare X64CODE:JCC,
   qual-scan X64CODE:LBL,
   X64ASM:R8 X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE qual-find X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:R8 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX $3A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E qual-bad X64CODE:JCC,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC  qual-scan X64CODE:JMP,
   qual-find X64CODE:LBL,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   HEAD-ROW-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bound X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX REC-CODE MOV-LOAD,
   X64ASM:RDX X64ASM:RSP SF-QUAL-PUB MOV-STORE,
   X64ASM:RCX X64ASM:RSP SF-COLON MOV-LOAD,
   X64ASM:RDI X64ASM:RSP SF-TOKEN MOV-LOAD,
   X64ASM:RDI X64ASM:RDI X64ASM:RCX 1 1 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RSI X64ASM:RSP SF-LEN MOV-LOAD,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bound X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP SF-QUAL-PUB MOV-LOAD,
   X64ASM:RCX DATA-REG PKG-PUB-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE bound X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP SF-COLON MOV-LOAD,
   X64ASM:RDI X64ASM:RSP SF-TOKEN MOV-LOAD,
   X64ASM:RDI X64ASM:RDI X64ASM:RCX 1 1 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RSI X64ASM:RSP SF-LEN MOV-LOAD,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:RDX BOOT:GLOBAL-WL-CELL CELL@,  FIND-LBL X64CODE:CALL,
   bound X64CODE:JMP,
   qual-bad X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   bound X64CODE:JMP,
   bare X64CODE:LBL,
   X64ASM:RDI X64ASM:RSP SF-TOKEN MOV-LOAD,  X64ASM:RSI X64ASM:RSP SF-LEN MOV-LOAD,
   X64ASM:RDX DATA-REG PKG-PRI-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E pub X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bound X64CODE:JCC,
   pub X64CODE:LBL,
   X64ASM:RDX DATA-REG PKG-PUB-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E enclosing-pri X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bound X64CODE:JCC,
   enclosing-pri X64CODE:LBL,
   X64ASM:RDX DATA-REG ENCLOSING:PRI MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E enclosing-pub X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bound X64CODE:JCC,
   enclosing-pub X64CODE:LBL,
   X64ASM:RDX DATA-REG ENCLOSING:PUB MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E global X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bound X64CODE:JCC,
   global X64CODE:LBL,
   X64ASM:RDX BOOT:GLOBAL-WL-CELL CELL@,
   FIND-LBL X64CODE:CALL,
   bound X64CODE:LBL,
   X64ASM:RCX ZERO-REG,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E seed-skip X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   SCOPE-SEED-LBL X64CODE:CALL,
   X64ASM:RDX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE seed-skip X64CODE:JCC,
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-OR-RI8
   seed-skip X64CODE:LBL,
   X64ASM:RAX PUSH,  X64ASM:RCX PUSH,
   X64ASM:RSP SCOPE-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ parse-name follows the shared source cursor. Whitespace is every byte at
\ or below space; on exhaustion the previous token cells remain untouched.
: PARSE-NAME-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: skip:label scan:label found:label none:label done:label finish:label :}
   X64ASM:RAX DATA-REG INP-CELL MOV-LOAD,
   X64ASM:RDX DATA-REG INE-CELL MOV-LOAD,
   skip X64CODE:LBL,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE none X64CODE:JCC,
   X64ASM:RCX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RCX 32 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A found X64CODE:JCC,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  skip X64CODE:JMP,
   found X64CODE:LBL,
   X64ASM:RSI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI DATA-REG TKA-CELL MOV-STORE,
   scan X64CODE:LBL,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE done X64CODE:JCC,
   X64ASM:RCX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RCX 32 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-BE done X64CODE:JCC,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  scan X64CODE:JMP,
   done X64CODE:LBL,
   X64ASM:RAX DATA-REG INP-CELL MOV-STORE,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX DATA-REG TKL-CELL MOV-STORE,
   X64ASM:RAX TOKENS-CELL CELL@,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RAX TOKENS-CELL CELL!,
   X64ASM:RSI PUSH,  X64ASM:RCX PUSH,
   finish X64CODE:JMP,
   none X64CODE:LBL,
   X64ASM:RAX DATA-REG INP-CELL MOV-STORE,
   X64ASM:RAX PUSH,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX PUSH,
   finish X64CODE:LBL, ;

\ bare-tok-imm? binds as bare-scope-bind does and answers the bound record's
\ DNAME-IMM bit as 2.
: TOK-IMM-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL
   {: scan:label colon:label bare:label qual-scan:label qual-find:label
      pub:label enclosing-pri:label enclosing-pub:label global:label
      result:label miss:label done:label :}
   X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSP 32 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI X64ASM:RSP 0 MOV-STORE,  X64ASM:RSI X64ASM:RSP 8 MOV-STORE,
   X64ASM:RCX ZERO-REG,
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE bare X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX $3A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E colon X64CODE:JCC,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  scan X64CODE:JMP,
   colon X64CODE:LBL,
   X64ASM:RCX X64ASM:RSP 16 MOV-STORE,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bare X64CODE:JCC,
   X64ASM:R8 X64ASM:RCX 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R8 X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE bare X64CODE:JCC,
   qual-scan X64CODE:LBL,
   X64ASM:R8 X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE qual-find X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:R8 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX $3A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E miss X64CODE:JCC,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC  qual-scan X64CODE:JMP,
   qual-find X64CODE:LBL,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   HEAD-ROW-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E miss X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX REC-CODE MOV-LOAD,
   X64ASM:RDX X64ASM:RSP 24 MOV-STORE,
   X64ASM:RCX X64ASM:RSP 16 MOV-LOAD,
   X64ASM:RDI X64ASM:RSP 0 MOV-LOAD,
   X64ASM:RDI X64ASM:RDI X64ASM:RCX 1 1 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RSI X64ASM:RSP 8 MOV-LOAD,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE result X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP 24 MOV-LOAD,
   X64ASM:RCX DATA-REG PKG-PUB-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE miss X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP 16 MOV-LOAD,
   X64ASM:RDI X64ASM:RSP 0 MOV-LOAD,
   X64ASM:RDI X64ASM:RDI X64ASM:RCX 1 1 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RSI X64ASM:RSP 8 MOV-LOAD,
   X64ASM:RSI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:RDX BOOT:GLOBAL-WL-CELL CELL@,  FIND-LBL X64CODE:CALL,
   result X64CODE:JMP,
   bare X64CODE:LBL,
   X64ASM:RDI X64ASM:RSP 0 MOV-LOAD,  X64ASM:RSI X64ASM:RSP 8 MOV-LOAD,
   X64ASM:RDX DATA-REG PKG-PRI-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E pub X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE result X64CODE:JCC,
   pub X64CODE:LBL,
   X64ASM:RDX DATA-REG PKG-PUB-CELL MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E enclosing-pri X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE result X64CODE:JCC,
   enclosing-pri X64CODE:LBL,
   X64ASM:RDX DATA-REG ENCLOSING:PRI MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E enclosing-pub X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE result X64CODE:JCC,
   enclosing-pub X64CODE:LBL,
   X64ASM:RDX DATA-REG ENCLOSING:PUB MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E global X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE result X64CODE:JCC,
   global X64CODE:LBL,
   X64ASM:RDX BOOT:GLOBAL-WL-CELL CELL@,  FIND-LBL X64CODE:CALL,
   result X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E miss X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX REC-FLAGS MOV-LOAD,
   X64ASM:RAX 59 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RAX 2 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   done X64CODE:JMP,
   miss X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   done X64CODE:LBL,
   X64ASM:RAX PUSH,
   X64ASM:RSP 32 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ Rounds R8 + [rsp] / [rsp+8] once, half to even, into a double's bits in R8,
\ signed by R10. R8 is the integer part, RAX the exact remainder, RSI the
\ scale, RCX the exponent, RDI the guard bit and R9 nonzero when sticky. The
\ remainder is below 10^18 throughout.
: NUM-FLOAT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: right:label left:label shift:label kept:label ready:label
      low:label round:label pack:label sign:label positive:label :}
   X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-OR-RR  X64ASM:C-E sign X64CODE:JCC,             \ preserve signed zero
   X64ASM:RCX 52 IMM32,
   X64ASM:RDX $20000000000000 IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B left X64CODE:JCC,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI ZERO-REG,
   right X64CODE:LBL,
   X64ASM:R9 X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDI 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:R8 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE right X64CODE:JCC,
   round X64CODE:JMP,
   left X64CODE:LBL,
   X64ASM:RDX $10000000000000 IMM64,
   shift X64CODE:LBL,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE ready X64CODE:JCC,
   X64ASM:R8 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B kept X64CODE:JCC,
   X64ASM:R8 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   kept X64CODE:LBL,
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  shift X64CODE:JMP,
   ready X64CODE:LBL,
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RDI ZERO-REG,
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B low X64CODE:JCC,
   X64ASM:RDI 1 IMM32,
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   low X64CODE:LBL,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   round X64CODE:LBL,
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E pack X64CODE:JCC,
   X64ASM:R11 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R11 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:R11 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-OR-RR  X64ASM:C-E pack X64CODE:JCC,             \ ties retain an even low bit
   X64ASM:R8 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RDX $20000000000000 IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE pack X64CODE:JCC,
   X64ASM:R8 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8                      \ rounding carried out of 53 bits
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   pack X64CODE:LBL,
   X64ASM:RCX 1023 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   X64ASM:RCX 52 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RDX $FFFFFFFFFFFFF IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-AND-RR
   X64ASM:R8 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   sign X64CODE:LBL,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE positive X64CODE:JCC,
   X64ASM:RDX $8000000000000000 IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   positive X64CODE:LBL, ;

\ num-parse reads exactly the ARM reader's spelling: optional minus and hex
\ prefix, then digits; decimal alone admits a dot with at least one fraction
\ digit. The frame holds fraction, scale and dot state. Overflow is latched
\ while the rest of the token is scanned, so a complete over-bound spelling
\ returns the same failed triple as a syntax miss.
: NUM-PARSE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL
   {: no-sign:label no-hex:label scan:label dot:label
      letter:label upper:label digit:label frac:label
      hex:label step:label overflow:label finish:label
      integer:label failed:label done:label
      sign-done:label radix-done:label neg-done:label
      frac-over:label frac-next:label hex-step:label :}
   X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:RSP 3 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:R8 ZERO-REG,  X64ASM:R9 10 IMM32,  X64ASM:R10 1 IMM32,
   X64ASM:R11 ZERO-REG,  X64ASM:RCX ZERO-REG,
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE failed X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX [char] - X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE no-sign X64CODE:JCC,
   X64ASM:R10 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   no-sign X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE failed X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX [char] $ X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE no-hex X64CODE:JCC,
   X64ASM:R9 16 IMM32,
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   no-hex X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE failed X64CODE:JCC,
   X64ASM:RDX ZERO-REG,  X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE finish X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX [char] . X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E dot X64CODE:JCC,
   X64ASM:RAX [char] 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RAX [char] 9 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A letter X64CODE:JCC,
   X64ASM:RAX [char] 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  digit X64CODE:JMP,
   letter X64CODE:LBL,
   X64ASM:R9 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE failed X64CODE:JCC,
   X64ASM:RAX [char] a X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-B upper X64CODE:JCC,
   X64ASM:RAX [char] f X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A failed X64CODE:JCC,
   X64ASM:RAX [char] a 10 - X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  digit X64CODE:JMP,
   upper X64CODE:LBL,
   X64ASM:RAX [char] A X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RAX [char] F X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A failed X64CODE:JCC,
   X64ASM:RAX [char] A 10 - X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   digit X64CODE:LBL,
   X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE frac X64CODE:JCC,
   X64ASM:R9 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E hex X64CODE:JCC,
   X64ASM:RDX 1844674407370955161 IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A overflow X64CODE:JCC,
   X64ASM:C-NE step X64CODE:JCC,
   X64ASM:RAX 5 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A overflow X64CODE:JCC,
   step X64CODE:LBL,
   X64ASM:R8 X64ASM:R8 10 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  scan X64CODE:JMP,
   hex X64CODE:LBL,
   X64ASM:RDX $0FFFFFFFFFFFFFFF IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A overflow X64CODE:JCC,
   hex-step X64CODE:LBL,
   X64ASM:R8 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  scan X64CODE:JMP,
   overflow X64CODE:LBL,
   X64ASM:R11 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:R9 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E hex-step X64CODE:JCC,
   step X64CODE:JMP,
   dot X64CODE:LBL,
   X64ASM:R9 10 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE failed X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE failed X64CODE:JCC,
   X64ASM:RDX 1 IMM32,  X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX ZERO-REG,  X64ASM:RDX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX 1 IMM32,  X64ASM:RDX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  scan X64CODE:JMP,
   frac X64CODE:LBL,
   X64ASM:RDX 2 IMM32,  X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 $0CCCCCCCCCCCCCCC IMM64,
   X64ASM:RDX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A frac-over X64CODE:JCC,
   X64ASM:R11 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE frac-next X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX 10 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RDX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX 10 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RDX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   frac-next X64CODE:LBL,
   X64ASM:RCX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  scan X64CODE:JMP,
   frac-over X64CODE:LBL,
   X64ASM:R11 -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   frac-next X64CODE:JMP,
   finish X64CODE:LBL,
   X64ASM:R11 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE failed X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP 2 CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E integer X64CODE:JCC,
   X64ASM:RDX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E failed X64CODE:JCC,
   X64ASM:RDX $7FFFFFFFFFFFFFFF IMM64,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A failed X64CODE:JCC,
   NUM-FLOAT,
   X64ASM:RSP 3 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R8 PUSH,  X64ASM:RAX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RAX PUSH,  X64ASM:RAX PUSH,  done X64CODE:JMP,
   integer X64CODE:LBL,
   X64ASM:R9 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E radix-done X64CODE:JCC,
   X64ASM:RDX $7FFFFFFFFFFFFFFF IMM64,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE sign-done X64CODE:JCC,
   X64ASM:RDX $8000000000000000 IMM64,
   sign-done X64CODE:LBL,
   X64ASM:R8 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A failed X64CODE:JCC,
   radix-done X64CODE:LBL,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE neg-done X64CODE:JCC,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-NEG
   neg-done X64CODE:LBL,
   X64ASM:RSP 3 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R8 PUSH,  X64ASM:RAX ZERO-REG,  X64ASM:RAX PUSH,
   X64ASM:RAX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32  X64ASM:RAX PUSH,  done X64CODE:JMP,
   failed X64CODE:LBL,
   X64ASM:RSP 3 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX ZERO-REG,  X64ASM:RAX PUSH,  X64ASM:RAX PUSH,  X64ASM:RAX PUSH,
   done X64CODE:LBL, ;

\ The interpreter installs these entries in the linked DATA image.
: PROVIDED, ( n -- )
   ROW$ TASK-GUARD,
   X64ASM:RAX DATA-REG rot MOV-LOAD,
   X64ASM:RAX X64CODE:JMP-REG, ;

public

: CONTROL, ( -- )
   FLOORREC-PENDING @ if
      false FLOORREC-PENDING !
   else
      X64CODE:LBL FLOORREC-ENTRY !
   then
   X64CODE:LBL {: after-floor:label :}
   after-floor X64CODE:JMP,
   \ An access below the data stack faults before touching a cell. Empty the
   \ stack before throwing; a catch restores its own cursor.
   FLOORREC-LBL X64CODE:LBL,
   DSP DATA-REG STACK-ABI:BASE-CELL MOV-LOAD,
   X64ASM:RAX -9851 IMM64,  THROW,                      \ E-UNDERFLOW
   \ The fault entry, the twin of habu1.f EMIT-FAULT-REC: the crash handler
   \ resumes a faulting thread here with the code in rax.
   X64BOOT:LFAULTREC @ >LABEL X64CODE:LBL,  THROW,
   after-floor X64CODE:LBL,
   s" execute" [: X64ASM:RAX POP, INVOKE, ;] PRIM
   s" execute-floor" [: EXECUTE-FLOOR, ;] PRIM
   s" 2>r" [: 2>R, ;] PRIM
   s" 2r>" [: 2R>, ;] PRIM
   s" 2r@" [: 2R@, ;] PRIM
   s" catch" [: CAUGHT,  X64ASM:RAX PUSH, ;] PRIM
   s" throw" [: X64ASM:RAX POP,  NONZERO-CODE,  THROW, ;] PRIM
   s" halt-throw" [: X64ASM:RAX POP,  THROW, ;] PRIM
   s" finally" [: FINALLY, ;] PRIM
   s" c2-invoke" [: C2-INVOKE, ;] PRIM
   s" c2-init-stow" [: C2-INIT-STOW, ;] PRIM
   s" c2-records-stow" [: C2-RECORDS-STOW, ;] PRIM
   s" die" [: DIE, ;] PRIM
   s" die-closed" [: DIE-CLOSED, ;] PRIM
   s" source-file-run" [: false CLOSED-CALL, ;] PRIM
   s" evaluate" [: PROVIDED-XT:EVALUATE-CELL PROVIDED, ;] PRIM
   s" evaluate-closed" [: true CLOSED-CALL, ;] PRIM
   s" evaluate-range" [: PROVIDED-XT:EVALUATE-RANGE-CELL PROVIDED, ;]
      PRIM
   s" literal" [: PROVIDED-XT:LITERAL-CELL PROVIDED, ;] PRIM
   s" compile," [: PROVIDED-XT:COMPILE-CELL PROVIDED, ;] PRIM
   s" ;" [: PROVIDED-XT:END-CELL PROVIDED, ;] PRIM
   s" create" [: CREATE-BODY ;] PRIM
   s" parse-name" [: PARSE-NAME-BODY ;] PRIM
   s" num-parse" [: NUM-PARSE, ;] PRIM
   s" bare-tok-imm?" [: TOK-IMM-BODY ;] PRIM
   s" bare-scope-bind" [: SCOPE-FIND-BODY ;] PRIM
   s" scope-kind?" [: X64ASM:RAX POP,  SCOPE-KIND-LBL X64CODE:CALL,  X64ASM:RAX PUSH, ;] PRIM ;

private

\ The twins of habu1.f BATFETCH .. BFENCE. x86-64 orders memory as total store
\ order: a plain load already has LDAR's acquire order, but a plain store may
\ pass a later load, which STLR followed by LDAR forbids, so the store is the
\ implicitly locked xchg.

: ATOMIC-FETCH, ( -- )
   0 X64RT:G-POP
   X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   0 X64RT:G-PUSH ;

: ATOMIC-STORE, ( -- )
   1 X64RT:G-POP  0 X64RT:G-POP
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-XCHG-MR ;

\ A 32-bit load zero-extends.
: ATOMIC-U32-FETCH, ( -- )
   0 X64RT:G-POP
   0 X64ASM:>R32 X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   0 X64RT:G-PUSH ;

: ATOMIC-U32-STORE, ( -- )
   1 X64RT:G-POP  0 X64RT:G-POP
   0 X64ASM:>R32 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-XCHG32-MR ;

\ atomic-add ( n ^n -- n ), answering the value the cell held.
: ATOMIC-ADD, ( -- )
   1 X64RT:G-POP  0 X64RT:G-POP
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-XADD-MR
   0 X64RT:G-PUSH ;

\ atomic-cas ( a a ^a -- a ): the expected value in rax and the new one in
\ rdx. It answers the value the cell held, which cmpxchg leaves in rax whether
\ or not it matched.
: ATOMIC-CAS, ( -- )
   1 X64RT:G-POP  2 X64RT:G-POP  0 X64RT:G-POP
   X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR
   0 X64RT:G-PUSH ;

public

: ATOMICS, ( -- )
   s" atomic@" [: ATOMIC-FETCH, ;] PRIM
   s" atomic!" [: ATOMIC-STORE, ;] PRIM
   s" atomic-u32@" [: ATOMIC-U32-FETCH, ;] PRIM
   s" atomic-u32!" [: ATOMIC-U32-STORE, ;] PRIM
   s" atomic-add" [: ATOMIC-ADD, ;] PRIM
   s" atomic-cas" [: ATOMIC-CAS, ;] PRIM
   s" fence" [: X64CODE:ASM-SINK X64ASM:ENC-MFENCE ;] PRIM ;

\ x86-64 code spans end on any byte.
\ Writes at CP pad to a CODE-SLOT: int3 after code, zeros after a record's name.

private

$CC constant INT3                      \ the byte code-publish fills a slot's gap with

\ The trap's label, made by PUBLICATION, in the stream it emits it into.
variable SEAL-TRAP-CELL
: SEAL-TRAP-LBL ( -- label ) SEAL-TRAP-CELL @ >LABEL ;

\ Where a span or index a guard rejects goes.
: SEAL-TRAP, ( -- )
   SEAL-TRAP-LBL X64CODE:LBL,  EXIT-STATUS:SEAL-VIOLATION EXIT-GROUP, ;

\ Over the bounded wid in rdi: rsi = the policy bitmap's containing cell,
\ rdx = its bit. It clobbers rcx; shl takes the wid's low six bits.
: POLICY-BITS, ( -- )
   X64ASM:RSI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI 6 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RSI DATA-REG X64ASM:RSI CELL POLICY-BITS-OFF X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX 1 IMM32,  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-SHL-CL ;

\ With the seal set, branch when rdx names an OWNER-API wordlist, the
\ protected ones.
: OPEN-WID, ( label -- ) {: prot:label :}
   X64CODE:LBL {: open:label :}
   X64ASM:RAX DATA-REG SEAL-NDICT-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E open X64CODE:JCC,
   X64ASM:RDX OWNER-API-PUB-WID X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E prot X64CODE:JCC,
   X64ASM:RDX OWNER-API-PRI-WID X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E prot X64CODE:JCC,
   open X64CODE:LBL, ;

\ patch32 ( n n -- ): write through the alias of the RX target.
: PATCH32, ( -- )
   X64ASM:R8 POP,  X64ASM:R9 POP,                                    \ the address, the word
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:R9 X64ASM:R64>N X64ASM:>R32 X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR
   CODE-SYNC, ;

: SLOT-UP, ( X64ASM:r64 X64ASM:r64 -- ) {: dst:X64ASM:r64 src:X64ASM:r64 :}
   dst src CODE-SLOT 1- X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   dst CODE-SLOT negate X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8 ;

\ Branch to the label unless a whole cell fits between rax and rcx; r9 = rax + CELL.
: CELL-FITS, ( label -- ) {: tail:label :}
   X64ASM:R9 X64ASM:RAX CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R9 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A tail X64CODE:JCC, ;

\ code-publish's frame: the source, the destination, the length and the slot
\ past the span.
0 constant PUB-SRC
8 constant PUB-DST
16 constant PUB-LEN
24 constant PUB-SLOT
32 constant PUB-FRAME

\ code-publish ( ^u8 n n -- ): the twin of BCODEPUBLISH. The span is nonzero,
\ does not wrap and lies in [DBASE+DICT-SIZE, DBASE+REGION), and dst is CP: a
\ publication is an append. int3 fills [dst+len, slot).
: PUBLISH, ( -- )
   ENGINE-GPR:X64-CP X64ASM:>R64 {: cp:X64ASM:r64 :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: copy:label copied:label fill:label filled:label :}
   X64CODE:LBL X64CODE:LBL {: copy-tail:label fill-tail:label :}
   X64ASM:RSI POP,  X64ASM:RDI POP,  X64ASM:RDX POP,                        \ len, dst, src
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,    \ a window of nothing
   X64ASM:RAX X64ASM:RDI X64ASM:RSI 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA            \ rax = the span's end
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,     \ an unsigned wrap
   X64ASM:RCX DBASE-REG DICT-SIZE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,     \ below the code interval
   X64ASM:RCX DBASE-REG REGION X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,     \ past the region
   X64ASM:RDI cp X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,     \ not an append at CP
   X64ASM:RSP PUB-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDX X64ASM:RSP PUB-SRC MOV-STORE,  X64ASM:RDI X64ASM:RSP PUB-DST MOV-STORE,  X64ASM:RSI X64ASM:RSP PUB-LEN MOV-STORE,
   X64ASM:RDI X64ASM:RAX SLOT-UP,  X64ASM:RDI X64ASM:RSP PUB-SLOT MOV-STORE,
   X64ASM:RDX X64ASM:RSP PUB-SRC MOV-LOAD,  X64ASM:RDI X64ASM:RSP PUB-DST MOV-LOAD,  X64ASM:RCX X64ASM:RSP PUB-LEN MOV-LOAD,
   X64ASM:RDI WRITE-ALIAS,
   X64ASM:RAX ZERO-REG,
   copy X64CODE:LBL,
   copy-tail CELL-FITS,
   X64ASM:R8 X64ASM:RDX X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 X64ASM:RDI X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   copy X64CODE:JMP,
   copy-tail X64CODE:LBL,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE copied X64CODE:JCC,
   X64ASM:R8 X64ASM:RDX X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   8 X64ASM:>R8 X64ASM:RDI X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   copy-tail X64CODE:JMP,
   copied X64CODE:LBL,
   X64ASM:RCX X64ASM:RSP PUB-SLOT MOV-LOAD,
   X64ASM:RCX X64ASM:RSP PUB-DST X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM          \ rcx = slot - RX dst
   X64ASM:R8 INT3 $0101010101010101 * IMM64,                   \ int3 in every byte
   fill X64CODE:LBL,                                           \ int3 to the slot
   fill-tail CELL-FITS,
   X64ASM:R8 X64ASM:RDI X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   fill X64CODE:JMP,
   fill-tail X64CODE:LBL,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE filled X64CODE:JCC,
   8 X64ASM:>R8 X64ASM:RDI X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   fill-tail X64CODE:JMP,
   filled X64CODE:LBL,
   CODE-SYNC,
   cp X64ASM:RSP PUB-SLOT MOV-LOAD,                           \ the slot is claimed
   X64ASM:RSP PUB-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ RAX is the new published count. Keep all caller scratch registers intact.
\ The first mapped cell is the last number issued; slot cells follow it.
: OCC-COUNT, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: lower:label up:label up-done:label down:label done:label :}
   X64CODE:LBL {: hw:label :}
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDX DEF-OCC:PTR-CELL CELL@,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR
   X64ASM:C-B lower X64CODE:JCC,  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R8 X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   up X64CODE:LBL,
      X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE up-done X64CODE:JCC,
      X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC
      X64ASM:R8 X64ASM:RDX X64ASM:RCX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  up X64CODE:JMP,
   up-done X64CODE:LBL,
      X64ASM:R8 X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      done X64CODE:JMP,
   lower X64CODE:LBL,
      X64PROF:TRIM,
      X64ASM:R9 NDICT-HW-CELL CELL@,                     \ raise the high-water to the count this lowers from
      X64ASM:R9 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE hw X64CODE:JCC,   \ (layout.f NDICT-HW-CELL)
      NDICT-REG NDICT-HW-CELL CELL!,
   hw X64CODE:LBL,
      X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:R11 ZERO-REG,
   down X64CODE:LBL,
      X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
      X64ASM:R11 X64ASM:RDX X64ASM:RCX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  down X64CODE:JMP,
   done X64CODE:LBL,
   NDICT-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-POP
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-POP
   X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP ;

: RECORD-AT, ( X64ASM:r64 -- ) {: idx:X64ASM:r64 :}
   X64ASM:R8 idx DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R8 DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR ;

\ xref-retarget ( n n n -- ): the twin of BXREFRETARGET. The index names a live
\ record or the pending one, NDICT, and none past it; a length of 0 or above
\ CODE-SPAN:MAX traps. Through the alias the length, the cell after the code
\ cell, is stored first and the address last: under total store order a plain
\ store publishes after the one before it, where ARM64 needs STLR.
: RETARGET, ( -- )
   X64CODE:LBL X64CODE:LBL {: pending:label done:label :}
   X64ASM:RCX POP,  X64ASM:R9 POP,  X64ASM:R10 POP,                         \ the index, the length, the start
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX CODE-SPAN:MAX IMM32,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E pending X64CODE:JCC,
   X64ASM:R11 DATA-REG DEF-OCC:PTR-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:R11 X64ASM:RCX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDX X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R11 X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   pending X64CODE:LBL,
   X64ASM:RCX RECORD-AT,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:R9 X64ASM:R8 REC-CODE CELL + MOV-STORE,
   X64ASM:R10 X64ASM:R8 REC-CODE MOV-STORE,
   X64ASM:RDX X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R11 X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:R11 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RAX X64ASM:R11 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   done X64CODE:LBL,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ rep-mark ( n -- ): or DNAME-REP into record n's flags through its alias; n is
\ not bounds-checked.
: REP-MARK, ( -- )
   X64ASM:RCX POP,  X64ASM:RCX RECORD-AT,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX DNAME-REP IMM64,
   X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-MR ;

\ Copy rcx bytes from rsi to rdi, stepping both past them. Clobbers rax.
: COPY-BYTES, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   next X64CODE:LBL,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   0 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
   next X64CODE:JMP,
   done X64CODE:LBL, ;

\ Store rcx zero bytes from rdi, stepping it past them. Clobbers rax.
: ZERO-BYTES, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label done:label :}
   X64ASM:RAX ZERO-REG,
   next X64CODE:LBL,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   0 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
   next X64CODE:JMP,
   done X64CODE:LBL, ;

\ does-record's frame.
0 constant DR-ENTRY                    \ the clause's entry
8 constant DR-LEN                      \ its recorded length
16 constant DR-NAME-LEN                \ its name's length
24 constant DR-NAME                    \ the parent's name bytes
32 constant DR-PAD                     \ the name's bytes at CP, CODE-SLOT whole
48 constant DR-FRAME

\ Store the suffix's bytes at rdi and step past them.
: SUFFIX, ( -- )
   DOES-CLAUSE:SUFFIX$ {: sfx:str :}
   sfx STR:LEN {: u:n :}
   u 0 ?do
      X64ASM:RAX sfx STR:DATA i + c@ IMM32,
      0 X64ASM:>R8 X64ASM:RDI i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   loop
   X64ASM:RDI u X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ does-record ( n n -- ): the twin of DOES-REC:NATIVE-PRIM. The parent is the
\ record PEND-CELL points at, and its clause takes the slot past the pending
\ one, NDICT+1: the entry and length given, the parent's name and ";does" out of
\ line at CP, zero-padded to a CODE-SLOT multiple, and the parent's wid.
: DOES-RECORD, ( -- )
   ENGINE-GPR:X64-CP X64ASM:>R64 {: cp:X64ASM:r64 :}
   X64ASM:RSP DR-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP DR-LEN MOV-STORE,
   X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP DR-ENTRY MOV-STORE,
   X64ASM:R11 DATA-REG PEND-CELL MOV-LOAD,                    \ the parent's record
   X64ASM:R11 X64ASM:RCX X64ASM:RAX NAME-LEN,
   X64ASM:R11 X64ASM:RDX X64ASM:RAX NAME-AT,
   X64ASM:RCX DOES-CLAUSE:SUFFIX$ STR:LEN X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RCX X64ASM:RSP DR-NAME-LEN MOV-STORE,
   X64ASM:RDX X64ASM:RSP DR-NAME MOV-STORE,
   X64ASM:RAX X64ASM:RCX SLOT-UP,
   X64ASM:RAX X64ASM:RSP DR-PAD MOV-STORE,
   X64ASM:RSI X64ASM:RSP DR-NAME MOV-LOAD,
   X64ASM:RDI cp X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RCX X64ASM:RSP DR-NAME-LEN MOV-LOAD,  X64ASM:RCX DOES-CLAUSE:SUFFIX$ STR:LEN X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   COPY-BYTES,                                         \ the parent's name
   SUFFIX,
   X64ASM:RCX X64ASM:RSP DR-PAD MOV-LOAD,
   X64ASM:RCX X64ASM:RSP DR-NAME-LEN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
   ZERO-BYTES,                                         \ the zeros to the pad
   X64ASM:RDI NDICT-REG 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI RECORD-AT,
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RAX X64ASM:RSP DR-ENTRY MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-CODE MOV-STORE,
   X64ASM:RAX X64ASM:RSP DR-LEN MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-CODE CELL + MOV-STORE,
   X64ASM:RAX DNAME-EXT IMM64,
   X64ASM:RAX X64ASM:RSP DR-NAME-LEN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:RDI REC-FLAGS MOV-STORE,
   cp X64ASM:RDI REC-NAME MOV-STORE,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RDI REC-NAME CELL + MOV-STORE,
   X64ASM:RAX DATA-REG PEND-CELL MOV-LOAD,
   X64ASM:RAX X64ASM:RAX REC-WID MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-WID MOV-STORE,
   cp X64ASM:RSP DR-PAD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX 1 IMM64,  X64ASM:RAX CLAUSE-CELL CELL!,                \ an imported definer had no `does>`
   X64ASM:RSP DR-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

public

: PUBLICATION, ( -- )
   X64CODE:LBL {: trap-end:label :}
   X64CODE:LBL SEAL-TRAP-CELL !
   SEAL-TRAP,
   trap-end X64CODE:LBL,
   s" (X64-SEAL-TRAP)" SEAL-TRAP-LBL LABEL>N trap-end LABEL>N
      ENGINE-PRIMS:HELPER-REGISTER
   s" patch32" [: PATCH32, ;] PRIM
   s" code-publish" [: PUBLISH, ;] PRIM
   s" xref-retarget" [: RETARGET, ;] PRIM
   s" rep-mark" [: REP-MARK, ;] PRIM
   s" does-record" [: DOES-RECORD, ;] PRIM ;

private

: FIND-ARGS, ( -- )
   X64ASM:RDX X64ASM:R64>N X64RT:G-POP  X64ASM:RSI X64ASM:R64>N X64RT:G-POP  X64ASM:RDI X64ASM:R64>N X64RT:G-POP ;

\ `bare-search-wl ( ^u8 n n -- n )`, the twin of habu1.f BSWL: the code cell
\ of the name's record in one wordlist, or 0. An engine helper's wid
\ (OWNER-API-PRI-WID) is rejected before the search and a DNAME-INT record
\ answers 0, so `search-wl execute` never reaches an internal word.
: SEARCH-WL-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: none:label push:label :}
   FIND-ARGS,
   X64ASM:RDX OWNER-API-PRI-WID X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E none X64CODE:JCC,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E push X64CODE:JCC,
   X64ASM:RCX DNAME-INT IMM64,
   X64ASM:RCX X64ASM:RAX REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-TEST-MR  X64ASM:C-NE none X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   push X64CODE:JMP,
   none X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   push X64CODE:LBL,
   X64ASM:RAX X64ASM:R64>N X64RT:G-PUSH ;

\ `xref-search-wl ( ^u8 n n -- ^n )`, the twin of habu1.f BCOMPILERSWL:
\ the record itself, or 0, with neither rejection; its checker row admits it
\ only at a trusted boundary.
: XREF-SEARCH-WL-BODY ( -- )
   FIND-ARGS,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:R64>N X64RT:G-PUSH ;

public

: DICT-SEARCH, ( -- )
   s" bare-search-wl" [: SEARCH-WL-BODY ;] PRIM
   s" xref-search-wl" [: XREF-SEARCH-WL-BODY ;] PRIM ;

private

\ The linker moves the heap bounds with DP into its build space.
\ Reject rdi outside those bounds, keeping it for the diagnostic.
: DP-CHECK, ( -- )
   X64ASM:RAX BOOT:DP-FLOOR-CELL CELL@,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-L DPBAD-LBL X64CODE:JCC,
   X64ASM:RAX BOOT:DP-CEILING-CELL CELL@,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-G DPBAD-LBL X64CODE:JCC, ;

\ rax = the target checker's operation at offset n of its record; with no
\ record or no operation it branches to the label.
: DECL-TARGET, ( n label -- ) {: off:n absent:label :}
   X64ASM:RAX NCOMP-DISPATCH:TARGET-DECL-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E absent X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E absent X64CODE:JCC, ;

\ `allot ( n -- )`, the twin of habu1.f BALLOT: a positive count zeroes the
\ part of [old DP, new DP) below the release high-water (layout.f DPHW-CELL),
\ the space a lowered DP gave back, and leaves the fresh pages above it
\ untouched. A negative count hands the new DP's DATA offset to the target
\ checker's DP-CUT, ends the `,` run and raises the high-water to the old
\ DP instead. The checker call keeps no scratch register: rdi waits on the
\ machine stack and the old DP is read again from its cell.
: ALLOT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: grow:label zero:label done:label no-owner:label :}
   s" allot" TASK-GUARD,  X64ASM:RCX POP,  X64ASM:RDX DP-CELL CELL@,          \ rdx = old DP
   X64ASM:RDI X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  DP-CHECK,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE grow X64CODE:JCC,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH
   CHECKER-OWNER-ABI:DP-CUT-OFF no-owner DECL-TARGET,
   X64ASM:RCX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  X64ASM:RCX PUSH,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   no-owner X64CODE:LBL,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-POP
   X64ASM:RAX ZERO-REG,  X64ASM:RAX COMMA-RUN-CELL CELL!,
   X64ASM:RDX DP-CELL CELL@,  X64ASM:RDX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  X64ASM:RAX DPHW-CELL CELL@,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RAX DPHW-CELL CELL!,  done X64CODE:JMP,
   grow X64CODE:LBL,
   X64ASM:RAX DPHW-CELL CELL@,  X64ASM:RAX DATA-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMOVCC   \ rax = min(high-water, new DP)
   X64ASM:RCX ZERO-REG,
   zero X64CODE:LBL,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
   X64ASM:RCX X64ASM:R64>N X64ASM:>R8 X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-INC  zero X64CODE:JMP,
   done X64CODE:LBL,
   X64ASM:RDI DP-CELL CELL!, ;

\ `, ( n -- )`, the twin of habu1.f BCOMMA: it keeps the latest run of its
\ cells that starts at a created body. A cell written at the body start starts
\ a run, even where the last run ended; one written at the run's end extends it.
: COMMA-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: more:label grow:label done:label :}
   s" ," TASK-GUARD,  X64ASM:RCX POP,  X64ASM:RDX DP-CELL CELL@,
   X64ASM:RDI X64ASM:RDX CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  DP-CHECK,
   X64ASM:RCX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR  X64ASM:RDI DP-CELL CELL!,
   X64ASM:RAX COMMA-BASE-CELL CELL@,  X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE more X64CODE:JCC,
   X64ASM:RDX COMMA-RUN-CELL CELL!,  grow X64CODE:JMP,
   more X64CODE:LBL,
   X64ASM:RAX COMMA-END-CELL CELL@,  X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE done X64CODE:JCC,
   grow X64CODE:LBL,
   X64ASM:RDI COMMA-END-CELL CELL!,
   done X64CODE:LBL, ;

\ The heap rows move DP, so each begins with the task-live guard, as the
\ ARM64 bodies do. `,` and c, check the DP past their store before it lands.
: HEAP, ( -- )
   s" here" [: X64ASM:RAX DP-CELL CELL@,  X64ASM:RAX PUSH, ;] PRIM
   s" allot" [: ALLOT-BODY ;] PRIM
   s" align" [:
      s" align" TASK-GUARD,  X64ASM:RDI DP-CELL CELL@,
      X64ASM:RDI CELL 1- X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
      X64ASM:RDI CELL negate X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
      DP-CHECK,  X64ASM:RDI DP-CELL CELL!, ;] PRIM
   s" ," [: COMMA-BODY ;] PRIM
   s" c," [:
      s" c," TASK-GUARD,  X64ASM:RCX POP,  X64ASM:RDX DP-CELL CELL@,
      X64ASM:RDI X64ASM:RDX 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  DP-CHECK,
      X64ASM:RCX X64ASM:R64>N X64ASM:>R8 X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR  X64ASM:RDI DP-CELL CELL!, ;] PRIM ;

\ .s ( -- ): print each cell from the base up, one per line, and leave them.
\ The cursor lives in SSCR-CELL, as on ARM64: a device write calls an xt that
\ may clobber every scratch register.
: DOT-S-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: loop:label done:label :}
   X64ASM:RAX S0-CELL CELL@,  X64ASM:RAX SSCR-CELL CELL!,
   loop X64CODE:LBL,
   X64ASM:RAX SSCR-CELL CELL@,  X64ASM:RAX DSP X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64RT:G-PRINT9
   X64ASM:RAX SSCR-CELL CELL@,  X64ASM:RAX CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX SSCR-CELL CELL!,
   loop X64CODE:JMP,
   done X64CODE:LBL, ;

\ Every printer leaves through X64RT's G-OUT, which honours OUT-CELL.
: PRINTERS, ( -- )
   s" ." [: X64ASM:RAX POP,  X64RT:G-PRINT9 ;] PRIM
   s" u." [: X64ASM:RAX POP,  X64RT:G-PRINTU9 ;] PRIM
   s" .s" [: DOT-S-BODY ;] PRIM
   s" depth" [:
      X64ASM:RAX DSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:RAX DATA-REG S0-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
      X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8  X64ASM:RAX PUSH, ;] PRIM
   s" emit" [: X64ASM:RAX POP,  X64RT:G-EMITC ;] PRIM
   s" cr" [: X64ASM:RAX STR:LF IMM32,  X64RT:G-EMITC ;] PRIM
   s" space" [: X64ASM:RAX STR:SPACE IMM32,  X64RT:G-EMITC ;] PRIM
   s" type" [: X64ASM:RDX POP,  X64ASM:RSI POP,  X64RT:G-OUT ;] PRIM ;

\ Branch to the label unless rax is a live code entry, DBASE <= rax < CP,
\ unsigned: set-check's install window. It catches a wild
\ install, not a well-formed pointer into live code.
: WINDOW, ( label -- ) {: bad:label :}
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B bad X64CODE:JCC,
   X64ASM:RAX CP-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE bad X64CODE:JCC, ;

\ set-check ( xt -- ): 0 turns checking off: an armed hook is parked with its
\ preflight in the SUSP-HOOK pair, and both cells empty; while already off the
\ parked pair stands. Any other xt must lie in the window; arming the parked
\ hook with the preflight cell empty brings its preflight back, and every arm
\ ends the suspension. The rule is habu1.f BSETCHECK's.
: SET-CHECK-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: bad:label disarm:label kept:label armed:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E disarm X64CODE:JCC,
   bad WINDOW,
   X64ASM:RAX HOOK-CELL CELL!,
   X64ASM:RCX COMPILE-PREFLIGHT-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE armed X64CODE:JCC,
   X64ASM:RCX SUSP-HOOK-CELL CELL@,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE armed X64CODE:JCC,
   X64ASM:RCX SUSP-PREFLIGHT-CELL CELL@,  X64ASM:RCX COMPILE-PREFLIGHT-CELL CELL!,
   armed X64CODE:LBL,
   X64ASM:RCX ZERO-REG,  X64ASM:RCX SUSP-HOOK-CELL CELL!,  X64ASM:RCX SUSP-PREFLIGHT-CELL CELL!,
   done X64CODE:JMP,
   disarm X64CODE:LBL,
   X64ASM:RCX HOOK-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E kept X64CODE:JCC,
   X64ASM:RCX SUSP-HOOK-CELL CELL!,
   X64ASM:RCX COMPILE-PREFLIGHT-CELL CELL@,  X64ASM:RCX SUSP-PREFLIGHT-CELL CELL!,
   kept X64CODE:LBL,
   X64ASM:RAX HOOK-CELL CELL!,  X64ASM:RAX COMPILE-PREFLIGHT-CELL CELL!,
   done X64CODE:JMP,
   bad X64CODE:LBL,  S\" set-check: invalid checker xt\n" EXIT-STATUS:REJECTED STDERR-EXIT,
   done X64CODE:LBL, ;

\ set-preflight ( xt -- ): installs once. With the cell set, the same xt is
\ inert and any other rejected; with it empty, as `0 set-check` leaves it, the
\ xt must lie in the window.
: SET-PREFLIGHT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: invalid:label empty:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RCX COMPILE-PREFLIGHT-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E empty X64CODE:JCC,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E done X64CODE:JCC,
   S\" set-preflight: invalid or replaced hook\n" EXIT-STATUS:REJECTED STDERR-EXIT,
   empty X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E invalid X64CODE:JCC,
   invalid WINDOW,
   X64ASM:RAX COMPILE-PREFLIGHT-CELL CELL!,
   done X64CODE:JMP,
   invalid X64CODE:LBL,  S\" set-preflight: invalid hook\n" EXIT-STATUS:REJECTED STDERR-EXIT,
   done X64CODE:LBL, ;

\ The checker's hooks and the suspended pair live in sealed DATA cells, so a
\ direct store from the row is their only writer once the engine is sealed.
: HOOKS, ( -- )
   s" set-check" [: SET-CHECK-BODY ;] PRIM
   s" check@" [: X64ASM:RAX HOOK-CELL CELL@,  X64ASM:RAX PUSH, ;] PRIM
   s" set-preflight" [: SET-PREFLIGHT-BODY ;] PRIM ;

\ The twins of habu1.f BCPFETCH .. BDATAFETCH, BRBASE, BCPSET, BNDSET and
\ BNDAPPEND; a row that moves r14 keeps the index authoritative through HIDX.

\ Once a count lowers, a create record at or beyond its new end is retired.
\ The next definition may reuse that address, so does-patch must forget it.
: LASTC-TRIM, ( -- )
   X64CODE:LBL {: live:label :}
   X64ASM:RDX NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RDX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RCX LASTC-CELL CELL@,
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B live X64CODE:JCC,
   X64ASM:RCX ZERO-REG,  X64ASM:RCX LASTC-CELL CELL!,
   live X64CODE:LBL, ;

\ An exact already-resolved record pointer becomes a process-local reference.
: DEF-SELECT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: bad:label done:label :}
   X64ASM:RAX POP,
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B bad X64CODE:JCC,
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDX ZERO-REG,  X64ASM:RCX DREC IMM32,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE bad X64CODE:JCC,
   X64ASM:R8 DEF-OCC:PTR-CELL CELL@,
   X64ASM:R9 X64ASM:R8 X64ASM:RAX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RAX PUSH,  X64ASM:R9 PUSH,  done X64CODE:JMP,
   bad X64CODE:LBL,
   X64ASM:RAX DEF-OCC:E-SELECT IMM64,  THROW,
   done X64CODE:LBL, ;

\ Check the slot and number before touching any dictionary record byte.
: DEF-RESOLVE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: stale:label done:label :}
   X64ASM:R10 POP,  X64ASM:RAX POP,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E stale X64CODE:JCC,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE stale X64CODE:JCC,
   X64ASM:R8 DEF-OCC:PTR-CELL CELL@,
   X64ASM:R9 X64ASM:R8 X64ASM:RAX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE stale X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX PUSH,  done X64CODE:JMP,
   stale X64CODE:LBL,
   X64ASM:RAX DEF-OCC:E-STALE IMM64,  THROW,
   done X64CODE:LBL, ;

\ A successful code rewind retires any published span that reaches its floor.
: DEF-CODE-RECLAIM-BODY ( -- )
   X64PROF:RECLAIM,
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: scan:label next:label span:label done:label :}
   X64ASM:RCX ZERO-REG,
   scan X64CODE:LBL,
      X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
      X64ASM:RCX RECORD-AT,
      X64ASM:R10 X64ASM:R8 REC-WID X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:R10 WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E span X64CODE:JCC,
      X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S next X64CODE:JCC,
   span X64CODE:LBL,
      X64ASM:R9 X64ASM:R8 REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:R9 CP-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE next X64CODE:JCC,
      X64ASM:R10 X64ASM:R8 REC-CODE CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:R9 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
      X64ASM:R9 X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE next X64CODE:JCC,
      X64ASM:RAX DEF-OCC:PTR-CELL CELL@,
      X64ASM:R11 ZERO-REG,
      X64ASM:R11 X64ASM:RAX X64ASM:RCX CELL CELL X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   next X64CODE:LBL,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  scan X64CODE:JMP,
   done X64CODE:LBL, ;

\ The source owner observes newly published dictionary rows and forwards to a
\ distinct target owner during checker replacement. Older owners have no
\ callback at this appended offset.
: RECORD-PUBLISHED, ( -- )
   X64CODE:LBL {: done:label :}
   X64ASM:RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX CHECKER-OWNER-ABI:HEADER-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RCX X64ASM:RDX 0 MOV-LOAD,
   X64ASM:RDI CHECKER-OWNER-ABI:MAGIC IMM64,
   X64ASM:RCX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE done X64CODE:JCC,
   X64ASM:RCX X64ASM:RDX 8 MOV-LOAD,
   X64ASM:RCX CHECKER-OWNER-ABI:RECORD-PUBLISHED-OFF CELL + X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B done X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX CHECKER-OWNER-ABI:RECORD-PUBLISHED-OFF MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RDI NDICT-REG -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RDI PUSH,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   done X64CODE:LBL, ;

\ ndict! ( n -- ): its callers lower the count only on the main thread with no
\ task live, to a count read there earlier. The index keeps the retired
\ records, whose probe skips a record at or past the count.
: NDICT-SET-BODY ( -- )
   X64ASM:RAX POP,
   OCC-COUNT,
   LASTC-TRIM, ;

\ ndict-append ( n -- ): count the compiler's pending record, which must
\ be record n at the count, or its does> companion one record past it while
\ a does> body is pending, and index it. Anything else exits SEAL-VIOLATION.
: NDICT-APPEND-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: owner:label bad:label done:label :}
   s" ndict-append" TASK-GUARD,
   X64ASM:RAX 0 PEEK,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX DICT-CAP X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-AE bad X64CODE:JCC,
   X64ASM:RCX NCOMP-DISPATCH:COMP-LATCH-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RDX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR                   \ rdx = record n
   X64ASM:RCX PEND-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E owner X64CODE:JCC,
   X64ASM:RCX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RCX CLAUSE-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   owner X64CODE:LBL,
   DROP,
   X64ASM:RAX NDICT-REG 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   OCC-COUNT,
   HIDX-ADD,
   RECORD-PUBLISHED,
   done X64CODE:JMP,
   bad X64CODE:LBL,
   s" E-SEAL-VIOLATION habu: ndict-append index not NDICT and the pending native record, or at or past DICT-CAP:"
   EXIT-STATUS:SEAL-VIOLATION X64ASM:RAX X64CRASH:DIE-LINE-VALUE,
   done X64CODE:LBL, ;

: DICT-COUNT, ( -- )
   s" cp@" [: CP-REG PUSH, ;] PRIM
   s" dbase@" [: DBASE-REG PUSH, ;] PRIM
   s" data-base" [: DATA-REG PUSH, ;] PRIM
   s" NULL-PTR" [: X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX PUSH, ;] PRIM
   s" rbase" [: X64ASM:RAX RBASE-CELL CELL@,  X64ASM:RAX PUSH, ;] PRIM
   s" ndict@" [: NDICT-REG PUSH, ;] PRIM
   s" cp!" [:
      X64ASM:RCX POP,
      X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      DEF-CODE-RECLAIM-BODY
      CP-REG X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;] PRIM
   s" ndict!" [: NDICT-SET-BODY ;] PRIM
   s" ndict-append" [: NDICT-APPEND-BODY ;] PRIM
   s" def-occ-select" [: DEF-SELECT-BODY ;] PRIM
   s" def-occ-resolve" [: DEF-RESOLVE-BODY ;] PRIM ;

\ The twin of habu2.f BDRAINPRETRUST. The pending pre-trust defer table at
\ PD-TABLE-OFF is a count and PD-SLOT byte slots, each a name and a signature
\ with their lengths.

: PD-COUNT ( -- X64ASM:mem ) DATA-REG PD-TABLE-OFF X64ASM:MEM-OFF ;

\ r8 = the base of the pending slot whose index rcx holds.
: SLOT-BASE, ( -- )
   X64ASM:R8 X64ASM:RCX PD-SLOT X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI32
   X64ASM:R8 DATA-REG X64ASM:R8 1 PD-TABLE-OFF PD-SLOTS-REL + X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA ;

\ r8 = the top pending slot's base. The count is read again from DATA each
\ time, since a checker call keeps no scratch register. It clobbers rcx.
: TOP-SLOT, ( -- )
   X64ASM:RCX PD-COUNT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
   SLOT-BASE, ;

\ Push a string of the slot at r8: the address of its bytes, at the first
\ offset, and its length, the cell at the second.
: PUSH-SLOT, ( n n -- ) {: at:n len:n :}
   X64ASM:RCX X64ASM:R8 at X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RCX PUSH,
   X64ASM:RCX X64ASM:R8 len X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RCX PUSH, ;

\ DRAIN-PRETRUST ( -- ): replay the table from the top: the target checker's
\ trust-decl with the slot's name and signature, then its checker-defer with
\ the name, then drop the slot. No trust-decl exits EXIT-STATUS:INTERNAL naming it on
\ fd 2; no checker-defer ends the drain with the slot still pending, as the
\ ARM64 twin does.
: DRAIN-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label absent:label done:label :}
   next X64CODE:LBL,
   X64ASM:RCX PD-COUNT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   NCOMP-DISPATCH:DECL-EFFECT-OFF absent DECL-TARGET,
   TOP-SLOT,
   PD-NAME-OFF PD-NLEN-OFF PUSH-SLOT,
   PD-SIG-OFF PD-SLEN-OFF PUSH-SLOT,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-TARGET,
   TOP-SLOT,
   PD-NAME-OFF PD-NLEN-OFF PUSH-SLOT,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RCX PD-COUNT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
   X64ASM:RCX PD-COUNT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   next X64CODE:JMP,
   absent X64CODE:LBL,
   S\" trust-decl\n" EXIT-STATUS:INTERNAL STDERR-EXIT,
   done X64CODE:LBL, ;

: SEAL-ROWS, ( -- )
   s" DRAIN-PRETRUST" [: DRAIN-BODY ;] PRIM ;

\ wide-mark ( -- ) and imm-mark ( -- ): set the flag, DNAME-WIDE or DNAME-IMM,
\ on the newest record, r14 - 1, through the alias, like BWIDEMARK and
\ C-IMMEDIATE on ARM.
: NEWEST-MARK-BODY ( n -- ) {: flag:n :}
   X64ASM:R8 NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R8 DBASE-REG X64ASM:R8 1 DREC negate X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX flag IMM64,
   X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-MR ;

: WIDE-MARK-BODY ( -- ) DNAME-WIDE NEWEST-MARK-BODY ;

\ The design seal is enforced by the captured interpreter. Admission
\ checks the same package row ARM64 reads, asks INTERPRETER's one
\ dispatch-keyword table for a collision, and publishes the bit only after both
\ checks pass. A record's notes read what the body pushed on rsp, FAULT-OPEN,
\ having moved rsp FAULT-FRAME below it.
: POLICY-ARG, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 off:n :}
   r X64ASM:RSP FAULT-RECORD:FAULT-FRAME off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;

\ Note `in` the word whose code holds the lr at rsp + off, read from the records
\ at r13, and help; never returns.
: POLICY-STOP, ( FAULT-RECORD:record str n -- ) {: h:str lr:n :}
   X64ASM:RDI lr POLICY-ARG,
   X64ASM:RSI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64CRASH:FAULT-IN,
   h EXIT-STATUS:POLICY X64CRASH:FAULT-STOP, ;

\ Unsealed it falls through, having written rax only.
: POLICY-OPEN, ( -- )
   X64CODE:LBL {: open:label :}
   X64ASM:RAX POLICY-NDICT-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E open X64CODE:JCC,
      s" E-POLICY" s" the vocabulary is sealed" X64CRASH:FAULT-OPEN,
      s" admit every package before `VOCABULARY:RESTRICT`" 0 POLICY-STOP,
   open X64CODE:LBL, ;

: POLICY-ADMIT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: found:label bounded:label checked:label :}
   POLICY-OPEN,
   X64ASM:RSI POP,  X64ASM:RDI POP,                                \ length, name
   X64ASM:RDX BOOT:NAMESPACE-WL-CELL CELL@,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE found X64CODE:JCC,
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH
      s" E-POLICY" s" no package has the name the program admits" X64CRASH:FAULT-OPEN,
      X64ASM:RSI 0 POLICY-ARG,  X64ASM:RDX 8 POLICY-ARG,
      s" package" X64ASM:RSI X64ASM:RDX X64CRASH:FAULT-TEXT,
      s" admit a package the program defines" 16 POLICY-STOP,
   found X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX POLICY-WID-MAX X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-B bounded X64CODE:JCC,
      s" E-POLICY habu: policy: package wid above the bound" EXIT-STATUS:POLICY
      X64CRASH:DIE-LINE,
   bounded X64CODE:LBL,
   X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX PUSH,                                         \ callback (wid -- name len hit)
   X64ASM:RCX POLICY-ABI:KEYWORD-CELL CELL@,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RAX POP,  X64ASM:RDX POP,  X64ASM:RSI POP,                     \ hit, length, keyword
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E checked X64CODE:JCC,
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-PUSH
      s" E-POLICY" s" a package publishes a word spelled like a keyword" X64CRASH:FAULT-OPEN,
      X64ASM:RSI 32 POLICY-ARG,  X64ASM:RDX 24 POLICY-ARG,
      s" package" X64ASM:RSI X64ASM:RDX X64CRASH:FAULT-TEXT,
      X64ASM:RSI 8 POLICY-ARG,  X64ASM:RDX 0 POLICY-ARG,
      s" keyword" X64ASM:RSI X64ASM:RDX X64CRASH:FAULT-TEXT,
      s" rename the word in that package" 40 POLICY-STOP,
   checked X64CODE:LBL,
   X64ASM:RDI X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   POLICY-BITS,
   X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSP 24 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

: POLICY-SEAL-BODY ( -- )
   POLICY-OPEN,
   NDICT-REG POLICY-NDICT-CELL CELL!, ;

: WORDLIST-ROWS, ( -- )
   s" wordlist" [:
      OVERLAY-GUARD,
      X64ASM:RAX WIDN-CELL CELL@,  X64ASM:RAX PUSH,
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RAX WIDN-CELL CELL!, ;] PRIM
   s" get-current" [: X64ASM:RAX CUR-CELL CELL@,  X64ASM:RAX PUSH, ;] PRIM
   s" policy-admit" [: POLICY-ADMIT-BODY ;] PRIM
   s" policy-seal" [: POLICY-SEAL-BODY ;] PRIM
   s" wide-mark" [: WIDE-MARK-BODY ;] PRIM ;

\ Capture needs address-cell declarations at fixed x86-64 addresses too.

\ The rows are authoritative; the index, rebuilt from them, finds a cell's row
\ by its DATA offset: a slot holds the row's ordinal + 1, 0 when empty.
variable MARK-CELL
: MARK-LBL ( -- label ) MARK-CELL @ >LABEL ;

\ (MARK)'s frame: the key (the cell's offset), its kind, a new mapping, its cells
\ and bytes, the index replaced, the rows indexed, the bucket, the table's base.
0 constant MARK-KEY
8 constant MARK-KIND
16 constant MARK-NEW
24 constant MARK-CAP
32 constant MARK-LEN
40 constant MARK-OLD
48 constant MARK-ROW
56 constant MARK-BUCKET
64 constant MARK-BASE
72 constant MARK-FRAME

\ r11 = the table's header; a syscall clobbers r11.
: MARK-HEADER, ( -- )
   X64ASM:R11 X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:R11 X64ASM:R11 RELOC:XTCELL-N-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA ;

\ rdx = the rows of the table whose header is r11, r8 = their count.
: MARK-ROWS, ( -- )
   X64CODE:LBL {: mapped:label :}
   X64ASM:R8 X64ASM:R11 0 MOV-LOAD,
   X64ASM:RDX X64ASM:R11 ADDRESS-CELLS:BASE-FIELD MOV-LOAD,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:MODE-FIELD MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE mapped X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP MARK-BASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   mapped X64CODE:LBL, ;

\ Tasks share the table: its lock is held through lookup, growth and publication.
: MARK-LOCK, ( -- )
   X64CODE:LBL {: retry:label :}
   retry X64CODE:LBL,
   X64ASM:RAX 1 IMM32,
   X64ASM:R11 X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:LOCK-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-XCHG-MR
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE retry X64CODE:JCC, ;

: MARK-UNLOCK, ( -- )
   X64ASM:RAX ZERO-REG,
   X64ASM:R11 X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:LOCK-CELL MOV-STORE, ;

\ r cells become the bytes of them and of a header of `head` bytes.
: CELL-BYTES, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 head:n :}
   r 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   r head X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ MARK-NEW = a mapping of MARK-CAP cells and a header of `head` bytes.
: MARK-MAP, ( n label -- ) {: head:n full:label :}
   X64ASM:RSI X64ASM:RSP MARK-CAP MOV-LOAD,
   X64ASM:RSI head CELL-BYTES,
   X64ASM:RSI X64ASM:RSP MARK-LEN MOV-STORE,
   MAP-ANON,  X64ASM:C-B full X64CODE:JCC,
   X64ASM:RAX X64ASM:RSP MARK-NEW MOV-STORE, ;

\ rsi = the index: its address and slot count are checked, not that it is mapped.
: INDEX-SHAPE, ( label -- ) {: bad:label :}
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE bad X64CODE:JCC,
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX 7 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX $7FFFFFFFFFFFFFFF ADDRESS-CELLS:INDEX-HEADER - IMM64,
   X64ASM:RSI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A bad X64CODE:JCC,
   X64ASM:RCX X64ASM:RSI ADDRESS-CELLS:INDEX-SLOTS MOV-LOAD,
   X64ASM:RCX 2 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-L bad X64CODE:JCC,
   X64ASM:RAX ADDRESS-CELLS:INDEX-MAX-SLOTS IMM64,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A bad X64CODE:JCC,
   X64ASM:RAX X64ASM:RCX -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RAX $7FFFFFFFFFFFFFFF IMM64,
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX ADDRESS-CELLS:INDEX-HEADER CELL-BYTES,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A bad X64CODE:JCC, ;

\ rsi = the index, rdx = the rows, r8 = their count, rdi = the key; all are kept.
\ A hit has rax = the row's kind, a miss rcx = the empty bucket; else it is bad.
: INDEX-PROBE, ( label label label -- ) {: hit:label miss:label bad:label :}
   X64CODE:LBL {: probe:label :}
   X64ASM:RAX $9E3779B97F4A7C15 IMM64,
   X64ASM:RCX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RAX 32 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   X64ASM:R10 X64ASM:RSI ADDRESS-CELLS:INDEX-SLOTS MOV-LOAD,
   X64ASM:R11 X64ASM:R10 -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-AND-RR
   X64ASM:R9 RELOC:XTCELL-OFF-MASK IMM64,
   probe X64CODE:LBL,
      X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
      X64ASM:RAX X64ASM:RSI X64ASM:RCX CELL ADDRESS-CELLS:INDEX-HEADER X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E miss X64CODE:JCC,
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC
      X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE bad X64CODE:JCC,
      X64ASM:RAX X64ASM:RDX X64ASM:RAX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      \ The key has no kind bits: they are what the row xor the key leaves.
      X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
      X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E hit X64CODE:JCC,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RCX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-AND-RR
      X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC  probe X64CODE:JMP, ;

\ MARK-CAP = the least power of two over 1 that holds r8 rows and one more at
\ half load.
: INDEX-SIZE, ( label -- ) {: full:label :}
   X64CODE:LBL X64CODE:LBL {: size:label sized:label :}
   X64ASM:RAX ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / 1- IMM64,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A full X64CODE:JCC,
   X64ASM:RAX X64ASM:R8 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:RCX ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / IMM64,
   X64ASM:R9 2 IMM32,
   size X64CODE:LBL,
      X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE sized X64CODE:JCC,
      X64ASM:R9 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A full X64CODE:JCC,
      X64ASM:R9 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  size X64CODE:JMP,
   sized X64CODE:LBL,
   X64ASM:R9 X64ASM:RSP MARK-CAP MOV-STORE, ;

\ Fills the index rsi from r8 rows at rdx; of two rows with one key the first
\ stays.
: INDEX-FILL, ( label -- ) {: bad:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: rows:label hole:label built:label :}
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RSP MARK-ROW MOV-STORE,
   rows X64CODE:LBL,
      X64ASM:RCX X64ASM:RSP MARK-ROW MOV-LOAD,
      X64ASM:RCX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE built X64CODE:JCC,
      X64ASM:RDI X64ASM:RDX X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RCX X64ASM:RSP MARK-ROW MOV-STORE,
      X64ASM:RAX RELOC:XTCELL-OFF-MASK IMM64,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-AND-RR
      rows hole bad INDEX-PROBE,
   hole X64CODE:LBL,
      X64ASM:RAX X64ASM:RSP MARK-ROW MOV-LOAD,
      X64ASM:RAX X64ASM:RSI X64ASM:RCX CELL ADDRESS-CELLS:INDEX-HEADER X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      rows X64CODE:JMP,
   built X64CODE:LBL, ;

\ Replaces the index rsi, 0 for none. The new one is complete before the old is
\ unmapped; a failure leaves the rows, the header and the old index whole.
: INDEX-BUILD, ( label label label label -- )
   {: lookup:label full:label bad:label discard:label :}
   X64CODE:LBL {: publish:label :}
   X64ASM:RSI X64ASM:RSP MARK-OLD MOV-STORE,
   full INDEX-SIZE,
   ADDRESS-CELLS:INDEX-HEADER full MARK-MAP,
   MARK-HEADER,  MARK-ROWS,
   X64ASM:RSI X64ASM:RSP MARK-NEW MOV-LOAD,
   X64ASM:RAX X64ASM:RSP MARK-CAP MOV-LOAD,  X64ASM:RAX X64ASM:RSI ADDRESS-CELLS:INDEX-SLOTS MOV-STORE,
   X64ASM:R8 X64ASM:RSI ADDRESS-CELLS:INDEX-COUNT MOV-STORE,
   bad INDEX-FILL,
   X64ASM:RDI X64ASM:RSP MARK-OLD MOV-LOAD,
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E publish X64CODE:JCC,
   X64ASM:RSI X64ASM:RDI ADDRESS-CELLS:INDEX-SLOTS MOV-LOAD,
   X64ASM:RSI ADDRESS-CELLS:INDEX-HEADER CELL-BYTES,
   NR-MUNMAP SYS,  X64ASM:C-B discard X64CODE:JCC,
   publish X64CODE:LBL,
   X64ASM:RSI X64ASM:RSP MARK-NEW MOV-LOAD,
   X64ASM:R11 X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:RSI X64ASM:R11 ADDRESS-CELLS:INDEX-CELL MOV-STORE,
   lookup X64CODE:JMP, ;

\ r11 = the header of the table whose row r8 is stored: publishes the row's slot,
\ then the index's count, then the table's.
: MARK-COMMIT, ( -- )
   X64ASM:RSI X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:RSI X64ASM:RSI ADDRESS-CELLS:INDEX-CELL MOV-LOAD,
   X64ASM:RCX X64ASM:RSP MARK-BUCKET MOV-LOAD,
   X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:R8 X64ASM:RSI X64ASM:RCX CELL ADDRESS-CELLS:INDEX-HEADER X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSI ADDRESS-CELLS:INDEX-COUNT MOV-STORE,
   X64ASM:R8 X64ASM:R11 0 MOV-STORE, ;

\ Copies the r8 rows at rdx to MARK-NEW, then unmaps them if they are a mapping.
: MARK-MOVE, ( label -- ) {: discard:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: copy:label copied:label moved:label :}
   X64ASM:RDI X64ASM:RSP MARK-NEW MOV-LOAD,
   X64ASM:RCX ZERO-REG,
   copy X64CODE:LBL,
      X64ASM:RCX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE copied X64CODE:JCC,
      X64ASM:RAX X64ASM:RDX X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64ASM:RDI X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC  copy X64CODE:JMP,
   copied X64CODE:LBL,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:MODE-FIELD MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E moved X64CODE:JCC,
   X64ASM:RDI X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI X64ASM:R11 ADDRESS-CELLS:CAP-FIELD MOV-LOAD,
   X64ASM:RSI 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   NR-MUNMAP SYS,  X64ASM:C-B discard X64CODE:JCC,
   MARK-HEADER,
   moved X64CODE:LBL, ;

\ Doubles the rows' r9 cells and stores the new row in them.
: MARK-GROW, ( label label label -- )
   {: full:label discard:label release:label :}
   X64ASM:RAX ADDRESS-CELLS:MAX-ROWS 2 / IMM64,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A full X64CODE:JCC,
   X64ASM:R9 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:R9 X64ASM:RSP MARK-CAP MOV-STORE,
   0 full MARK-MAP,
   MARK-HEADER,  MARK-ROWS,
   discard MARK-MOVE,
   X64ASM:RDI X64ASM:RSP MARK-NEW MOV-LOAD,
   X64ASM:RAX X64ASM:RSP MARK-KEY MOV-LOAD,
   X64ASM:RAX X64ASM:RSP MARK-KIND X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:RDI X64ASM:R8 CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDI X64ASM:R11 ADDRESS-CELLS:BASE-FIELD MOV-STORE,
   X64ASM:RAX X64ASM:RSP MARK-CAP MOV-LOAD,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:CAP-FIELD MOV-STORE,
   X64ASM:RAX 1 IMM32,  X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:MODE-FIELD MOV-STORE,
   MARK-COMMIT,  release X64CODE:JMP, ;

\ rdi = the cell, rsi = its kind. A rejected cell is never written: xt! and
\ ptr-cell! store after it returns.
: MARK-BODY, ( -- )
   X64CODE:LBL {: start:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL
   {: finish:label shape:label mapped:label ready:label lookup:label
      hit:label missing:label rebuild:label grow:label release:label
      discard:label full:label band:label kind:label :}
   start LABEL>N MARK-CELL !
   s" (MARK)" start LABEL>N finish LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start X64CODE:LBL,
   X64ASM:RSP MARK-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64CODE:LBL {: selected:label :}
   X64ASM:RAX SHARED-DATA,
   X64ASM:RCX BOOT:IMAGE-BASE-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E selected X64CODE:JCC,
   X64ASM:RDX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:R8 LINUX-LAYOUT:DATA-SIZE IMM64,
   X64ASM:RDX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE selected X64CODE:JCC,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   selected X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP MARK-BASE MOV-STORE,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX LINUX-LAYOUT:DATA-SIZE CELL - IMM64,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A band X64CODE:JCC,
   X64ASM:RDI X64ASM:RSP MARK-KEY MOV-STORE,
   X64ASM:RSI X64ASM:RSP MARK-KIND MOV-STORE,
   MARK-LOCK,
   MARK-HEADER,
   X64ASM:RAX X64ASM:R11 ADDRESS-CELLS:MAGIC-FIELD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX ADDRESS-CELLS:MAGIC IMM64,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE shape X64CODE:JCC,
   X64ASM:R8 X64ASM:R11 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 X64ASM:R11 ADDRESS-CELLS:CAP-FIELD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE shape X64CODE:JCC,
   X64ASM:RAX ADDRESS-CELLS:MAX-ROWS IMM64,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   X64ASM:R8 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   X64ASM:RDX X64ASM:R11 ADDRESS-CELLS:BASE-FIELD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R11 ADDRESS-CELLS:MODE-FIELD X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E mapped X64CODE:JCC,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE shape X64CODE:JCC,
   X64ASM:RAX LINUX-LAYOUT:DATA-SIZE IMM64,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   X64ASM:RAX X64ASM:RSP MARK-BASE MOV-LOAD,  X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   ready X64CODE:JMP,
   mapped X64CODE:LBL,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE shape X64CODE:JCC,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX 7 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8  X64ASM:C-NE shape X64CODE:JCC,
   X64ASM:RAX $7FFFFFFFFFFFFFFF IMM64,
   X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   ready X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP MARK-BASE MOV-LOAD,
   X64ASM:RSI X64ASM:RAX ADDRESS-CELLS:INDEX-CELL MOV-LOAD,
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E rebuild X64CODE:JCC,
   shape INDEX-SHAPE,
   X64ASM:RAX X64ASM:RSI ADDRESS-CELLS:INDEX-COUNT MOV-LOAD,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE rebuild X64CODE:JCC,
   X64ASM:RAX X64ASM:RSI ADDRESS-CELLS:INDEX-SLOTS MOV-LOAD,
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A shape X64CODE:JCC,
   lookup X64CODE:LBL,
   X64ASM:RDI X64ASM:RSP MARK-KEY MOV-LOAD,
   hit missing shape INDEX-PROBE,
   \ A cell declared again keeps one row: a second would relocate it twice.
   hit X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP MARK-KIND X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE kind X64CODE:JCC,
   release X64CODE:JMP,
   missing X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R9 X64ASM:RSI ADDRESS-CELLS:INDEX-SLOTS MOV-LOAD,
   X64ASM:R9 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A rebuild X64CODE:JCC,
   X64ASM:RCX X64ASM:RSP MARK-BUCKET MOV-STORE,
   MARK-HEADER,
   X64ASM:R9 X64ASM:R11 ADDRESS-CELLS:CAP-FIELD MOV-LOAD,
   X64ASM:R8 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E grow X64CODE:JCC,
   X64ASM:RDI X64ASM:RSP MARK-KIND X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RDI X64ASM:RDX X64ASM:R8 CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   MARK-COMMIT,  release X64CODE:JMP,
   rebuild X64CODE:LBL,
   lookup full shape discard INDEX-BUILD,
   grow X64CODE:LBL,
   full discard release MARK-GROW,
   release X64CODE:LBL,
   MARK-UNLOCK,
   X64ASM:RSP MARK-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64CODE:RET,
   discard X64CODE:LBL,
   X64ASM:RDI X64ASM:RSP MARK-NEW MOV-LOAD,  X64ASM:RSI X64ASM:RSP MARK-LEN MOV-LOAD,  NR-MUNMAP SYS,
   full X64CODE:LBL,
   \ Every branch here leaves the header, rows and index whole, so the throw is
   \ safe to catch.
   MARK-UNLOCK,
   X64ASM:RAX -9855 IMM64,  THROW,                      \ E-XTCELL-GROW
   \ The die line's lr is the return into the row that called (MARK).
   shape X64CODE:LBL,  X64ASM:RSP MARK-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   s" E-XTCELL habu: invalid address-cell storage header" EXIT-STATUS:ADDRESS-CELL
   X64CRASH:DIE-LINE,
   band X64CODE:LBL,  X64ASM:RSP MARK-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   s" E-XTBAND habu: address cell out of range" EXIT-STATUS:ADDRESS-CELL
   X64CRASH:DIE-LINE,
   kind X64CODE:LBL,  X64ASM:RSP MARK-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   s" E-XTKIND habu: address cell kind mismatch" EXIT-STATUS:ADDRESS-CELL
   X64CRASH:DIE-LINE,
   finish X64CODE:LBL,  X64CODE:DATA-AFTER, ;

\ A build routes header stores to the image and records their destination in
\ the mapped band below it. The store and its declaration then share a base.
: ROUTE-HEADER, ( -- )
   X64CODE:LBL {: own:label :}
   X64ASM:RCX 0 PEEK,
   X64ASM:R8 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R8 DATA-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:R9 DATA-START IMM64,
   X64ASM:R8 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE own X64CODE:JCC,
   X64ASM:R9 BOOT:IMAGE-BASE-CELL CELL@,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E own X64CODE:JCC,
   X64ASM:RCX X64ASM:R9 X64ASM:R8 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:R9 CELL negate MOV-STORE,
   X64ASM:R8 ROUTED-BYTES IMM64,
   X64ASM:R9 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R9 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX X64ASM:R9 0 MOV-STORE,
   X64ASM:RCX DSP CELL negate MOV-STORE,
   own X64CODE:LBL, ;

\ record-facts! ( n n -- ): its callers run on the main thread and pass an
\ index from the seal floor to NDICT.
: RECORD-FACTS-BODY ( -- )
   X64ASM:RAX POP,  X64ASM:R9 POP,
   X64ASM:RAX RECORD-AT,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:R9 X64ASM:R8 32 MOV-STORE, ;

\ The probe's answers in SHA256-CELL.
1 constant SHA256-YES
2 constant SHA256-NO

\ r9 = the answer. CPUID writes rbx, the data stack pointer, which waits in r8.
\ Leaf 7's EBX bit 29 is SHA; leaf 1's ECX bits 9 and 19 are SSSE3 and SSE4.1.
: SHA256-ASK, ( -- )
   X64CODE:LBL {: told:label :}
   X64ASM:R8 DSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R9 SHA256-NO IMM32,
   X64ASM:RAX ZERO-REG,  X64CODE:ASM-SINK X64ASM:ENC-CPUID
   X64ASM:RAX 7 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-B told X64CODE:JCC,
   X64ASM:RAX 1 IMM32,  X64CODE:ASM-SINK X64ASM:ENC-CPUID
   X64ASM:RCX $80200 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32
   X64ASM:RCX $80200 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-NE told X64CODE:JCC,
   X64ASM:RAX 7 IMM32,  X64ASM:RCX ZERO-REG,  X64CODE:ASM-SINK X64ASM:ENC-CPUID
   X64ASM:RBX $20000000 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-AND-RI32  X64ASM:C-E told X64CODE:JCC,
   X64ASM:R9 SHA256-YES IMM32,
   told X64CODE:LBL,
   DSP X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;

\ Passes on where the CPU has the instructions, else goes to the label. CPUID
\ costs more than a block's rounds, so the process asks once.
: SHA256-PROBE, ( label -- )
   {: none:label :}
   X64CODE:LBL {: known:label :}
   X64ASM:R11 SHARED-DATA,
   X64ASM:R9 X64ASM:R11 SHA256-CELL MOV-LOAD,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE known X64CODE:JCC,
   SHA256-ASK,
   X64ASM:R9 X64ASM:R11 SHA256-CELL MOV-STORE,
   known X64CODE:LBL,
   X64ASM:R9 SHA256-YES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE none X64CODE:JCC, ;

\ The constants, two words to a cell, then PSHUFB's byte swap of four words:
\ 34 little-endian cells.
34 CELL * constant SHA256-TABLE-LEN
SHA256-TABLE-LEN BUFFER: SHA256-TABLE

: TABLE-CELL! ( n n -- ) {: v:n k:n :}
   8 0 ?do  v i 8 * rshift $FF and  SHA256-TABLE k CELL * i + + c!  loop ;

: SHA256-TABLE-FILL ( -- )
   32 0 ?do
      KK i 2 * cells + @  KK i 2 * 1+ cells + @ 32 lshift or  i TABLE-CELL!
   loop
   $0405060700010203 32 TABLE-CELL!  $0C0D0E0F08090A0B 33 TABLE-CELL! ;
SHA256-TABLE-FILL

: SHA256-TABLE-BYTES ( -- str ) SHA256-TABLE SHA256-TABLE-LEN >str ;

16 constant XMM-BYTES
64 4 * constant SHA256-SWAP

\ The register of schedule quad k, xmm3-xmm6.
: SHA256-W ( n -- X64ASM:xmm ) 4 mod 3 + X64ASM:>XMM ;

: PSHUFD, ( X64ASM:xmm X64ASM:xmm n -- ) X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-PSHUFD-RRI8 ;

\ xmm1 = ABEF and xmm2 = CDGH, the first letter the high word, from the low
\ halves of rdi's cells.
: SHA256-LOAD, ( -- )
   X64ASM:XMM3 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   X64ASM:XMM4 X64ASM:RDI 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   X64ASM:XMM1 X64ASM:RDI 32 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   X64ASM:XMM2 X64ASM:RDI 48 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   5 1 do  i X64ASM:>XMM i X64ASM:>XMM $02 PSHUFD,  loop
   X64ASM:XMM1 X64ASM:XMM3 X64CODE:ASM-SINK X64ASM:ENC-PUNPCKLQDQ-RR
   X64ASM:XMM2 X64ASM:XMM4 X64CODE:ASM-SINK X64ASM:ENC-PUNPCKLQDQ-RR ;

\ Two of the register's words, chosen by the PSHUFD order, as cells k and k + 1.
: SHA256-PAIR, ( X64ASM:xmm n n -- )
   {: src:X64ASM:xmm order:n k:n :}
   X64ASM:XMM0 src order PSHUFD,
   X64ASM:XMM0 X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-PMOVZXDQ-RR
   X64ASM:XMM0 X64ASM:RDI k CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-MR ;

: SHA256-STORE, ( -- )
   X64ASM:XMM1 $0B 0 SHA256-PAIR,  X64ASM:XMM2 $0B 2 SHA256-PAIR,
   X64ASM:XMM1 $01 4 SHA256-PAIR,  X64ASM:XMM2 $01 6 SHA256-PAIR, ;

\ Quad k's register becomes quad k + 4.
: SHA256-SCHEDULE, ( n -- )
   {: k:n :}
   k SHA256-W k 1+ SHA256-W X64CODE:ASM-SINK X64ASM:ENC-SHA256MSG1-RR
   X64ASM:XMM7 k 3 + SHA256-W X64CODE:ASM-SINK X64ASM:ENC-MOVDQA-RR
   X64ASM:XMM7 k 2 + SHA256-W 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-PALIGNR-RRI8
   k SHA256-W X64ASM:XMM7 X64CODE:ASM-SINK X64ASM:ENC-PADDD-RR
   k SHA256-W k 3 + SHA256-W X64CODE:ASM-SINK X64ASM:ENC-SHA256MSG2-RR ;

\ Four rounds; r8 = the constants.
: SHA256-QUAD, ( n -- )
   {: k:n :}
   X64ASM:XMM0 X64ASM:R8 k XMM-BYTES * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   X64ASM:XMM0 k SHA256-W X64CODE:ASM-SINK X64ASM:ENC-PADDD-RR
   k 12 < if k SHA256-SCHEDULE, then
   X64ASM:XMM2 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-SHA256RNDS2-RR
   X64ASM:XMM0 X64ASM:XMM0 $0E PSHUFD,
   X64ASM:XMM1 X64ASM:XMM2 X64CODE:ASM-SINK X64ASM:ENC-SHA256RNDS2-RR ;

\ The block at rsi; xmm8 = the byte swap.
: SHA256-BLOCK, ( -- )
   4 0 ?do
      i SHA256-W X64ASM:RSI i XMM-BYTES * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
      i SHA256-W X64ASM:XMM8 X64CODE:ASM-SINK X64ASM:ENC-PSHUFB-RR
   loop
   X64ASM:XMM9 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-MOVDQA-RR  X64ASM:XMM10 X64ASM:XMM2 X64CODE:ASM-SINK X64ASM:ENC-MOVDQA-RR
   16 0 ?do  i SHA256-QUAD,  loop
   X64ASM:XMM1 X64ASM:XMM9 X64CODE:ASM-SINK X64ASM:ENC-PADDD-RR  X64ASM:XMM2 X64ASM:XMM10 X64CODE:ASM-SINK X64ASM:ENC-PADDD-RR ;

\ sha256-blocks ( ^n ^u8 n -- n )
: SHA256-BLOCKS-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: none:label next:label done:label table:label :}
   X64ASM:R10 POP,  X64ASM:RSI POP,  X64ASM:RDI POP,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-LE none X64CODE:JCC,
   none SHA256-PROBE,
   X64ASM:RDX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R8 table X64CODE:MOVABS,
   X64ASM:XMM8 X64ASM:R8 SHA256-SWAP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVDQU-RM
   SHA256-LOAD,
   next X64CODE:LBL,
   SHA256-BLOCK,
   X64ASM:RSI 64 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE next X64CODE:JCC,
   SHA256-STORE,
   done X64CODE:JMP,
   table SHA256-TABLE-BYTES X64CODE:DATA-AFTER
   none X64CODE:LBL,  X64ASM:RDX ZERO-REG,
   done X64CODE:LBL,  X64ASM:RDX PUSH, ;

: SCOPE-ROWS, ( -- )
   MARK-BODY,
   s" xt!" [:
      ROUTE-HEADER, X64ASM:RCX POP, X64ASM:RAX POP,
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
      X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI ZERO-REG,  MARK-LBL X64CODE:CALL,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP
      X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;] PRIM
   s" ptr-cell-mark" [:
      X64ASM:RDI POP,
      X64ASM:RSI RELOC:XTCELL-DATA-TAG IMM64,  MARK-LBL X64CODE:CALL, ;] PRIM
   s" scratch-cell-mark" [:
      X64ASM:RDI POP,
      X64ASM:RSI RELOC:XTCELL-DATA-TAG RELOC:XTCELL-SCRATCH-TAG or IMM64,
      MARK-LBL X64CODE:CALL, ;] PRIM
   s" ptr-cell!" [:
      ROUTE-HEADER, X64ASM:RCX POP, X64ASM:RAX POP,
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
      X64ASM:RDI X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI RELOC:XTCELL-DATA-TAG IMM64,  MARK-LBL X64CODE:CALL,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP
      X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;] PRIM
   s" record-facts!" [: RECORD-FACTS-BODY ;] PRIM
   s" sha256-blocks" [: SHA256-BLOCKS-BODY ;] PRIM
   s" tick-order@" [: X64ASM:RAX ZERO-REG,  X64ASM:RAX PUSH,  X64ASM:RAX PUSH, ;] PRIM ;

public

: ENGINE-STATE, ( -- )
   HEAP,  PRINTERS,  HOOKS,
   DICT-COUNT,  SEAL-ROWS,  WORDLIST-ROWS,  SCOPE-ROWS, ;

\ ffi-call and ffi-call-n: integer SysV calls through SYSV-CALL,. argbuf holds
\ at least eight cells, the contract ARM64's unconditional load of x0..x7 sets;
\ SysV takes six in registers, so the other two, and every cell past eight that
\ ffi-call-n passes, go on the stack.
\
\ ffi-call-abi(-r) and ffi-call-struct, the rows src/lib/ffi-abi.f
\ plans a call for: the integer arguments in argbuf, the doubles in fpbuf and
\ the stack arguments prepacked in stackbuf. rdi..r9 take argbuf[0..6),
\ xmm0..7 always take fpbuf[0..8), and the stack takes max(nstack, 0)
\ stackbuf cells and nothing else. argbuf[6..9) pass nowhere. No argument
\ carries a count of doubles, so al is VEC-REGS, the bound SysV admits.

private

6 constant REG-CELLS                    \ rdi rsi rdx rcx r8 r9
8 constant BUF-CELLS                    \ x0..x7
8 constant VEC-REGS                     \ xmm0..7

\ Outbound C calls publish a per-thread VM frame for callback-entry. A thread
\ pointer cannot name the free (0) or mover-busy (1) callback-row states.
0 constant CBO-ARG
8 constant CBO-COUNT
16 constant CBO-FN
24 constant CBO-CLAIM
32 constant CBO-XDS
40 constant CBO-DBASE
48 constant CBO-NDICT
56 constant CBO-CP
64 constant CBO-BYTES

public

\ The linked boot has established the VM registers and the main stacks.
: START-RUNTIME, ( -- )
   X64ASM:RSP MAIN-STACK-TOP-CELL CELL!,
   X64BOOT:CB-SELF,  X64ASM:RAX MAIN-THREAD-CELL CELL!,
   DBASE-REG DBASE-CELL CELL!, ;

private

\ The owner is claimed before any cursor is published. A nested outbound call
\ on the same thread retains the claim; another thread dies 106 with the region
\ untouched. Its record's `in` is the word at the primitive's return address,
\ n bytes above rsp at entry: FAULT-OPEN, moves rsp, so r12 carries it, and r13
\ and r14, which the record never writes, still name the records.
: CB-ENTER, ( n -- ) {: pushed:n :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: held:label busy:label done:label :}
   X64ASM:RSP CBO-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RAX X64ASM:RSP CBO-ARG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R10 X64ASM:RSP CBO-COUNT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R11 X64ASM:RSP CBO-FN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64BOOT:CB-SELF,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R8 DATA-REG CB-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E held X64CODE:JCC,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE busy X64CODE:JCC,
   X64ASM:RAX ZERO-REG,
   X64ASM:R9 X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE busy X64CODE:JCC,
   X64ASM:RCX 1 IMM32,  done X64CODE:JMP,
   held X64CODE:LBL,  X64ASM:RCX ZERO-REG,
   done X64CODE:LBL,
   X64ASM:RCX X64ASM:RSP CBO-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX DATA-REG CB-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RSP CBO-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX DATA-REG CB-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RSP CBO-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX DATA-REG CB-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RSP CBO-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX DATA-REG CB-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RSP CBO-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   DSP DATA-REG CB-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   DBASE-REG DATA-REG CB-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   NDICT-REG DATA-REG CB-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   CP-REG DATA-REG CB-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX X64ASM:RSP CBO-ARG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:RSP CBO-COUNT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R11 X64ASM:RSP CBO-FN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64CODE:LBL {: past:label :}
   past X64CODE:JMP,
   busy X64CODE:LBL,
   INDEX-REG X64ASM:RSP CBO-BYTES pushed + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   s" E-CALLBACK" FAULT-RECORD:CB-CONTEXT-BUSY$ X64CRASH:FAULT-OPEN,
   X64ASM:RDI INDEX-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RSI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64CRASH:FAULT-IN,
   FAULT-RECORD:CB-CONTEXT-BUSY-HELP$ EXIT-STATUS:CALLBACK X64CRASH:FAULT-STOP,
   past X64CODE:LBL, ;

\ Restore the previous frame before releasing a claim. x86 TSO stores give
\ the release ordering that ARM's STLR supplies in BCB-LEAVE.
: CB-LEAVE, ( -- )
   X64CODE:LBL {: kept:label :}
   X64ASM:R8 X64ASM:RSP CBO-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 DATA-REG CB-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSP CBO-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 DATA-REG CB-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSP CBO-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 DATA-REG CB-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSP CBO-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 DATA-REG CB-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSP CBO-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSP CBO-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E kept X64CODE:JCC,
   X64ASM:R8 ZERO-REG,
   X64ASM:R8 DATA-REG CB-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   kept X64CODE:LBL, ;

\ r10 = max(nargs, BUF-CELLS) - REG-CELLS, the stack cells, from nargs in r10;
\ signed.
: STACK-CELLS, ( -- )
   X64CODE:LBL {: wide:label :}
   X64ASM:R10 BUF-CELLS X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-GE wide X64CODE:JCC,
   X64ASM:R10 BUF-CELLS IMM32,
   wide X64CODE:LBL,
   X64ASM:R10 REG-CELLS X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8 ;

\ Load rdi rsi rdx rcx r8 r9 from the six cells a register points at, rcx
\ last, so the register may be rcx.
: REG-ARGS, ( X64ASM:r64 -- ) {: base:X64ASM:r64 :}
   X64ASM:RDI base 0 MOV-LOAD,  X64ASM:RSI base CELL MOV-LOAD,  X64ASM:RDX base 2 CELL * MOV-LOAD,
   X64ASM:R8 base 4 CELL * MOV-LOAD,  X64ASM:R9 base 5 CELL * MOV-LOAD,
   X64ASM:RCX base 3 CELL * MOV-LOAD, ;

\ With rax = argbuf, r10 = the stack cells and r11 = the function: load the
\ register arguments, point rax at the first stack cell, call, push the answer.
: BUF-CALL, ( -- )
   0 CB-ENTER,
   X64ASM:RAX REG-ARGS,
   X64ASM:RAX REG-CELLS CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   0 SYSV-CALL,
   CB-LEAVE,
   X64ASM:RAX PUSH, ;

\ ffi-call ( argbuf nargs fn -- ret ): eight cells, whatever nargs is.
: FFI-CALL-BODY ( -- )
   X64ASM:R11 POP,  DROP,  X64ASM:RAX POP,
   X64ASM:R10 BUF-CELLS REG-CELLS - IMM32,
   BUF-CALL, ;

\ ffi-call-n ( argbuf nargs fn -- ret ): max(nargs, 8) cells.
: FFI-CALL-N-BODY ( -- )
   X64ASM:R11 POP,  X64ASM:R10 POP,  X64ASM:RAX POP,
   STACK-CELLS,
   BUF-CALL, ;

\ The SysV register of integer argument n, 0 to 5.
: ARG-REG ( n -- X64ASM:r64 )
   dup 0 = if drop X64ASM:RDI exit then
   dup 1 = if drop X64ASM:RSI exit then
   dup 2 = if drop X64ASM:RDX exit then
   dup 3 = if drop X64ASM:RCX exit then
   4 = if X64ASM:R8 exit then
   X64ASM:R9 ;

\ The body of ffi-call-k ( x1 .. xk fn -- n ), k from 0 to 8: x1..x6, x1
\ the deepest cell, go to rdi..r9, and x7 and x8 to the machine stack
\ through SYSV-CALL,'s copy from rax. CB-ENTER, uses rcx, r8 and r9, so
\ the loads follow it; the frame it publishes holds the stack without the
\ cells, and every one is read before the call can call back.
: FFI-CALL-K-BODY ( n -- ) {: k:n :}
   X64ASM:R11 POP,
   k 0 > if  DSP k CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  then
   k REG-CELLS > if
      X64ASM:R10 k REG-CELLS - IMM32,
      X64ASM:RAX DSP REG-CELLS CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   else
      X64ASM:R10 ZERO-REG,
   then
   0 CB-ENTER,
   k REG-CELLS min 0 ?do  i ARG-REG DSP i CELL * MOV-LOAD,  loop
   0 SYSV-CALL,
   CB-LEAVE,
   X64ASM:RAX PUSH, ;

\ With r11 = the function, r10 = nstack and argbuf fpbuf stackbuf the top three
\ cells: pop them, load xmm0..7 from fpbuf and rdi..r9 from argbuf, and call
\ with the stackbuf cells. nstack is the caller's data, and SYSV-CALL, copies
\ r10 cells, so a negative count copies none, as BFFI-COPY-ABI-STACK skips.
: ABI-CALL, ( -- )
   X64CODE:LBL {: counted:label :}
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE counted X64CODE:JCC,
   X64ASM:R10 ZERO-REG,
   counted X64CODE:LBL,
   X64ASM:RAX POP,
   X64ASM:RCX POP,
   X64ASM:R9 POP,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-PUSH
   2 CELL * CB-ENTER,
   X64ASM:RCX X64ASM:RSP CBO-BYTES CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   VEC-REGS 0 ?do  i X64ASM:>XMM X64ASM:RCX i CELL * X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVSD-RM  loop
   X64ASM:RCX X64ASM:RSP CBO-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX REG-ARGS,
   VEC-REGS SYSV-CALL,
   CB-LEAVE,
   X64ASM:RSP 2 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ ffi-call-abi(-r) ( argbuf fpbuf stackbuf nstack fn -- n|r ).
: FFI-CALL-ABI-BODY ( -- )
   X64ASM:R11 POP,  X64ASM:R10 POP,
   ABI-CALL, ;

\ The -r rows' answer: xmm0's bits.
: XMM0-PUSH, ( -- ) X64ASM:RAX X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-MOVQ-RX  X64ASM:RAX PUSH, ;

public

: FFI, ( -- )
   s" dlopen-entry" [: s" dlopen" LIBC-ENTRY, X64ASM:RAX PUSH, ;] PRIM
   s" dlsym-entry" [: s" dlsym" LIBC-ENTRY, X64ASM:RAX PUSH, ;] PRIM
   s" ffi-call" [: FFI-CALL-BODY ;] PRIM
   s" ffi-call-n" [: FFI-CALL-N-BODY ;] PRIM
   s" ffi-call-0" [: 0 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-1" [: 1 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-2" [: 2 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-3" [: 3 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-4" [: 4 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-5" [: 5 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-6" [: 6 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-7" [: 7 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-8" [: 8 FFI-CALL-K-BODY ;] PRIM
   s" ffi-call-abi" [: FFI-CALL-ABI-BODY  X64ASM:RAX PUSH, ;] PRIM
   s" ffi-call-abi-r" [: FFI-CALL-ABI-BODY  XMM0-PUSH, ;] PRIM
   \ SysV records have no indirect copies. Drop the zero relocation mask;
   \ preserve both integer return registers for the result store.
   s" ffi-call-struct"
      [: X64ASM:R11 POP, DROP, X64ASM:R11 PUSH, FFI-CALL-ABI-BODY X64ASM:RAX PUSH, X64ASM:RDX PUSH, ;] PRIM ;

\ The twin of habu1.f BTASK-ENTRY. `task-entry` answers an immutable pthread
\ entry, not a Habu execution token: a SysV function whose one argument, in
\ rdi, is the TASK-ABI descriptor, and which enters that task's VM state. It
\ saves rbx rbp r12-r15, the SysV callee-saved set, which is exactly the VM's
\ registers (docs/xcomp.md "Machine model"), so no vector register needs
\ BTASK-ENTRY's d8..d15 save. The thread's alternate signal stack is registered
\ below the pushes before the body and disabled and given back after it
\ (X64BOOT:SIGNAL-STACK,). It writes two cells of the task's own: the TCB
\ address into the task's region, and DONE into the status once the body
\ returns. It never writes RUNNING: src/lib/task.f ACTIVATE stores that before
\ pthread_create, and a store here could put RUNNING back over the HALT-REQ a
\ TASK:HALT in the create window leaves. DONE is the last write the task makes
\ to anything another thread reads, so it is published with atomic!'s xchg,
\ the release BTASK-ENTRY gets from STLR (ATOMICS, above).

private

: SAVE-VM, ( -- )
   INDEX-REG X64CODE:ASM-SINK X64ASM:ENC-PUSH  DATA-REG X64CODE:ASM-SINK X64ASM:ENC-PUSH  DSP X64CODE:ASM-SINK X64ASM:ENC-PUSH
   DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-PUSH  NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-PUSH  CP-REG X64CODE:ASM-SINK X64ASM:ENC-PUSH ;
: RESTORE-VM, ( -- )
   CP-REG X64CODE:ASM-SINK X64ASM:ENC-POP  NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-POP  DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-POP
   DSP X64CODE:ASM-SINK X64ASM:ENC-POP  DATA-REG X64CODE:ASM-SINK X64ASM:ENC-POP  INDEX-REG X64CODE:ASM-SINK X64ASM:ENC-POP ;

\ The entry. The xt is called as execute calls one, and the body keeps rbp, so
\ the TCB is read back through the region. The six pushes leave rsp 8 off the
\ 16-byte alignment at that call, which Habu code does not need: SYSV-CALL,
\ aligns rsp itself before any C call.
: TASK-ENTRY, ( -- )
   SAVE-VM,
   INDEX-REG X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR           \ the descriptor across the registration
   X64BOOT:SIGNAL-STACK,                       \ this thread's own, below the pushes
   X64ASM:RDI INDEX-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   DATA-REG X64ASM:RDI TASK-ABI:REGION-OFF MOV-LOAD,
   DSP X64ASM:RDI TASK-ABI:STACK-OFF MOV-LOAD,
   DBASE-REG X64ASM:RDI TASK-ABI:DBASE-OFF MOV-LOAD,
   NDICT-REG X64ASM:RDI TASK-ABI:NDICT-OFF MOV-LOAD,
   CP-REG X64ASM:RDI TASK-ABI:CP-OFF MOV-LOAD,
   X64ASM:RDI DATA-REG TASK-TCB-CELL MOV-STORE,
   X64ASM:RAX X64ASM:RDI TASK-ABI:XT-OFF MOV-LOAD,
   CODE-SYNC,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64BOOT:SIGNAL-STACK-OFF,
   X64ASM:RSP STACK-ABI:SIGNAL-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32      \ the band back
   X64ASM:RCX DATA-REG TASK-TCB-CELL MOV-LOAD,
   X64ASM:RAX TASK-ABI:DONE IMM32,
   X64ASM:RAX X64ASM:RCX TASK-ABI:STATUS-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-XCHG-MR
   X64ASM:RAX ZERO-REG,
   RESTORE-VM,
   X64CODE:RET, ;

\ task-entry ( -- n ): push the entry's address and jump over it, so the entry
\ lies inside the row's record, as BTASK-ENTRY's does.
: TASK-ENTRY-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: entry:label done:label :}
   X64ASM:RAX entry X64CODE:MOVABS,  X64ASM:RAX PUSH,  done X64CODE:JMP,
   entry X64CODE:LBL,  TASK-ENTRY,
   done X64CODE:LBL, ;

public

private

\ A C entry saves SysV's six integer and eight vector argument registers in
\ the marshal band, then keeps only stack-resident state across checked code.
\ SAVE-VM, covers exactly the six SysV callee-saved registers used by the VM.
0 constant CBT-INTEGER
$40 constant CBT-FLOATS
$80 constant CBT-SLOT
$88 constant CBT-ROW
$90 constant CBT-REGION
$98 constant CBT-ROW-CLAIM
$A0 constant CBT-OWNER-CLAIM
$A8 constant CBT-OUTER
$B0 constant CBT-SELF
$C8 constant CBT-BYTES              \ entry rsp becomes 16-byte aligned
CBT-BYTES 6 cells + constant CBT-LR  \ above the six saved VM registers
16 constant CBT-STUB-BYTES

\ A callback rejection at the label, rsp at the thunk's frame: its record with
\ the slot and C's return address, which no word owns, then exit
\ EXIT-STATUS:CALLBACK. FAULT-OPEN, moves rsp, so rbx and r12, which the
\ record never writes, carry both.
: CB-DIE, ( str str label -- ) {: msg:str help:str at:label :}
   at X64CODE:LBL,
   X64ASM:RBX X64ASM:RSP CBT-SLOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R12 X64ASM:RSP CBT-LR X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   s" E-CALLBACK" msg X64CRASH:FAULT-OPEN,
   s" slot" X64ASM:RBX X64CRASH:FAULT-NUMBER,
   s" caller" X64ASM:R12 X64CRASH:FAULT-ADDRESS,
   help EXIT-STATUS:CALLBACK X64CRASH:FAULT-STOP, ;

\ The twin of crash.f C-SIGNAL-STACK-ABSENT>R12: rax is nonzero when this
\ thread has no alternate stack registered, as sigaltstack(NULL, old) answers
\ SS_DISABLE in old's int flags. A query cannot be rejected: its one error is
\ an unwritable buffer, and the buffer is this frame. It clobbers rax rcx rdi
\ rsi r11.
: SIGNAL-STACK-ABSENT, ( -- )
   X64ASM:RSP X64BOOT:SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI ZERO-REG,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  NR-SIGALTSTACK SYS,
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:RSP X64BOOT:SS-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RAX X64BOOT:SS-DISABLE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:RSP X64BOOT:SS-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ ZF is set when rax holds a caller row's owner. It clobbers rcx.
: CALLER-OWNER?, ( -- )
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
   X64ASM:RCX CB-ROW-CALLER X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8 ;

\ The alternate signal stack is handled as at habu1.f BCALLBACK-THUNK: carved
\ by X64BOOT:SIGNAL-STACK, and recorded in a 16-byte cell at the call's rsp.
\ A caller row is entered by adding 4 to its owner, claim 2, and left by
\ subtracting 4; its region is this thread's own, which CB-TLS-GET answers.
: CB-THUNK, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: row-read:label row-wait:label row-held:label owner-held:label
      owner-kept:label row-kept:label no-table:label row-busy:label
      unbound:label owner-busy:label no-frame:label no-xt:label
      defined:label done:label row-claimed:label signal-had:label
      signal-kept:label caller-row:label region-found:label
      caller-leave:label no-region:label :}
   SAVE-VM,
   X64ASM:RSP CBT-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   X64ASM:RDI X64ASM:RSP CBT-INTEGER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI X64ASM:RSP CBT-INTEGER CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RDX X64ASM:RSP CBT-INTEGER 2 CELL * + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RCX X64ASM:RSP CBT-INTEGER 3 CELL * + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:RSP CBT-INTEGER 4 CELL * + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R9 X64ASM:RSP CBT-INTEGER 5 CELL * + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   VEC-REGS 0 ?do  i X64ASM:>XMM X64ASM:RSP CBT-FLOATS i CELL * + X64ASM:MEM-OFF
      X64CODE:ASM-SINK X64ASM:ENC-MOVSD-MR  loop
   X64ASM:R10 X64ASM:RSP CBT-SLOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64BOOT:CB-SELF,
   X64ASM:RAX X64ASM:RSP CBT-SELF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R11 SHARED-DATA,
   X64ASM:R11 X64ASM:R11 CB-ROWS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R11 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E no-table X64CODE:JCC,
   X64ASM:R10 X64ASM:RSP CBT-SLOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 CB-ROW-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R11 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:R11 X64ASM:RSP CBT-ROW X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:R11 CB-ROW-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R9 X64ASM:RSP CBT-SELF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   row-read X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   CALLER-OWNER?,  X64ASM:C-E caller-row X64CODE:JCC,
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E row-held X64CODE:JCC,
   X64ASM:RAX CB-ROW-BUSY X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E row-wait X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE row-busy X64CODE:JCC,
   X64ASM:R9 X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR
   CALLER-OWNER?,  X64ASM:C-E row-read X64CODE:JCC,
   X64ASM:RAX CB-ROW-BUSY X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E row-wait X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE row-busy X64CODE:JCC,
   X64ASM:R10 1 IMM32,  row-claimed X64CODE:JMP,
   row-held X64CODE:LBL,  X64ASM:R10 ZERO-REG,
   row-claimed X64CODE:LBL,
   X64ASM:R10 X64ASM:RSP CBT-ROW-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R11 X64ASM:RSP CBT-ROW X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R11 X64ASM:R11 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R11 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E unbound X64CODE:JCC,
   region-found X64CODE:LBL,
   X64ASM:R11 X64ASM:RSP CBT-REGION X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R8 X64ASM:R11 CB-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E owner-held X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE owner-busy X64CODE:JCC,
   X64ASM:R9 X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE owner-busy X64CODE:JCC,
   X64ASM:R10 1 IMM32,  done X64CODE:JMP,
   owner-held X64CODE:LBL,  X64ASM:R10 ZERO-REG,
   done X64CODE:LBL,
   X64ASM:R10 X64ASM:RSP CBT-OWNER-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R10 X64ASM:R11 CB-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E no-frame X64CODE:JCC,
   X64ASM:R8 SHARED-DATA,
   X64ASM:R8 X64ASM:R8 CB-XTS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E no-xt X64CODE:JCC,
   X64ASM:R10 X64ASM:RSP CBT-SLOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 5 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8     \ five cells per descriptor
   X64ASM:R8 X64ASM:R8 X64ASM:R10 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E no-xt X64CODE:JCC,
   X64ASM:R10 X64ASM:R11 CB-FRAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:RSP CBT-OUTER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSP X64ASM:R11 CB-FRAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   DSP X64ASM:R11 CB-XDS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   DATA-REG X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   DBASE-REG X64ASM:R11 CB-DBASE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   NDICT-REG X64ASM:R11 CB-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   CP-REG X64ASM:R11 CB-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX ZERO-REG,
   X64ASM:R10 X64ASM:RSP CBT-OWNER-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E signal-had X64CODE:JCC,
   SIGNAL-STACK-ABSENT,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E signal-had X64CODE:JCC,
   X64BOOT:SIGNAL-STACK,
   X64ASM:RAX 1 IMM32,
   signal-had X64CODE:LBL,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   CODE-SYNC,  X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E signal-kept X64CODE:JCC,
   X64BOOT:SIGNAL-STACK-OFF,
   X64ASM:RSP STACK-ABI:SIGNAL-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32      \ the band back
   signal-kept X64CODE:LBL,
   X64ASM:R11 X64ASM:RSP CBT-REGION X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R11 CB-NDICT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE defined X64CODE:JCC,
   X64ASM:R10 X64ASM:R11 CB-CP X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 CP-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE defined X64CODE:JCC,
   X64ASM:R10 X64ASM:RSP CBT-OUTER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R11 CB-FRAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R10 X64ASM:RSP CBT-OWNER-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E owner-kept X64CODE:JCC,
   X64ASM:R10 ZERO-REG,
   X64ASM:R10 X64ASM:R11 CB-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   owner-kept X64CODE:LBL,
   X64ASM:R10 X64ASM:RSP CBT-ROW-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E row-kept X64CODE:JCC,
   X64ASM:R11 X64ASM:RSP CBT-ROW X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:R10 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE caller-leave X64CODE:JCC,
   X64ASM:R10 ZERO-REG,
   X64ASM:R10 X64ASM:R11 CB-ROW-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   row-kept X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP CBT-INTEGER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:XMM0 X64ASM:RSP CBT-FLOATS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVSD-RM
   X64ASM:RSP CBT-BYTES X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32
   RESTORE-VM,
   X64CODE:RET,
   \ rax is the owner read, r8 its cell; rsp is 16-byte aligned for the C call.
   caller-row X64CODE:LBL,
      X64ASM:RCX X64ASM:RAX 4 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
      X64ASM:RCX X64ASM:R8 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR  X64ASM:C-NE row-read X64CODE:JCC,
      X64ASM:R10 2 IMM32,  X64ASM:R10 X64ASM:RSP CBT-ROW-CLAIM X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
      X64ASM:R11 SHARED-DATA,
      X64ASM:RAX X64ASM:R11 CB-TLS-GET X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E unbound X64CODE:JCC,
      X64ASM:RDI X64ASM:R11 CB-TLS-KEY X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG                     \ C: rcx rdx rsi rdi r8-r11 are lost
      X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E no-region X64CODE:JCC,
      X64ASM:R11 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
      X64ASM:R9 X64ASM:RSP CBT-SELF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      region-found X64CODE:JMP,
   \ r11 is the row.
   caller-leave X64CODE:LBL,
      X64ASM:RAX X64ASM:R11 CB-ROW-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RCX X64ASM:RAX -4 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
      X64ASM:RCX X64ASM:R11 CB-ROW-OWNER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LOCK-CMPXCHG-MR  X64ASM:C-NE caller-leave X64CODE:JCC,
      row-kept X64CODE:JMP,
   row-wait X64CODE:LBL,  X64CODE:ASM-SINK X64ASM:ENC-NOP  row-read X64CODE:JMP,
   FAULT-RECORD:CB-NO-TABLE$ FAULT-RECORD:CB-NO-TABLE-HELP$ no-table CB-DIE,
   FAULT-RECORD:CB-ROW-BUSY$ FAULT-RECORD:CB-ROW-BUSY-HELP$ row-busy CB-DIE,
   FAULT-RECORD:CB-UNBOUND$ FAULT-RECORD:CB-UNBOUND-HELP$ unbound CB-DIE,
   FAULT-RECORD:CB-NO-REGION$ FAULT-RECORD:CB-NO-REGION-HELP$ no-region CB-DIE,
   FAULT-RECORD:CB-OWNER-BUSY$ FAULT-RECORD:CB-OWNER-BUSY-HELP$
      owner-busy CB-DIE,
   FAULT-RECORD:CB-NO-FRAME$ FAULT-RECORD:CB-NO-FRAME-HELP$ no-frame CB-DIE,
   FAULT-RECORD:CB-NO-XT$ FAULT-RECORD:CB-NO-XT-HELP$ no-xt CB-DIE,
   FAULT-RECORD:CB-DEFINED$ FAULT-RECORD:CB-DEFINED-HELP$ defined CB-DIE, ;

variable CB-THUNK-CELL
: CB-THUNK-LBL ( -- label ) CB-THUNK-CELL @ >LABEL ;

\ Answers the shared thunk; every callback stub is compiler-owned.
: CALLBACK-ENTRY-BODY ( -- )
   X64ASM:RAX CB-THUNK-LBL X64CODE:MOVABS,  X64ASM:RAX PUSH, ;

\ Saved callback stubs name this exact kernel entry. Its record follows the
\ public primitive's completed record, so their code spans are disjoint.
: CB-THUNK-HELPER, ( -- )
   X64CODE:LBL {: end:label :}
   s" (CALLBACK-THUNK)" CB-THUNK-LBL LABEL>N end LABEL>N
      ENGINE-PRIMS:HELPER-REGISTER
   CB-THUNK-LBL X64CODE:LBL,  CB-THUNK,
   end X64CODE:LBL,  X64CODE:DATA-AFTER, ;

public

: TASK, ( -- )
   s" task-entry" [: TASK-ENTRY-BODY ;] PRIM
   X64CODE:LBL CB-THUNK-CELL !
   s" callback-entry" [: CALLBACK-ENTRY-BODY ;] PRIM
   CB-THUNK-HELPER, ;

\ The rows an interpreter written in Habu publishes definitions, namespace
\ rows, aliases and package scope through and marks a word immediate with, and
\ the checker overlay's replay rows, whose contracts src/habu/prims.f states:
\ the twins of habu2.f DEFWRITE's bodies and of its `immediate`, C-IMMEDIATE,
\ in each twin's check order. Every rejection of a row with a body comes before
\ the first store and writes nothing on fd 2: a live task exits
\ EXIT-STATUS:TASK-COMPILE in the four dictionary rows and the replay rows, a
\ protected wid after the seal EXIT-STATUS:SEAL-WORDLIST, and every other rejection
\ EXIT-STATUS:SEAL-VIOLATION. The exception is an open overlay in def-open,
\ namespace-private and alias-record: OVERLAY-GUARD, throws E-OVERLAY-OPEN.
\ Past the checks the index helper exits EXIT-STATUS:SYSTEM when its mapping fails,
\ or EXIT-STATUS:INTERNAL for an internal index invariant.

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

: FRAME-OPEN, ( -- ) X64ASM:RSP DW-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8 ;
: FRAME-CLOSE, ( -- ) X64ASM:RSP DW-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

: POP-TO, ( n -- ) {: off:n :} X64ASM:RAX POP,  X64ASM:RAX X64ASM:RSP off MOV-STORE, ;

\ A pending definition's record is slot NDICT, the slot a record writer fills.
: NOT-PENDING, ( -- )
   X64ASM:RAX PEND-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC, ;

\ Slot DICT-CAP is the control-flow band, not a record.
: DICT-ROOM, ( -- )
   NDICT-REG DICT-CAP X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC, ;

\ The frame's name: reject an empty one, and a long one whose slots at CP
\ would reach the code ceiling. The compares are unsigned and never wrap: a CP
\ already at the ceiling, and a length no region holds, a negative one among
\ them, are rejected rather than added. Clobbers rax rcx rsi.
: NAME-SIZE, ( -- )
   X64CODE:LBL {: fits:label :}
   X64ASM:RSI X64ASM:RSP DW-LEN MOV-LOAD,
   X64ASM:RSI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RSI DNAME-INL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-BE fits X64CODE:JCC,
   X64ASM:RAX DBASE-REG CODE-CEILING X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   CP-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX CP-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR                     \ rax = the room below it
   X64ASM:RSI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,   \ the bytes alone reach it
   X64ASM:RCX X64ASM:RSI SLOT-UP,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,   \ so do their slots
   fits X64CODE:LBL, ;

\ Reject a live record of the frame's folded name in the frame's wid: the
\ one-wordlist search's probe, FIND-LBL, answers it in rax.
: FRESH, ( -- )
   X64ASM:RDI X64ASM:RSP DW-NAME MOV-LOAD,  X64ASM:RSI X64ASM:RSP DW-LEN MOV-LOAD,  X64ASM:RDX X64ASM:RSP DW-WID MOV-LOAD,
   FIND-LBL X64CODE:CALL,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC, ;

\ Every wordlist is nonnegative; all negative values are record markers.
: REAL-WID, ( -- )
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S SEAL-TRAP-LBL X64CODE:JCC, ;

\ rax is a package marker in the current or an imported namespace, else a jump
\ to bad.
: LIVE-NAMESPACE, ( label -- ) {: bad:label :}
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NS bad X64CODE:JCC,
   X64ASM:RAX WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:RAX WL:RETIRED-NAMESPACE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E bad X64CODE:JCC, ;

\ Store the frame's name in record NDICT, whose address DW-REC takes: [16]
\ its length, with DNAME-EXT for a long one; [24] and [32] the bytes inline,
\ zero past them, or [24] the address of a copy at CP, zero-padded to the
\ next code slot, with CP moved to that slot. Stores use the alias; DW-REC
\ and the long name's stored pointer keep their RX addresses.
: NAME-STORE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: banded:label long:label done:label :}
   NDICT-REG RECORD-AT,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-STORE,
   X64ASM:RDI X64ASM:RSP DW-LEN MOV-LOAD,
   X64ASM:RDI DNAME-INL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-BE banded X64CODE:JCC,
   X64ASM:RDI CP-REG X64ASM:RDI 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDI X64ASM:RDI SLOT-UP,
   X64ASM:RDI X64ASM:RSP DW-SLOT MOV-STORE,
   banded X64CODE:LBL,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:R8 WRITE-ALIAS,
   X64ASM:RSI X64ASM:RSP DW-NAME MOV-LOAD,  X64ASM:RCX X64ASM:RSP DW-LEN MOV-LOAD,
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX X64ASM:R8 REC-NAME MOV-STORE,  X64ASM:RAX X64ASM:R8 REC-NAME CELL + MOV-STORE,
   X64ASM:RCX DNAME-INL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-A long X64CODE:JCC,
   X64ASM:RCX X64ASM:R8 REC-FLAGS MOV-STORE,
   X64ASM:RDI X64ASM:R8 REC-NAME X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   COPY-BYTES,
   done X64CODE:JMP,
   long X64CODE:LBL,
   X64ASM:RAX DNAME-EXT IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   X64ASM:RAX X64ASM:R8 REC-FLAGS MOV-STORE,
   CP-REG X64ASM:R8 REC-NAME MOV-STORE,
   X64ASM:RDI CP-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
   COPY-BYTES,
   X64ASM:RCX X64ASM:RSP DW-SLOT MOV-LOAD,  X64ASM:RCX WRITE-ALIAS,
   X64ASM:RCX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   ZERO-BYTES,
   CP-REG X64ASM:RSP DW-SLOT MOV-LOAD,
   done X64CODE:LBL, ;

: PUBLISH-RECORD, ( -- )
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX NDICT-REG 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   OCC-COUNT,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP
   HIDX-ADD,
   RECORD-PUBLISHED, ;

\ The twin of habu2.f DEFWRITE:HW-RAISE,. Clobbers rax.
: HW-RAISE, ( -- )
   X64CODE:LBL {: keep:label :}
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E keep X64CODE:JCC,
   X64ASM:RAX REPLAY-SCOPE:HW CELL@,
   NDICT-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE keep X64CODE:JCC,
   NDICT-REG REPLAY-SCOPE:HW CELL!,
   keep X64CODE:LBL, ;

\ namespace-record ( ^u8 n bool -- n ): habu2.f DEFWRITE:NAMESPACE-RECORD's
\ twin; src/habu/prims.f states its contract.
: NAMESPACE-RECORD-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label next:label clean:label one:label :}
   s" namespace-record" TASK-GUARD,
   FRAME-OPEN,
   DW-ARG POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   NOT-PENDING,
   DICT-ROOM,
   NAME-SIZE,
   X64ASM:RDI X64ASM:RSP DW-NAME MOV-LOAD,  X64ASM:RSI X64ASM:RSP DW-LEN MOV-LOAD,  X64ASM:RCX ZERO-REG,
   X64ASM:RDX ZERO-REG,                                      \ rdx = a colon seen
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE clean X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX NAME-COLON X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE next X64CODE:JCC,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX X64ASM:RCX 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RDX 1 IMM32,
   next X64CODE:LBL,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   scan X64CODE:JMP,
   clean X64CODE:LBL,
   X64ASM:RAX BOOT:NAMESPACE-WL-CELL CELL@,
   X64ASM:RAX X64ASM:RSP DW-WID MOV-STORE,
   FRESH,
   NAME-STORE,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX WIDN-CELL CELL@,
   X64ASM:RAX X64ASM:R8 REC-CODE MOV-STORE,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RCX ZERO-REG,
   X64ASM:RDX X64ASM:RSP DW-ARG MOV-LOAD,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E one X64CODE:JCC,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   one X64CODE:LBL,
   X64ASM:RCX X64ASM:R8 REC-CODE CELL + MOV-STORE,
   X64ASM:RAX WIDN-CELL CELL!,
   X64ASM:RAX X64ASM:RSP DW-WID MOV-LOAD,  X64ASM:RAX X64ASM:R8 REC-WID MOV-STORE,
   PUBLISH-RECORD,
   HW-RAISE,                                          \ an overlay's row dies at replay-close
   FRAME-CLOSE,
   X64ASM:RAX NDICT-REG -1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RAX PUSH, ;

\ namespace-private ( n -- ): namespace row n, below NDICT, unsigned, whose [8]
\ is 0, takes a fresh private wid.
: NAMESPACE-PRIVATE-BODY ( -- )
   s" namespace-private" TASK-GUARD,
   OVERLAY-GUARD,
   X64ASM:RCX POP,
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX RECORD-AT,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,
   SEAL-TRAP-LBL LIVE-NAMESPACE,
   X64ASM:RAX X64ASM:R8 REC-CODE CELL + MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX WIDN-CELL CELL@,
   X64ASM:RAX X64ASM:R8 REC-CODE CELL + MOV-STORE,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RAX WIDN-CELL CELL!, ;

\ alias-record: record NDICT with the source's [0], [8] and IMM, WIDE, TARGET, COPY and immutable VAL
\ bits; a namespace, retired or DNAME-INT source is rejected, whose alias would run an internal body.
: ALIAS-RECORD-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: prot:label done:label notval:label :}
   s" alias-record" TASK-GUARD,
   OVERLAY-GUARD,
   FRAME-OPEN,
   DW-WID POP-TO,  DW-ARG POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   X64ASM:RDX X64ASM:RSP DW-WID MOV-LOAD,
   REAL-WID,
   prot OPEN-WID,
   NOT-PENDING,
   X64ASM:RCX X64ASM:RSP DW-ARG MOV-LOAD,
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX RECORD-AT,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-S SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX DNAME-INT IMM64,
   X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-TEST-MR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   DICT-ROOM,
   NAME-SIZE,
   FRESH,
   NAME-STORE,
   X64ASM:RDI X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RCX X64ASM:RSP DW-ARG MOV-LOAD,  X64ASM:RCX RECORD-AT,           \ r8 = the source
   X64ASM:RAX X64ASM:R8 REC-CODE MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-CODE MOV-STORE,
   X64ASM:RAX X64ASM:R8 REC-CODE CELL + MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-CODE CELL + MOV-STORE,
   X64ASM:RAX DNAME-IMM DNAME-WIDE or DNAME-TARGET or DNAME-COPY or IMM64,
   X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-AND-RM
   X64ASM:RAX X64ASM:RDI REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-MR
   X64ASM:RAX X64ASM:R8 REC-FLAGS MOV-LOAD,
   X64ASM:RCX DKIND:MASK IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-AND-RR
   X64ASM:RCX DKIND:VAL IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE notval X64CODE:JCC,
      X64ASM:RCX X64ASM:RDI REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-MR
   notval X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP DW-WID MOV-LOAD,  X64ASM:RAX X64ASM:RDI REC-WID MOV-STORE,
   PUBLISH-RECORD,
   FRAME-CLOSE,
   done X64CODE:JMP,
   prot X64CODE:LBL,  EXIT-STATUS:SEAL-WORDLIST EXIT-GROUP,
   done X64CODE:LBL, ;

\ package-scope! ( n n -- ): habu2.f DEFWRITE:PACKAGE-SCOPE's twin;
\ src/habu/prims.f states its contract.
: PACKAGE-SCOPE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   X64CODE:LBL
   {: bad:label set:label scan:label part:label top:label store:label done:label :}
   X64ASM:RDX POP,  X64ASM:RCX POP,                                 \ the parent wid, the row
   X64ASM:RCX -1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE set X64CODE:JCC,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bad X64CODE:JCC,  \ only `-1 0` clears
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX PKG-PUB-CELL CELL!,  X64ASM:RAX PKG-PRI-CELL CELL!,
   X64ASM:RAX PKG-PARENT-CELL CELL!,  X64ASM:RAX PKG-REC-CELL CELL!,
   X64ASM:RAX ENCLOSING:PUB CELL!,  X64ASM:RAX ENCLOSING:PRI CELL!,
   X64ASM:RAX ENCLOSING:PARENT CELL!,  X64ASM:RAX ENCLOSING:REC CELL!,
   done X64CODE:JMP,
   set X64CODE:LBL,
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE bad X64CODE:JCC,
   X64ASM:RCX RECORD-AT,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,
   bad LIVE-NAMESPACE,
   X64ASM:R9 X64ASM:R8 REC-CODE CELL + MOV-LOAD,                    \ r9 = the private wid
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,
   X64ASM:R8 X64ASM:RSI X64ASM:RAX NAME-LEN,  X64ASM:R8 X64ASM:RDI X64ASM:RAX NAME-AT,
   X64ASM:RCX ZERO-REG,
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE top X64CODE:JCC,
   X64ASM:RAX X64ASM:RDI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX NAME-COLON X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E part X64CODE:JCC,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   scan X64CODE:JMP,
   part X64CODE:LBL,
   X64ASM:RAX PKG-PUB-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bad X64CODE:JCC,  \ no package open
   X64ASM:RAX ENCLOSING:PUB CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE bad X64CODE:JCC, \ a part is open
   X64ASM:RAX PKG-PUB-CELL CELL@,  X64ASM:RAX ENCLOSING:PUB CELL!,
   X64ASM:RAX PKG-PRI-CELL CELL@,  X64ASM:RAX ENCLOSING:PRI CELL!,
   X64ASM:RAX PKG-PARENT-CELL CELL@,  X64ASM:RAX ENCLOSING:PARENT CELL!,
   X64ASM:RAX PKG-REC-CELL CELL@,  X64ASM:RAX ENCLOSING:REC CELL!,
   store X64CODE:JMP,
   top X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX ENCLOSING:PUB CELL!,  X64ASM:RAX ENCLOSING:PRI CELL!,
   X64ASM:RAX ENCLOSING:PARENT CELL!,  X64ASM:RAX ENCLOSING:REC CELL!,
   store X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 REC-CODE MOV-LOAD,
   X64ASM:RAX PKG-PUB-CELL CELL!,  X64ASM:R9 PKG-PRI-CELL CELL!,
   X64ASM:RDX PKG-PARENT-CELL CELL!,  X64ASM:R8 PKG-REC-CELL CELL!,
   done X64CODE:JMP,
   bad X64CODE:LBL,
   s" E-SEAL-VIOLATION habu: package-scope! row not a live package row below NDICT with a private wordlist, a part outside a package or inside a part, or -1 with a parent"
   EXIT-STATUS:SEAL-VIOLATION X64CRASH:DIE-LINE,
   done X64CODE:LBL, ;

\ def-open ( ^u8 n n n -- ) name, wid, kind: record NDICT unpublished, [0]
\ CP past the name, [8] 0, the kind beside the length and the wid in [40];
\ PEND-CELL the record; TSIG, TCSIG, DOESB, CLAUSE and TRUSTED clear;
\ COMP-LATCH-CELL takes the latch. A DKIND:ADDR kind carries DNAME-ALLOC, and
\ DNAME-CELL as well for a variable.
: DEF-OPEN-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: latched:label prot:label done:label :}
   s" def-open" TASK-GUARD,
   OVERLAY-GUARD,
   FRAME-OPEN,
   DW-ARG POP-TO,  DW-WID POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   X64ASM:RAX DKIND:MASK DNAME-CELL or DNAME-ALLOC or DNAME-TARGET or invert IMM64,
   X64ASM:RAX X64ASM:RSP DW-ARG X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-TEST-MR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP DW-WID MOV-LOAD,
   REAL-WID,
   prot OPEN-WID,
   NOT-PENDING,
   X64ASM:RAX DBASE-REG CODE-CEILING X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   CP-REG X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   DICT-ROOM,
   NAME-SIZE,
   FRESH,
   NAME-STORE,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:R8 WRITE-ALIAS,
   CP-REG X64ASM:R8 REC-CODE MOV-STORE,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:R8 REC-CODE CELL + MOV-STORE,
   X64ASM:RAX X64ASM:RSP DW-ARG MOV-LOAD,  X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-MR
   X64ASM:RAX X64ASM:RSP DW-WID MOV-LOAD,  X64ASM:RAX X64ASM:R8 REC-WID MOV-STORE,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:R8 PEND-CELL CELL!,
   X64ASM:RAX TKA-CELL CELL@,  X64ASM:RAX PENDTKA-CELL CELL!,  \ keep the name token for source-end location
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX TSIG-A-CELL CELL!,  X64ASM:RAX TSIG-U-CELL CELL!,
   X64ASM:RAX TCSIG-A-CELL CELL!,  X64ASM:RAX TCSIG-U-CELL CELL!,
   X64ASM:RAX DOESB-CELL CELL!,  X64ASM:RAX TRUSTED-CELL CELL!,  X64ASM:RAX CLAUSE-CELL CELL!,
   X64ASM:RAX NCOMP-DISPATCH:COMP-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E latched X64CODE:JCC,
   X64ASM:RAX 1 IMM32,
   latched X64CODE:LBL,
   X64ASM:RAX NCOMP-DISPATCH:COMP-LATCH-CELL CELL!,
   FRAME-CLOSE,
   done X64CODE:JMP,
   prot X64CODE:LBL,  EXIT-STATUS:SEAL-WORDLIST EXIT-GROUP,
   done X64CODE:LBL, ;

\ The twin of habu2.f DEFWRITE:SAVED,. Clobbers rax.
: SAVED, ( n n bool -- ) {: off:n cell:n save:bool :}
   save if
      X64ASM:RAX off CELL@,  X64ASM:RAX cell CELL!,
   else
      X64ASM:RAX cell CELL@,  X64ASM:RAX off CELL!,
   then ;

\ The twin of habu2.f DEFWRITE:SCOPE,, which names the cells: the scope
\ replay-open saves and replay-close restores beside NDICT and CP. Clobbers rax.
: SCOPE, ( bool -- ) {: save:bool :}
   WIDN-CELL REPLAY-SCOPE:WIDN save SAVED,
   CUR-CELL REPLAY-SCOPE:CUR save SAVED,
   PKG-PUB-CELL REPLAY-SCOPE:PKG-PUB save SAVED,
   PKG-PRI-CELL REPLAY-SCOPE:PKG-PRI save SAVED,
   PKG-PARENT-CELL REPLAY-SCOPE:PKG-PARENT save SAVED,
   PKG-REC-CELL REPLAY-SCOPE:PKG-REC save SAVED,
   ENCLOSING:END ENCLOSING:PUB - 0 do
      ENCLOSING:PUB i +  REPLAY-SCOPE:ENCLOSING i +  save SAVED,
   CELL +loop ;

\ replay-open ( -- ): park a pending definition: PEND-CELL and record NDICT's
\ bytes move into REPLAY-SCOPE:PEND and PEND-REC; with a task live it traps.
: REPLAY-OPEN-BODY ( -- )
   X64CODE:LBL {: none:label :}
   s" replay-open" TASK-GUARD,
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,   \ an overlay is open
   X64ASM:RCX PEND-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   X64ASM:RAX TASKS-LIVE-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   NDICT-REG RECORD-AT,
   DREC 0 do  X64ASM:RAX X64ASM:R8 i MOV-LOAD,  X64ASM:RAX REPLAY-SCOPE:PEND-REC i + CELL!,  CELL +loop
   X64ASM:RCX REPLAY-SCOPE:PEND CELL!,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX PEND-CELL CELL!,
   none X64CODE:LBL,
   NDICT-REG REPLAY-SCOPE:NDICT CELL!,
   CP-REG REPLAY-SCOPE:CP CELL!,
   true SCOPE,
   NDICT-REG REPLAY-SCOPE:HW CELL!,
   X64ASM:RAX 1 IMM32,  X64ASM:RAX REPLAY-SCOPE:LATCH CELL!, ;

variable REPLAY-TRAP-CELL
: REPLAY-TRAP-LBL ( -- label ) REPLAY-TRAP-CELL @ >LABEL ;

: REPLAY-REQUIRED, ( -- )
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC, ;

\ A replay row can bind a checked name, but can only trap if executed.
: REPLAY-RECORD-BODY ( -- )
   s" replay-record" TASK-GUARD,
   FRAME-OPEN,
   DW-WID POP-TO,  DW-LEN POP-TO,  DW-NAME POP-TO,
   X64ASM:RDX X64ASM:RSP DW-WID MOV-LOAD,  REAL-WID,
   REPLAY-REQUIRED,
   NOT-PENDING,  DICT-ROOM,  NAME-SIZE,  FRESH,
   NAME-STORE,
   X64ASM:R8 X64ASM:RSP DW-REC MOV-LOAD,  X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX REPLAY-TRAP-LBL X64CODE:MOVABS,  X64ASM:RAX X64ASM:R8 REC-CODE MOV-STORE,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:R8 REC-CODE CELL + MOV-STORE,
   X64ASM:RAX X64ASM:RSP DW-WID MOV-LOAD,  X64ASM:RAX X64ASM:R8 REC-WID MOV-STORE,
   PUBLISH-RECORD,  HW-RAISE,
   FRAME-CLOSE, ;

\ Re-keying admits a wordlist or one of the two retired markers, never a live
\ package marker. Its index is repaired after the store, including restores.
: REPLAY-WID, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64CODE:LBL {: admitted:label :}
   r r X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NS admitted X64CODE:JCC,
   r WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E admitted X64CODE:JCC,
   r WL:RETIRED-NAMESPACE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   admitted X64CODE:LBL, ;

: RECORD-WID-BODY ( -- )
   s" record-wid!" TASK-GUARD,
   X64ASM:R10 POP,  X64ASM:R9 POP,
   REPLAY-REQUIRED,
   X64ASM:R10 NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R10 RECORD-AT,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,  X64ASM:RAX REPLAY-WID,  X64ASM:R9 REPLAY-WID,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:R9 X64ASM:R8 REC-WID MOV-STORE,
   X64ASM:RDI X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  REKEY-LBL X64CODE:CALL, ;

\ record-rekey ( n -- ) index, below NDICT: the body checks nothing.
: RECORD-REKEY-BODY ( -- )
   X64ASM:RDI POP,  REKEY-LBL X64CODE:CALL, ;

\ A private wordlist granted inside an overlay may only be withdrawn at or
\ above that overlay's saved WIDN. The namespace row itself remains indexed.
: REPLAY-PRIVATE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: give:label store:label :}
   s" replay-private" TASK-GUARD,
   X64ASM:R10 POP,  X64ASM:RCX POP,
   REPLAY-REQUIRED,
   X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX RECORD-AT,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,  SEAL-TRAP-LBL LIVE-NAMESPACE,
   X64ASM:R9 X64ASM:R8 REC-CODE CELL + MOV-LOAD,
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE give X64CODE:JCC,
   X64ASM:RAX REPLAY-SCOPE:WIDN CELL@,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R9 ZERO-REG,  store X64CODE:JMP,
   give X64CODE:LBL,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R9 WIDN-CELL CELL@,
   X64ASM:RAX X64ASM:R9 1 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RAX WIDN-CELL CELL!,
   store X64CODE:LBL,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:R9 X64ASM:R8 REC-CODE CELL + MOV-STORE, ;

\ replay-close ( -- ): zero the records [NDICT, HW), which the owner dropped. A
\ parked definition returns, PEND-CELL last.
: REPLAY-CLOSE-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: unparked:label done:label :}
   s" replay-close" TASK-GUARD,
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,    \ no overlay
   NOT-PENDING,
   X64ASM:R8 REPLAY-SCOPE:NDICT CELL@,
   NDICT-REG X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R9 REPLAY-SCOPE:HW CELL@,
   NDICT-REG X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:R10 REPLAY-SCOPE:CP CELL@,
   CP-REG X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX REPLAY-SCOPE:WIDN CELL@,  X64ASM:RDX WIDN-CELL CELL@,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX REPLAY-SCOPE:PEND CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E unparked X64CODE:JCC,
   X64ASM:RAX TASKS-LIVE-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   unparked X64CODE:LBL,
   X64ASM:RDI NDICT-REG DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RDI DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RCX REPLAY-SCOPE:HW CELL@,  X64ASM:RCX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX X64ASM:RCX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   ZERO-BYTES,
   false SCOPE,
   X64ASM:RDI DATA-REG REPLAY-SCOPE:LATCH X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX REPLAY-SCOPE:END REPLAY-SCOPE:LATCH - IMM32,
   ZERO-BYTES,
   X64ASM:RDI DATA-REG REPLAY-SCOPE:ENCLOSING X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX ENCLOSING:END ENCLOSING:PUB - IMM32,
   ZERO-BYTES,
   X64ASM:RCX REPLAY-SCOPE:PEND CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   NDICT-REG RECORD-AT,  X64ASM:R8 WRITE-ALIAS,
   DREC 0 do  X64ASM:RAX REPLAY-SCOPE:PEND-REC i + CELL@,  X64ASM:RAX X64ASM:R8 i MOV-STORE,  CELL +loop
   X64ASM:RCX PEND-CELL CELL!,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX REPLAY-SCOPE:PEND CELL!,
   DREC 0 do  X64ASM:RAX REPLAY-SCOPE:PEND-REC i + CELL!,  CELL +loop
   done X64CODE:LBL, ;

\ replay-widn! ( n -- ): habu2.f DEFWRITE:REPLAY-WIDN's twin; src/habu/prims.f
\ states its contract. The compares are unsigned.
: REPLAY-WIDN-BODY ( -- )
   s" replay-widn!" TASK-GUARD,
   X64ASM:RCX POP,                                           \ the mark
   X64ASM:RAX REPLAY-SCOPE:LATCH CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,    \ no overlay
   X64ASM:RAX REPLAY-SCOPE:WIDN CELL@,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX WIDN-CELL CELL@,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX WIDN-CELL CELL!, ;

\ habu1.f EMIT-BCAP's BODY-SOURCE row for the run rsi/rcx landing at BODYLEN
\ (rax), in the block a run has room in: row i is at BODY-TEXT:BASE-CELL +
\ BODY-TEXT:CAP-CELL + 16i, BODY-TEXT:ROWS-CELL counts them. It keeps rax,
\ rcx, rdx and rsi, and clobbers rdi and r8-r11.
: BODY-ROW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: pop:label put:label seed:label none:label src:label rowed:label :}
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E rowed X64CODE:JCC,
   X64ASM:R8 BODY-TEXT:BASE-CELL CELL@,
   X64ASM:R8 DATA-REG BODY-TEXT:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:R9 BODY-TEXT:ROWS-CELL CELL@,      \ r8 = the rows, r9 = their count
   pop X64CODE:LBL,
   X64ASM:R9 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E put X64CODE:JCC,
   X64ASM:R10 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:R10 X64ASM:R8 X64ASM:R10 1 2 cells negate X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOV-RM   \ the last row's text offset
   X64ASM:R10 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B put X64CODE:JCC,
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC  pop X64CODE:JMP,
   put X64CODE:LBL,
   X64ASM:R10 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:R10 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR                              \ the new row is at r10
   X64ASM:RAX X64ASM:R10 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:R9 BODY-TEXT:ROWS-CELL CELL!,
   X64ASM:R9 SRCLOC:INB-CELL CELL@,  X64ASM:R11 INE-CELL CELL@,
   \ The first run is the name, which a head seeds.
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E seed X64CODE:JCC,
   X64ASM:RDI PENDTKA-CELL CELL@,
   X64ASM:RDI X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B none X64CODE:JCC,
   X64ASM:RDI X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE none X64CODE:JCC,
   seed X64CODE:LBL,
   X64ASM:RSI X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B none X64CODE:JCC,
   X64ASM:RSI X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE none X64CODE:JCC,
   X64ASM:RDI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  src X64CODE:JMP,
   none X64CODE:LBL,
   X64ASM:RDI BODY-SOURCE:NONE IMM64,
   src X64CODE:LBL,
   X64ASM:RDI X64ASM:R10 CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   rowed X64CODE:LBL, ;

\ A capacity doubled to 2^59 is rejected before its map: 17 * 2^58 stays below
\ 2^63, so a block's size never wraps.
59 constant GROW-LIMIT-SHIFT
variable GROW-CELL
: GROW-LBL ( -- label ) GROW-CELL @ >LABEL ;

\ GROW-LBL ( rdx = the text length the block must hold ): the growth of
\ src/habu/layout.f BODY-TEXT, modelled on ADDRESS-CELLS's. The capacity is
\ BODY-TEXT:FIRST-CAP doubled until it holds rdx; it maps 17 times that,
\ copies BODYLEN text bytes to the new base and the rows to new base + new
\ capacity, unmaps the old block when there was one, rebases TCSIG-A when it
\ lies in the old text and stores BASE, CAP and SIZE. A failed map throws
\ E-BODY-TEXT; the old block stays as it was. It keeps rax,
\ rcx, rdx and rsi, and clobbers rdi and r8-r11.
: GROW-HELPER, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: size:label sized:label kept:label fresh:label :}
   X64CODE:LBL {: fail:label :}
   GROW-LBL X64CODE:LBL,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:R9 BODY-TEXT:FIRST-CAP IMM32,
   size X64CODE:LBL,
   X64ASM:R9 X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE sized X64CODE:JCC,
   X64ASM:R9 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   X64ASM:R10 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 GROW-LIMIT-SHIFT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:R10 X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE fail X64CODE:JCC,
   size X64CODE:JMP,
   sized X64CODE:LBL,
   X64ASM:R10 X64ASM:R9 17 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-PUSH  X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-PUSH      \ [rsp] the size, [rsp+8] the capacity
   X64ASM:RSI X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   MAP-ANON,  X64ASM:C-B fail X64CODE:JCC,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR                       \ r8 = the new base
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI BODY-TEXT:BASE-CELL CELL@,  X64ASM:RCX BODYLEN-CELL CELL@,
   COPY-BYTES,                                      \ the text
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RSI BODY-TEXT:BASE-CELL CELL@,
   X64ASM:RSI DATA-REG BODY-TEXT:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RCX BODY-TEXT:ROWS-CELL CELL@,  X64ASM:RCX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8
   COPY-BYTES,                                      \ the rows
   X64ASM:RDI BODY-TEXT:BASE-CELL CELL@,
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E fresh X64CODE:JCC,
   X64ASM:RSI BODY-TEXT:SIZE-CELL CELL@,
   NR-MUNMAP SYS,
   fresh X64CODE:LBL,
   X64ASM:RAX TCSIG-A-CELL CELL@,  X64ASM:RDI BODY-TEXT:BASE-CELL CELL@,
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RAX DATA-REG BODY-TEXT:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-AE kept X64CODE:JCC,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:RAX TCSIG-A-CELL CELL!,
   kept X64CODE:LBL,
   X64ASM:R8 BODY-TEXT:BASE-CELL CELL!,
   X64ASM:RAX X64ASM:RSP CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX BODY-TEXT:CAP-CELL CELL!,
   X64ASM:RAX X64ASM:RSP 0 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX BODY-TEXT:SIZE-CELL CELL!,
   X64ASM:RSP 2 cells X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-POP
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP
   X64CODE:RET,
   fail X64CODE:LBL,
   X64ASM:RAX -9853 IMM64,  THROW, ;                    \ E-BODY-TEXT

\ body-capture ( ^u8 n -- ): habu2.f BODY-APPEND's twin, with BODY-ROW,.
\ BODYLEN past the capacity and a length that wraps exit SEAL-VIOLATION; with
\ no block, or none the run and its space fit, GROW-LBL grows it first.
: BODY-APPEND-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: grow:label fits:label :}
   X64ASM:RCX POP,  X64ASM:RSI POP,                                 \ the count, the bytes
   X64ASM:RAX BODYLEN-CELL CELL@,
   X64ASM:RAX DATA-REG BODY-TEXT:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-A SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RDX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:C-B SEAL-TRAP-LBL X64CODE:JCC,   \ the length past the space
   X64ASM:R8 BODY-TEXT:BASE-CELL CELL@,
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E grow X64CODE:JCC,
   X64ASM:RDX DATA-REG BODY-TEXT:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-BE fits X64CODE:JCC,
   grow X64CODE:LBL,  GROW-LBL X64CODE:CALL,
   fits X64CODE:LBL,
   BODY-ROW,
   X64ASM:RDI BODY-TEXT:BASE-CELL CELL@,
   X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR                        \ the run lands at BODYLEN
   COPY-BYTES,
   X64ASM:RAX STR:SPACE IMM32,
   0 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RDX BODYLEN-CELL CELL!, ;

\ ( ^u8 n -- ): the span into the cells aoff and uoff while a
\ definition is pending.
: PENDING-SPAN-BODY ( n n -- ) {: aoff:n uoff:n :}
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RDX PEND-CELL CELL@,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX aoff CELL!,  X64ASM:RCX uoff CELL!, ;

\ trust-sig! ( ^u8 n -- ): the pending definition's signature span into
\ TSIG-A and TSIG-U.
: TRUST-SIG-BODY ( -- )
   TSIG-A-CELL TSIG-U-CELL PENDING-SPAN-BODY ;

\ created-sig! ( ^u8 n -- ): the pending definition's `does>` signature
\ span into TCSIG-A and TCSIG-U.
: CREATED-SIG-BODY ( -- )
   TCSIG-A-CELL TCSIG-U-CELL PENDING-SPAN-BODY ;

\ def-close ( -- ): the pending definition ends: COMP-LATCH-CELL, TSIG, TCSIG,
\ DOESB, CLAUSE, TRUSTED and PEND-CELL clear; before they clear, LASTC-CELL
\ takes the record for a nonzero kind other than DKIND:CAST. Nothing pending,
\ and COMP-LATCH-CELL 0 (no compiler), are rejected.
: DEF-CLOSE-BODY ( -- )
   X64CODE:LBL {: nolast:label :}
   X64ASM:RDX PEND-CELL CELL@,
   X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX NCOMP-DISPATCH:COMP-LATCH-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RCX X64ASM:RDX REC-FLAGS MOV-LOAD,                        \ a published body that pushes a cell is the slot `does>` patches, set after the body as the kernel's definers set it
   X64ASM:RAX DKIND:MASK IMM64,  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-AND-RR
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E nolast X64CODE:JCC,
   X64ASM:RAX DKIND:CAST IMM64,  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E nolast X64CODE:JCC,
   X64ASM:RDX LASTC-CELL CELL!,
   nolast X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   X64ASM:RAX NCOMP-DISPATCH:COMP-LATCH-CELL CELL!,
   X64ASM:RAX TSIG-A-CELL CELL!,  X64ASM:RAX TSIG-U-CELL CELL!,
   X64ASM:RAX TCSIG-A-CELL CELL!,  X64ASM:RAX TCSIG-U-CELL CELL!,
   X64ASM:RAX DOESB-CELL CELL!,  X64ASM:RAX TRUSTED-CELL CELL!,  X64ASM:RAX CLAUSE-CELL CELL!,
   CLEAR-LOCALS,
   X64ASM:RAX PEND-CELL CELL!, ;

\ Trusted recovery drops a rejected REPL line's stack without looping through
\ dynamic depth, which the native compiler cannot certify as a fixed effect.
: STACK-CLEAR-BODY ( -- )
   DSP DATA-REG STACK-ABI:BASE-CELL MOV-LOAD, ;

: IMM-MARK-BODY ( -- ) DNAME-IMM NEWEST-MARK-BODY ;

$C3 constant RET-OP

\ A created word's routine ends `jmp rel32; ret`, its last six bytes: the jump
\ falls through to the return while its displacement is 0, and does-patch aims
\ it at a clause, where ARM64 rewrites the routine's final RET word (habu2.f
\ LDOESPATCH). The slot is found by its position in the record's code.
\ The clause lies in the code region or the kernel's text, as every routine a
\ linked call reaches does, so its displacement fits rel32.

$E9 constant JMP-REL32                 \ the slot's jump
X64ASM:CALL-REL32-OFF 4 + constant JMP-BYTES  \ the jump, to its displacement's end
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
   X64ASM:RSI BODY-TEXT:BASE-CELL CELL@,
   X64ASM:RCX ZERO-REG,
   X64ASM:RDX BODYLEN-CELL CELL@,
   scan X64CODE:LBL,
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE done X64CODE:JCC,
   X64ASM:RAX X64ASM:RSI X64ASM:RCX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX STR:SPACE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   scan X64CODE:JMP,
   done X64CODE:LBL,
   X64ASM:RSI PUSH,  X64ASM:RCX PUSH, ;

\ Call the registrar in rax with the captured name and CRSIG, the effect the
\ clause declared. A registrar keeps no scratch register.
: RAW-CALL, ( -- )
   X64ASM:RAX X64ASM:RSP DP-XT MOV-STORE,
   PUSH-DREC-NAME,
   X64ASM:RAX CRSIG-A-CELL CELL@,  X64ASM:RAX PUSH,
   X64ASM:RAX CRSIG-U-CELL CELL@,  X64ASM:RAX PUSH,
   X64ASM:RAX X64ASM:RSP DP-XT MOV-LOAD,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG ;

\ The twin of habu2.f LASTC-TRUST:PUBLISH: the active checker's trust-raw;
\ without one, with the check hook armed, the target checker's, or the process
\ ends naming it; then the target checker's too unless the active one holds
\ the same operation. With neither a checker nor the hook nothing registers.
: RAW-PUBLISH, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: inactive:label ready:label other:label absent:label done:label :}
   X64ASM:RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E inactive X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX NCOMP-DISPATCH:DECL-RAW-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE ready X64CODE:JCC,
   inactive X64CODE:LBL,
   X64ASM:RCX HOOK-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-TARGET,
   ready X64CODE:LBL,
   RAW-CALL,
   X64ASM:RCX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   NCOMP-DISPATCH:DECL-RAW-OFF done DECL-TARGET,
   X64ASM:RCX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RCX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E other X64CODE:JCC,
   X64ASM:RCX X64ASM:RCX NCOMP-DISPATCH:DECL-RAW-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-E done X64CODE:JCC,
   other X64CODE:LBL,
   RAW-CALL,
   done X64CODE:JMP,
   absent X64CODE:LBL,
   s" trust-raw" EXIT-STATUS:INTERNAL STDERR-EXIT,
   done X64CODE:LBL, ;

\ rax = the active checker's operation at offset n of its record; with no
\ record or no operation the process ends naming it.
: OWNER-OP, ( n str -- ) {: off:n t:str :}
   X64CODE:LBL X64CODE:LBL {: absent:label found:label :}
   X64ASM:RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E absent X64CODE:JCC,
   X64ASM:RAX X64ASM:RAX off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE found X64CODE:JCC,
   absent X64CODE:LBL,
   t EXIT-STATUS:INTERNAL STDERR-EXIT,
   found X64CODE:LBL, ;

\ The active checker publishes width for NDICT-1 through its owner record.
\ A linked engine has that owner even when HOOK-CELL is zero.
: WIDE-PUBLISH, ( -- )
   X64CODE:LBL {: done:label :}
   X64ASM:RAX NCOMP-DISPATCH:DECL-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF s" rec-wide-publish" OWNER-OP,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   done X64CODE:LBL, ;

\ does-patch ( n str -- ) clause entry, created signature: the twin of
\ habu2.f DOESPATCH:PRIM and LDOESPATCH over the record LASTC-CELL names. Its
\ absent or retired record throws named code 70 before any patch. Its routine
\ must end in the slot, E9 at slot and C3 at slot+5, else the row exits 83
\ before it writes. Through the alias the displacement becomes the entry
\ less the slot's end, and the
\ record's DKIND clears: the body is a clause now, not a push. Entry 0 writes
\ displacement 0, the bare body, and leaves the kind as it is; with the
\ displacement already 0 it writes nothing. A signature then registers as
\ the created word's raw effect, the record's DNAME-WIDE clears for the
\ checker's tail to set again, and CRSIG clears.
\ Clear the DKIND of the record LASTC-CELL names and of every record that
\ shares its body: the twin of habu2.f DOESPATCH CLEAR-KIND. An alias
\ (alias-record) copies its source's [0] and DKIND:VAL, so the patch falsifies
\ its stamp too. An alias is newer than its source, so the scan walks the
\ records above LASTC, in every wordlist, and only when LASTC had a stamp. Each
\ match is written through its alias.
\ Clobbers rax rcx rdx rsi rdi r8 r10.
: DOES-KIND-CLEAR, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label next:label done:label :}
   X64CODE:LBL {: hw:label :}
   X64ASM:R8 LASTC-CELL CELL@,
   X64ASM:RAX X64ASM:R8 REC-FLAGS MOV-LOAD,
   X64ASM:RDI X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
   X64ASM:RSI DKIND:MASK invert IMM64,
   X64ASM:RSI X64ASM:RDI REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-AND-MR
   X64ASM:RCX DKIND:MASK IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-AND-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:R10 X64ASM:R8 DREC X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA                 \ the record above LASTC
   scan X64CODE:LBL,
   X64ASM:RAX NDICT-HW-CELL CELL@,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE hw X64CODE:JCC,
   X64ASM:RAX NDICT-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   hw X64CODE:LBL,
   X64ASM:RAX X64ASM:RAX DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:RAX DBASE-REG X64ASM:RAX 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA      \ the end of every record ever published
   X64ASM:R10 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE done X64CODE:JCC,
   X64ASM:R8 LASTC-CELL CELL@,  X64ASM:RCX X64ASM:R8 REC-CODE MOV-LOAD,      \ the patched body
   X64ASM:RDX X64ASM:R10 REC-CODE MOV-LOAD,
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE next X64CODE:JCC,
      X64ASM:RDI X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDI WRITE-ALIAS,
      X64ASM:RSI DKIND:MASK invert IMM64,
      X64ASM:RSI X64ASM:RDI REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-AND-MR
   next X64CODE:LBL,
   X64ASM:R10 DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   scan X64CODE:JMP,
   done X64CODE:LBL, ;

: DOES-NONE-CLASS$ ( -- str )
   s" E-DOES-UNCREATED habu: " ;

: DOES-NONE$ ( -- str )
   S\" does> has no created word\n" ;

: DOES-PATCH-BODY ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: patch:label write:label declared:label nocr:label noshadow:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL
   {: none:label have:label class:label msg:label bare:label :}
   X64ASM:RCX POP,  X64ASM:RCX CRSIG-U-CELL CELL!,
   X64ASM:RCX POP,  X64ASM:RCX CRSIG-A-CELL CELL!,
   X64ASM:RSP DP-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   DP-ENTRY POP-TO,
   X64ASM:R8 LASTC-CELL CELL@,
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E none X64CODE:JCC,
   X64ASM:RAX X64ASM:R8 REC-WID MOV-LOAD,
   X64ASM:RAX WL:RETIRED X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-NE have X64CODE:JCC,
   none X64CODE:LBL,
   class DOES-NONE-CLASS$ STR:LEN STDERR-WRITE,
   SOURCE-PLACE,
   msg DOES-NONE$ STR:LEN STDERR-WRITE,
   X64ASM:RAX 70 IMM32,  THROW,
   class DOES-NONE-CLASS$ X64CODE:DATA-AFTER  msg DOES-NONE$ X64CODE:DATA-AFTER
   have X64CODE:LBL,
   X64ASM:RDX X64ASM:R8 REC-CODE CELL + MOV-LOAD,
   X64ASM:RDX X64ASM:R8 REC-CODE X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RDX X64ASM:RDX PATCH-SLOT negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX JMP-REL32 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RAX X64ASM:RDX JMP-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
   X64ASM:RAX RET-OP X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-NE SEAL-TRAP-LBL X64CODE:JCC,
   X64ASM:RDX X64ASM:RSP DP-SLOT MOV-STORE,
   X64ASM:RAX X64ASM:RSP DP-ENTRY MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE patch X64CODE:JCC,
   0 X64ASM:>R32 X64ASM:RDX X64ASM:CALL-REL32-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E declared X64CODE:JCC,
   patch X64CODE:LBL,
   X64ASM:RAX X64ASM:RSP DP-ENTRY MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E bare X64CODE:JCC,
   DOES-KIND-CLEAR,
   bare X64CODE:LBL,
   X64ASM:RCX X64ASM:RSP DP-SLOT MOV-LOAD,
   X64ASM:RDX ZERO-REG,
   X64ASM:RAX X64ASM:RSP DP-ENTRY MOV-LOAD,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E write X64CODE:JCC,
   X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RDX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDX JMP-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   write X64CODE:LBL,
   X64ASM:RCX WRITE-ALIAS,
   2 X64ASM:>R32 X64ASM:RCX X64ASM:CALL-REL32-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR
   CODE-SYNC,
   declared X64CODE:LBL,
   X64ASM:R11 NCOMP-DISPATCH:DOES-SHADOW-CELL CELL@,
   X64ASM:R11 X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E noshadow X64CODE:JCC,
   X64ASM:RAX LASTC-CELL CELL@,  X64ASM:RAX DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RDX ZERO-REG,  X64ASM:RCX DREC IMM32,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:RAX PUSH,
   X64ASM:RAX X64ASM:RSP DP-ENTRY MOV-LOAD,  X64ASM:RAX PUSH,
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   noshadow X64CODE:LBL,
   X64ASM:RAX CRSIG-U-CELL CELL@,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E nocr X64CODE:JCC,
   RAW-PUBLISH,
   X64ASM:R8 LASTC-CELL CELL@,
   X64ASM:R8 WRITE-ALIAS,
   X64ASM:RAX DNAME-WIDE invert IMM64,
   X64ASM:RAX X64ASM:R8 REC-FLAGS X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-AND-MR
   WIDE-PUBLISH,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX CRSIG-A-CELL CELL!,  X64ASM:RAX CRSIG-U-CELL CELL!,
   nocr X64CODE:LBL,
   X64ASM:RSP DP-FRAME X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

public

\ GROW-LBL, which body-capture calls, comes first.
: DEFINITION, ( -- )
   X64CODE:LBL GROW-CELL !  GROW-HELPER,
   X64CODE:LBL REPLAY-TRAP-CELL !
   REPLAY-TRAP-LBL X64CODE:LBL,
   s" E-REPLAY-RECORD habu: replay record executed" EXIT-STATUS:INTERNAL X64CRASH:DIE-LINE,
   X64CODE:DATA-AFTER,
   s" namespace-record" [: NAMESPACE-RECORD-BODY ;] PRIM
   s" namespace-private" [: NAMESPACE-PRIVATE-BODY ;] PRIM
   s" alias-record" [: ALIAS-RECORD-BODY ;] PRIM
   s" package-scope!" [: PACKAGE-SCOPE-BODY ;] PRIM
   s" def-open" [: DEF-OPEN-BODY ;] PRIM
   s" body-capture" [: BODY-APPEND-BODY ;] PRIM
   s" trust-sig!" [: TRUST-SIG-BODY ;] PRIM
   s" created-sig!" [: CREATED-SIG-BODY ;] PRIM
   s" def-close" [: DEF-CLOSE-BODY ;] PRIM
   s" def-abort" [: DEF-ABORT-BODY ;] PRIM
   s" replay-open" [: REPLAY-OPEN-BODY ;] PRIM
   s" replay-close" [: REPLAY-CLOSE-BODY ;] PRIM
   s" replay-widn!" [: REPLAY-WIDN-BODY ;] PRIM
   s" replay-record" [: REPLAY-RECORD-BODY ;] PRIM
   s" record-wid!" [: RECORD-WID-BODY ;] PRIM
   s" record-rekey" [: RECORD-REKEY-BODY ;] PRIM
   s" replay-private" [: REPLAY-PRIVATE-BODY ;] PRIM
   s" stack-clear" [: STACK-CLEAR-BODY ;] PRIM
   s" imm-mark" [: IMM-MARK-BODY ;] PRIM
   s" does-patch" [: DOES-PATCH-BODY ;] PRIM ;

\ The arithmetic, comparison, shuffle, memory and float rows, each the twin of
\ its habu1.f body and written over the shapes below. A row reads its operands
\ where they stand and writes its answer over the deepest cell it consumed, so
\ a row that leaves as many cells as it takes never moves DSP. The guarded
\ stores are the twins of habu1.f BSTORE, BCSTORE and BPLUSSTORE, which
\ compiled stores call rather than inline, and f. is the twin of habu1.f BFDOT,
\ which writes its own line.
\
\ A float is a double's bits in a cell, worked in xmm0 and xmm1. An operation
\ that makes a NaN from numbers makes SSE's default NaN, $FFF8000000000000,
\ where ARM64 makes $7FF8000000000000, so the arithmetic rows flip that sign
\ (FCLOSE,). A NaN operand comes back quieted, but of two NaN operands SSE
\ answers the deeper and ARM64 a signalling one before a quiet one: on x86
\ `$7FF8000000000002 $7FF0000000000001 f+` is $7FF8000000000002, on ARM64
\ $7FF8000000000001.

private

$8000000000000000 constant SIGN-BIT     \ a double's sign
$43DFFFFFFFFFFFFF constant REALINT-TOP  \ every double above it is 2^63 or more

: POKE, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 depth:n :}
   r DSP depth 1+ CELL * negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: TOP!, ( X64ASM:r64 -- ) 0 POKE, ;

\ ( a b -- a ): rax = a and rcx = b; the row writes its answer over a.
: PAIR, ( -- ) X64ASM:RCX POP,  X64ASM:RAX 0 PEEK, ;

\ setcc into the register's low byte, which leaves the flags as they are.
: SET8, ( X64ASM:condition X64ASM:r64 -- ) X64ASM:R64>N X64ASM:>R8 X64CODE:ASM-SINK X64ASM:ENC-SETCC ;

\ The register's low byte, 0 or 1, as a flag cell: zero or all ones.
: BYTE>FLAG, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r r X64ASM:R64>N X64ASM:>R8 X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RR  r X64CODE:ASM-SINK X64ASM:ENC-NEG ;

: FLAG, ( X64ASM:condition X64ASM:r64 -- ) {: c:X64ASM:condition r:X64ASM:r64 :}
   c r SET8,  r BYTE>FLAG, ;

\ ( a b -- flag ): a against b, signed.
: COMPARE, ( X64ASM:condition -- ) {: c:X64ASM:condition :}
   PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  c X64ASM:RAX FLAG,  X64ASM:RAX TOP!, ;

: ZERO-COMPARE, ( X64ASM:condition -- ) {: c:X64ASM:condition :}
   X64ASM:RAX 0 PEEK,  X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  c X64ASM:RAX FLAG,  X64ASM:RAX TOP!, ;

\ ( a b -- a|b ): b where a stands in the relation to b, else a, as habu1.f
\ BMIN and BMAX keep a unless it is past b.
: EXTREME, ( X64ASM:condition -- ) {: c:X64ASM:condition :}
   PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  c X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC  X64ASM:RAX TOP!, ;

\ ( a -- |a| ): the sign smeared over rcx flips a negative a and adds one, so
\ MIN-N stays MIN-N as habu1.f BABS's negation leaves it. No branch and no
\ call: the row is marked scalar.
: ABS-BODY ( -- )
   X64ASM:RAX 0 PEEK,  X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX 63 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SAR-RI8
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  X64ASM:RAX TOP!, ;

\ ( a b -- ): rax = a / b toward zero, rdx = a mod b; idiv raises #DE on 0,
\ untested, and on MIN-N -1, so -1 negates: MIN-N stays MIN-N, remainder 0.
: DIVIDE, ( -- )
   X64CODE:LBL X64CODE:LBL {: hot:label done:label :}
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RCX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-NE hot X64CODE:JCC,
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NEG  X64ASM:RDX ZERO-REG,  done X64CODE:JMP,
   hot X64CODE:LBL,
   X64CODE:ASM-SINK X64ASM:ENC-CQO  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-IDIV
   done X64CODE:LBL, ;

: CFETCH, ( X64ASM:r64 X64ASM:r64 -- ) {: d:X64ASM:r64 a:X64ASM:r64 :}
   d a X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM ;

\ ( a b -- a ): xmm0 = a and xmm1 = b.
: FPAIR, ( -- )
   PAIR,  X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR  X64ASM:XMM1 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR ;

\ ( a -- a ): xmm0 = a and xmm1 = 0.0.
: FZERO, ( -- )
   X64ASM:RAX 0 PEEK,  X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR  X64ASM:XMM1 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-XORPD-RR ;

\ An arithmetic row's operands, ucomisd setting the parity flag where one is a
\ NaN: the SSE operations between it and FCLOSE, leave the flags alone.
: FBIN, ( -- ) FPAIR,  X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR ;
: FONE, ( -- )
   X64ASM:RAX 0 PEEK,  X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR  X64ASM:XMM0 X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR ;

\ Write xmm0 over the top cell, its sign flipped when it is a NaN its operands
\ were not: SSE's default NaN becomes ARM64's.
: FCLOSE, ( -- )
   X64CODE:LBL {: done:label :}
   X64ASM:RAX X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-MOVQ-RX
   X64ASM:C-P done X64CODE:JCC,
   X64ASM:XMM0 X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR  X64ASM:C-NP done X64CODE:JCC,
   X64ASM:RCX SIGN-BIT IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   done X64CODE:LBL,
   X64ASM:RAX TOP!, ;

\ All ones over the top cell where c holds for ucomisd x, y. An unordered pair
\ sets ZF, PF and CF, so A and AE are false when either is a NaN.
: FCOMPARE, ( X64ASM:xmm X64ASM:xmm X64ASM:condition -- ) {: x:X64ASM:xmm y:X64ASM:xmm c:X64ASM:condition :}
   x y X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR  c X64ASM:RAX FLAG,  X64ASM:RAX TOP!, ;

\ All ones over the top cell where x = y: equal and ordered.
: FEQ, ( X64ASM:xmm X64ASM:xmm -- ) {: x:X64ASM:xmm y:X64ASM:xmm :}
   x y X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR
   X64ASM:C-E X64ASM:RAX SET8,  X64ASM:C-NP X64ASM:RCX SET8,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-AND-RR  X64ASM:RAX BYTE>FLAG,  X64ASM:RAX TOP!, ;

\ ( a -- n ): habu1.f's fcvtzs, which truncates toward zero, saturates at both
\ ends and answers 0 for a NaN. cvttsd2si answers MIN-N past either end and
\ for a NaN: a double above REALINT-TOP flips it to MAX-N, and a NaN, unordered
\ with itself, takes zero.
: F>S-BODY ( -- )
   X64ASM:RAX 0 PEEK,  X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR
   X64ASM:RCX REALINT-TOP IMM64,  X64ASM:XMM1 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR
   X64ASM:RAX X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-CVTTSD2SI-RR
   X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR  X64ASM:C-A X64ASM:RCX FLAG,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   X64ASM:RCX ZERO-REG,
   X64ASM:XMM0 X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR  X64ASM:C-P X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RAX TOP!, ;

: STORE-BODY ( -- )
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: CSTORE-BODY ( -- )
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RAX X64ASM:R64>N X64ASM:>R8 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR ;

: ADDSTORE-BODY ( -- )
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-ADD-MR ;

\ f.'s frame, written downward from its top: a sign, 19 integer digits, the
\ point, six fraction digits and the newline are 28 bytes.
32 constant FDOT-BYTES
1000000 constant FRAC-SCALE             \ six fraction digits
6 constant FRAC-DIGITS

\ Put the low byte of a register one below rsi, which moves down to it.
: PUT, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   r X64ASM:R64>N X64ASM:>R8 X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR ;

: CHAR, ( n -- ) {: c:n :}  X64ASM:RCX c IMM32,  X64ASM:RCX PUT, ;

\ rax = ARM64 fcvtzs of a double that is a NaN or not negative, which is all
\ f. converts. cvttsd2si answers MIN-N for a NaN and past a cell, where fcvtzs
\ answers 0 and saturates to MAX-N: MIN-N's sign smeared over rcx flips it to
\ MAX-N, and a NaN, unordered with itself, then takes zero. It clobbers rcx.
: TRUNC-MAG, ( X64ASM:xmm -- ) {: x:X64ASM:xmm :}
   X64ASM:RAX x X64CODE:ASM-SINK X64ASM:ENC-CVTTSD2SI-RR
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RCX 63 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SAR-RI8
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR
   X64ASM:RCX ZERO-REG,
   x x X64CODE:ASM-SINK X64ASM:ENC-UCOMISD-RR  X64ASM:C-P X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC ;

\ The twin of habu1.f BFDOT: one write through the output device of `-`
\ when bit 63 is set, the decimal of I = fcvtzs(|x|), `.`, the six low digits,
\ zero-padded, of fcvtzs((|x| - I) * 1e6) and a newline. |x| clears bit 63, so
\ an infinity prints MAX-N and MAX-N's six low digits, and a NaN 0.000000
\ after its sign.
: FDOT-BODY ( -- )
   X64CODE:LBL X64CODE:LBL {: frac:label pos:label :}
   X64ASM:R8 POP,                                          \ the bits
   X64ASM:RSP FDOT-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RSI X64ASM:RSP FDOT-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   STR:LF CHAR,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
   X64ASM:XMM1 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR                    \ |x|
   X64ASM:XMM1 TRUNC-MAG,  X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR      \ r9 = I
   X64ASM:XMM2 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CVTSI2SD-RR
   X64ASM:XMM1 X64ASM:XMM2 X64CODE:ASM-SINK X64ASM:ENC-SUBSD-RR
   X64ASM:RCX FRAC-SCALE IMM32,  X64ASM:XMM2 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CVTSI2SD-RR
   X64ASM:XMM1 X64ASM:XMM2 X64CODE:ASM-SINK X64ASM:ENC-MULSD-RR
   X64ASM:XMM1 TRUNC-MAG,                                  \ rax = the fraction
   X64ASM:R10 FRAC-DIGITS IMM32,  X64ASM:RCX 10 IMM32,
   frac X64CODE:LBL,
      X64ASM:RDX ZERO-REG,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
      X64ASM:RDX [char] 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:RDX PUT,
      X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE frac X64CODE:JCC,
   [char] . CHAR,
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64RT:DIGITS,
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NS pos X64CODE:JCC,
   [char] - CHAR,
   pos X64CODE:LBL,
   X64ASM:RDX X64ASM:RSP FDOT-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64RT:G-OUT
   X64ASM:RSP FDOT-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

: ARITH-ROWS, ( -- )
   s" +" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:RAX TOP!, ;] PRIM
   s" -" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  X64ASM:RAX TOP!, ;] PRIM
   s" *" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR  X64ASM:RAX TOP!, ;] PRIM
   s" /" [: DIVIDE,  X64ASM:RAX PUSH, ;] PRIM
   s" mod" [: DIVIDE,  X64ASM:RDX PUSH, ;] PRIM
   s" /mod" [: DIVIDE,  X64ASM:RDX PUSH,  X64ASM:RAX PUSH, ;] PRIM
   s" abs" [: ABS-BODY ;] PRIM
   s" min" [: X64ASM:C-G EXTREME, ;] PRIM
   s" max" [: X64ASM:C-L EXTREME, ;] PRIM ;

\ A shift takes its count modulo 64 in cl, as habu1.f's lslv and lsrv do.
: COMPARE-ROWS, ( -- )
   s" =" [: X64ASM:C-E COMPARE, ;] PRIM
   s" <>" [: X64ASM:C-NE COMPARE, ;] PRIM
   s" <" [: X64ASM:C-L COMPARE, ;] PRIM
   s" >" [: X64ASM:C-G COMPARE, ;] PRIM
   s" <=" [: X64ASM:C-LE COMPARE, ;] PRIM
   s" >=" [: X64ASM:C-GE COMPARE, ;] PRIM
   s" 0=" [: X64ASM:C-E ZERO-COMPARE, ;] PRIM
   s" 0<" [: X64ASM:C-L ZERO-COMPARE, ;] PRIM
   s" 1+" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RAX TOP!, ;] PRIM
   s" 1-" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:RAX TOP!, ;] PRIM
   s" and" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-AND-RR  X64ASM:RAX TOP!, ;] PRIM
   s" or" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-OR-RR  X64ASM:RAX TOP!, ;] PRIM
   s" xor" [: PAIR,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX TOP!, ;] PRIM
   s" invert" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NOT  X64ASM:RAX TOP!, ;] PRIM
   s" negate" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NEG  X64ASM:RAX TOP!, ;] PRIM
   s" lshift" [: PAIR,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SHL-CL  X64ASM:RAX TOP!, ;] PRIM
   s" rshift" [: PAIR,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-SHR-CL  X64ASM:RAX TOP!, ;] PRIM ;

: STACK-ROWS, ( -- )
   s" dup" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX PUSH, ;] PRIM
   s" drop" [: DROP, ;] PRIM
   s" swap" [: X64ASM:RAX 0 PEEK,  X64ASM:RCX 1 PEEK,  X64ASM:RAX 1 POKE,  X64ASM:RCX 0 POKE, ;] PRIM
   s" nip" [: X64ASM:RAX POP,  X64ASM:RAX TOP!, ;] PRIM
   s" over" [: X64ASM:RAX 1 PEEK,  X64ASM:RAX PUSH, ;] PRIM
   s" tuck" [: X64ASM:RAX 0 PEEK,  X64ASM:RCX 1 PEEK,  X64ASM:RAX 1 POKE,  X64ASM:RCX 0 POKE,  X64ASM:RAX PUSH, ;] PRIM
   s" rot" [:
      X64ASM:RAX 2 PEEK,  X64ASM:RCX 1 PEEK,  X64ASM:RDX 0 PEEK,
      X64ASM:RCX 2 POKE,  X64ASM:RDX 1 POKE,  X64ASM:RAX 0 POKE, ;] PRIM
   s" -rot" [:
      X64ASM:RAX 2 PEEK,  X64ASM:RCX 1 PEEK,  X64ASM:RDX 0 PEEK,
      X64ASM:RDX 2 POKE,  X64ASM:RAX 1 POKE,  X64ASM:RCX 0 POKE, ;] PRIM
   s" 2dup" [: X64ASM:RAX 1 PEEK,  X64ASM:RCX 0 PEEK,  X64ASM:RAX PUSH,  X64ASM:RCX PUSH, ;] PRIM
   s" 2drop" [: DSP 2 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8 ;] PRIM
   s" 2swap" [:
      X64ASM:RAX 3 PEEK,  X64ASM:RCX 2 PEEK,  X64ASM:RDX 1 PEEK,  X64ASM:RSI 0 PEEK,
      X64ASM:RDX 3 POKE,  X64ASM:RSI 2 POKE,  X64ASM:RAX 1 POKE,  X64ASM:RCX 0 POKE, ;] PRIM
   s" 2over" [: X64ASM:RAX 3 PEEK,  X64ASM:RCX 2 PEEK,  X64ASM:RAX PUSH,  X64ASM:RCX PUSH, ;] PRIM ;

\ The views, PTR>N, >str, str> and chars change only the checked type: their bodies are the
\ ret.
: MEMORY-ROWS, ( -- )
   s" @" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  X64ASM:RAX TOP!, ;] PRIM
   s" !" [: STORE-BODY ;] PRIM
   s" ptr-field" [:
      PAIR,  X64ASM:RAX X64ASM:RAX X64ASM:RCX CELL 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA  X64ASM:RAX TOP!, ;] PRIM
   s" byte-view" [: ;] PRIM
   s" cell-view" [: ;] PRIM
   s" PTR>N" [: ;] PRIM
   s" >str" [: ;] PRIM
   s" str>" [: ;] PRIM
   s" >span" [: ;] PRIM
   s" span>" [: ;] PRIM
   s" data" [: DROP, ;] PRIM
   s" len" [: X64ASM:RAX POP,  X64ASM:RAX TOP!, ;] PRIM
   s" +!" [: ADDSTORE-BODY ;] PRIM
   s" c@" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64ASM:RAX CFETCH,  X64ASM:RAX TOP!, ;] PRIM
   s" c!" [: CSTORE-BODY ;] PRIM
   s" u16@" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-16-RM  X64ASM:RAX TOP!, ;] PRIM
   s" u32@" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV32-RM  X64ASM:RAX TOP!, ;] PRIM
   s" u16!" [: X64ASM:RCX POP,  X64ASM:RAX POP,  X64ASM:RAX X64ASM:R64>N X64ASM:>R16 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV16-MR ;] PRIM
   s" u32!" [: X64ASM:RCX POP,  X64ASM:RAX POP,  X64ASM:RAX X64ASM:R64>N X64ASM:>R32 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV32-MR ;] PRIM
   s" cells" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX 3 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX TOP!, ;] PRIM
   s" cell+" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX CELL X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:RAX TOP!, ;] PRIM
   s" chars" [: ;] PRIM
   s" char+" [: X64ASM:RAX 0 PEEK,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RAX TOP!, ;] PRIM
   s" count" [:
      X64ASM:RAX 0 PEEK,  X64ASM:RCX X64ASM:RAX CFETCH,
      X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RAX TOP!,  X64ASM:RCX PUSH, ;] PRIM ;

: FLOAT-ROWS, ( -- )
   s" f+" [: FBIN,  X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-ADDSD-RR  FCLOSE, ;] PRIM
   s" f-" [: FBIN,  X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-SUBSD-RR  FCLOSE, ;] PRIM
   s" f*" [: FBIN,  X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-MULSD-RR  FCLOSE, ;] PRIM
   s" f/" [: FBIN,  X64ASM:XMM0 X64ASM:XMM1 X64CODE:ASM-SINK X64ASM:ENC-DIVSD-RR  FCLOSE, ;] PRIM
   s" f<" [: FPAIR,  X64ASM:XMM1 X64ASM:XMM0 X64ASM:C-A FCOMPARE, ;] PRIM
   s" f=" [: FPAIR,  X64ASM:XMM0 X64ASM:XMM1 FEQ, ;] PRIM
   s" f>" [: FPAIR,  X64ASM:XMM0 X64ASM:XMM1 X64ASM:C-A FCOMPARE, ;] PRIM
   s" f<=" [: FPAIR,  X64ASM:XMM1 X64ASM:XMM0 X64ASM:C-AE FCOMPARE, ;] PRIM
   s" f>=" [: FPAIR,  X64ASM:XMM0 X64ASM:XMM1 X64ASM:C-AE FCOMPARE, ;] PRIM
   s" f0<" [: FZERO,  X64ASM:XMM1 X64ASM:XMM0 X64ASM:C-A FCOMPARE, ;] PRIM
   s" f0=" [: FZERO,  X64ASM:XMM0 X64ASM:XMM1 FEQ, ;] PRIM
   s" fabs" [:
      X64ASM:RAX 0 PEEK,  X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX 1 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
      X64ASM:RAX TOP!, ;] PRIM
   s" fnegate" [:
      X64ASM:RAX 0 PEEK,  X64ASM:RCX SIGN-BIT IMM64,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX TOP!, ;] PRIM
   s" fsqrt" [: FONE,  X64ASM:XMM0 X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-SQRTSD-RR  FCLOSE, ;] PRIM
   s" s>f" [:
      X64ASM:RAX 0 PEEK,  X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CVTSI2SD-RR  X64ASM:RAX X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-MOVQ-RX
      X64ASM:RAX TOP!, ;] PRIM
   s" f>s" [: F>S-BODY ;] PRIM
   s" f." [: FDOT-BODY ;] PRIM ;

public

: PURE, ( -- )
   ARITH-ROWS,  COMPARE-ROWS,  STACK-ROWS,  MEMORY-ROWS,  FLOAT-ROWS, ;
\ habu1.f's profiler rows: src/habu/prof-x64.f emits the SIGALRM handler, its
\ restorer, the index helpers, the sync, the printers and the reports once,
\ then each body.

private

\ prof-rate's guard, the twin of prof.f BPROF-RATE's: an interval below 0 or
\ above OS-TIMER-MAX-USEC throws PROF-ABI:E-PROF-RATE before
\ X64PROF:RATE-BODY maps the arena or stores the rate, so a caller that catches
\ it keeps the rate it had.
: RATE-GUARD, ( -- )
   X64CODE:LBL {: ok:label :}
   X64ASM:RAX DSP CELL negate MOV-LOAD,
   X64ASM:RCX OS-TIMER-MAX-USEC IMM64,
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-BE ok X64CODE:JCC,
   X64ASM:RAX PROF-ABI:E-PROF-RATE X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   THROW,
   ok X64CODE:LBL, ;

public

: PROFILER, ( -- )
   s" prof-on" [: X64PROF:ON-BODY ;] PRIM
   s" prof-off" [: X64PROF:OFF-BODY ;] PRIM
   s" prof-reset" [: X64PROF:RESET-BODY ;] PRIM
   s" prof-rate" [: RATE-GUARD,  X64PROF:RATE-BODY ;] PRIM
   s" prof-pc>rec" [: X64PROF:PCREC-BODY ;] PRIM
   s" prof-report" [: X64PROF:REPORT-BODY ;] PRIM
   s" prof-json" [: X64PROF:JSON-BODY ;] PRIM
   s" prof-row" [: X64PROF:ROW-BODY ;] PRIM
   \ A forked child has no interval timer and records its own owner at its
   \ prof-on, so clearing PROF-ARMED, prof-off's body, is the whole reset.
   s" prof-child-reset" [: X64PROF:OFF-BODY ;] PRIM ;

private

9 TYPED-BUFFER SCOPE-SITES n

public

: SCOPE-SITE ( n -- label ) SCOPE-SITES @ >LABEL ;

\ The linker fills the immediate of each ten-byte movabs after placement.
\ These are plain bytes, not assembler references that FIXUPS! could replace.
: SCOPE-OBJECT, ( -- )
   SCOPE-KIND-LBL X64CODE:LBL,
   9 2 ?do
      X64CODE:LBL {: next:label :}
      X64CODE:LBL dup LABEL>N i SCOPE-SITES ! X64CODE:LBL,
      X64ASM:RDX 0 IMM64,
      X64ASM:RDX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E next X64CODE:JCC,
      X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE next X64CODE:JCC,
      X64ASM:RAX i IMM32,  X64CODE:RET,
      next X64CODE:LBL,
   loop
   X64ASM:RAX ZERO-REG,  X64CODE:RET, ;

\ The kernel writer checks every emitted primitive against prims.f;
\ provided rows dispatch through cells the linked Habu runtime fills.
: KERNEL, ( -- )
   HELPERS,
   X64CODE:LBL SCOPE-SEED-CELL !
   X64CODE:LBL SCOPE-SEED-END-CELL !
   X64CODE:LBL SCOPE-KIND-CELL !
   s" (SCOPE-SEED)" SCOPE-SEED-LBL LABEL>N SCOPE-SEED-END-LBL LABEL>N
   ENGINE-PRIMS:HELPER-REGISTER
   SYSCALLS,
   UNCAUGHT-HELPER,
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
   SCOPE-SEED-LBL X64CODE:LBL,
   X64ASM:R8 ENGINE-PRIMS:COUNT DREC * IMM32,
   X64CODE:RET,
   SCOPE-SEED-END-LBL X64CODE:LBL, ;

;package
