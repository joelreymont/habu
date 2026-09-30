\ prof-x64.f - the x86-64 twin of src/habu/prof.f, package X64PROF: the
\ SIGALRM handler with its limit test, the pc index it searches, the sync a
\ report runs first, the text and JSON reports and the one-row report with
\ their printers, and the bodies of prof-on, prof-off, prof-reset, prof-rate,
\ prof-pc>rec, prof-report, prof-json and prof-row, which
\ src/habu/kernel-x64.f PROFILER, registers.
\
\ The band and the arena are PROF-ABI's (src/habu/prof-abi.f), the band at the
\ top of the x86-64 target's DATA (X64LAYOUT), so a report reads one layout on
\ every target. prof.f's header gives the design this keeps: the handler takes
\ nothing from the live registers, attributes only a context whose DATA and
\ DBASE registers hold what the band records, searches the index prof-on
\ builds instead of the dictionary, and runs on its own alternate stack. What
\ differs is the frame. x86-64 has no link register, so the walk starts at the
\ interrupted rsp, whose first cell is a leaf's return address, and searches
\ each code cell one byte back, inside the call that pushed it.
\
\ THE CALL CONTRACTS (docs/x86-64.md "Profiler rows"). FIND, EDGE and INDEX
\ keep every VM register (rbx rbp r12-r15) and clobber only the scratch
\ registers each names, as the kernel's helpers do, so a body calls them
\ without a frame. The handler owns every register: rt_sigreturn restores the
\ interrupted context whole from the frame the kernel built, which the handler
\ only reads. SYNC keeps every VM register too. A report saves rbx and r12-r15,
\ which it walks in, and never touches rbp, so the limit's dump from the
\ handler still leaves DATA there; it reads the dictionary base from
\ PROF-DBASE and the record count from ARN-NDICT, never from r13 and r14, which
\ the handler's walk has reused by the time it dumps. The printers clobber
\ every scratch register.
require lib/byte-buffer.f
require lib/string.f
require src/habu/layout.f
require src/habu/code-span.f
require src/habu/prof-abi.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/rt.f
require src/os/linux-x86-64/sys.f
require src/os/linux-x86-64/target-layout.f
require src/habu/boot-x64.f

package X64PROF
using X64ASM
using X64CODE
using X64RT
using PROF-ABI

\ The band, absolute: the x86-64 boot maps DATA fixed at DATA-VA.
X64LAYOUT:DATA-VA VA>N X64LAYOUT:DATA-SIZE PROF-BAND-AT constant BAND-VA
BAND-VA PROF-STATE-BYTES + constant CNT-VA      \ one counter per record
1000 constant DEFAULT-USEC                      \ the interval prof-on arms when prof-rate set none
1 constant STDOUT
2 constant STDERR
16 constant REC-FLAGS                           \ a record's flags: its name's length, low
24 constant REC-NAME                            \ its name, or under DNAME-EXT its address
48 constant NUM-BYTES                           \ NUM's frame: any cell's digits and a row's padding
8 constant COLS                                 \ a row's and a caller line's count columns
3 constant PROT-RW                              \ PROT_READ|PROT_WRITE
0 constant ITIMER-REAL
31 constant SPAN-FULL-BIT                       \ CODE-SPAN:FULL's bit
48 constant HASH-SHIFT                          \ a product's top 16 bits: the slot index, PROF-CALL-SLOTS wide
\ struct itimerval: the interval, then the first expiry, each seconds then
\ microseconds.
0 constant IT-INTERVAL-SEC
8 constant IT-INTERVAL-USEC
16 constant IT-VALUE-SEC
24 constant IT-VALUE-USEC
32 constant ITIMER-BYTES
\ stack_t: the base, the flags (an int and its padding), the size.
0 constant SS-SP
8 constant SS-FLAGS
16 constant SS-SIZE
24 constant SS-BYTES

\ The labels, made by HELPERS, in the stream it emits them into.
variable HANDLER-CELL
variable RESTORER-CELL
variable FIND-CELL
variable EDGE-CELL
variable INDEX-CELL
variable SYNC-CELL
variable NUM-CELL
variable PCT-CELL
variable NAME-CELL
variable QUAL-CELL
variable DUMP-CELL
variable JSON-CELL
variable ROW-CELL
: HANDLER-LBL ( -- label ) HANDLER-CELL @ >LABEL ;
: RESTORER-LBL ( -- label ) RESTORER-CELL @ >LABEL ;
: FIND-LBL ( -- label ) FIND-CELL @ >LABEL ;
: EDGE-LBL ( -- label ) EDGE-CELL @ >LABEL ;
: INDEX-LBL ( -- label ) INDEX-CELL @ >LABEL ;
: SYNC-LBL ( -- label ) SYNC-CELL @ >LABEL ;
: NUM-LBL ( -- label ) NUM-CELL @ >LABEL ;
: PCT-LBL ( -- label ) PCT-CELL @ >LABEL ;
: NAME-LBL ( -- label ) NAME-CELL @ >LABEL ;
: QUAL-LBL ( -- label ) QUAL-CELL @ >LABEL ;
: DUMP-LBL ( -- label ) DUMP-CELL @ >LABEL ;
: JSON-LBL ( -- label ) JSON-CELL @ >LABEL ;
: ROW-LBL ( -- label ) ROW-CELL @ >LABEL ;

: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: NDICT-REG ( -- r64 ) ENGINE-GPR:X64-NDICT >R64 ;

: LOAD, ( r64 mem -- ) ASM-SINK ENC-MOV-RM ;
: STORE, ( r64 mem -- ) ASM-SINK ENC-MOV-MR ;
: COPY, ( r64 r64 -- ) ASM-SINK ENC-MOV-RR ;
: LEA, ( r64 mem -- ) ASM-SINK ENC-LEA ;
: CMP-REG, ( r64 r64 -- ) ASM-SINK ENC-CMP-RR ;
: CMP-MEM, ( r64 mem -- ) ASM-SINK ENC-CMP-RM ;
: TEST, ( r64 -- ) dup ASM-SINK ENC-TEST-RR ;
: INC, ( r64 -- ) ASM-SINK ENC-INC ;
: DEC, ( r64 -- ) ASM-SINK ENC-DEC ;
: ADDI, ( r64 n -- ) >IMM32 ASM-SINK ENC-ADD-RI32 ;
: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;
: SCALE, ( r64 r64 n -- ) >IMM8 ASM-SINK ENC-IMUL-RRI8 ;
: RET, ( -- ) ASM-SINK ENC-RET ;

\ mov r32, imm32, which zero-extends: a small non-negative count or number.
: IMM32, ( r64 n -- ) {: r:r64 v:n :}
   r R64>N >R32 v >IMM32 ASM-SINK ENC-MOV32-RI32 ;

\ mov r64, imm32 sign-extended: -1 in seven bytes.
: ALL-ONES, ( r64 -- ) -1 >IMM32 ASM-SINK ENC-MOV-RI32 ;

\ Add one to the cell `off` past the register. It clobbers rcx.
: BUMP, ( r64 n -- ) {: base:r64 off:n :}
   RCX base off MEM-OFF LOAD,  RCX INC,  RCX base off MEM-OFF STORE, ;

\ rsi = the arena, read from the band.
: ARENA>RSI, ( -- )  RSI BAND-VA IMM,  RSI RSI PROF-ARENA MEM-OFF LOAD, ;

\ ---- the index ----------------------------------------------------------------
\ FIND ( rdi = pc, rsi = arena -- rax = index entry or 0 ): the twin of prof.f
\ LPROFFIND, the whole of pc attribution, shared by the handler and by
\ prof-pc>rec, so the row answers what a tick counts: the entry with the
\ greatest start <= pc, when pc lies below its end. Bounded: one range test
\ and at most log2(DICT-CAP) = 16 compares. Clobbers rcx rdx r8 r9 and the
\ flags; rdi and rsi survive.
: FIND-HELPER, ( -- )
   FIND-LBL LBL,
   LBL LBL LBL LBL LBL {: halve:label upper:label found:label miss:label out:label :}
   RCX RSI ARN-COUNT MEM-OFF LOAD,  RCX TEST,  C-E miss JCC,     \ hi = the entry count
   RDI RSI ARN-LO MEM-OFF CMP-MEM,  C-B miss JCC,                \ below every span
   RDI RSI ARN-HI MEM-OFF CMP-MEM,  C-AE miss JCC,               \ at or above every span
   RDX ZERO-REG,                                                 \ lo
   halve LBL,                                                    \ the rightmost entry with start <= pc
      RDX RCX CMP-REG,  C-AE found JCC,
      R8 RDX RCX 1 0 MEM-IDX LEA,  R8 1 >IMM8 ASM-SINK ENC-SHR-RI8   \ mid = (lo + hi) / 2
      R9 R8 PROF-ENT SCALE,
      R9 RSI R9 1 ARN-IDX ENT-START + MEM-IDX LOAD,
      R9 RDI CMP-REG,  C-A upper JCC,                            \ start > pc: mid is the new hi
      RDX R8 1 MEM-OFF LEA,  halve JMP,
   upper LBL,  RCX R8 COPY,  halve JMP,
   found LBL,
   RDX TEST,  C-E miss JCC,                                      \ every start is above the pc
   RAX RDX PROF-ENT SCALE,
   RAX RSI RAX 1 ARN-IDX PROF-ENT - MEM-IDX LEA,                 \ entry lo - 1
   RDI RAX ENT-END MEM-OFF CMP-MEM,  C-B out JCC,                \ else the pc sits in a gap above it
   miss LBL,  RAX ZERO-REG,
   out LBL,  RET, ;

\ EDGE ( rsi = arena, rdi = the sampled record, rdx = its caller's ): the twin
\ of prof.f LPROFEDGE, one caller edge counted in the open-addressed table,
\ keyed (sampled << PROF-REC-BITS | caller) + 1 as there. A sample whose own
\ record is unknown (rdi < 0) has no edge to key, and a full probe window
\ counts a drop rather than overwriting another pair. Clobbers rax rcx r8-r11
\ and the flags; rsi, rdi and rdx survive.
: EDGE-HELPER, ( -- )
   EDGE-LBL LBL,
   LBL LBL LBL LBL {: probe:label free:label hit:label done:label :}
   RDI TEST,  C-S done JCC,
   RAX RDI COPY,  RAX PROF-REC-BITS >IMM8 ASM-SINK ENC-SHL-RI8
   RAX RDX ASM-SINK ENC-OR-RR  RAX INC,                          \ the key, never 0
   RCX PROF-HASH IMM,  RCX RAX ASM-SINK ENC-IMUL-RR
   RCX HASH-SHIFT >IMM8 ASM-SINK ENC-SHR-RI8                     \ its first slot
   R8 RSI ARN-CALL MEM-OFF LEA,
   R9 PROF-CALL-PROBE IMM32,
   probe LBL,
      R10 RCX PROF-CALL-ENT SCALE,  R10 R8 ASM-SINK ENC-ADD-RR
      R11 R10 MEM-AT LOAD,
      R11 TEST,  C-E free JCC,
      R11 RAX CMP-REG,  C-E hit JCC,
      RCX INC,  RCX PROF-CALL-MASK >IMM32 ASM-SINK ENC-AND-RI32
      R9 DEC,  C-NE probe JCC,
   RSI ARN-DROP BUMP,  done JMP,
   free LBL,  RAX R10 MEM-AT STORE,  R11 1 IMM32,  R11 R10 CELL MEM-OFF STORE,  done JMP,
   hit LBL,  R10 CELL BUMP,
   done LBL,  RET, ;

\ rax = a record's raw length cell: its code bytes, the twin of prof.f
\ C-PROF-SPAN-BYTES (src/habu/code-span.f): an exact span is its body, a
\ legacy one its body and the final slot it is recorded before. Clobbers rbx.
: SPAN-BYTES, ( -- )
   RBX RAX COPY,  RBX SPAN-FULL-BIT >IMM8 ASM-SINK ENC-SHR-RI8     \ 1 for an exact span
   RAX CODE-SPAN:MASK >IMM32 ASM-SINK ENC-AND-RI32
   RAX CODE-SPAN:INSN-BYTES ADDI,
   RBX RBX CODE-SPAN:INSN-BYTES SCALE,  RAX RBX ASM-SINK ENC-SUB-RR ;

\ One entry per live record of [0, r14) with code of its own, from r13, in
\ dictionary order, then the header: the twin of prof.f C-PROF-INDEX-BUILD. A
\ record in DICT-WL:NAMESPACE (-1) or DICT-WL:RETIRED (-2), whose span is empty
\ or whose code cell is 0 owns no code; the +2 test names both wordlists at
\ once. Leaves rcx = the entry count and rbp = 0.
: BUILD, ( -- )
   LBL LBL LBL LBL {: turn:label next:label done:label low:label :}
   RDI RSI ARN-IDX MEM-OFF LEA,                                  \ the entry cursor
   RCX ZERO-REG,  RDX ZERO-REG,  R8 DBASE-REG COPY,              \ entries, record index, record
   R9 ALL-ONES,  R10 ZERO-REG,  RBP ZERO-REG,                    \ lo, hi, a zero
   turn LBL,
      RDX NDICT-REG CMP-REG,  C-AE done JCC,
      RAX R8 DICT-WL-OFF MEM-OFF LOAD,
      RAX DICT-WL:RETIRED negate ADDI,
      RAX DICT-WL:RETIRED negate >IMM8 ASM-SINK ENC-CMP-RI8  C-B next JCC,
      RAX R8 CELL MEM-OFF LOAD,  SPAN-BYTES,  RAX TEST,  C-E next JCC,
      R11 R8 MEM-AT LOAD,  R11 TEST,  C-E next JCC,
      R11 RDI ENT-START MEM-OFF STORE,
      RAX R11 ASM-SINK ENC-ADD-RR  RAX RDI ENT-END MEM-OFF STORE,
      RDX RDI ENT-IDX MEM-OFF STORE,
      RBP RDI ENT-INCL MEM-OFF STORE,
      RDI PROF-ENT ADDI,  RCX INC,
      R11 R9 CMP-REG,  C-AE low JCC,  R9 R11 COPY,
      low LBL,
      RAX R10 CMP-REG,  C-BE next JCC,  R10 RAX COPY,
   next LBL,  R8 DREC ADDI,  RDX INC,  turn JMP,
   done LBL,
   RCX RSI ARN-COUNT MEM-OFF STORE,  R9 RSI ARN-LO MEM-OFF STORE,
   R10 RSI ARN-HI MEM-OFF STORE,  NDICT-REG RSI ARN-NDICT MEM-OFF STORE, ;

\ One entry from the register's address to r15's, both advancing. Clobbers rax.
: ENT-COPY, ( r64 -- ) {: src:r64 :}
   PROF-ENT 0 ?do  RAX src i MEM-OFF LOAD,  RAX R15 i MEM-OFF STORE,  CELL +loop
   src PROF-ENT ADDI,  R15 PROF-ENT ADDI, ;

\ An odd number of passes leaves the sorted entries in the scratch half; bring
\ them home, rcx bytes from rdi, so the index always lives where the handler
\ looks: the twin of prof.f C-PROF-SORT-LAND.
: LAND, ( -- )
   LBL LBL {: turn:label done:label :}
   RAX RSI ARN-IDX MEM-OFF LEA,
   RDI RAX CMP-REG,  C-E done JCC,
   RDX ZERO-REG,
   turn LBL,
      RDX RCX CMP-REG,  C-AE done JCC,
      R9 RDI RDX 1 0 MEM-IDX LOAD,  R9 RAX RDX 1 0 MEM-IDX STORE,
      RDX CELL ADDI,  turn JMP,
   done LBL, ;

\ Bottom-up merge sort of rcx entries by start, the arena's scratch half the
\ other side of each pass: the twin of prof.f C-PROF-SORT. Stable, so records
\ that share a start keep dictionary order and FIND's "last start <= pc" is
\ the answer an exhaustive scan in record order gives. rdi and r8 are the
\ pass's input and output, r9 the input's end, r10 the merge width and rcx the
\ bytes, all in bytes; r11 the block, rdx its middle, rbx its end, rbp and r12
\ the two cursors, r15 the output cursor.
: SORT, ( -- )
   LBL LBL LBL LBL LBL LBL {: pass:label block:label merge:label ta:label tb:label arest:label :}
   LBL LBL LBL LBL LBL {: brest:label cb:label bfin:label done:label skip:label :}
   RCX 1 >IMM8 ASM-SINK ENC-CMP-RI8  C-BE skip JCC,
   RDI RSI ARN-IDX MEM-OFF LEA,
   R8 RSI ARN-SCR MEM-OFF LEA,
   RCX RCX PROF-ENT SCALE,
   R10 PROF-ENT IMM32,
   pass LBL,
      R10 RCX CMP-REG,  C-AE done JCC,
      R11 RDI COPY,  R9 RDI RCX 1 0 MEM-IDX LEA,
      block LBL,
         R11 R9 CMP-REG,  C-AE bfin JCC,
         RDX R11 R10 1 0 MEM-IDX LEA,  RDX R9 CMP-REG,  C-A RDX R9 ASM-SINK ENC-CMOVCC
         RBX RDX R10 1 0 MEM-IDX LEA,  RBX R9 CMP-REG,  C-A RBX R9 ASM-SINK ENC-CMOVCC
         RBP R11 COPY,  R12 RDX COPY,
         R15 R11 COPY,  R15 RDI ASM-SINK ENC-SUB-RR  R15 R8 ASM-SINK ENC-ADD-RR
         merge LBL,
            RBP RDX CMP-REG,  C-AE brest JCC,
            R12 RBX CMP-REG,  C-AE arest JCC,
            RAX RBP ENT-START MEM-OFF LOAD,  RAX R12 ENT-START MEM-OFF CMP-MEM,  C-A tb JCC,
         ta LBL,  RBP ENT-COPY,  merge JMP,
         tb LBL,  R12 ENT-COPY,  merge JMP,
         arest LBL,  RBP RDX CMP-REG,  C-AE cb JCC,  RBP ENT-COPY,  arest JMP,
         brest LBL,  R12 RBX CMP-REG,  C-AE cb JCC,  R12 ENT-COPY,  brest JMP,
         cb LBL,  R11 RBX COPY,  block JMP,
      bfin LBL,
      RDI R8 ASM-SINK ENC-XCHG-RR                                \ this pass's output is the next one's input
      R10 1 >IMM8 ASM-SINK ENC-SHL-RI8  pass JMP,
   done LBL,
   LAND,
   skip LBL, ;

\ INDEX ( rsi = arena ): build the index from the live dictionary, r13 and
\ r14, and sort it, so the header's count, lo, hi and record count describe
\ it. It borrows rbx rbp r12 r15 on the machine stack and gives them back, so
\ it keeps rsi and every VM register; it clobbers rax rcx rdx rdi r8-r11 and
\ the flags.
: INDEX-HELPER, ( -- )
   INDEX-LBL LBL,
   RBX ASM-SINK ENC-PUSH  RBP ASM-SINK ENC-PUSH  R12 ASM-SINK ENC-PUSH  R15 ASM-SINK ENC-PUSH
   BUILD,
   SORT,
   R15 ASM-SINK ENC-POP  R12 ASM-SINK ENC-POP  RBP ASM-SINK ENC-POP  RBX ASM-SINK ENC-POP
   RET, ;

\ ---- the sync ------------------------------------------------------------------
\ rsi = arena, the clock stopped: empty every entry's inclusive count into the
\ per-record array, so the rebuild that follows can move the entries freely:
\ the twin of prof.f C-PROF-INCL-FOLD, whose C-PROF-INCL+ comment gives why
\ inclusive counts live in two places. Clobbers rax rcx rdx r8.
: FOLD, ( -- )
   LBL LBL LBL {: turn:label next:label done:label :}
   RCX RSI ARN-IDX MEM-OFF LEA,
   RDX RSI ARN-COUNT MEM-OFF LOAD,  RDX RDX PROF-ENT SCALE,  RDX RCX ASM-SINK ENC-ADD-RR
   turn LBL,
      RCX RDX CMP-REG,  C-AE done JCC,
      RAX RCX ENT-INCL MEM-OFF LOAD,  RAX TEST,  C-E next JCC,
      R8 RCX ENT-IDX MEM-OFF LOAD,
      RAX RSI R8 CELL ARN-INCL MEM-IDX ASM-SINK ENC-ADD-MR
      RAX ZERO-REG,  RAX RCX ENT-INCL MEM-OFF STORE,
   next LBL,  RCX PROF-ENT ADDI,  turn JMP,
   done LBL, ;

\ Add one to the cell of the per-record array at rsi + reg * CELL + off.
\ Clobbers rax.
: RECORD-BUMP, ( r64 n -- ) {: ix:r64 off:n :}
   RAX RSI ix CELL off MEM-IDX LOAD,  RAX INC,  RAX RSI ix CELL off MEM-IDX STORE, ;

\ rsi = arena: replay the deferred samples through the rebuilt index, the twin
\ of prof.f C-PROF-REPLAY, whose comment gives the rules. The pc names the
\ sampled record, which takes the exclusive and the inclusive sample the
\ handler could not give it. The cell the handler kept at the interrupted rsp
\ names the caller when it is not 0, searched one byte back as the walk
\ searches a return address; a caller only the rebuild reached, its start at or
\ above ARN-OLDHI, takes the inclusive sample the handler's walk could not. A
\ pc that still names no record counts in ARN-NEW. It borrows rbx and r12, the
\ slot and the samples left, on the machine stack, and r10 holds the sampled
\ record across FIND, which keeps it.
: REPLAY, ( -- )
   LBL LBL LBL LBL LBL LBL {: turn:label miss:label nocall:label go:label next:label done:label :}
   RBX ASM-SINK ENC-PUSH  R12 ASM-SINK ENC-PUSH
   RBX RSI ARN-DEF MEM-OFF LEA,
   R12 RSI ARN-DEFER MEM-OFF LOAD,
   turn LBL,
      R12 TEST,  C-E done JCC,
      RDI RBX MEM-AT LOAD,
      FIND-LBL CALL,
      RAX TEST,  C-E miss JCC,
      R10 RAX ENT-IDX MEM-OFF LOAD,
      R8 CNT-VA IMM,
      RAX R8 R10 CELL 0 MEM-IDX LOAD,  RAX INC,  RAX R8 R10 CELL 0 MEM-IDX STORE,
      R10 ARN-INCL RECORD-BUMP,                                 \ a word is inside itself
      RDI RBX CELL MEM-OFF LOAD,
      RDI TEST,  C-E nocall JCC,
      RDI DEC,                                                  \ inside the call, not past it
      FIND-LBL CALL,
      RAX TEST,  C-E nocall JCC,
      RDX RAX ENT-IDX MEM-OFF LOAD,
      RDX R10 CMP-REG,  C-E nocall JCC,
      RCX RAX ENT-START MEM-OFF LOAD,
      RCX RSI ARN-OLDHI MEM-OFF CMP-MEM,  C-B go JCC,           \ the old index named it
      RDX ARN-INCL RECORD-BUMP,
      go JMP,
      nocall LBL,  RDX PROF-CALLER-NONE IMM32,
      go LBL,
      RDI R10 COPY,  EDGE-LBL CALL,
      next JMP,
      miss LBL,  RSI ARN-NEW BUMP,
   next LBL,  RBX PROF-DEFER-ENT ADDI,  R12 DEC,  turn JMP,
   done LBL,
   RAX ZERO-REG,  RAX RSI ARN-DEFER MEM-OFF STORE,
   R12 ASM-SINK ENC-POP  RBX ASM-SINK ENC-POP ;

\ SYNC ( -- ): rebuild the index from the dictionary as it now stands and
\ replay the deferred samples through it, the twin of prof.f LPROFSYNC, whose
\ comment gives why a report is the moment to name what the phase compiled.
\ ARN-OLDHI keeps the high mark the handler searched under. Only a body calls
\ it, never the limit's dump: INDEX reads r13 and r14, which only Habu code
\ holds live. With no arena it does nothing.
: SYNC-HELPER, ( -- )
   SYNC-LBL LBL,
   LBL {: none:label :}
   ARENA>RSI,  RSI TEST,  C-E none JCC,
   RAX RSI ARN-HI MEM-OFF LOAD,  RAX RSI ARN-OLDHI MEM-OFF STORE,
   FOLD,
   INDEX-LBL CALL,
   REPLAY,
   none LBL,  RET, ;

\ ---- the tick -------------------------------------------------------------------
\ The handler's registers across its calls, which keep them. rbp stays the
\ interrupted context's, DATA in a Habu sample, so a fault in the handler would
\ still reach the crash handler with the DATA its guard cases read.
: CUR-REG ( -- r64 ) RBX ;              \ the walk's cursor, first the interrupted rsp
: END-REG ( -- r64 ) R15 ;              \ where the walk stops
: SAMPLE-REG ( -- r64 ) R12 ;           \ the sampled record, -1 when the index names none
: SEEN-REG ( -- r64 ) R13 ;             \ the record the walk named last
: EDGED-REG ( -- r64 ) R14 ;            \ nonzero once the immediate caller's edge is in

\ Add one to the band's cell at n. It clobbers rax and rcx.
: BAND-BUMP, ( n -- ) {: off:n :}  RAX BAND-VA IMM,  RAX off BUMP, ;

\ The ucontext slots the handler reads (src/habu/boot-x64.f UC-GREG).
: DATA-SLOT ( -- n ) ENGINE-GPR:X64-RBASE >R64 X64BOOT:UC-GREG ;
: DBASE-SLOT ( -- n ) DBASE-REG X64BOOT:UC-GREG ;
: RSP-SLOT ( -- n ) RSP X64BOOT:UC-GREG ;

\ rax = an index entry, rdx = its record: one inclusive sample for that record,
\ and only the first time THIS sample reaches it, the twin of prof.f
\ C-PROF-INCL-ONCE, whose comment gives the reason. The stamp is the sample's
\ serial, PROF-TOT + 1, because TOT is bumped last. Clobbers rcx and r8.
: INCL-ONCE, ( -- )
   LBL {: seen:label :}
   RCX BAND-VA IMM,  RCX RCX PROF-TOT MEM-OFF LOAD,  RCX INC,
   R8 RSI RDX CELL ARN-STAMP MEM-IDX LEA,
   RCX R8 MEM-AT CMP-MEM,  C-E seen JCC,
   RCX R8 MEM-AT STORE,
   RAX ENT-INCL BUMP,
   seen LBL, ;

\ rdi = the pc: keep a sample whose code the index cannot name yet, the twin
\ of prof.f C-PROF-DEFER. It keeps the pc and the cell at the interrupted rsp,
\ where a leaf's call left its return address and aarch64 keeps x30, at a
\ fixed stride; a full buffer counts a spill. Clobbers rcx r8 r9.
: DEFER, ( -- )
   LBL LBL {: full:label done:label :}
   RCX RSI ARN-DEFER MEM-OFF LOAD,
   RCX PROF-DEFER-SLOTS >IMM32 ASM-SINK ENC-CMP-RI32  C-AE full JCC,
   R8 RCX PROF-DEFER-ENT SCALE,
   R8 RSI R8 1 ARN-DEF MEM-IDX LEA,
   RDI R8 MEM-AT STORE,
   R9 CUR-REG MEM-AT LOAD,  R9 R8 CELL MEM-OFF STORE,
   RCX INC,  RCX RSI ARN-DEFER MEM-OFF STORE,
   done JMP,
   full LBL,  RSI ARN-SPILL BUMP,
   done LBL, ;

\ The conservative machine-stack walk, the twin of prof.f C-PROF-WALK, whose
\ comment gives the technique, without its x30 step: the interrupted rsp's
\ first cell is a leaf's return address, so the scan meets the immediate caller
\ first. A cell is searched one byte back, inside the call that pushed it, so a
\ call that ends its record's span still names that record; a cell whose byte
\ before lies outside [ARN-LO, ARN-HI) is no code. The scan never leaves rsp's
\ 4 KiB block: every byte of it is mapped because rsp is, and the next block
\ may be a guard page. Habu code moves rsp by whole cells, so no cell the scan
\ reads straddles the block's end.
: WALK, ( -- )
   LBL LBL LBL {: scan:label done:label out:label :}
   SEEN-REG SAMPLE-REG COPY,                                     \ dedup seed: the sampled word
   EDGED-REG ZERO-REG,
   END-REG RSI ARN-WALK MEM-OFF LOAD,  END-REG END-REG CELL SCALE,
   END-REG CUR-REG ASM-SINK ENC-ADD-RR                           \ rsp + ARN-WALK cells
   RAX CUR-REG COPY,  RAX PROF-PAGE-MASK >IMM32 ASM-SINK ENC-OR-RI32  RAX INC,
   END-REG RAX CMP-REG,  C-A END-REG RAX ASM-SINK ENC-CMOVCC      \ or the block's end, if nearer
   scan LBL,
      CUR-REG END-REG CMP-REG,  C-AE done JCC,
      RDI CUR-REG MEM-AT LOAD,  CUR-REG CELL ADDI,
      RDI DEC,                                                   \ inside the call, not past it
      RDI RSI ARN-LO MEM-OFF CMP-MEM,  C-B scan JCC,
      RDI RSI ARN-HI MEM-OFF CMP-MEM,  C-AE scan JCC,
      FIND-LBL CALL,
      RAX TEST,  C-E scan JCC,
      RDX RAX ENT-IDX MEM-OFF LOAD,
      RDX SEEN-REG CMP-REG,  C-E scan JCC,                       \ the frame below named the same word
      INCL-ONCE,
      SEEN-REG RDX COPY,
      RSI ARN-FRAMES BUMP,
      EDGED-REG TEST,  C-NE scan JCC,                            \ the immediate caller is already in
      EDGED-REG 1 IMM32,
      RDI SAMPLE-REG COPY,  EDGE-LBL CALL,
      scan JMP,
   done LBL,
   EDGED-REG TEST,  C-NE out JCC,                                \ nothing named a caller: say so
   RDI SAMPLE-REG COPY,  RDX PROF-CALLER-NONE IMM32,  EDGE-LBL CALL,
   out LBL, ;

\ The limit test, the sample counted: the twin of prof.f EMIT-PROF's. A Habu
\ sample that brings PROF-TOT to a nonzero PROF-LIM, compared signed, prints
\ the text report and exits PROF-LIMIT-RC. The dump does not sync, so its
\ deferred samples stay in the header's "defer".
: LIMIT, ( -- )
   LBL {: below:label :}
   RAX BAND-VA IMM,
   RCX RAX PROF-LIM MEM-OFF LOAD,  RCX TEST,  C-E below JCC,     \ limit 0: sample until prof-off
   RCX RAX PROF-TOT MEM-OFF CMP-MEM,  C-G below JCC,            \ TOT below LIM
   DUMP-LBL CALL,
   RDI PROF-LIMIT-RC IMM32,  NR-EXIT-GROUP SYS,
   below LBL, ;

\ The handler: the twin of prof.f EMIT-PROF. The kernel enters it on the
\ alternate stack with rdx = the ucontext. A context whose rbp is DATA and
\ whose r13 is the DBASE prof-on recorded is a Habu sample; any other, a
\ foreign callee's, counts in PROF-FOREIGN and is not searched. With no arena
\ a Habu sample counts in PROF-OTHER. Otherwise the pc goes to the index: an
\ indexed pc bumps its record's counter and inclusive count; one at or above
\ ARN-HI, compiled after prof-on, is deferred; any other, engine code or a
\ gap, counts in PROF-OTHER; and each walks. PROF-TOT counts the sample last,
\ so sum(counters) + ARN-NEW + ARN-DEFER + ARN-SPILL + PROF-OTHER +
\ PROF-FOREIGN == PROF-TOT whenever no tick is running, the sample that
\ reaches the limit included. A foreign sample then returns; a Habu one runs
\ the limit test first, so a limit reached on a foreign sample reports at the
\ next Habu one. `ret` enters the restorer. No tick lands in the handler or
\ its restorer: the action has no SA_NODEFER, so the kernel blocks SIGALRM
\ until rt_sigreturn restores the interrupted mask.
: HANDLER, ( -- )
   HANDLER-LBL LBL,
   LBL LBL LBL {: unnamed:label other:label walk:label :}
   LBL LBL LBL {: noarena:label foreign:label total:label :}
   RAX RDX DATA-SLOT MEM-OFF LOAD,
   RCX X64LAYOUT:DATA-VA VA>N IMM,  RAX RCX CMP-REG,  C-NE foreign JCC,   \ rbp must be DATA
   RAX RDX DBASE-SLOT MEM-OFF LOAD,
   RCX BAND-VA IMM,
   RAX RCX PROF-DBASE MEM-OFF CMP-MEM,  C-NE foreign JCC,       \ r13 must be the recorded DBASE
   RSI RCX PROF-ARENA MEM-OFF LOAD,  RSI TEST,  C-E noarena JCC,
   CUR-REG RDX RSP-SLOT MEM-OFF LOAD,
   RDI RDX X64BOOT:UC-RIP MEM-OFF LOAD,
   FIND-LBL CALL,
   RAX TEST,  C-E unnamed JCC,
   SAMPLE-REG RAX ENT-IDX MEM-OFF LOAD,                          \ the record owning the pc
   R8 CNT-VA IMM,
   RCX R8 SAMPLE-REG CELL 0 MEM-IDX LOAD,  RCX INC,  RCX R8 SAMPLE-REG CELL 0 MEM-IDX STORE,
   RDX SAMPLE-REG COPY,  INCL-ONCE,                              \ a word is inside itself
   walk JMP,
   unnamed LBL,
   SAMPLE-REG ALL-ONES,                                          \ no record of its own: callers only
   RDI RSI ARN-HI MEM-OFF CMP-MEM,  C-B other JCC,               \ below the high mark: engine code or a gap
   DEFER,                                                        \ at or above it: compiled after prof-on
   walk JMP,
   other LBL,  PROF-OTHER BAND-BUMP,
   walk LBL,  WALK,  total JMP,
   foreign LBL,  PROF-FOREIGN BAND-BUMP,  PROF-TOT BAND-BUMP,    \ no search, and no report from here
   RET,
   noarena LBL,  PROF-OTHER BAND-BUMP,
   total LBL,  PROF-TOT BAND-BUMP,
   LIMIT,
   RET, ;

\ ---- printing --------------------------------------------------------------------
\ Everything a report prints goes through these, so a row, a caller line and a
\ JSON field agree about widths and about where a name comes from: the twins
\ of prof.f's printers. Each writes fd 1 directly, as those do.

\ Write the text; its bytes sit in the stream behind a jump.
: SAY, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,  a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past LBL,
   RDI STDOUT IMM32,  RSI text MOVABS,  RDX u IMM32,  NR-WRITE SYS, ;

\ Write the one byte, from a cell pushed for it.
: BYTE, ( n -- ) {: c:n :}
   RAX c IMM32,  RAX ASM-SINK ENC-PUSH
   RDI STDOUT IMM32,  RSI RSP COPY,  RDX 1 IMM32,  NR-WRITE SYS,
   RSP CELL >IMM8 ASM-SINK ENC-ADD-RI8 ;

: NL, ( -- ) s\" \n" SAY, ;
: DQ, ( -- ) s\" \q" SAY, ;

\ NUM ( rax = value, r8 = width ): unsigned decimal, right-aligned in r8
\ columns and never truncated, no newline: the twin of prof.f LPROFNUM. The
\ padding lands in the frame the digits do, so one write puts out the column.
: NUM-HELPER, ( -- )
   NUM-LBL LBL,
   LBL LBL {: pad:label done:label :}
   RSP NUM-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RSI RSP NUM-BYTES MEM-OFF LEA,
   DIGITS,                                                       \ rsi = the first digit
   RDX RSP NUM-BYTES MEM-OFF LEA,  RDX RSI ASM-SINK ENC-SUB-RR
   R8 RDX ASM-SINK ENC-SUB-RR                                    \ the columns the digits leave
   RCX STR-SPACE IMM32,
   pad LBL,
      R8 TEST,  C-LE done JCC,
      RSI DEC,  RCX R64>N >R8 RSI MEM-AT ASM-SINK ENC-MOV8-MR
      R8 DEC,  pad JMP,
   done LBL,
   RDX RSP NUM-BYTES MEM-OFF LEA,  RDX RSI ASM-SINK ENC-SUB-RR
   RDI STDOUT IMM32,  NR-WRITE SYS,
   RSP NUM-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   RET, ;

\ rax = the value, printed in n columns.
: NUM, ( n -- ) {: w:n :}  R8 w IMM32,  NUM-LBL CALL, ;

\ PCT ( rax = count, r8 = total ): the share in tenths of a percent, as
\ "100.0" right-aligned in six columns; a zero total prints 0.0 rather than
\ dividing by it: the twin of prof.f LPROFPCT, whose product wraps as this
\ one does.
: PCT-HELPER, ( -- )
   PCT-LBL LBL,
   LBL LBL {: zero:label go:label :}
   R8 TEST,  C-E zero JCC,
   RAX RAX 1000 >IMM32 ASM-SINK ENC-IMUL-RRI32
   RDX ZERO-REG,  R8 ASM-SINK ENC-DIV
   go JMP,
   zero LBL,  RAX ZERO-REG,
   go LBL,
   RCX 10 IMM32,  RDX ZERO-REG,  RCX ASM-SINK ENC-DIV            \ rax the whole percent, rdx the tenth
   RDX ASM-SINK ENC-PUSH
   4 NUM,
   [char] . BYTE,
   RAX ASM-SINK ENC-POP  1 NUM,
   RET, ;

\ NAME ( rdi = record ): its own name bytes, the twin of prof.f LPROFNAME. The
\ length is the flags cell under DNAME-LEN-MASK, below the four fields above
\ it, and the name sits inline unless DNAME-EXT says the cell holds its
\ address.
: NAME-HELPER, ( -- )
   NAME-LBL LBL,
   LBL {: inline:label :}
   RDX RDI REC-FLAGS MEM-OFF LOAD,
   RSI RDI REC-NAME MEM-OFF LEA,
   RAX DNAME-EXT IMM,  RAX RDX ASM-SINK ENC-TEST-RR  C-E inline JCC,
   RSI RDI REC-NAME MEM-OFF LOAD,
   inline LBL,
   RAX DNAME-LEN-MASK IMM,  RDX RAX ASM-SINK ENC-AND-RR
   RDI STDOUT IMM32,  NR-WRITE SYS,
   RET, ;

\ QUAL ( rdi = record ): "PKG:" when the record's wordlist cell is a package
\ row's public or private wid, the first such row in dictionary order, and
\ nothing for the global wordlist 0: the twin of prof.f LPROFQUAL, whose
\ comment gives why the rows themselves answer this. It walks the records
\ from PROF-DBASE up to ARN-NDICT and keeps rdi.
: QUAL-HELPER, ( -- )
   QUAL-LBL LBL,
   LBL LBL LBL LBL {: turn:label next:label found:label done:label :}
   RAX RDI DICT-WL-OFF MEM-OFF LOAD,
   RAX TEST,  C-E done JCC,
   R9 BAND-VA IMM,
   R8 R9 PROF-DBASE MEM-OFF LOAD,                                \ the record
   R10 R9 PROF-ARENA MEM-OFF LOAD,  R10 R10 ARN-NDICT MEM-OFF LOAD,   \ the records left
   turn LBL,
      R10 TEST,  C-E done JCC,
      RDX R8 DICT-WL-OFF MEM-OFF LOAD,
      RDX DICT-WL:NAMESPACE >IMM8 ASM-SINK ENC-CMP-RI8  C-NE next JCC,
      RAX R8 MEM-AT CMP-MEM,  C-E found JCC,                     \ its public wid
      RAX R8 CELL MEM-OFF CMP-MEM,  C-E found JCC,               \ its private wid
   next LBL,  R8 DREC ADDI,  R10 DEC,  turn JMP,
   found LBL,
   RDI ASM-SINK ENC-PUSH
   RDI R8 COPY,  NAME-LBL CALL,
   [char] : BYTE,
   RDI ASM-SINK ENC-POP
   done LBL,  RET, ;

\ rdi = a record: its qualified name.
: NAMED, ( -- )  QUAL-LBL CALL,  NAME-LBL CALL, ;

\ ---- the reports -----------------------------------------------------------------
\ The twins of prof.f LPROFDUMP, LPROFJSON and LPROFROW, whose comments give
\ the design: rows chosen by repeated maximum, so the counters stay exactly as
\ they were; JSON carrying every attributed word and every caller edge.
\
\ REGISTERS. What lives across a printed column is in rbx and r12-r15, which
\ a report saves, or in its frame: r15 the arena, rbx the row's entry, r12 the
\ row rank, r13 and r14 the (count, entry) threshold the next row must fall
\ under. The JSON walks take rbx for their cursor, r12 for the comma and r13
\ for the end or the slots left.
0 constant AT-SAMPLES                   \ PROF-TOT, read once
8 constant AT-EXCL                      \ the row's exclusive count
16 constant AT-DENOM                    \ what its caller shares are of
24 constant AT-RANK                     \ the caller rank
32 constant AT-CAP                      \ the caller count the next line must fall under
40 constant AT-REC                      \ the caller line's record, 0 for none
48 constant AT-N                        \ the caller line's count
56 constant AT-WORDS                    \ samples attributed to a word
64 constant AT-ATTR                     \ index entries either count reached
72 constant FRAME-BYTES

: SLOT@, ( r64 n -- ) {: r:r64 off:n :}  r RSP off MEM-OFF LOAD, ;
: SLOT!, ( r64 n -- ) {: r:r64 off:n :}  r RSP off MEM-OFF STORE, ;

: OPEN, ( -- )
   RBX ASM-SINK ENC-PUSH  R12 ASM-SINK ENC-PUSH  R13 ASM-SINK ENC-PUSH
   R14 ASM-SINK ENC-PUSH  R15 ASM-SINK ENC-PUSH
   RSP FRAME-BYTES >IMM8 ASM-SINK ENC-SUB-RI8 ;

: CLOSE, ( -- )
   RSP FRAME-BYTES >IMM8 ASM-SINK ENC-ADD-RI8
   R15 ASM-SINK ENC-POP  R14 ASM-SINK ENC-POP  R13 ASM-SINK ENC-POP
   R12 ASM-SINK ENC-POP  RBX ASM-SINK ENC-POP
   RET, ;

\ r15 = the arena and the samples in their slot, read from the band.
: SAMPLES, ( -- )
   RAX BAND-VA IMM,
   RCX RAX PROF-TOT MEM-OFF LOAD,  RCX AT-SAMPLES SLOT!,
   R15 RAX PROF-ARENA MEM-OFF LOAD, ;

\ The register = the address one past the index's last entry, from its first.
: INDEX-END, ( r64 r64 -- ) {: end:r64 first:r64 :}
   end R15 ARN-COUNT MEM-OFF LOAD,  end end PROF-ENT SCALE,  end first ASM-SINK ENC-ADD-RR ;

\ rax = the row's exclusive count, its record's counter.
: ROW-EXCL, ( -- )
   RAX RBX ENT-IDX MEM-OFF LOAD,
   RCX CNT-VA IMM,  RAX RCX RAX CELL 0 MEM-IDX LOAD, ;

\ rax = the row's inclusive count: the per-record array plus what its entry
\ took since the last fold. An auto-report at the limit never folds, so the
\ second half is the only half it has.
: ROW-INCL, ( -- )
   RAX RBX ENT-IDX MEM-OFF LOAD,
   RAX R15 RAX CELL ARN-INCL MEM-IDX LOAD,
   RAX RBX ENT-INCL MEM-OFF ASM-SINK ENC-ADD-RM ;

\ rdi = the record behind the row.
: ROW-REC, ( -- )
   RDI RBX ENT-IDX MEM-OFF LOAD,  RDI RDI DREC SCALE,
   RAX BAND-VA IMM,  RDI RAX PROF-DBASE MEM-OFF ASM-SINK ENC-ADD-RM ;

\ The header's two sums: the samples attributed to a word, over every
\ record's counter up to ARN-NDICT, whose reason prof.f
\ C-PROF-REP-WORDSUM gives, and how many index entries either count reached,
\ which is how many rows the reports can name.
: WORDSUM, ( -- )
   LBL LBL LBL LBL LBL {: sum:label summed:label turn:label next:label done:label :}
   RAX ZERO-REG,  RCX CNT-VA IMM,  RDX R15 ARN-NDICT MEM-OFF LOAD,
   sum LBL,
      RDX TEST,  C-E summed JCC,
      RAX RCX MEM-AT ASM-SINK ENC-ADD-RM
      RCX CELL ADDI,  RDX DEC,  sum JMP,
   summed LBL,  RAX AT-WORDS SLOT!,
   R8 ZERO-REG,  R11 CNT-VA IMM,
   RCX R15 ARN-IDX MEM-OFF LEA,  RDX RCX INDEX-END,
   turn LBL,
      RCX RDX CMP-REG,  C-AE done JCC,
      R9 RCX ENT-IDX MEM-OFF LOAD,
      RAX R11 R9 CELL 0 MEM-IDX LOAD,
      RAX RCX ENT-INCL MEM-OFF ASM-SINK ENC-OR-RM
      RAX R15 R9 CELL ARN-INCL MEM-IDX ASM-SINK ENC-OR-RM
      C-E next JCC,  R8 INC,                                     \ a word either count reached
   next LBL,  RCX PROF-ENT ADDI,  turn JMP,
   done LBL,  R8 AT-ATTR SLOT!, ;

\ A header field: the text, then the value in one column, from the frame, the
\ band or the arena.
: FRAME-FIELD, ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   a u SAY,  RAX off SLOT@,  1 NUM, ;
: BAND-FIELD, ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   a u SAY,  RAX BAND-VA IMM,  RAX RAX off MEM-OFF LOAD,  1 NUM, ;
: ARENA-FIELD, ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   a u SAY,  RAX R15 off MEM-OFF LOAD,  1 NUM, ;

\ The header: words + other + new + defer + spill + foreign == samples is the
\ identity it states. After a sync, defer is 0 and new holds the deferred
\ samples whose pc belongs to no live record; the limit's dump does not sync.
: HEAD, ( bool -- ) {: json:bool :}
   json if s\" {\"samples\":" else s" profiler samples " then AT-SAMPLES FRAME-FIELD,
   json if s\" ,\"words\":" else s"  words " then AT-WORDS FRAME-FIELD,
   json if s\" ,\"other\":" else s"  other " then PROF-OTHER BAND-FIELD,
   json if s\" ,\"new\":" else s"  new " then ARN-NEW ARENA-FIELD,
   json if s\" ,\"defer\":" else s"  defer " then ARN-DEFER ARENA-FIELD,
   json if s\" ,\"spill\":" else s"  spill " then ARN-SPILL ARENA-FIELD,
   json if s\" ,\"foreign\":" else s"  foreign " then PROF-FOREIGN BAND-FIELD,
   json if s\" ,\"frames\":" else s"  frames " then ARN-FRAMES ARENA-FIELD,
   json if s\" ,\"dropped\":" else s"  dropped " then ARN-DROP ARENA-FIELD,
   json if s\" ,\"indexed\":" else s"  indexed " then ARN-COUNT ARENA-FIELD,
   json if s\" ,\"usec\":" else s"  usec " then ARN-USEC ARENA-FIELD,
   json if s\" ,\"attributed\":" else s"  attributed " then AT-ATTR FRAME-FIELD,
   json if exit then
   NL, ;

\ One text row: exclusive, its share, inclusive, its share, then the word.
: REP-ROW, ( -- )
   RAX AT-EXCL SLOT@,  COLS NUM,
   RAX AT-EXCL SLOT@,  R8 AT-SAMPLES SLOT@,  PCT-LBL CALL,
   ROW-INCL,  COLS NUM,
   ROW-INCL,  R8 AT-SAMPLES SLOT@,  PCT-LBL CALL,
   s"   " SAY,
   ROW-REC,  NAMED,
   NL, ;

\ rax = the record behind the caller index in r9, or 0 for PROF-CALLER-NONE,
\ which stands for a sample the walk could not attribute and owns no row.
: CALLER-REC, ( -- )
   LBL LBL {: none:label done:label :}
   R9 PROF-CALLER-NONE >IMM32 ASM-SINK ENC-CMP-RI32  C-E none JCC,
   RAX R9 DREC SCALE,
   RCX BAND-VA IMM,  RAX RCX PROF-DBASE MEM-OFF ASM-SINK ENC-ADD-RM
   done JMP,
   none LBL,  RAX ZERO-REG,
   done LBL, ;

\ rdi = a record or 0: its qualified name, or (unknown).
: CALLER-NAME, ( -- )
   LBL LBL {: none:label done:label :}
   RDI TEST,  C-E none JCC,
   NAMED,  done JMP,
   none LBL,  s" (unknown)" SAY,
   done LBL, ;

\ One caller line: r9 = the caller's record index, r8 = the edge count, and its
\ share of the row's own denominator, whose reason prof.f C-PROF-REP-CALLER
\ gives.
: REP-CALLER, ( -- )
   CALLER-REC,  RAX AT-REC SLOT!,  R8 AT-N SLOT!,
   s"          <- " SAY,
   RAX AT-N SLOT@,  COLS NUM,
   RAX AT-N SLOT@,  R8 AT-DENOM SLOT@,  PCT-LBL CALL,
   s"  " SAY,
   RDI AT-REC SLOT@,  CALLER-NAME,
   NL, ;

\ The row's top callers: at most PROF-CALLERS passes over the edge table, each
\ taking the largest count below the one before it, the first slot on a tie:
\ the twin of prof.f C-PROF-REP-CALLERS.
: REP-CALLERS, ( -- )
   LBL LBL LBL LBL LBL {: rank:label scan:label next:label scanned:label done:label :}
   RAX ZERO-REG,  RAX AT-RANK SLOT!,
   RAX ALL-ONES,  RAX AT-CAP SLOT!,                              \ nothing printed yet
   rank LBL,
      RAX AT-RANK SLOT@,  RAX PROF-CALLERS >IMM8 ASM-SINK ENC-CMP-RI8  C-AE done JCC,
      R8 ZERO-REG,  R9 ZERO-REG,                                 \ the best count, its caller
      RCX R15 ARN-CALL MEM-OFF LEA,  RDX PROF-CALL-SLOTS IMM32,
      R10 RBX ENT-IDX MEM-OFF LOAD,  R11 AT-CAP SLOT@,
      scan LBL,
         RDX TEST,  C-E scanned JCC,
         RAX RCX MEM-AT LOAD,  RAX TEST,  C-E next JCC,
         RAX DEC,  RSI RAX COPY,  RSI PROF-REC-BITS >IMM8 ASM-SINK ENC-SHR-RI8
         RSI R10 CMP-REG,  C-NE next JCC,                        \ another word's edge
         RAX PROF-REC-MASK >IMM32 ASM-SINK ENC-AND-RI32
         RDI RCX CELL MEM-OFF LOAD,
         RDI R11 CMP-REG,  C-AE next JCC,                        \ at or above the cap: printed
         RDI R8 CMP-REG,  C-BE next JCC,
         R8 RDI COPY,  R9 RAX COPY,
      next LBL,  RCX PROF-CALL-ENT ADDI,  RDX DEC,  scan JMP,
      scanned LBL,
      R8 TEST,  C-E done JCC,                                    \ no caller edge left
      R8 AT-CAP SLOT!,
      REP-CALLER,
      RAX AT-RANK SLOT@,  RAX INC,  RAX AT-RANK SLOT!,
      rank JMP,
   done LBL, ;

\ rcx = an index entry: r9 = what it is ranked on, its exclusive count or the
\ inclusive one a phase word is only ever visible by. Clobbers r10 r11.
: RANK, ( bool -- ) {: incl:bool :}
   R10 RCX ENT-IDX MEM-OFF LOAD,
   incl if
      R9 R15 R10 CELL ARN-INCL MEM-IDX LOAD,
      R9 RCX ENT-INCL MEM-OFF ASM-SINK ENC-ADD-RM
      exit
   then
   R11 CNT-VA IMM,  R9 R11 R10 CELL 0 MEM-IDX LOAD, ;

\ One text section: at most PROF-ROWS rows ranked on one of the two counts,
\ each with its callers, the twin of prof.f C-PROF-REP-SECTION: a row takes
\ the greatest count under the threshold, the first entry on a tie, and prints
\ its exclusive count whichever it was ranked on.
: SECTION, ( bool -- ) {: incl:bool :}
   LBL LBL LBL LBL {: row:label sel:label tie:label skip:label :}
   LBL LBL LBL {: take:label chosen:label done:label :}
   R13 ALL-ONES,  R14 ALL-ONES,  R12 ZERO-REG,
   row LBL,
      R8 ZERO-REG,  RBX ALL-ONES,
      RCX R15 ARN-IDX MEM-OFF LEA,  RDX RCX INDEX-END,
      sel LBL,
         RCX RDX CMP-REG,  C-AE chosen JCC,
         incl RANK,
         R9 TEST,  C-E skip JCC,
         R9 R13 CMP-REG,  C-A skip JCC,                          \ above the threshold: printed
         C-NE tie JCC,
         RCX R14 CMP-REG,  C-BE skip JCC,                        \ the threshold row itself, or before it
         tie LBL,
         R9 R8 CMP-REG,  C-A take JCC,
         C-NE skip JCC,
         RCX RBX CMP-REG,  C-AE skip JCC,                        \ the same count later: keep the first
         take LBL,
         R8 R9 COPY,  RBX RCX COPY,
      skip LBL,  RCX PROF-ENT ADDI,  sel JMP,
      chosen LBL,
      R8 TEST,  C-E done JCC,                                    \ no counted row left
      R13 R8 COPY,  R14 RBX COPY,
      ROW-EXCL,  RAX AT-EXCL SLOT!,
      incl if ROW-INCL, then  RAX AT-DENOM SLOT!,
      REP-ROW,
      REP-CALLERS,
      R12 INC,
      R12 PROF-ROWS >IMM8 ASM-SINK ENC-CMP-RI8  C-B row JCC,
   done LBL, ;

\ Every index entry either count reached, as one object each, in index order.
: JSON-ROWS, ( -- )
   LBL LBL LBL LBL {: turn:label first:label next:label done:label :}
   s\" ,\"rows\":[" SAY,
   R12 ZERO-REG,
   RBX R15 ARN-IDX MEM-OFF LEA,  R13 RBX INDEX-END,
   turn LBL,
      RBX R13 CMP-REG,  C-AE done JCC,
      ROW-EXCL,  RAX AT-EXCL SLOT!,
      ROW-INCL,  RAX RSP AT-EXCL MEM-OFF ASM-SINK ENC-OR-RM
      C-E next JCC,                                              \ neither count moved: no row
      R12 TEST,  C-E first JCC,  s" ," SAY,
      first LBL,  R12 1 IMM32,
      s\" {\"word\":\"" SAY,
      ROW-REC,  NAMED,
      DQ,  s\" ,\"excl\":" SAY,
      RAX AT-EXCL SLOT@,  1 NUM,
      s\" ,\"incl\":" SAY,
      ROW-INCL,  1 NUM,
      s" }" SAY,
   next LBL,  RBX PROF-ENT ADDI,  turn JMP,
   done LBL,
   s" ]" SAY, ;

\ r9 = a record index or PROF-CALLER-NONE: its name as a JSON string body.
: JSON-NAME, ( -- )  CALLER-REC,  RDI RAX COPY,  CALLER-NAME, ;

\ Every caller edge, in slot order, flat rather than under its row.
: JSON-EDGES, ( -- )
   LBL LBL LBL LBL {: turn:label first:label next:label done:label :}
   s\" ,\"edges\":[" SAY,
   R12 ZERO-REG,
   RBX R15 ARN-CALL MEM-OFF LEA,  R13 PROF-CALL-SLOTS IMM32,
   turn LBL,
      R13 TEST,  C-E done JCC,
      RAX RBX MEM-AT LOAD,  RAX TEST,  C-E next JCC,
      R12 TEST,  C-E first JCC,  s" ," SAY,
      first LBL,  R12 1 IMM32,
      s\" {\"word\":\"" SAY,
      R9 RBX MEM-AT LOAD,  R9 DEC,  R9 PROF-REC-BITS >IMM8 ASM-SINK ENC-SHR-RI8
      JSON-NAME,                                                 \ the sampled word
      DQ,  s\" ,\"caller\":\"" SAY,
      R9 RBX MEM-AT LOAD,  R9 DEC,  R9 PROF-REC-MASK >IMM32 ASM-SINK ENC-AND-RI32
      JSON-NAME,                                                 \ its caller
      DQ,  s\" ,\"n\":" SAY,
      RAX RBX CELL MEM-OFF LOAD,  1 NUM,
      s" }" SAY,
   next LBL,  RBX PROF-CALL-ENT ADDI,  R13 DEC,  turn JMP,
   done LBL,
   s\" ]}\n" SAY, ;

\ DUMP or JSON ( -- ): the report of the band and the arena as they stand, the
\ flag picking the punctuation at build time, so the two can never disagree
\ about what they counted. With no arena it says so.
: REPORT-HELPER, ( bool -- ) {: json:bool :}
   json if JSON-LBL else DUMP-LBL then LBL,
   LBL {: armed:label :}
   OPEN,
   SAMPLES,
   R15 TEST,  C-NE armed JCC,
   json if s\" {\"samples\":0,\"armed\":false}\n" else s\" profiler not armed\n" then SAY,
   CLOSE,
   armed LBL,
   WORDSUM,
   json HEAD,
   json if
      JSON-ROWS,  JSON-EDGES,
   else
      false SECTION,
      s" by inclusive" SAY,  NL,
      true SECTION,
   then
   CLOSE, ;

\ ROW ( rdi = a record index ): the text row for that record whatever its
\ rank, with its callers, or a line saying the index has none: the twin of
\ prof.f LPROFROW. Its caller shares are of the row's inclusive count.
: ROW-HELPER, ( -- )
   ROW-LBL LBL,
   LBL LBL LBL {: turn:label found:label none:label :}
   OPEN,
   SAMPLES,
   R15 TEST,  C-E none JCC,
   RBX R15 ARN-IDX MEM-OFF LEA,  R13 RBX INDEX-END,
   turn LBL,
      RBX R13 CMP-REG,  C-AE none JCC,
      RDI RBX ENT-IDX MEM-OFF CMP-MEM,  C-E found JCC,
      RBX PROF-ENT ADDI,  turn JMP,
   found LBL,
   ROW-EXCL,  RAX AT-EXCL SLOT!,
   ROW-INCL,  RAX AT-DENOM SLOT!,
   REP-ROW,
   REP-CALLERS,
   CLOSE,
   none LBL,
   s" profiler: no row for that record" SAY,  NL,
   CLOSE, ;

public

\ Make the labels and emit the handler, its restorer, the index helpers, the
\ sync, the printers and the reports, where no control falls in; the rows that
\ call them follow in the same stream. src/habu/kernel-x64.f PROFILER, calls
\ it.
: HELPERS, ( -- )
   LBL HANDLER-CELL !  LBL RESTORER-CELL !
   LBL FIND-CELL !  LBL EDGE-CELL !  LBL INDEX-CELL !  LBL SYNC-CELL !
   LBL NUM-CELL !  LBL PCT-CELL !  LBL NAME-CELL !  LBL QUAL-CELL !
   LBL DUMP-CELL !  LBL JSON-CELL !  LBL ROW-CELL !
   HANDLER,
   RESTORER-LBL X64BOOT:RESTORER,
   FIND-HELPER,  EDGE-HELPER,  INDEX-HELPER,  SYNC-HELPER,
   NUM-HELPER,  PCT-HELPER,  NAME-HELPER,  QUAL-HELPER,
   false REPORT-HELPER,  true REPORT-HELPER,  ROW-HELPER, ;

\ The calls into the three helpers, under the contracts above.
: FIND, ( -- ) FIND-LBL CALL, ;
: EDGE, ( -- ) EDGE-LBL CALL, ;
: INDEX, ( -- ) INDEX-LBL CALL, ;

private

\ ---- the bodies' pieces ---------------------------------------------------------
\ Write the text on fd 2 and exit PROF-MAP-RC; the text follows the exit.
: REFUSED, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: msg:label :}
   RDI STDERR IMM32,  RSI msg MOVABS,  RDX u IMM32,  NR-WRITE SYS,
   RDI PROF-MAP-RC IMM32,  NR-EXIT-GROUP SYS,
   msg LBL,  a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN ;

\ rax = n fresh read/write bytes. A refused mapping writes the text on fd 2
\ and exits PROF-MAP-RC: a profiler that ran without its stack or its arena
\ would report numbers nobody could trust (prof.f C-PROF-ARENA-MAP).
: MAP, ( n ptr u8 n -- ) {: len:n a:ptr u:n :}
   LBL {: ok:label :}
   RDI ZERO-REG,  RSI len IMM32,  RDX PROT-RW IMM32,  R10 MAP-ANON-PRIVATE IMM32,
   R8 ALL-ONES,  R9 ZERO-REG,
   NR-MMAP SYS,
   C-AE ok JCC,
   a u REFUSED,
   ok LBL, ;

\ The arena, mapped on the first prof-on or prof-rate of the process and kept
\ in the band. Clobbers rax rcx rdx rsi rdi r8-r11.
: ARENA-MAP, ( -- )
   LBL {: have:label :}
   RAX BAND-VA IMM,  RAX RAX PROF-ARENA MEM-OFF LOAD,  RAX TEST,  C-NE have JCC,
   ARN-BYTES PROFARNMSG$ MAP,
   RCX BAND-VA IMM,  RAX RCX PROF-ARENA MEM-OFF STORE,
   have LBL, ;

\ The handler's alternate stack: mapped on the first prof-on of the process,
\ kept in the band and registered with sigaltstack by every prof-on before the
\ install, so the SA_ONSTACK handler never runs on the interrupted program's
\ stack: the twin of prof.f C-PROF-ALTSTACK. A refused registration would
\ leave the handler there, so it is named and fatal too.
: ALTSTACK, ( -- )
   LBL LBL {: have:label ok:label :}
   RAX BAND-VA IMM,  RAX RAX PROF-STACK MEM-OFF LOAD,  RAX TEST,  C-NE have JCC,
   PROF-STACK-BYTES PROFMMAPMSG$ MAP,
   RCX BAND-VA IMM,  RAX RCX PROF-STACK MEM-OFF STORE,
   have LBL,                                                     \ rax = the stack
   RSP SS-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RAX RSP SS-SP MEM-OFF STORE,
   RCX ZERO-REG,  RCX RSP SS-FLAGS MEM-OFF STORE,
   RCX PROF-STACK-BYTES IMM32,  RCX RSP SS-SIZE MEM-OFF STORE,
   RDI RSP COPY,  RSI ZERO-REG,  NR-SIGALTSTACK SYS,
   RSP RSP SS-BYTES MEM-OFF LEA,                                 \ the frame back, CF kept
   C-AE ok JCC,
   PROFSTKMSG$ REFUSED,
   ok LBL, ;

\ setitimer(ITIMER_REAL) with the interval and the first expiry both the
\ register's microseconds, which disarms the clock when it is 0. The register
\ must not be one the call's arguments take.
: TIMER, ( r64 -- ) {: usec:r64 :}
   RSP ITIMER-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RCX ZERO-REG,
   RCX RSP IT-INTERVAL-SEC MEM-OFF STORE,  usec RSP IT-INTERVAL-USEC MEM-OFF STORE,
   RCX RSP IT-VALUE-SEC MEM-OFF STORE,  usec RSP IT-VALUE-USEC MEM-OFF STORE,
   RDI ITIMER-REAL IMM32,  RSI RSP COPY,  RDX ZERO-REG,  NR-SETITIMER SYS,
   RSP ITIMER-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ rsi = arena: arm the clock at the interval prof-rate left there, or
\ DEFAULT-USEC, which is written back so a report states the rate it sampled
\ at: the twin of prof.f C-PROF-TIMER-FRAME and C-PROF-TIMER.
: TIMER-START, ( -- )
   LBL {: set:label :}
   RAX RSI ARN-USEC MEM-OFF LOAD,  RAX TEST,  C-NE set JCC,
   RAX DEFAULT-USEC IMM32,
   set LBL,  RAX RSI ARN-USEC MEM-OFF STORE,
   RAX TIMER, ;

\ Zero [rcx, rdx) a cell at a time with rax = 0.
: ZERO-SPAN, ( -- )
   LBL LBL {: turn:label done:label :}
   turn LBL,
      RCX RDX CMP-REG,  C-AE done JCC,
      RAX RCX MEM-AT STORE,  RCX CELL ADDI,  turn JMP,
   done LBL, ;

\ Clear PROF-TOT, PROF-OTHER, PROF-FOREIGN and the counter of each record of
\ [0, r14). Clobbers rax rcx rdx.
: BAND-CLEAR, ( -- )
   RAX ZERO-REG,  RCX BAND-VA IMM,
   RAX RCX PROF-TOT MEM-OFF STORE,  RAX RCX PROF-OTHER MEM-OFF STORE,
   RAX RCX PROF-FOREIGN MEM-OFF STORE,
   RCX CNT-VA IMM,  RDX NDICT-REG CELL SCALE,  RDX RCX ASM-SINK ENC-ADD-RR
   ZERO-SPAN, ;

\ rsi = arena: the counts a phase accumulates, the twin of prof.f
\ C-PROF-COUNTERS-CLEAR and C-PROF-CALL-CLEAR, cleared by prof-on and
\ prof-reset and never by the index build. Every edge goes: the table is keyed
\ on record indices, which a rebuilt index gives new meanings. Clobbers rax
\ rcx rdx.
: COUNTS-CLEAR, ( -- )
   RAX ZERO-REG,
   RAX RSI ARN-NEW MEM-OFF STORE,  RAX RSI ARN-DROP MEM-OFF STORE,
   RAX RSI ARN-FRAMES MEM-OFF STORE,  RAX RSI ARN-SPILL MEM-OFF STORE,
   RAX RSI ARN-DEFER MEM-OFF STORE,
   RCX PROF-WALK-CELLS IMM32,  RCX RSI ARN-WALK MEM-OFF STORE,
   RCX RSI ARN-CALL MEM-OFF LEA,  RDX RSI ARN-DEF MEM-OFF LEA,    \ the edges, the inclusive array
   ZERO-SPAN,                                                    \ and the stamps lie back to back
   RCX RSI ARN-IDX ENT-INCL + MEM-OFF LEA,
   RDX RSI ARN-COUNT MEM-OFF LOAD,  RDX RDX PROF-ENT SCALE,  RDX RCX ASM-SINK ENC-ADD-RR
   LBL LBL {: turn:label done:label :}
   turn LBL,
      RCX RDX CMP-REG,  C-AE done JCC,
      RAX RCX MEM-AT STORE,  RCX PROF-ENT ADDI,  turn JMP,
   done LBL, ;

\ A REPORT NEVER SAMPLES ITSELF, for the reason prof.f C-PROF-REPORT-CALL
\ gives: the clock stops and the index syncs before the report reads PROF-TOT,
\ and the clock starts again at the recorded interval after it, unless
\ prof-off had stopped it.
: QUIET, ( -- )
   RAX ZERO-REG,  RAX TIMER,
   SYNC-LBL CALL, ;

: RESUME, ( -- )
   LBL {: stopped:label :}
   RCX BAND-VA IMM,  RAX RCX PROF-ARMED MEM-OFF LOAD,  RAX TEST,  C-E stopped JCC,
   ARENA>RSI,  TIMER-START,
   stopped LBL, ;

public

\ ---- the bodies -----------------------------------------------------------------
\ prof-on ( n -- ): the twin of prof.f BPROF-ON. It runs as Habu code, where
\ r13 and r14 are live: it stores the limit, records the DBASE the handler
\ trusts, clears the band's counts, maps the arena and builds the index, clears
\ the phase's counts and edges, registers the alternate stack, installs the
\ handler for SIGALRM and arms the clock.
: ON-BODY ( -- )
   RAX R64>N G-POP  RCX BAND-VA IMM,  RAX RCX PROF-LIM MEM-OFF STORE,
   DBASE-REG RCX PROF-DBASE MEM-OFF STORE,
   BAND-CLEAR,
   ARENA-MAP,
   ARENA>RSI,  INDEX,
   COUNTS-CLEAR,
   ALTSTACK,
   SIGALRM LINUX-SA-PROF-FLAGS HANDLER-LBL RESTORER-LBL X64BOOT:SIGACTION,
   ARENA>RSI,  TIMER-START,
   RCX BAND-VA IMM,  RAX 1 IMM32,  RAX RCX PROF-ARMED MEM-OFF STORE, ;

\ prof-off ( -- ): stop the clock and nothing else, so the phase can be
\ reported afterwards. A tick already raised is delivered as the setitimer
\ returns, before the next instruction, so the counts are final once this does.
: OFF-BODY ( -- )
   RAX ZERO-REG,  RAX TIMER,
   RCX BAND-VA IMM,  RAX ZERO-REG,  RAX RCX PROF-ARMED MEM-OFF STORE, ;

\ prof-reset ( -- ): clear the counts and keep the index.
: RESET-BODY ( -- )
   LBL {: none:label :}
   BAND-CLEAR,
   ARENA>RSI,  RSI TEST,  C-E none JCC,
   COUNTS-CLEAR,
   none LBL, ;

\ prof-rate ( n -- ): the interval in microseconds the NEXT prof-on arms, not
\ a re-arm: a rate written while no handler is installed would hand the
\ process a SIGALRM it has no handler for.
: RATE-BODY ( -- )
   ARENA-MAP,
   ARENA>RSI,  RAX R64>N G-POP  RAX RSI ARN-USEC MEM-OFF STORE, ;

\ prof-pc>rec ( n -- n ): the record the armed index gives the pc, or -1,
\ through the handler's own FIND.
: PCREC-BODY ( -- )
   LBL LBL {: miss:label out:label :}
   RDI R64>N G-POP
   ARENA>RSI,  RSI TEST,  C-E miss JCC,
   FIND,
   RAX TEST,  C-E miss JCC,
   RAX RAX ENT-IDX MEM-OFF LOAD,  out JMP,
   miss LBL,  RAX ALL-ONES,
   out LBL,  RAX R64>N G-PUSH ;

\ prof-report ( -- ) and prof-json ( -- ): the phase as text or as JSON, the
\ twins of prof.f BPROF-REPORT and BPROF-JSON.
: REPORT-BODY ( -- )  QUIET,  DUMP-LBL CALL,  RESUME, ;

: JSON-BODY ( -- )  QUIET,  JSON-LBL CALL,  RESUME, ;

\ prof-row ( n -- ): the row for record n whatever its rank, the twin of
\ prof.f BPROF-ROW. n waits on the machine stack across the sync.
: ROW-BODY ( -- )
   RAX R64>N G-POP  RAX ASM-SINK ENC-PUSH
   QUIET,
   RDI ASM-SINK ENC-POP  ROW-LBL CALL,
   RESUME, ;

;using
;using
;using
;using
;package
