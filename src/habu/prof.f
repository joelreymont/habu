\ prof.f - the in-binary sampling profiler for the native engine (prof-on,
\ prof-off, prof-reset, prof-report, prof-json, prof-row, prof-rate, prof-pc>rec).
\ Load after habu1.f, before habu2.f. Handler and report state live in the
\ PROF-ABI band at the top of DATA.
\
\ Caller attribution is a conservative walk of the machine stack. Emitted words
\ carry no frame pointer: the prologue is `sub sp,sp,#16` + `str x30,[sp]`, so
\ there is no chain to follow and return addresses are recognised by their
\ value: a cell that lands inside the index's code range is taken for a return
\ address. That is a superset of the true chain: an uninitialised spill slot
\ still holding a code address from a returned call adds a frame. The immediate
\ caller is usually in x30, not on the stack: a leaf primitive entered by BL
\ never stores it, and a word that has not called anything yet still holds its
\ own caller there. When x30 points into the sampled word itself - the word
\ called something that has already returned - the stack scan answers instead,
\ because the word's own prologue saved the real return address. A sample whose
\ SP lies on the main stack, [ARN-STACK-LOW, ARN-STACK-TOP), scans from SP up to
\ the top boot recorded, every Habu frame on that thread: every sample runs on
\ the owner thread, and the bounds are set only when that is the main thread,
\ so SP up to the top is the interrupted thread's live stack. Any other SP
\ scans at most ARN-WALK cells and never leaves SP's 4 KiB block: SP is
\ mapped, so every byte of its block is; one cell further could be the
\ unmapped page above a thread stack, and a SIGSEGV inside a SIGALRM handler
\ is not a diagnosis anybody can use.
\
\ Report rows and a row's callers are selected by repeated maximum, not sorted:
\ each pass takes the largest count below the one before it, or the same count
\ at a later position, so equal counts each print once. No second array is
\ needed, and the counters are left exactly as they were, so a later prof-report
\ says the same thing.
\
\ Inclusive counts live in two places. The handler bumps them in the index
\ entry it just searched, already in a register, since the walk does so once
\ per stack frame. A sync rebuilds the index to name code compiled after
\ prof-on, and a rebuild moves every entry, so C-PROF-INCL-FOLD first empties
\ the entries into the per-record array, the durable home, and historical rows
\ remain readable after their code leaves the searchable index.
\
\ Only the thread that ran prof-on counts. setitimer's SIGALRM is
\ process-directed: the kernel delivers it to any thread that does not block it,
\ and Habu tasks are threads that do not; macOS delivers it to the main thread
\ unless that thread blocks it, so a task that ran prof-on would get no sample.
\ A delivery on another thread sends SIGALRM to the owner, ARN-OWNER-TID, and
\ returns, writing no profiler state, so only the owner's handler writes
\ profiler state from a handler, and every word that leaves ARMED-RUNS waits
\ until that handler is out; the public control words and the engine's trim
\ and reclaim run one at a time, in the order they asked (PROF-CTL-NEXT). A
\ forked child starts with a free profiler: prof-child-reset, which the fork
\ runs in the child, lets go of every handler, ticket and owner the fork
\ copied, and keeps the counters and index for a report. The forwarded
\ signal is the owner's sample, weighed by every interval since ARN-LAST.
\
\ A sample is weighed by the time since the last one. SIGALRM does not queue:
\ every tick that expires while the process waits in a syscall (an fsync, a
\ regular-file read) or is descheduled arrives as one signal, so a count of
\ signals would show a blocked second as about one sample. The handler reads the
\ clock the timer expires on and counts (now - ARN-LAST + ARN-CARRY) / ARN-TICKS
\ wherever it counts a sample, and keeps the remainder in ARN-CARRY, so the
\ weights sum to the whole intervals armed. A weight measured on another clock
\ would floor an expiry that arrives early by that clock's drift to 0. A hold of
\ a running clock adds the armed time since the last sample to ARN-CARRY, which
\ can then exceed ARN-TICKS. Arming sets ARN-LAST, and a resume's first expiry
\ falls at the interval less the carry, so the first sample after it weighs the
\ carry, held or stopped time is never counted, and a program that holds the
\ clock more often than the interval is still sampled. The time lost at prof-off
\ is the carry left, under one interval. A sample that weighs 0 moves ARN-SEQ
\ and ends there: no count, edge, deferred slot or walk.
\
\ A foreign sample (x20 not the owner's DATA, ARN-OWNER-DATA, which on a task
\ is the task's own region, or x26 not the recorded DBASE, or a pc no record
\ owns outside the kernel text and [DBASE, DBASE + REGION), where C that keeps
\ both, libc's pthread_sigmask, runs) is inside C a Habu word called: an FFI
\ word, libSystem's fsync. Its weight goes to the
\ nearest Habu word, the one the interrupted x30 returns into, else the first
\ the scan up from SP finds, bounded as the walk's, as that word's own
\ exclusive sample, and the walk names its callers from there. ARN-FCALLED
\ counts that weight; PROF-FOREIGN counts only the foreign samples for which
\ neither names a Habu word.
require src/core/exit-status.f
require src/habu/prof-abi.f

require src/arch/arm64/icode.f
require src/arch/arm64/asm.f
require src/arch/arm64/mnem.f
require src/habu/crash.f
require src/habu/habu1.f
require src/habu/rt.f
HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/sys.f
[then]

package PROF

public
variable LPROFH   variable LPROFDUMP   variable LPROFFIND   variable LPROFEDGE
variable LPROFJSON   variable LPROFNUM   variable LPROFPCT   variable LPROFNAME
variable LPROFSYNC   variable LPROFROW
private

\ The rejection is a die because this file loads inside the engine-build window,
\ which has no src/lib/ in it.
: PROF-TARGET-OK ( -- )
   HB-TARGET-LINUX-ARM64? if exit then
   HB-TARGET-MACOS? if exit then
   s" hb: this target's signal frame is not modelled" EXIT-STATUS:INTERNAL die ;
PROF-TARGET-OK

DATA-VA VA>N DATA-SIZE PROF-ABI:PROF-BAND-AT constant PROF-BAND-VA   \ absolute: DATA is fixed at DATA-VA
PROF-BAND-VA PROF-STATE-BYTES + constant PROF-CNT-VA
DATA-VA VA>N MAIN-STACK-TOP-CELL + constant PROF-TOP-VA
DATA-VA VA>N MAIN-THREAD-CELL + constant PROF-THREAD-VA
DATA-VA VA>N RBASE-CELL + constant PROF-RBASE-VA          \ main DATA's: a task's region has no text size
DATA-VA VA>N TEXT-SIZE-CELL + constant PROF-TEXT-SIZE-VA
$0043 constant MACOS-SA-PROF-FLAGS        \ SA_ONSTACK | SA_RESTART | SA_SIGINFO

: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX-ARM64? IF 21 2 CRASH:LINUX-UC-MCTX-OFF A64ICODE:ADDI, exit THEN
   21 4 CRASH:MCTX-OFF A64ICODE:LDR, ;

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX-ARM64? IF 9 21 CRASH:LINUX-MCTX-PC-OFF A64ICODE:LDR, exit THEN
   9 21 CRASH:MACOS-MCTX-PC-OFF A64ICODE:LDR, ;

\ rd = the interrupted context's x<n>, read from the signal frame (x21 = mcontext).
: C-PROF-CTX-X>R ( n n -- ) {: x:n rd:n :}
   HB-TARGET-LINUX-ARM64? IF rd 21 CRASH:LINUX-MCTX-X0-OFF x 8 * + A64ICODE:LDR, exit THEN
   rd 21 CRASH:SS-OFF x 8 * + A64ICODE:LDR, ;

public

\ LPROFFIND ( x9 = pc, x8 = arena -- x6 = index entry or 0 ): the whole of pc
\ attribution, shared by the signal handler and by `prof-pc>rec`. Clobbers
\ x10-x13 and obeys the primitive ABI (x19-x28 untouched), because prof-pc>rec
\ calls it from compiled Habu code.
: EMIT-PROFFIND ( -- )
   LPROFFIND A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bl bhi bdone miss out :}
   10 8 PROF-ABI:ARN-COUNT A64ICODE:LDR,  10 miss A64ICODE:CBZ,
   11 8 PROF-ABI:ARN-LO A64ICODE:LDR,  9 11 A64ICODE:CMP,  A64ASM:C-CC miss A64ICODE:BCOND,
   11 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 11 A64ICODE:CMP,  A64ASM:C-CS miss A64ICODE:BCOND,
   11 0 A64ICODE:MOVZ,                                         \ lo
   bl A64ICODE:LBL,                                            \ rightmost entry with start <= pc
      11 10 A64ICODE:CMP,  A64ASM:C-CS bdone A64ICODE:BCOND,
      12 11 10 A64ICODE:ADD,  12 12 1 A64ICODE:LSRI,                    \ mid = (lo + hi) / 2
      13 12 5 A64ICODE:LSLI,  13 8 13 A64ICODE:ADD,  13 13 PROF-ABI:ARN-IDX A64ICODE:ADDI,
      13 13 PROF-ABI:ENT-START A64ICODE:LDR,
      13 9 A64ICODE:CMP,  A64ASM:C-HI bhi A64ICODE:BCOND,                      \ start > pc: mid is the new hi
      11 12 1 A64ICODE:ADDI,  bl A64ICODE:B,
   bhi A64ICODE:LBL,  10 12 0 A64ICODE:ADDI,  bl A64ICODE:B,
   bdone A64ICODE:LBL,
   11 miss A64ICODE:CBZ,                                       \ every start is above the pc
   11 11 1 A64ICODE:SUBI,  6 11 5 A64ICODE:LSLI,  6 8 6 A64ICODE:ADD,  6 6 PROF-ABI:ARN-IDX A64ICODE:ADDI,
   13 6 PROF-ABI:ENT-END A64ICODE:LDR,  9 13 A64ICODE:CMP,  A64ASM:C-CC out A64ICODE:BCOND,      \ the pc sits in a gap above it
   miss A64ICODE:LBL,  6 0 A64ICODE:MOVZ,
   out A64ICODE:LBL,  A64ICODE:RET, ;

\ LPROFEDGE ( x8 = arena, x22 = sampled record, x10 = caller record, x3 = the
\ sample's weight ): add the weight to one caller edge. Clobbers x11-x16. A
\ sample whose own record is unknown (x22 < 0) has no edge to key, and a full
\ probe window counts the weight as dropped rather than overwriting somebody
\ else's pair.
: EMIT-PROFEDGE ( -- )
   LPROFEDGE A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: eprobe efree ehit edone :}
   22 0 A64ICODE:CMPI,  A64ASM:C-LT edone A64ICODE:BCOND,
   11 22 PROF-ABI:PROF-REC-BITS A64ICODE:LSLI,  11 11 10 A64ICODE:ORR,  11 11 1 A64ICODE:ADDI,   \ key, never 0
   12 PROF-ABI:PROF-HASH A64ICODE:LIT64,  12 11 12 A64ICODE:MUL,  12 12 48 A64ICODE:LSRI,
   14 PROF-ABI:ARN-CALL A64ICODE:LIT64,  14 8 14 A64ICODE:ADD,
   13 PROF-ABI:PROF-CALL-PROBE A64ICODE:MOVZ,
   eprobe A64ICODE:LBL,
      15 12 4 A64ICODE:LSLI,  15 14 15 A64ICODE:ADD,
      16 15 0 A64ICODE:LDR,
      16 efree A64ICODE:CBZ,
      16 11 A64ICODE:CMP,  A64ASM:C-EQ ehit A64ICODE:BCOND,
      12 12 1 A64ICODE:ADDI,  12 12 PROF-ABI:PROF-CALL-MASK A64ICODE:ANDI,
      13 13 1 A64ICODE:SUBI,  13 eprobe A64ICODE:CBNZ,
   16 8 PROF-ABI:ARN-DROP A64ICODE:LDR,  16 16 3 A64ICODE:ADD,  16 8 PROF-ABI:ARN-DROP A64ICODE:STR,  edone A64ICODE:B,
   efree A64ICODE:LBL,  11 15 0 A64ICODE:STR,  3 15 8 A64ICODE:STR,  edone A64ICODE:B,
   ehit A64ICODE:LBL,  16 15 8 A64ICODE:LDR,  16 16 3 A64ICODE:ADD,  16 15 8 A64ICODE:STR,
   edone A64ICODE:LBL,  A64ICODE:RET, ;

\ x23 = SP off the main stack: x24 = the scan's end, ARN-WALK cells above SP
\ or the end of SP's 4 KiB block, whichever is lower. Clobbers x10.
: C-PROF-BLOCK-END ( -- )
   10 8 PROF-ABI:ARN-WALK A64ICODE:LDR,  10 10 3 A64ICODE:LSLI,  24 23 10 A64ICODE:ADD,
   10 23 PROF-ABI:PROF-PAGE-MASK A64ICODE:ORRI,  10 10 1 A64ICODE:ADDI,
   24 10 A64ICODE:CMP,  24 24 10 A64ASM:C-LS A64ICODE:CSEL, ;

\ x8 = arena, x6 = an index entry, x10 = its record index, x3 = the sample's
\ weight: one inclusive sample for that record, and only the first time THIS
\ sample reaches it. The conservative walk can see one word twice - a recursive call, or a
\ stale spill slot still holding a return address into it - and an inclusive
\ count that ran past the sample total would say a word took longer than the
\ program did. The stamp is ARN-SEQ, which a weight above one does not move.
\ Clobbers x11-x13.
: C-PROF-INCL-ONCE ( -- )
   A64ICODE:LBL {: seen :}
   11 PROF-ABI:ARN-STAMP A64ICODE:LIT64,  11 8 11 A64ICODE:ADD,  12 10 3 A64ICODE:LSLI,  11 11 12 A64ICODE:ADD,
   12 11 0 A64ICODE:LDR,
   13 8 PROF-ABI:ARN-SEQ A64ICODE:LDR,
   12 13 A64ICODE:CMP,  A64ASM:C-EQ seen A64ICODE:BCOND,
   13 11 0 A64ICODE:STR,
   12 6 PROF-ABI:ENT-INCL A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 6 PROF-ABI:ENT-INCL A64ICODE:STR,
   seen A64ICODE:LBL, ;

\ x8 = arena, x9 = the interrupted pc, x21 = mcontext, x3 = the weight: keep one
\ sample whose code the index cannot name yet. Only the pc, the interrupted x30
\ and the weight are kept, at a fixed stride, so the handler still walks nothing here and allocates nothing; the
\ next report rebuilds the index and replays them. Clobbers x10-x14.
: C-PROF-DEFER ( -- )
   A64ICODE:LBL A64ICODE:LBL {: full done :}
   10 8 PROF-ABI:ARN-DEFER A64ICODE:LDR,
   11 PROF-ABI:PROF-DEFER-SLOTS A64ICODE:LIT64,  10 11 A64ICODE:CMP,  A64ASM:C-CS full A64ICODE:BCOND,
   12 PROF-ABI:ARN-DEF A64ICODE:LIT64,  12 8 12 A64ICODE:ADD,
   13 PROF-ABI:PROF-DEFER-ENT A64ICODE:MOVZ,  13 10 13 A64ICODE:MUL,  12 12 13 A64ICODE:ADD,
   9 12 0 A64ICODE:STR,
   30 14 C-PROF-CTX-X>R  14 12 8 A64ICODE:STR,  3 12 PROF-ABI:PROF-SLOT-WEIGHT A64ICODE:STR,
   10 10 1 A64ICODE:ADDI,  10 8 PROF-ABI:ARN-DEFER A64ICODE:STR,
   done A64ICODE:B,
   full A64ICODE:LBL,  10 8 PROF-ABI:ARN-SPILL A64ICODE:LDR,  10 10 3 A64ICODE:ADD,  10 8 PROF-ABI:ARN-SPILL A64ICODE:STR,
   done A64ICODE:LBL, ;

: C-PROF-WALK ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: wl wlr wdone wret wblock :}
   25 22 0 A64ICODE:ADDI,                                     \ dedup seed: the sampled word
   19 0 A64ICODE:MOVZ,                                        \ no caller edge recorded yet
   30 9 C-PROF-CTX-X>R
   9 9 4 A64ICODE:SUBI,                                       \ the call site, not the return address
   10 8 PROF-ABI:ARN-LO A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CC wlr A64ICODE:BCOND,
   10 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CS wlr A64ICODE:BCOND,
   LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
   6 wlr A64ICODE:CBZ,
   10 6 PROF-ABI:ENT-IDX A64ICODE:LDR,  10 25 A64ICODE:CMP,  A64ASM:C-EQ wlr A64ICODE:BCOND,    \ x30 still points inside the sample
   C-PROF-INCL-ONCE
   25 10 0 A64ICODE:ADDI,  19 1 A64ICODE:MOVZ,
   11 8 PROF-ABI:ARN-FRAMES A64ICODE:LDR,  11 11 1 A64ICODE:ADDI,  11 8 PROF-ABI:ARN-FRAMES A64ICODE:STR,
   LPROFEDGE A64ICODE:LABEL@ A64ICODE:BL,
   wlr A64ICODE:LBL,
   31 23 C-PROF-CTX-X>R                              \ the interrupted machine stack
   10 8 PROF-ABI:ARN-STACK-LOW A64ICODE:LDR,  23 10 A64ICODE:CMP,  A64ASM:C-CC wblock A64ICODE:BCOND,
   24 8 PROF-ABI:ARN-STACK-TOP A64ICODE:LDR,  23 24 A64ICODE:CMP,  A64ASM:C-CC wl A64ICODE:BCOND,
   wblock A64ICODE:LBL,  C-PROF-BLOCK-END                     \ not the main stack
   wl A64ICODE:LBL,
      23 24 A64ICODE:CMP,  A64ASM:C-CS wdone A64ICODE:BCOND,
      9 23 0 A64ICODE:LDR,  23 23 8 A64ICODE:ADDI,
      10 8 PROF-ABI:ARN-LO A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CC wl A64ICODE:BCOND,
      10 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CS wl A64ICODE:BCOND,
      9 9 4 A64ICODE:SUBI,
      LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
      6 wl A64ICODE:CBZ,
      10 6 PROF-ABI:ENT-IDX A64ICODE:LDR,  10 25 A64ICODE:CMP,  A64ASM:C-EQ wl A64ICODE:BCOND,  \ the frame below named the same word
      C-PROF-INCL-ONCE
      25 10 0 A64ICODE:ADDI,
      11 8 PROF-ABI:ARN-FRAMES A64ICODE:LDR,  11 11 1 A64ICODE:ADDI,  11 8 PROF-ABI:ARN-FRAMES A64ICODE:STR,
      19 wl A64ICODE:CBNZ,                                    \ the immediate caller is already in
      19 1 A64ICODE:MOVZ,
      LPROFEDGE A64ICODE:LABEL@ A64ICODE:BL,
      wl A64ICODE:B,
   wdone A64ICODE:LBL,
   19 wret A64ICODE:CBNZ,                                     \ nothing named a caller: say so
   10 PROF-ABI:PROF-CALLER-NONE A64ICODE:LIT64,  LPROFEDGE A64ICODE:LABEL@ A64ICODE:BL,
   wret A64ICODE:LBL, ;

: C-PROF-SAY ( str -- ) {: s:str :}
   A64ICODE:LBL A64ICODE:LBL {: skip txt :}
   skip A64ICODE:B,
   txt A64ICODE:LBL,  s A64ICODE:BYTES,
   skip A64ICODE:LBL,
   0 1 A64ICODE:MOVZ,  1 txt A64ICODE:ADR,  2 s STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS, ;

: C-PROF-BYTE ( n -- ) {: c:n :}
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  13 c A64ICODE:MOVZ,  13 A64ICODE:SP 0 A64ICODE:STRB,
   0 1 A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 1 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

: C-PROF-NL ( -- )  s\" \n" C-PROF-SAY ;
: C-PROF-DQ ( -- )  s\" \q" C-PROF-SAY ;

: C-PROF-FIELD ( str n -- ) {: s:str reg:n :}
   s C-PROF-SAY
   9 reg 0 A64ICODE:ADDI,  10 1 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL, ;

\ LPROFNUM ( x9 = value, x10 = width ): unsigned decimal, right-aligned in x10
\ columns and never truncated, no newline. Clobbers x0-x2, x9-x15.
: EMIT-PROFNUM ( -- )
   LPROFNUM A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: dl pl pd :}
   15 10 0 A64ICODE:ADDI,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,
   12 A64ICODE:SP 48 A64ICODE:ADDI,
   11 10 A64ICODE:MOVZ,
   dl A64ICODE:LBL,
      14 9 11 A64ICODE:UDIV,  13 14 11 A64ICODE:MUL,  13 9 13 A64ICODE:SUB,
      13 13 $30 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
      9 14 0 A64ICODE:ADDI,  9 dl A64ICODE:CBNZ,
   10 A64ICODE:SP 48 A64ICODE:ADDI,  10 10 12 A64ICODE:SUB,
   15 15 10 A64ICODE:SUB,
   pl A64ICODE:LBL,
      15 0 A64ICODE:CMPI,  A64ASM:C-LE pd A64ICODE:BCOND,
      13 $20 A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
      15 15 1 A64ICODE:SUBI,  pl A64ICODE:B,
   pd A64ICODE:LBL,
   0 1 A64ICODE:MOVZ,  1 12 0 A64ICODE:ADDI,  2 A64ICODE:SP 48 A64ICODE:ADDI,  2 2 12 A64ICODE:SUB,  SYSCALL:NR-WRITE SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI,  A64ICODE:RET, ;

\ LPROFPCT ( x9 = count, x10 = total ): the share as "100.0", one decimal, right
\ aligned in six columns. A zero total prints 0.0 rather than dividing by it.
: EMIT-PROFPCT ( -- )
   LPROFPCT A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL {: zero go :}
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   10 zero A64ICODE:CBZ,
   11 1000 A64ICODE:MOVZ,  9 9 11 A64ICODE:MUL,  9 9 10 A64ICODE:UDIV,
   go A64ICODE:B,
   zero A64ICODE:LBL,  9 0 A64ICODE:MOVZ,
   go A64ICODE:LBL,
   12 10 A64ICODE:MOVZ,  13 9 12 A64ICODE:UDIV,  14 13 12 A64ICODE:MUL,  14 9 14 A64ICODE:SUB,
   14 A64ICODE:SP 8 A64ICODE:STR,
   9 13 0 A64ICODE:ADDI,  10 4 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
   $2E C-PROF-BYTE
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 1 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,  A64ICODE:RET, ;

\ LPROFNAME ( x5 = profile row ): the complete copied qualified name.
: EMIT-PROFNAME ( -- )
   LPROFNAME A64ICODE:LABEL@ A64ICODE:LBL,
   0 1 A64ICODE:MOVZ,  1 5 PROF-ABI:RID-NAME A64ICODE:LDR,  2 5 PROF-ABI:RID-LEN A64ICODE:LDR,  SYSCALL:NR-WRITE SYSCALL:SYS,
   A64ICODE:RET, ;

\ LPROFDUMP / LPROFJSON ( -- ): the same walk, printed two ways.
\
\ REGISTERS. LPROFNUM and LPROFPCT clobber x9-x15, so every value that has to
\ live across a printed column sits in x3-x7, x17 or x19-x25: x19 arena, x7
\ band until the header has printed, x21 the row's index entry, x22 samples,
\ x23/x24 the (count, entry) threshold the next row must fall under, x25 the
\ row rank, x7 the row's exclusive count, x17 its record index, x6/x4 the
\ (count, slot) threshold the next caller must fall under, x3 the caller rank.
\ x20 is never written: a fault here must find the caller's DATA in it. x17,
\ x19 and x21-x25 are saved and restored because the compiled callers own them.
: C-PROF-REP-OPEN ( -- )
   A64ICODE:SP A64ICODE:SP 96 A64ICODE:SUBI,
   30 A64ICODE:SP 0 A64ICODE:STR,  19 A64ICODE:SP 8 A64ICODE:STR,  21 A64ICODE:SP 24 A64ICODE:STR,  22 A64ICODE:SP 32 A64ICODE:STR,
   23 A64ICODE:SP 40 A64ICODE:STR,  24 A64ICODE:SP 48 A64ICODE:STR,  25 A64ICODE:SP 56 A64ICODE:STR,  17 A64ICODE:SP 64 A64ICODE:STR, ;

: C-PROF-REP-CLOSE ( -- )
   30 A64ICODE:SP 0 A64ICODE:LDR,  19 A64ICODE:SP 8 A64ICODE:LDR,  21 A64ICODE:SP 24 A64ICODE:LDR,  22 A64ICODE:SP 32 A64ICODE:LDR,
   23 A64ICODE:SP 40 A64ICODE:LDR,  24 A64ICODE:SP 48 A64ICODE:LDR,  25 A64ICODE:SP 56 A64ICODE:LDR,  17 A64ICODE:SP 64 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP 96 A64ICODE:ADDI,  A64ICODE:RET, ;

\ x0 = arena, and the clock stopped. Empty every entry's inclusive count into the
\ per-record array, so the rebuild that follows can move the entries freely.
: C-PROF-INCL-FOLD ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: fl fdone fnext :}
   8 0 0 A64ICODE:ADDI,
   12 PROF-ABI:ARN-IDX A64ICODE:LIT64,  12 8 12 A64ICODE:ADD,
   13 8 PROF-ABI:ARN-COUNT A64ICODE:LDR,  13 13 5 A64ICODE:LSLI,  13 12 13 A64ICODE:ADD,
   fl A64ICODE:LBL,
      12 13 A64ICODE:CMP,  A64ASM:C-CS fdone A64ICODE:BCOND,
      14 12 PROF-ABI:ENT-INCL A64ICODE:LDR,  14 fnext A64ICODE:CBZ,
      10 12 PROF-ABI:ENT-IDX A64ICODE:LDR,
      11 PROF-ABI:ARN-INCL A64ICODE:LIT64,  11 8 11 A64ICODE:ADD,  15 10 3 A64ICODE:LSLI,  11 11 15 A64ICODE:ADD,
      15 11 0 A64ICODE:LDR,  15 15 14 A64ICODE:ADD,  15 11 0 A64ICODE:STR,
      14 0 A64ICODE:MOVZ,  14 12 PROF-ABI:ENT-INCL A64ICODE:STR,
      fnext A64ICODE:LBL,  12 12 PROF-ABI:PROF-ENT A64ICODE:ADDI,  fl A64ICODE:B,
   fdone A64ICODE:LBL, ;

\ x11 = the count for profile ID x12. Clobbers x10.
: C-PROF-RANK ( bool -- ) {: incl:bool :}
   incl IF 11 PROF-ABI:ARN-INCL A64ICODE:LIT64,  11 19 11 A64ICODE:ADD,
   ELSE 11 PROF-CNT-VA A64ICODE:LIT64, THEN
   10 12 3 A64ICODE:LSLI,  11 11 10 A64ICODE:ADD,  11 11 0 A64ICODE:LDR, ;

\ The report walks every stable profile ID, including reclaimed code. Folding
\ first gives each identity one inclusive count independent of index lifetime.
: C-PROF-REP-WORDSUM ( -- )
   A64ICODE:LBL A64ICODE:LBL {: turn done :}
   4 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  12 0 A64ICODE:MOVZ,
   13 19 PROF-ABI:ARN-IDS A64ICODE:LDR,
   turn A64ICODE:LBL,
      12 13 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      false C-PROF-RANK
      4 4 11 A64ICODE:ADD,  14 11 0 A64ICODE:ADDI,
      true C-PROF-RANK
      14 14 11 A64ICODE:ORR,
      A64ICODE:LBL {: next :}
      14 next A64ICODE:CBZ,  3 3 1 A64ICODE:ADDI,
      next A64ICODE:LBL,  12 12 1 A64ICODE:ADDI,  turn A64ICODE:B,
   done A64ICODE:LBL, ;

\ The header line names every field: samples (sampling intervals elapsed, the
\ unit of every count below but frames), words (ticks in a dictionary word, the sum of
\ the exclusive column), other (Habu code in no word:
\ the main loop, engine helpers, gaps between spans), new (deferred ticks whose pc
\ no live record owns), defer (ticks in code compiled after prof-on, still
\ unnamed), spill (deferred ticks the buffer had no room for), foreign (ticks
\ without the engine's XREG-RBASE and dictionary registers, or with both at a pc
\ outside the kernel text and the code region, charged to no word), foreign
\ charged to callers
\ (foreign ticks a calling word took, already in words), frames (caller frames
\ attributed), dropped (caller edges a full probe window could not key), indexed
\ (index entries), usec (the interval sampled at) and attributed (words that took
\ a sample of either kind; the text report prints PROF-ROWS of them per section,
\ prof-json all). A text row is exclusive, its percent of samples, inclusive, its
\ percent, then the package-qualified word; a caller line `<- count percent
\ caller` is a share of the row's exclusive count in the flat section and of its
\ inclusive count under `by inclusive`. `(unknown)` is the share whose caller the
\ walk could not establish. A word whose last call is a tail call has released
\ its frame before that callee runs, so the samples its tail callee takes do not
\ name it.
\ words + other + new + defer + spill + foreign == samples is the accounting this
\ line states. After a sync, defer is 0 and new holds only the deferred samples
\ whose pc belongs to no live record; an auto-report at the limit does not sync,
\ so its deferred samples are still sitting in defer.
\ x19 = arena: x10 = the weight of the deferred samples. Clobbers x11-x13.
: C-PROF-DEFER-WEIGHT ( -- )
   A64ICODE:LBL A64ICODE:LBL {: dl dd :}
   10 0 A64ICODE:MOVZ,  11 19 PROF-ABI:ARN-DEFER A64ICODE:LDR,
   12 PROF-ABI:ARN-DEF A64ICODE:LIT64,  12 19 12 A64ICODE:ADD,
   dl A64ICODE:LBL,  11 dd A64ICODE:CBZ,
      13 12 PROF-ABI:PROF-SLOT-WEIGHT A64ICODE:LDR,  10 10 13 A64ICODE:ADD,
      12 12 PROF-ABI:PROF-DEFER-ENT A64ICODE:ADDI,  11 11 1 A64ICODE:SUBI,  dl A64ICODE:B,
   dd A64ICODE:LBL, ;

: C-PROF-REP-HEAD ( bool -- ) {: json:bool :}
   json IF s\" {\"samples\":" ELSE s" profiler samples " THEN 22 C-PROF-FIELD
   json IF s\" ,\"words\":" ELSE s"  words " THEN 4 C-PROF-FIELD
   10 7 PROF-ABI:PROF-OTHER A64ICODE:LDR,
   json IF s\" ,\"other\":" ELSE s"  other " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-NEW A64ICODE:LDR,
   json IF s\" ,\"new\":" ELSE s"  new " THEN 10 C-PROF-FIELD
   C-PROF-DEFER-WEIGHT
   json IF s\" ,\"defer\":" ELSE s"  defer " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-SPILL A64ICODE:LDR,
   json IF s\" ,\"spill\":" ELSE s"  spill " THEN 10 C-PROF-FIELD
   10 7 PROF-ABI:PROF-FOREIGN A64ICODE:LDR,
   json IF s\" ,\"foreign\":" ELSE s"  foreign " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-FCALLED A64ICODE:LDR,
   json IF s\" ,\"foreign charged to callers\":"
   ELSE s"  foreign charged to callers " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-FRAMES A64ICODE:LDR,
   json IF s\" ,\"frames\":" ELSE s"  frames " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-DROP A64ICODE:LDR,
   json IF s\" ,\"dropped\":" ELSE s"  dropped " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-COUNT A64ICODE:LDR,
   json IF s\" ,\"indexed\":" ELSE s"  indexed " THEN 10 C-PROF-FIELD
   10 19 PROF-ABI:ARN-USEC A64ICODE:LDR,
   json IF s\" ,\"usec\":" ELSE s"  usec " THEN 10 C-PROF-FIELD
   json IF s\" ,\"attributed\":" ELSE s"  attributed " THEN 3 C-PROF-FIELD
   json IF exit THEN
   C-PROF-NL ;

\ x21 is the stable profile ID selected for this row.
: C-PROF-ROW-INCL ( -- )
   10 PROF-ABI:ARN-INCL A64ICODE:LIT64,  10 19 10 A64ICODE:ADD,
   9 21 3 A64ICODE:LSLI,  9 10 9 A64ICODE:ADD,  9 9 0 A64ICODE:LDR, ;

: C-PROF-REP-ROW-REC ( -- )
   17 21 0 A64ICODE:ADDI,
   5 PROF-ABI:ARN-RECS A64ICODE:LIT64,  5 19 5 A64ICODE:ADD,
   10 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  10 17 10 A64ICODE:MUL,  5 5 10 A64ICODE:ADD, ;

: C-PROF-REP-ROW ( -- )
   9 7 0 A64ICODE:ADDI,  10 8 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
   9 7 0 A64ICODE:ADDI,  10 22 0 A64ICODE:ADDI,  LPROFPCT A64ICODE:LABEL@ A64ICODE:BL,
   C-PROF-ROW-INCL  10 8 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
   C-PROF-ROW-INCL  10 22 0 A64ICODE:ADDI,  LPROFPCT A64ICODE:LABEL@ A64ICODE:BL,
   s"   " C-PROF-SAY
   C-PROF-REP-ROW-REC
   LPROFNAME A64ICODE:LABEL@ A64ICODE:BL,
   C-PROF-NL ;

\ x5 = the record behind a caller index in x9, or 0 for PROF-CALLER-NONE, which
\ stands for a sample the walk could not attribute and owns no dictionary row.
: C-PROF-CALLER-REC ( -- )
   A64ICODE:LBL A64ICODE:LBL {: none done :}
   11 PROF-ABI:PROF-CALLER-NONE A64ICODE:LIT64,  9 11 A64ICODE:CMP,  A64ASM:C-EQ none A64ICODE:BCOND,
   5 PROF-ABI:ARN-RECS A64ICODE:LIT64,  5 19 5 A64ICODE:ADD,
   11 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  11 9 11 A64ICODE:MUL,  5 5 11 A64ICODE:ADD,
   done A64ICODE:B,
   none A64ICODE:LBL,  5 0 A64ICODE:MOVZ,
   done A64ICODE:LBL, ;

: C-PROF-CALLER-NAME ( -- )
   A64ICODE:LBL A64ICODE:LBL {: none done :}
   5 none A64ICODE:CBZ,
   LPROFNAME A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:B,
   none A64ICODE:LBL,  s" (unknown)" C-PROF-SAY
   done A64ICODE:LBL, ;

\ One caller line: x9 = the caller's record index, x10 = the edge count.
\
\ THE SHARE IS OF THE ROW'S OWN DENOMINATOR, AT SP 88. A caller edge is only
\ recorded for a sample whose pc was inside the row's word, so in the flat
\ section the denominator is that exclusive count and the shares add to it. In
\ the inclusive section a phase word may have almost no exclusive samples, and
\ a share of those would read as "100% of this row" for a single edge; there
\ the denominator is the inclusive count, so the line says how much of the row
\ the edges actually cover.
: C-PROF-REP-CALLER ( -- )
   C-PROF-CALLER-REC
   10 A64ICODE:SP 72 A64ICODE:STR,
   s"          <- " C-PROF-SAY
   9 A64ICODE:SP 72 A64ICODE:LDR,  10 8 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
   9 A64ICODE:SP 72 A64ICODE:LDR,  10 A64ICODE:SP 88 A64ICODE:LDR,  LPROFPCT A64ICODE:LABEL@ A64ICODE:BL,
   s"  " C-PROF-SAY
   C-PROF-CALLER-NAME
   C-PROF-NL ;

: C-PROF-REP-CALLERS ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: cr cl cnext cdone ccmp crd :}
   3 0 A64ICODE:MOVZ,
   6 0 A64ICODE:MOVN,  4 0 A64ICODE:MOVN,                            \ threshold: nothing printed yet
   cr A64ICODE:LBL,
      3 PROF-ABI:PROF-CALLERS A64ICODE:CMPI,  A64ASM:C-CS crd A64ICODE:BCOND,
      15 0 A64ICODE:MOVZ,  16 0 A64ICODE:MOVZ,
      11 PROF-ABI:ARN-CALL A64ICODE:LIT64,  11 19 11 A64ICODE:ADD,
      12 PROF-ABI:PROF-CALL-SLOTS A64ICODE:LIT64,
      cl A64ICODE:LBL,
         12 cdone A64ICODE:CBZ,
         13 11 0 A64ICODE:LDR,  13 cnext A64ICODE:CBZ,
         13 13 1 A64ICODE:SUBI,  10 13 PROF-ABI:PROF-REC-BITS A64ICODE:LSRI,  10 17 A64ICODE:CMP,  A64ASM:C-NE cnext A64ICODE:BCOND,
         13 13 PROF-ABI:PROF-REC-MASK A64ICODE:ANDI,
         9 11 8 A64ICODE:LDR,
         9 6 A64ICODE:CMP,  A64ASM:C-HI cnext A64ICODE:BCOND,               \ above the threshold: printed
         A64ASM:C-NE ccmp A64ICODE:BCOND,
         11 4 A64ICODE:CMP,  A64ASM:C-LS cnext A64ICODE:BCOND,              \ the threshold slot itself, or before it
         ccmp A64ICODE:LBL,
         9 15 A64ICODE:CMP,  A64ASM:C-LS cnext A64ICODE:BCOND,              \ same count, later slot: keep the first
         15 9 0 A64ICODE:ADDI,  16 13 0 A64ICODE:ADDI,  14 11 0 A64ICODE:ADDI,
         cnext A64ICODE:LBL,  11 11 PROF-ABI:PROF-CALL-ENT A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  cl A64ICODE:B,
      cdone A64ICODE:LBL,
      15 crd A64ICODE:CBZ,                                  \ no caller edge left
      6 15 0 A64ICODE:ADDI,  4 14 0 A64ICODE:ADDI,
      9 16 0 A64ICODE:ADDI,  10 15 0 A64ICODE:ADDI,
      C-PROF-REP-CALLER
      3 3 1 A64ICODE:ADDI,  cr A64ICODE:B,
   crd A64ICODE:LBL, ;

: C-PROF-REP-SECTION ( bool -- ) {: incl:bool :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: rowl sell selcmp selnx seld seltake rowsd :}
   23 0 A64ICODE:MOVN,  24 0 A64ICODE:MOVN,  25 0 A64ICODE:MOVZ,
   rowl A64ICODE:LBL,
      7 0 A64ICODE:MOVZ,  21 0 A64ICODE:MOVN,
      12 0 A64ICODE:MOVZ,  13 19 PROF-ABI:ARN-IDS A64ICODE:LDR,
      sell A64ICODE:LBL,
         12 13 A64ICODE:CMP,  A64ASM:C-CS seld A64ICODE:BCOND,
         incl C-PROF-RANK
         11 selnx A64ICODE:CBZ,
         11 23 A64ICODE:CMP,  A64ASM:C-HI selnx A64ICODE:BCOND,             \ above the threshold: already printed
         A64ASM:C-NE selcmp A64ICODE:BCOND,
         12 24 A64ICODE:CMP,  A64ASM:C-LS selnx A64ICODE:BCOND,             \ the threshold row itself, or before it
         selcmp A64ICODE:LBL,
         11 7 A64ICODE:CMP,  A64ASM:C-HI seltake A64ICODE:BCOND,
         A64ASM:C-NE selnx A64ICODE:BCOND,
         12 21 A64ICODE:CMP,  A64ASM:C-CS selnx A64ICODE:BCOND,             \ same count, later entry: keep the first
         seltake A64ICODE:LBL,
         7 11 0 A64ICODE:ADDI,  21 12 0 A64ICODE:ADDI,
         selnx A64ICODE:LBL,  12 12 1 A64ICODE:ADDI,  sell A64ICODE:B,
      seld A64ICODE:LBL,
      7 rowsd A64ICODE:CBZ,                                 \ no counted row left
      23 7 0 A64ICODE:ADDI,  24 21 0 A64ICODE:ADDI,
      17 21 0 A64ICODE:ADDI,
      7 PROF-CNT-VA A64ICODE:LIT64,  10 17 3 A64ICODE:LSLI,  7 7 10 A64ICODE:ADD,  7 7 0 A64ICODE:LDR,  \ the row prints exclusive
      incl IF C-PROF-ROW-INCL ELSE 9 7 0 A64ICODE:ADDI, THEN  9 A64ICODE:SP 88 A64ICODE:STR,
      C-PROF-REP-ROW
      C-PROF-REP-CALLERS
      25 25 1 A64ICODE:ADDI,
      25 PROF-ABI:PROF-ROWS A64ICODE:CMPI,  A64ASM:C-CC rowl A64ICODE:BCOND,
   rowsd A64ICODE:LBL, ;

\ EVERY ATTRIBUTED WORD, not a top-N. A phase word - a compiler pass, a verifier
\ entry - is often invisible in an exclusive ranking and is exactly what a
\ per-phase table needs, so the machine-readable report carries the whole set:
\ one object per profile ID that took a sample, and the caller edges as a flat
\ array rather than nested under each row. Nesting them would mean one pass over
\ the edge table per row - quadratic, and this walk runs over every row - while
\ one pass emits every edge there is. Both are bounded by the arena, at most
\ ARN-IDS rows and PROF-CALL-SLOTS edges, so nothing is truncated.
: C-PROF-JSON-ROWS ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: jl jdone jnext jcomma :}
   s\" ,\"rows\":[" C-PROF-SAY
   25 0 A64ICODE:MOVZ,
   21 0 A64ICODE:MOVZ,  23 19 PROF-ABI:ARN-IDS A64ICODE:LDR,
   jl A64ICODE:LBL,
      21 23 A64ICODE:CMP,  A64ASM:C-CS jdone A64ICODE:BCOND,
      17 21 0 A64ICODE:ADDI,
      7 PROF-CNT-VA A64ICODE:LIT64,  10 17 3 A64ICODE:LSLI,  7 7 10 A64ICODE:ADD,  7 7 0 A64ICODE:LDR,
      C-PROF-ROW-INCL
      10 7 9 A64ICODE:ORR,  10 jnext A64ICODE:CBZ,                   \ neither count moved: not a row
      25 jcomma A64ICODE:CBZ,  s" ," C-PROF-SAY
      jcomma A64ICODE:LBL,  25 1 A64ICODE:MOVZ,
      s\" {\"word\":\"" C-PROF-SAY
      C-PROF-REP-ROW-REC
      LPROFNAME A64ICODE:LABEL@ A64ICODE:BL,
      C-PROF-DQ  s\" ,\"excl\":" C-PROF-SAY
      9 7 0 A64ICODE:ADDI,  10 1 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
      s\" ,\"incl\":" C-PROF-SAY
      C-PROF-ROW-INCL  10 1 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
      s" }" C-PROF-SAY
   jnext A64ICODE:LBL,  21 21 1 A64ICODE:ADDI,  jl A64ICODE:B,
   jdone A64ICODE:LBL,
   s" ]" C-PROF-SAY ;

\ x9 = a record index, or PROF-CALLER-NONE: its name as a JSON string body.
: C-PROF-JSON-NAME ( -- )
   C-PROF-CALLER-REC
   C-PROF-CALLER-NAME ;

: C-PROF-JSON-EDGES ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: el edone enext ecomma :}
   s\" ,\"edges\":[" C-PROF-SAY
   25 0 A64ICODE:MOVZ,
   21 PROF-ABI:ARN-CALL A64ICODE:LIT64,  21 19 21 A64ICODE:ADD,
   23 PROF-ABI:PROF-CALL-SLOTS A64ICODE:LIT64,
   el A64ICODE:LBL,
      23 edone A64ICODE:CBZ,
      17 21 0 A64ICODE:LDR,  17 enext A64ICODE:CBZ,
      17 17 1 A64ICODE:SUBI,
      7 17 PROF-ABI:PROF-REC-BITS A64ICODE:LSRI,                     \ the sampled word
      17 17 PROF-ABI:PROF-REC-MASK A64ICODE:ANDI,                    \ its caller
      10 21 8 A64ICODE:LDR,  10 A64ICODE:SP 72 A64ICODE:STR,
      25 ecomma A64ICODE:CBZ,  s" ," C-PROF-SAY
      ecomma A64ICODE:LBL,  25 1 A64ICODE:MOVZ,
      s\" {\"word\":\"" C-PROF-SAY
      9 7 0 A64ICODE:ADDI,  C-PROF-JSON-NAME
      C-PROF-DQ  s\" ,\"caller\":\"" C-PROF-SAY
      9 17 0 A64ICODE:ADDI,  C-PROF-JSON-NAME
      C-PROF-DQ  s\" ,\"n\":" C-PROF-SAY
      9 A64ICODE:SP 72 A64ICODE:LDR,  10 1 A64ICODE:MOVZ,  LPROFNUM A64ICODE:LABEL@ A64ICODE:BL,
      s" }" C-PROF-SAY
   enext A64ICODE:LBL,  21 21 PROF-ABI:PROF-CALL-ENT A64ICODE:ADDI,  23 23 1 A64ICODE:SUBI,  el A64ICODE:B,
   edone A64ICODE:LBL,
   s\" ]}\n" C-PROF-SAY ;

: EMIT-PROFREP ( bool -- ) {: json:bool :}
   json IF LPROFJSON A64ICODE:LABEL@ A64ICODE:LBL, ELSE LPROFDUMP A64ICODE:LABEL@ A64ICODE:LBL, THEN
   A64ICODE:LBL {: armed :}
   C-PROF-REP-OPEN
   7 PROF-BAND-VA A64ICODE:LIT64,
   22 7 PROF-ABI:PROF-TOT A64ICODE:LDR,
   19 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   19 armed A64ICODE:CBNZ,
      json IF s\" {\"samples\":0,\"armed\":false}\n" ELSE s\" profiler not armed\n" THEN C-PROF-SAY
      C-PROF-REP-CLOSE
   armed A64ICODE:LBL,
   0 19 0 A64ICODE:ADDI,  C-PROF-INCL-FOLD
   C-PROF-REP-WORDSUM
   json C-PROF-REP-HEAD
   json IF
      C-PROF-JSON-ROWS
      C-PROF-JSON-EDGES
      C-PROF-REP-CLOSE
   THEN
   false C-PROF-REP-SECTION
   s" by inclusive" C-PROF-SAY  C-PROF-NL
   true C-PROF-REP-SECTION
   C-PROF-REP-CLOSE ;

\ LPROFROW ( -- ): the text row for one dictionary record whatever its rank,
\ with its callers.
: EMIT-PROFROW ( -- )
   LPROFROW A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL {: rnone :}
   C-PROF-REP-OPEN                                 \ x6 = the profile ID requested
   7 PROF-BAND-VA A64ICODE:LIT64,
   22 7 PROF-ABI:PROF-TOT A64ICODE:LDR,
   19 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   19 rnone A64ICODE:CBZ,
   6 0 A64ICODE:CMPI,  A64ASM:C-LT rnone A64ICODE:BCOND,
   23 19 PROF-ABI:ARN-IDS A64ICODE:LDR,  6 23 A64ICODE:CMP,  A64ASM:C-CS rnone A64ICODE:BCOND,
   21 6 0 A64ICODE:ADDI,
   C-PROF-REP-ROW-REC
   10 5 PROF-ABI:RID-START A64ICODE:LDR,  10 rnone A64ICODE:CBZ,
   0 19 0 A64ICODE:ADDI,  C-PROF-INCL-FOLD
   17 21 0 A64ICODE:ADDI,
   7 PROF-CNT-VA A64ICODE:LIT64,  10 17 3 A64ICODE:LSLI,  7 7 10 A64ICODE:ADD,  7 7 0 A64ICODE:LDR,
   C-PROF-ROW-INCL  9 A64ICODE:SP 88 A64ICODE:STR,
   C-PROF-REP-ROW
   C-PROF-REP-CALLERS
   C-PROF-REP-CLOSE
   rnone A64ICODE:LBL,
   s" profiler: no row for that record" C-PROF-SAY  C-PROF-NL
   C-PROF-REP-CLOSE ;

\ x8 = arena, on a thread that is not ARN-OWNER: SIGALRM to ARN-OWNER-TID. An
\ owner that exited leaves a handle the kernel can give this thread; that
\ delivery is dropped: a signal to itself would arrive again at its sigreturn,
\ and the thread would never run its own code.
\ Clobbers x0-x2, x8-x11 and x16.
: C-PROF-FORWARD ( -- )
   A64ICODE:LBL {: fself :}
   10 8 PROF-ABI:ARN-OWNER-TID A64ICODE:LDR,
   9 SYSCALL:OS-THREAD-HANDLE
   9 10 A64ICODE:CMP,  A64ASM:C-EQ fself A64ICODE:BCOND,
   11 PROF-ABI:SIGALRM A64ICODE:MOVZ,  11 10 SYSCALL:OS-SIGNAL-THREAD
   fself A64ICODE:LBL,  9 SYSCALL:OS-THREAD-RELEASE ;

\ x7 = band, x8 = arena: x3 = this sample's weight, as the file header states;
\ ARN-LAST moves to now, ARN-CARRY keeps the remainder and ARN-SEQ counts the
\ sample, whatever its weight. Plain loads and stores suffice: only the owner's
\ handler reaches here, and a word that leaves ARMED-RUNS waits for it. The ISB
\ keeps the timer clock from being read before the admission's load-acquire,
\ ahead of the ARN-LAST a resume wrote.
\ Clobbers x10-x12.
: C-PROF-WEIGHT ( -- )
   A64ICODE:ISB,  10 SYSCALL:OS-TIMER-NOW
   11 8 PROF-ABI:ARN-LAST A64ICODE:LDR,  10 8 PROF-ABI:ARN-LAST A64ICODE:STR,
   12 8 PROF-ABI:ARN-SEQ A64ICODE:LDR,  12 12 1 A64ICODE:ADDI,  12 8 PROF-ABI:ARN-SEQ A64ICODE:STR,
   12 8 PROF-ABI:ARN-TICKS A64ICODE:LDR,
   10 10 11 A64ICODE:SUB,  11 8 PROF-ABI:ARN-CARRY A64ICODE:LDR,  10 10 11 A64ICODE:ADD,
   3 10 12 A64ICODE:UDIV,  11 3 12 A64ICODE:MUL,  10 10 11 A64ICODE:SUB,
   10 8 PROF-ABI:ARN-CARRY A64ICODE:STR, ;

\ x8 = arena, x21 = mcontext, a foreign sample: x6 = the index entry of the
\ Habu word nearest it, or 0 when neither x30 nor the scan names one. Off the
\ main stack, a task owner's stack among them, the scan stays in SP's block.
\ x30 and each scanned cell are tested as C-PROF-WALK tests them.
\ Clobbers x9-x13, x23 and x24.
: C-PROF-FCALLER ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: fl fnone fdone fblock fx30 :}
   31 23 C-PROF-CTX-X>R
   10 8 PROF-ABI:ARN-STACK-LOW A64ICODE:LDR,  23 10 A64ICODE:CMP,  A64ASM:C-CC fblock A64ICODE:BCOND,
   24 8 PROF-ABI:ARN-STACK-TOP A64ICODE:LDR,  23 24 A64ICODE:CMP,  A64ASM:C-CC fx30 A64ICODE:BCOND,
   fblock A64ICODE:LBL,  C-PROF-BLOCK-END
   fx30 A64ICODE:LBL,
   30 9 C-PROF-CTX-X>R
   9 9 4 A64ICODE:SUBI,
   10 8 PROF-ABI:ARN-LO A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CC fl A64ICODE:BCOND,
   10 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CS fl A64ICODE:BCOND,
   LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
   6 fdone A64ICODE:CBNZ,
   fl A64ICODE:LBL,
      23 24 A64ICODE:CMP,  A64ASM:C-CS fnone A64ICODE:BCOND,
      9 23 0 A64ICODE:LDR,  23 23 8 A64ICODE:ADDI,
      10 8 PROF-ABI:ARN-LO A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CC fl A64ICODE:BCOND,
      10 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-CS fl A64ICODE:BCOND,
      9 9 4 A64ICODE:SUBI,
      LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
      6 fl A64ICODE:CBZ,
      fdone A64ICODE:B,
   fnone A64ICODE:LBL,  6 0 A64ICODE:MOVZ,
   fdone A64ICODE:LBL, ;

\ x7 = band, x8 = arena, x6 = an index entry, x3 = the weight: the sample is
\ that word's own, exclusive and inclusive; x22 = its record for the walk.
\ Clobbers x10-x14.
: C-PROF-CHARGE ( -- )
   12 6 PROF-ABI:ENT-IDX A64ICODE:LDR,
   14 PROF-CNT-VA A64ICODE:LIT64,  13 12 3 A64ICODE:LSLI,  14 14 13 A64ICODE:ADD,
   12 14 0 A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 14 0 A64ICODE:STR,
   10 6 PROF-ABI:ENT-IDX A64ICODE:LDR,  C-PROF-INCL-ONCE                \ a word is inside itself
   22 6 PROF-ABI:ENT-IDX A64ICODE:LDR, ;

\ x7 = band: PROF-IN += n, 1 or -1, by an acquire-release atomic add.
\ Clobbers x9 and x10.
: C-PROF-IN-ADD ( n -- ) {: n:n :}
   n 0 < IF 9 0 A64ICODE:MOVN, ELSE 9 n A64ICODE:MOVZ, THEN
   10 7 PROF-ABI:PROF-IN A64ICODE:ADDI,  9 9 10 A64ICODE:LDADDAL, ;

\ THE HANDLER TAKES NOTHING FROM THE LIVE REGISTERS: a foreign callee (libc's
\ dlsym, libzip's inflate) reuses x20 (XREG-RBASE), x26 (DBASE) and x27 (NDICT).
\ The handler runs on the crash handler's alternate signal stack, so a deep
\ machine stack never pushes the frame over the data stack.
\
\ PROF-ARMED is read with a load-acquire, the pair of C-PROF-ARM's release. An
\ admitted handler adds 1 to PROF-IN, reads PROF-ARMED again and takes 1 away
\ on every way out; a word that leaves ARMED-RUNS stores the new state by
\ store-release and waits for PROF-IN to read 0. A store-release is ordered
\ before a later load-acquire, so either the handler reads the new state and
\ leaves, or the word waits for it. A delivery off the owner thread forwards
\ SIGALRM to the owner, which would otherwise miss the sample, while still
\ counted in PROF-IN, so prof-on cannot release that handle under it.
\
\ Weigh the sample, attribute the interrupted pc, THEN add the weight to
\ PROF-TOT and test the limit, so every sample the running clock delivers that
\ weighs an interval is counted, including the sample that reaches the limit.
\ x3 holds the weight: no word the handler calls writes it.
: EMIT-PROF ( -- )
   LPROFH A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: pnew pother pforeign plost psig pexit :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: pfwd pleave pregion :}
   7 PROF-BAND-VA A64ICODE:LIT64,
   9 7 PROF-ABI:PROF-ARMED A64ICODE:ADDI,  9 9 A64ICODE:LDAR,  9 PROF-ABI:ARMED-RUNS A64ICODE:CMPI,  A64ASM:C-NE pexit A64ICODE:BCOND,
   1 C-PROF-IN-ADD
   9 7 PROF-ABI:PROF-ARMED A64ICODE:ADDI,  9 9 A64ICODE:LDAR,  9 PROF-ABI:ARMED-RUNS A64ICODE:CMPI,  A64ASM:C-NE pleave A64ICODE:BCOND,
   8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   10 ENGINE-EMIT:CB-SELF,  11 8 PROF-ABI:ARN-OWNER A64ICODE:LDR,  10 11 A64ICODE:CMP,  A64ASM:C-NE pfwd A64ICODE:BCOND,
   C-PROF-WEIGHT
   3 pleave A64ICODE:CBZ,
   C-PROF-MCTX>R21  C-PROF-PC>R9
   17 1 A64ICODE:MOVZ,                                        \ x17 = 1: a Habu context, its count may report (x16 is the Darwin syscall number, never kept across a sys)
   20 10 C-PROF-CTX-X>R  11 8 PROF-ABI:ARN-OWNER-DATA A64ICODE:LDR,  10 11 A64ICODE:CMP,  A64ASM:C-NE pforeign A64ICODE:BCOND,
   26 10 C-PROF-CTX-X>R  11 7 PROF-ABI:PROF-DBASE A64ICODE:LDR,  10 11 A64ICODE:CMP,  A64ASM:C-NE pforeign A64ICODE:BCOND,
   LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,                            \ the handler never returns, so x30 needs no frame
   6 pnew A64ICODE:CBZ,
   C-PROF-CHARGE
   C-PROF-WALK
   psig A64ICODE:B,
   pnew A64ICODE:LBL,
   22 0 A64ICODE:MOVN,                                        \ no record of its own: callers only
   10 7 PROF-ABI:PROF-DBASE A64ICODE:LDR,  10 9 10 A64ICODE:SUB,
   11 REGION A64ICODE:LIT64,  10 11 A64ICODE:CMP,  A64ASM:C-CC pregion A64ICODE:BCOND,  \ in the code region
   10 PROF-RBASE-VA A64ICODE:LIT64,  10 10 0 A64ICODE:LDR,  10 9 10 A64ICODE:SUB,
   11 PROF-TEXT-SIZE-VA A64ICODE:LIT64,  11 11 0 A64ICODE:LDR,
   10 11 A64ICODE:CMP,  A64ASM:C-CC pother A64ICODE:BCOND,                    \ in the kernel text: engine helpers, main loop, a gap
   pforeign A64ICODE:B,                                       \ outside Habu's code: C that keeps x20 and x26
   pregion A64ICODE:LBL,
   11 8 PROF-ABI:ARN-HI A64ICODE:LDR,  9 11 A64ICODE:CMP,  A64ASM:C-CC pother A64ICODE:BCOND,   \ below the index's high mark: a gap
   C-PROF-DEFER                                      \ at or above it: compiled after prof-on built the index
   C-PROF-WALK
   psig A64ICODE:B,
   pother A64ICODE:LBL,
   12 7 PROF-ABI:PROF-OTHER A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 7 PROF-ABI:PROF-OTHER A64ICODE:STR,
   C-PROF-WALK
   psig A64ICODE:B,
   pforeign A64ICODE:LBL,
   17 0 A64ICODE:MOVZ,                                        \ not Habu code: no report from here
   C-PROF-FCALLER
   6 plost A64ICODE:CBZ,
   12 8 PROF-ABI:ARN-FCALLED A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 8 PROF-ABI:ARN-FCALLED A64ICODE:STR,
   C-PROF-CHARGE
   C-PROF-WALK
   psig A64ICODE:B,
   plost A64ICODE:LBL,
   12 7 PROF-ABI:PROF-FOREIGN A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 7 PROF-ABI:PROF-FOREIGN A64ICODE:STR,
   psig A64ICODE:LBL,
   10 7 PROF-ABI:PROF-TOT A64ICODE:LDR,  10 10 3 A64ICODE:ADD,  10 7 PROF-ABI:PROF-TOT A64ICODE:STR,
   11 7 PROF-ABI:PROF-LIM A64ICODE:LDR,  11 pleave A64ICODE:CBZ,                \ limit 0: sample until prof-off
   10 11 A64ICODE:CMP,  A64ASM:C-LT pleave A64ICODE:BCOND,
   \ the limit reached on a foreign sample: the next Habu sample that weighs
   \ an interval reports
   17 pleave A64ICODE:CBZ,
   LPROFDUMP A64ICODE:LABEL@ A64ICODE:BL,
   7 PROF-BAND-VA A64ICODE:LIT64,  -1 C-PROF-IN-ADD
   0 EXIT-STATUS:PROF-LIMIT A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   pfwd A64ICODE:LBL,  C-PROF-FORWARD
   pleave A64ICODE:LBL,  -1 C-PROF-IN-ADD
   pexit A64ICODE:LBL,  0 4 0 A64ICODE:ADDI,  SYSCALL:NR-SIGRETURN SYSCALL:SYS,  A64ICODE:BRK, ;

private

: C-PROF-SIGACTION-FRAME ( -- )
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
   9 LPROFH A64ICODE:LABEL@ A64ICODE:ADR,  9 A64ICODE:SP 0 A64ICODE:STR,
   HB-TARGET-LINUX-ARM64? IF
      10 PROF-ABI:LINUX-SA-PROF-FLAGS A64ICODE:LIT64,  10 A64ICODE:SP 8 A64ICODE:STR,
      10 0 A64ICODE:MOVZ,  10 A64ICODE:SP 16 A64ICODE:STR,  10 A64ICODE:SP 24 A64ICODE:STR,
      exit
   THEN
   9 A64ICODE:SP 8 A64ICODE:STR,
   10 MACOS-SA-PROF-FLAGS A64ICODE:MOVZ,  10 10 32 A64ICODE:LSLI,  10 A64ICODE:SP 16 A64ICODE:STR, ;

: C-PROF-SIGACTION ( -- )
   0 PROF-ABI:SIGALRM A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,
   HB-TARGET-LINUX-ARM64? IF 3 ENGINE-EMIT:LINUX-SIGSET-SIZE A64ICODE:MOVZ, THEN
   SYSCALL:NR-SIGACTION SYSCALL:SYS, ;

: C-PROF-SIGACTION-DONE ( -- )
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI, ;

\ x0 = arena: x10 = the interval prof-rate left there, or the 1000 us default,
\ written back so the report states the rate it actually sampled at.
: C-PROF-USEC>R10 ( -- )
   A64ICODE:LBL {: dflt :}
   10 0 PROF-ABI:ARN-USEC A64ICODE:LDR,  10 dflt A64ICODE:CBNZ,  10 1000 A64ICODE:MOVZ,
   dflt A64ICODE:LBL,  10 0 PROF-ABI:ARN-USEC A64ICODE:STR, ;

\ x0 = arena; run before ARMED-RUNS. Seconds and the rest scale apart so no
\ product wraps for an interval prof-rate takes. Clobbers x10-x12.
: C-PROF-CLOCK-MARK ( -- )
   A64ICODE:LBL {: tk :}
   C-PROF-USEC>R10
   11 PROF-ABI:USEC-PER-SEC A64ICODE:LIT64,  12 10 11 A64ICODE:UDIV,
   11 12 11 A64ICODE:MUL,  10 10 11 A64ICODE:SUB,
   11 SYSCALL:OS-TIMER-HZ  12 12 11 A64ICODE:MUL,  10 10 11 A64ICODE:MUL,
   11 PROF-ABI:USEC-PER-SEC A64ICODE:LIT64,  10 10 11 A64ICODE:UDIV,  10 10 12 A64ICODE:ADD,
   10 tk A64ICODE:CBNZ,  10 1 A64ICODE:MOVZ,
   tk A64ICODE:LBL,  10 0 PROF-ABI:ARN-TICKS A64ICODE:STR,
   10 SYSCALL:OS-TIMER-NOW  10 0 PROF-ABI:ARN-LAST A64ICODE:STR, ;

\ x0 = arena: the interval split for setitimer. The signed divide keeps an
\ interval below 0 negative, which C-PROF-TIMER's arm rejects rather than
\ reading it as years. Clobbers x9-x11.
: C-PROF-TIMER-FRAME ( -- )
   C-PROF-USEC>R10
   11 PROF-ABI:USEC-PER-SEC A64ICODE:LIT64,  9 10 11 A64ICODE:SDIV,               \ x9 = seconds
   11 9 11 A64ICODE:MUL,  10 10 11 A64ICODE:SUB,                        \ x10 = microseconds
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
   9 A64ICODE:SP 0 A64ICODE:STR,  10 A64ICODE:SP 8 A64ICODE:STR,
   9 A64ICODE:SP 16 A64ICODE:STR,  10 A64ICODE:SP 24 A64ICODE:STR, ;

\ A failed setitimer is named and fatal: the phase would sample nothing and
\ report zeros.
: C-PROF-TIMER ( -- )
   A64ICODE:LBL A64ICODE:LBL {: ok msg :}
   0 0 A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-SETITIMER SYSCALL:SYS,
   A64ASM:C-CC ok A64ICODE:BCOND,
      1 msg A64ICODE:ADR,  2 PROF-ABI:PROFARMMSG$ STR:LEN A64ICODE:MOVZ,  0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      0 EXIT-STATUS:SYSTEM A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
      msg A64ICODE:LBL,  PROF-ABI:PROFARMMSG$ A64ICODE:BYTES,
   ok A64ICODE:LBL, ;

: C-PROF-TIMER-DONE ( -- )
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI, ;

\ x0 = arena, a C-PROF-TIMER-FRAME frame on SP: first expiry = interval less
\ carry, seconds and rest scaled apart so no product wraps. Clobbers x9-x11.
: C-PROF-TIMER-REST ( -- )
   A64ICODE:LBL A64ICODE:LBL {: whole split :}
   9 0 PROF-ABI:ARN-CARRY A64ICODE:LDR,  10 0 PROF-ABI:ARN-TICKS A64ICODE:LDR,
   9 10 A64ICODE:CMP,  A64ASM:C-CS whole A64ICODE:BCOND,
   10 SYSCALL:OS-TIMER-HZ  11 9 10 A64ICODE:UDIV,
   10 11 10 A64ICODE:MUL,  9 9 10 A64ICODE:SUB,
   10 PROF-ABI:USEC-PER-SEC A64ICODE:LIT64,  11 11 10 A64ICODE:MUL,  9 9 10 A64ICODE:MUL,
   10 SYSCALL:OS-TIMER-HZ  9 9 10 A64ICODE:UDIV,  9 9 11 A64ICODE:ADD,
   11 0 PROF-ABI:ARN-USEC A64ICODE:LDR,  9 11 9 A64ICODE:SUB,
   split A64ICODE:B,
   whole A64ICODE:LBL,  9 1 A64ICODE:MOVZ,
   split A64ICODE:LBL,
   11 PROF-ABI:USEC-PER-SEC A64ICODE:LIT64,  10 9 11 A64ICODE:UDIV,
   11 10 11 A64ICODE:MUL,  9 9 11 A64ICODE:SUB,
   10 A64ICODE:SP 16 A64ICODE:STR,  9 A64ICODE:SP 24 A64ICODE:STR, ;

: C-PROF-ARENA-MAP ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: have mok amsg :}
   7 PROF-BAND-VA A64ICODE:LIT64,  9 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  9 have A64ICODE:CBNZ,
      0 0 A64ICODE:MOVZ,  1 PROF-ABI:ARN-BYTES A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
      SYSCALL:NR-MMAP SYSCALL:SYS,
      A64ASM:C-CC mok A64ICODE:BCOND,
         1 amsg A64ICODE:ADR,  2 PROF-ABI:PROFARNMSG$ STR:LEN A64ICODE:MOVZ,  0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
         0 EXIT-STATUS:SYSTEM A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
      amsg A64ICODE:LBL,  PROF-ABI:PROFARNMSG$ A64ICODE:BYTES,
      mok A64ICODE:LBL,
      7 PROF-BAND-VA A64ICODE:LIT64,  0 7 PROF-ABI:PROF-ARENA A64ICODE:STR,
   have A64ICODE:LBL, ;

: C-PROF-DIE ( str -- ) {: s:str :}
   A64ICODE:LBL {: msg :}
   0 2 A64ICODE:MOVZ,  1 msg A64ICODE:ADR,  2 s STR:LEN A64ICODE:LIT64,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 EXIT-STATUS:SYSTEM A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   msg A64ICODE:LBL,  s A64ICODE:BYTES, ;

\ x5 = dictionary record; x1/x2 = raw name pointer/length. The external
\ pointer is read while the record is live, then its exact bytes are copied.
: C-PROF-RAW-NAME ( -- )
   A64ICODE:LBL {: inl :}
   1 5 24 A64ICODE:ADDI,  2 5 16 A64ICODE:LDR,
   0 2 DNAME-EXT A64ICODE:ANDI,  0 inl A64ICODE:CBZ,
   1 5 24 A64ICODE:LDR,
   inl A64ICODE:LBL,  2 2 DNAME-FLAG-BITS A64ICODE:LSLI,  2 2 DNAME-FLAG-BITS A64ICODE:LSRI, ;

\ x1 = destination, x2 = source, x3 = byte count. Advance the destination.
: C-PROF-COPY-NAME ( -- )
   A64ICODE:LBL A64ICODE:LBL {: cp done :}
   cp A64ICODE:LBL,  3 done A64ICODE:CBZ,
      0 2 0 A64ICODE:LDRB,  0 1 0 A64ICODE:STRB,
      1 1 1 A64ICODE:ADDI,  2 2 1 A64ICODE:ADDI,  3 3 1 A64ICODE:SUBI,  cp A64ICODE:B,
   done A64ICODE:LBL, ;

\ x17 = arena. Build WID -> namespace row in the sort scratch; keep x6 and x17.
\ A namespace has at most two WIDs, so the cell table remains at most half full.
: C-PROF-QUAL-INDEX ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: clear cleared recs next insert probe store keydone :}
   A64ICODE:LBL {: done :}
   13 PROF-ABI:ARN-SCR A64ICODE:LIT64,  13 17 13 A64ICODE:ADD,
   14 13 0 A64ICODE:ADDI,  15 PROF-ABI:ARN-IDX-BYTES A64ICODE:LIT64,  15 13 15 A64ICODE:ADD,
   0 0 A64ICODE:MOVZ,
   clear A64ICODE:LBL,
      14 15 A64ICODE:CMP,  A64ASM:C-CS cleared A64ICODE:BCOND,
      0 14 0 A64ICODE:STR,  14 14 CELL A64ICODE:ADDI,  clear A64ICODE:B,
   cleared A64ICODE:LBL,
   4 0 A64ICODE:MOVZ,  5 DBASE 0 A64ICODE:ADDI,
   recs A64ICODE:LBL,
      4 NDICT A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      0 5 PROF-ABI:DICT-WL-OFF A64ICODE:LDR,  0 0 1 A64ICODE:ADDI,  0 next A64ICODE:CBNZ,
      0 5 0 A64ICODE:LDR,  7 5 CELL A64ICODE:LDR,
      insert A64ICODE:LBL,
         0 keydone A64ICODE:CBZ,
         9 PROF-ABI:PROF-HASH A64ICODE:LIT64,  9 0 9 A64ICODE:MUL,  9 9 PROF-ABI:PROF-QUAL-MASK A64ICODE:ANDI,
      probe A64ICODE:LBL,
         10 9 3 A64ICODE:LSLI,  10 13 10 A64ICODE:ADD,
         11 10 0 A64ICODE:LDR,  11 store A64ICODE:CBZ,
         12 11 $FFFFFFFF A64ICODE:ANDI,  12 0 A64ICODE:CMP,  A64ASM:C-EQ keydone A64ICODE:BCOND,
         9 9 1 A64ICODE:ADDI,  9 9 PROF-ABI:PROF-QUAL-MASK A64ICODE:ANDI,  probe A64ICODE:B,
      store A64ICODE:LBL,
         11 4 1 A64ICODE:ADDI,  11 11 PROF-ABI:PROF-QUAL-BITS A64ICODE:LSLI,  11 11 0 A64ICODE:ORR,
         11 10 0 A64ICODE:STR,
      keydone A64ICODE:LBL,
         0 7 0 A64ICODE:ADDI,  7 0 A64ICODE:MOVZ,  0 insert A64ICODE:CBNZ,
   next A64ICODE:LBL,  4 4 1 A64ICODE:ADDI,  5 5 DREC A64ICODE:ADDI,  recs A64ICODE:B,
   done A64ICODE:LBL, ;

\ x0 = arena. Assign a stable ID to each newly live ordinal and copy its
\ qualified name into session-owned mmap chunks. The live map is -1 for an
\ unassigned ordinal. Prepare namespace
\ lookup only when new IDs need names; the later merge sort reuses its scratch.
\ Reports keep copied names.
: C-PROF-SNAPSHOT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: recs next qscan qfound qdone usechunk newchunk :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: idok sized mapok qready owncopy copydone done :}
   A64ICODE:LBL A64ICODE:LBL {: pending prepare :}
   17 0 0 A64ICODE:ADDI,  6 0 A64ICODE:MOVZ,
   pending A64ICODE:LBL,
      6 NDICT A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      15 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  15 17 15 A64ICODE:ADD,  0 6 3 A64ICODE:LSLI,  15 15 0 A64ICODE:ADD,
      0 15 0 A64ICODE:LDR,  1 0 A64ICODE:MOVN,  0 1 A64ICODE:CMP,  A64ASM:C-EQ prepare A64ICODE:BCOND,
      6 6 1 A64ICODE:ADDI,  pending A64ICODE:B,
   prepare A64ICODE:LBL,
   C-PROF-QUAL-INDEX
   recs A64ICODE:LBL,
      6 NDICT A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      15 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  15 17 15 A64ICODE:ADD,  0 6 3 A64ICODE:LSLI,  15 15 0 A64ICODE:ADD,
      0 15 0 A64ICODE:LDR,  1 0 A64ICODE:MOVN,  0 1 A64ICODE:CMP,  A64ASM:C-NE next A64ICODE:BCOND,
      10 17 PROF-ABI:ARN-IDS A64ICODE:LDR,
      0 DICT-CAP A64ICODE:LIT64,  10 0 A64ICODE:CMP,  A64ASM:C-CC idok A64ICODE:BCOND,
      PROF-ABI:PROFIDMSG$ C-PROF-DIE
      idok A64ICODE:LBL,
      7 PROF-ABI:ARN-RECS A64ICODE:LIT64,  7 17 7 A64ICODE:ADD,
      0 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  0 10 0 A64ICODE:MUL,  7 7 0 A64ICODE:ADD,
      5 DREC A64ICODE:MOVZ,  5 6 5 A64ICODE:MUL,  5 DBASE 5 A64ICODE:ADD,
      14 0 A64ICODE:MOVZ,                                  \ no qualifier yet
      0 5 PROF-ABI:DICT-WL-OFF A64ICODE:LDR,
      0 0 A64ICODE:CMPI,  A64ASM:C-LE qdone A64ICODE:BCOND,
      13 PROF-ABI:ARN-SCR A64ICODE:LIT64,  13 17 13 A64ICODE:ADD,
      15 PROF-ABI:PROF-HASH A64ICODE:LIT64,  15 0 15 A64ICODE:MUL,  15 15 PROF-ABI:PROF-QUAL-MASK A64ICODE:ANDI,
      qscan A64ICODE:LBL,
         1 15 3 A64ICODE:LSLI,  1 13 1 A64ICODE:ADD,  1 1 0 A64ICODE:LDR,  1 qdone A64ICODE:CBZ,
         2 1 $FFFFFFFF A64ICODE:ANDI,  0 2 A64ICODE:CMP,  A64ASM:C-EQ qfound A64ICODE:BCOND,
         15 15 1 A64ICODE:ADDI,  15 15 PROF-ABI:PROF-QUAL-MASK A64ICODE:ANDI,  qscan A64ICODE:B,
      qfound A64ICODE:LBL,
         14 1 PROF-ABI:PROF-QUAL-BITS A64ICODE:LSRI,  14 14 1 A64ICODE:SUBI,
         1 DREC A64ICODE:MOVZ,  14 14 1 A64ICODE:MUL,  14 DBASE 14 A64ICODE:ADD,
      qdone A64ICODE:LBL,
      C-PROF-RAW-NAME  9 1 0 A64ICODE:ADDI,  10 2 0 A64ICODE:ADDI,  \ own source and length
      11 0 A64ICODE:MOVZ,  12 0 A64ICODE:MOVZ,
      14 qready A64ICODE:CBZ,
      5 14 0 A64ICODE:ADDI,  C-PROF-RAW-NAME
      11 1 0 A64ICODE:ADDI,  12 2 1 A64ICODE:ADDI,                    \ qualifier plus ':'
      qready A64ICODE:LBL,
      13 10 12 A64ICODE:ADD,                               \ total exact name bytes
      14 17 PROF-ABI:ARN-CHUNKS A64ICODE:LDR,
      14 newchunk A64ICODE:CBZ,
      15 14 PROF-ABI:CHUNK-USED A64ICODE:LDR,  0 15 13 A64ICODE:ADD,
      1 14 PROF-ABI:CHUNK-LEN A64ICODE:LDR,  0 1 A64ICODE:CMP,  A64ASM:C-LS usechunk A64ICODE:BCOND,
      newchunk A64ICODE:LBL,
         1 13 PROF-ABI:CHUNK-DATA A64ICODE:ADDI,
         15 PROF-ABI:PROF-NAME-CHUNK A64ICODE:LIT64,  1 15 A64ICODE:CMP,  A64ASM:C-CS sized A64ICODE:BCOND,
         1 15 0 A64ICODE:ADDI,
      sized A64ICODE:LBL,
         15 1 0 A64ICODE:ADDI,                              \ Darwin mmap clears x1; x15 keeps the requested extent
         0 0 A64ICODE:MOVZ,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,
         4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,
         A64ASM:C-CC mapok A64ICODE:BCOND,
         PROF-ABI:PROFNAMEMSG$ C-PROF-DIE
      mapok A64ICODE:LBL,
         14 0 0 A64ICODE:ADDI,
         0 17 PROF-ABI:ARN-CHUNKS A64ICODE:LDR,  0 14 PROF-ABI:CHUNK-NEXT A64ICODE:STR,
         15 14 PROF-ABI:CHUNK-LEN A64ICODE:STR,
         15 13 PROF-ABI:CHUNK-DATA A64ICODE:ADDI,  15 14 PROF-ABI:CHUNK-USED A64ICODE:STR,
         14 17 PROF-ABI:ARN-CHUNKS A64ICODE:STR,
         4 14 PROF-ABI:CHUNK-DATA A64ICODE:ADDI,  owncopy A64ICODE:B,
      usechunk A64ICODE:LBL,
         4 14 15 A64ICODE:ADD,  15 15 13 A64ICODE:ADD,  15 14 PROF-ABI:CHUNK-USED A64ICODE:STR,
      owncopy A64ICODE:LBL,
      6 7 PROF-ABI:RID-OWNER A64ICODE:STR,  4 7 PROF-ABI:RID-NAME A64ICODE:STR,  13 7 PROF-ABI:RID-LEN A64ICODE:STR,
      0 0 A64ICODE:MOVZ,  0 7 PROF-ABI:RID-START A64ICODE:STR,  0 7 PROF-ABI:RID-END A64ICODE:STR,
      1 4 0 A64ICODE:ADDI,
      11 copydone A64ICODE:CBZ,
      2 11 0 A64ICODE:ADDI,  3 12 1 A64ICODE:SUBI,  C-PROF-COPY-NAME
      0 $3A A64ICODE:MOVZ,  0 1 0 A64ICODE:STRB,  1 1 1 A64ICODE:ADDI,
      copydone A64ICODE:LBL,
      2 9 0 A64ICODE:ADDI,  3 10 0 A64ICODE:ADDI,  C-PROF-COPY-NAME
      15 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  15 17 15 A64ICODE:ADD,  0 6 3 A64ICODE:LSLI,  15 15 0 A64ICODE:ADD,
      0 17 PROF-ABI:ARN-IDS A64ICODE:LDR,  0 15 0 A64ICODE:STR,
      0 0 1 A64ICODE:ADDI,  0 17 PROF-ABI:ARN-IDS A64ICODE:STR,
   next A64ICODE:LBL,  6 6 1 A64ICODE:ADDI,  recs A64ICODE:B,
   done A64ICODE:LBL,  0 17 0 A64ICODE:ADDI, ;

: C-PROF-SESSION-START ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: chunks clear done :}
   17 0 0 A64ICODE:ADDI,
   9 17 PROF-ABI:ARN-CHUNKS A64ICODE:LDR,
   chunks A64ICODE:LBL,
      9 clear A64ICODE:CBZ,
      10 9 PROF-ABI:CHUNK-NEXT A64ICODE:LDR,
      0 9 0 A64ICODE:ADDI,  1 9 PROF-ABI:CHUNK-LEN A64ICODE:LDR,  SYSCALL:NR-MUNMAP SYSCALL:SYS,
      A64ASM:C-CC done A64ICODE:BCOND,
      PROF-ABI:PROFNAMEFREEMSG$ C-PROF-DIE
      done A64ICODE:LBL,
      9 10 0 A64ICODE:ADDI,  chunks A64ICODE:B,
   clear A64ICODE:LBL,
   9 0 A64ICODE:MOVZ,  9 17 PROF-ABI:ARN-CHUNKS A64ICODE:STR,  9 17 PROF-ABI:ARN-IDS A64ICODE:STR,
   10 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  10 17 10 A64ICODE:ADD,
   11 DICT-CAP A64ICODE:LIT64,  11 11 3 A64ICODE:LSLI,  11 10 11 A64ICODE:ADD,
   9 0 A64ICODE:MOVN,
   A64ICODE:LBL A64ICODE:LBL {: zeroed turn :}
   turn A64ICODE:LBL,
      10 11 A64ICODE:CMP,  A64ASM:C-CS zeroed A64ICODE:BCOND,
      9 10 0 A64ICODE:STR,  10 10 8 A64ICODE:ADDI,  turn A64ICODE:B,
   zeroed A64ICODE:LBL,  0 17 0 A64ICODE:ADDI, ;

\ One index entry per profile ID with code of its own. A live or retired
\ word's ID takes its record's span; a seed-retired ID takes its stored span;
\ a package row has no entry. The old index's inclusive counts are folded first.
: C-PROF-INDEX-BUILD ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bl bnext bdone blo bhi retained span :}
   A64ICODE:LBL {: word :}
   9 PROF-ABI:ARN-IDX A64ICODE:LIT64,  9 0 9 A64ICODE:ADD,
   11 0 A64ICODE:MOVZ,  6 0 A64ICODE:MOVZ,
   1 0 A64ICODE:MOVN,  2 0 A64ICODE:MOVZ,                              \ lo = all ones, hi = 0
   bl A64ICODE:LBL,
      12 0 PROF-ABI:ARN-IDS A64ICODE:LDR,  6 12 A64ICODE:CMP,  A64ASM:C-CS bdone A64ICODE:BCOND,
      5 PROF-ABI:ARN-RECS A64ICODE:LIT64,  5 0 5 A64ICODE:ADD,
      12 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  12 6 12 A64ICODE:MUL,  5 5 12 A64ICODE:ADD,
      12 5 PROF-ABI:RID-OWNER A64ICODE:LDR,  12 0 A64ICODE:CMPI,  A64ASM:C-LT retained A64ICODE:BCOND,
      15 0 A64ICODE:MOVZ,  15 5 PROF-ABI:RID-START A64ICODE:STR,  15 5 PROF-ABI:RID-END A64ICODE:STR,
      13 DREC A64ICODE:MOVZ,  13 12 13 A64ICODE:MUL,  13 DBASE 13 A64ICODE:ADD,
      14 13 PROF-ABI:DICT-WL-OFF A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-GE word A64ICODE:BCOND,
      15 WL:RETIRED invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-NE bnext A64ICODE:BCOND,
      word A64ICODE:LBL,
      13 13 8 A64ICODE:LDR,  13 bnext A64ICODE:CBZ,
      12 5 PROF-ABI:RID-OWNER A64ICODE:LDR,  15 DREC A64ICODE:MOVZ,  12 12 15 A64ICODE:MUL,
      12 DBASE 12 A64ICODE:ADD,  12 12 0 A64ICODE:LDR,  12 bnext A64ICODE:CBZ,
      14 12 13 A64ICODE:ADD,
      12 5 PROF-ABI:RID-START A64ICODE:STR,  14 5 PROF-ABI:RID-END A64ICODE:STR,  span A64ICODE:B,
      retained A64ICODE:LBL,
      12 5 PROF-ABI:RID-START A64ICODE:LDR,  14 5 PROF-ABI:RID-END A64ICODE:LDR,
      12 bnext A64ICODE:CBZ,
      12 14 A64ICODE:CMP,  A64ASM:C-CS bnext A64ICODE:BCOND,
      span A64ICODE:LBL,
      12 9 PROF-ABI:ENT-START A64ICODE:STR,
      14 9 PROF-ABI:ENT-END A64ICODE:STR,
      6 9 PROF-ABI:ENT-IDX A64ICODE:STR,
      15 0 A64ICODE:MOVZ,  15 9 PROF-ABI:ENT-INCL A64ICODE:STR,
      9 9 PROF-ABI:PROF-ENT A64ICODE:ADDI,  11 11 1 A64ICODE:ADDI,
      12 1 A64ICODE:CMP,  A64ASM:C-CS blo A64ICODE:BCOND,  1 12 0 A64ICODE:ADDI,
      blo A64ICODE:LBL,  14 2 A64ICODE:CMP,  A64ASM:C-LS bhi A64ICODE:BCOND,  2 14 0 A64ICODE:ADDI,
      bhi A64ICODE:LBL,
   bnext A64ICODE:LBL,  6 6 1 A64ICODE:ADDI,  bl A64ICODE:B,
   bdone A64ICODE:LBL,
   11 0 PROF-ABI:ARN-COUNT A64ICODE:STR,  1 0 PROF-ABI:ARN-LO A64ICODE:STR,  2 0 PROF-ABI:ARN-HI A64ICODE:STR,
   NDICT 0 PROF-ABI:ARN-NDICT A64ICODE:STR, ;

\ Cleared by prof-on and prof-reset, and NEVER by C-PROF-INDEX-BUILD, which a
\ report runs again over a phase already counted.
: C-PROF-COUNTERS-CLEAR ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: il idone el edone :}
   15 0 A64ICODE:MOVZ,
   15 0 PROF-ABI:ARN-NEW A64ICODE:STR,  15 0 PROF-ABI:ARN-DROP A64ICODE:STR,  15 0 PROF-ABI:ARN-FRAMES A64ICODE:STR,
   15 0 PROF-ABI:ARN-SPILL A64ICODE:STR,  15 0 PROF-ABI:ARN-DEFER A64ICODE:STR,  15 0 PROF-ABI:ARN-FCALLED A64ICODE:STR,
   15 0 PROF-ABI:ARN-CARRY A64ICODE:STR,                            \ a phase's weights sum to its own armed time
   15 PROF-ABI:PROF-WALK-CELLS A64ICODE:MOVZ,  15 0 PROF-ABI:ARN-WALK A64ICODE:STR,
   12 PROF-ABI:ARN-INCL A64ICODE:LIT64,  12 0 12 A64ICODE:ADD,               \ the inclusive array and the stamps
   13 PROF-ABI:ARN-DEF A64ICODE:LIT64,  13 0 13 A64ICODE:ADD,                \ sit back to back, so one loop clears both
   15 0 A64ICODE:MOVZ,
   il A64ICODE:LBL,
      12 13 A64ICODE:CMP,  A64ASM:C-CS idone A64ICODE:BCOND,
      15 12 0 A64ICODE:STR,  12 12 8 A64ICODE:ADDI,  il A64ICODE:B,
   idone A64ICODE:LBL,
   12 PROF-ABI:ARN-IDX A64ICODE:LIT64,  12 0 12 A64ICODE:ADD,
   13 0 PROF-ABI:ARN-COUNT A64ICODE:LDR,  13 13 5 A64ICODE:LSLI,  13 12 13 A64ICODE:ADD,
   el A64ICODE:LBL,
      12 13 A64ICODE:CMP,  A64ASM:C-CS edone A64ICODE:BCOND,
      15 12 PROF-ABI:ENT-INCL A64ICODE:STR,  12 12 PROF-ABI:PROF-ENT A64ICODE:ADDI,  el A64ICODE:B,
   edone A64ICODE:LBL, ;

\ Every caller edge from the last run has to go before this one counts: the
\ table is keyed on record indices, and a rebuilt index gives them new meanings.
: C-PROF-CALL-CLEAR ( -- )
   A64ICODE:LBL A64ICODE:LBL {: cl cd :}
   9 PROF-ABI:ARN-CALL A64ICODE:LIT64,  9 0 9 A64ICODE:ADD,
   10 PROF-ABI:ARN-CALL-BYTES A64ICODE:LIT64,  10 9 10 A64ICODE:ADD,
   11 0 A64ICODE:MOVZ,
   cl A64ICODE:LBL,
      9 10 A64ICODE:CMP,  A64ASM:C-CS cd A64ICODE:BCOND,
      11 9 0 A64ICODE:STR,  9 9 8 A64ICODE:ADDI,  cl A64ICODE:B,
   cd A64ICODE:LBL, ;

\ The merge's two moves: one entry from the a side (x17) or the b side (x5) to
\ the output cursor (x6), each source advancing with it.
: C-PROF-ENT-COPY-A ( -- )
   3 17 0 A64ICODE:LDR,   3 6 0 A64ICODE:STR,
   3 17 8 A64ICODE:LDR,   3 6 8 A64ICODE:STR,
   3 17 16 A64ICODE:LDR,  3 6 16 A64ICODE:STR,
   3 17 24 A64ICODE:LDR,  3 6 24 A64ICODE:STR,
   17 17 PROF-ABI:PROF-ENT A64ICODE:ADDI,  6 6 PROF-ABI:PROF-ENT A64ICODE:ADDI, ;

: C-PROF-ENT-COPY-B ( -- )
   3 5 0 A64ICODE:LDR,   3 6 0 A64ICODE:STR,
   3 5 8 A64ICODE:LDR,   3 6 8 A64ICODE:STR,
   3 5 16 A64ICODE:LDR,  3 6 16 A64ICODE:STR,
   3 5 24 A64ICODE:LDR,  3 6 24 A64ICODE:STR,
   5 5 PROF-ABI:PROF-ENT A64ICODE:ADDI,  6 6 PROF-ABI:PROF-ENT A64ICODE:ADDI, ;

\ An odd number of passes leaves the sorted entries in the scratch half; bring
\ them home so the index always lives at one address the handler knows.
: C-PROF-SORT-LAND ( -- )
   A64ICODE:LBL A64ICODE:LBL {: cpl fin :}
   2 PROF-ABI:ARN-IDX A64ICODE:LIT64,  2 0 2 A64ICODE:ADD,
   9 2 A64ICODE:CMP,  A64ASM:C-EQ fin A64ICODE:BCOND,
   3 0 A64ICODE:MOVZ,
   cpl A64ICODE:LBL,
      3 14 A64ICODE:CMP,  A64ASM:C-CS fin A64ICODE:BCOND,
      4 9 3 A64ICODE:ADD,   4 4 0 A64ICODE:LDR,
      15 2 3 A64ICODE:ADD,  4 15 0 A64ICODE:STR,
      3 3 8 A64ICODE:ADDI,  cpl A64ICODE:B,
   fin A64ICODE:LBL, ;

\ Bottom-up merge sort of x11 entries by start address, arena in x0. Stable,
\ so records that share a start stay in dictionary order and the search's
\ "last start <= pc" answer is the same one an exhaustive scan in record order
\ reports.
: C-PROF-SORT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: pass block mrg ta tb arest brest bfin cb done skip :}
   11 1 A64ICODE:CMPI,  A64ASM:C-LS skip A64ICODE:BCOND,
   9 PROF-ABI:ARN-IDX A64ICODE:LIT64,  9 0 9 A64ICODE:ADD,
   10 PROF-ABI:ARN-SCR A64ICODE:LIT64,  10 0 10 A64ICODE:ADD,
   14 PROF-ABI:PROF-ENT A64ICODE:MOVZ,  14 11 14 A64ICODE:MUL,                  \ total bytes
   12 PROF-ABI:PROF-ENT A64ICODE:MOVZ,                                 \ merge width, bytes
   pass A64ICODE:LBL,
      12 14 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      13 9 0 A64ICODE:ADDI,  11 9 14 A64ICODE:ADD,
      block A64ICODE:LBL,
         13 11 A64ICODE:CMP,  A64ASM:C-CS bfin A64ICODE:BCOND,
         15 13 12 A64ICODE:ADD,  15 11 A64ICODE:CMP,  15 15 11 A64ASM:C-LS A64ICODE:CSEL,     \ mid
         16 15 12 A64ICODE:ADD,  16 11 A64ICODE:CMP,  16 16 11 A64ASM:C-LS A64ICODE:CSEL,     \ hi
         17 13 0 A64ICODE:ADDI,  5 15 0 A64ICODE:ADDI,
         6 13 9 A64ICODE:SUB,  6 6 10 A64ICODE:ADD,
         mrg A64ICODE:LBL,
            17 15 A64ICODE:CMP,  A64ASM:C-CS brest A64ICODE:BCOND,
            5 16 A64ICODE:CMP,  A64ASM:C-CS arest A64ICODE:BCOND,
            3 17 PROF-ABI:ENT-START A64ICODE:LDR,  4 5 PROF-ABI:ENT-START A64ICODE:LDR,  3 4 A64ICODE:CMP,  A64ASM:C-HI tb A64ICODE:BCOND,
         ta A64ICODE:LBL,  C-PROF-ENT-COPY-A  mrg A64ICODE:B,
         tb A64ICODE:LBL,  C-PROF-ENT-COPY-B  mrg A64ICODE:B,
         arest A64ICODE:LBL,  17 15 A64ICODE:CMP,  A64ASM:C-CS cb A64ICODE:BCOND,  C-PROF-ENT-COPY-A  arest A64ICODE:B,
         brest A64ICODE:LBL,  5 16 A64ICODE:CMP,  A64ASM:C-CS cb A64ICODE:BCOND,  C-PROF-ENT-COPY-B  brest A64ICODE:B,
         cb A64ICODE:LBL,  13 16 0 A64ICODE:ADDI,  block A64ICODE:B,
      bfin A64ICODE:LBL,
      3 9 0 A64ICODE:ADDI,  9 10 0 A64ICODE:ADDI,  10 3 0 A64ICODE:ADDI,
      12 12 1 A64ICODE:LSLI,  pass A64ICODE:B,
   done A64ICODE:LBL,
   C-PROF-SORT-LAND
   skip A64ICODE:LBL, ;

\ x8 = arena, x10 = a record index, x3 = the sample's weight: one inclusive
\ sample for that record. Clobbers x11 and x12, and preserves x10 for the caller.
: C-PROF-INCL+ ( -- )
   11 PROF-ABI:ARN-INCL A64ICODE:LIT64,  11 8 11 A64ICODE:ADD,  12 10 3 A64ICODE:LSLI,  11 11 12 A64ICODE:ADD,
   12 11 0 A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 11 0 A64ICODE:STR, ;

\ x0 = arena. The handler already took an inclusive sample for every frame the
\ OLD index could name, so replay adds one only for a record that index did not
\ reach - start at or above ARN-OLDHI. Each sample counts at the weight the
\ handler kept with it, in x3.
: C-PROF-REPLAY ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: rl rdone rmiss rnocall rgo rnext :}
   8 0 0 A64ICODE:ADDI,                                       \ LPROFFIND and LPROFEDGE take the arena in x8
   24 8 PROF-ABI:ARN-DEFER A64ICODE:LDR,
   23 PROF-ABI:ARN-DEF A64ICODE:LIT64,  23 8 23 A64ICODE:ADD,
   rl A64ICODE:LBL,
      24 rdone A64ICODE:CBZ,
      9 23 0 A64ICODE:LDR,  3 23 16 A64ICODE:LDR,
      LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
      6 rmiss A64ICODE:CBZ,
      22 6 PROF-ABI:ENT-IDX A64ICODE:LDR,
      14 PROF-CNT-VA A64ICODE:LIT64,  13 22 3 A64ICODE:LSLI,  14 14 13 A64ICODE:ADD,
      12 14 0 A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 14 0 A64ICODE:STR,       \ the exclusive sample this stands for
      10 22 0 A64ICODE:ADDI,  C-PROF-INCL+                    \ and the word is inside itself
      9 23 8 A64ICODE:LDR,  9 rnocall A64ICODE:CBZ,
      9 9 4 A64ICODE:SUBI,                                    \ the call site, not the return address
      LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
      6 rnocall A64ICODE:CBZ,
      10 6 PROF-ABI:ENT-IDX A64ICODE:LDR,  10 22 A64ICODE:CMP,  A64ASM:C-EQ rnocall A64ICODE:BCOND,
      13 6 PROF-ABI:ENT-START A64ICODE:LDR,  14 8 PROF-ABI:ARN-OLDHI A64ICODE:LDR,  13 14 A64ICODE:CMP,  A64ASM:C-CC rgo A64ICODE:BCOND,
      C-PROF-INCL+
      rgo A64ICODE:B,
      rnocall A64ICODE:LBL,  10 PROF-ABI:PROF-CALLER-NONE A64ICODE:LIT64,
      rgo A64ICODE:LBL,
      LPROFEDGE A64ICODE:LABEL@ A64ICODE:BL,
      rnext A64ICODE:B,
      rmiss A64ICODE:LBL,  12 8 PROF-ABI:ARN-NEW A64ICODE:LDR,  12 12 3 A64ICODE:ADD,  12 8 PROF-ABI:ARN-NEW A64ICODE:STR,
      rnext A64ICODE:LBL,  23 23 PROF-ABI:PROF-DEFER-ENT A64ICODE:ADDI,  24 24 1 A64ICODE:SUBI,  rl A64ICODE:B,
   rdone A64ICODE:LBL,
   12 0 A64ICODE:MOVZ,  12 8 PROF-ABI:ARN-DEFER A64ICODE:STR, ;

\ LPROFSYNC ( -- ): rebuild the pc index from the dictionary as it now stands and
\ replay the deferred samples through it.
\ IT RUNS FROM HABU CODE ONLY, never from the handler's auto-report at the limit:
\ the build reads NDICT, and the handler validates the interrupted x20 and x26
\ but not x27.
: EMIT-PROFSYNC ( -- )
   LPROFSYNC A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL {: nosync :}
   A64ICODE:SP A64ICODE:SP 96 A64ICODE:SUBI,
   30 A64ICODE:SP 0 A64ICODE:STR,  19 A64ICODE:SP 8 A64ICODE:STR,  21 A64ICODE:SP 24 A64ICODE:STR,  22 A64ICODE:SP 32 A64ICODE:STR,
   23 A64ICODE:SP 40 A64ICODE:STR,  24 A64ICODE:SP 48 A64ICODE:STR,  25 A64ICODE:SP 56 A64ICODE:STR,  17 A64ICODE:SP 64 A64ICODE:STR,
   7 PROF-BAND-VA A64ICODE:LIT64,  0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  0 nosync A64ICODE:CBZ,
   9 0 PROF-ABI:ARN-HI A64ICODE:LDR,  9 0 PROF-ABI:ARN-OLDHI A64ICODE:STR,
   C-PROF-INCL-FOLD
   C-PROF-SNAPSHOT
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-REPLAY
   nosync A64ICODE:LBL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  19 A64ICODE:SP 8 A64ICODE:LDR,  21 A64ICODE:SP 24 A64ICODE:LDR,  22 A64ICODE:SP 32 A64ICODE:LDR,
   23 A64ICODE:SP 40 A64ICODE:LDR,  24 A64ICODE:SP 48 A64ICODE:LDR,  25 A64ICODE:SP 56 A64ICODE:LDR,  17 A64ICODE:SP 64 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP 96 A64ICODE:ADDI,  A64ICODE:RET, ;

\ Clear ARN-OWNER-TID, so a forwarding handler reads 0, which names no thread,
\ then give the handle back. Clobbers x0-x2, x7-x9 and x16.
: C-PROF-OWNER-RELEASE ( -- )
   A64ICODE:LBL {: none :}
   7 PROF-BAND-VA A64ICODE:LIT64,  9 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  9 none A64ICODE:CBZ,
   2 9 PROF-ABI:ARN-OWNER-TID A64ICODE:LDR,
   0 0 A64ICODE:MOVZ,  0 9 PROF-ABI:ARN-OWNER-TID A64ICODE:STR,
   2 none A64ICODE:CBZ,
   2 SYSCALL:OS-THREAD-RELEASE
   none A64ICODE:LBL, ;

\ The calling thread into ARN-OWNER and ARN-OWNER-TID, its x20 into
\ ARN-OWNER-DATA (prof-on runs on the owner, in Habu code), the previous handle
\ given back, and the main stack's bounds into the arena: the top boot stored,
\ less the soft RLIMIT_STACK, or less PROF-STACK-BYTES when getrlimit fails,
\ answers no limit or one above the top. An owner that is not the main thread
\ gets bounds of 0: its own stack's extent is unknown, so every walk stays
\ block-bounded. Leaves x0 = arena, x7 = band; clobbers x1, x2, x9-x11 and the
\ syscall's registers.
: C-PROF-OWNER ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: dflt have other done :}
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,
   0 SYSCALL:RLIMIT-STACK A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  SYSCALL:NR-GETRLIMIT SYSCALL:SYS,
   10 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
   A64ASM:C-CS dflt A64ICODE:BCOND,
   11 SYSCALL:RLIM-INFINITY A64ICODE:LIT64,  10 11 A64ICODE:CMP,  A64ASM:C-EQ dflt A64ICODE:BCOND,
   9 PROF-TOP-VA A64ICODE:LIT64,  9 9 0 A64ICODE:LDR,  10 9 A64ICODE:CMP,  A64ASM:C-LS have A64ICODE:BCOND,
   dflt A64ICODE:LBL,  10 PROF-ABI:PROF-STACK-BYTES A64ICODE:LIT64,
   have A64ICODE:LBL,  C-PROF-OWNER-RELEASE  2 SYSCALL:OS-THREAD-HANDLE
   7 PROF-BAND-VA A64ICODE:LIT64,  0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   2 0 PROF-ABI:ARN-OWNER-TID A64ICODE:STR,  XREG-RBASE 0 PROF-ABI:ARN-OWNER-DATA A64ICODE:STR,
   11 ENGINE-EMIT:CB-SELF,  11 0 PROF-ABI:ARN-OWNER A64ICODE:STR,
   9 PROF-THREAD-VA A64ICODE:LIT64,  9 9 0 A64ICODE:LDR,  9 11 A64ICODE:CMP,  A64ASM:C-NE other A64ICODE:BCOND,
   9 PROF-TOP-VA A64ICODE:LIT64,  9 9 0 A64ICODE:LDR,  9 0 PROF-ABI:ARN-STACK-TOP A64ICODE:STR,
   9 9 10 A64ICODE:SUB,  9 0 PROF-ABI:ARN-STACK-LOW A64ICODE:STR,  done A64ICODE:B,
   other A64ICODE:LBL,
   9 0 A64ICODE:MOVZ,  9 0 PROF-ABI:ARN-STACK-TOP A64ICODE:STR,  9 0 PROF-ABI:ARN-STACK-LOW A64ICODE:STR,
   done A64ICODE:LBL, ;

\ x7 = band: PROF-ARMED = ARMED-RUNS by store-release, so a handler on any
\ thread that sees ARMED-RUNS sees ARN-OWNER and the session's cells. Clobbers
\ x9 and x10.
: C-PROF-ARM ( -- )
   9 PROF-ABI:ARMED-RUNS A64ICODE:MOVZ,  10 7 PROF-ABI:PROF-ARMED A64ICODE:ADDI,  9 10 A64ICODE:STLR, ;

\ x7 = band: PROF-ARMED = state by store-release, then wait until PROF-IN reads
\ 0. On the owner thread it reads 0 at once: its handler is never suspended by
\ the code it interrupted. Clobbers x9 and x10.
: C-PROF-LEAVE-RUNS ( n -- ) {: state:n :}
   A64ICODE:LBL A64ICODE:LBL {: spin out :}
   9 state A64ICODE:MOVZ,  10 7 PROF-ABI:PROF-ARMED A64ICODE:ADDI,  9 10 A64ICODE:STLR,
   10 7 PROF-ABI:PROF-IN A64ICODE:ADDI,
   spin A64ICODE:LBL,  9 10 A64ICODE:LDAR,  9 out A64ICODE:CBZ,  A64ICODE:YIELD,  spin A64ICODE:B,
   out A64ICODE:LBL, ;

\ The ticket lock: take a ticket from PROF-CTL-NEXT by an acquire-release
\ atomic add, then wait until PROF-CTL-SERVE reads it. The handler never
\ touches either cell, so the owner's handler cannot wait on its own thread.
\ Clobbers x9-x11.
: C-PROF-CTL-TAKE ( -- )
   A64ICODE:LBL A64ICODE:LBL {: spin got :}
   9 1 A64ICODE:MOVZ,  10 PROF-BAND-VA PROF-ABI:PROF-CTL-NEXT + A64ICODE:LIT64,
   9 9 10 A64ICODE:LDADDAL,
   11 PROF-BAND-VA PROF-ABI:PROF-CTL-SERVE + A64ICODE:LIT64,
   spin A64ICODE:LBL,  10 11 A64ICODE:LDAR,  10 9 A64ICODE:CMP,  A64ASM:C-EQ got A64ICODE:BCOND,  A64ICODE:YIELD,  spin A64ICODE:B,
   got A64ICODE:LBL, ;

\ PROF-CTL-SERVE + 1 by store-release: the next ticket runs. Clobbers x10 and
\ x11.
: C-PROF-CTL-FREE ( -- )
   11 PROF-BAND-VA PROF-ABI:PROF-CTL-SERVE + A64ICODE:LIT64,
   10 11 0 A64ICODE:LDR,  10 10 1 A64ICODE:ADDI,  10 11 A64ICODE:STLR, ;

\ The handler counts nothing until the arm: a clock already running keeps
\ ticking through the clears and the rebuild.
: BPROF-ON ( -- )
   A64ICODE:LBL A64ICODE:LBL {: zl zd :}
   C-PROF-CTL-TAKE
   7 PROF-BAND-VA A64ICODE:LIT64,
   0 C-PROF-LEAVE-RUNS
   ENGINE-EMIT:A RT:G-POP  ENGINE-EMIT:A 7 PROF-ABI:PROF-LIM A64ICODE:STR,
   9 0 A64ICODE:MOVZ,  9 7 PROF-ABI:PROF-TOT A64ICODE:STR,  9 7 PROF-ABI:PROF-OTHER A64ICODE:STR,  9 7 PROF-ABI:PROF-FOREIGN A64ICODE:STR,
   DBASE 7 PROF-ABI:PROF-DBASE A64ICODE:STR,
   14 PROF-CNT-VA A64ICODE:LIT64,  8 DICT-CAP A64ICODE:LIT64,
   zl A64ICODE:LBL,  8 zd A64ICODE:CBZ,  9 0 A64ICODE:MOVZ,  9 14 0 A64ICODE:STR,  14 14 8 A64ICODE:ADDI,  8 8 1 A64ICODE:SUBI,  zl A64ICODE:B,
   zd A64ICODE:LBL,
   C-PROF-ARENA-MAP
   C-PROF-OWNER
   C-PROF-SESSION-START
   C-PROF-SNAPSHOT
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-COUNTERS-CLEAR
   C-PROF-CALL-CLEAR
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   7 PROF-BAND-VA A64ICODE:LIT64,  0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,   \ the sigaction calls above own x0
   C-PROF-CLOCK-MARK
   C-PROF-ARM
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE
   C-PROF-CTL-FREE ;

\ Clobbers x0-x2, x8, x9 and x16.
: C-PROF-TIMER-STOP ( -- )
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
   9 0 A64ICODE:MOVZ,  9 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  9 A64ICODE:SP 16 A64ICODE:STR,  9 A64ICODE:SP 24 A64ICODE:STR,
   0 0 A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-SETITIMER SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI, ;

\ Hold the clock for a walk or a clear: a running clock goes to ARMED-HELD
\ before the timer stops, so the handler counts nothing from here on, and its
\ time since the last sample goes to ARN-CARRY once its handler is out; a held
\ or stopped clock only has its timer stopped. Clobbers x0-x2, x7-x10 and x16.
: C-PROF-CLOCK-HOLD ( -- )
   A64ICODE:LBL {: off :}
   7 PROF-BAND-VA A64ICODE:LIT64,  9 7 PROF-ABI:PROF-ARMED A64ICODE:LDR,
   9 PROF-ABI:ARMED-RUNS A64ICODE:CMPI,  A64ASM:C-NE off A64ICODE:BCOND,
   PROF-ABI:ARMED-HELD C-PROF-LEAVE-RUNS
   0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  0 off A64ICODE:CBZ,
   9 SYSCALL:OS-TIMER-NOW  1 0 PROF-ABI:ARN-LAST A64ICODE:LDR,  9 9 1 A64ICODE:SUB,
   1 0 PROF-ABI:ARN-CARRY A64ICODE:LDR,  9 9 1 A64ICODE:ADD,  9 0 PROF-ABI:ARN-CARRY A64ICODE:STR,
   off A64ICODE:LBL,
   C-PROF-TIMER-STOP ;

\ Start a held clock again at the recorded interval, its first expiry at the
\ interval's remainder after ARN-CARRY; only a held clock moves, so one
\ prof-off stopped stays stopped. Clobbers x0-x2, x7 and x9-x12.
: C-PROF-CLOCK-RESUME ( -- )
   A64ICODE:LBL {: off :}
   7 PROF-BAND-VA A64ICODE:LIT64,  9 7 PROF-ABI:PROF-ARMED A64ICODE:LDR,
   9 PROF-ABI:ARMED-HELD A64ICODE:CMPI,  A64ASM:C-NE off A64ICODE:BCOND,
   0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   C-PROF-CLOCK-MARK
   C-PROF-ARM
   C-PROF-TIMER-FRAME
   C-PROF-TIMER-REST
   C-PROF-TIMER
   C-PROF-TIMER-DONE
   off A64ICODE:LBL, ;

\ Retirement is reached from engine helpers whose scratch registers still
\ belong to their caller. It holds the ticket lock from before its first read
\ to its one exit, as a control word does, so a trim on one thread never
\ rebuilds the index under another thread's report. x9 is the bodies' input.
: C-PROF-RETIRE-OPEN ( label -- )
   {: absent:label :}
   A64ICODE:SP A64ICODE:SP 160 A64ICODE:SUBI,
   18 0 ?do i A64ICODE:SP i cells A64ICODE:STR, loop
   30 A64ICODE:SP 144 A64ICODE:STR,
   C-PROF-CTL-TAKE  9 A64ICODE:SP 9 cells A64ICODE:LDR,
   7 PROF-BAND-VA A64ICODE:LIT64,  8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  8 absent A64ICODE:CBZ,
   10 8 PROF-ABI:ARN-IDS A64ICODE:LDR,  10 absent A64ICODE:CBZ, ;

: C-PROF-RETIRE-CLOSE ( -- )
   C-PROF-CTL-FREE
   18 0 ?do i A64ICODE:SP i cells A64ICODE:LDR, loop
   30 A64ICODE:SP 144 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 160 A64ICODE:ADDI,  A64ICODE:RET, ;

\ Compact the sorted index in place, dropping IDs whose span was released. After
\ a sync every inclusive count is durable, so their history stays.
: C-PROF-INDEX-PRUNE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: turn next done :}
   8 0 0 A64ICODE:ADDI,
   1 PROF-ABI:ARN-IDX A64ICODE:LIT64,  1 8 1 A64ICODE:ADD,  2 1 0 A64ICODE:ADDI,
   3 8 PROF-ABI:ARN-COUNT A64ICODE:LDR,  3 3 5 A64ICODE:LSLI,  3 1 3 A64ICODE:ADD,
   4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   turn A64ICODE:LBL,
      1 3 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      6 1 PROF-ABI:ENT-IDX A64ICODE:LDR,
      7 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  6 6 7 A64ICODE:MUL,
      7 PROF-ABI:ARN-RECS A64ICODE:LIT64,  7 8 7 A64ICODE:ADD,  6 7 6 A64ICODE:ADD,
      6 6 PROF-ABI:RID-START A64ICODE:LDR,  6 next A64ICODE:CBZ,
      6 1 PROF-ABI:ENT-START A64ICODE:LDR,  7 1 PROF-ABI:ENT-END A64ICODE:LDR,
      6 2 PROF-ABI:ENT-START A64ICODE:STR,  7 2 PROF-ABI:ENT-END A64ICODE:STR,
      10 1 PROF-ABI:ENT-IDX A64ICODE:LDR,  10 2 PROF-ABI:ENT-IDX A64ICODE:STR,
      10 0 A64ICODE:MOVZ,  10 2 PROF-ABI:ENT-INCL A64ICODE:STR,
      6 4 A64ICODE:CMP,  4 6 4 A64ASM:C-CC A64ICODE:CSEL,
      7 5 A64ICODE:CMP,  5 7 5 A64ASM:C-HI A64ICODE:CSEL,
      2 2 PROF-ABI:PROF-ENT A64ICODE:ADDI,
   next A64ICODE:LBL,  1 1 PROF-ABI:PROF-ENT A64ICODE:ADDI,  turn A64ICODE:B,
   done A64ICODE:LBL,
   1 PROF-ABI:ARN-IDX A64ICODE:LIT64,  1 8 1 A64ICODE:ADD,
   2 2 1 A64ICODE:SUB,  2 2 5 A64ICODE:LSRI,
   2 8 PROF-ABI:ARN-COUNT A64ICODE:STR,  4 8 PROF-ABI:ARN-LO A64ICODE:STR,  5 8 PROF-ABI:ARN-HI A64ICODE:STR, ;

\ NDICT lowering: resolve deferred PCs before their dictionary rows disappear.
: EMIT-PROFTRIM ( -- )
   PROF-ABI:LPROFTRIM A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: absent scan sync turn next :}
   absent C-PROF-RETIRE-OPEN
   10 8 PROF-ABI:ARN-DEFER A64ICODE:LDR,  10 sync A64ICODE:CBNZ,
   6 9 0 A64ICODE:ADDI,
   scan A64ICODE:LBL,
      6 NDICT A64ICODE:CMP,  A64ASM:C-CS absent A64ICODE:BCOND,
      10 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  10 8 10 A64ICODE:ADD,  11 6 3 A64ICODE:LSLI,  10 10 11 A64ICODE:ADD,
      10 10 0 A64ICODE:LDR,  10 0 A64ICODE:CMPI,  A64ASM:C-GE sync A64ICODE:BCOND,
      6 6 1 A64ICODE:ADDI,  scan A64ICODE:B,
   sync A64ICODE:LBL,
   C-PROF-CLOCK-HOLD
   LPROFSYNC A64ICODE:LABEL@ A64ICODE:BL,
   6 A64ICODE:SP 72 A64ICODE:LDR,
   7 PROF-BAND-VA A64ICODE:LIT64,  8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   turn A64ICODE:LBL,
      6 NDICT A64ICODE:CMP,  A64ASM:C-CS next A64ICODE:BCOND,
      10 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  10 8 10 A64ICODE:ADD,  11 6 3 A64ICODE:LSLI,  10 10 11 A64ICODE:ADD,
      11 10 0 A64ICODE:LDR,
      A64ICODE:LBL {: skip :}
      11 0 A64ICODE:CMPI,  A64ASM:C-LT skip A64ICODE:BCOND,
      12 0 A64ICODE:MOVN,  12 10 0 A64ICODE:STR,
      10 PROF-ABI:ARN-RECS A64ICODE:LIT64,  10 8 10 A64ICODE:ADD,
      13 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  11 11 13 A64ICODE:MUL,  10 10 11 A64ICODE:ADD,
      12 10 PROF-ABI:RID-OWNER A64ICODE:STR,
      12 0 A64ICODE:MOVZ,  12 10 PROF-ABI:RID-START A64ICODE:STR,  12 10 PROF-ABI:RID-END A64ICODE:STR,
      skip A64ICODE:LBL,  6 6 1 A64ICODE:ADDI,  turn A64ICODE:B,
   next A64ICODE:LBL,
   0 8 0 A64ICODE:ADDI,  C-PROF-INDEX-PRUNE
   C-PROF-CLOCK-RESUME
   absent A64ICODE:LBL,  C-PROF-RETIRE-CLOSE ;

\ CP lowering: no raw PC may survive reuse of its bytes. Retire live and
\ seed-retained spans that intersect [new CP,old CP), after naming old samples.
: EMIT-PROFRECLAIM ( -- )
   PROF-ABI:LPROFRECLAIM A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: absent turn next done :}
   absent C-PROF-RETIRE-OPEN
   9 CP A64ICODE:CMP,  A64ASM:C-CS absent A64ICODE:BCOND,
   C-PROF-CLOCK-HOLD
   LPROFSYNC A64ICODE:LABEL@ A64ICODE:BL,
   9 A64ICODE:SP 72 A64ICODE:LDR,
   7 PROF-BAND-VA A64ICODE:LIT64,  8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   6 0 A64ICODE:MOVZ,  13 8 PROF-ABI:ARN-IDS A64ICODE:LDR,
   turn A64ICODE:LBL,
      6 13 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      10 PROF-ABI:ARN-RECS A64ICODE:LIT64,  10 8 10 A64ICODE:ADD,
      11 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  11 6 11 A64ICODE:MUL,  10 10 11 A64ICODE:ADD,
      11 10 PROF-ABI:RID-START A64ICODE:LDR,  11 next A64ICODE:CBZ,
      11 CP A64ICODE:CMP,  A64ASM:C-CS next A64ICODE:BCOND,
      11 10 PROF-ABI:RID-END A64ICODE:LDR,  11 9 A64ICODE:CMP,  A64ASM:C-LS next A64ICODE:BCOND,
      \ Keep a published ordinal's existing ID: SNAPSHOT must not recreate
      \ this invalidated dictionary span before ordinary trimming drops it.
      11 0 A64ICODE:MOVN,  11 10 PROF-ABI:RID-OWNER A64ICODE:STR,
      11 0 A64ICODE:MOVZ,  11 10 PROF-ABI:RID-START A64ICODE:STR,  11 10 PROF-ABI:RID-END A64ICODE:STR,
   next A64ICODE:LBL,  6 6 1 A64ICODE:ADDI,  turn A64ICODE:B,
   done A64ICODE:LBL,
   0 8 0 A64ICODE:ADDI,  C-PROF-INDEX-PRUNE
   C-PROF-CLOCK-RESUME
   absent A64ICODE:LBL,  C-PROF-RETIRE-CLOSE ;

\ A REPORT NEVER SAMPLES ITSELF. It reads PROF-TOT once and then walks every
\ counter; a tick in between would leave the header's own identity false by one.
: C-PROF-REPORT-CALL ( label -- ) {: rep:label :}
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   C-PROF-CLOCK-HOLD
   LPROFSYNC A64ICODE:LABEL@ A64ICODE:BL,
   rep A64ICODE:BL,
   C-PROF-CLOCK-RESUME
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

\ prof-off stops the clock and gives back the owner's handle, nothing else: the
\ handler, the index and every counter stay, so the phase can be reported
\ afterwards. PROF-ARMED goes to 0 before the timer stops, so a tick that
\ arrives after the stop counts nothing.
: BPROF-OFF ( -- )
   C-PROF-CTL-TAKE
   7 PROF-BAND-VA A64ICODE:LIT64,  0 C-PROF-LEAVE-RUNS
   C-PROF-TIMER-STOP
   C-PROF-OWNER-RELEASE
   C-PROF-CTL-FREE ;

\ prof-reset clears the counts and keeps the index, so a second phase skips the
\ sort. The clock is held for the clears: PROF-TOT is cleared first and each
\ bucket after it, so a tick counted in between would survive in PROF-TOT and
\ not in its bucket, and the identity would be off by one.
: BPROF-RESET ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: zl zd noarena :}
   C-PROF-CTL-TAKE
   C-PROF-CLOCK-HOLD
   7 PROF-BAND-VA A64ICODE:LIT64,
   9 0 A64ICODE:MOVZ,  9 7 PROF-ABI:PROF-TOT A64ICODE:STR,  9 7 PROF-ABI:PROF-OTHER A64ICODE:STR,  9 7 PROF-ABI:PROF-FOREIGN A64ICODE:STR,
   14 PROF-CNT-VA A64ICODE:LIT64,  8 DICT-CAP A64ICODE:LIT64,
   zl A64ICODE:LBL,  8 zd A64ICODE:CBZ,  9 0 A64ICODE:MOVZ,  9 14 0 A64ICODE:STR,  14 14 8 A64ICODE:ADDI,  8 8 1 A64ICODE:SUBI,  zl A64ICODE:B,
   zd A64ICODE:LBL,
   0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  0 noarena A64ICODE:CBZ,
   C-PROF-COUNTERS-CLEAR
   C-PROF-CALL-CLEAR
   noarena A64ICODE:LBL,
   C-PROF-CLOCK-RESUME
   C-PROF-CTL-FREE ;

: BPROF-REPORT ( -- )
   C-PROF-CTL-TAKE  LPROFDUMP A64ICODE:LABEL@ C-PROF-REPORT-CALL  C-PROF-CTL-FREE ;

: BPROF-JSON ( -- )
   C-PROF-CTL-TAKE  LPROFJSON A64ICODE:LABEL@ C-PROF-REPORT-CALL  C-PROF-CTL-FREE ;

\ prof-row ( n -- ): the row for dictionary record n, whatever its rank.
: BPROF-ROW ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad ready :}
   C-PROF-CTL-TAKE
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   6 RT:G-POP  6 A64ICODE:SP 8 A64ICODE:STR,
   C-PROF-CLOCK-HOLD
   LPROFSYNC A64ICODE:LABEL@ A64ICODE:BL,
   6 A64ICODE:SP 8 A64ICODE:LDR,
   6 0 A64ICODE:CMPI,  A64ASM:C-LT bad A64ICODE:BCOND,
   6 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   7 PROF-BAND-VA A64ICODE:LIT64,  8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  8 bad A64ICODE:CBZ,
   9 PROF-ABI:ARN-LIVE A64ICODE:LIT64,  9 8 9 A64ICODE:ADD,
   10 6 3 A64ICODE:LSLI,  9 9 10 A64ICODE:ADD,  6 9 0 A64ICODE:LDR,
   ready A64ICODE:B,
   bad A64ICODE:LBL,  6 0 A64ICODE:MOVN,
   ready A64ICODE:LBL,
   LPROFROW A64ICODE:LABEL@ A64ICODE:BL,
   C-PROF-CLOCK-RESUME
   C-PROF-CTL-FREE
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

\ prof-rate sets the interval the NEXT prof-on arms, rather than re-arming here:
\ a rate written while no handler is installed would hand the process a SIGALRM
\ it has no handler for. x10 holds n across C-PROF-ARENA-MAP, which writes only
\ x0-x5, x7-x9 and x16.
: BPROF-RATE ( -- )
   A64ICODE:LBL {: ok :}
   C-PROF-CTL-TAKE
   10 RT:G-POP
   11 SYSCALL:OS-TIMER-MAX-USEC A64ICODE:LIT64,  10 11 A64ICODE:CMP,  A64ASM:C-LS ok A64ICODE:BCOND,
      C-PROF-CTL-FREE
      9 PROF-ABI:E-PROF-RATE A64ICODE:LIT64,  9 RT:G-PUSH  ENGINE-EMIT:BTHROW
   ok A64ICODE:LBL,
   C-PROF-ARENA-MAP
   7 PROF-BAND-VA A64ICODE:LIT64,  0 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,
   10 0 PROF-ABI:ARN-USEC A64ICODE:STR,
   C-PROF-CTL-FREE ;

\ prof-pc>rec ( pc -- n ): the record index the armed index gives that pc, or -1.
: BPROF-PCREC ( -- )
   A64ICODE:LBL A64ICODE:LBL {: miss out :}
   C-PROF-CTL-TAKE
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   9 RT:G-POP
   7 PROF-BAND-VA A64ICODE:LIT64,  8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  8 miss A64ICODE:CBZ,
   LPROFFIND A64ICODE:LABEL@ A64ICODE:BL,
   6 miss A64ICODE:CBZ,
   10 6 PROF-ABI:ENT-IDX A64ICODE:LDR,
   9 PROF-ABI:ARN-RECS A64ICODE:LIT64,  9 8 9 A64ICODE:ADD,
   11 PROF-ABI:PROF-ID-ENT A64ICODE:MOVZ,  11 10 11 A64ICODE:MUL,  9 9 11 A64ICODE:ADD,
   9 9 PROF-ABI:RID-OWNER A64ICODE:LDR,  out A64ICODE:B,
   miss A64ICODE:LBL,  9 0 A64ICODE:MOVN,
   out A64ICODE:LBL,  9 RT:G-PUSH
   C-PROF-CTL-FREE
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

\ prof-child-reset ( -- ): in a process just forked, which has only the forking
\ thread, the profiler state other threads were part way through: no handler
\ counted in, no ticket waiting, the clock stopped (POSIX fork(): the child's
\ interval timers are cleared), and no owner. The owner's handle is cleared,
\ never released: a Mach port name is the parent's, never the child's. Plain
\ stores, no lock, no syscall: it runs where FORK-CHILD's resets run.
: BPROF-CHILD-RESET ( -- )
   A64ICODE:LBL {: none :}
   7 PROF-BAND-VA A64ICODE:LIT64,
   9 0 A64ICODE:MOVZ,  9 7 PROF-ABI:PROF-IN A64ICODE:STR,  9 7 PROF-ABI:PROF-ARMED A64ICODE:STR,
   10 7 PROF-ABI:PROF-CTL-NEXT A64ICODE:LDR,  10 7 PROF-ABI:PROF-CTL-SERVE A64ICODE:STR,
   8 7 PROF-ABI:PROF-ARENA A64ICODE:LDR,  8 none A64ICODE:CBZ,
   9 8 PROF-ABI:ARN-OWNER A64ICODE:STR,  9 8 PROF-ABI:ARN-OWNER-TID A64ICODE:STR,
   none A64ICODE:LBL, ;

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON ENGINE-EMIT:FPRIM-L  s" prof-report" ['] BPROF-REPORT ENGINE-EMIT:FPRIM-L
   s" prof-off" ['] BPROF-OFF ENGINE-EMIT:FPRIM-L  s" prof-reset" ['] BPROF-RESET ENGINE-EMIT:FPRIM-L
   s" prof-rate" ['] BPROF-RATE ENGINE-EMIT:FPRIM-L  s" prof-json" ['] BPROF-JSON ENGINE-EMIT:FPRIM-L
   s" prof-row" ['] BPROF-ROW ENGINE-EMIT:FPRIM-L
   s" prof-pc>rec" ['] BPROF-PCREC ENGINE-EMIT:FPRIM-L
   s" prof-child-reset" ['] BPROF-CHILD-RESET ENGINE-EMIT:FPRIM-L ;

: EMIT-PROF-REPORTS ( -- )
   EMIT-PROFNUM  EMIT-PROFPCT  EMIT-PROFNAME  EMIT-PROFSYNC
   EMIT-PROFTRIM  EMIT-PROFRECLAIM
   false EMIT-PROFREP  true EMIT-PROFREP  EMIT-PROFROW ;

;package
