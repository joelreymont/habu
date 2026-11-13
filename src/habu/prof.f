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
\ each pass takes the largest count below the one before it. No second array is
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
\ is the task's own region, or x26 not the recorded DBASE) is inside C a
\ Habu word called: an FFI word, libSystem's fsync. Its weight goes to the
\ nearest Habu word, the one the interrupted x30 returns into, else the first
\ the scan up from SP finds, bounded as the walk's, as that word's own
\ exclusive sample, and the walk names its callers from there. ARN-FCALLED
\ counts that weight; PROF-FOREIGN counts only the foreign samples for which
\ neither names a Habu word.
require src/habu/prof-abi.f

require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
using PROF-ABI
using CRASH
using ENGINE-EMIT
using RT
using SYSCALL

package PROF

public
variable LPROFH   variable LPROFDUMP   variable LPROFFIND   variable LPROFEDGE
variable LPROFJSON   variable LPROFNUM   variable LPROFPCT   variable LPROFNAME
variable LPROFSYNC   variable LPROFROW
private

\ The refusal is a die because this file loads inside the engine-build window,
\ which has no src/lib/ in it.
: PROF-TARGET-OK ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   s" hb: this target's signal frame is not modelled" 76 die ;
PROF-TARGET-OK

DATA-VA VA>N DATA-SIZE PROF-BAND-AT constant PROF-BAND-VA   \ absolute: DATA is fixed at DATA-VA
PROF-BAND-VA PROF-STATE-BYTES + constant PROF-CNT-VA
DATA-VA VA>N MAIN-STACK-TOP-CELL + constant PROF-TOP-VA
DATA-VA VA>N MAIN-THREAD-CELL + constant PROF-THREAD-VA
$0043 constant MACOS-SA-PROF-FLAGS        \ SA_ONSTACK | SA_RESTART | SA_SIGINFO

: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 4 MCTX-OFF LDR, ;

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;

\ rd = the interrupted context's x<n>, read from the signal frame (x21 = mcontext).
: C-PROF-CTX-X>R ( n n -- ) {: x:n rd:n :}
   HB-TARGET-LINUX? IF rd 21 LINUX-MCTX-X0-OFF x 8 * + LDR, exit THEN
   rd 21 SS-OFF x 8 * + LDR, ;

\ x13 = the record's code bytes from its raw length cell:
\ bit 31 marks an exact span, otherwise the body precedes one final slot.
: C-PROF-SPAN-BYTES ( -- )
   14 13 31 LSRI,
   13 13 CODE-SPAN:MASK ANDI,
   13 13 CODE-SPAN:INSN-BYTES ADDI,
   14 14 2 LSLI,  13 13 14 SUB, ;

public

\ LPROFFIND ( x9 = pc, x8 = arena -- x6 = index entry or 0 ): the whole of pc
\ attribution, shared by the signal handler and by `prof-pc>rec`. Clobbers
\ x10-x13 and obeys the primitive ABI (x19-x28 untouched), because prof-pc>rec
\ calls it from compiled Habu code.
: EMIT-PROFFIND ( -- )
   LPROFFIND LABEL@ LBL,
   LBL LBL LBL LBL LBL {: bl bhi bdone miss out :}
   10 8 ARN-COUNT LDR,  10 miss CBZ,
   11 8 ARN-LO LDR,  9 11 CMP,  C-CC miss BCOND,
   11 8 ARN-HI LDR,  9 11 CMP,  C-CS miss BCOND,
   11 0 MOVZ,                                         \ lo
   bl LBL,                                            \ rightmost entry with start <= pc
      11 10 CMP,  C-CS bdone BCOND,
      12 11 10 ADD,  12 12 1 LSRI,                    \ mid = (lo + hi) / 2
      13 12 5 LSLI,  13 8 13 ADD,  13 13 ARN-IDX ADDI,
      13 13 ENT-START LDR,
      13 9 CMP,  C-HI bhi BCOND,                      \ start > pc: mid is the new hi
      11 12 1 ADDI,  bl B,
   bhi LBL,  10 12 0 ADDI,  bl B,
   bdone LBL,
   11 miss CBZ,                                       \ every start is above the pc
   11 11 1 SUBI,  6 11 5 LSLI,  6 8 6 ADD,  6 6 ARN-IDX ADDI,
   13 6 ENT-END LDR,  9 13 CMP,  C-CC out BCOND,      \ the pc sits in a gap above it
   miss LBL,  6 0 MOVZ,
   out LBL,  RET, ;

\ LPROFEDGE ( x8 = arena, x22 = sampled record, x10 = caller record, x3 = the
\ sample's weight ): add the weight to one caller edge. Clobbers x11-x16. A
\ sample whose own record is unknown (x22 < 0) has no edge to key, and a full
\ probe window counts the weight as dropped rather than overwriting somebody
\ else's pair.
: EMIT-PROFEDGE ( -- )
   LPROFEDGE LABEL@ LBL,
   LBL LBL LBL LBL {: eprobe efree ehit edone :}
   22 0 CMPI,  C-LT edone BCOND,
   11 22 PROF-REC-BITS LSLI,  11 11 10 ORR,  11 11 1 ADDI,   \ key, never 0
   12 PROF-HASH LIT64,  12 11 12 MUL,  12 12 48 LSRI,
   14 ARN-CALL LIT64,  14 8 14 ADD,
   13 PROF-CALL-PROBE MOVZ,
   eprobe LBL,
      15 12 4 LSLI,  15 14 15 ADD,
      16 15 0 LDR,
      16 efree CBZ,
      16 11 CMP,  C-EQ ehit BCOND,
      12 12 1 ADDI,  12 12 PROF-CALL-MASK ANDI,
      13 13 1 SUBI,  13 eprobe CBNZ,
   16 8 ARN-DROP LDR,  16 16 3 ADD,  16 8 ARN-DROP STR,  edone B,
   efree LBL,  11 15 0 STR,  3 15 8 STR,  edone B,
   ehit LBL,  16 15 8 LDR,  16 16 3 ADD,  16 15 8 STR,
   edone LBL,  RET, ;

\ x23 = SP off the main stack: x24 = the scan's end, ARN-WALK cells above SP
\ or the end of SP's 4 KiB block, whichever is lower. Clobbers x10.
: C-PROF-BLOCK-END ( -- )
   10 8 ARN-WALK LDR,  10 10 3 LSLI,  24 23 10 ADD,
   10 23 PROF-PAGE-MASK ORRI,  10 10 1 ADDI,
   24 10 CMP,  24 24 10 C-LS CSEL, ;

\ x8 = arena, x6 = an index entry, x10 = its record index, x3 = the sample's
\ weight: one inclusive sample for that record, and only the first time THIS
\ sample reaches it. The conservative walk can see one word twice - a recursive call, or a
\ stale spill slot still holding a return address into it - and an inclusive
\ count that ran past the sample total would say a word took longer than the
\ program did. The stamp is ARN-SEQ, which a weight above one does not move.
\ Clobbers x11-x13.
: C-PROF-INCL-ONCE ( -- )
   LBL {: seen :}
   11 ARN-STAMP LIT64,  11 8 11 ADD,  12 10 3 LSLI,  11 11 12 ADD,
   12 11 0 LDR,
   13 8 ARN-SEQ LDR,
   12 13 CMP,  C-EQ seen BCOND,
   13 11 0 STR,
   12 6 ENT-INCL LDR,  12 12 3 ADD,  12 6 ENT-INCL STR,
   seen LBL, ;

\ x8 = arena, x9 = the interrupted pc, x21 = mcontext, x3 = the weight: keep one
\ sample whose code the index cannot name yet. Only the pc, the interrupted x30
\ and the weight are kept, at a fixed stride, so the handler still walks nothing here and allocates nothing; the
\ next report rebuilds the index and replays them. Clobbers x10-x14.
: C-PROF-DEFER ( -- )
   LBL LBL {: full done :}
   10 8 ARN-DEFER LDR,
   11 PROF-DEFER-SLOTS LIT64,  10 11 CMP,  C-CS full BCOND,
   12 ARN-DEF LIT64,  12 8 12 ADD,
   13 PROF-DEFER-ENT MOVZ,  13 10 13 MUL,  12 12 13 ADD,
   9 12 0 STR,
   30 14 C-PROF-CTX-X>R  14 12 8 STR,  3 12 16 STR,
   10 10 1 ADDI,  10 8 ARN-DEFER STR,
   done B,
   full LBL,  10 8 ARN-SPILL LDR,  10 10 3 ADD,  10 8 ARN-SPILL STR,
   done LBL, ;

: C-PROF-WALK ( -- )
   LBL LBL LBL LBL LBL {: wl wlr wdone wret wblock :}
   25 22 0 ADDI,                                     \ dedup seed: the sampled word
   19 0 MOVZ,                                        \ no caller edge recorded yet
   30 9 C-PROF-CTX-X>R
   9 9 4 SUBI,                                       \ the call site, not the return address
   10 8 ARN-LO LDR,  9 10 CMP,  C-CC wlr BCOND,
   10 8 ARN-HI LDR,  9 10 CMP,  C-CS wlr BCOND,
   LPROFFIND LABEL@ BL,
   6 wlr CBZ,
   10 6 ENT-IDX LDR,  10 25 CMP,  C-EQ wlr BCOND,    \ x30 still points inside the sample
   C-PROF-INCL-ONCE
   25 10 0 ADDI,  19 1 MOVZ,
   11 8 ARN-FRAMES LDR,  11 11 1 ADDI,  11 8 ARN-FRAMES STR,
   LPROFEDGE LABEL@ BL,
   wlr LBL,
   31 23 C-PROF-CTX-X>R                              \ the interrupted machine stack
   10 8 ARN-STACK-LOW LDR,  23 10 CMP,  C-CC wblock BCOND,
   24 8 ARN-STACK-TOP LDR,  23 24 CMP,  C-CC wl BCOND,
   wblock LBL,  C-PROF-BLOCK-END                     \ not the main stack
   wl LBL,
      23 24 CMP,  C-CS wdone BCOND,
      9 23 0 LDR,  23 23 8 ADDI,
      10 8 ARN-LO LDR,  9 10 CMP,  C-CC wl BCOND,
      10 8 ARN-HI LDR,  9 10 CMP,  C-CS wl BCOND,
      9 9 4 SUBI,
      LPROFFIND LABEL@ BL,
      6 wl CBZ,
      10 6 ENT-IDX LDR,  10 25 CMP,  C-EQ wl BCOND,  \ the frame below named the same word
      C-PROF-INCL-ONCE
      25 10 0 ADDI,
      11 8 ARN-FRAMES LDR,  11 11 1 ADDI,  11 8 ARN-FRAMES STR,
      19 wl CBNZ,                                    \ the immediate caller is already in
      19 1 MOVZ,
      LPROFEDGE LABEL@ BL,
      wl B,
   wdone LBL,
   19 wret CBNZ,                                     \ nothing named a caller: say so
   10 PROF-CALLER-NONE LIT64,  LPROFEDGE LABEL@ BL,
   wret LBL, ;

: C-PROF-SAY ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: skip txt :}
   skip B,
   txt LBL,  a u BYTES,
   skip LBL,
   0 1 MOVZ,  1 txt ADR,  2 u MOVZ,  NR-WRITE SYS, ;

: C-PROF-BYTE ( n -- ) {: c:n :}
   SP SP 16 SUBI,  13 c MOVZ,  13 SP 0 STRB,
   0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   SP SP 16 ADDI, ;

: C-PROF-NL ( -- )  s\" \n" C-PROF-SAY ;
: C-PROF-DQ ( -- )  s\" \q" C-PROF-SAY ;

: C-PROF-FIELD ( ptr u8 n n -- ) {: a:ptr u:n reg:n :}
   a u C-PROF-SAY
   9 reg 0 ADDI,  10 1 MOVZ,  LPROFNUM LABEL@ BL, ;

\ LPROFNUM ( x9 = value, x10 = width ): unsigned decimal, right-aligned in x10
\ columns and never truncated, no newline. Clobbers x0-x2, x9-x15.
: EMIT-PROFNUM ( -- )
   LPROFNUM LABEL@ LBL,
   LBL LBL LBL {: dl pl pd :}
   15 10 0 ADDI,
   SP SP 48 SUBI,
   12 SP 48 ADDI,
   11 10 MOVZ,
   dl LBL,
      14 9 11 UDIV,  13 14 11 MUL,  13 9 13 SUB,
      13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
      9 14 0 ADDI,  9 dl CBNZ,
   10 SP 48 ADDI,  10 10 12 SUB,
   15 15 10 SUB,
   pl LBL,
      15 0 CMPI,  C-LE pd BCOND,
      13 $20 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
      15 15 1 SUBI,  pl B,
   pd LBL,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 48 ADDI,  2 2 12 SUB,  NR-WRITE SYS,
   SP SP 48 ADDI,  RET, ;

\ LPROFPCT ( x9 = count, x10 = total ): the share as "100.0", one decimal, right
\ aligned in six columns. A zero total prints 0.0 rather than dividing by it.
: EMIT-PROFPCT ( -- )
   LPROFPCT LABEL@ LBL,
   LBL LBL {: zero go :}
   SP SP 16 SUBI,  30 SP 0 STR,
   10 zero CBZ,
   11 1000 MOVZ,  9 9 11 MUL,  9 9 10 UDIV,
   go B,
   zero LBL,  9 0 MOVZ,
   go LBL,
   12 10 MOVZ,  13 9 12 UDIV,  14 13 12 MUL,  14 9 14 SUB,
   14 SP 8 STR,
   9 13 0 ADDI,  10 4 MOVZ,  LPROFNUM LABEL@ BL,
   $2E C-PROF-BYTE
   9 SP 8 LDR,  10 1 MOVZ,  LPROFNUM LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LPROFNAME ( x5 = profile row ): the complete copied qualified name.
: EMIT-PROFNAME ( -- )
   LPROFNAME LABEL@ LBL,
   0 1 MOVZ,  1 5 RID-NAME LDR,  2 5 RID-LEN LDR,  NR-WRITE SYS,
   RET, ;

\ LPROFDUMP / LPROFJSON ( -- ): the same walk, printed two ways.
\
\ REGISTERS. LPROFNUM and LPROFPCT clobber x9-x15, so every value that has to
\ live across a printed column sits in x3-x7, x17 or x19-x25: x19 arena, x7
\ band until the header has printed, x21 the row's index entry, x22 samples,
\ x23/x24 the (count, entry) threshold the next row must fall under, x25 the
\ row rank, x7 the row's exclusive count, x17 its record index, x6/x3 the
\ caller threshold and rank. x20 is never written: a fault here must find the
\ caller's DATA in it. x17, x19 and x21-x25 are saved and restored because the
\ compiled callers own them.
: C-PROF-REP-OPEN ( -- )
   SP SP 96 SUBI,
   30 SP 0 STR,  19 SP 8 STR,  21 SP 24 STR,  22 SP 32 STR,
   23 SP 40 STR,  24 SP 48 STR,  25 SP 56 STR,  17 SP 64 STR, ;

: C-PROF-REP-CLOSE ( -- )
   30 SP 0 LDR,  19 SP 8 LDR,  21 SP 24 LDR,  22 SP 32 LDR,
   23 SP 40 LDR,  24 SP 48 LDR,  25 SP 56 LDR,  17 SP 64 LDR,
   SP SP 96 ADDI,  RET, ;

\ x0 = arena, and the clock stopped. Empty every entry's inclusive count into the
\ per-record array, so the rebuild that follows can move the entries freely.
: C-PROF-INCL-FOLD ( -- )
   LBL LBL LBL {: fl fdone fnext :}
   8 0 0 ADDI,
   12 ARN-IDX LIT64,  12 8 12 ADD,
   13 8 ARN-COUNT LDR,  13 13 5 LSLI,  13 12 13 ADD,
   fl LBL,
      12 13 CMP,  C-CS fdone BCOND,
      14 12 ENT-INCL LDR,  14 fnext CBZ,
      10 12 ENT-IDX LDR,
      11 ARN-INCL LIT64,  11 8 11 ADD,  15 10 3 LSLI,  11 11 15 ADD,
      15 11 0 LDR,  15 15 14 ADD,  15 11 0 STR,
      14 0 MOVZ,  14 12 ENT-INCL STR,
      fnext LBL,  12 12 PROF-ENT ADDI,  fl B,
   fdone LBL, ;

\ x11 = the count for profile ID x12. Clobbers x10.
: C-PROF-RANK ( bool -- ) {: incl:bool :}
   incl IF 11 ARN-INCL LIT64,  11 19 11 ADD,
   ELSE 11 PROF-CNT-VA LIT64, THEN
   10 12 3 LSLI,  11 11 10 ADD,  11 11 0 LDR, ;

\ The report walks every stable profile ID, including reclaimed code. Folding
\ first gives each identity one inclusive count independent of index lifetime.
: C-PROF-REP-WORDSUM ( -- )
   LBL LBL {: turn done :}
   4 0 MOVZ,  3 0 MOVZ,  12 0 MOVZ,
   13 19 ARN-IDS LDR,
   turn LBL,
      12 13 CMP,  C-CS done BCOND,
      false C-PROF-RANK
      4 4 11 ADD,  14 11 0 ADDI,
      true C-PROF-RANK
      14 14 11 ORR,
      LBL {: next :}
      14 next CBZ,  3 3 1 ADDI,
      next LBL,  12 12 1 ADDI,  turn B,
   done LBL, ;

\ The header line names every field: samples (sampling intervals elapsed, the
\ unit of every count below but frames), words (ticks in a dictionary word, the sum of
\ the exclusive column), other (Habu code in no word:
\ the main loop, engine helpers, gaps between spans), new (deferred ticks whose pc
\ no live record owns), defer (ticks in code compiled after prof-on, still
\ unnamed), spill (deferred ticks the buffer had no room for), foreign (ticks
\ without the engine's DATA and dictionary registers charged to no word; a
\ foreign leaf that keeps both lands in other), foreign charged to callers
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
   LBL LBL {: dl dd :}
   10 0 MOVZ,  11 19 ARN-DEFER LDR,
   12 ARN-DEF LIT64,  12 19 12 ADD,
   dl LBL,  11 dd CBZ,
      13 12 16 LDR,  10 10 13 ADD,
      12 12 PROF-DEFER-ENT ADDI,  11 11 1 SUBI,  dl B,
   dd LBL, ;

: C-PROF-REP-HEAD ( bool -- ) {: json:bool :}
   json IF s\" {\"samples\":" ELSE s" profiler samples " THEN 22 C-PROF-FIELD
   json IF s\" ,\"words\":" ELSE s"  words " THEN 4 C-PROF-FIELD
   10 7 PROF-OTHER LDR,
   json IF s\" ,\"other\":" ELSE s"  other " THEN 10 C-PROF-FIELD
   10 19 ARN-NEW LDR,
   json IF s\" ,\"new\":" ELSE s"  new " THEN 10 C-PROF-FIELD
   C-PROF-DEFER-WEIGHT
   json IF s\" ,\"defer\":" ELSE s"  defer " THEN 10 C-PROF-FIELD
   10 19 ARN-SPILL LDR,
   json IF s\" ,\"spill\":" ELSE s"  spill " THEN 10 C-PROF-FIELD
   10 7 PROF-FOREIGN LDR,
   json IF s\" ,\"foreign\":" ELSE s"  foreign " THEN 10 C-PROF-FIELD
   10 19 ARN-FCALLED LDR,
   json IF s\" ,\"foreign charged to callers\":"
   ELSE s"  foreign charged to callers " THEN 10 C-PROF-FIELD
   10 19 ARN-FRAMES LDR,
   json IF s\" ,\"frames\":" ELSE s"  frames " THEN 10 C-PROF-FIELD
   10 19 ARN-DROP LDR,
   json IF s\" ,\"dropped\":" ELSE s"  dropped " THEN 10 C-PROF-FIELD
   10 19 ARN-COUNT LDR,
   json IF s\" ,\"indexed\":" ELSE s"  indexed " THEN 10 C-PROF-FIELD
   10 19 ARN-USEC LDR,
   json IF s\" ,\"usec\":" ELSE s"  usec " THEN 10 C-PROF-FIELD
   json IF s\" ,\"attributed\":" ELSE s"  attributed " THEN 3 C-PROF-FIELD
   json IF exit THEN
   C-PROF-NL ;

\ x21 is the stable profile ID selected for this row.
: C-PROF-ROW-INCL ( -- )
   10 ARN-INCL LIT64,  10 19 10 ADD,
   9 21 3 LSLI,  9 10 9 ADD,  9 9 0 LDR, ;

: C-PROF-REP-ROW-REC ( -- )
   17 21 0 ADDI,
   5 ARN-RECS LIT64,  5 19 5 ADD,
   10 PROF-ID-ENT MOVZ,  10 17 10 MUL,  5 5 10 ADD, ;

: C-PROF-REP-ROW ( -- )
   9 7 0 ADDI,  10 8 MOVZ,  LPROFNUM LABEL@ BL,
   9 7 0 ADDI,  10 22 0 ADDI,  LPROFPCT LABEL@ BL,
   C-PROF-ROW-INCL  10 8 MOVZ,  LPROFNUM LABEL@ BL,
   C-PROF-ROW-INCL  10 22 0 ADDI,  LPROFPCT LABEL@ BL,
   s"   " C-PROF-SAY
   C-PROF-REP-ROW-REC
   LPROFNAME LABEL@ BL,
   C-PROF-NL ;

\ x5 = the record behind a caller index in x9, or 0 for PROF-CALLER-NONE, which
\ stands for a sample the walk could not attribute and owns no dictionary row.
: C-PROF-CALLER-REC ( -- )
   LBL LBL {: none done :}
   11 PROF-CALLER-NONE LIT64,  9 11 CMP,  C-EQ none BCOND,
   5 ARN-RECS LIT64,  5 19 5 ADD,
   11 PROF-ID-ENT MOVZ,  11 9 11 MUL,  5 5 11 ADD,
   done B,
   none LBL,  5 0 MOVZ,
   done LBL, ;

: C-PROF-CALLER-NAME ( -- )
   LBL LBL {: none done :}
   5 none CBZ,
   LPROFNAME LABEL@ BL,
   done B,
   none LBL,  s" (unknown)" C-PROF-SAY
   done LBL, ;

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
   10 SP 72 STR,
   s"          <- " C-PROF-SAY
   9 SP 72 LDR,  10 8 MOVZ,  LPROFNUM LABEL@ BL,
   9 SP 72 LDR,  10 SP 88 LDR,  LPROFPCT LABEL@ BL,
   s"  " C-PROF-SAY
   C-PROF-CALLER-NAME
   C-PROF-NL ;

: C-PROF-REP-CALLERS ( -- )
   LBL LBL LBL LBL LBL LBL {: cr cl cnext cdone ctake crd :}
   3 0 MOVZ,
   6 0 MOVN,                                       \ threshold: nothing printed yet
   cr LBL,
      3 PROF-CALLERS CMPI,  C-CS crd BCOND,
      15 0 MOVZ,  16 0 MOVZ,
      11 ARN-CALL LIT64,  11 19 11 ADD,
      12 PROF-CALL-SLOTS LIT64,
      cl LBL,
         12 cdone CBZ,
         13 11 0 LDR,  13 cnext CBZ,
         13 13 1 SUBI,  10 13 PROF-REC-BITS LSRI,  10 17 CMP,  C-NE cnext BCOND,
         13 13 PROF-REC-MASK ANDI,
         9 11 8 LDR,
         9 6 CMP,  C-CS cnext BCOND,               \ at or above the threshold: printed
         9 15 CMP,  C-LS cnext BCOND,
         15 9 0 ADDI,  16 13 0 ADDI,
         cnext LBL,  11 11 PROF-CALL-ENT ADDI,  12 12 1 SUBI,  cl B,
      cdone LBL,
      15 crd CBZ,                                  \ no caller edge left
      6 15 0 ADDI,
      9 16 0 ADDI,  10 15 0 ADDI,
      C-PROF-REP-CALLER
      3 3 1 ADDI,  cr B,
   crd LBL, ;

: C-PROF-REP-SECTION ( bool -- ) {: incl:bool :}
   LBL LBL LBL LBL LBL LBL LBL {: rowl sell selcmp selnx seld seltake rowsd :}
   23 0 MOVN,  24 0 MOVN,  25 0 MOVZ,
   rowl LBL,
      7 0 MOVZ,  21 0 MOVN,
      12 0 MOVZ,  13 19 ARN-IDS LDR,
      sell LBL,
         12 13 CMP,  C-CS seld BCOND,
         incl C-PROF-RANK
         11 selnx CBZ,
         11 23 CMP,  C-HI selnx BCOND,             \ above the threshold: already printed
         C-NE selcmp BCOND,
         12 24 CMP,  C-LS selnx BCOND,             \ the threshold row itself, or before it
         selcmp LBL,
         11 7 CMP,  C-HI seltake BCOND,
         C-NE selnx BCOND,
         12 21 CMP,  C-CS selnx BCOND,             \ same count, later entry: keep the first
         seltake LBL,
         7 11 0 ADDI,  21 12 0 ADDI,
         selnx LBL,  12 12 1 ADDI,  sell B,
      seld LBL,
      7 rowsd CBZ,                                 \ no counted row left
      23 7 0 ADDI,  24 21 0 ADDI,
      17 21 0 ADDI,
      7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,  \ the row prints exclusive
      incl IF C-PROF-ROW-INCL ELSE 9 7 0 ADDI, THEN  9 SP 88 STR,
      C-PROF-REP-ROW
      C-PROF-REP-CALLERS
      25 25 1 ADDI,
      25 PROF-ROWS CMPI,  C-CC rowl BCOND,
   rowsd LBL, ;

\ EVERY ATTRIBUTED WORD, not a top-N. A phase word - a compiler pass, a verifier
\ entry - is often invisible in an exclusive ranking and is exactly what a
\ per-phase table needs, so the machine-readable report carries the whole set:
\ one object per profile ID that took a sample, and the caller edges as a flat
\ array rather than nested under each row. Nesting them would mean one pass over
\ the edge table per row - quadratic, and this walk runs over every row - while
\ one pass emits every edge there is. Both are bounded by the arena, at most
\ ARN-IDS rows and PROF-CALL-SLOTS edges, so nothing is truncated.
: C-PROF-JSON-ROWS ( -- )
   LBL LBL LBL LBL {: jl jdone jnext jcomma :}
   s\" ,\"rows\":[" C-PROF-SAY
   25 0 MOVZ,
   21 0 MOVZ,  23 19 ARN-IDS LDR,
   jl LBL,
      21 23 CMP,  C-CS jdone BCOND,
      17 21 0 ADDI,
      7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,
      C-PROF-ROW-INCL
      10 7 9 ORR,  10 jnext CBZ,                   \ neither count moved: not a row
      25 jcomma CBZ,  s" ," C-PROF-SAY
      jcomma LBL,  25 1 MOVZ,
      s\" {\"word\":\"" C-PROF-SAY
      C-PROF-REP-ROW-REC
      LPROFNAME LABEL@ BL,
      C-PROF-DQ  s\" ,\"excl\":" C-PROF-SAY
      9 7 0 ADDI,  10 1 MOVZ,  LPROFNUM LABEL@ BL,
      s\" ,\"incl\":" C-PROF-SAY
      C-PROF-ROW-INCL  10 1 MOVZ,  LPROFNUM LABEL@ BL,
      s" }" C-PROF-SAY
   jnext LBL,  21 21 1 ADDI,  jl B,
   jdone LBL,
   s" ]" C-PROF-SAY ;

\ x9 = a record index, or PROF-CALLER-NONE: its name as a JSON string body.
: C-PROF-JSON-NAME ( -- )
   C-PROF-CALLER-REC
   C-PROF-CALLER-NAME ;

: C-PROF-JSON-EDGES ( -- )
   LBL LBL LBL LBL {: el edone enext ecomma :}
   s\" ,\"edges\":[" C-PROF-SAY
   25 0 MOVZ,
   21 ARN-CALL LIT64,  21 19 21 ADD,
   23 PROF-CALL-SLOTS LIT64,
   el LBL,
      23 edone CBZ,
      17 21 0 LDR,  17 enext CBZ,
      17 17 1 SUBI,
      7 17 PROF-REC-BITS LSRI,                     \ the sampled word
      17 17 PROF-REC-MASK ANDI,                    \ its caller
      10 21 8 LDR,  10 SP 72 STR,
      25 ecomma CBZ,  s" ," C-PROF-SAY
      ecomma LBL,  25 1 MOVZ,
      s\" {\"word\":\"" C-PROF-SAY
      9 7 0 ADDI,  C-PROF-JSON-NAME
      C-PROF-DQ  s\" ,\"caller\":\"" C-PROF-SAY
      9 17 0 ADDI,  C-PROF-JSON-NAME
      C-PROF-DQ  s\" ,\"n\":" C-PROF-SAY
      9 SP 72 LDR,  10 1 MOVZ,  LPROFNUM LABEL@ BL,
      s" }" C-PROF-SAY
   enext LBL,  21 21 PROF-CALL-ENT ADDI,  23 23 1 SUBI,  el B,
   edone LBL,
   s\" ]}\n" C-PROF-SAY ;

: EMIT-PROFREP ( bool -- ) {: json:bool :}
   json IF LPROFJSON LABEL@ ELSE LPROFDUMP LABEL@ THEN LBL,
   LBL {: armed :}
   C-PROF-REP-OPEN
   7 PROF-BAND-VA LIT64,
   22 7 PROF-TOT LDR,
   19 7 PROF-ARENA LDR,
   19 armed CBNZ,
      json IF s\" {\"samples\":0,\"armed\":false}\n" ELSE s\" profiler not armed\n" THEN C-PROF-SAY
      C-PROF-REP-CLOSE
   armed LBL,
   0 19 0 ADDI,  C-PROF-INCL-FOLD
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
   LPROFROW LABEL@ LBL,
   LBL {: rnone :}
   C-PROF-REP-OPEN                                 \ x6 = the profile ID requested
   7 PROF-BAND-VA LIT64,
   22 7 PROF-TOT LDR,
   19 7 PROF-ARENA LDR,
   19 rnone CBZ,
   6 0 CMPI,  C-LT rnone BCOND,
   23 19 ARN-IDS LDR,  6 23 CMP,  C-CS rnone BCOND,
   21 6 0 ADDI,
   C-PROF-REP-ROW-REC
   10 5 RID-START LDR,  10 rnone CBZ,
   0 19 0 ADDI,  C-PROF-INCL-FOLD
   17 21 0 ADDI,
   7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,
   C-PROF-ROW-INCL  9 SP 88 STR,
   C-PROF-REP-ROW
   C-PROF-REP-CALLERS
   C-PROF-REP-CLOSE
   rnone LBL,
   s" profiler: no row for that record" C-PROF-SAY  C-PROF-NL
   C-PROF-REP-CLOSE ;

\ x8 = arena, on a thread that is not ARN-OWNER: SIGALRM to ARN-OWNER-TID. An
\ owner that exited leaves a handle the kernel can give this thread; that
\ delivery is dropped: a signal to itself would arrive again at its sigreturn,
\ and the thread would never run its own code.
\ Clobbers x0-x2, x8-x11 and x16.
: C-PROF-FORWARD ( -- )
   LBL {: fself :}
   10 8 ARN-OWNER-TID LDR,
   9 OS-THREAD-HANDLE
   9 10 CMP,  C-EQ fself BCOND,
   11 SIGALRM MOVZ,  11 10 OS-SIGNAL-THREAD
   fself LBL,  9 OS-THREAD-RELEASE ;

\ x7 = band: x3 = this sample's weight, as the file header states; ARN-LAST
\ moves to now, ARN-CARRY keeps the remainder and ARN-SEQ counts the sample,
\ whatever its weight. Plain loads and stores suffice: only the owner's handler
\ reaches here, and a word that leaves ARMED-RUNS waits for it. The ISB keeps
\ the timer clock from being read before the admission's load-acquire, ahead of
\ the ARN-LAST a resume wrote.
\ Clobbers x8 and x10-x12.
: C-PROF-WEIGHT ( -- )
   LBL {: wdone :}
   3 1 MOVZ,
   8 7 PROF-ARENA LDR,  8 wdone CBZ,
   ISB,  10 OS-TIMER-NOW
   11 8 ARN-LAST LDR,  10 8 ARN-LAST STR,
   12 8 ARN-SEQ LDR,  12 12 1 ADDI,  12 8 ARN-SEQ STR,
   12 8 ARN-TICKS LDR,  12 wdone CBZ,
   10 10 11 SUB,  11 8 ARN-CARRY LDR,  10 10 11 ADD,
   3 10 12 UDIV,  11 3 12 MUL,  10 10 11 SUB,
   10 8 ARN-CARRY STR,
   wdone LBL, ;

\ x8 = arena, x21 = mcontext, a foreign sample: x6 = the index entry of the
\ Habu word nearest it, or 0 when neither x30 nor the scan names one. Off the
\ main stack, a task owner's stack among them, the scan stays in SP's block.
\ x30 and each scanned cell are tested as C-PROF-WALK tests them.
\ Clobbers x9-x13, x23 and x24.
: C-PROF-FCALLER ( -- )
   LBL LBL LBL LBL LBL {: fl fnone fdone fblock fx30 :}
   31 23 C-PROF-CTX-X>R
   10 8 ARN-STACK-LOW LDR,  23 10 CMP,  C-CC fblock BCOND,
   24 8 ARN-STACK-TOP LDR,  23 24 CMP,  C-CC fx30 BCOND,
   fblock LBL,  C-PROF-BLOCK-END
   fx30 LBL,
   30 9 C-PROF-CTX-X>R
   9 9 4 SUBI,
   10 8 ARN-LO LDR,  9 10 CMP,  C-CC fl BCOND,
   10 8 ARN-HI LDR,  9 10 CMP,  C-CS fl BCOND,
   LPROFFIND LABEL@ BL,
   6 fdone CBNZ,
   fl LBL,
      23 24 CMP,  C-CS fnone BCOND,
      9 23 0 LDR,  23 23 8 ADDI,
      10 8 ARN-LO LDR,  9 10 CMP,  C-CC fl BCOND,
      10 8 ARN-HI LDR,  9 10 CMP,  C-CS fl BCOND,
      9 9 4 SUBI,
      LPROFFIND LABEL@ BL,
      6 fl CBZ,
      fdone B,
   fnone LBL,  6 0 MOVZ,
   fdone LBL, ;

\ x7 = band, x8 = arena, x6 = an index entry, x3 = the weight: the sample is
\ that word's own, exclusive and inclusive; x22 = its record for the walk.
\ Clobbers x10-x14.
: C-PROF-CHARGE ( -- )
   12 6 ENT-IDX LDR,
   14 PROF-CNT-VA LIT64,  13 12 3 LSLI,  14 14 13 ADD,
   12 14 0 LDR,  12 12 3 ADD,  12 14 0 STR,
   10 6 ENT-IDX LDR,  C-PROF-INCL-ONCE                \ a word is inside itself
   22 6 ENT-IDX LDR, ;

\ x7 = band: PROF-IN += n, 1 or -1, by an acquire-release atomic add.
\ Clobbers x9 and x10.
: C-PROF-IN-ADD ( n -- ) {: n:n :}
   n 0 < IF 9 0 MOVN, ELSE 9 n MOVZ, THEN
   10 7 PROF-IN ADDI,  9 9 10 LDADDAL, ;

\ THE HANDLER TAKES NOTHING FROM THE LIVE REGISTERS: a foreign callee (libc's
\ dlsym, libzip's inflate) reuses x20 (DATA), x26 (DBASE) and x27 (NDICT).
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
   LPROFH LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL {: pnew pother pdone pforeign plost psig pexit :}
   LBL LBL LBL {: powner pfwd pleave :}
   7 PROF-BAND-VA LIT64,
   9 7 PROF-ARMED ADDI,  9 9 LDAR,  9 ARMED-RUNS CMPI,  C-NE pexit BCOND,
   1 C-PROF-IN-ADD
   9 7 PROF-ARMED ADDI,  9 9 LDAR,  9 ARMED-RUNS CMPI,  C-NE pleave BCOND,
   8 7 PROF-ARENA LDR,  8 powner CBZ,
   10 CB-SELF,  11 8 ARN-OWNER LDR,  10 11 CMP,  C-NE pfwd BCOND,
   powner LBL,
   C-PROF-WEIGHT
   3 pleave CBZ,
   C-PROF-MCTX>R21  C-PROF-PC>R9
   17 1 MOVZ,                                        \ x17 = 1: a Habu context, its count may report (x16 is the Darwin syscall number, never kept across a sys)
   8 7 PROF-ARENA LDR,  8 pdone CBZ,
   20 10 C-PROF-CTX-X>R  11 8 ARN-OWNER-DATA LDR,  10 11 CMP,  C-NE pforeign BCOND,
   26 10 C-PROF-CTX-X>R  11 7 PROF-DBASE LDR,  10 11 CMP,  C-NE pforeign BCOND,
   LPROFFIND LABEL@ BL,                            \ the handler never returns, so x30 needs no frame
   6 pnew CBZ,
   C-PROF-CHARGE
   C-PROF-WALK
   psig B,
   pnew LBL,
   22 0 MOVN,                                        \ no record of its own: callers only
   11 8 ARN-HI LDR,  9 11 CMP,  C-CC pother BCOND,   \ below the index's high mark: engine helpers, main loop, a gap
   C-PROF-DEFER                                      \ at or above it: compiled after prof-on built the index
   C-PROF-WALK
   psig B,
   pother LBL,
   12 7 PROF-OTHER LDR,  12 12 3 ADD,  12 7 PROF-OTHER STR,
   C-PROF-WALK
   psig B,
   pdone LBL,
   12 7 PROF-OTHER LDR,  12 12 3 ADD,  12 7 PROF-OTHER STR,
   psig B,
   pforeign LBL,
   17 0 MOVZ,                                        \ not Habu code: no report from here
   8 7 PROF-ARENA LDR,  8 plost CBZ,
   C-PROF-FCALLER
   6 plost CBZ,
   12 8 ARN-FCALLED LDR,  12 12 3 ADD,  12 8 ARN-FCALLED STR,
   C-PROF-CHARGE
   C-PROF-WALK
   psig B,
   plost LBL,
   12 7 PROF-FOREIGN LDR,  12 12 3 ADD,  12 7 PROF-FOREIGN STR,
   psig LBL,
   10 7 PROF-TOT LDR,  10 10 3 ADD,  10 7 PROF-TOT STR,
   11 7 PROF-LIM LDR,  11 pleave CBZ,                \ limit 0: sample until prof-off
   10 11 CMP,  C-LT pleave BCOND,
   \ the limit reached on a foreign sample: the next Habu sample that weighs
   \ an interval reports
   17 pleave CBZ,
   LPROFDUMP LABEL@ BL,
   7 PROF-BAND-VA LIT64,  -1 C-PROF-IN-ADD
   0 PROF-LIMIT-RC MOVZ,  NR-EXIT-GROUP SYS,
   pfwd LBL,  C-PROF-FORWARD
   pleave LBL,  -1 C-PROF-IN-ADD
   pexit LBL,  0 4 0 ADDI,  NR-SIGRETURN SYS,  BRK, ;

private

: C-PROF-SIGACTION-FRAME ( -- )
   SP SP 32 SUBI,
   9 LPROFH LABEL@ ADR,  9 SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-PROF-FLAGS LIT64,  10 SP 8 STR,
      10 0 MOVZ,  10 SP 16 STR,  10 SP 24 STR,
      exit
   THEN
   9 SP 8 STR,
   10 MACOS-SA-PROF-FLAGS MOVZ,  10 10 32 LSLI,  10 SP 16 STR, ;

: C-PROF-SIGACTION ( -- )
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

: C-PROF-SIGACTION-DONE ( -- )
   SP SP 32 ADDI, ;

\ x0 = arena: x10 = the interval prof-rate left there, or the 1000 us default,
\ written back so the report states the rate it actually sampled at.
: C-PROF-USEC>R10 ( -- )
   LBL {: dflt :}
   10 0 ARN-USEC LDR,  10 dflt CBNZ,  10 1000 MOVZ,
   dflt LBL,  10 0 ARN-USEC STR, ;

\ x0 = arena: ARN-TICKS = the interval in the timer clock's ticks, at least 1,
\ and ARN-LAST = now. Run before PROF-ARMED goes to ARMED-RUNS, so a sample
\ never weighs time the clock was held or stopped. Clobbers x10-x12.
: C-PROF-CLOCK-MARK ( -- )
   LBL {: tk :}
   C-PROF-USEC>R10
   11 OS-TIMER-HZ  10 10 11 MUL,
   11 USEC-PER-SEC LIT64,  10 10 11 UDIV,
   10 tk CBNZ,  10 1 MOVZ,
   tk LBL,  10 0 ARN-TICKS STR,
   10 OS-TIMER-NOW  10 0 ARN-LAST STR, ;

\ x0 = arena: the interval split for setitimer. The signed divide keeps an
\ interval below 0 negative, which C-PROF-TIMER's arm refuses rather than
\ reading it as years. Clobbers x9-x11.
: C-PROF-TIMER-FRAME ( -- )
   C-PROF-USEC>R10
   11 USEC-PER-SEC LIT64,  9 10 11 SDIV,               \ x9 = seconds
   11 9 11 MUL,  10 10 11 SUB,                        \ x10 = microseconds
   SP SP 32 SUBI,
   9 SP 0 STR,  10 SP 8 STR,
   9 SP 16 STR,  10 SP 24 STR, ;

\ A refused setitimer is named and fatal: the phase would sample nothing and
\ report zeros.
: C-PROF-TIMER ( -- )
   LBL LBL {: ok msg :}
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS,
   C-CC ok BCOND,
      1 msg ADR,  2 PROFARMMSG$ nip MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
      0 PROF-MAP-RC MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  PROFARMMSG$ BYTES,
   ok LBL, ;

: C-PROF-TIMER-DONE ( -- )
   SP SP 32 ADDI, ;

\ x0 = arena, a C-PROF-TIMER-FRAME frame on SP: its first expiry becomes the
\ interval less ARN-CARRY, the carry in microseconds by the ratio of ARN-USEC
\ to ARN-TICKS, or 1 us once the carry reaches a whole interval. Clobbers
\ x9-x11.
: C-PROF-TIMER-REST ( -- )
   LBL LBL {: whole split :}
   9 0 ARN-CARRY LDR,  10 0 ARN-TICKS LDR,  11 0 ARN-USEC LDR,
   9 10 CMP,  C-CS whole BCOND,
   9 9 11 MUL,  9 9 10 UDIV,  9 11 9 SUB,
   split B,
   whole LBL,  9 1 MOVZ,
   split LBL,
   11 USEC-PER-SEC LIT64,  10 9 11 UDIV,
   11 10 11 MUL,  9 9 11 SUB,
   10 SP 16 STR,  9 SP 24 STR, ;

: C-PROF-ARENA-MAP ( -- )
   LBL LBL LBL {: have mok amsg :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARENA LDR,  9 have CBNZ,
      0 0 MOVZ,  1 ARN-BYTES LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      C-CC mok BCOND,
         1 amsg ADR,  2 PROFARNMSG$ nip MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
         0 PROF-MAP-RC MOVZ,  NR-EXIT-GROUP SYS,
      amsg LBL,  PROFARNMSG$ BYTES,
      mok LBL,
      7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA STR,
   have LBL, ;

: C-PROF-DIE ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: msg :}
   0 2 MOVZ,  1 msg ADR,  2 u LIT64,  NR-WRITE SYS,
   0 PROF-MAP-RC MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  a u BYTES, ;

\ x5 = dictionary record; x1/x2 = raw name pointer/length. The external
\ pointer is read while the record is live, then its exact bytes are copied.
: C-PROF-RAW-NAME ( -- )
   LBL {: inl :}
   1 5 24 ADDI,  2 5 16 LDR,
   0 2 DNAME-EXT ANDI,  0 inl CBZ,
   1 5 24 LDR,
   inl LBL,  2 2 DNAME-FLAG-BITS LSLI,  2 2 DNAME-FLAG-BITS LSRI, ;

\ x1 = destination, x2 = source, x3 = byte count. Advance the destination.
: C-PROF-COPY-NAME ( -- )
   LBL LBL {: cp done :}
   cp LBL,  3 done CBZ,
      0 2 0 LDRB,  0 1 0 STRB,
      1 1 1 ADDI,  2 2 1 ADDI,  3 3 1 SUBI,  cp B,
   done LBL, ;

\ x17 = arena. Build WID -> namespace row in the sort scratch; keep x6 and x17.
\ A namespace has at most two WIDs, so the cell table remains at most half full.
: C-PROF-QUAL-INDEX ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: clear cleared recs next insert probe store keydone :}
   LBL {: done :}
   13 ARN-SCR LIT64,  13 17 13 ADD,
   14 13 0 ADDI,  15 ARN-IDX-BYTES LIT64,  15 13 15 ADD,
   0 0 MOVZ,
   clear LBL,
      14 15 CMP,  C-CS cleared BCOND,
      0 14 0 STR,  14 14 CELL ADDI,  clear B,
   cleared LBL,
   4 0 MOVZ,  5 DBASE 0 ADDI,
   recs LBL,
      4 NDICT CMP,  C-CS done BCOND,
      0 5 DICT-WL-OFF LDR,  0 0 1 ADDI,  0 next CBNZ,
      0 5 0 LDR,  7 5 CELL LDR,
      insert LBL,
         0 keydone CBZ,
         9 PROF-HASH LIT64,  9 0 9 MUL,  9 9 PROF-QUAL-MASK ANDI,
      probe LBL,
         10 9 3 LSLI,  10 13 10 ADD,
         11 10 0 LDR,  11 store CBZ,
         12 11 $FFFFFFFF ANDI,  12 0 CMP,  C-EQ keydone BCOND,
         9 9 1 ADDI,  9 9 PROF-QUAL-MASK ANDI,  probe B,
      store LBL,
         11 4 1 ADDI,  11 11 PROF-QUAL-BITS LSLI,  11 11 0 ORR,
         11 10 0 STR,
      keydone LBL,
         0 7 0 ADDI,  7 0 MOVZ,  0 insert CBNZ,
   next LBL,  4 4 1 ADDI,  5 5 DREC ADDI,  recs B,
   done LBL, ;

\ x0 = arena. Assign a stable ID to each newly live ordinal and copy its
\ qualified name into session-owned mmap chunks. The live map is -1 for an
\ unassigned ordinal. Prepare namespace
\ lookup only when new IDs need names; the later merge sort reuses its scratch.
\ Reports keep copied names.
: C-PROF-SNAPSHOT ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: recs next qscan qfound qdone usechunk newchunk :}
   LBL LBL LBL LBL LBL LBL LBL {: idok sized mapok qready owncopy copydone done :}
   LBL LBL {: pending prepare :}
   17 0 0 ADDI,  6 0 MOVZ,
   pending LBL,
      6 NDICT CMP,  C-CS done BCOND,
      15 ARN-LIVE LIT64,  15 17 15 ADD,  0 6 3 LSLI,  15 15 0 ADD,
      0 15 0 LDR,  1 0 MOVN,  0 1 CMP,  C-EQ prepare BCOND,
      6 6 1 ADDI,  pending B,
   prepare LBL,
   C-PROF-QUAL-INDEX
   recs LBL,
      6 NDICT CMP,  C-CS done BCOND,
      15 ARN-LIVE LIT64,  15 17 15 ADD,  0 6 3 LSLI,  15 15 0 ADD,
      0 15 0 LDR,  1 0 MOVN,  0 1 CMP,  C-NE next BCOND,
      10 17 ARN-IDS LDR,
      0 DICT-CAP LIT64,  10 0 CMP,  C-CC idok BCOND,
      PROFIDMSG$ C-PROF-DIE
      idok LBL,
      7 ARN-RECS LIT64,  7 17 7 ADD,
      0 PROF-ID-ENT MOVZ,  0 10 0 MUL,  7 7 0 ADD,
      5 DREC MOVZ,  5 6 5 MUL,  5 DBASE 5 ADD,
      14 0 MOVZ,                                  \ no qualifier yet
      0 5 DICT-WL-OFF LDR,
      0 0 CMPI,  C-LE qdone BCOND,
      13 ARN-SCR LIT64,  13 17 13 ADD,
      15 PROF-HASH LIT64,  15 0 15 MUL,  15 15 PROF-QUAL-MASK ANDI,
      qscan LBL,
         1 15 3 LSLI,  1 13 1 ADD,  1 1 0 LDR,  1 qdone CBZ,
         2 1 $FFFFFFFF ANDI,  0 2 CMP,  C-EQ qfound BCOND,
         15 15 1 ADDI,  15 15 PROF-QUAL-MASK ANDI,  qscan B,
      qfound LBL,
         14 1 PROF-QUAL-BITS LSRI,  14 14 1 SUBI,
         1 DREC MOVZ,  14 14 1 MUL,  14 DBASE 14 ADD,
      qdone LBL,
      C-PROF-RAW-NAME  9 1 0 ADDI,  10 2 0 ADDI,  \ own source and length
      11 0 MOVZ,  12 0 MOVZ,
      14 qready CBZ,
      5 14 0 ADDI,  C-PROF-RAW-NAME
      11 1 0 ADDI,  12 2 1 ADDI,                    \ qualifier plus ':'
      qready LBL,
      13 10 12 ADD,                               \ total exact name bytes
      14 17 ARN-CHUNKS LDR,
      14 newchunk CBZ,
      15 14 CHUNK-USED LDR,  0 15 13 ADD,
      1 14 CHUNK-LEN LDR,  0 1 CMP,  C-LS usechunk BCOND,
      newchunk LBL,
         1 13 CHUNK-DATA ADDI,
         15 PROF-NAME-CHUNK LIT64,  1 15 CMP,  C-CS sized BCOND,
         1 15 0 ADDI,
      sized LBL,
         15 1 0 ADDI,                              \ Darwin mmap clears x1; x15 keeps the requested extent
         0 0 MOVZ,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,
         4 0 MOVN,  5 0 MOVZ,  NR-MMAP SYS,
         C-CC mapok BCOND,
         PROFNAMEMSG$ C-PROF-DIE
      mapok LBL,
         14 0 0 ADDI,
         0 17 ARN-CHUNKS LDR,  0 14 CHUNK-NEXT STR,
         15 14 CHUNK-LEN STR,
         15 13 CHUNK-DATA ADDI,  15 14 CHUNK-USED STR,
         14 17 ARN-CHUNKS STR,
         4 14 CHUNK-DATA ADDI,  owncopy B,
      usechunk LBL,
         4 14 15 ADD,  15 15 13 ADD,  15 14 CHUNK-USED STR,
      owncopy LBL,
      6 7 RID-OWNER STR,  4 7 RID-NAME STR,  13 7 RID-LEN STR,
      0 0 MOVZ,  0 7 RID-START STR,  0 7 RID-END STR,
      1 4 0 ADDI,
      11 copydone CBZ,
      2 11 0 ADDI,  3 12 1 SUBI,  C-PROF-COPY-NAME
      0 $3A MOVZ,  0 1 0 STRB,  1 1 1 ADDI,
      copydone LBL,
      2 9 0 ADDI,  3 10 0 ADDI,  C-PROF-COPY-NAME
      15 ARN-LIVE LIT64,  15 17 15 ADD,  0 6 3 LSLI,  15 15 0 ADD,
      0 17 ARN-IDS LDR,  0 15 0 STR,
      0 0 1 ADDI,  0 17 ARN-IDS STR,
   next LBL,  6 6 1 ADDI,  recs B,
   done LBL,  0 17 0 ADDI, ;

: C-PROF-SESSION-START ( -- )
   LBL LBL LBL {: chunks clear done :}
   17 0 0 ADDI,
   9 17 ARN-CHUNKS LDR,
   chunks LBL,
      9 clear CBZ,
      10 9 CHUNK-NEXT LDR,
      0 9 0 ADDI,  1 9 CHUNK-LEN LDR,  NR-MUNMAP SYS,
      C-CC done BCOND,
      PROFNAMEFREEMSG$ C-PROF-DIE
      done LBL,
      9 10 0 ADDI,  chunks B,
   clear LBL,
   9 0 MOVZ,  9 17 ARN-CHUNKS STR,  9 17 ARN-IDS STR,
   10 ARN-LIVE LIT64,  10 17 10 ADD,
   11 DICT-CAP LIT64,  11 11 3 LSLI,  11 10 11 ADD,
   9 0 MOVN,
   LBL LBL {: zeroed turn :}
   turn LBL,
      10 11 CMP,  C-CS zeroed BCOND,
      9 10 0 STR,  10 10 8 ADDI,  turn B,
   zeroed LBL,  0 17 0 ADDI, ;

\ One index entry per profile ID with code of its own. A live ID takes its
\ current dictionary span; a seed-retired ID takes its stored span; any other
\ retired ID has no entry. The old index's inclusive counts are folded first.
: C-PROF-INDEX-BUILD ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: bl bnext bdone blo bhi retained span :}
   9 ARN-IDX LIT64,  9 0 9 ADD,
   11 0 MOVZ,  6 0 MOVZ,
   1 0 MOVN,  2 0 MOVZ,                              \ lo = all ones, hi = 0
   bl LBL,
      12 0 ARN-IDS LDR,  6 12 CMP,  C-CS bdone BCOND,
      5 ARN-RECS LIT64,  5 0 5 ADD,
      12 PROF-ID-ENT MOVZ,  12 6 12 MUL,  5 5 12 ADD,
      12 5 RID-OWNER LDR,  12 0 CMPI,  C-LT retained BCOND,
      15 0 MOVZ,  15 5 RID-START STR,  15 5 RID-END STR,
      13 DREC MOVZ,  13 12 13 MUL,  13 DBASE 13 ADD,
      14 13 DICT-WL-OFF LDR,  14 14 2 ADDI,  14 2 CMPI,  C-CC bnext BCOND,
      13 13 8 LDR,  C-PROF-SPAN-BYTES  13 bnext CBZ,
      12 5 RID-OWNER LDR,  15 DREC MOVZ,  12 12 15 MUL,
      12 DBASE 12 ADD,  12 12 0 LDR,  12 bnext CBZ,
      14 12 13 ADD,
      12 5 RID-START STR,  14 5 RID-END STR,  span B,
      retained LBL,
      12 5 RID-START LDR,  14 5 RID-END LDR,
      12 bnext CBZ,
      12 14 CMP,  C-CS bnext BCOND,
      span LBL,
      12 9 ENT-START STR,
      14 9 ENT-END STR,
      6 9 ENT-IDX STR,
      15 0 MOVZ,  15 9 ENT-INCL STR,
      9 9 PROF-ENT ADDI,  11 11 1 ADDI,
      12 1 CMP,  C-CS blo BCOND,  1 12 0 ADDI,
      blo LBL,  14 2 CMP,  C-LS bhi BCOND,  2 14 0 ADDI,
      bhi LBL,
   bnext LBL,  6 6 1 ADDI,  bl B,
   bdone LBL,
   11 0 ARN-COUNT STR,  1 0 ARN-LO STR,  2 0 ARN-HI STR,
   NDICT 0 ARN-NDICT STR, ;

\ Cleared by prof-on and prof-reset, and NEVER by C-PROF-INDEX-BUILD, which a
\ report runs again over a phase already counted.
: C-PROF-COUNTERS-CLEAR ( -- )
   LBL LBL LBL LBL {: il idone el edone :}
   15 0 MOVZ,
   15 0 ARN-NEW STR,  15 0 ARN-DROP STR,  15 0 ARN-FRAMES STR,
   15 0 ARN-SPILL STR,  15 0 ARN-DEFER STR,  15 0 ARN-FCALLED STR,
   15 0 ARN-CARRY STR,                            \ a phase's weights sum to its own armed time
   15 PROF-WALK-CELLS MOVZ,  15 0 ARN-WALK STR,
   12 ARN-INCL LIT64,  12 0 12 ADD,               \ the inclusive array and the stamps
   13 ARN-DEF LIT64,  13 0 13 ADD,                \ sit back to back, so one loop clears both
   15 0 MOVZ,
   il LBL,
      12 13 CMP,  C-CS idone BCOND,
      15 12 0 STR,  12 12 8 ADDI,  il B,
   idone LBL,
   12 ARN-IDX LIT64,  12 0 12 ADD,
   13 0 ARN-COUNT LDR,  13 13 5 LSLI,  13 12 13 ADD,
   el LBL,
      12 13 CMP,  C-CS edone BCOND,
      15 12 ENT-INCL STR,  12 12 PROF-ENT ADDI,  el B,
   edone LBL, ;

\ Every caller edge from the last run has to go before this one counts: the
\ table is keyed on record indices, and a rebuilt index gives them new meanings.
: C-PROF-CALL-CLEAR ( -- )
   LBL LBL {: cl cd :}
   9 ARN-CALL LIT64,  9 0 9 ADD,
   10 ARN-CALL-BYTES LIT64,  10 9 10 ADD,
   11 0 MOVZ,
   cl LBL,
      9 10 CMP,  C-CS cd BCOND,
      11 9 0 STR,  9 9 8 ADDI,  cl B,
   cd LBL, ;

\ The merge's two moves: one entry from the a side (x17) or the b side (x5) to
\ the output cursor (x6), each source advancing with it.
: C-PROF-ENT-COPY-A ( -- )
   3 17 0 LDR,   3 6 0 STR,
   3 17 8 LDR,   3 6 8 STR,
   3 17 16 LDR,  3 6 16 STR,
   3 17 24 LDR,  3 6 24 STR,
   17 17 PROF-ENT ADDI,  6 6 PROF-ENT ADDI, ;

: C-PROF-ENT-COPY-B ( -- )
   3 5 0 LDR,   3 6 0 STR,
   3 5 8 LDR,   3 6 8 STR,
   3 5 16 LDR,  3 6 16 STR,
   3 5 24 LDR,  3 6 24 STR,
   5 5 PROF-ENT ADDI,  6 6 PROF-ENT ADDI, ;

\ An odd number of passes leaves the sorted entries in the scratch half; bring
\ them home so the index always lives at one address the handler knows.
: C-PROF-SORT-LAND ( -- )
   LBL LBL {: cpl fin :}
   2 ARN-IDX LIT64,  2 0 2 ADD,
   9 2 CMP,  C-EQ fin BCOND,
   3 0 MOVZ,
   cpl LBL,
      3 14 CMP,  C-CS fin BCOND,
      4 9 3 ADD,   4 4 0 LDR,
      15 2 3 ADD,  4 15 0 STR,
      3 3 8 ADDI,  cpl B,
   fin LBL, ;

\ Bottom-up merge sort of x11 entries by start address, arena in x0. Stable,
\ so records that share a start stay in dictionary order and the search's
\ "last start <= pc" answer is the same one an exhaustive scan in record order
\ reports.
: C-PROF-SORT ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL {: pass block mrg ta tb arest brest bfin cb done skip :}
   11 1 CMPI,  C-LS skip BCOND,
   9 ARN-IDX LIT64,  9 0 9 ADD,
   10 ARN-SCR LIT64,  10 0 10 ADD,
   14 PROF-ENT MOVZ,  14 11 14 MUL,                  \ total bytes
   12 PROF-ENT MOVZ,                                 \ merge width, bytes
   pass LBL,
      12 14 CMP,  C-CS done BCOND,
      13 9 0 ADDI,  11 9 14 ADD,
      block LBL,
         13 11 CMP,  C-CS bfin BCOND,
         15 13 12 ADD,  15 11 CMP,  15 15 11 C-LS CSEL,     \ mid
         16 15 12 ADD,  16 11 CMP,  16 16 11 C-LS CSEL,     \ hi
         17 13 0 ADDI,  5 15 0 ADDI,
         6 13 9 SUB,  6 6 10 ADD,
         mrg LBL,
            17 15 CMP,  C-CS brest BCOND,
            5 16 CMP,  C-CS arest BCOND,
            3 17 ENT-START LDR,  4 5 ENT-START LDR,  3 4 CMP,  C-HI tb BCOND,
         ta LBL,  C-PROF-ENT-COPY-A  mrg B,
         tb LBL,  C-PROF-ENT-COPY-B  mrg B,
         arest LBL,  17 15 CMP,  C-CS cb BCOND,  C-PROF-ENT-COPY-A  arest B,
         brest LBL,  5 16 CMP,  C-CS cb BCOND,  C-PROF-ENT-COPY-B  brest B,
         cb LBL,  13 16 0 ADDI,  block B,
      bfin LBL,
      3 9 0 ADDI,  9 10 0 ADDI,  10 3 0 ADDI,
      12 12 1 LSLI,  pass B,
   done LBL,
   C-PROF-SORT-LAND
   skip LBL, ;

\ x8 = arena, x10 = a record index, x3 = the sample's weight: one inclusive
\ sample for that record. Clobbers x11 and x12, and preserves x10 for the caller.
: C-PROF-INCL+ ( -- )
   11 ARN-INCL LIT64,  11 8 11 ADD,  12 10 3 LSLI,  11 11 12 ADD,
   12 11 0 LDR,  12 12 3 ADD,  12 11 0 STR, ;

\ x0 = arena. The handler already took an inclusive sample for every frame the
\ OLD index could name, so replay adds one only for a record that index did not
\ reach - start at or above ARN-OLDHI. Each sample counts at the weight the
\ handler kept with it, in x3.
: C-PROF-REPLAY ( -- )
   LBL LBL LBL LBL LBL LBL {: rl rdone rmiss rnocall rgo rnext :}
   8 0 0 ADDI,                                       \ LPROFFIND and LPROFEDGE take the arena in x8
   24 8 ARN-DEFER LDR,
   23 ARN-DEF LIT64,  23 8 23 ADD,
   rl LBL,
      24 rdone CBZ,
      9 23 0 LDR,  3 23 16 LDR,
      LPROFFIND LABEL@ BL,
      6 rmiss CBZ,
      22 6 ENT-IDX LDR,
      14 PROF-CNT-VA LIT64,  13 22 3 LSLI,  14 14 13 ADD,
      12 14 0 LDR,  12 12 3 ADD,  12 14 0 STR,       \ the exclusive sample this stands for
      10 22 0 ADDI,  C-PROF-INCL+                    \ and the word is inside itself
      9 23 8 LDR,  9 rnocall CBZ,
      9 9 4 SUBI,                                    \ the call site, not the return address
      LPROFFIND LABEL@ BL,
      6 rnocall CBZ,
      10 6 ENT-IDX LDR,  10 22 CMP,  C-EQ rnocall BCOND,
      13 6 ENT-START LDR,  14 8 ARN-OLDHI LDR,  13 14 CMP,  C-CC rgo BCOND,
      C-PROF-INCL+
      rgo B,
      rnocall LBL,  10 PROF-CALLER-NONE LIT64,
      rgo LBL,
      LPROFEDGE LABEL@ BL,
      rnext B,
      rmiss LBL,  12 8 ARN-NEW LDR,  12 12 3 ADD,  12 8 ARN-NEW STR,
      rnext LBL,  23 23 PROF-DEFER-ENT ADDI,  24 24 1 SUBI,  rl B,
   rdone LBL,
   12 0 MOVZ,  12 8 ARN-DEFER STR, ;

\ LPROFSYNC ( -- ): rebuild the pc index from the dictionary as it now stands and
\ replay the deferred samples through it.
\ IT RUNS FROM HABU CODE ONLY, never from the handler's auto-report at the limit:
\ the build reads NDICT, and the handler validates the interrupted x20 and x26
\ but not x27.
: EMIT-PROFSYNC ( -- )
   LPROFSYNC LABEL@ LBL,
   LBL {: nosync :}
   SP SP 96 SUBI,
   30 SP 0 STR,  19 SP 8 STR,  21 SP 24 STR,  22 SP 32 STR,
   23 SP 40 STR,  24 SP 48 STR,  25 SP 56 STR,  17 SP 64 STR,
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,  0 nosync CBZ,
   9 0 ARN-HI LDR,  9 0 ARN-OLDHI STR,
   C-PROF-INCL-FOLD
   C-PROF-SNAPSHOT
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-REPLAY
   nosync LBL,
   30 SP 0 LDR,  19 SP 8 LDR,  21 SP 24 LDR,  22 SP 32 LDR,
   23 SP 40 LDR,  24 SP 48 LDR,  25 SP 56 LDR,  17 SP 64 LDR,
   SP SP 96 ADDI,  RET, ;

\ Clear ARN-OWNER-TID, so a forwarding handler reads 0, which names no thread,
\ then give the handle back. Clobbers x0-x2, x7-x9 and x16.
: C-PROF-OWNER-RELEASE ( -- )
   LBL {: none :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARENA LDR,  9 none CBZ,
   2 9 ARN-OWNER-TID LDR,
   0 0 MOVZ,  0 9 ARN-OWNER-TID STR,
   2 none CBZ,
   2 OS-THREAD-RELEASE
   none LBL, ;

\ The calling thread into ARN-OWNER and ARN-OWNER-TID, its x20 into
\ ARN-OWNER-DATA (prof-on runs on the owner, in Habu code), the previous handle
\ given back, and the main stack's bounds into the arena: the top boot stored,
\ less the soft RLIMIT_STACK, or less PROF-STACK-BYTES when getrlimit fails,
\ answers no limit or one above the top. An owner that is not the main thread
\ gets bounds of 0: its own stack's extent is unknown, so every walk stays
\ block-bounded. Leaves x0 = arena, x7 = band; clobbers x1, x2, x9-x11 and the
\ syscall's registers.
: C-PROF-OWNER ( -- )
   LBL LBL LBL LBL {: dflt have other done :}
   SP SP 16 SUBI,
   0 RLIMIT-STACK MOVZ,  1 SP 0 ADDI,  NR-GETRLIMIT SYS,
   10 SP 0 LDR,  SP SP 16 ADDI,
   C-CS dflt BCOND,
   11 RLIM-INFINITY LIT64,  10 11 CMP,  C-EQ dflt BCOND,
   9 PROF-TOP-VA LIT64,  9 9 0 LDR,  10 9 CMP,  C-LS have BCOND,
   dflt LBL,  10 PROF-STACK-BYTES LIT64,
   have LBL,  C-PROF-OWNER-RELEASE  2 OS-THREAD-HANDLE
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   2 0 ARN-OWNER-TID STR,  DATA 0 ARN-OWNER-DATA STR,
   11 CB-SELF,  11 0 ARN-OWNER STR,
   9 PROF-THREAD-VA LIT64,  9 9 0 LDR,  9 11 CMP,  C-NE other BCOND,
   9 PROF-TOP-VA LIT64,  9 9 0 LDR,  9 0 ARN-STACK-TOP STR,
   9 9 10 SUB,  9 0 ARN-STACK-LOW STR,  done B,
   other LBL,
   9 0 MOVZ,  9 0 ARN-STACK-TOP STR,  9 0 ARN-STACK-LOW STR,
   done LBL, ;

\ x7 = band: PROF-ARMED = ARMED-RUNS by store-release, so a handler on any
\ thread that sees ARMED-RUNS sees ARN-OWNER and the session's cells. Clobbers
\ x9 and x10.
: C-PROF-ARM ( -- )
   9 ARMED-RUNS MOVZ,  10 7 PROF-ARMED ADDI,  9 10 STLR, ;

\ x7 = band: PROF-ARMED = state by store-release, then wait until PROF-IN reads
\ 0. On the owner thread it reads 0 at once: its handler is never suspended by
\ the code it interrupted. Clobbers x9 and x10.
: C-PROF-LEAVE-RUNS ( n -- ) {: state:n :}
   LBL LBL {: spin out :}
   9 state MOVZ,  10 7 PROF-ARMED ADDI,  9 10 STLR,
   10 7 PROF-IN ADDI,
   spin LBL,  9 10 LDAR,  9 out CBZ,  YIELD,  spin B,
   out LBL, ;

\ The ticket lock: take a ticket from PROF-CTL-NEXT by an acquire-release
\ atomic add, then wait until PROF-CTL-SERVE reads it. The handler never
\ touches either cell, so the owner's handler cannot wait on its own thread.
\ Clobbers x9-x11.
: C-PROF-CTL-TAKE ( -- )
   LBL LBL {: spin got :}
   9 1 MOVZ,  10 PROF-BAND-VA PROF-CTL-NEXT + LIT64,
   9 9 10 LDADDAL,
   11 PROF-BAND-VA PROF-CTL-SERVE + LIT64,
   spin LBL,  10 11 LDAR,  10 9 CMP,  C-EQ got BCOND,  YIELD,  spin B,
   got LBL, ;

\ PROF-CTL-SERVE + 1 by store-release: the next ticket runs. Clobbers x10 and
\ x11.
: C-PROF-CTL-FREE ( -- )
   11 PROF-BAND-VA PROF-CTL-SERVE + LIT64,
   10 11 0 LDR,  10 10 1 ADDI,  10 11 STLR, ;

\ The handler counts nothing until the arm: a clock already running keeps
\ ticking through the clears and the rebuild.
: BPROF-ON ( -- )
   LBL LBL {: zl zd :}
   C-PROF-CTL-TAKE
   7 PROF-BAND-VA LIT64,
   0 C-PROF-LEAVE-RUNS
   A G-POP  A 7 PROF-LIM STR,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   DBASE 7 PROF-DBASE STR,
   14 PROF-CNT-VA LIT64,  8 DICT-CAP LIT64,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
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
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,   \ the sigaction calls above own x0
   C-PROF-CLOCK-MARK
   C-PROF-ARM
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE
   C-PROF-CTL-FREE ;

\ Clobbers x0-x2, x8, x9 and x16.
: C-PROF-TIMER-STOP ( -- )
   SP SP 32 SUBI,
   9 0 MOVZ,  9 SP 0 STR,  9 SP 8 STR,  9 SP 16 STR,  9 SP 24 STR,
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS,
   SP SP 32 ADDI, ;

\ Hold the clock for a walk or a clear: a running clock goes to ARMED-HELD
\ before the timer stops, so the handler counts nothing from here on, and its
\ time since the last sample goes to ARN-CARRY once its handler is out; a held
\ or stopped clock only has its timer stopped. Clobbers x0-x2, x7-x10 and x16.
: C-PROF-CLOCK-HOLD ( -- )
   LBL {: off :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARMED LDR,
   9 ARMED-RUNS CMPI,  C-NE off BCOND,
   ARMED-HELD C-PROF-LEAVE-RUNS
   0 7 PROF-ARENA LDR,  0 off CBZ,
   9 OS-TIMER-NOW  1 0 ARN-LAST LDR,  9 9 1 SUB,
   1 0 ARN-CARRY LDR,  9 9 1 ADD,  9 0 ARN-CARRY STR,
   off LBL,
   C-PROF-TIMER-STOP ;

\ Start a held clock again at the recorded interval, its first expiry at the
\ interval's remainder after ARN-CARRY; only a held clock moves, so one
\ prof-off stopped stays stopped. Clobbers x0-x2, x7 and x9-x12.
: C-PROF-CLOCK-RESUME ( -- )
   LBL {: off :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARMED LDR,
   9 ARMED-HELD CMPI,  C-NE off BCOND,
   0 7 PROF-ARENA LDR,
   C-PROF-CLOCK-MARK
   C-PROF-ARM
   C-PROF-TIMER-FRAME
   C-PROF-TIMER-REST
   C-PROF-TIMER
   C-PROF-TIMER-DONE
   off LBL, ;

\ Retirement is reached from engine helpers whose scratch registers still
\ belong to their caller. It holds the ticket lock from before its first read
\ to its one exit, as a control word does, so a trim on one thread never
\ rebuilds the index under another thread's report. x9 is the bodies' input.
: C-PROF-RETIRE-OPEN ( label -- )
   {: absent:label :}
   SP SP 160 SUBI,
   18 0 ?do i SP i cells STR, loop
   30 SP 144 STR,
   C-PROF-CTL-TAKE  9 SP 9 cells LDR,
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,  8 absent CBZ,
   10 8 ARN-IDS LDR,  10 absent CBZ, ;

: C-PROF-RETIRE-CLOSE ( -- )
   C-PROF-CTL-FREE
   18 0 ?do i SP i cells LDR, loop
   30 SP 144 LDR,  SP SP 160 ADDI,  RET, ;

\ Compact the sorted index in place, dropping IDs whose span was released. After
\ a sync every inclusive count is durable, so their history stays.
: C-PROF-INDEX-PRUNE ( -- )
   LBL LBL LBL {: turn next done :}
   8 0 0 ADDI,
   1 ARN-IDX LIT64,  1 8 1 ADD,  2 1 0 ADDI,
   3 8 ARN-COUNT LDR,  3 3 5 LSLI,  3 1 3 ADD,
   4 0 MOVN,  5 0 MOVZ,
   turn LBL,
      1 3 CMP,  C-CS done BCOND,
      6 1 ENT-IDX LDR,
      7 PROF-ID-ENT MOVZ,  6 6 7 MUL,
      7 ARN-RECS LIT64,  7 8 7 ADD,  6 7 6 ADD,
      6 6 RID-START LDR,  6 next CBZ,
      6 1 ENT-START LDR,  7 1 ENT-END LDR,
      6 2 ENT-START STR,  7 2 ENT-END STR,
      10 1 ENT-IDX LDR,  10 2 ENT-IDX STR,
      10 0 MOVZ,  10 2 ENT-INCL STR,
      6 4 CMP,  4 6 4 C-CC CSEL,
      7 5 CMP,  5 7 5 C-HI CSEL,
      2 2 PROF-ENT ADDI,
   next LBL,  1 1 PROF-ENT ADDI,  turn B,
   done LBL,
   1 ARN-IDX LIT64,  1 8 1 ADD,
   2 2 1 SUB,  2 2 5 LSRI,
   2 8 ARN-COUNT STR,  4 8 ARN-LO STR,  5 8 ARN-HI STR, ;

\ NDICT lowering: resolve deferred PCs before their dictionary rows disappear.
: EMIT-PROFTRIM ( -- )
   LPROFTRIM LABEL@ LBL,
   LBL LBL LBL LBL LBL {: absent scan sync turn next :}
   absent C-PROF-RETIRE-OPEN
   10 8 ARN-DEFER LDR,  10 sync CBNZ,
   6 9 0 ADDI,
   scan LBL,
      6 NDICT CMP,  C-CS absent BCOND,
      10 ARN-LIVE LIT64,  10 8 10 ADD,  11 6 3 LSLI,  10 10 11 ADD,
      10 10 0 LDR,  10 0 CMPI,  C-GE sync BCOND,
      6 6 1 ADDI,  scan B,
   sync LBL,
   C-PROF-CLOCK-HOLD
   LPROFSYNC LABEL@ BL,
   6 SP 72 LDR,
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,
   turn LBL,
      6 NDICT CMP,  C-CS next BCOND,
      10 ARN-LIVE LIT64,  10 8 10 ADD,  11 6 3 LSLI,  10 10 11 ADD,
      11 10 0 LDR,
      LBL {: skip :}
      11 0 CMPI,  C-LT skip BCOND,
      12 0 MOVN,  12 10 0 STR,
      10 ARN-RECS LIT64,  10 8 10 ADD,
      13 PROF-ID-ENT MOVZ,  11 11 13 MUL,  10 10 11 ADD,
      12 10 RID-OWNER STR,
      12 0 MOVZ,  12 10 RID-START STR,  12 10 RID-END STR,
      skip LBL,  6 6 1 ADDI,  turn B,
   next LBL,
   0 8 0 ADDI,  C-PROF-INDEX-PRUNE
   C-PROF-CLOCK-RESUME
   absent LBL,  C-PROF-RETIRE-CLOSE ;

\ CP lowering: no raw PC may survive reuse of its bytes. Retire live and
\ seed-retained spans that intersect [new CP,old CP), after naming old samples.
: EMIT-PROFRECLAIM ( -- )
   LPROFRECLAIM LABEL@ LBL,
   LBL LBL LBL LBL {: absent turn next done :}
   absent C-PROF-RETIRE-OPEN
   9 CP CMP,  C-CS absent BCOND,
   C-PROF-CLOCK-HOLD
   LPROFSYNC LABEL@ BL,
   9 SP 72 LDR,
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,
   6 0 MOVZ,  13 8 ARN-IDS LDR,
   turn LBL,
      6 13 CMP,  C-CS done BCOND,
      10 ARN-RECS LIT64,  10 8 10 ADD,
      11 PROF-ID-ENT MOVZ,  11 6 11 MUL,  10 10 11 ADD,
      11 10 RID-START LDR,  11 next CBZ,
      11 CP CMP,  C-CS next BCOND,
      11 10 RID-END LDR,  11 9 CMP,  C-LS next BCOND,
      \ Keep a published ordinal's existing ID: SNAPSHOT must not recreate
      \ this invalidated dictionary span before ordinary trimming drops it.
      11 0 MOVN,  11 10 RID-OWNER STR,
      11 0 MOVZ,  11 10 RID-START STR,  11 10 RID-END STR,
   next LBL,  6 6 1 ADDI,  turn B,
   done LBL,
   0 8 0 ADDI,  C-PROF-INDEX-PRUNE
   C-PROF-CLOCK-RESUME
   absent LBL,  C-PROF-RETIRE-CLOSE ;

\ A REPORT NEVER SAMPLES ITSELF. It reads PROF-TOT once and then walks every
\ counter; a tick in between would leave the header's own identity false by one.
: C-PROF-REPORT-CALL ( label -- ) {: rep:label :}
   SP SP 16 SUBI,  30 SP 0 STR,
   C-PROF-CLOCK-HOLD
   LPROFSYNC LABEL@ BL,
   rep BL,
   C-PROF-CLOCK-RESUME
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-off stops the clock and gives back the owner's handle, nothing else: the
\ handler, the index and every counter stay, so the phase can be reported
\ afterwards. PROF-ARMED goes to 0 before the timer stops, so a tick that
\ arrives after the stop counts nothing.
: BPROF-OFF ( -- )
   C-PROF-CTL-TAKE
   7 PROF-BAND-VA LIT64,  0 C-PROF-LEAVE-RUNS
   C-PROF-TIMER-STOP
   C-PROF-OWNER-RELEASE
   C-PROF-CTL-FREE ;

\ prof-reset clears the counts and keeps the index, so a second phase skips the
\ sort. The clock is held for the clears: PROF-TOT is cleared first and each
\ bucket after it, so a tick counted in between would survive in PROF-TOT and
\ not in its bucket, and the identity would be off by one.
: BPROF-RESET ( -- )
   LBL LBL LBL {: zl zd noarena :}
   C-PROF-CTL-TAKE
   C-PROF-CLOCK-HOLD
   7 PROF-BAND-VA LIT64,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   14 PROF-CNT-VA LIT64,  8 DICT-CAP LIT64,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   0 7 PROF-ARENA LDR,  0 noarena CBZ,
   C-PROF-COUNTERS-CLEAR
   C-PROF-CALL-CLEAR
   noarena LBL,
   C-PROF-CLOCK-RESUME
   C-PROF-CTL-FREE ;

: BPROF-REPORT ( -- )
   C-PROF-CTL-TAKE  LPROFDUMP LABEL@ C-PROF-REPORT-CALL  C-PROF-CTL-FREE ;

: BPROF-JSON ( -- )
   C-PROF-CTL-TAKE  LPROFJSON LABEL@ C-PROF-REPORT-CALL  C-PROF-CTL-FREE ;

\ prof-row ( n -- ): the row for dictionary record n, whatever its rank.
: BPROF-ROW ( -- )
   LBL LBL {: bad ready :}
   C-PROF-CTL-TAKE
   SP SP 16 SUBI,  30 SP 0 STR,
   6 G-POP  6 SP 8 STR,
   C-PROF-CLOCK-HOLD
   LPROFSYNC LABEL@ BL,
   6 SP 8 LDR,
   6 0 CMPI,  C-LT bad BCOND,
   6 NDICT CMP,  C-CS bad BCOND,
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,  8 bad CBZ,
   9 ARN-LIVE LIT64,  9 8 9 ADD,
   10 6 3 LSLI,  9 9 10 ADD,  6 9 0 LDR,
   ready B,
   bad LBL,  6 0 MOVN,
   ready LBL,
   LPROFROW LABEL@ BL,
   C-PROF-CLOCK-RESUME
   C-PROF-CTL-FREE
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-rate sets the interval the NEXT prof-on arms, rather than re-arming here:
\ a rate written while no handler is installed would hand the process a SIGALRM
\ it has no handler for. x10 holds n across C-PROF-ARENA-MAP, which writes only
\ x0-x5, x7-x9 and x16.
: BPROF-RATE ( -- )
   LBL {: ok :}
   C-PROF-CTL-TAKE
   10 G-POP
   10 0 CMPI,  C-GE ok BCOND,
      C-PROF-CTL-FREE
      9 PROF-ABI:E-PROF-RATE LIT64,  9 G-PUSH  BTHROW
   ok LBL,
   C-PROF-ARENA-MAP
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   10 0 ARN-USEC STR,
   C-PROF-CTL-FREE ;

\ prof-pc>rec ( pc -- n ): the record index the armed index gives that pc, or -1.
: BPROF-PCREC ( -- )
   LBL LBL {: miss out :}
   C-PROF-CTL-TAKE
   SP SP 16 SUBI,  30 SP 0 STR,
   9 G-POP
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,  8 miss CBZ,
   LPROFFIND LABEL@ BL,
   6 miss CBZ,
   10 6 ENT-IDX LDR,
   9 ARN-RECS LIT64,  9 8 9 ADD,
   11 PROF-ID-ENT MOVZ,  11 10 11 MUL,  9 9 11 ADD,
   9 9 RID-OWNER LDR,  out B,
   miss LBL,  9 0 MOVN,
   out LBL,  9 G-PUSH
   C-PROF-CTL-FREE
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-child-reset ( -- ): in a process just forked, which has only the forking
\ thread, the profiler state other threads were part way through: no handler
\ counted in, no ticket waiting, the clock stopped (POSIX fork(): the child's
\ interval timers are cleared), and no owner. The owner's handle is cleared,
\ never released: a Mach port name is the parent's, never the child's. Plain
\ stores, no lock, no syscall: it runs where FORK-CHILD's resets run.
: BPROF-CHILD-RESET ( -- )
   LBL {: none :}
   7 PROF-BAND-VA LIT64,
   9 0 MOVZ,  9 7 PROF-IN STR,  9 7 PROF-ARMED STR,
   10 7 PROF-CTL-NEXT LDR,  10 7 PROF-CTL-SERVE STR,
   8 7 PROF-ARENA LDR,  8 none CBZ,
   9 8 ARN-OWNER STR,  9 8 ARN-OWNER-TID STR,
   none LBL, ;

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L
   s" prof-off" ['] BPROF-OFF FPRIM-L  s" prof-reset" ['] BPROF-RESET FPRIM-L
   s" prof-rate" ['] BPROF-RATE FPRIM-L  s" prof-json" ['] BPROF-JSON FPRIM-L
   s" prof-row" ['] BPROF-ROW FPRIM-L
   s" prof-pc>rec" ['] BPROF-PCREC FPRIM-L
   s" prof-child-reset" ['] BPROF-CHILD-RESET FPRIM-L ;

: EMIT-PROF-REPORTS ( -- )
   EMIT-PROFNUM  EMIT-PROFPCT  EMIT-PROFNAME  EMIT-PROFSYNC
   EMIT-PROFTRIM  EMIT-PROFRECLAIM
   false EMIT-PROFREP  true EMIT-PROFREP  EMIT-PROFROW ;

;package

;using
;using
;using
;using
;using
;using
;using
