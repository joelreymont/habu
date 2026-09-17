\ prof.f — the in-binary sampling profiler for the native engine. `n prof-on`
\ arms SIGALRM at the `prof-rate` interval (1 ms by default); each tick maps the
\ interrupted pc to its dictionary word, counts it, and walks the interrupted
\ machine stack for the word's callers. `prof-off` stops the clock and leaves
\ every counter readable, `prof-reset` clears them and keeps the index, and
\ `prof-report` / `prof-json` print the same walk as text or JSON: exclusive and
\ inclusive counts with percent and the top callers under each row. A non-zero
\ `prof-on` limit still reports and exits 99 at that many samples, for a program
\ that cannot reach a `prof-off` of its own. prof-pc>rec answers the handler's
\ attribution question from Habu code.
\ Load after habu1.f (uses DBASE/NDICT/DREC/A/FPRIM-L), before habu2.f.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
\
\ The tick lands in whatever the timer interrupted, and that is not always Habu
\ code: a foreign callee (libc's dlsym, libzip's inflate) reuses the callee-saved
\ registers the engine reserves for DATA (x20), DBASE (x26) and NDICT (x27), and
\ a handler that read them walked garbage (SIGSEGV in the handler, x0 = SIGALRM).
\ So the handler takes nothing from the live registers. Its state lives in the
\ profiler band at the top of the DATA region, reached through the fixed DATA-VA;
\ prof-on records the dictionary base there; the interrupted context's x20 and
\ x26 are read from the signal frame and compared with those two facts, and only
\ a context that holds both is attributed; any other context is a "(foreign)"
\ sample. The handler runs on its own alternate stack, so a tick with the
\ machine stack deep never pushes the signal frame over the data stack. The
\ report reads the same band, so it needs no register either.
using A64ASM

package PROF

public
variable LPROFH   variable LPROFDUMP   variable LPROFFIND   variable LPROFEDGE
variable LPROFJSON   variable LPROFNUM   variable LPROFPCT   variable LPROFNAME
variable LPROFQUAL   variable LPROFSYNC   variable LPROFROW
private

\ ---- the target this file's signal frame belongs to --------------------------
\ Everything below reads a signal frame: the ucontext-to-mcontext offset, the
\ mcontext PC and x0 slots, the sigaction frame and its sigset size, and the
\ instructions that load them. Those are AArch64 and Darwin/Linux facts, and a
\ third target matches none of them - reading them under another machine would
\ attribute samples from a frame that is not there. Signal handlers are target
\ ABI boundaries (docs/porting.md), so this file refuses a target whose frame it
\ does not model, at load, rather than emitting the aarch64 one for it. The
\ refusal is a die and not lib/errors.f's E-PLATFORM because this file loads
\ inside the engine-build window, which has no lib/ in it.
: PROF-TARGET-OK ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   s" hb: this target's signal frame is not modelled" 76 die ;
PROF-TARGET-OK

\ ---- the band: PROF-STATE-BYTES of state cells, then one counter per dict record
DATA-SIZE PROF-CNT-BYTES - constant PROF-BAND           \ band base, DATA-relative
DATA-VA VA>N PROF-BAND + constant PROF-BAND-VA          \ absolute: DATA is MAP_FIXED at DATA-VA
PROF-BAND-VA PROF-STATE-BYTES + constant PROF-CNT-VA    \ the counters
0  constant PROF-TOT        \ samples delivered
8  constant PROF-LIM        \ report + exit(99) at this many, 0 = sample until prof-off
16 constant PROF-OTHER      \ Habu-code samples outside any dict word (main loop, helpers)
24 constant PROF-FOREIGN    \ samples in a context that is not Habu code
32 constant PROF-DBASE      \ the dictionary base, recorded by prof-on
40 constant PROF-STACK      \ the handler's alternate stack, mapped once per process
48 constant PROF-ARENA      \ the profiler arena, mapped once per process
56 constant PROF-ARMED      \ 1 while the clock runs: a report stops it and puts it back

\ ---- the arena: the pc index the handler searches -----------------------------
\ THE HANDLER NEVER WALKS THE DICTIONARY. It searches a pc-sorted live-range
\ index prof-on builds, so a tick costs log2(entries) compares instead of a scan
\ that grows with the dictionary (measured 240,021 instructions per tick at
\ ndict 15,835 before this index; the walk stopped at the FIRST record holding
\ the pc, so its cost was the hot word's record number, not a constant).
\
\ The band cell PROF-ARENA holds the arena base and every table below it sits at
\ a build-time offset from that base, so the handler reaches any of them with one
\ load and no bound of its own. The arena is mapped once per process, like the
\ alternate stack, and rebuilt in place by each prof-on.
\
\ WHAT THE INDEX ANSWERS, exactly, so a linear reference can state the same rule:
\ the entry with the greatest start <= pc, reported only when pc < that entry's
\ end. Entries are sorted by start with a STABLE merge, so records that share a
\ start (an alias and its original) keep dictionary order and the search lands on
\ the last of them. Records with no code of their own (a namespace row, a retired
\ row, a zero-length body) are not in the index at all.
128 constant ARN-HDR        \ arena header bytes, then the index
0  constant ARN-COUNT       \ index entries
8  constant ARN-LO          \ lowest code address in the index
16 constant ARN-HI          \ one past the highest
24 constant ARN-NDICT       \ the record count the index was built from
32 constant ARN-NEW         \ samples at or above ARN-HI: code compiled after the build
40 constant ARN-DROP        \ caller edges dropped: the probe window was full
48 constant ARN-FRAMES      \ caller frames attributed out of the machine stack
56 constant ARN-WALK        \ machine-stack cells one sample may scan
64 constant ARN-USEC        \ sampling interval in microseconds, 0 = the 1000 default
72 constant ARN-SPILL       \ new-code samples the deferred buffer had no room for
80 constant ARN-DEFER       \ deferred samples waiting for the next report to name them
88 constant ARN-OLDHI       \ the high mark before a rebuild: what the handler could name
32 constant PROF-ENT        \ index entry bytes
0  constant ENT-START
8  constant ENT-END
16 constant ENT-IDX         \ the dictionary record index, which owns the counter
24 constant ENT-INCL        \ inclusive samples since the last rebuild, folded by it

\ ---- caller edges -------------------------------------------------------------
\ One open-addressed table for every (sampled word, caller) pair, keyed on the
\ two record indices packed into 32 bits, probed at most PROF-CALL-PROBE times so
\ a tick's cost stays bounded; a pair that finds no slot is counted in ARN-DROP
\ and reported, never silently merged into another row.
$10000 constant PROF-CALL-SLOTS
$FFFF  constant PROF-CALL-MASK          \ the slot index, PROF-CALL-SLOTS wide
17 constant PROF-REC-BITS               \ a record index plus the one value above it
$1FFFF constant PROF-REC-MASK
DICT-CAP constant PROF-CALLER-NONE      \ no record index reaches it: the unknown caller
16 constant PROF-CALL-ENT               \ key+1, then the count
8  constant PROF-CALL-PROBE
$9E3779B97F4A7C15 constant PROF-HASH    \ golden-ratio multiplier; the top 16 bits index
64 constant PROF-WALK-CELLS             \ default machine-stack scan, in cells
$FFF constant PROF-PAGE-MASK            \ a sample never reads past its own 4 KiB block
\ ---- deferred samples -----------------------------------------------------------
\ A tick in code compiled after prof-on has no index entry to name it. The handler
\ keeps the pc and the interrupted x30 - two cells at a fixed stride, no walk, no
\ allocation - and the next report rebuilds the index from the dictionary as it
\ then stands and replays them, which is the only point at which every word the
\ phase compiled exists. The slot count is sized from the measurement that opened
\ the dot: a 92-second self-build at 1 kHz put 13,991 of 92,000 samples in this
\ bucket, so $40000 slots hold about eighteen such builds; past that a sample is
\ counted in ARN-SPILL and reported, never dropped in silence.
16 constant PROF-DEFER-ENT              \ the sample's pc, then its x30
$40000 constant PROF-DEFER-SLOTS
64 constant PROF-ROWS                   \ rows one report prints: enough that a compiler phase's roots,
                                        \ which carry a large inclusive share on a small exclusive one, reach the report
5  constant PROF-CALLERS                \ caller lines under each row: the top few, not every one
DICT-CAP PROF-ENT * constant ARN-IDX-BYTES
PROF-CALL-SLOTS PROF-CALL-ENT * constant ARN-CALL-BYTES
ARN-HDR constant ARN-IDX                       \ the index itself
ARN-IDX ARN-IDX-BYTES + constant ARN-SCR       \ the merge sort's second half
ARN-SCR ARN-IDX-BYTES + constant ARN-CALL      \ the caller table
DICT-CAP cells constant ARN-INCL-BYTES
PROF-DEFER-SLOTS PROF-DEFER-ENT * constant ARN-DEF-BYTES
ARN-CALL ARN-CALL-BYTES + constant ARN-INCL    \ inclusive samples, one per RECORD
ARN-INCL ARN-INCL-BYTES + constant ARN-STAMP   \ the sample serial each record was last counted in
ARN-STAMP ARN-INCL-BYTES + constant ARN-DEF    \ the deferred samples
ARN-DEF ARN-DEF-BYTES + constant ARN-BYTES
$10000 constant PROF-STACK-BYTES
14  constant SIGALRM
$18000004 constant LINUX-SA-PROF-FLAGS   \ SA_SIGINFO | SA_ONSTACK | SA_RESTART
$0043 constant MACOS-SA-PROF-FLAGS        \ SA_ONSTACK | SA_RESTART | SA_SIGINFO
42 constant PROFMMAPMSG-LEN               \ "hb: prof-on: cannot map the handler stack\n"
46 constant PROFSTKMSG-LEN                \ "hb: prof-on: cannot install the handler stack\n"
43 constant PROFARNMSG-LEN                \ "hb: prof-on: cannot map the profiler arena\n"
40 constant DICT-WL-OFF                   \ the record's wordlist cell (habu1.f BSWL reads the same 40)

\ Profiler helpers emit ARM64 signal-context, sigaction/timer-frame, syscall,
\ sampling, and primitive-publication code.
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

\ x13 = the record's code bytes from its raw length cell (src/habu/code-span.f):
\ bit 31 marks an exact span, otherwise the body precedes one final slot. Read
\ raw, that bit made the first record below the pc own every sample, and the
\ report's first line was DEFER-UNSET.
: C-PROF-SPAN-BYTES ( -- )
   14 13 31 LSRI,                         \ x14 = the exact-span bit
   13 13 CODE-SPAN:MASK ANDI,             \ x13 = body
   13 13 CODE-SPAN:INSN-BYTES ADDI,       \ a legacy body ends before its final slot
   14 14 2 LSLI,  13 13 14 SUB, ;         \ an exact span does not

public

\ LPROFFIND ( x9 = pc, x8 = arena -- x6 = index entry or 0 ): the whole of pc
\ attribution, shared by the signal handler and by `prof-pc>rec`, so the property
\ test exercises the code the handler runs and not a copy of it. Bounded: one
\ range test plus at most log2(DICT-CAP) = 16 compares. Clobbers x10-x13 and
\ obeys the primitive ABI (x19-x28 untouched), because prof-pc>rec calls it from
\ compiled Habu code.
: EMIT-PROFFIND ( -- )
   LPROFFIND LABEL@ LBL,
   LBL LBL LBL LBL LBL {: bl bhi bdone miss out :}
   10 8 ARN-COUNT LDR,  10 miss CBZ,
   11 8 ARN-LO LDR,  9 11 CMP,  C-CC miss BCOND,      \ below every span
   11 8 ARN-HI LDR,  9 11 CMP,  C-CS miss BCOND,      \ at or above every span
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

\ LPROFEDGE ( x8 = arena, x22 = sampled record, x10 = caller record ): count one
\ caller edge. Clobbers x11-x16. A sample whose own record is unknown (x22 < 0)
\ has no edge to key, and a full probe window counts a drop rather than
\ overwriting somebody else's pair.
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
   16 8 ARN-DROP LDR,  16 16 1 ADDI,  16 8 ARN-DROP STR,  edone B,
   efree LBL,  11 15 0 STR,  16 1 MOVZ,  16 15 8 STR,  edone B,
   ehit LBL,  16 15 8 LDR,  16 16 1 ADDI,  16 15 8 STR,
   edone LBL,  RET, ;

\ x8 = arena, x7 = band, x6 = an index entry, x10 = its record index: one
\ inclusive sample for that record, and only the first time THIS sample reaches
\ it. The conservative walk can see one word twice - a recursive call, or a
\ stale spill slot still holding a return address into it - and an inclusive
\ count that ran past the sample total would say a word took longer than the
\ program did. The stamp is the sample serial, so the test is one load and one
\ compare and needs no per-sample set. Clobbers x11-x13.
: C-PROF-INCL-ONCE ( -- )
   LBL {: seen :}
   11 ARN-STAMP LIT64,  11 8 11 ADD,  12 10 3 LSLI,  11 11 12 ADD,
   12 11 0 LDR,
   13 7 PROF-TOT LDR,  13 13 1 ADDI,               \ this sample's serial: TOT is bumped after
   12 13 CMP,  C-EQ seen BCOND,
   13 11 0 STR,
   12 6 ENT-INCL LDR,  12 12 1 ADDI,  12 6 ENT-INCL STR,
   seen LBL, ;

\ x8 = arena, x9 = the interrupted pc, x21 = mcontext: keep one sample whose code
\ the index cannot name yet. Only the pc and the interrupted x30 are kept, at a
\ fixed stride, so the handler still walks nothing here and allocates nothing; the
\ next report rebuilds the index and replays them. A full buffer counts a spill
\ rather than overwriting a sample already taken. Clobbers x10-x14.
: C-PROF-DEFER ( -- )
   LBL LBL {: full done :}
   10 8 ARN-DEFER LDR,
   11 PROF-DEFER-SLOTS LIT64,  10 11 CMP,  C-CS full BCOND,
   12 ARN-DEF LIT64,  12 8 12 ADD,
   13 PROF-DEFER-ENT MOVZ,  13 10 13 MUL,  12 12 13 ADD,
   9 12 0 STR,
   30 14 C-PROF-CTX-X>R  14 12 8 STR,
   10 10 1 ADDI,  10 8 ARN-DEFER STR,
   done B,
   full LBL,  10 8 ARN-SPILL LDR,  10 10 1 ADDI,  10 8 ARN-SPILL STR,
   done LBL, ;

\ The conservative machine-stack walk, the whole of caller attribution.
\
\ EMITTED WORDS CARRY NO FRAME POINTER: the prologue is `sub sp,sp,#16` +
\ `str x30,[sp]`, so there is no chain to follow and the return addresses have to
\ be recognised by their VALUE - a cell that lands inside the index's code range
\ is taken for a return address, which is the standard frame-pointer-less
\ technique and is a superset of the true chain: an uninitialised spill slot
\ still holding a code address from a returned call adds a frame. The x29 chain
\ the dot offers as the alternative is not built: it would cost every emitted
\ word in the default build two more instructions and a register, and this walk
\ measured 230 instructions a tick with it.
\
\ THE IMMEDIATE CALLER IS USUALLY IN x30, not on the stack: a leaf primitive
\ entered by BL never stores it, and a word that has not called anything yet
\ still holds its own caller there. When x30 points into the sampled word itself
\ - the word called something that has already returned - the scan below answers
\ instead, because the word's own prologue saved the real return address.
\
\ THE SCAN NEVER LEAVES SP's 4 KiB BLOCK. SP is mapped, so every byte of its
\ block is; one cell further could be the unmapped page above a thread stack, and
\ a SIGSEGV inside a SIGALRM handler is not a diagnosis anybody can use.
: C-PROF-WALK ( -- )
   LBL LBL LBL LBL {: wl wlr wdone wret :}
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
   10 8 ARN-WALK LDR,  10 10 3 LSLI,  24 23 10 ADD,
   10 23 PROF-PAGE-MASK ORRI,  10 10 1 ADDI,
   24 10 CMP,  24 24 10 C-LS CSEL,
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

\ ---- printing ----------------------------------------------------------------
\ Everything the report prints goes through these four, so a row, a caller line
\ and a JSON field all agree about widths and about where a name comes from.

\ Emit a literal string write. The bytes sit in the instruction stream behind a
\ branch, the way the profiler's error messages already do.
: C-PROF-SAY ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: skip txt :}
   skip B,
   txt LBL,  a u BYTES,
   skip LBL,
   0 1 MOVZ,  1 txt ADR,  2 u MOVZ,  NR-WRITE SYS, ;

\ One literal byte.
\
\ NO ESCAPED STRING IN THIS FILE MAY START ITS PAYLOAD WITH A BACKSLASH.
\ tools/lint/token.f splits on whitespace and has no model of a string literal,
\ so the payload of `s\" \n"` reaches it as the chunk `\n"` - and a chunk
\ beginning with a backslash is Forth's line comment, which swallows the rest of
\ the line, this definition's `;` included. clobber-lint then reads the next
\ definition as part of this one and reports a register clobber across the join.
\ A bare line feed and a bare quote therefore come from their byte values here,
\ the way LPROFPCT's decimal point always has.
: C-PROF-BYTE ( n -- ) {: c:n :}
   SP SP 16 SUBI,  13 c MOVZ,  13 SP 0 STRB,
   0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   SP SP 16 ADDI, ;

: C-PROF-NL ( -- )  $0A C-PROF-BYTE ;
: C-PROF-DQ ( -- )  $22 C-PROF-BYTE ;

\ A header field: its name, then the register's value.
: C-PROF-FIELD ( ptr u8 n n -- ) {: a:ptr u:n reg:n :}
   a u C-PROF-SAY
   9 reg 0 ADDI,  10 1 MOVZ,  LPROFNUM LABEL@ BL, ;

\ LPROFNUM ( x9 = value, x10 = width ): unsigned decimal, right-aligned in x10
\ columns and never truncated, no newline. The padding is prepended into the
\ same buffer the digits land in, so one write puts out the whole column.
\ Clobbers x0-x2, x9-x15.
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

\ LPROFNAME ( x5 = record ): the record's own name bytes.
\ The record's flags cell carries the name length in its low bits and four
\ fields above it: DKIND (50-51), DNAME-MIN-IN (52-59) and the IMM/EXT/WIDE/INT
\ nibble (60-63). Clearing the top FOURTEEN is what leaves the length alone -
\ src/habu/layout.f states the band. A clear of twelve leaves a definer's stamp
\ in the count and hands `write` a length of 2^50 bytes, which writes nothing
\ and loses the row's name.
: EMIT-PROFNAME ( -- )
   LPROFNAME LABEL@ LBL,
   LBL {: pinl :}
   0 1 MOVZ,  1 5 24 ADDI,  2 5 16 LDR,
   9 2 DNAME-EXT ANDI,  9 pinl CBZ,
      1 5 24 LDR,
   pinl LBL,  2 2 14 LSLI,  2 2 14 LSRI,  NR-WRITE SYS,
   RET, ;

\ LPROFQUAL ( x5 = record, x19 = arena ): the package qualifier, "PKG:", when the
\ record's wordlist cell is some package's public or private wid - which is the
\ whole of the ambiguity the bare name leaves, because two packages may each
\ have a private word of the same name. A package row carries DICT-WL:NAMESPACE
\ in its wordlist cell, its public wid in [0] and its private wid in [8]
\ (habu2.f C-PACKAGE-NEW-RECORD), so the rows themselves answer this and no
\ second table can fall out of step with the dictionary. Preserves x5.
: EMIT-PROFQUAL ( -- )
   LPROFQUAL LABEL@ LBL,
   LBL LBL LBL LBL {: ql qnext qhit qdone :}
   LBL {: qfound :}
   SP SP 16 SUBI,  30 SP 0 STR,
   9 5 DICT-WL-OFF LDR,
   9 qdone CBZ,                                    \ the global wordlist has no qualifier
   10 PROF-BAND-VA LIT64,  11 10 PROF-DBASE LDR,
   12 19 ARN-NDICT LDR,
   13 0 MOVZ,
   ql LBL,
      13 12 CMP,  C-CS qdone BCOND,
      14 11 DICT-WL-OFF LDR,  14 14 1 ADDI,  14 qhit CBZ,
      qnext LBL,  11 11 DREC ADDI,  13 13 1 ADDI,  ql B,
   qhit LBL,
      14 11 0 LDR,  9 14 CMP,  C-EQ qfound BCOND,
      14 11 8 LDR,  9 14 CMP,  C-NE qnext BCOND,
   qfound LBL,
      5 SP 8 STR,
      5 11 0 ADDI,  LPROFNAME LABEL@ BL,
      SP SP 16 SUBI,  13 $3A MOVZ,  13 SP 0 STRB,
      0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      5 SP 8 LDR,
   qdone LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ ---- the report ---------------------------------------------------------------
\ LPROFDUMP / LPROFJSON ( -- ): the same walk, printed two ways.
\ The Forth-level flag picks the punctuation at build time, so the text report
\ and the JSON report can never disagree about what they counted.
\
\ ROWS ARE SELECTED BY REPEATED MAXIMUM, not sorted: at most PROF-ROWS rows are
\ printed, one pass over the index costs less than sorting every entry, no second
\ array is needed, and - the reason that matters - the counters are left exactly
\ as they were, so a later prof-report says the same thing.
\
\ REGISTERS. LPROFNUM and LPROFPCT clobber x9-x15, so every value that has to
\ live across a printed column sits in x3-x7, x17 or x19-x25: x19 arena, x20
\ band, x21 the row's index entry, x22 samples, x23/x24 the (count, entry)
\ threshold the next row must fall under, x25 the row rank, x7 the row's
\ exclusive count, x17 its record index, x6/x3 the caller threshold and rank.
\ x19-x25 are saved and restored because the compiled callers own them.
: C-PROF-REP-OPEN ( -- )
   SP SP 96 SUBI,
   30 SP 0 STR,  19 SP 8 STR,  20 SP 16 STR,  21 SP 24 STR,  22 SP 32 STR,
   23 SP 40 STR,  24 SP 48 STR,  25 SP 56 STR,  17 SP 64 STR, ;

: C-PROF-REP-CLOSE ( -- )
   30 SP 0 LDR,  19 SP 8 LDR,  20 SP 16 LDR,  21 SP 24 LDR,  22 SP 32 LDR,
   23 SP 40 LDR,  24 SP 48 LDR,  25 SP 56 LDR,  17 SP 64 LDR,
   SP SP 96 ADDI,  RET, ;

\ x4 = samples attributed to a word, x3 = how many words took one.
\
\ THE SUM WALKS RECORDS, NOT INDEX ENTRIES. A word counted during the phase can
\ lose its entry before the report: the sync rebuilds the index from the
\ dictionary as it then stands, and a record retired in between is not in it.
\ Summing the entries orphaned those counters and left the header's identity
\ short - 397 samples of 92,013 on a self-build, which is exactly how this was
\ found. The counter array is keyed by record, so walking it counts them all.
\ x3 stays entry-based: it says how many rows the reports can name.
: C-PROF-REP-WORDSUM ( -- )
   LBL LBL LBL LBL LBL {: wl wd wnext cl cd :}
   4 0 MOVZ,  3 0 MOVZ,
   11 PROF-CNT-VA LIT64,
   12 19 ARN-NDICT LDR,
   cl LBL,
      12 cd CBZ,
      10 11 0 LDR,  4 4 10 ADD,
      11 11 8 ADDI,  12 12 1 SUBI,  cl B,
   cd LBL,
   12 ARN-IDX LIT64,  12 19 12 ADD,
   13 19 ARN-COUNT LDR,  13 13 5 LSLI,  13 12 13 ADD,
   wl LBL,
      12 13 CMP,  C-CS wd BCOND,
      10 12 ENT-IDX LDR,  11 PROF-CNT-VA LIT64,  10 10 3 LSLI,  11 11 10 ADD,
      10 11 0 LDR,
      14 12 ENT-INCL LDR,  14 14 10 ORR,
      15 12 ENT-IDX LDR,  11 ARN-INCL LIT64,  11 19 11 ADD,  15 15 3 LSLI,  11 11 15 ADD,
      15 11 0 LDR,  14 14 15 ORR,
      14 wnext CBZ,  3 3 1 ADDI,                   \ a word either count reached
      wnext LBL,
      12 12 PROF-ENT ADDI,  wl B,
   wd LBL, ;

\ words + other + new + defer + spill + foreign == samples is the accounting this
\ line states. After a sync, defer is 0 and new holds only the deferred samples
\ whose pc belongs to no live record; an auto-report at the limit does not sync,
\ so its deferred samples are still sitting in defer.
: C-PROF-REP-HEAD ( bool -- ) {: json:bool :}
   json IF s\" {\"samples\":" ELSE s" profiler samples " THEN 22 C-PROF-FIELD
   json IF s\" ,\"words\":" ELSE s"  words " THEN 4 C-PROF-FIELD
   10 20 PROF-OTHER LDR,
   json IF s\" ,\"other\":" ELSE s"  other " THEN 10 C-PROF-FIELD
   10 19 ARN-NEW LDR,
   json IF s\" ,\"new\":" ELSE s"  new " THEN 10 C-PROF-FIELD
   10 19 ARN-DEFER LDR,
   json IF s\" ,\"defer\":" ELSE s"  defer " THEN 10 C-PROF-FIELD
   10 19 ARN-SPILL LDR,
   json IF s\" ,\"spill\":" ELSE s"  spill " THEN 10 C-PROF-FIELD
   10 20 PROF-FOREIGN LDR,
   json IF s\" ,\"foreign\":" ELSE s"  foreign " THEN 10 C-PROF-FIELD
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

\ x9 = the current row's inclusive count: what the per-record array holds plus
\ what the entry has taken since the last fold. An auto-report at the limit never
\ folds, so the second half is the only half it has.
: C-PROF-ROW-INCL ( -- )
   9 21 ENT-IDX LDR,  10 ARN-INCL LIT64,  10 19 10 ADD,
   9 9 3 LSLI,  9 10 9 ADD,  9 9 0 LDR,
   10 21 ENT-INCL LDR,  9 9 10 ADD, ;

\ x5 = the record behind the current row, x17 = its record index.
: C-PROF-REP-ROW-REC ( -- )
   17 21 ENT-IDX LDR,
   5 20 PROF-DBASE LDR,  10 DREC MOVZ,  10 17 10 MUL,  5 5 10 ADD, ;

\ x11 = what the current entry (x12) is ranked on: its exclusive count, or the
\ inclusive one a phase word is only ever visible by. Clobbers x10.
: C-PROF-RANK ( bool -- ) {: incl:bool :}
   10 12 ENT-IDX LDR,
   incl IF
      11 ARN-INCL LIT64,  11 19 11 ADD,  10 10 3 LSLI,  11 11 10 ADD,
      11 11 0 LDR,
      10 12 ENT-INCL LDR,  11 11 10 ADD,
      exit
   THEN
   11 PROF-CNT-VA LIT64,  10 10 3 LSLI,  11 11 10 ADD,  11 11 0 LDR, ;

\ One text row: exclusive, its share, inclusive, its share, then the word.
: C-PROF-REP-ROW ( -- )
   9 7 0 ADDI,  10 8 MOVZ,  LPROFNUM LABEL@ BL,
   9 7 0 ADDI,  10 22 0 ADDI,  LPROFPCT LABEL@ BL,
   C-PROF-ROW-INCL  10 8 MOVZ,  LPROFNUM LABEL@ BL,
   C-PROF-ROW-INCL  10 22 0 ADDI,  LPROFPCT LABEL@ BL,
   s"   " C-PROF-SAY
   C-PROF-REP-ROW-REC
   LPROFQUAL LABEL@ BL,  LPROFNAME LABEL@ BL,
   C-PROF-NL ;

\ x5 = the record behind a caller index in x9, or 0 for PROF-CALLER-NONE, which
\ stands for a sample the walk could not attribute and owns no dictionary row.
: C-PROF-CALLER-REC ( -- )
   LBL LBL {: none done :}
   11 PROF-CALLER-NONE LIT64,  9 11 CMP,  C-EQ none BCOND,
   5 20 PROF-DBASE LDR,  11 DREC MOVZ,  11 9 11 MUL,  5 5 11 ADD,
   done B,
   none LBL,  5 0 MOVZ,
   done LBL, ;

: C-PROF-CALLER-NAME ( -- )
   LBL LBL {: none done :}
   5 none CBZ,
   LPROFQUAL LABEL@ BL,  LPROFNAME LABEL@ BL,
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

\ The row's top callers: at most PROF-CALLERS passes over the edge table, each
\ taking the largest count below the one before it. The text report shows the
\ top few; prof-json carries every edge there is.
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

\ One text section: at most PROF-ROWS rows ranked on one of the two counts, each
\ with its callers.
\
\ ROWS ARE SELECTED BY REPEATED MAXIMUM, not sorted: one pass over the index per
\ row costs less than sorting every entry, no second array is needed, and - the
\ reason that matters - the counters are left exactly as they were, so a later
\ prof-report says the same thing.
: C-PROF-REP-SECTION ( bool -- ) {: incl:bool :}
   LBL LBL LBL LBL LBL LBL LBL {: rowl sell selcmp selnx seld seltake rowsd :}
   23 0 MOVN,  24 0 MOVN,  25 0 MOVZ,
   rowl LBL,
      7 0 MOVZ,  21 0 MOVN,
      12 ARN-IDX LIT64,  12 19 12 ADD,
      13 19 ARN-COUNT LDR,  13 13 5 LSLI,  13 12 13 ADD,
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
         selnx LBL,  12 12 PROF-ENT ADDI,  sell B,
      seld LBL,
      7 rowsd CBZ,                                 \ no counted row left
      23 7 0 ADDI,  24 21 0 ADDI,
      17 21 ENT-IDX LDR,
      7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,  \ the row prints exclusive
      incl IF C-PROF-ROW-INCL ELSE 9 7 0 ADDI, THEN  9 SP 88 STR,
      C-PROF-REP-ROW
      C-PROF-REP-CALLERS
      25 25 1 ADDI,
      25 PROF-ROWS CMPI,  C-CC rowl BCOND,
   rowsd LBL, ;

\ ---- JSON -----------------------------------------------------------------------
\ EVERY ATTRIBUTED WORD, not a top-N. A phase word - a compiler pass, a verifier
\ entry - is often invisible in an exclusive ranking and is exactly what a
\ per-phase table needs, so the machine-readable report carries the whole set:
\ one object per index entry that took a sample, and the caller edges as a flat
\ array rather than nested under each row. Nesting them would mean one pass over
\ the edge table per row - quadratic, and this walk runs over every row - while
\ one pass emits every edge there is. Both are bounded by the arena: at most
\ ARN-COUNT rows and PROF-CALL-SLOTS edges, so the report cannot outgrow it and
\ nothing is truncated.
: C-PROF-JSON-ROWS ( -- )
   LBL LBL LBL LBL {: jl jdone jnext jcomma :}
   s\" ,\"rows\":[" C-PROF-SAY
   25 0 MOVZ,
   21 ARN-IDX LIT64,  21 19 21 ADD,
   23 19 ARN-COUNT LDR,  23 23 5 LSLI,  23 21 23 ADD,
   jl LBL,
      21 23 CMP,  C-CS jdone BCOND,
      17 21 ENT-IDX LDR,
      7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,
      C-PROF-ROW-INCL
      10 7 9 ORR,  10 jnext CBZ,                   \ neither count moved: not a row
      25 jcomma CBZ,  s" ," C-PROF-SAY
      jcomma LBL,  25 1 MOVZ,
      s\" {\"word\":\"" C-PROF-SAY
      C-PROF-REP-ROW-REC
      LPROFQUAL LABEL@ BL,  LPROFNAME LABEL@ BL,
      C-PROF-DQ  s\" ,\"excl\":" C-PROF-SAY
      9 7 0 ADDI,  10 1 MOVZ,  LPROFNUM LABEL@ BL,
      s\" ,\"incl\":" C-PROF-SAY
      C-PROF-ROW-INCL  10 1 MOVZ,  LPROFNUM LABEL@ BL,
      s" }" C-PROF-SAY
   jnext LBL,  21 21 PROF-ENT ADDI,  jl B,
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
   20 PROF-BAND-VA LIT64,
   22 20 PROF-TOT LDR,
   19 20 PROF-ARENA LDR,
   19 armed CBNZ,
      json IF s\" {\"samples\":0,\"armed\":false}\n" ELSE s\" profiler not armed\n" THEN C-PROF-SAY
      C-PROF-REP-CLOSE
   armed LBL,
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
\ with its callers. `prof-row` is how a phase word that no exclusive ranking
\ would ever show gets read; the record index comes from the caller, which is
\ where a name lookup belongs.
: EMIT-PROFROW ( -- )
   LPROFROW LABEL@ LBL,
   LBL LBL LBL LBL {: rl rfound rdone rnone :}
   C-PROF-REP-OPEN                                 \ x6 = the record the caller asked for
   20 PROF-BAND-VA LIT64,
   22 20 PROF-TOT LDR,
   19 20 PROF-ARENA LDR,
   19 rnone CBZ,
   21 ARN-IDX LIT64,  21 19 21 ADD,
   23 19 ARN-COUNT LDR,  23 23 5 LSLI,  23 21 23 ADD,
   rl LBL,
      21 23 CMP,  C-CS rnone BCOND,
      10 21 ENT-IDX LDR,  10 6 CMP,  C-EQ rfound BCOND,
      21 21 PROF-ENT ADDI,  rl B,
   rfound LBL,
   17 21 ENT-IDX LDR,
   7 PROF-CNT-VA LIT64,  10 17 3 LSLI,  7 7 10 ADD,  7 7 0 LDR,
   C-PROF-ROW-INCL  9 SP 88 STR,                   \ one chosen row is read for its inclusive time
   C-PROF-REP-ROW
   C-PROF-REP-CALLERS
   rdone LBL,
   C-PROF-REP-CLOSE
   rnone LBL,
   s" profiler: no row for that record" C-PROF-SAY  C-PROF-NL
   C-PROF-REP-CLOSE ;

\ Attribute the interrupted pc FIRST (a dict word's counter, the deferred buffer,
\ PROF-OTHER or PROF-FOREIGN), THEN bump PROF-TOT once and test the limit, so
\ every delivered sample is counted: sum(word counters) + ARN-NEW + ARN-DEFER +
\ ARN-SPILL + PROF-OTHER + PROF-FOREIGN == PROF-TOT exactly, including the sample
\ that reaches the limit, and that identity is the report's header line. Below the limit we sigreturn;
\ at it we report + exit(99), but only from a validated sample; a foreign sample
\ at the limit sigreturns and the next validated one reports.
: EMIT-PROF ( -- )
   LPROFH LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL {: pnew pother pdone pforeign psig pexit :}
   C-PROF-MCTX>R21  C-PROF-PC>R9
   7 PROF-BAND-VA LIT64,
   20 10 C-PROF-CTX-X>R  11 DATA-VA VA>N LIT64,  10 11 CMP,  C-NE pforeign BCOND,   \ context x20 must be DATA
   26 10 C-PROF-CTX-X>R  11 7 PROF-DBASE LDR,  10 11 CMP,  C-NE pforeign BCOND,   \ context x26 must be the recorded DBASE
   17 1 MOVZ,                                        \ x17 = 1: a Habu context, its count may report (x16 is the Darwin syscall number, never kept across a sys)
   8 7 PROF-ARENA LDR,  8 pdone CBZ,                 \ no index: the sample is still counted
   LPROFFIND LABEL@ BL,                              \ the handler never returns, so x30 needs no frame
   6 pnew CBZ,
   12 6 ENT-IDX LDR,                                 \ the record owning the pc: bump its counter
   14 PROF-CNT-VA LIT64,  13 12 3 LSLI,  14 14 13 ADD,
   12 14 0 LDR,  12 12 1 ADDI,  12 14 0 STR,
   10 6 ENT-IDX LDR,  C-PROF-INCL-ONCE                \ a word is inside itself
   22 6 ENT-IDX LDR,
   C-PROF-WALK
   psig B,
   pnew LBL,                                         \ Habu code the index does not name
   22 0 MOVN,                                        \ no record of its own: callers only
   11 8 ARN-HI LDR,  9 11 CMP,  C-CC pother BCOND,   \ below the index's high mark: engine helpers, main loop, a gap
   C-PROF-DEFER                                      \ at or above it: compiled after prof-on built the index
   C-PROF-WALK
   psig B,
   pother LBL,
   12 7 PROF-OTHER LDR,  12 12 1 ADDI,  12 7 PROF-OTHER STR,
   C-PROF-WALK
   psig B,
   pdone LBL,                                        \ no arena at all: count and leave
   12 7 PROF-OTHER LDR,  12 12 1 ADDI,  12 7 PROF-OTHER STR,
   psig B,
   pforeign LBL,
   17 0 MOVZ,                                        \ not Habu code: no search, no report from here
   12 7 PROF-FOREIGN LDR,  12 12 1 ADDI,  12 7 PROF-FOREIGN STR,
   psig LBL,
   10 7 PROF-TOT LDR,  10 10 1 ADDI,  10 7 PROF-TOT STR,
   11 7 PROF-LIM LDR,  11 pexit CBZ,                 \ limit 0: sample until prof-off
   10 11 CMP,  C-LT pexit BCOND,
   17 pexit CBZ,                                     \ the limit reached on a foreign sample: the next Habu sample reports
   LPROFDUMP LABEL@ BL,  0 99 MOVZ,  NR-EXIT-GROUP SYS,
   pexit LBL,  0 4 0 ADDI,  NR-SIGRETURN SYS, ;

private

\ The handler's alternate stack: mapped on the first prof-on of the process,
\ kept in the band, registered with sigaltstack before every sigaction so the
\ SA_ONSTACK handler never runs on the interrupted program's stack.
: C-PROF-ALTSTACK ( -- )
   LBL LBL LBL LBL LBL {: have mok pmsg sok smsg :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-STACK LDR,  9 have CBNZ,
      0 0 MOVZ,  1 PROF-STACK-BYTES LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      C-CC mok BCOND,
         1 pmsg ADR,  2 PROFMMAPMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      pmsg LBL,  S\" hb: prof-on: cannot map the handler stack\n" BYTES,   \ BYTES, pads the stream back to a word
      mok LBL,
      7 PROF-BAND-VA LIT64,  0 7 PROF-STACK STR,
   have LBL,
   7 PROF-BAND-VA LIT64,
   SP SP 32 SUBI,
   9 7 PROF-STACK LDR,  9 SP 0 STR,
   HB-TARGET-LINUX? IF
      10 0 MOVZ,  10 SP 8 STR,                       \ ss_flags (int) and its padding
      10 PROF-STACK-BYTES LIT64,  10 SP 16 STR,      \ ss_size
   ELSE
      10 PROF-STACK-BYTES LIT64,  10 SP 8 STR,       \ Darwin: ss_size second
      10 0 MOVZ,  10 SP 16 STR,                      \ ss_flags third
   THEN
   0 SP 0 ADDI,  1 0 MOVZ,  NR-SIGALTSTACK SYS,
   C-CC sok BCOND,                                   \ a refused alternate stack would leave the handler on the interrupted one: fail closed, named
      1 smsg ADR,  2 PROFSTKMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
      smsg LBL,  S\" hb: prof-on: cannot install the handler stack\n" BYTES,
   sok LBL,
   SP SP 32 ADDI, ;

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

\ x0 = arena: the interval prof-rate left there, or the 1000 us default, which
\ prof-on writes back so the report states the rate it actually sampled at.
: C-PROF-TIMER-FRAME ( -- )
   LBL {: dflt :}
   10 0 ARN-USEC LDR,  10 dflt CBNZ,  10 1000 MOVZ,
   dflt LBL,  10 0 ARN-USEC STR,
   SP SP 32 SUBI,
   9 0 MOVZ,   9 SP 0 STR,  10 SP 8 STR,
   9 SP 16 STR,  10 SP 24 STR, ;

: C-PROF-TIMER ( -- )
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS, ;

: C-PROF-TIMER-DONE ( -- )
   SP SP 32 ADDI, ;

\ The arena, mapped on the first prof-on of the process and kept in the band.
\ A refused mapping is named and fatal for the same reason the handler stack is:
\ a profiler that silently kept the old dictionary walk would report numbers
\ nobody could trust.
: C-PROF-ARENA-MAP ( -- )
   LBL LBL LBL {: have mok amsg :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARENA LDR,  9 have CBNZ,
      0 0 MOVZ,  1 ARN-BYTES LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      C-CC mok BCOND,
         1 amsg ADR,  2 PROFARNMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      amsg LBL,  S\" hb: prof-on: cannot map the profiler arena\n" BYTES,
      mok LBL,
      7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA STR,
   have LBL, ;

\ One index entry per live record with code of its own, in dictionary order.
\ x0 = arena; leaves x11 = the entry count and fills the header's lo/hi marks.
\ A record whose wordlist cell is DICT-WL:NAMESPACE (-1) or DICT-WL:RETIRED (-2)
\ owns no code, and neither does one whose code span is empty; the +2 test names
\ both of those wordlists at once.
: C-PROF-INDEX-BUILD ( -- )
   LBL LBL LBL LBL LBL {: bl bnext bdone blo bhi :}
   9 ARN-IDX LIT64,  9 0 9 ADD,
   11 0 MOVZ,  6 0 MOVZ,  5 DBASE 0 ADDI,
   1 0 MOVN,  2 0 MOVZ,                              \ lo = all ones, hi = 0
   bl LBL,
      6 NDICT CMP,  C-CS bdone BCOND,
      12 5 DICT-WL-OFF LDR,  12 12 2 ADDI,  12 2 CMPI,  C-CC bnext BCOND,
      13 5 8 LDR,  C-PROF-SPAN-BYTES  13 bnext CBZ,
      12 5 0 LDR,  12 bnext CBZ,
      12 9 ENT-START STR,
      14 12 13 ADD,  14 9 ENT-END STR,
      6 9 ENT-IDX STR,
      15 0 MOVZ,  15 9 ENT-INCL STR,
      9 9 PROF-ENT ADDI,  11 11 1 ADDI,
      12 1 CMP,  C-CS blo BCOND,  1 12 0 ADDI,
      blo LBL,  14 2 CMP,  C-LS bhi BCOND,  2 14 0 ADDI,
      bhi LBL,
   bnext LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  bl B,
   bdone LBL,
   11 0 ARN-COUNT STR,  1 0 ARN-LO STR,  2 0 ARN-HI STR,
   NDICT 0 ARN-NDICT STR, ;

\ The counters a phase accumulates. Cleared by prof-on and prof-reset, and NEVER
\ by C-PROF-INDEX-BUILD, which a report runs again over a phase already counted.
: C-PROF-COUNTERS-CLEAR ( -- )
   LBL LBL LBL LBL {: il idone el edone :}
   15 0 MOVZ,
   15 0 ARN-NEW STR,  15 0 ARN-DROP STR,  15 0 ARN-FRAMES STR,
   15 0 ARN-SPILL STR,  15 0 ARN-DEFER STR,
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

\ Bottom-up merge sort of x11 entries by start address, arena in x0, using the
\ second half of the arena as the other side of each pass. Stable, so records
\ that share a start stay in dictionary order and the search's "last start <= pc"
\ answer is the same one an exhaustive scan in record order reports. O(n log n)
\ once per prof-on rather than O(n) in every tick.
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
      3 9 0 ADDI,  9 10 0 ADDI,  10 3 0 ADDI,        \ this pass's output is the next pass's input
      12 12 1 LSLI,  pass B,
   done LBL,
   C-PROF-SORT-LAND
   skip LBL, ;

\ x8 = arena, x10 = a record index: one inclusive sample for that record.
\
\ WHY INCLUSIVE COUNTS LIVE IN TWO PLACES. The handler takes them in the index
\ ENTRY, where the entry it just searched is already in a register and the bump
\ costs three instructions - the walk does this once per stack frame, about
\ fourteen times a tick, so it is the profiler's hottest store. But a report
\ REBUILDS the index to name code compiled after prof-on, and a rebuild moves
\ every entry. So the durable home is this per-RECORD array, and C-PROF-INCL-FOLD
\ empties the entries into it immediately before each rebuild. A row prints the
\ sum of the two, which is correct whether or not a sync has run.
\ Clobbers x11 and x12, and preserves x10 for the caller.
: C-PROF-INCL+ ( -- )
   11 ARN-INCL LIT64,  11 8 11 ADD,  12 10 3 LSLI,  11 11 12 ADD,
   12 11 0 LDR,  12 12 1 ADDI,  12 11 0 STR, ;

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

\ x0 = arena. Replay the deferred samples through the rebuilt index: the pc names
\ a record now and the interrupted x30 names the caller. A pc that still resolves
\ to nothing is counted in ARN-NEW, which is what the report's "new" field means
\ after a sync - code that ran, was sampled, and belongs to no live record.
\
\ INCLUSIVE IS COUNTED ONCE. The handler already took an inclusive sample for
\ every frame the OLD index could name, so replay adds one only for a record that
\ index did not reach - start at or above ARN-OLDHI. Counting a caller twice
\ would be a worse answer than the one this leaves.
: C-PROF-REPLAY ( -- )
   LBL LBL LBL LBL LBL LBL {: rl rdone rmiss rnocall rgo rnext :}
   8 0 0 ADDI,                                       \ LPROFFIND and LPROFEDGE take the arena in x8
   24 8 ARN-DEFER LDR,
   23 ARN-DEF LIT64,  23 8 23 ADD,
   rl LBL,
      24 rdone CBZ,
      9 23 0 LDR,
      LPROFFIND LABEL@ BL,
      6 rmiss CBZ,
      22 6 ENT-IDX LDR,
      14 PROF-CNT-VA LIT64,  13 22 3 LSLI,  14 14 13 ADD,
      12 14 0 LDR,  12 12 1 ADDI,  12 14 0 STR,      \ the exclusive sample this stands for
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
      rmiss LBL,  12 8 ARN-NEW LDR,  12 12 1 ADDI,  12 8 ARN-NEW STR,
      rnext LBL,  23 23 PROF-DEFER-ENT ADDI,  24 24 1 SUBI,  rl B,
   rdone LBL,
   12 0 MOVZ,  12 8 ARN-DEFER STR, ;

\ LPROFSYNC ( -- ): rebuild the pc index from the dictionary as it now stands and
\ replay the deferred samples through it. This is the whole answer to code
\ compiled after prof-on: the report is the first moment at which every word the
\ phase compiled exists, so it is the moment to name them.
\
\ IT RUNS FROM HABU CODE ONLY, never from the handler's auto-report at the limit:
\ the build reads NDICT, and the handler validates the interrupted x20 and x26
\ but not x27. An auto-report therefore leaves its deferred samples in the
\ header's "defer" bucket, which is exactly what that field says.
: EMIT-PROFSYNC ( -- )
   LPROFSYNC LABEL@ LBL,
   LBL {: nosync :}
   SP SP 96 SUBI,
   30 SP 0 STR,  19 SP 8 STR,  20 SP 16 STR,  21 SP 24 STR,  22 SP 32 STR,
   23 SP 40 STR,  24 SP 48 STR,  25 SP 56 STR,  17 SP 64 STR,
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,  0 nosync CBZ,
   9 0 ARN-HI LDR,  9 0 ARN-OLDHI STR,
   C-PROF-INCL-FOLD
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-REPLAY
   nosync LBL,
   30 SP 0 LDR,  19 SP 8 LDR,  20 SP 16 LDR,  21 SP 24 LDR,  22 SP 32 LDR,
   23 SP 40 LDR,  24 SP 48 LDR,  25 SP 56 LDR,  17 SP 64 LDR,
   SP SP 96 ADDI,  RET, ;

\ prof-on runs as Habu code, where DBASE and NDICT are live: it records the base
\ the handler will trust, clears one counter per current record, and builds the
\ index the handler searches.
: BPROF-ON ( -- )
   LBL LBL {: zl zd :}
   7 PROF-BAND-VA LIT64,
   A G-POP  A 7 PROF-LIM STR,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   DBASE 7 PROF-DBASE STR,
   14 PROF-CNT-VA LIT64,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   C-PROF-ARENA-MAP
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-COUNTERS-CLEAR
   C-PROF-CALL-CLEAR
   C-PROF-ALTSTACK
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,   \ the stack syscalls above own x0
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE
   7 PROF-BAND-VA LIT64,  9 1 MOVZ,  9 7 PROF-ARMED STR, ;

\ Disarm the interval timer. Clobbers x0-x2 and x9.
: C-PROF-TIMER-STOP ( -- )
   SP SP 32 SUBI,
   9 0 MOVZ,  9 SP 0 STR,  9 SP 8 STR,  9 SP 16 STR,  9 SP 24 STR,
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS,
   SP SP 32 ADDI, ;

\ Re-arm it at the recorded interval. Clobbers x0-x2, x7, x9 and x10.
: C-PROF-TIMER-START ( -- )
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE ;

\ A REPORT NEVER SAMPLES ITSELF. It reads PROF-TOT once and then walks every
\ counter; a tick in between would leave the header's own identity - words +
\ other + new + foreign == samples - false by one. The report also owns x20, so
\ its own ticks read as a foreign context and land in a different bucket than
\ the total they were counted in. So the clock stops for the walk and starts
\ again only when prof-off has not already stopped it; the phase loses at most
\ one interval.
: C-PROF-REPORT-CALL ( label -- ) {: rep:label :}
   LBL {: stopped :}
   SP SP 16 SUBI,  30 SP 0 STR,
   C-PROF-TIMER-STOP
   LPROFSYNC LABEL@ BL,
   rep BL,
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARMED LDR,
   9 stopped CBZ,
   C-PROF-TIMER-START
   stopped LBL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-off stops the clock and nothing else: the handler stays installed, the
\ index and every counter stay exactly as the last sample left them, so the phase
\ that was profiled can be reported afterwards - which is the whole point of
\ having a stop word rather than the old dump-and-exit.
: BPROF-OFF ( -- )
   C-PROF-TIMER-STOP
   7 PROF-BAND-VA LIT64,  9 0 MOVZ,  9 7 PROF-ARMED STR, ;

\ prof-reset clears the counts and keeps the index, so a second phase can be
\ measured without paying for the sort again.
: BPROF-RESET ( -- )
   LBL LBL LBL {: zl zd noarena :}
   7 PROF-BAND-VA LIT64,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   14 PROF-CNT-VA LIT64,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   0 7 PROF-ARENA LDR,  0 noarena CBZ,
   C-PROF-COUNTERS-CLEAR
   C-PROF-CALL-CLEAR
   noarena LBL, ;

: BPROF-REPORT ( -- )  LPROFDUMP LABEL@ C-PROF-REPORT-CALL ;

: BPROF-JSON ( -- )  LPROFJSON LABEL@ C-PROF-REPORT-CALL ;

\ prof-row ( n -- ): the row for dictionary record n, whatever its rank. The
\ name lookup stays in the caller, where XREF already answers it.
: BPROF-ROW ( -- )
   LBL {: stopped :}
   SP SP 16 SUBI,  30 SP 0 STR,
   6 G-POP  6 SP 8 STR,
   C-PROF-TIMER-STOP
   LPROFSYNC LABEL@ BL,
   6 SP 8 LDR,
   LPROFROW LABEL@ BL,
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARMED LDR,
   9 stopped CBZ,
   C-PROF-TIMER-START
   stopped LBL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-rate sets the interval the NEXT prof-on arms, rather than re-arming here:
\ a rate written while no handler is installed would hand the process a SIGALRM
\ it has no handler for.
: BPROF-RATE ( -- )
   C-PROF-ARENA-MAP
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   A G-POP  A 0 ARN-USEC STR, ;

\ prof-pc>rec ( pc -- n ): the record index the armed index gives that pc, or -1.
\ It runs the handler's own search, so a test that compares it with an exhaustive
\ scan of the live dictionary is testing the attribution the handler performs.
: BPROF-PCREC ( -- )
   LBL LBL {: miss out :}
   SP SP 16 SUBI,  30 SP 0 STR,
   9 G-POP
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,  8 miss CBZ,
   LPROFFIND LABEL@ BL,
   6 miss CBZ,
   9 6 ENT-IDX LDR,  out B,
   miss LBL,  9 0 MOVN,
   out LBL,  9 G-PUSH
   30 SP 0 LDR,  SP SP 16 ADDI, ;

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L
   s" prof-off" ['] BPROF-OFF FPRIM-L  s" prof-reset" ['] BPROF-RESET FPRIM-L
   s" prof-rate" ['] BPROF-RATE FPRIM-L  s" prof-json" ['] BPROF-JSON FPRIM-L
   s" prof-row" ['] BPROF-ROW FPRIM-L
   s" prof-pc>rec" ['] BPROF-PCREC FPRIM-L ;

: EMIT-PROF-REPORTS ( -- )
   EMIT-PROFNUM  EMIT-PROFPCT  EMIT-PROFNAME  EMIT-PROFQUAL  EMIT-PROFSYNC
   false EMIT-PROFREP  true EMIT-PROFREP  EMIT-PROFROW ;

;using

;package
