\ tier-census.f - per-word native code size, at one compiler tier.
\
\ Run:
\     bin/hb --load tools/tier-census.f -- <tier> <report> <corpus.f>...
\
\ Selects the tier, loads the corpus through the engine's own `required`, and
\ writes one line per word the load defined:
\
\     W <name> <bytes> <instructions> <bl> <ldr-sp> <str-sp> <mov> <movk>
\
\ and then the same columns on a `T` line, with the word count first.
\ tools/tier-census-join.f puts two such reports side by side.
\
\ WHY IT LOADS REAL FILES AND NOT A MODEL OF THEM. A per-word size that was
\ measured on a renamed copy of a body would answer for the copy: the calls
\ tier 1 folds, the locals it coalesces and the frame it builds all depend on
\ what the body actually names. The corpus goes through `require`, the load
\ path every other program uses, so the numbers are the engine's own.
\
\ WHY `require` AND NOT `include`. The registry is what keeps a shared
\ dependency single: two corpus files that both want lib/fs.f must compile it
\ once, or the second load dies of a duplicate definition and the census stops
\ half written. It also means a file the engine already carries baked - the
\ cold prefix, src/core, src/compiler - is skipped rather than recompiled, so
\ such a file contributes no rows. The word count on the `T` line is what shows
\ that: a corpus entry already in the engine moves it by nothing.
\
\ WHY TWO PROCESSES AND NOT ONE. `set-tier` is engine-global and a file loads
\ once: measuring both tiers in one process would mean unloading a whole
\ corpus - every package, every data record - and trusting that the unload left
\ nothing behind. Two processes each get a pristine engine, and the tier is
\ selected before the first corpus token is read.
\
\ WHY THIS FILE REQUIRES NOTHING. lib/string.f and lib/fmt.f are the string and
\ formatting corpus this census most wants to weigh, and a tool that loaded
\ them for its own output could not: they would already be in the dictionary
\ when the corpus asked for them. The decimal writer and the line buffer below
\ are the price of being able to measure them.
\
\ WHAT THE COUNTS MEAN. Bytes and instructions are the whole baked span
\ (XREF-CODE-BYTES). `bl` counts branch-with-link instructions - every call the
\ tier did not fold. `ldr-sp` and `str-sp` count 64-bit loads and stores whose
\ base register is the stack pointer: the frame traffic a register allocator
\ spends on spills, reloads and locals. `mov` counts register-to-register moves
\ (`orr xD,xzr,xS`), which is what a copy between two allocation classes costs
\ once neither end lives in the frame - the register-level twin of the
\ frame-slot copy that src/compiler/native/regalloc.f MB-IDENTITY-COPY? already
\ drops. `movk` counts the 64-bit keep-moves that finish a relocatable address:
\ docs/compiler-ir-design.md pins every such address to a fixed four-instruction
\ MOVZ/MOVK stencil so a later pass can recognize it, so one inline pointer
\ costs 16 bytes and `movk` over three is the number of them.

package TIER-CENSUS
private

$4000 constant BUF-CAP
$3F00 constant BUF-HIGH            \ flush above this, leaving room for a line
1024 constant PATH-CAP
32 constant DIG-CAP
64 constant USAGE-RC               \ sysexits EX_USAGE
74 constant ERR-RC                 \ sysexits EX_IOERR
$0A constant LF
$20 constant SP
$3A constant COLON
48 constant ZERO-C
1537 constant O-WRITE-NEW          \ the engine's portable O_WRONLY|O_CREAT|O_TRUNC
420 constant MODE-0644

$FC000000 constant BL-MASK
$94000000 constant BL-BITS
$FFC00000 constant LS-MASK
$F9400000 constant LDR-BITS        \ ldr xT,[xN,#imm12]
$F9000000 constant STR-BITS        \ str xT,[xN,#imm12]
$3E0 constant RN-MASK              \ bits 9:5, the base register
31 constant RN-SP                  \ x31 in a load/store base is sp
$FFE0FFE0 constant MOV-MASK        \ orr xD,xzr,xS with both registers free
$AA0003E0 constant MOV-BITS
$FF800000 constant MOVK-MASK
$F2800000 constant MOVK-BITS       \ movk xD,#imm16,lsl #s, the 64-bit form

create BUF BUF-CAP allot
variable BUF-N
variable FD

create PATHZ PATH-CAP 1 + allot

create DIG DIG-CAP allot
variable DEC-V
variable DEC-K

variable CODE-A                    \ code address under the instruction walk
variable WORDS
variable T-BYTES
variable T-INSTR
variable T-BL
variable T-LDSP
variable T-STSP
variable T-MOV
variable T-MOVK

variable W-BL
variable W-LDSP
variable W-STSP
variable W-MOV
variable W-MOVK

variable WID-CACHE                 \ last wordlist id resolved to a package name
variable WID-CACHE-IX              \ its namespace record index, -1 when none

\ ---- the one trust boundary -------------------------------------------------
\ `set-tier` is refused inside a plain checked body. It gets one row with
\ nothing else in it, so the unchecked surface is the primitive and not the
\ census around it.

TRUSTED: SELECT-TIER ( n -- ) set-tier ;

\ ---- output ----------------------------------------------------------------

: FLUSH ( -- )
   BUF-N @ 0= if exit then
   FD @ BUF BUF-N @ write BUF-N @ <> if
      s" tier-census: report write failed" ERR-RC die
   then
   0 BUF-N ! ;

: EMIT-C ( n -- ) {: c :}
   BUF-N @ BUF-CAP >= if FLUSH then
   c BUF BUF-N @ + c!
   BUF-N @ 1 + BUF-N ! ;

: EMIT$ ( ptr u8 n -- ) {: a:ptr u :}
   u 0 ?do a i + c@ EMIT-C loop ;

: EMIT-DEC ( n -- )
   DEC-V !
   DEC-V @ 0= if ZERO-C EMIT-C exit then
   0 DEC-K !
   begin DEC-V @ 0 > while
      DEC-V @ 10 mod ZERO-C + DIG DEC-K @ + c!
      DEC-K @ 1 + DEC-K !
      DEC-V @ 10 / DEC-V !
   repeat
   begin DEC-K @ 0 > while
      DEC-K @ 1 - DEC-K !
      DIG DEC-K @ + c@ EMIT-C
   repeat ;

: EMIT-FIELD ( n -- ) SP EMIT-C EMIT-DEC ;

: LINE-END ( -- )
   LF EMIT-C
   BUF-N @ BUF-HIGH > if FLUSH then ;

\ ---- the instruction walk ---------------------------------------------------
\ The record's code span is an address the engine hands back as a number; the
\ ptr-field reinterpretation below is the only way a checked body can read the
\ bytes at it, and tools/jitdump-core.f reads baked code the same way.

: CODE-P ( -- ptr u8 )
   CODE-A 0 ptr-field @ ;

: W32@ ( n -- n ) {: off :}
   CODE-P off + c@
   CODE-P off 1 + + c@ 8 lshift or
   CODE-P off 2 + + c@ 16 lshift or
   CODE-P off 3 + + c@ 24 lshift or ;

: BL? ( n -- bool ) BL-MASK and BL-BITS = ;

: SP-BASED? ( n -- bool ) RN-MASK and 5 rshift RN-SP = ;

: LDR-SP? ( n -- bool ) {: w :}
   w LS-MASK and LDR-BITS = w SP-BASED? and ;

: STR-SP? ( n -- bool ) {: w :}
   w LS-MASK and STR-BITS = w SP-BASED? and ;

: MOV? ( n -- bool ) MOV-MASK and MOV-BITS = ;

: MOVK? ( n -- bool ) MOVK-MASK and MOVK-BITS = ;

: TALLY ( n -- ) {: w :}
   w BL? if W-BL @ 1 + W-BL ! then
   w LDR-SP? if W-LDSP @ 1 + W-LDSP ! then
   w STR-SP? if W-STSP @ 1 + W-STSP ! then
   w MOV? if W-MOV @ 1 + W-MOV ! then
   w MOVK? if W-MOVK @ 1 + W-MOVK ! then ;

: SCAN ( n n -- )                  \ start, bytes -> the five per-word counts
   {: start bytes :}
   start CODE-A !
   0 W-BL !  0 W-LDSP !  0 W-STSP !  0 W-MOV !  0 W-MOVK !
   bytes 4 / 0 ?do i 4 * W32@ TALLY loop ;

\ ---- names ------------------------------------------------------------------
\ A word's wordlist cell carries its package's id, and a namespace record's
\ start cell carries the id it owns (src/habu/xref.f XREF-FIND-QUALIFIED reads
\ the pair the same way round). Resolving it means a scan, so the last answer
\ is kept: the records of one package arrive in a run.

: NS-INDEX ( n -- n ) {: wid :}
   ndict@ 1 - begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-NAMESPACE-WL = if
         dup XREF-REC XREF-START wid = if exit then
      then
      1 -
   repeat ;

: PKG-INDEX ( n -- n ) {: wid :}
   wid WID-CACHE @ = if WID-CACHE-IX @ exit then
   wid NS-INDEX {: ix :}
   wid WID-CACHE !  ix WID-CACHE-IX !
   ix ;

: EMIT-NAME ( ptr n -- ) {: rec:ptr :}
   rec XREF-WORDLIST {: wid :}
   wid 0<> if
      wid PKG-INDEX {: ix :}
      ix 0 >= if ix XREF-REC XREF-NAME$ EMIT$  COLON EMIT-C then
   then
   rec XREF-NAME$ EMIT$ ;

\ ---- the census -------------------------------------------------------------

: MEASURABLE? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-RETIRED? if 0 0= 0= exit then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if 0 0= 0= exit then
   rec XREF-CODE-BYTES 0 > ;

: ONE ( n -- ) {: ix :}
   ix XREF-REC {: rec:ptr :}
   rec MEASURABLE? 0= if exit then
   rec XREF-CODE-BYTES {: bytes :}
   rec XREF-START bytes SCAN
   s" W " EMIT$
   rec EMIT-NAME
   bytes EMIT-FIELD
   bytes 4 / EMIT-FIELD
   W-BL @ EMIT-FIELD
   W-LDSP @ EMIT-FIELD
   W-STSP @ EMIT-FIELD
   W-MOV @ EMIT-FIELD
   W-MOVK @ EMIT-FIELD
   LINE-END
   WORDS @ 1 + WORDS !
   T-BYTES @ bytes + T-BYTES !
   T-INSTR @ bytes 4 / + T-INSTR !
   T-BL @ W-BL @ + T-BL !
   T-LDSP @ W-LDSP @ + T-LDSP !
   T-STSP @ W-STSP @ + T-STSP !
   T-MOV @ W-MOV @ + T-MOV !
   T-MOVK @ W-MOVK @ + T-MOVK ! ;

: REPORT-RANGE ( n n -- ) {: first last :}
   last first ?do i ONE loop ;

: TOTALS ( -- )
   s" T" EMIT$
   WORDS @ EMIT-FIELD
   T-BYTES @ EMIT-FIELD
   T-INSTR @ EMIT-FIELD
   T-BL @ EMIT-FIELD
   T-LDSP @ EMIT-FIELD
   T-STSP @ EMIT-FIELD
   T-MOV @ EMIT-FIELD
   T-MOVK @ EMIT-FIELD
   LINE-END ;

\ ---- arguments and the load -------------------------------------------------

: USAGE ( -- )
   s" usage: bin/hb --load tools/tier-census.f -- <tier> <report> <corpus.f>..."
   USAGE-RC die ;

: ZPATH ( ptr u8 n -- ) {: a:ptr u :}
   u PATH-CAP > if s" tier-census: path too long" ERR-RC die then
   u 0 ?do a i + c@ PATHZ i + c! loop
   0 PATHZ u + c! ;

: OPEN-REPORT ( ptr u8 n -- )
   ZPATH
   PATHZ O-WRITE-NEW MODE-0644 open FD !
   FD @ 0 < if s" tier-census: cannot open report" ERR-RC die then ;

: TIER-ARG ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 <> if USAGE then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if USAGE then
   t ;

\ `required` is the registry-aware load as a plain word on a string - what
\ `require <path>` reaches once its path token is parsed. An argument is
\ already a string, so calling it directly spares the census a source line to
\ build, an `evaluate` to run it, and the trust row that `evaluate` would need.
: LOAD-CORPUS ( -- )
   script-argc 2 ?do i script-argv$ required loop ;

: HEADER ( n -- ) {: tier :}
   s" # tier " EMIT$ tier EMIT-DEC LINE-END
   script-argc 2 ?do
      s" # corpus " EMIT$ i script-argv$ EMIT$ LINE-END
   loop ;

public

\ The corpus loads AFTER the report is open and the tier is selected, and
\ nothing is written to the report until the load has returned - so a corpus
\ that dies mid-load leaves an empty report rather than a plausible one, and
\ the dictionary mark is taken before the first corpus token is read.
: MAIN ( -- )
   script-argc 3 < if USAGE then
   0 script-argv$ TIER-ARG {: tier :}
   -1 WID-CACHE !  -1 WID-CACHE-IX !
   1 script-argv$ OPEN-REPORT
   tier SELECT-TIER
   ndict@ {: first :}
   LOAD-CORPUS
   tier HEADER
   first ndict@ REPORT-RANGE
   TOTALS
   FLUSH
   FD @ close ;

;package

TIER-CENSUS:MAIN
