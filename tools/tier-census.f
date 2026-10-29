\ tier-census.f - per-word native code size at one compiler tier.
\ Run: bin/hb --load tools/tier-census.f -- <tier> <report> <corpus.f>...
\ One line per word the load defined, then the same columns totalled on a `T`
\ line with the word count first:
\     W <name> <bytes> <instructions> <bl> <ldr-sp> <str-sp> <mov> <movk>
\ tools/tier-census-join.f puts two reports side by side.

\ Only this require (`set-tier` is an engine word), loaded before MAIN takes its
\ `ndict@` mark: a file this tool loads, like one the engine provides
\ (src/lib/string.f), is already in the dictionary when the corpus asks and
\ weighs nothing (src/lib/fmt.f and src/lib/fs.f are corpus, hence the local
\ decimal writer and line buffer). Measure code-bytes.f from a tree where it is
\ not this tool's dependency.
require src/habu/code-bytes.f

package TIER-CENSUS
private

$4000 constant BUF-CAP
$3F00 constant BUF-HIGH            \ flush above this, leaving room for a line
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
\ The unscaled signed field holds the writeback frame transfers
\ (`str x30,[sp,#-16]!`, `ldr x30,[sp],#16`); both spellings count in one column.
$FFE00000 constant LSU-MASK
$F8400000 constant LDU-BITS        \ ldur/ldr xT,[xN],#imm9 / [xN,#imm9]!
$F8000000 constant STU-BITS
$3E0 constant RN-MASK              \ bits 9:5, the base register
31 constant RN-SP                  \ x31 in a load/store base is sp
$FFE0FFE0 constant MOV-MASK        \ orr xD,xzr,xS with both registers free
$AA0003E0 constant MOV-BITS
$FF800000 constant MOVK-MASK
$F2800000 constant MOVK-BITS       \ movk xD,#imm16,lsl #s, the 64-bit form

create BUF BUF-CAP allot
variable BUF-N
variable FD

create DIG DIG-CAP allot
variable DEC-V
variable DEC-K

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
\ CODE-BYTES:AT (src/habu/code-bytes.f) bounds the span against the running
\ image's code, so a record whose span is not code is refused, not tallied. SCAN
\ takes the span once, so the bound costs one check per word.
\ Counts: bytes and instructions over the whole baked span (XREF-CODE-BYTES);
\ bl, every call the tier did not fold; ldr-sp and str-sp, 64-bit loads and
\ stores based on sp in both scaled and unscaled spellings; mov, `orr xD,xzr,xS`,
\ the register twin of the frame copy regalloc.f MB-IDENTITY-COPY? drops; movk,
\ every 64-bit keep-move (an absolute address has three, a compact DATA carrier
\ two, scalars some), so instruction traffic, not address sites.

: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: BL? ( n -- bool ) BL-MASK and BL-BITS = ;

: SP-BASED? ( n -- bool ) RN-MASK and 5 rshift RN-SP = ;

: LDR-SP? ( n -- bool ) {: w :}
   w SP-BASED? 0= if false exit then
   w LS-MASK and LDR-BITS = if true exit then
   w LSU-MASK and LDU-BITS = ;

: STR-SP? ( n -- bool ) {: w :}
   w SP-BASED? 0= if false exit then
   w LS-MASK and STR-BITS = if true exit then
   w LSU-MASK and STU-BITS = ;

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
   start bytes CODE-BYTES:AT drop {: code:ptr :}
   0 W-BL !  0 W-LDSP !  0 W-STSP !  0 W-MOV !  0 W-MOVK !
   bytes 4 / 0 ?do code i 4 * + W32@ TALLY loop ;

\ ---- names ------------------------------------------------------------------
\ A word's wordlist cell carries its package's id, and a namespace record's
\ start cell carries the id it owns (src/habu/xref.f XREF-FIND-QUALIFIED reads
\ the pair the same way round). Resolving it means a scan, so the last answer
\ is kept: the records of one package arrive in a run.

: NS-INDEX ( n -- n ) {: wid :}
   ndict@ 1 - begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-NAMESPACE-WL = if
         dup XREF-REC XREF-PKG-PUBLIC wid = if exit then
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

\ The path goes through the engine's PATH0, not FS: this file may require
\ nothing the corpus could want to weigh (see the header).
: OPEN-REPORT ( ptr u8 n -- )
   PATH0 O-WRITE-NEW MODE-0644 open FD !
   FD @ 0 < if s" tier-census: cannot open report" ERR-RC die then ;

: TIER-ARG ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 <> if USAGE then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if USAGE then
   t ;

\ `require`, not `include`: the registry compiles a shared dependency once (a
\ second load would die of a duplicate definition), and a file the engine
\ carries baked (cold prefix, src/core, src/compiler) is skipped and adds no
\ rows, which the `T` word count shows. `required` is the word `require`
\ reaches, so an argument string needs no `evaluate` or its trust row.
: LOAD-CORPUS ( -- )
   script-argc 2 ?do i script-argv$ required loop ;

: HEADER ( n -- ) {: tier :}
   s" # tier " EMIT$ tier EMIT-DEC LINE-END
   script-argc 2 ?do
      s" # corpus " EMIT$ i script-argv$ EMIT$ LINE-END
   loop ;

public

\ The tier is selected and the report opened before the first corpus token is
\ read, and nothing is written until the load returns, so a corpus that dies
\ mid-load leaves an empty report. One tier per process: `set-tier` is
\ engine-global and a file loads once.
: MAIN ( -- )
   script-argc 3 < if USAGE then
   0 script-argv$ TIER-ARG {: tier :}
   -1 WID-CACHE !  -1 WID-CACHE-IX !
   1 script-argv$ OPEN-REPORT
   tier set-tier
   ndict@ {: first :}
   LOAD-CORPUS
   tier HEADER
   first ndict@ REPORT-RANGE
   TOTALS
   FLUSH
   FD @ close ;

;package

TIER-CENSUS:MAIN
