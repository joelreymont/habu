\ codegen-workload-scan.f - read the machine code this process is running and
\ answer two questions about any published word: who CALLS it, and would the
\ engine COPY it into a caller instead. One concern: the compiled code as
\ evidence.
\
\ WHY A WORKLOAD MEASUREMENT NEEDS THIS FIRST. A word can be republished with
\ better code and nothing in the running system gets faster, because the callers
\ that mattered do not call it any more: src/habu/habu2.f, C-CALL, copies a small
\ enough callee's body into its caller at the moment the caller is compiled, and
\ a body that was copied leaves no call instruction behind to redirect. So
\ "migrating TAG made the checker faster" is a claim that has to be checked
\ against the emitted code before it is timed, and this file is where it is
\ checked.
\
\ THE ENGINE'S RULE, AS THE ENGINE WRITES IT. C-CALL is handed the callee's code
\ address and the length its dictionary record holds, and decides between one
\ direct call instruction and a verbatim copy of the body. ENGINE-COPIES? below
\ is that decision, over the same bytes:
\
\   1. If the callee's first instruction is the compiled-word prologue
\      (`sub sp, sp, #16`, C-CALL-PROLOGUE-INSTR), the body is the record minus
\      that prologue and minus the two-instruction epilogue - and the record must
\      be no longer than INL-MAX plus those sixteen bytes.
\   2. Otherwise the body is the whole record, the record must be no longer than
\      INL-MAX, and the word just past it must be a `ret`: a definition whose
\      return slot was patched (`does>`) is never copied.
\   3. Every instruction of the body must be one that survives being moved.
\      C-CALL-REJECT-UNSAFE names them: any pc-relative branch (`bl`, `b`,
\      `b.cond`, `cbz`, `cbnz`, `tbz`, `tbnz`), any register branch (`br`,
\      `blr`), a `ret`, and an `adr`. ONE branch anywhere in a body is enough to
\      make it uncopyable - which is why every word with an `if` or a loop in it
\      is a real call in engine code, and why those are the words a migration can
\      reach.
\
\ The constants are habu2.f's own, and are named here with the spelling that file
\ uses so the two can be read side by side. They are a copy - this file cannot
\ reach into the engine builder's dictionary - and the acceptance suite pins the
\ rule against the live engine rather than against these numbers: it asks the
\ predicate about words whose call sites it also counts, and a disagreement
\ between the rule and the count is a finding.
\
\ AND THE COUNT, WHICH IS THE OTHER HALF. CALL-SITES walks every live dictionary
\ record and counts the `bl` instructions whose target is the subject's own code
\ address. A `bl` carries a signed 26-bit word displacement, so the target is
\ arithmetic on the site's address and not a guess. What the count cannot see is
\ a call made through an execution token (`execute`, a deferred word's `blr`),
\ which is why the invariant the suite pins runs the other way: a body the engine
\ copies has no call site anywhere, and a body it will not copy is free to have
\ none because nothing calls it.
\
\ WHICH RECORDS ARE WALKED. Every record whose wordlist number is a real
\ wordlist. The two negative ones are not: -1 marks a package NAME, whose record
\ carries the package's identity rather than a word's code, and -2 marks a
\ retired record. Reading a package record's `start` as a code address is how an
\ earlier version of this scan walked into address 1 and took the process down.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package CODEGEN-SCAN

\ ---- what this file refuses -------------------------------------------------
public

-7220 constant E-WLSCAN-SUBJECT   \ a named subject word is not in the live dictionary

private

\ ---- reading one instruction out of the code region -------------------------
\ An address arrives as a number - that is what a dictionary record holds and
\ what a branch displacement computes - and a byte read needs a pointer, so the
\ number goes through a cell the checker will hand back as one. This is the same
\ route tools/codegen-compare-test.f reads emitted instructions by.
variable AT

: AT-PTR ( -- ptr u8 )
   AT 0 ptr-field @ ;

: INSN@ ( n -- n ) {: a:n :}
   a AT !
   AT-PTR c@
   AT-PTR 1 + c@ 8 lshift or
   AT-PTR 2 + c@ 16 lshift or
   AT-PTR 3 + c@ 24 lshift or ;

\ ---- the engine's own constants, spelled as src/habu/habu2.f spells them -----
$28 constant INL-MAX                  \ the most bytes of BODY the engine copies
16 constant FRAME-BYTES               \ prologue plus epilogue of a compiled word
4 constant INSN-BYTES

$D10043FF constant C-CALL-PROLOGUE-INSTR
$D65F03C0 constant C-CALL-RET-INSTR
$FC000000 constant C-CALL-B-IMM-MASK
$94000000 constant C-CALL-BL-IMM
$14000000 constant C-CALL-B-IMM
$FF000010 constant C-CALL-B-COND-MASK
$54000000 constant C-CALL-B-COND
$7E000000 constant C-CALL-CBZ-TBZ-MASK
$34000000 constant C-CALL-CBZ
$36000000 constant C-CALL-TBZ
$FFFFFC1F constant C-CALL-BR-MASK
$D63F0000 constant C-CALL-BLR
$D61F0000 constant C-CALL-BR
$1F000000 constant C-CALL-ADR-MASK
$10000000 constant C-CALL-ADR

\ A `bl`'s displacement: 26 bits of signed instruction count.
$03FFFFFF constant IMM26
$02000000 constant IMM26-SIGN
$04000000 constant IMM26-SPAN

: MASKED? ( n n n -- bool ) {: w:n mask:n op:n :}
   w mask and op = ;

public

: BL? ( n -- bool )
   C-CALL-B-IMM-MASK C-CALL-BL-IMM MASKED? ;

\ Where a `bl` at this address goes. The displacement is counted in instructions
\ from the site itself, and its top bit is its sign.
: BL-TARGET ( n n -- n ) {: pc:n w:n :}
   w IMM26 and {: d:n :}
   d IMM26-SIGN and 0<> if d IMM26-SPAN - INSN-BYTES * pc + exit then
   d INSN-BYTES * pc + ;

private

\ One instruction of a candidate body, against C-CALL-REJECT-UNSAFE's list.
: MOVABLE? ( n -- bool ) {: w:n :}
   w C-CALL-B-IMM-MASK C-CALL-BL-IMM MASKED? if false exit then
   w C-CALL-B-IMM-MASK C-CALL-B-IMM MASKED? if false exit then
   w C-CALL-B-COND-MASK C-CALL-B-COND MASKED? if false exit then
   w C-CALL-CBZ-TBZ-MASK C-CALL-CBZ MASKED? if false exit then
   w C-CALL-CBZ-TBZ-MASK C-CALL-TBZ MASKED? if false exit then
   w C-CALL-BR-MASK C-CALL-BLR MASKED? if false exit then
   w C-CALL-BR-MASK C-CALL-BR MASKED? if false exit then
   w C-CALL-RET-INSTR = if false exit then
   w C-CALL-ADR-MASK C-CALL-ADR MASKED? if false exit then
   true ;

\ ---- the live dictionary ----------------------------------------------------
-1 constant NAMESPACE-WL              \ a package name, not a word
0 constant LOWEST-WL

: REC-WL ( n -- n ) {: k:n :}
   k XREF-REC XREF-WORDLIST ;

: REC-START ( n -- n ) {: k:n :}
   k XREF-REC XREF-START ;

: REC-LEN ( n -- n ) {: k:n :}
   k XREF-REC XREF-LEN ;

\ A record that holds a word's code: a real wordlist, and something in it.
: CODED? ( n -- bool ) {: k:n :}
   k REC-WL LOWEST-WL < if false exit then
   k REC-LEN 0 > ;

public

: WORD-REC ( ptr u8 n -- ptr a ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if
      drop E-WLSCAN-SUBJECT throw
   then ;

: WORD-ENTRY ( ptr u8 n -- n )
   WORD-REC XREF-START ;

: WORD-BYTES ( ptr u8 n -- n )
   WORD-REC XREF-LEN ;

: LIVE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u XREF-FIND XREF-FOUND? ;

private

\ ---- the copy rule ----------------------------------------------------------
\ The candidate body, as the two spans C-CALL computes. Both are answered as a
\ half-open [lo, hi) pair; a body the rule has already refused answers an empty
\ one, which the scan below reads as "nothing to copy" without a second flag.
variable LO
variable HI

: NO-BODY ( -- )
   0 LO ! 0 HI ! ;

: PROLOGUE-BODY ( n n -- ) {: s:n len:n :}
   len INL-MAX FRAME-BYTES + > if NO-BODY exit then
   s 8 + LO !  s len + 8 - HI ! ;

: PLAIN-BODY ( n n -- ) {: s:n len:n :}
   len INL-MAX > if NO-BODY exit then
   s len + INSN@ C-CALL-RET-INSTR <> if NO-BODY exit then
   s LO !  s len + HI ! ;

: BODY-SPAN ( n n -- ) {: s:n len:n :}
   len INSN-BYTES < if NO-BODY exit then
   s INSN@ C-CALL-PROLOGUE-INSTR = if s len PROLOGUE-BODY exit then
   s len PLAIN-BODY ;

variable MOVABLE

: SCAN-BODY ( -- )
   true MOVABLE !
   HI @ LO @ - INSN-BYTES / 0 ?do
      LO @ i INSN-BYTES * + INSN@ MOVABLE? 0= if false MOVABLE ! leave then
   loop ;

public

\ Would the engine copy this word's body into a caller it compiles from now on,
\ rather than emit a call to it? This is C-CALL's decision, over C-CALL's bytes.
: ENGINE-COPIES? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u WORD-ENTRY  a u WORD-BYTES  BODY-SPAN
   HI @ LO @ <= if false exit then
   SCAN-BODY
   MOVABLE @ ;

\ How many bytes of body a copy would cost a caller. Zero when the engine would
\ emit a call instead, so a reader does not have to hold the rule in their head
\ to know which of the two a row is.
: COPY-BYTES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u ENGINE-COPIES? 0= if 0 exit then
   HI @ LO @ - ;

private

\ ---- counting the calls -----------------------------------------------------
variable TARGET
variable SITES
variable IN-REC
variable CALLERS

: SITE ( n n -- ) {: pc:n w:n :}
   w BL? 0= if exit then
   pc w BL-TARGET TARGET @ <> if exit then
   SITES @ 1+ SITES !
   IN-REC @ 1+ IN-REC ! ;

: SCAN-SPAN ( n n -- ) {: s:n len:n :}
   len INSN-BYTES / 0 ?do
      s i INSN-BYTES * +  dup INSN@  SITE
   loop ;

: SCAN-REC ( n -- ) {: k:n :}
   k CODED? 0= if exit then
   0 IN-REC !
   k REC-START k REC-LEN SCAN-SPAN
   IN-REC @ 0 > if CALLERS @ 1+ CALLERS ! then ;

: SWEEP ( n -- ) {: t:n :}
   t TARGET !  0 SITES !  0 CALLERS !
   ndict@ 0 ?do i SCAN-REC loop ;

public

\ How many call instructions in the whole live dictionary enter this word.
: CALL-SITES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u WORD-ENTRY SWEEP
   SITES @ ;

\ How many distinct words hold at least one of them.
: CALLERS-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u WORD-ENTRY SWEEP
   CALLERS @ ;

\ How many call instructions this word's own code contains.
: BLS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u WORD-ENTRY {: s:n :}
   a u WORD-BYTES {: len:n :}
   s AT !
   0
   len INSN-BYTES / 0 ?do
      s i INSN-BYTES * + INSN@ BL? if 1+ then
   loop ;

private

variable FOUND
variable WANT

: CALL-SITE? ( n n -- ) {: pc:n w:n :}
   w BL? 0= if exit then
   pc w BL-TARGET WANT @ <> if exit then
   true FOUND ! ;

public

\ Does the caller's own code hold a call instruction that enters the callee?
\ This is the question a two-arm measurement has to answer about each of its
\ arms: an arm whose driver calls the OTHER arm's word is measuring the same
\ code twice and would report a delta of nothing while looking healthy.
: CALLS? ( ptr u8 n ptr u8 n -- bool ) {: ca:ptr cu:n ta:ptr tu:n :}
   false FOUND !
   ta tu WORD-ENTRY WANT !
   ca cu WORD-ENTRY {: s:n :}
   ca cu WORD-BYTES {: len:n :}
   len INSN-BYTES / 0 ?do
      s i INSN-BYTES * +  dup INSN@  CALL-SITE?
   loop
   FOUND @ ;

;package
