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
\ reach into the engine builder's dictionary - and a copy is only worth as much
\ as the gate that holds it to its original.
\
\ HOW THE SUITE HOLDS THE COPY TO THE ENGINE, CLAUSE BY CLAUSE. The gate is one
\ identity, checked per fixture: for a callee S and a caller C compiled after it
\ with a single mention of S,
\
\    ENGINE-COPIES? S   is true exactly when   C CALLS? S   is false.
\
\ The left side is this file's rule. The right side is not a rule at all - it is
\ the engine's decision, already made and already written into C's machine code
\ as a call instruction or as the absence of one. So the two sides can only agree
\ by the copy still saying what the engine says, and either one drifting is a
\ failing case. tools/codegen-workload-test.f carries a fixture per reason a body
\ can be refused - one instruction over the size limit, and one instruction of
\ each refusal class in an otherwise movable body - so that a clause that went
\ missing from either side has a case whose only reason to refuse was that
\ clause. Four of the nine classes have no such fixture and cannot get one; that
\ file records which, and why, rather than pretending otherwise.
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
\ That test, and the walk it guards, now live in
\ src/compiler/native/codewalk.f: the redirection seam that MOVES the call sites
\ this file counts has to visit exactly the same instructions, and one walk is
\ the only way those two answers can be about one thing.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/branch.f
require src/compiler/native/codewalk.f

package CODEGEN-SCAN

\ ---- what this file refuses -------------------------------------------------
public

-7220 constant E-WLSCAN-SUBJECT   \ a named subject word is not in the live dictionary
-7227 constant E-WLSCAN-INSN      \ an instruction index the walk over a record never reached

private

\ ---- reading one instruction out of the code region -------------------------
\ src/compiler/native/codewalk.f is the reader, and the walk below is its walk.
\ It was this file's until the redirection seam needed the same one, and one
\ walk over live code is the whole point of the section further down: two of
\ them written out side by side can drift apart while each stays plausible.
: INSN@ ( n -- n )
   NWALK:INSN@ ;

\ ---- the engine's own constants, spelled as src/habu/habu2.f spells them -----
\ The three sizes and the two frame instructions are public because the
\ acceptance suite states its fixtures' sizes and ends against the engine's own
\ numbers rather than against a second copy of them: a fixture "one instruction
\ over the limit" is a fixture whose record is INL-MAX + FRAME-BYTES +
\ INSN-BYTES long, and that sentence should read the same in the suite as it
\ does here.
public

$28 constant INL-MAX                  \ the most bytes of BODY the engine copies
16 constant FRAME-BYTES               \ prologue plus epilogue of a compiled word
4 constant INSN-BYTES

$D10043FF constant C-CALL-PROLOGUE-INSTR

\ The instruction that undoes the prologue: `add sp, sp, #16`, the last one
\ inside a compiled word's record (src/habu/habu2.f:522 emits it, and the `ret`
\ that follows it lives one instruction PAST the record - which is the fact
\ PLAIN-BODY below leans on). habu2.f has no name for it because it never has to
\ recognise it; the suite does, because a record's last instruction is how a walk
\ that stopped one short is caught.
$910043FF constant C-CALL-FRAME-DOWN-INSTR

private

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

: MASKED? ( n n n -- bool ) {: w:n mask:n op:n :}
   w mask and op = ;

\ ---- the one walk over a record's instructions -------------------------------
\ Every answer in this file that is read out of emitted code is read through
\ SPAN-EACH: the call counter, the per-word call count, the two-arm wiring
\ question, and the two words the acceptance suite reads a record with. There is
\ one loop over a record's code and it is not in this file any more - it is
\ src/compiler/native/codewalk.f, because the redirection seam
\ (src/compiler/native/reach.f) has to walk exactly the same instructions in
\ order to move the call sites this file counts.
\
\ THAT IS THE POINT, not tidiness. A walk that started one instruction late or
\ stopped one instruction early would undercount calls in every record that had
\ one at that end, and a second walk written out beside it could stay correct
\ while this one drifted - which is exactly how a scan comes to report a number
\ nobody can check, and how a seam comes to move some of a word's callers and
\ not the rest. Because the suite reads its fixtures through the same walk, "the
\ walk covers the record end to end" is a case it can state directly, and a
\ dropped end fails it before it can quietly change a count.
\
\ typed-local-lint: allow-bare-local - q receives an instruction's address and
\ the instruction at it, and a local annotation cannot carry a quotation effect.
: SPAN-EACH ( n n [ n n -- ] -- ) {: s:n len:n q :}
   s len q NWALK:SPAN-EACH ;

public

\ The branch form is src/compiler/native/branch.f's, for the reason the walk is
\ codewalk.f's: the seam that MOVES a call site and the scan that counts them
\ have to agree about which instruction is one and where it goes.
: BL? ( n -- bool )
   NBR:BL? ;

: BL-TARGET ( n n -- n )
   NBR:BL-TARGET ;

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

\ ---- reading a record back through the walk ----------------------------------
\ The suite's own view of a record, and it is deliberately the walk's view and
\ not the dictionary's: WORD-BYTES says how long the record is, these say what
\ the walk actually visited in it. The two agreeing is the case that catches a
\ walk which lost an end.
variable INSN-N
variable INSN-WANT
variable INSN-GOT
variable INSN-SEEN

: COUNT-INSN ( n n -- )
   drop drop
   INSN-N @ 1+ INSN-N ! ;

: PICK-INSN ( n n -- )
   nip {: w:n :}
   INSN-N @ INSN-WANT @ = if w INSN-GOT !  true INSN-SEEN ! then
   INSN-N @ 1+ INSN-N ! ;

public

\ How many instructions the walk sees in this word's record.
: WORD-INSNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 INSN-N !
   a u WORD-ENTRY  a u WORD-BYTES  [: COUNT-INSN ;] SPAN-EACH
   INSN-N @ ;

\ The k'th instruction the walk sees, counted from the record's first. An index
\ the walk never reached is refused rather than answered: a caller asking for the
\ last instruction of a record has to be told when the walk stopped short of it,
\ and a stale or zero word would read as an answer.
: WORD-INSN-AT ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   0 INSN-N !  k INSN-WANT !  false INSN-SEEN !
   a u WORD-ENTRY  a u WORD-BYTES  [: PICK-INSN ;] SPAN-EACH
   INSN-SEEN @ 0= if E-WLSCAN-INSN throw then
   INSN-GOT @ ;

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

: SCAN-SPAN ( n n -- )
   [: SITE ;] SPAN-EACH ;

: SCAN-REC ( n -- ) {: k:n :}
   k NWALK:CODED? 0= if exit then
   0 IN-REC !
   k NWALK:REC-START k NWALK:REC-LEN SCAN-SPAN
   IN-REC @ 0 > if CALLERS @ 1+ CALLERS ! then ;

: SWEEP ( n -- ) {: t:n :}
   t TARGET !  0 SITES !  0 CALLERS !
   NWALK:RECS 0 ?do i SCAN-REC loop ;

public

\ How many call instructions in the whole live dictionary enter this word.
: CALL-SITES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u WORD-ENTRY SWEEP
   SITES @ ;

\ How many distinct words hold at least one of them.
: CALLERS-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u WORD-ENTRY SWEEP
   CALLERS @ ;

private

variable BL-N

: COUNT-BL ( n n -- )
   nip BL? if BL-N @ 1+ BL-N ! then ;

public

\ How many call instructions this word's own code contains.
: BLS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 BL-N !
   a u WORD-ENTRY  a u WORD-BYTES  [: COUNT-BL ;] SPAN-EACH
   BL-N @ ;

private

variable STUCK-N

: COUNT-STUCK ( n n -- )
   nip MOVABLE? 0= if STUCK-N @ 1+ STUCK-N ! then ;

public

\ How many instructions of this word's code the rule refuses to move. This is
\ the other half of a refusal, and the acceptance suite needs it stated rather
\ than assumed: a fixture meant to test the SIZE limit is only testing the size
\ limit while this answers zero, and a fixture meant to isolate ONE refusal
\ clause is only isolating it while this answers one. A comment claiming either
\ is a comment; this is a number the suite can fail on.
: UNMOVABLE-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 STUCK-N !
   a u WORD-ENTRY  a u WORD-BYTES  [: COUNT-STUCK ;] SPAN-EACH
   STUCK-N @ ;

private

variable HITS
variable WANT

: CALL-SITE? ( n n -- ) {: pc:n w:n :}
   w BL? 0= if exit then
   pc w BL-TARGET WANT @ <> if exit then
   HITS @ 1+ HITS ! ;

public

\ How many call instructions in the caller's own code enter the callee. A driver
\ that reaches one subject twice and another once is a driver whose work is split
\ two to one between them, and that ratio is read off here rather than assumed:
\ the mixed-coverage workloads state a coverage fraction, and this is the fact
\ that fraction is made of.
: CALLS-IN ( ptr u8 n ptr u8 n -- n ) {: ca:ptr cu:n ta:ptr tu:n :}
   0 HITS !
   ta tu WORD-ENTRY WANT !
   ca cu WORD-ENTRY  ca cu WORD-BYTES  [: CALL-SITE? ;] SPAN-EACH
   HITS @ ;

\ Does the caller's own code hold a call instruction that enters the callee?
\ This is the question a two-arm measurement has to answer about each of its
\ arms: an arm whose driver calls the OTHER arm's word is measuring the same
\ code twice and would report a delta of nothing while looking healthy.
: CALLS? ( ptr u8 n ptr u8 n -- bool )
   CALLS-IN 0 > ;

private

\ ---- and the same question about a subject the engine COPIED ----------------
\ WHY THERE HAS TO BE A SECOND QUESTION. A call instruction names an address, so
\ CALLS? above can say which of two words an arm really entered. A COPIED subject
\ leaves no address anywhere: the caller holds its instructions and nothing that
\ points at it. So an arm over a copied subject cannot be checked by CALLS? at
\ all, and "the arm resolved the bare name to its own column's word" would be an
\ assumption about a search order rather than a fact read off the emitted code -
\ which is exactly what this file exists not to do.
\
\ WHAT IS READ INSTEAD. The subject's copyable span is the bytes the engine would
\ have moved, and that is what a caller that copied it holds, instruction for
\ instruction, somewhere in its own code. So the caller's code is searched for
\ that span. Two subjects computing one answer from different code - which is
\ what the two columns of this harness are - are different bytes, so finding one
\ is finding that column and not the other.
variable CFROM-AT                    \ where in the caller a match is being tried
variable CFROM-OK

: CFROM-MATCH? ( n n n -- bool ) {: at:n lo:n len:n :}
   true CFROM-OK !
   len INSN-BYTES / 0 ?do
      at i INSN-BYTES * + INSN@
      lo i INSN-BYTES * + INSN@ <> if false CFROM-OK ! leave then
   loop
   CFROM-OK @ ;

public

\ Does the caller's own code hold the callee's copyable body, verbatim? False
\ when the engine would not copy that callee at all, because then there is no
\ body to look for and the question is CALLS?'s.
: COPIED-FROM? ( ptr u8 n ptr u8 n -- bool ) {: ca:ptr cu:n ta:ptr tu:n :}
   ta tu ENGINE-COPIES? 0= if false exit then
   ta tu WORD-ENTRY  ta tu WORD-BYTES  BODY-SPAN
   HI @ LO @ - {: len:n :}
   len 0 <= if false exit then
   LO @ {: lo:n :}
   ca cu WORD-ENTRY CFROM-AT !
   ca cu WORD-BYTES len - INSN-BYTES / 1+ {: tries:n :}
   false
   tries 0 ?do
      CFROM-AT @ i INSN-BYTES * +  lo len  CFROM-MATCH? if drop true leave then
   loop ;

;package
