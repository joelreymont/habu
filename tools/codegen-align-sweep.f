\ codegen-align-sweep.f - publish ONE body at a controlled sequence of addresses
\ and time ONE driver against each placement. One concern: separating "where the
\ routine landed" from "what the routine is".
\
\   bin/hb --load tools/codegen-align-sweep.f
\
\ Hand-run, on a quiet machine, the way tools/codegen-compare.f and
\ tools/codegen-workload.f are run. Nothing here is scheduled and nothing here
\ throws on a number: a timing that can fail is a timing that fails for host
\ load. What DOES throw is every fact the timings would be meaningless without -
\ that each arm landed at the address it was steered to, that every arm's subject
\ is byte-identical machine code, that every arm's driver is byte-identical
\ machine code apart from the one branch displacement that differs by
\ construction, that each driver reaches its OWN copy of the subject, and that
\ every folding arm reached the same answer.
\
\ THE QUESTION. tools/codegen-workload.f measured two byte-identical engine
\ publications of one 144-byte body, reached by two byte-identical drivers, 18 to
\ 35 per cent apart on a workload that calls the body once per byte. The delta
\ tracked the CALLEE's publication, reproducibly, across runs. That is larger
\ than anything either code generator does to that body, so until it has a name
\ the workload cannot report a code-generation result on that shape at all.
\
\ WHY A SWEEP AND NOT MORE PAIRS. A pair of publications differs in every address
\ at once: the callee's entry, the call site inside the driver, the page each
\ falls on, the distance between them. Any of those could be the variable, and
\ another pair says nothing about which. A sweep fixes all of them but one and
\ walks that one across its whole period, so the shape of the curve is the
\ answer: an effect keyed to the 128-byte line shows a step at a line boundary,
\ an effect keyed to nothing shows a flat line with noise on it.
\
\ THE FOUR THINGS SWEPT, each its own phase below:
\
\   1  THE CALLEE'S ENTRY, across all thirty-two four-byte positions in one
\      128-byte line, with the call site pinned. TWICE per position: two arms
\      steered to the same residue at different absolute addresses. Those pairs
\      are the FLOOR - the residue is the only thing they share, so whatever they
\      disagree by is what this harness produces when the swept variable did not
\      change. A step in the curve smaller than the floor is not a step.
\   2  THE CALL SITE, across the same thirty-two positions, with the callee's
\      entry residue pinned. Same body, same driver text, same everything - only
\      the address of the `bl` instruction moves.
\   3  THE CHAIN'S OWN PUBLICATION of the same source, swept over the same
\      thirty-two callee positions. The chain emits 72 bytes for the body the
\      engine emits 144 bytes for, so "the chain's leaf is slower" and "the
\      chain's leaf landed badly" are two claims, and only a sweep separates
\      them: at matched entry residue the placement is held still and what is
\      left is the code.
\   4  THE PAGE. Eight arms at one pinned line residue, placed at chosen offsets
\      inside a 16 KiB page - including one whose 144-byte body straddles the
\      page boundary - because a line effect and a page effect look the same in a
\      pair of samples and different in a sweep.
\   5  THE SAME SWEEP OVER A BRANCHLESS CALLEE of exactly the same record length,
\      published under exactly the same name and reached by exactly the same
\      driver text. This is the phase that turns a curve into a mechanism. A
\      front-end effect - fetch blocks, decode windows, page walks - depends on
\      where the bytes are and not on what they do, so it must survive taking the
\      branches out. Anything keyed to the callee's BRANCHES must not. The two
\      bodies are held to that difference by the emitted code: same 144 bytes,
\      and the copy rule's own count of unmovable instructions is zero for one
\      and four for the other.
\
\ AND THE BASELINE, which is what makes a per-call number honest. The scan driver
\ costs a loop, a byte load, an add AND a call per byte; dividing its whole time
\ by the byte count reports all four as the call. Two baseline arms run the same
\ loop with the call removed, so the call's own cost is a DIFFERENCE of two
\ measured numbers rather than a whole number attributed to one of its parts.
\ That difference is what makes this file's per-call figures comparable with
\ tools/codegen-compare.f's, which times the routine and not a loop around it.
\
\ HOW A PLACEMENT IS CHOSEN, AND WHY IT IS HONEST. The engine compiles every
\ definition into one bump pointer, and src/compiler/native/publish.f NEXT-SLOT
\ is that pointer: a word published now starts exactly there. So a placement is
\ chosen by moving the pointer the only way a program is allowed to move it -
\ by publishing REAL definitions in front of the subject. STEER-TO below emits
\ empty and single-`drop` definitions, whose records are 20 and 24 bytes, in the
\ combination that advances the pointer by exactly the wanted number of bytes,
\ and then REFUSES if the pointer did not land where the arithmetic said. No cell
\ of engine state is poked: a poked slot pointer would leave the arena's own
\ bookkeeping disagreeing with the code in it, and every address in the table
\ would then be a number this file wrote rather than a number the engine chose.
\
\ WHAT AN ARM IS. Its own package, holding its own engine-or-chain publication of
\ the four subject strings of tools/codegen-workload-hot.f, and its own driver
\ compiled from that file's own SCAN-BODY$ text. The driver names the subject
\ BARE, and a bare tail resolves in the open package first, so every arm's driver
\ text is the same characters and every arm's driver reaches its own copy. The
\ subject text and the driver text are never written down here; they are the
\ workload's, so a body that changes there changes here.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-workload-scan.f
require tools/codegen-workload-hot.f
require src/compiler/native/publish.f

package CALIGN

public

-7228 constant E-CALIGN-CAP     \ the arm table is full, or a rendered source is longer than its buffer
-7229 constant E-CALIGN-STEER   \ the padding did not land the code pointer where the arithmetic said it would
-7230 constant E-CALIGN-WIRE    \ an arm's driver does not hold the calls its kind requires, or reached another answer
-7231 constant E-CALIGN-CODE    \ two arms that must hold identical machine code do not
-7232 constant E-CALIGN-CLOCK   \ the monotonic clock reported no elapsed time across a whole run

private

\ ---- the machine, and the periods a placement is swept over ------------------
\ The line is the host's: `sysctl hw.cachelinesize` answers 128 on the Apple M2
\ this was measured on, which is already the first correction to the question -
\ the dot asks about 64-byte line straddling, and on this machine a 64-byte step
\ is half a line. Sweeping the whole 128 covers both readings at once: a residue
\ taken modulo 64 is the low half of a residue taken modulo 128, so one curve
\ answers both.
$80 constant LINE
$40 constant HALF-LINE
$4000 constant PAGE                 \ `sysctl hw.pagesize`
4 constant INSN-BYTES
$20 constant RESIDUES               \ LINE / INSN-BYTES: every position a routine can start at

\ ---- what one padding definition costs --------------------------------------
\ Measured, not assumed - and the measurement is checked on every use, because
\ STEER-TO refuses when the pointer does not land where these two numbers said.
\ Two sizes are needed rather than one: with 20 alone the reachable advances are
\ the multiples of 20, and a placement four bytes further on would be
\ unreachable. With 20 and 24 every multiple of four from 80 upwards is a whole
\ number of each, which is every advance this file asks for.
5 constant PAD-EMPTY-Q              \ `: NAME ( -- ) ;` is 20 bytes: five instructions
6 constant PAD-DROP-Q               \ `: NAME ( n -- ) drop ;` is 24 bytes: six
80 constant PAD-FLOOR               \ the smallest advance both sizes can compose
24 constant PAD-CHUNK               \ definitions per rendered source

\ ---- how much work one timed run does ---------------------------------------
\ One run drives the whole 4 KiB buffer REPS times, which is a millisecond or so:
\ long enough that the clock's resolution is nothing beside it, short enough that
\ the fastest-run rule has a real chance of finding a window with nothing else in
\ it. That is tools/codegen-workload-time.f's reasoning and its numbers.
50 constant REPS
31 constant ROUNDS

$7FFFFFFFFFFFFFFF constant NS-MAX
1000 constant PERMILLE
1000 constant PS-PER-NS

\ ---- the arms ---------------------------------------------------------------
\ A kind says what the arm is FOR, and every arm of one kind is the same
\ experiment at a different address.
0 constant K-CALLEE                 \ phase 1: engine callee entry swept, call site pinned
1 constant K-SITE                   \ phase 2: call site swept, engine callee entry pinned
2 constant K-CHAIN                  \ phase 3: chain callee entry swept, call site pinned
3 constant K-PAGE                   \ phase 4: page offset swept, line residue pinned
4 constant K-BASE                   \ the loop with the call taken out
5 constant K-FLAT                   \ phase 5: the same sweep over a BRANCHLESS callee of the same size

$C0 constant ARM-MAX

create A-KIND ARM-MAX cells allot
create A-SUBJ ARM-MAX cells allot   \ the subject's first instruction
create A-DRV ARM-MAX cells allot    \ the driver's first instruction
create A-SITE ARM-MAX cells allot   \ the `bl` that enters the subject
create A-FAST ARM-MAX cells allot
create A-SLOW ARM-MAX cells allot
create A-SUM ARM-MAX cells allot    \ what the arm's driver computed
variable ARM-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: ARM-OK ( n -- n )
   dup 0 < over ARM-N @ >= or if E-CALIGN-CAP throw then ;

: KIND ( n -- n ) {: k:n :}
   A-KIND k ARM-OK SLOT @ ;

: SUBJ ( n -- n ) {: k:n :}
   A-SUBJ k ARM-OK SLOT @ ;

: DRV ( n -- n ) {: k:n :}
   A-DRV k ARM-OK SLOT @ ;

: SITE ( n -- n ) {: k:n :}
   A-SITE k ARM-OK SLOT @ ;

: FAST ( n -- n ) {: k:n :}
   A-FAST k ARM-OK SLOT @ ;

: SLOW ( n -- n ) {: k:n :}
   A-SLOW k ARM-OK SLOT @ ;

\ ---- rendering the source an arm is published from --------------------------
\ Everything this file puts into the dictionary goes through one buffer and one
\ call into the workload's own `evaluate` boundary, so there is one place where
\ generated text becomes code.
$2000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U

$30 constant DIGIT-ZERO

: CP$ ( ptr u8 n ptr u8 -- )
   swap STR-LEN BYTE-COPY-LEN ;

: DIG3! ( ptr u8 n -- ) {: b:ptr v:n :}
   v 100 / DIGIT-ZERO +  b c!
   v 10 / 10 mod DIGIT-ZERO +  b 1 + c!
   v 10 mod DIGIT-ZERO +  b 2 + c! ;

: DIG4! ( ptr u8 n -- ) {: b:ptr v:n :}
   v 1000 / DIGIT-ZERO +  b c!
   b 1 +  v 1000 mod  DIG3! ;

: S-RESET ( -- )
   0 SRC-U ! ;

: S+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC-U @ u + SRC-CAP > if E-CALIGN-CAP throw then
   a  SRC-BUF SRC-U @ +  u STR-LEN BYTE-COPY-LEN
   SRC-U @ u + SRC-U ! ;

: S-DIGITS ( n n -- ) {: v:n w:n :}
   SRC-U @ w + SRC-CAP > if E-CALIGN-CAP throw then
   SRC-BUF SRC-U @ +  v  w 3 = if DIG3! else DIG4! then
   SRC-U @ w + SRC-U ! ;

3 constant PKG-DIGITS
4 constant PAD-DIGITS

: S-3D ( n -- )
   PKG-DIGITS S-DIGITS ;

: S-4D ( n -- )
   PAD-DIGITS S-DIGITS ;

: S-EV ( -- )
   SRC-BUF SRC-U @ CODEGEN-HOT:EV ;

\ ---- moving the code pointer by publishing ----------------------------------
\ The padding definitions live in one package of their own and are numbered
\ across the whole run, because a package wordlist rejects a repeated tail and a
\ repeat here would be a refusal in the middle of a sweep rather than a name
\ collision anybody could see.
variable PAD-SEQ

: PAD-NAME ( -- )
   s" : Q" S+
   PAD-SEQ @ S-4D
   PAD-SEQ @ 1+ PAD-SEQ ! ;

: PAD-EMPTY+ ( -- )
   PAD-NAME s"  ( -- ) ; " S+ ;

: PAD-DROP+ ( -- )
   PAD-NAME s"  ( n -- ) drop ; " S+ ;

\ One rendered source holding up to PAD-CHUNK definitions. The counts arrive
\ already decided; this only writes them out.
: PAD-BATCH ( n n -- ) {: e:n d:n :}
   e d + 0= if exit then
   S-RESET
   s" package CALIGNPAD " S+
   e 0 ?do PAD-EMPTY+ loop
   d 0 ?do PAD-DROP+ loop
   s" ;package " S+
   S-EV ;

variable PAD-LEFT

\ Publish n definitions of one size, in batches small enough that every rendered
\ source fits the buffer. The size is chosen by the caller and passed straight
\ through, so this word decides nothing but how the run is cut up.
: PAD-EMPTIES ( n -- )
   PAD-LEFT !
   begin PAD-LEFT @ 0 > while
      PAD-LEFT @ PAD-CHUNK min {: n:n :}
      n 0 PAD-BATCH
      PAD-LEFT @ n - PAD-LEFT !
   repeat ;

: PAD-DROPS ( n -- )
   PAD-LEFT !
   begin PAD-LEFT @ 0 > while
      PAD-LEFT @ PAD-CHUNK min {: n:n :}
      0 n PAD-BATCH
      PAD-LEFT @ n - PAD-LEFT !
   repeat ;

\ How many of each size compose an advance of exactly D bytes. Every advance is a
\ multiple of four, so it is a whole number of instructions m = D/4, and the two
\ sizes are 5 and 6 instructions: 5a + 6b = m. Taking b as m modulo 5 makes 6b
\ agree with m modulo 5, so the remainder divides by 5 exactly, and it is
\ non-negative for every m at or above PAD-FLOOR's twenty.
: PAD-EMPTY-N ( n -- n ) {: m:n :}
   m  PAD-DROP-Q  m PAD-EMPTY-Q mod  *  -  PAD-EMPTY-Q / ;

: FREE-SLOT ( -- n )
   NPUB:NEXT-SLOT ;

\ Advance the code pointer by exactly D bytes, and refuse if it did not go
\ exactly that far. This is where the two record sizes are held to their
\ measurement: a definition that stopped costing 20 bytes fails here rather than
\ silently placing every arm somewhere else.
: ADVANCE ( n -- ) {: d:n :}
   d 0= if exit then
   d PAD-FLOOR < if E-CALIGN-STEER throw then
   d INSN-BYTES mod 0<> if E-CALIGN-STEER throw then
   FREE-SLOT {: before:n :}
   d INSN-BYTES / {: m:n :}
   m PAD-EMPTY-N PAD-EMPTIES
   m PAD-EMPTY-Q mod PAD-DROPS
   FREE-SLOT before - d <> if E-CALIGN-STEER throw then ;

\ Put the code pointer at WANT modulo M, by advancing to the next such address
\ far enough away for the padding to compose. One correction is always enough
\ because every modulus this file uses is larger than PAD-FLOOR.
: STEER-TO ( n n -- ) {: want:n m:n :}
   m PAD-FLOOR <= if E-CALIGN-STEER throw then
   want  FREE-SLOT -  m 1- and {: g:n :}
   g PAD-FLOOR < if g m + else g then ADVANCE
   FREE-SLOT m 1- and  want m 1- and  <> if E-CALIGN-STEER throw then ;

\ ---- an arm's names ---------------------------------------------------------
\ Each arm is one package, and the two words this file asks about are that
\ package's copy of the subject and that package's driver. The two names live in
\ two buffers because a caller comparing one arm's record with another's holds
\ both at once.
$20 constant NAME-CAP
6 constant PKG-LEN                  \ "HSW" and three digits
7 constant SUBJ-TAIL                \ ":FOLD-C"
5 constant DRV-TAIL                 \ ":SCAN"

create SUBJ-NAME NAME-CAP allot
create DRV-NAME NAME-CAP allot

: ARM-SUBJ$ ( n -- ptr u8 n ) {: k:n :}
   s" HSW" SUBJ-NAME CP$
   SUBJ-NAME PKG-DIGITS + k DIG3!
   s" :FOLD-C" SUBJ-NAME PKG-LEN + CP$
   SUBJ-NAME  PKG-LEN SUBJ-TAIL + ;

: ARM-DRV$ ( n -- ptr u8 n ) {: k:n :}
   s" HSW" DRV-NAME CP$
   DRV-NAME PKG-DIGITS + k DIG3!
   s" :SCAN" DRV-NAME PKG-LEN + CP$
   DRV-NAME  PKG-LEN DRV-TAIL + ;

\ ---- the driver with the call taken out -------------------------------------
\ The one body written down in this file, and it is written down because it is
\ this file's control and exists nowhere else: SCAN-BODY$ with the call removed
\ and nothing else changed. Its answer differs from every other arm's, which is
\ the point - it is not folding anything - so the answer check below excludes it
\ by kind rather than by name.
: BASE-BODY$ ( -- ptr u8 n )
   s"  ( ptr u8 n -- n ) {: a:ptr u:n :} 0 u 0 ?do a i + c@ + loop ;" ;

\ ---- the callee with the branches taken out ---------------------------------
\ The second body written down here, and the one that turns a curve into a
\ mechanism. It is published under the SAME name the fold is published under, so
\ the driver is the workload's own text either way, and the engine compiles it to
\ a record of exactly the fold's 144 bytes - so it occupies the same number of
\ instruction-fetch blocks at every entry offset the sweep visits. The one thing
\ it does not have is a branch: the fold's two data-dependent `cbz`s are gone and
\ a chain of mask-and-or steps of the same length stands in their place. If the
\ band in phase 1 is a front-end fetch effect it must appear here too; if it
\ needs the branches it must not. CODE-CK holds the two halves of that sentence
\ to the emitted code - 144 bytes both, no unmovable instruction here and some
\ there - so the comparison cannot quietly stop being the comparison.
: FLAT-BODY$ ( -- ptr u8 n )
   s" : FOLD-C ( n -- n ) {: c:n :} c 32 or 90 and 7 or 3 or 5 or 9 or 17 or 33 or 65 or 129 or 257 or 513 or ;" ;

\ ---- publishing one arm -----------------------------------------------------
: PKG-OPEN ( n -- ) {: k:n :}
   S-RESET
   s" package HSW" S+
   k S-3D
   s"  public " S+ ;

: PKG-CLOSE ( -- )
   s"  ;package " S+
   S-EV ;

\ The subject, published the way the arm's kind says. Both publishers are the
\ workload's own, over the workload's own four strings, so the code an arm holds
\ is the code that file's rows are measured against.
: PUB-WORD ( n -- ) {: k:n :}
   k KIND K-FLAT = if FLAT-BODY$ S+ exit then
   k KIND K-CHAIN = if s" CODEGEN-HOT:PUBLISH-CHAIN" S+ exit then
   s" CODEGEN-HOT:PUBLISH-ENGINE" S+ ;

: PUB-SUBJECT ( n -- ) {: k:n :}
   k PKG-OPEN
   k PUB-WORD
   PKG-CLOSE ;

\ The driver, reopening the arm's package so the bare subject name in
\ SCAN-BODY$ resolves to the copy this arm just published.
: PUB-DRIVER ( n -- ) {: k:n :}
   k PKG-OPEN
   s" : SCAN" S+
   k KIND K-BASE = if BASE-BODY$ else CODEGEN-HOT:SCAN-BODY$ then S+
   PKG-CLOSE ;

\ ---- reading the call site out of the driver's code -------------------------
\ Through tools/codegen-workload-scan.f's own walk over a record and its own
\ branch decoder: this file asks which instruction the walk saw, and that file
\ answers whether it is a call and where it goes. There is no second decoder
\ here, because a second decoder is how two tools come to disagree about one
\ instruction stream.
: SITE-AT ( n n -- n ) {: k:n i:n :}
   k ARM-DRV$ i CODEGEN-SCAN:WORD-INSN-AT {: w:n :}
   w CODEGEN-SCAN:BL? 0= if -1 exit then
   k DRV  i INSN-BYTES *  + {: pc:n :}
   pc w CODEGEN-SCAN:BL-TARGET  k SUBJ <> if -1 exit then
   pc ;

: FIND-SITE ( n -- n ) {: k:n :}
   -1
   k ARM-DRV$ CODEGEN-SCAN:WORD-INSNS 0 ?do
      k i SITE-AT dup 0 < if drop else nip then
   loop ;

\ What every arm of a call-carrying kind must be true of: one call instruction in
\ the whole driver, and it enters this arm's own subject. An arm whose driver
\ reached another arm's copy would be timing that copy's placement under this
\ one's row.
: WIRE-CK ( n -- ) {: k:n :}
   k ARM-DRV$ CODEGEN-SCAN:BLS-IN {: bls:n :}
   k KIND K-BASE = if
      bls 0<> if E-CALIGN-WIRE throw then
      exit
   then
   bls 1 <> if E-CALIGN-WIRE throw then
   k ARM-DRV$ k ARM-SUBJ$ CODEGEN-SCAN:CALLS-IN 1 <> if E-CALIGN-WIRE throw then ;

\ ---- one arm, published at a chosen pair of addresses ------------------------
\ The two steers are the whole design. The subject is steered first, then the
\ four subject strings are published, then the pointer is steered AGAIN before
\ the driver is compiled. Without the second steer the driver would land a fixed
\ distance behind the subject and its call site would carry the subject's residue
\ with it - the two variables would move together and no curve could tell them
\ apart.
: ARM-ENTRIES ( n -- ) {: k:n :}
   k KIND K-BASE = if 0 else k ARM-SUBJ$ CODEGEN-SCAN:WORD-ENTRY then
   A-SUBJ k SLOT !
   k ARM-DRV$ CODEGEN-SCAN:WORD-ENTRY  A-DRV k SLOT !
   k WIRE-CK
   k KIND K-BASE = if -1 else k FIND-SITE then  A-SITE k SLOT ! ;

: ADD-ARM ( n n n -- ) {: kd:n swant:n dwant:n :}
   ARM-N @ ARM-MAX >= if E-CALIGN-CAP throw then
   ARM-N @ {: k:n :}
   k 1+ ARM-N !
   kd A-KIND k SLOT !
   NS-MAX A-FAST k SLOT !  0 A-SLOW k SLOT !
   kd K-PAGE = if swant PAGE STEER-TO else swant LINE STEER-TO then
   k PUB-SUBJECT
   dwant LINE STEER-TO
   k PUB-DRIVER
   k ARM-ENTRIES ;

\ ---- the machine code every arm must share ----------------------------------
\ Two arms holding the same body compiled from the same string by the same
\ publisher must have the same record length and the same instructions. Their
\ DRIVERS must match too, everywhere except the one instruction that is the call
\ - a branch displacement is measured from the site to the target and those
\ differ by construction, which is exactly the thing being swept.
: LEN-CK ( n n -- ) {: a:n b:n :}
   a b <> if E-CALIGN-CODE throw then ;

: SAME-SUBJECT ( n n -- ) {: a:n b:n :}
   a ARM-SUBJ$ CODEGEN-SCAN:WORD-INSNS {: n:n :}
   b ARM-SUBJ$ CODEGEN-SCAN:WORD-INSNS n LEN-CK
   n 0 ?do
      a ARM-SUBJ$ i CODEGEN-SCAN:WORD-INSN-AT
      b ARM-SUBJ$ i CODEGEN-SCAN:WORD-INSN-AT <> if E-CALIGN-CODE throw then
   loop ;

\ The same over two drivers, with a call instruction in both records at one index
\ counted as agreement: what differs between two `bl`s there is only where each
\ one points, which is the arm's placement and not its code.
: INSN-PAIR-CK ( n n -- ) {: x:n y:n :}
   x y = if exit then
   x CODEGEN-SCAN:BL? y CODEGEN-SCAN:BL? and 0= if E-CALIGN-CODE throw then ;

: SAME-DRIVER ( n n -- ) {: a:n b:n :}
   a ARM-DRV$ CODEGEN-SCAN:WORD-INSNS {: n:n :}
   b ARM-DRV$ CODEGEN-SCAN:WORD-INSNS n LEN-CK
   n 0 ?do
      a ARM-DRV$ i CODEGEN-SCAN:WORD-INSN-AT
      b ARM-DRV$ i CODEGEN-SCAN:WORD-INSN-AT INSN-PAIR-CK
   loop ;

\ The first arm of a kind is that kind's reference.
: REF-OF ( n -- n ) {: kd:n :}
   -1
   ARM-N @ 0 ?do
      dup 0 <  i KIND kd =  and if drop i then
   loop ;

\ Which arm this one's SUBJECT must match: its own publisher's reference. Every
\ engine-published subject in the run is the same four strings through the same
\ path, so they are all held against phase 1's first arm and not merely against
\ their own phase's.
: SUBJ-REF ( n -- n ) {: k:n :}
   k KIND K-CHAIN = if K-CHAIN REF-OF exit then
   k KIND K-FLAT = if K-FLAT REF-OF exit then
   K-CALLEE REF-OF ;

: DRV-REF ( n -- n ) {: k:n :}
   k KIND K-BASE = if K-BASE REF-OF exit then
   K-CALLEE REF-OF ;

: SUBJ-CK ( n -- ) {: k:n :}
   k KIND K-BASE = if exit then
   k SUBJ-REF {: r:n :}
   r k = if exit then
   r k SAME-SUBJECT ;

: DRV-CK ( n -- ) {: k:n :}
   k DRV-REF {: r:n :}
   r k = if exit then
   r k SAME-DRIVER ;

\ The one fact the branchless control rests on, read off the emitted code rather
\ than off the source it was written from: the flat subject holds no instruction
\ the engine's own copy rule refuses to move - which is its list of branches,
\ register branches and returns - and the ENGINE's fold holds some.
\
\ AND THE CHAIN'S FOLD NOW HOLDS NONE EITHER, which is what phase 3 came here to
\ measure. When this file was written both folds branched and the chain's
\ branched harder - six branch instructions in 76 bytes against the engine's
\ four in 148 - and that density is what docs/codegen-placement.md named as the
\ mechanism behind the chain leaf's twenty-eight per cent and its wider
\ placement band. src/compiler/native/select.f now turns a selection whose arms
\ are single values into a machine select, so the chain's copy of this body has
\ no branch at all. The check is written as that statement rather than relaxed
\ into silence: a chain that started branching here again would be the
\ regression this whole measurement exists to catch, and the sweep refuses
\ rather than quietly reporting a different body's numbers under phase 3's name.
: FLAT-BRANCH-CK ( n -- ) {: b:n :}
   b 0<> if E-CALIGN-CODE throw then ;

: CHAIN-BRANCH-CK ( n -- ) {: b:n :}
   b 0<> if E-CALIGN-CODE throw then ;

: FOLD-BRANCH-CK ( n -- ) {: b:n :}
   b 0= if E-CALIGN-CODE throw then ;

: BRANCH-CK ( n -- ) {: k:n :}
   k KIND K-BASE = if exit then
   k ARM-SUBJ$ CODEGEN-SCAN:UNMOVABLE-IN {: b:n :}
   k KIND K-FLAT = if b FLAT-BRANCH-CK exit then
   k KIND K-CHAIN = if b CHAIN-BRANCH-CK exit then
   b FOLD-BRANCH-CK ;

: CODE-CK ( -- )
   ARM-N @ 0 ?do
      i SUBJ-CK
      i DRV-CK
      i BRANCH-CK
   loop ;

\ ---- the arms, laid out ------------------------------------------------------
\ Phase 1 walks the callee across the line twice, and the two passes end up at
\ opposite ends of the selector ladder the timing loop reaches arms through: if a
\ pair at one residue agrees while sitting thirty-two rungs apart, the rung is
\ not what the pair measured.
0 constant PIN                      \ the residue a pinned variable is held at

: PHASE-CALLEE ( -- )
   RESIDUES 0 ?do K-CALLEE  i INSN-BYTES *  PIN ADD-ARM loop
   RESIDUES 0 ?do K-CALLEE  i INSN-BYTES *  PIN ADD-ARM loop ;

: PHASE-SITE ( -- )
   RESIDUES 0 ?do K-SITE PIN  i INSN-BYTES *  ADD-ARM loop ;

: PHASE-CHAIN ( -- )
   RESIDUES 0 ?do K-CHAIN  i INSN-BYTES *  PIN ADD-ARM loop ;

\ Eight offsets inside one 16 KiB page, all at the same line residue so the line
\ is held still while the page moves. The subject's engine record is 144 bytes,
\ so the last of them straddles the page boundary and the one before it ends two
\ lines short of it.
8 constant PAGE-ARMS

create PAGE-OFFS PAGE-ARMS cells allot

: PAGE-PLAN ( -- )
   0                PAGE-OFFS 0 SLOT !
   LINE             PAGE-OFFS 1 SLOT !
   LINE 2 *         PAGE-OFFS 2 SLOT !
   PAGE 4 /         PAGE-OFFS 3 SLOT !
   PAGE 2 /         PAGE-OFFS 4 SLOT !
   PAGE 4 / 3 *     PAGE-OFFS 5 SLOT !
   PAGE LINE 2 * -  PAGE-OFFS 6 SLOT !
   PAGE LINE -      PAGE-OFFS 7 SLOT ! ;

: PHASE-PAGE ( -- )
   PAGE-PLAN
   PAGE-ARMS 0 ?do K-PAGE  PAGE-OFFS i SLOT @  PIN ADD-ARM loop ;

: PHASE-FLAT ( -- )
   RESIDUES 0 ?do K-FLAT  i INSN-BYTES *  PIN ADD-ARM loop ;

: PHASE-BASE ( -- )
   K-BASE PIN PIN ADD-ARM
   K-BASE PIN PIN ADD-ARM ;

\ ---- the selector the timing loop reaches every arm through ------------------
\ A generated ladder, because the arms do not exist until they have been
\ published and a quotation cannot be handed an index. It costs a handful of
\ integer comparisons once per RUN, against the two hundred thousand calls a run
\ makes - and phase 1's two passes sit a whole group apart in it, so a pair of
\ replicates that agrees has agreed across different rungs and the rung is not
\ what the pair measured.
defer ARM-RUN ( n ptr u8 n -- n )

variable ARM-K

: STEP ( -- )
   ARM-K @ CODEGEN-HOT:BYTES$ ARM-RUN drop ;

\ THE LADDER IS TWO LEVELS BECAUSE ONE IS TOO DEEP. Each `of` opens a control
\ frame, and the engine's nesting limit is reached somewhere between one and two
\ hundred of them - a hundred and seventy arms refuses with "control-flow nesting
\ too deep". So the arms are cut into groups of GROUP-SIZE, one selector each,
\ and one selector over the groups. That costs a second call and a division per
\ RUN rather than per call, and it takes the worst-case comparison count from the
\ arm count down to the group size plus the group count.
$20 constant GROUP-SIZE
80 constant LADDER-SLACK

: LADDER-ARM ( n -- ) {: k:n :}
   s"       " S+ k S-3D
   s"  of a u HSW" S+ k S-3D
   s" :SCAN endof " S+ ;

: LADDER-SIG ( -- )
   s"  ( n ptr u8 n -- n ) {: k:n a:ptr u:n :} k " S+ ;

: LADDER-CLOSE ( -- )
   s" 0 swap endcase ; ;package " S+
   S-EV ;

: GROUPS ( -- n )
   ARM-N @ GROUP-SIZE + 1- GROUP-SIZE / ;

\ One group's selector: the arms of that group, named by their run-wide index so
\ a row of the report and a rung of the ladder are the same number.
: GROUP-LADDER ( n -- ) {: g:n :}
   S-RESET
   s" package CALIGN private : RUN-G" S+ g S-3D
   LADDER-SIG
   s" case " S+
   GROUP-SIZE 0 ?do
      SRC-U @ SRC-CAP LADDER-SLACK - > if E-CALIGN-CAP throw then
      g GROUP-SIZE * i + {: k:n :}
      k ARM-N @ < if k LADDER-ARM then
   loop
   LADDER-CLOSE ;

: TOP-ARM ( n -- ) {: g:n :}
   s"       " S+ g S-3D
   s"  of k a u RUN-G" S+ g S-3D
   s"  endof " S+ ;

: TOP-LADDER ( -- )
   S-RESET
   s" package CALIGN private : RUN-TOP" S+
   LADDER-SIG
   GROUP-SIZE S-3D
   s"  / case " S+
   GROUPS 0 ?do i TOP-ARM loop
   LADDER-CLOSE ;

\ The deferred word the timing loop calls is installed from generated source
\ because the selector it reaches did not exist when this file was compiled.
: INSTALL-LADDER ( -- )
   S-RESET
   s" package CALIGN private : ARM-INSTALL ( -- ) [: RUN-TOP ;] is ARM-RUN ; ARM-INSTALL ;package " S+
   S-EV ;

: BUILD-LADDER ( -- )
   GROUPS 0 ?do i GROUP-LADDER loop
   TOP-LADDER
   INSTALL-LADDER ;

\ ---- the clock ---------------------------------------------------------------
\ tools/codegen-workload-time.f's discipline, over N arms instead of two: one
\ timed run executes the body a fixed number of times and the elapsed monotonic
\ nanoseconds are the measurement; a run can only be made slower by the rest of
\ the machine, never faster, so the FASTEST of several runs is the closest
\ estimate available and the spread between fastest and slowest says how noisy
\ the host was. A ROUND runs every arm once, so whatever the host does during the
\ measurement is spread across all of them and each arm's fastest run is drawn
\ from the same sequence of windows as every other arm's. That file's SWEEP does
\ exactly this for five arms and keeps only the two extremes; a sweep needs the
\ whole curve, so the arms are kept one by one here.
: RUN-ONCE ( -- n )
   mono-ns {: t0:n :}
   REPS 0 ?do STEP loop
   mono-ns t0 - ;

: SAMPLE ( n n -- ) {: k:n ns:n :}
   ns 0= if E-CALIGN-CLOCK throw then
   ns A-FAST k SLOT @ < if ns A-FAST k SLOT ! then
   ns A-SLOW k SLOT @ > if ns A-SLOW k SLOT ! then ;

: ANSWERS ( -- )
   ARM-N @ 0 ?do
      i ARM-K !
      i CODEGEN-HOT:BYTES$ ARM-RUN  A-SUM i SLOT !
   loop ;

: MEASURE ( -- )
   ANSWERS
   ROUNDS 0 ?do
      ARM-N @ 0 ?do
         i ARM-K !
         i RUN-ONCE SAMPLE
      loop
   loop ;

\ Every arm running one program over the same bytes must reach the same answer,
\ and an arm that did not is an arm whose driver reached something else - its
\ time is a time for another program. There are three programs in the run and so
\ three answers: the fold, the loop with the call removed, and the branchless
\ stand-in. Each arm is held against the first arm that runs the same one.
: ANS-REF ( n -- n ) {: k:n :}
   k KIND K-BASE = if K-BASE REF-OF exit then
   k KIND K-FLAT = if K-FLAT REF-OF exit then
   K-CALLEE REF-OF ;

: ANSWER-CK ( -- )
   ARM-N @ 0 ?do
      A-SUM i ANS-REF SLOT @
      A-SUM i SLOT @ <> if E-CALIGN-WIRE throw then
   loop ;

\ ---- what a row says ---------------------------------------------------------
CODEGEN-HOT:BYTE-N REPS * constant CALLS-PER-RUN

: BASE-FAST ( -- n )
   NS-MAX
   ARM-N @ 0 ?do
      i KIND K-BASE = if i FAST min then
   loop ;

\ Picoseconds of driver time per byte of buffer, and picoseconds of CALL time per
\ byte: the second is the first with the baseline's loop taken off it, which is
\ the only figure in this file comparable with a measurement of the routine
\ alone.
: PS-BYTE-OF ( n -- n ) {: ns:n :}
   ns PS-PER-NS * CALLS-PER-RUN / ;

: PS-CALL-OF ( n -- n ) {: ns:n :}
   ns BASE-FAST -  PS-PER-NS *  CALLS-PER-RUN / ;

: SPREAD ( n -- n ) {: k:n :}
   k SLOW k FAST - PERMILLE * k FAST / ;

\ ---- printing ---------------------------------------------------------------
$20 constant SPACE-BYTE

: SP ( n -- )
   dup 0 <= if drop exit then
   0 ?do SPACE-BYTE emit loop ;

: WIDTH ( n -- n ) {: v:n :}
   SB-RESET v FMT:SB-INT SB$ nip ;

: N. ( n n -- ) {: v:n w:n :}
   w v WIDTH - SP
   v FMT:.INT ;

: HEAD ( -- )
   ." arm  subj-entry   e%128  e%64  page-off  site%128   fastest-ns  spr/1000  ps/byte  ps/call" cr ;

: ROW. ( n -- ) {: k:n :}
   k 3 N.
   k SUBJ 12 N.
   k SUBJ LINE 1- and 7 N.
   k SUBJ HALF-LINE 1- and 6 N.
   k SUBJ PAGE 1- and 10 N.
   k SITE LINE 1- and 10 N.
   k FAST 13 N.
   k SPREAD 10 N.
   k FAST PS-BYTE-OF 9 N.
   k FAST PS-CALL-OF 9 N.
   cr ;

: KIND. ( n -- ) {: kd:n :}
   ARM-N @ 0 ?do
      i KIND kd = if i ROW. then
   loop ;

\ ---- the summary the verdict is read off ------------------------------------
: MIN-OF ( n -- n ) {: kd:n :}
   NS-MAX
   ARM-N @ 0 ?do
      i KIND kd = if i FAST min then
   loop ;

: MAX-OF ( n -- n ) {: kd:n :}
   0
   ARM-N @ 0 ?do
      i KIND kd = if i FAST max then
   loop ;

: GAP-PERMILLE ( n n -- n ) {: lo:n hi:n :}
   lo 0= if E-CALIGN-CLOCK throw then
   hi lo - PERMILLE * lo / ;

: SPAN-PERMILLE ( n -- n ) {: kd:n :}
   kd MIN-OF  kd MAX-OF  GAP-PERMILLE ;

\ The fastest and slowest arm of a kind that landed at one residue. When a kind
\ has two arms per residue these two differ only in absolute address, so the gap
\ between them is the floor: whatever this harness produces when the variable
\ being swept did not change.
: BEST-AT ( n n -- n ) {: kd:n r:n :}
   NS-MAX
   ARM-N @ 0 ?do
      i KIND kd =  i SUBJ LINE 1- and r =  and if i FAST min then
   loop ;

: WORST-AT ( n n -- n ) {: kd:n r:n :}
   0
   ARM-N @ 0 ?do
      i KIND kd =  i SUBJ LINE 1- and r =  and if i FAST max then
   loop ;

: FLOOR-PERMILLE ( -- n )
   0
   RESIDUES 0 ?do
      K-CALLEE  i INSN-BYTES *  BEST-AT {: lo:n :}
      K-CALLEE  i INSN-BYTES *  WORST-AT {: hi:n :}
      lo hi GAP-PERMILLE max
   loop ;

\ The two curves held against each other at MATCHED placement: for each residue,
\ how much slower the chain arm is than the engine arm that landed there. The
\ whole set is reported rather than a mean over placements, because a mean over
\ placements is exactly the number that could not answer the question before.
: MATCH-ROW ( n -- ) {: r:n :}
   K-CALLEE r BEST-AT {: e:n :}
   K-CHAIN r BEST-AT {: c:n :}
   e NS-MAX = c NS-MAX = or if exit then
   e PS-CALL-OF {: ep:n :}
   c PS-CALL-OF {: cp:n :}
   r 5 N.
   ep 15 N.
   cp 15 N.
   ep cp GAP-PERMILLE 17 N.
   cr ;

public

\ ---- the run -----------------------------------------------------------------
: PUBLISH ( -- )
   CODEGEN-HOT:FILL-DATA
   PHASE-BASE
   PHASE-CALLEE
   PHASE-SITE
   PHASE-CHAIN
   PHASE-PAGE
   PHASE-FLAT
   CODE-CK
   BUILD-LADDER ;

: RUN ( -- )
   MEASURE
   ANSWER-CK ;

: REPORT ( -- )
   cr ." == PLACEMENT SWEEP ==" cr
   ." host line " LINE FMT:.INT ."  bytes, page " PAGE FMT:.INT ."  bytes" cr
   ." arms " ARM-N @ FMT:.INT ." , reps " REPS FMT:.INT ." , rounds " ROUNDS FMT:.INT
   ." , calls per run " CALLS-PER-RUN FMT:.INT cr
   ." subject record: engine " K-CALLEE REF-OF ARM-SUBJ$ CODEGEN-SCAN:WORD-BYTES FMT:.INT
   ."  bytes, chain " K-CHAIN REF-OF ARM-SUBJ$ CODEGEN-SCAN:WORD-BYTES FMT:.INT
   ."  bytes, branchless stand-in " K-FLAT REF-OF ARM-SUBJ$ CODEGEN-SCAN:WORD-BYTES FMT:.INT
   ."  bytes; driver " K-CALLEE REF-OF ARM-DRV$ CODEGEN-SCAN:WORD-BYTES FMT:.INT ."  bytes" cr
   ." branch instructions in the subject: engine "
   K-CALLEE REF-OF ARM-SUBJ$ CODEGEN-SCAN:UNMOVABLE-IN FMT:.INT
   ." , chain " K-CHAIN REF-OF ARM-SUBJ$ CODEGEN-SCAN:UNMOVABLE-IN FMT:.INT
   ." , branchless " K-FLAT REF-OF ARM-SUBJ$ CODEGEN-SCAN:UNMOVABLE-IN FMT:.INT cr

   cr ." -- baseline: the same loop with the call removed --" cr
   HEAD K-BASE KIND.

   cr ." -- phase 1: engine callee entry swept, call site pinned --" cr
   HEAD K-CALLEE KIND.

   cr ." -- phase 2: call site swept, engine callee entry pinned --" cr
   HEAD K-SITE KIND.

   cr ." -- phase 3: chain callee entry swept, call site pinned --" cr
   HEAD K-CHAIN KIND.

   cr ." -- phase 4: page offset swept, line residue pinned --" cr
   HEAD K-PAGE KIND.

   cr ." -- phase 5: BRANCHLESS callee of the same 144 bytes, entry swept --" cr
   HEAD K-FLAT KIND.

   cr ." -- spans, in parts per thousand of the fastest arm of the kind --" cr
   ." callee sweep " K-CALLEE SPAN-PERMILLE FMT:.INT cr
   ." site sweep   " K-SITE SPAN-PERMILLE FMT:.INT cr
   ." chain sweep  " K-CHAIN SPAN-PERMILLE FMT:.INT cr
   ." page sweep   " K-PAGE SPAN-PERMILLE FMT:.INT cr
   ." flat sweep   " K-FLAT SPAN-PERMILLE FMT:.INT cr
   ." replicate floor (same residue, different address) " FLOOR-PERMILLE FMT:.INT cr

   cr ." -- chain against engine at MATCHED callee residue --" cr
   ." e%128   engine ps/call    chain ps/call  chain loss/1000" cr
   RESIDUES 0 ?do i INSN-BYTES * MATCH-ROW loop
   cr ;

;package

CALIGN:PUBLISH
CALIGN:RUN
CALIGN:REPORT
