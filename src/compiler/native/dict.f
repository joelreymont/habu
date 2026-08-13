\ dict.f - what the running engine's dictionary says a spelling denotes. One
\ concern: turning a name a program wrote into the fact the chain needs about it.
\
\ TWO QUESTIONS, ONE RESOLVER. A definition the chain compiles names other words,
\ and the chain needs two different facts about them: where a called word's code
\ starts, and what value a `create`d data word or a `constant` pushes. Both begin
\ with the same question - which record does this spelling denote, here, in the
\ scope this definition is compiled in - so the walk that answers it lives here
\ once and both askers read it. It used to live inside the migration entry, where
\ only the first asker could reach it.
\
\ THE ORDER IS THE ENGINE'S OWN AND NOT A SECOND OPINION ABOUT IT.
\ src/habu/habu1.f EMIT-FIND resolves a bare token in the open package's private
\ wordlist, then its public one, then the global wordlist, and a NAME:tail token
\ through the namespace record for NAME - which is what src/habu/xref.f
\ XREF-FIND-QUALIFIED is. That lookup is what resolves a definition's own body
\ when the engine compiles it, in the scope the chain's caller runs in, so any
\ other question would answer about a word the body does not name. What is NOT
\ walked is the used-publics leg the engine reaches after the global wordlist: a
\ word named through a `using` answers absent here and the compilation is refused
\ rather than made against an unconfirmed answer, which is the fail-closed
\ direction. Dot habu-walk-the-used-96694010 carries it, and dot
\ habu-one-resolver-for-4e9e3e59 carries giving the engine, the checker and this
\ file one resolver instead of three walks of one order.
\
\ WHY A DATA WORD'S VALUE IS OBTAINED BY RUNNING IT. There is no record slot
\ holding it. src/habu/xref.f's slots are START, LEN, FLAGS, NAME and WORDLIST;
\ the address a `create`d word pushes lives only as the four-instruction chain
\ src/habu/habu2.f C-ADDR-RAW baked into that word's own code, and
\ src/habu/layout.f:771-773 forbids recovering it by decoding - "Nothing ever
\ recognises a chain by looking at region bytes or at the value a chain carries: a
\ compiled word may hold inline non-instruction data, and an ordinary integer may
\ hold any value at all." So "ask the engine what this word is" has exactly one
\ honest form: enter the word and take what it leaves. A created word's published
\ effect is `-- ptr a` (habu2.f LASTC-TRUST:PUBLISH-PTR-A) and a `constant`'s is
\ `-- a`; both bodies push one value and return, so running one IS the engine
\ answering.
\
\ AND WHY IT IS SAFE TO ENTER ONE, WHICH IS A DIFFERENT QUESTION. Entering a word
\ to see what it leaves is only honest if the word's body is a push and nothing
\ else; entering an ordinary word to find that out would be finding out by doing
\ whatever it does. That is why WHICH DEFINER made a record is RECORDED and never
\ recognised: `constant`, `create` and `variable` stamp the record's flags cell
\ (src/habu/layout.f DKIND) at the moment they emit the body, and `does>` - the
\ one writer that replaces such a body with a clause - clears the stamp in the
\ same window it patches the return. So SPELL-FIXED below asks the record, and
\ only a record whose body is still the definer's own is ever entered.
\
\ AND WHY THAT IS THE HONEST PLACE FOR THE ANSWER RATHER THAN THE CALLER'S. The
\ address is a fact of the running process, and a caller that states it states a
\ number it obtained the same way a moment earlier. Two authorities for one fact
\ is one authority too many: the caller's copy goes stale the instant the word is
\ retired and redefined, and nothing downstream can tell a stale address from a
\ live one - it is an ordinary integer either way. So the question is asked where
\ the answer is used, and there is no parameter left for anyone to answer it
\ wrongly.

require lib/prelude.f
require lib/errors.f

package NDICT

public

\ ---- the scope every question below is asked in ------------------------------
\ The two wordlists the open package owns. They are published because WHICH scope
\ is open is itself a fact a caller may need to hold on to: the migration entry
\ takes them when it stages the first of several callees and refuses a staging
\ whose later rows were resolved in a different scope. A zero private cell is the
\ engine's own test for no package open.
: OPEN-PRI ( -- n )
   data-base PKG-PRI-CELL + @ ;

: OPEN-PUB ( -- n )
   data-base PKG-PUB-CELL + @ ;

private

\ ---- which record a spelling denotes -----------------------------------------
\ `search-wl` is the engine's own scan and case fold over one wordlist, and it
\ answers the record's code start - which is the address a call site branches to
\ and the address a word is entered at. Zero is its absent answer, and no word's
\ code starts there. XREF-QUAL-INDEX's answer for a token a second colon makes
\ name nothing.
-2 constant QUAL-BAD

\ The open package's two wordlists, in the engine's order. No package open
\ answers absent, so the global leg below is then the whole of the search.
: OPEN-START ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   OPEN-PRI 0= if 0 exit then
   a u OPEN-PRI search-wl {: pri:n :}
   pri 0<> if pri exit then
   a u OPEN-PUB search-wl ;

: BARE-START ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u OPEN-START {: open:n :}
   open 0<> if open exit then
   a u 0 search-wl ;

: QUAL-START ( ptr u8 n n -- n )
   XREF-FIND-QUALIFIED
   dup XREF-FOUND? 0= if drop 0 exit then
   XREF-START ;

\ ---- the same walk, answering the record rather than the code start ----------
\ WHY THIS EXISTS AT ALL, GIVEN THE WALK ABOVE. Some questions are about the
\ RECORD and not about the address in its first slot - whether the word runs at
\ compile time, whether it is engine-internal - and `search-wl` answers the slot,
\ not the record. So the legs are walked once more with the finder that answers a
\ record, in the same order.
\
\ AND WHY THAT IS NOT A SECOND OPINION. It would be, if either answer were
\ allowed to stand on its own. `search-wl` is the engine's own primitive and it
\ closes doors this finder does not - it short-circuits the owner-API private
\ wordlist to absent (src/habu/habu1.f) - so it stays the authority on WHETHER a
\ spelling denotes anything here and WHERE. The record found below is used only
\ for the slots the start does not carry, and it is REFUSED unless its own start
\ is the start the authority already answered. Two walks that must agree are one
\ answer with a check on it; the moment they disagree, nothing is compiled. Dot
\ habu-one-resolver-for-4e9e3e59 carries collapsing them into one walk.
: OPEN-REC ( ptr u8 n -- ptr a )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   OPEN-PRI 0= if XREF-NULL exit then
   a u OPEN-PRI XREF-FIND-WL {: pri:ptr :}
   pri XREF-FOUND? if pri exit then
   a u OPEN-PUB XREF-FIND-WL ;

: BARE-REC ( ptr u8 n -- ptr a )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u OPEN-REC {: open:ptr :}
   open XREF-FOUND? if open exit then
   a u 0 XREF-FIND-WL ;

: SPELL-REC ( ptr u8 n -- ptr a )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u XREF-QUAL-INDEX {: q:n :}
   q QUAL-BAD = if XREF-NULL exit then
   q 0 >= if a u q XREF-FIND-QUALIFIED exit then
   a u BARE-REC ;

\ ---- entering the word the spelling denoted ----------------------------------
\ THE ONE BOUNDARY THIS FILE NEEDS, AND WHY THE CHECKER CANNOT CERTIFY IT.
\ `execute` enters a word the dictionary named at run time, so what it consumes
\ and leaves is not known where the call is written. The checker can only admit
\ such a call once it can be given the effect the entered word is required to
\ have and can hold the call to it - an arity-guarded `execute` with a typed
\ result row, which is dot habu-guard-an-executed-8a0f2f77. Until that lands the
\ trust is here and it is one word wide: nothing but the `execute` is unchecked,
\ the resolution above it and the arity check below it are ordinary checked Habu.
\ It is the same shape src/compiler/ir/context.f CE-RUN keeps around the one
\ operation its checker cannot express.
TRUSTED: RUN-WORD ( n -- n )
   execute ;

\ The depth the answer has to stand one above. It is a cell rather than a local
\ because a local binding taken AFTER the entered word ran would itself read
\ whatever that word left - so a word that pushed nothing would be diagnosed by
\ reading a value belonging to the caller, which is the failure this check exists
\ to refuse. Written before the entry and read after it, the count is the
\ caller's own and cannot be moved by what the entered word did.
variable FX-BASE

public

\ Where the code of the word this spelling denotes starts, or zero when it
\ denotes no word in the scope this runs in.
: SPELL-START ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u XREF-QUAL-INDEX {: q:n :}
   q QUAL-BAD = if 0 exit then
   q 0 >= if a u q QUAL-START exit then
   a u BARE-START ;

\ ---- which definer made the record a spelling denotes -------------------------
\ THE THREE ANSWERS ARE THE THREE THINGS A MENTION OF A NAME CAN MEAN. A record
\ the `constant` definer made means a number; one `create` or `variable` made
\ means the address of that word's own storage, which a snapshot moves with the
\ DATA region and which therefore may not be compiled as an ordinary number; and
\ anything else means a call. The stamp is the definer's, made when the body was
\ emitted and cleared by the only writer that replaces such a body, so this is a
\ question about the record and not a guess about the bytes it points at.
\
\ IT IS ASKED THROUGH THE SAME WALK AND THE SAME THREE REFUSALS CALL-TARGET USES,
\ because a stamp on a record nobody can reach by this name says nothing about
\ what this body means. A record the finder does not agree with the engine's own
\ start about, and a retired one, answer NONE - the second is the staleness that
\ matters most here, since a retired data word's address is still a perfectly
\ ordinary integer.
0 constant FIXED-NONE                \ nothing a mention of this name folds to
1 constant FIXED-VAL                 \ `constant`: the body pushes a decided number
2 constant FIXED-ADDR                \ `create`/`variable`: the body pushes a DATA address

: SPELL-FIXED ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u SPELL-START {: start:n :}
   start 0= if FIXED-NONE exit then
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if FIXED-NONE exit then
   rec XREF-START start <> if FIXED-NONE exit then
   rec XREF-RETIRED? if FIXED-NONE exit then
   rec XREF-FLAGS {: f:n :}
   f DKIND:VAL and 0<> if FIXED-VAL exit then
   f DKIND:ADDR and 0<> if FIXED-ADDR exit then
   FIXED-NONE ;

\ The value the word this spelling denotes pushes: a `create`d word's address, a
\ `constant`'s number. The three refusals are the three ways the question has no
\ answer. A spelling that denotes nothing is refused because there is no second
\ authority to prefer and no number to fall back on. A spelling whose record no
\ definer stamped is refused BEFORE the word is entered, because entering it is
\ what a caller may only do to a body that is a push. And a word that did not
\ leave exactly one value where it was entered is refused because it is not a
\ word of this kind at all, and whatever is on the stack is not its answer.
\
\ WHAT THE COUNT PROVES AND WHAT IT LEAVES TO THE MISSING CAPABILITY. It settles
\ the arity: a word that leaves none, or two, or that consumed one and left none,
\ is caught, and the throw unwinds to the caller's own recovery with the stack
\ restored. What it cannot settle is the TYPE - a word that consumed one value
\ and left two answers the count and still answered with the wrong thing - and
\ that is precisely the typed result row dot habu-guard-an-executed-8a0f2f77
\ carries. The residual gap is named rather than papered over.
: FIXED-VALUE ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u SPELL-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
   a u SPELL-FIXED FIXED-NONE = if E-NDICT-KIND throw then
   depth FX-BASE !
   start RUN-WORD
   depth FX-BASE @ 1+ <> if E-NDICT-VALUE throw then ;

\ ---- and how many cells a call to it moves --------------------------------
\ THE THIRD QUESTION, AND IT HAS THE SAME ANSWER-WHERE-IT-IS-USED SHAPE. A call
\ site has to know how many cells it hands the callee and how many it takes back.
\ That is not the caller's opinion either: it is what the CHECKER accepted for
\ that name, and the checker is the only authority on it. A migration that states
\ the arity itself compiles a routine that moves the wrong number of cells and
\ nothing downstream refuses it - the selector builds the store run, the load run
\ and both byte counts from the one stated number, so every derivation it holds
\ against itself agrees (dot habu-resolve-a-callee-0340dfde, mutation of
\ 2026-08-02). So the count is read off the checker here, beside the address, and
\ a caller states a NAME and nothing else.
\
\ AND CELLS ARE ASKED FOR DIRECTLY, BECAUSE TERMS ARE A DIFFERENT NUMBER. The
\ checker used to publish an effect only as a count of TERMS, and a term is not a
\ cell: `ptr u8 n` is two terms and two cells, but one term of a three-cell
\ layout family is one term and three cells. This file read the term count and
\ then had to argue its way back to cells from a coarse per-term family enum -
\ sound only for the terms the enum resolved, so a row carrying any other term
\ was answered absent. That cost every named CONSTANT, whose published effect is
\ `-- a`: a bare raw type variable the enum leaves gray, refused rather than
\ compiled against a guessed width. Dot habu-export-the-checker-2bbc831c closed
\ it at the source - the checker now records each row cell's width as it stores
\ the effect, using the same ROW-TERM-CELLS that computes ER.MINI, and publishes
\ the row's total through EFFECT-DIN-CELLS / EFFECT-DOUT-CELLS. So the answer
\ arrives as the number this file actually wants, from the only authority that
\ knows it, and the reconstruction is gone rather than improved.
\
\ IT STILL FAILS CLOSED, on the checker's word rather than this file's. A row the
\ checker cannot width answers CELLS-NONE - the case a snapshot written before
\ the width field existed restores - and that arrives here as ARITY-NONE and a
\ named refusal, exactly as an uncertified name does.

\ The store's readers are sig-less colon words the seal marks internal, so this
\ used to arrive past a one-word trusted boundary - the only route the checker
\ admitted. src/core/checker.f now carries a `PRIM:` row per reader stating the
\ effect its own definition declares, so the call is ordinary checked Habu and
\ the word below is the file's own vocabulary rather than a boundary
\ (dot habu-turn-the-registry-4c064064).
: EFF-CELLS ( ptr u8 n -- n n )
   EFFECT-QUERY if EFFECT-DIN-CELLS EFFECT-DOUT-CELLS else -1 -1 then ;

\ ---- and whether control comes back from it ----------------------------------
\ THE FOURTH QUESTION, AND IT HAS THE SAME OWNER AS THE ARITY. A call to `throw`
\ or `die`, or to any word whose own body ends in one, has no normal
\ continuation: the values below it never reach the block after the call,
\ because control never gets there. A caller that compiled such a call as an
\ ordinary one has to make the path it is on join the next one, and there is
\ nothing to join with - which is the E-NELAB-JOIN the chain refused
\ `: JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;` with.
\
\ THE ANSWER IS THE CHECKER'S AND THIS FILE ASKS FOR IT BY NAME. The checker
\ records a control flag per WORD, in the same store and keyed by the same
\ symbol as everything else it certifies (src/core/checker.f, NORET-AXIOMS and
\ CTL-FLAGS): `throw` and `die` carry theirs as axioms, and a definition whose
\ own paths all end in one earns the flag when it is certified. So there is no
\ list of dead words here and no spelling compared - the same discipline
\ SPELL-ARITY keeps, for the same reason. A second list would go stale the
\ moment a package defined its own `throw`, and the checker learned that the
\ hard way: keying deadness on a spelling certified a body whose arm returned
\ nothing where its signature promised a cell.
\
\ THE QUESTION IS ASKED AND NOT THE BITS, and the mask has moved to the encoding
\ it reads. Which bit means dead is the checker's; what this file needs to know
\ is whether the call comes back. That used to be a copy of `CTL-DEAD and 0 <>`
\ here behind a trusted boundary, because a boot prefix reader was all this file
\ could reach; src/core/checker.f CTL-DEAD? now answers the question itself and
\ SPELL-DEAD? below calls it, so there is one mask and it lives with its encoding.

\ ---- which cells of a row may not be separated from one another ---------------
\ WHY A CALL SITE HAS TO KNOW THIS AND CANNOT WORK IT OUT. A value of a layout
\ family occupies several stack cells, and those cells are one value: reordering
\ them, or moving one without the others, destroys it. The compile-time value
\ vector in src/compiler/native/elaborate.f holds one entry per CELL, so a
\ rename - which is nothing but a permutation of that vector - will happily take
\ a bundle apart, and the CELL counts still add up, so nothing downstream
\ notices. That is dot habu-rename-over-rows-982167af, measured as four working
\ programs the chain compiled into wrong ones.
\
\ WHAT THE CHECKER ALREADY KNOWS AND HOW IT SPELLS IT. A declared signature
\ records a layout value as one term PER CELL, each carrying its position in the
\ bundle (EFFECT-DIN-SLOT / EFFECT-DOUT-SLOT: slot+1, 0 for an ordinary term).
\ That is the only fact that separates `( option<n> n -- )` from `( a b n -- )`:
\ the two agree on the term count, the cell count and the family of every term,
\ and disagree only here.
\
\ AND WHERE IT SPELLS IT THE OTHER WAY. A generated constructor's row keeps the
\ value as ONE term several cells wide instead - `OPTION:SOME` leaves one term of
\ two cells - so its terms and its cells disagree in number and no per-term slot
\ marks anything. There is no exported per-term width to say WHICH term is the
\ wide one, so a row whose two counts disagree is reported glued THROUGHOUT. That
\ is deliberately more than the truth: it can only cause a refusal where a finer
\ answer would have compiled, never the reverse, and the finer answer belongs
\ with the row-wise rename itself (dot habu-rename-rows-row-143c0331).
\
\ THE ANSWER IS A BITMASK OVER CELLS, bit i for the i-th cell from the BOTTOM of
\ the row, which is how the value vector indexes it. One cell holds it because a
\ row wider than the vector is refused before it can be asked about.

\ ---- and what a quotation TERM of one of those rows takes and leaves ----------
\ THE FIFTH QUESTION, AND IT HAS THE SAME OWNER AS THE ARITY. A body written
\ `[: … ;]` is compiled as a routine, and a routine's arity is how many cells its
\ caller hands it and how many it takes back. Nothing at the place the body is
\ WRITTEN says what those are: the numbers belong to the term that CONSUMES it -
\ the operand a callee declares, or the result the enclosing definition declares -
\ and the checker is the only authority on either.
\
\ THE CHECKER PUBLISHES IT AS A DESCENT. A quotation is one term of a row
\ carrying a whole effect of its own, and src/core/checker.f moves its row latch
\ onto that effect and back (EFFECT-DIN-QUOT / EFFECT-DOUT-QUOT / EFFECT-QUOT-UP).
\ So the readers below descend, read the same two counts every other row is read
\ with, and come back up - and the descent is closed on every path, including the
\ ones that decline, because a latch left displaced would answer the next
\ question about this quotation instead of about the row that asked it.
\
\ TERMS ARE COUNTED FROM THE TOP AND CELLS FROM THE BOTTOM, and that is why the
\ index is refused rather than converted when the two counts disagree. A caller
\ knows which CELL of the row it is holding; the descent is indexed by TERM; and
\ the two are the same index only while every term of the row is one cell wide,
\ which is exactly the boundary this file already draws for SPELL-ARITY. A row
\ where they differ carries a term several cells wide with nothing to say which
\ term that is, so the answer is "no quotation there" and the caller refuses by
\ name instead of descending into whichever term the arithmetic landed on.
\
\ AND A BODY THAT IS NOT AN ORDINARY ROUTINE IS DECLINED. EFFECT-QUOT-SIMPLE? is
\ the checker's own three-clause question - the return rows are neutral, there is
\ no throw edge, and the fall-through is live - and a body failing any of them is
\ not something a caller may reach with a branch and come back from. Nothing here
\ re-derives those clauses; this asks the one word that owns them.
\ The seven readers those clauses need are named directly at the call sites
\ below. Each used to have a one-line `TRUSTED:` bridge here whose whole body was
\ the call, and a bridge that renames a word it can now simply call would be a
\ second name for one question. The one row that is still written out is the one
\ that computes: four reads in a fixed order, so a caller takes the counts of ONE
\ row rather than four answers it has to keep straight itself.
: EFF-COUNTS ( -- n n n n )        \ din terms, din cells, dout terms, dout cells
   EFFECT-DIN-N EFFECT-DIN-CELLS EFFECT-DOUT-N EFFECT-DOUT-CELLS ;

public
0 constant GLUE-NONE                 \ no cell of the row belongs to a bundle
private

64 constant GLUE-MAX                 \ cells of one row a mask can describe

: GLUE-ALL ( n -- n ) {: cells:n :}   \ every cell of a `cells`-wide row glued
   cells 0 <= if GLUE-NONE exit then
   cells GLUE-MAX >= if -1 exit then
   1 cells lshift 1 - ;

\ ONE CELL IS NOT A BUNDLE, and that is the whole subtlety of this walk. Every
\ value of a layout family is recorded cell by cell with its position, INCLUDING
\ the families whose values are a single cell - a plain closed enum is one tag and
\ nothing else. Marking those would refuse renames over ordinary one-cell values
\ for no reason: a value that occupies one cell cannot be taken apart by moving
\ cells around. So a term's slot is not the question; the WIDTH of the value the
\ term belongs to is, and that width is readable from the top of each run - the
\ cells of one value carry slots W, W-1 ... 1 downwards, so the first slot met
\ going down IS the width. A run of one is skipped, and a longer one is marked
\ whole and stepped over.
\
\ Terms arrive top first and cells are numbered from the bottom, so term i is cell
\ cells-1-i. That correspondence holds only while the two counts agree; when they
\ do not, the row carries a term wider than one cell with no slot to find it by,
\ and the whole row is glued without walking it.
variable RG-MASK   variable RG-I   variable RG-S

: RG-BIT ( n -- ) {: bit:n :}
   RG-MASK @  1 bit lshift or  RG-MASK ! ;

: RG-RUN ( n n n -- ) {: cells:n top:n width:n :}   \ one value of `width` cells whose top term is `top`
   width 0 ?do  cells 1 - top i + -  RG-BIT  loop ;

: ROW-GLUE ( n n bool -- n ) {: terms:n cells:n din:bool :}
   cells 0 <= if GLUE-NONE exit then
   cells GLUE-MAX > if cells GLUE-ALL exit then
   terms cells <> if cells GLUE-ALL exit then
   GLUE-NONE RG-MASK !
   0 RG-I !
   begin RG-I @ terms < while
      din if RG-I @ EFFECT-DIN-SLOT else RG-I @ EFFECT-DOUT-SLOT then RG-S !
      RG-S @ 2 < if
         RG-I @ 1 + RG-I !
      else
         RG-I @ RG-S @ + terms > if              \ a run reaching past the row: fail closed
            cells GLUE-ALL RG-MASK !  terms RG-I !
         else
            cells RG-I @ RG-S @ RG-RUN
            RG-I @ RG-S @ + RG-I !
         then
      then
   repeat
   RG-MASK @ ;

public

\ Where a compiled CALL may branch to for this spelling, or zero when it may not
\ branch there at all. It answers the ADDRESS rather than a yes, because every
\ caller that wants the answer wants the address too and asking twice would walk
\ the dictionary twice for one question.
\
\ A NAME IN A BODY IS NOT ALWAYS A CALL, and the two flags that say so are the
\ two src/compiler/native/publish.f already refuses a publication for, for the
\ same reason. An IMMEDIATE word runs while the source around it is being
\ compiled - a body that writes one is asking for something to happen THEN, and a
\ routine that branches to it at run time does not do that thing late, it does a
\ different thing entirely. An ENGINE-INTERNAL word has no name past the seal, so
\ a body naming one is naming something that will not be there. Neither is a
\ capability this chain is missing, so neither is guessed at: the spelling is
\ answered not-callable and refused as unmodelled, by name.
\
\ THIS IS MEASURED AND NOT ARGUED, AND THE WALK IS A TOOL. Asking every record in
\ a loaded engine which ones SPELL-START resolves AND SPELL-ARITY sizes - the two
\ answers that would otherwise be enough to build a call - finds 1632 words, and
\ among them exactly `include` and `require`, both IMMEDIATE, plus the RETIRED
\ records the walk sets aside separately. Those are exactly what this refuses and
\ nothing else does: without it a body
\ naming `include` compiles into a routine that branches, at run time, into the
\ word that loads a file while the compiler is reading one. The internal clause
\ finds nothing today, and it is kept because it is publish.f's own rule about
\ publish.f's own flag, and a predicate that answers "may a call branch here"
\ half way is worse than one nobody has to remember the exceptions to.
\
\ The walk is tools/callable-arity-probe.f, so the paragraph above can be
\ re-measured instead of believed. It moved once already: before the checker
\ published cell widths the same walk found 805, and every one of the 827 words
\ it gained is a name whose width used to be unstatable here.
\
\ A RETIRED RECORD IS THE THIRD, and it is the staleness the caller-stated
\ address could never see. `forget` and a redefinition retire the old record and
\ leave its code where it was, so its start is still a perfectly ordinary integer
\ pointing at instructions nobody can reach by name any more. Asked here, at the
\ moment the call is compiled, it answers retired and nothing is emitted.
: CALL-TARGET ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u SPELL-START {: start:n :}
   start 0= if 0 exit then
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START start <> if 0 exit then
   rec XREF-RETIRED? if 0 exit then
   rec XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if 0 exit then
   f DNAME-IMM and 0<> if 0 exit then
   start ;

-1 constant ARITY-NONE

\ How many cells a call to the word this spelling denotes consumes, and how many
\ it leaves. ARITY-NONE twice when the checker certified no effect for the name,
\ or when it certified one whose width IT cannot state. Both rows are demanded:
\ a call that knows what it hands over and not what it takes back is no more
\ compilable than one that knows neither, so either row absent refuses the pair.
\
\ ABSENT IS TESTED AS "NOT A COUNT", not as a particular number. A width is a
\ count of cells and so is never negative; the checker spells its own absence
\ CELLS-NONE and the boundary spells an uncertified name -1, and this asks the
\ property both of them have instead of matching either spelling. The pair this
\ file publishes for it is its own ARITY-NONE, so no caller has to know that the
\ two vocabularies currently agree on a value.
: SPELL-ARITY ( ptr u8 n -- n n )
   EFF-CELLS {: din:n dout:n :}
   din 0 < if ARITY-NONE ARITY-NONE exit then
   dout 0 < if ARITY-NONE ARITY-NONE exit then
   din dout ;

\ Which cells of this spelling's two rows belong to a multi-cell value, as the
\ bitmasks described above: bit i for the i-th cell from the bottom of the row.
\ GLUE-NONE twice for a name the checker holds no effect for, which is the same
\ answer as "nothing here is bundled" and safe to be: a name with no effect is
\ refused by SPELL-ARITY before any caller reaches for its glue.
: SPELL-GLUE ( ptr u8 n -- n n )
   EFFECT-QUERY 0= if GLUE-NONE GLUE-NONE exit then
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   dn dc true ROW-GLUE
   on oc false ROW-GLUE ;

\ ---- the quotation a term of one of those rows IS ----------------------------
public
-1 constant QUOT-NONE                \ that term is no quotation this chain may compile
private

\ The two counts, read while the latch is down, with the latch put back before
\ either is answered. Declining and answering leave the latch in the same place,
\ which is what makes a caller's next question about the row it thinks it is
\ about.
: QUOT-CELLS ( -- n n )
   EFFECT-QUOT-SIMPLE? {: simple:bool :}
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   EFFECT-QUOT-UP 0= if QUOT-NONE QUOT-NONE exit then
   simple 0= if QUOT-NONE QUOT-NONE exit then
   dc 0 < oc 0 < or if QUOT-NONE QUOT-NONE exit then
   dn dc <> on oc <> or if QUOT-NONE QUOT-NONE exit then
   dc oc ;

\ Whether this row's terms and cells are the same index, which is what makes term
\ `i` and cell `i` the same thing to ask about.
: ROW-INDEXABLE? ( bool -- bool )
   {: din:bool :}
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   din if dn dc = dc 0 >= and exit then
   on oc = oc 0 >= and ;

public

\ What the quotation at term `i` of this spelling's INPUT row takes and leaves,
\ in cells, counting terms from the TOP of the row as the checker does.
\ QUOT-NONE twice when there is no such term, when it is not a quotation, when
\ the row's terms and cells are not the same index, or when the body is not one a
\ caller may reach with a branch and come back from.
: SPELL-QUOT-DIN ( ptr u8 n n -- n n )
   {: a u:n i:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u EFFECT-QUERY 0= if QUOT-NONE QUOT-NONE exit then
   true ROW-INDEXABLE? 0= if QUOT-NONE QUOT-NONE exit then
   i EFFECT-DIN-QUOT 0= if QUOT-NONE QUOT-NONE exit then
   QUOT-CELLS ;

\ The same for term `i` of its OUTPUT row.
: SPELL-QUOT-DOUT ( ptr u8 n n -- n n )
   {: a u:n i:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u EFFECT-QUERY 0= if QUOT-NONE QUOT-NONE exit then
   false ROW-INDEXABLE? 0= if QUOT-NONE QUOT-NONE exit then
   i EFFECT-DOUT-QUOT 0= if QUOT-NONE QUOT-NONE exit then
   QUOT-CELLS ;

\ Whether control comes back from a call to the word this spelling denotes.
\ False for a name the checker holds no control flag for, which is every
\ ordinary word: a call that comes back is the common case and the one a caller
\ needs no permission for. A name the checker certified nothing at all for is
\ refused by SPELL-ARITY before any caller reaches for this.
: SPELL-DEAD? ( ptr u8 n -- bool )
   CTL-DEAD? ;

\ ---- and whether a call to it leaves the caller's return stack alone ----------
\ THE SEVENTH QUESTION, AND THE NATIVE CHAIN CANNOT COMPILE A CALL WITHOUT IT.
\ src/compiler/native/elaborate.f models the return stack ENTIRELY at compile
\ time: `>r` moves a value id between two compile-time vectors, emits no
\ instruction, and never touches the engine's return-stack region. That rests on
\ the parked values being the ELABORATOR's own bookkeeping - and a callee whose
\ declared effect takes a cell off its caller's return stack, or leaves one on it,
\ moves a stack the caller's bookkeeping is the only record of. There is nowhere
\ to put that motion, so a call to such a word is refused rather than compiled
\ into one the elaborator's two vectors no longer describe.
\
\ IT ASKS WHAT THE ROWS SAY AND NOT WHAT THE SIGNATURE SPELLS, which is the whole
\ reason the checker publishes the question instead of a flag for the `|` clause.
\ `( n | R -- n | R )` writes a clause and moves nothing; a word with no clause at
\ all recorded two empty rows because the checker's own balance check PROVED it
\ moves nothing, which is almost every word in the tree. Reading the clause would
\ refuse the first and admit neither fact.
\
\ FALSE WHEN NOTHING RESOLVES, the direction every reader in this file takes: a
\ name the checker holds no effect for promises nothing about a return stack
\ either, and answering neutral would let a call be compiled on a promise nobody
\ made. Such a name is refused by SPELL-ARITY first in every path that reaches
\ here, so this is the second derivation rather than the only one.
: SPELL-RET-NEUTRAL? ( ptr u8 n -- bool )
   EFFECT-QUERY 0= if false exit then
   EFFECT-RET-NEUTRAL? ;

private

\ ---- and which cell a deferred word dispatches through ------------------------
\ THE SIXTH QUESTION, AND IT IS THE ONE `is` NEEDS. `[: … ;] is NAME` stores an
\ execution token into NAME's dispatch cell, and where that cell is, is a fact of
\ the running engine exactly as a callee's entry address is: `defer` allocates
\ the cell in the DP heap and writes its address into a meta trailer just past
\ the word's code (src/habu/habu2.f C-DEFER-CELL and C-DEFER-META-WRITE). So the
\ question is asked here, beside the other five, and a caller names a spelling
\ and nothing else.
\
\ THE TRAILER IS RECOGNISED BY ITS MAGIC AND NEVER BY ITS SHAPE. Past any
\ record's code stands whatever the next definition put there, and
\ src/habu/layout.f's own rule about the region says it plainly: "an ordinary
\ integer may hold any value at all". So the first cell of the trailer is held
\ against DEFER-MAGIC - src/habu/layout.f's constant, the same one the engine
\ writes and the same one the engine's own `is` checks in C-DEFER-TARGET-META -
\ and a record whose trailer does not carry it answers absent. That is what
\ makes "this spelling names a deferred word" a structural question rather than
\ a guess about a number: a `create`d word whose data happens to begin with the
\ magic would have to have been written with the magic in it, and a word whose
\ code is followed by a plausible-looking address is answered absent because the
\ cell BEFORE that address is not the magic.
\
\ THE TWO READS ARE THE ONE THING THIS FILE CANNOT DO IN CHECKED HABU, for the
\ same reason RUN-WORD above cannot: the trailer is memory at an address the
\ dictionary handed over, and the checker has no type for it. The boundary is two
\ cells wide, does no deciding, and every refusal around it is ordinary checked
\ Habu. Dot habu-typed-xt-storage-ddad4af8's typed cell is the capability that
\ would let the trailer be read as a declared shape.
TRUSTED: TRAILER@ ( n -- n )
   @ ;

public

\ Where the word this spelling denotes dispatches through, or zero when it is not
\ a deferred word here. Zero is the absent answer for the same reason it is
\ CALL-TARGET's: no dispatch cell is at address zero.
\
\ THE RECORD IS THE AUTHORITY ON WHERE THE TRAILER IS. A record carries its
\ code's start and its length, and the trailer begins where the code ends - so
\ the two slots the walk above already answers are the whole of the arithmetic,
\ and there is no second opinion about a word's extent for this to drift from.
: SPELL-DEFER-CELL ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-RETIRED? if 0 exit then
   rec XREF-START  rec XREF-LEN +  {: meta:n :}
   meta TRAILER@ DEFER-MAGIC <> if 0 exit then
   meta 8 + TRAILER@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
