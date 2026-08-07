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

\ The value the word this spelling denotes pushes: a `create`d word's address, a
\ `constant`'s number. The two refusals are the two ways the question has no
\ answer. A spelling that denotes nothing is refused because there is no second
\ authority to prefer and no number to fall back on. A word that did not leave
\ exactly one value where it was entered is refused because it is not a word of
\ this kind at all, and whatever is on the stack is not its answer.
\
\ WHAT THE COUNT PROVES AND WHAT IT LEAVES TO THE MISSING CAPABILITY. It settles
\ the arity: a word that leaves none, or two, or that consumed one and left none,
\ is caught, and the throw unwinds to the caller's own recovery with the stack
\ restored. What it cannot settle is the TYPE - a word that consumed one value
\ and left two answers the count and still answered with the wrong thing - and
\ that is precisely the typed result row dot habu-guard-an-executed-8a0f2f77
\ carries. The residual gap is named rather than papered over.
: FIXED-VALUE ( ptr u8 n -- n )
   SPELL-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
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

\ The checker's effect store answers only past a trusted boundary: its readers
\ are sig-less colon words that the seal strips, so checked code reaches them as
\ compiled calls behind a declared signature. The boundary is one word wide and
\ does no deciding - the refusing above it is ordinary checked Habu.
TRUSTED: EFF-CELLS ( ptr u8 n -- n n )
   EFFECT-QUERY if EFFECT-DIN-CELLS EFFECT-DOUT-CELLS else -1 -1 then ;

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

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
