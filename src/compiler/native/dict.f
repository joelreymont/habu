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

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
