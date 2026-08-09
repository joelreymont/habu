\ migrate.f - the production entry: one checked colon definition, compiled by
\ the engine and then recompiled by the native chain, published as an ordinary
\ word. One concern: driving the whole chain for one definition.
\
\ WHAT A MIGRATION IS. The caller states the definition's source. The engine
\ compiles it exactly as it compiles every other definition - same interpreter,
\ same checker, same emitter - and publishes the word. That compilation is what
\ produces the tape: the checker's own reader fills it while it consumes the
\ definition, so the tokens the chain elaborates are the tokens the checker
\ certified and not a second reading of the same text. The chain then compiles
\ that tape, and the publication seam points the word's dictionary record at the
\ result. Afterwards the word is entered the way every word is entered, and the
\ code the old emitter produced for it is simply no longer reachable by that
\ name.
\
\ WHY THE SOURCE IS HANDED OVER RATHER THAN READ FROM THE INPUT STREAM. The tape
\ only exists while a recording unit is open, and a unit is opened by a word - so
\ something has to run before the definition and something after it, and the two
\ have to be one word or the tape's context dies between them. Handing the
\ definition over as text makes that one word. It is not a weaker tie to the
\ definition than typing it at top level would be: `evaluate` IS the interpret
\ path, the word this text publishes is a real dictionary record, and the tape is
\ filled by the checker's reader during that publication. Giving the migration a
\ definer that reads a definition out of the input stream is dot
\ habu-parse-a-migrated-b38a83d9, and nothing else here changes when it lands.
\
\ THE NAME IS NOT A PARAMETER. Which word was defined is read off the dictionary
\ rather than restated by the caller: the migration requires that the source
\ published exactly one record and takes its name from that record. A caller
\ therefore cannot ask for one definition's code to be installed under another
\ name, and a source that defines nothing, or two things, is refused instead of
\ migrating whichever record happened to be newest.
\
\ WHAT IT REFUSES. A second migration inside a live one, source longer than the
\ recorder's buffer, a definition the engine's own check did not certify, a
\ source that did not publish exactly one word, and a name that no longer
\ resolves to that record. Everything the chain refuses keeps the chain's own
\ name - a body outside the dialect's vocabulary is E-HIR-UNMODELED, a control
\ structure the elaborator cannot close is E-NELAB-CTRL, register pressure the
\ frame cannot absorb is E-A64RA-PRESSURE - and none of them is caught here, so a
\ word the chain cannot compile is refused by name with its dictionary record
\ untouched and keeps running the code the engine compiled for it.
\
\ WHAT IT STILL STATES. How many values the definition takes and leaves, and how
\ many scratch registers its routine may use. The checker knows the first two -
\ it parsed the declared effect during the very scan that filled the tape - but
\ publishes an effect only through a lookup by name into its live store, which
\ answers about whoever carries that name now rather than about this definition.
\ Binding the accepted effect to the recorded unit is dot
\ habu-bind-checker-env-ed4f9f87; the register count is a budget, and choosing it
\ from the routine rather than from the caller is dot
\ habu-choose-the-register-a95390ac.
\
\ AND, FOR A DEFINITION THAT CALLS ANOTHER WORD, NOTHING AT ALL ANY MORE. The
\ body names the word and that is the whole of it. Where the callee's code starts
\ and how many cells a call to it moves are facts of the running engine and of
\ the checker, and src/compiler/native/elaborate.f RESOLVE-SCAN asks them at the
\ point they are used, through src/compiler/native/dict.f, for every name in the
\ body the dialect does not model. There is no parameter left for a caller to
\ answer wrongly, and no ceiling on how many words a body may call.
\
\ THE STAGING ENTRIES BELOW ARE WHAT IS LEFT OF THE OLD ARRANGEMENT. CALLEE still
\ takes a spelling, an address and an effect, and DEFINE-CALLING still spends the
\ list; a caller that uses them can still state an effect that is not the one the
\ checker certified, which is the lie dot habu-resolve-a-callee-0340dfde was
\ opened for. Resolution closes that class for every name nobody stages - which
\ is every name in a body by default - and deleting the staging path outright is
\ the remaining half of that dot. What holds the staging together meanwhile: a
\ spelling and an address are ONE fact stated twice, so RESOLVES-TO-ENTRY below
\ refuses an address that is not where that spelling's word starts, settling the
\ name, the package it was published in and the address in one comparison.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/inline.f
require src/compiler/native/select.f
require src/compiler/native/spill.f
require src/compiler/native/combine.f
require src/compiler/native/emit.f
require src/compiler/native/publish.f

package NMIGRATE

private

\ ---- the one boundary this file needs ---------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how a definition reaches the engine's own compile path from inside a word.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

512 constant TEXT-CAP                \ the longest definition a unit may record
128 constant TAPE-CAP                \ the most tokens one definition may hold
\ The longest name or spelling this file holds. Two things are measured by it -
\ the name the migration publishes under, and each callee spelling a caller
\ stages - and a staged spelling may be QUALIFIED where a publication's own name
\ is never more than a tail, so the ceiling is the longer of the two forms. A
\ spelling past it is refused rather than truncated into a name that denotes
\ another word.
64 constant NAME-CAP

create TXT TEXT-CAP allot
create NAME-BUF NAME-CAP allot
variable NAME-U
variable NAME-WID

\ ---- the words one migrated definition calls ---------------------------------
\ A body may call several DIFFERENT words - `||a-b|| / ||b||` calls one word for
\ the distance and another for the norm - so the caller stages one row per callee
\ and then migrates. Each row is what the word model needs and nothing more: the
\ spelling the body writes, where the callee's code starts, and the effect it
\ declares. All three are the caller's statement for the same reason a data
\ word's address is (dot habu-resolve-a-callee-0340dfde).
\
\ THE LIST BELONGS TO ONE MIGRATION AND IS CLEARED BY IT. RUN empties it when the
\ run ends, whether the run succeeded or threw, so every migration starts with an
\ empty list and a row staged for a migration that then failed cannot be picked
\ up by the next one. An entry that takes no list refuses a staged one rather
\ than running with rows nothing will use.
4 constant CALLEES-MAX

create CALLEE-BUF CALLEES-MAX NAME-CAP * allot
here CELL 1- and CELL swap - CELL 1- and allot
variable CALLEE-N
create CALLEE-U CALLEES-MAX cells allot
create CALLEE-ADDR CALLEES-MAX cells allot
create CALLEE-IN CALLEES-MAX cells allot
create CALLEE-OUT CALLEES-MAX cells allot

: CALLEE-AT ( n -- n )
   dup 0 < over CALLEE-N @ >= or if E-NMIGRATE-STATE throw then ;

: CALLEE$ ( n -- ptr u8 n )
   CALLEE-AT {: k:n :}
   CALLEE-BUF k NAME-CAP * +  k cells CALLEE-U + @ ;

: CALLEES-CLEAR ( -- )
   0 CALLEE-N ! ;

: CALLEES-NONE-CK ( -- )
   CALLEE-N @ 0<> if E-NMIGRATE-STATE throw then ;

\ ---- holding a staged spelling against the address staged with it -------------
\ WHY THE TWO ARE ONE FACT. The caller obtained the address by resolving the
\ spelling and the body reaches the routine by writing it, so a staging where
\ they disagree is a caller contradicting itself. Nothing downstream catches it:
\ the recorded-body check (src/compiler/native/elaborate.f CALLEE-COPY?) is only
\ reached for an address that HAS a row, and the CALL a site emits instead
\ branches to the stated address whatever the spelling said. So it is settled
\ here, where the two arrive together, and settling it settles the NAME, the
\ PACKAGE the routine was published in and the ADDRESS in one comparison.
\
\ WHICH RECORD A SPELLING DENOTES IS NOT ASKED HERE ANY MORE.
\ src/compiler/native/dict.f owns that walk - the engine's own order, its
\ fail-closed treatment of a name reached only through a `using`, and the reason
\ both are what they are. It moved there when the word model started asking the
\ same question about a data word's spelling: one resolver, two askers.
\
\ The one refusal. A spelling that denotes nothing is refused for the same reason
\ a mismatched address is: there is no second authority to prefer, and a
\ migration compiled against an unconfirmed address is a routine that branches
\ somewhere nobody named.
: RESOLVES-TO-ENTRY ( ptr u8 n n -- )
   {: ca cu:n entry:n :} \ typed-local-lint: allow-bare-local - ca keeps the ptr u8 byte-span role
   ca cu NDICT:SPELL-START {: start:n :}
   start 0= if E-NMIGRATE-CALLEE throw then
   start entry <> if E-NMIGRATE-CALLEE throw then ;

\ ---- and the scope a staged list is spent in ---------------------------------
\ A spelling is resolved in the scope the STAGING runs in, and the definition it
\ was staged for is compiled in the scope the MIGRATION runs in. Those are one
\ scope for every caller there is, because a caller stages its callees and
\ migrates in one word - and the whole of the check above rests on it, so it is
\ REFUSED rather than assumed. The wordlists the resolver walked are recorded
\ with the first row and held against the ones each later row and the run itself
\ find, which turns "the scope did not move between them" into the same kind of
\ refusal the publication seam holds an emission's placement to.
\
\ IT RETIRES WITH THE CALLER'S ADDRESS. When the migration reads a callee's
\ address off the dictionary itself (dot habu-resolve-a-callee-0340dfde) the
\ resolution happens inside the run, in the one scope that can matter, and both
\ this and the check above go with it.
variable CALLEE-PRI
variable CALLEE-PUB

: CALLEES-SCOPE! ( -- )
   NDICT:OPEN-PRI CALLEE-PRI !
   NDICT:OPEN-PUB CALLEE-PUB ! ;

: CALLEES-SCOPE-CK ( -- )
   CALLEE-N @ 0= if exit then
   NDICT:OPEN-PRI CALLEE-PRI @ <> if E-NMIGRATE-CALLEE throw then
   NDICT:OPEN-PUB CALLEE-PUB @ <> if E-NMIGRATE-CALLEE throw then ;

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER M-CTX IR-CTX:ctx
1 TYPED-BUFFER M-BLD IR-BUILD:builder
1 TYPED-BUFFER M-TAPE IR-ARENA:view

\ Everything the run needs, parked: a quotation cannot read the enclosing word's
\ locals and the whole run is one quotation.
PTR-VARIABLE M-SRC
variable M-SRC-U
PTR-VARIABLE M-DATA
variable M-DATA-U
variable M-IN
variable M-OUT
variable M-REGS
variable M-OPEN                      \ a migration is running
variable M-RC                        \ the code the run inside the context reached
variable M-VERDICT                   \ the verdict the recorded scan reached
variable M-SPILLS                    \ frame slots this definition proved it needs

\ ---- compiling without publishing --------------------------------------------
\ A DEFAULT migration lets the engine publish the definition and then points the
\ published record at the chain's code. That makes the chain a second pass over
\ a word that already exists, and it is why the old emitter cannot be removed:
\ every definition must succeed through it first.
\
\ A HELD migration asks the engine to certify the definition and publish
\ NOTHING. The checker still runs, the tape is still filled by its reader, and
\ the record `:` built is still there - but the count does not move, the name
\ never enters the index, and the emission's code space is given straight back.
\ The chain then compiles the tape and its own publisher commits that record
\ (src/compiler/native/publish.f COMMIT-HELD). Nothing is reachable under the
\ name until code the validator accepted stands behind it.
\
\ M-HELD-PENDING IS THE OBLIGATION, not a copy of the mode. It is raised once the
\ engine has really held a record for this run and lowered when that record is
\ committed, so the failure path can tell "a definition is being withheld and
\ nobody will ever publish it" from "nothing was held". What it owes on that path
\ is the checker's side of the retraction: the engine gave the code space back by
\ itself, and the count never moved, but the certified signature the checker
\ recorded under this name did not go away with them.
\
\ AND A MEASURED MIGRATION IS A HELD ONE THAT NEVER COMMITS. It asks the whole
\ chain the same question - the engine certifies, the reader fills the tape, the
\ dialect elaborates it, the allocator accepts it, the emitter seals it, and the
\ publication seam makes every refusal it can make - and then keeps none of the
\ answer: no code, no record, no clobber row, no replacement-log row and no
\ count. The held record is retracted on the way out exactly as a refused run
\ retracts it, because a measured run leaves the same thing behind: a definition
\ that certified and was never published.
\
\ WHY THAT IS A MODE AND NOT A CALLER'S OWN AFFAIR. What a publication keeps is
\ kept in two records that may not drop a row to make space - a row is the whole
\ of what a caller compiled against it - so a caller that asks the question a few
\ thousand times fills them, and the refusal it then gets says the chain ran out
\ of table rather than that the definition is one it cannot compile. That is a
\ measurement of the instrument. tools/chain-census-core.f is the caller that
\ asks, and it read the first of those tables as the size of the compilable tree.
variable M-HELD                      \ this migration compiles without publishing
variable M-HELD-PENDING              \ a held record is waiting to be committed or retracted
variable M-MEASURE                   \ this migration proves the publication instead of making it

: CC ( -- IR-CTX:ctx )           0 M-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 M-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 M-TAPE @ ;
: MKEY ( -- IR-ID:ir-module-key ) BB IR-BUILD:MODULE-KEY ;

: SRC$ ( -- ptr u8 n )
   M-SRC @ M-SRC-U @ ;

: DATA$ ( -- ptr u8 n )
   M-DATA @ M-DATA-U @ ;


\ ---- the module the definition is compiled into ------------------------------
: HIR-MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

\ The dialect's source-word model: which Habu word means which operation. A
\ definition that mentions a `create`d data word needs one row more, because
\ which data words a program names is the program's and not the dialect's. What
\ that row holds is the engine's answer and not the caller's: the caller names
\ the word, the word model asks the dictionary.
: EXTRA-ROWS ( -- n )
   CALLEE-N @
   M-DATA-U @ 0<> if 1+ then ;

\ How many rows the table is committed to, and why that number is read off the
\ TAPE rather than chosen. The dialect's own words are a fixed count and the
\ staged extras are counted above, but the elaborator now adds a row for every
\ name the body writes that the dialect does not model and the engine does
\ (src/compiler/native/elaborate.f RESOLVE-SCAN). Which names those are is not
\ known until the body has been read, and the thing that read it is the tape - so
\ the ceiling is the tape's own token count, which is the most distinct names a
\ body can possibly write. That makes the table as large as the program needs and
\ no larger, with no number for anyone to pick: a body that names nothing outside
\ the dialect never fills the headroom, and one that names a hundred words cannot
\ run out of it.
: MODEL-ROWS ( -- n )
   HIR-WORD:WORDS EXTRA-ROWS + TAPE NTAPE:TOKENS + ;

: DECLARE-DATA ( IR-ARENA:arena -- ) {: r:IR-ARENA:arena :}
   M-DATA-U @ 0= if exit then
   CC BB r  CC BB DATA$ IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED ;

\ Every word this definition calls, as the word model's own rows. The list is the
\ caller's, staged before the migration; this only reads it, one row at a time,
\ so a body that calls one word and a body that calls four go down the same path.
: DECLARE-CALLEE1 ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena k:n :}
   CC BB r  CC BB k CALLEE$ IR-BUILD:INTERN-SYMBOL
   k cells CALLEE-ADDR + @
   k cells CALLEE-IN + @
   k cells CALLEE-OUT + @  HIR-WORD:DECLARE-CALLABLE ;

: DECLARE-CALLEE ( IR-ARENA:arena -- ) {: r:IR-ARENA:arena :}
   CALLEE-N @ 0 ?do  r i DECLARE-CALLEE1  loop ;

: MODEL ( -- IR-ARENA:arena IR-ARENA:arena )
   CC BB IR-BUILD:MODULE-KEY MODEL-ROWS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB p r HIR-WORD:REGISTER-WORDS
   r DECLARE-DATA
   r DECLARE-CALLEE
   p r ;

\ ---- stage N0: the definition the engine compiles ----------------------------
\ The engine compiles the definition and the unit answers the sealed tape and the
\ verdict the scan reached. Both are parked rather than left on the stack,
\ because this runs inside the quotation the recovery below catches.
: SCAN ( -- )
   SRC$ EV
   NFEED:END-UNIT M-VERDICT !  0 M-TAPE ! ;

\ Open a unit, run the scan, and close it. Anything that fails between the two -
\ the engine refusing the source, the checker refusing the definition, a source
\ that opened no scan at all or a second one - leaves the producer holding a
\ half-recorded unit, and the ONE route out of that is to give it up. So the
\ failure is caught here only to release the recorder, and is rethrown with its
\ own code: without this every later migration in the process would be refused
\ for the state this one left behind rather than for anything about itself.
\ The hold is opened around the scan and closed on every path out of it. Leaving
\ it armed would withhold the NEXT definition the process compiles - one this
\ migration knows nothing about and no chain is waiting for - so the close is
\ paired with the open rather than left to the caller.
: HOLD-OPEN ( -- )
   M-HELD @ 0= if exit then
   CHECKER-TAPE:HOLD-ARM ;

: HOLD-CLOSE ( -- )
   M-HELD @ 0= if exit then
   CHECKER-TAPE:HOLD-DISARM ;

: RECORD ( -- n )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   HOLD-OPEN
   ndict@ {: before:n :}
   [: SCAN ;] catch {: rc:n :}
   HOLD-CLOSE
   rc 0 <> if NFEED:ABANDON-UNIT rc throw then
   M-VERDICT @ -1 <> if E-NMIGRATE-VERDICT throw then
   before ;

\ How many bytes the reader handed over, as the registry recorded them. The
\ source is named off the tape's own first span, so the length asked for is the
\ length of the text the recorded rows span into, read off the live builder
\ because selection takes its binding before the module freezes.
: TEXT-LEN ( -- n )
   CC BB  TAPE MKEY 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC
   IR-BUILD:SOURCE-LEN ;

\ ---- which word the source published -----------------------------------------
\ Exactly one record, and the name is that record's. A source that published
\ none or several is refused rather than having its newest record migrated.
: PUBLISHED-ONE ( n -- ) {: before:n :}
   ndict@ before 1+ <> if E-NMIGRATE-NAME throw then ;

\ The held migration's version of the same question, and it is the OPPOSITE
\ assertion. A source that published anything under a hold either defined
\ something the hold does not cover - a `create`, a `constant`, a second
\ definition - or the hold did not take, and in both cases the record this
\ migration is about is not the one the count points at. So the count must not
\ have moved at all.
: PUBLISHED-NONE ( n -- ) {: before:n :}
   ndict@ before <> if E-NMIGRATE-NAME throw then ;

: SOURCE-PUBLICATION-CK ( n -- ) {: before:n :}
   M-HELD @ 0<> if before PUBLISHED-NONE exit then
   before PUBLISHED-ONE ;

\ Which record this migration is about. A published one is the newest; a held one
\ is the unpublished slot the count still points at, which is exactly the slot
\ src/compiler/native/publish.f will commit.
: REC-INDEX ( -- n )
   M-HELD @ 0<> if ndict@ exit then
   ndict@ 1- ;

: LATEST-NAME$ ( -- ptr u8 n )
   REC-INDEX XREF-REC XREF-NAME$ ;

\ Which wordlist the definition landed in. A word is a tail in a wordlist, and
\ where a definition lands is decided by the package scope open when the source
\ is evaluated, so the wordlist is read off the record rather than assumed.
: LATEST-WID ( -- n )
   REC-INDEX XREF-REC XREF-WORDLIST ;

\ The record's name is a span of the dictionary the next definition may move, so
\ the migration keeps its own copy of the name it published.
: KEEP-NAME ( -- )
   LATEST-NAME$ {: a:ptr u:n :}
   u NAME-CAP > if E-NMIGRATE-TEXT throw then
   a NAME-BUF u STR-LEN BYTE-COPY-LEN
   u NAME-U ! ;

\ In its own wordlist, the tail has to resolve to the record the evaluation just
\ made. If an earlier record of the same tail wins that lookup, the republication
\ would rewrite the wrong one, so this is refused instead.
: RESOLVES-TO-LATEST ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   a u wid XREF-FIND-WL-INDEX ndict@ 1- <> if E-NMIGRATE-NAME throw then ;

\ ---- recording this definition's body for its callers ------------------------
\ WHY A MIGRATION RECORDS ANYTHING. A definition small enough that copying its
\ body into a call site costs no more instructions than the call did is one every
\ later caller should copy rather than call, and the only moment its body can be
\ kept is while it is being compiled: the tokens live in a module that dies with
\ this run, and their spellings can only be asked of that module's own interner.
\ src/compiler/native/inline.f is the record and carries the argument for why an
\ address is the right key and what the size rule is.
\
\ WHICH BODY QUALIFIES IS NOT DECIDED HERE. Whether a token could stand in a
\ copied body is the ELABORATOR's rule - it is the pass that would have to stage
\ it - and it is asked here, token by token, through NELAB:SPLICEABLE?. This file
\ decides nothing about the body; it copies the tokens out and states the arity
\ the caller declared for the definition.
\
\ AND WHAT IS WRITTEN DOWN FOR A CALL IS WHAT THE ROUTINE HAS, NOT WHAT THE
\ SOURCE SAID. A body may write a call that the elaboration COPIED - the routine
\ published for `: T-GET-N ( ptr a n -- r ) T-AT-N @ ;` contains T-AT-N's
\ operations and no branch at all - and staging the call token for it would key a
\ row full of calls to an address holding straight-line code. So the elaborator
\ is asked which sites it copied (NELAB:COPIED?) and from which row
\ (NELAB:COPIED-ENTRY), and that row is spliced into the staging in place of the
\ call. src/compiler/native/inline.f carries the argument for why a flattened row
\ is the honest one and why the recording still terminates.
\
\ AND THE TOKENS ARE STAGED BEFORE THE ROW EXISTS, WHICH IS WHY THE RECORD HAS
\ THREE STEPS. The spellings have to be read while the module is still being
\ built. The address the routine will occupy is settled once the emission has
\ been placed against a slot, but it is not the definition's yet, because a
\ refusal is still possible between the placement and the publication - and a row
\ keyed to a slot no publication claimed would be a body waiting for whatever
\ word is published there next. So the staging is CLAIMED while a refusal still
\ costs nothing, which is where every refusal the record can make is asked, and
\ committed only after the seam has written the routine at the address the claim
\ named. A run that never got that far throws its staging away in RUN.
64 constant SPELL-CAP                \ the longest spelling one staged token may have

create SPELL-BUF SPELL-CAP allot

here CELL 1- and CELL swap - CELL 1- and allot
variable REC-OK                      \ the body staged so far is still one worth keeping

\ A call the elaboration copied, staged as the row it was copied FROM. The row is
\ already flat - no row holds a call - so this adds operations and never another
\ call, and one splice is the whole of it. A row that will not fit beside what is
\ staged already ends the recording, exactly as an over-long spelling does: this
\ definition is one its callers will call, and calling it is correct.
: REC-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix NELAB:COPIED-ENTRY {: entry:n :}
   entry NINL:TOKENS NINL:STAGE-FITS? 0= if 0 REC-OK ! exit then
   entry NINL:STAGE-RECORD ;

\ One body token, copied into the staging area. A token the elaborator could not
\ stage inside a copied body, one whose spelling is longer than a record holds,
\ and one that no longer fits in a row, all end the recording: this definition is
\ one its callers will call.
\
\ THE COPIED CALL IS ASKED ABOUT FIRST, because SPLICEABLE? answers about the
\ token as written and every call is written as a call. Asking it first is what
\ makes the two questions one order rather than two overlapping rules.
: REC-TOKEN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   REC-OK @ 0= if exit then
   ix NELAB:COPIED? if r ix REC-CALL exit then
   1 NINL:STAGE-FITS? 0= if 0 REC-OK ! exit then
   TAPE r ix  CC BB  TAPE MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM
   NELAB:SPLICEABLE? 0= if 0 REC-OK ! exit then
   TAPE ix NTAPE:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if
      TAPE ix NTAPE:LIT@ NINL:STAGE-INT exit
   then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if
      TAPE ix NTAPE:LIT@ NINL:STAGE-REAL exit
   then
   CC BB  TAPE MKEY ix NTAPE:SPELL@  SPELL-BUF SPELL-CAP IR-BUILD:SYMBOL-COPY
   {: u:n :}
   u NINL:SPELL-FITS? 0= if 0 REC-OK ! exit then
   SPELL-BUF u NINL:STAGE-NAME ;

\ The whole body, or nothing. It runs while the module is still being built,
\ which is why it stands between the elaboration and the emission.
\
\ THE CAPACITY IS NOT ASKED ABOUT THE SOURCE HERE ANY MORE, because a source
\ token is no longer one staged token: a call the elaboration copied stages a
\ whole row. A count taken before the walk could only be a guess in both
\ directions, so the ceiling is asked at each step instead, by the step that
\ knows what it is about to add.
: STAGE-BODY ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   TAPE NTAPE:TOKENS {: n:n :}
   M-IN @ M-OUT @ NINL:STAGE-BEGIN
   1 REC-OK !
   n 1 ?do
      r i REC-TOKEN
   loop
   REC-OK @ 0= if NINL:STAGE-CLEAR then ;

\ The size rule, asked of the emission the validator accepted. It is the last
\ thing that can disqualify a body, and it is asked here because this is the
\ first moment the definition's own instruction count exists.
\
\ WHAT IT IS ASKED ABOUT IS THE BODY AND NOT THE WHOLE EMISSION, because the body
\ is what a caller copying this routine would write - the emitter measured it
\ while it wrote it, and src/compiler/native/inline.f carries the argument for
\ why that measurement and not an arity-derived subtraction. A routine that calls
\ has no answer there, and none is needed: a body with a call in it never reaches
\ this word, because every token of it had to be one the elaborator could splice
\ and a call is not.
: SIZE-CK ( -- )
   NINL:STAGED? 0= if exit then
   A64EMIT:LEAVES-BY-BRANCH? if NINL:STAGE-CLEAR exit then
   M-IN @ M-OUT @ A64EMIT:BODY-INSNS NINL:SMALL? 0= if NINL:STAGE-CLEAR then ;

\ Claim the row for the staged body: the address the routine is about to be
\ published at. The emitter's own recorded placement is that address before the
\ publication as much as after it - the seam refuses to publish an emission whose
\ placement is not the slot it is claiming, so a publication that returns wrote
\ the routine at exactly the address this asked about.
\
\ IT IS ASKED HERE BECAUSE THIS IS THE LAST MOMENT A REFUSAL IS FREE. Everything
\ the record can refuse - an address that is not one, an address that already has
\ a row - is refused with the word still running the code the engine compiled for
\ it, which is what every other refusal in this chain leaves behind. And a record
\ with no room for another body refuses nothing at all: it declines the row, the
\ word publishes, and its callers call it, exactly as they call a body the size
\ rule turned down.
\
\ AND A MEASURED RUN CLAIMS NOTHING, because the address it would claim is one no
\ routine is going to occupy: the run publishes nothing, the code pointer never
\ moves, and the next definition compiled in this process starts exactly there. A
\ row claimed for it would be a body kept against somebody else's code - and the
\ record says so itself, by refusing the second measurement's claim at the same
\ address (E-NINL-DUP). The staging is left for RUN to drop, which is the same
\ path a refused run's staging leaves by.
: CLAIM-ROW ( -- )
   NINL:STAGED? 0= if exit then
   M-MEASURE @ 0<> if exit then
   A64EMIT:PLACEMENT NINL:CLAIM ;

\ Write the row the claim reserved, now that the seam has published the routine
\ at the address it was claimed for. A staging that was declined a row left no
\ claim behind, so this is the same question as "is there still a body to keep".
: KEEP-BODY ( -- )
   NINL:CLAIMED? 0= if exit then
   NINL:COMMIT ;

\ ---- the chain ---------------------------------------------------------------
\ Select, allocate, have the allocation accepted and emit, under the convention a
\ Habu word is entered through. The contract is built more than once from the
\ same four numbers because a routine value cannot be held in a local; every one
\ of them is the same declaration, so the selector, the allocator and the
\ validator are answering about one routine.
\
\ WHETHER THIS ROUTINE CALLS IS THE ELABORATOR'S ANSWER AND NOT THE CALLER'S. It
\ was the caller's while every call written in a body became a call in the
\ module, and it stopped being so when a small callee started being copied into
\ its caller instead: a definition can now be written with four calls in it and
\ contain none. The module is what the contract has to describe - the selector
\ holds the two against each other and refuses a frame reserved for a call that
\ is not there - so the contract is read off the pass that built the module.
\
\ THERE IS NO LONGER A SECOND DIRECTION TO KEEP. A check used to refuse a
\ migration whose body called when the caller had not entered it as one that
\ calls. That was never a fact about the definition, only a test of whether the
\ caller had predicted the walk; and now that a body's names resolve off the
\ dictionary, any body may turn out to call and no caller can predict it. It went
\ with the flag it read.
\
\ AND HOW MUCH FRAME IT TAKES, which is the one thing stated here that is nobody's
\ declaration. How many values a definition cannot keep in its registers is
\ decided by the allocator and by nothing else, so M-SPILLS starts at zero and is
\ filled in by EMITTED below out of what the walk proved. Zero is what the four
\ unframed forms declared before this file counted anything, so a definition whose
\ values all fit is compiled under exactly the contract it always had.
: ROUTINE ( -- A64EFF:routine )
   NELAB:TAIL-CALLED?  NELAB:TAIL-ENTRY@ NPUB:IN-REGION?  and if
      NELAB:CALLS-BACK? if
         0 M-REGS @ M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-CALLING-FRAMED exit
      then
      0 M-REGS @ M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-FRAMED exit
   then
   NELAB:CALLED? if
      0 M-REGS @ M-IN @ M-OUT @ M-SPILLS @ NABI:CALL-FRAMED exit
   then
   0 M-REGS @ M-IN @ M-OUT @ M-SPILLS @ NABI:LEAF-FRAMED ;

: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

\ The recorded length is read off the LIVE builder, before the freeze consumes
\ the handle, because selection is handed the text the unit kept and refuses it
\ unless it digests to the source the module was compiled from.
\
\ THE LOWERING PASS IS BOUND HERE TOO, because a module's symbols are its own
\ ordinals and this is the only moment the machine dialect can be asked them: the
\ module a rewrite would read is the one selection is about to write. A migration
\ whose walk decides no spill gives that binding straight back, which is what the
\ RELEASE in EMITTED below is.
: SELECTED ( n -- IR-BUILD:module )
   {: len:n :}
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC ab A64RAV:BIND-DIALECT
   CC ab A64EMIT:BIND-DIALECT
   CC ab A64SPILL:BIND-DIALECT
   CC ab A64COMB:BIND-DIALECT
   CC m ab TXT len ROUTINE A64SEL:SELECT ;

\ The module in which a multiply and the addition that reads its product are one
\ multiply-add. It stands between selection and allocation because that is where
\ the pattern is visible: the two operations are values here and registers
\ afterwards, and the allocator's own reuse of a register is what makes the same
\ pair unfusable once it has run.
\
\ A MODULE WITH NO SUCH PAIR IS HANDED BACK UNTOUCHED, which is not an
\ optimisation of this pass but the thing that keeps every other routine's bytes
\ what they were. Rebuilding a module renumbers its values, and the allocator
\ breaks ties on those numbers, so a routine that gained nothing could still come
\ out holding different registers and therefore different bytes. The scan is
\ asked first and the binding given straight back when it answers zero, exactly
\ as EMITTED below gives the spill binding back for a routine that spills
\ nothing.
: COMBINED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   m A64COMB:FUSIONS {: n:n :}
   n 0= if A64COMB:RELEASE m exit then
   A64RA:RELEASE
   A64EMIT:RELEASE
   A64SPILL:RELEASE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC nb A64EMIT:BIND-DIALECT
   CC nb A64SPILL:BIND-DIALECT
   CC m nb TXT len A64COMB:REWRITE {: m1:IR-BUILD:module :}
   A64COMB:FUSED n <> if E-A64COMB-SHAPE throw then
   m IR-BUILD:RETIRE
   m1 ;

\ The emission is made against the slot the publication seam is about to claim,
\ which is what a branch to another word is measured from. It is declared for
\ EVERY migration and not only for one that calls: a migration always publishes
\ through that seam at that slot, so stating it once here is one rule rather than
\ a condition, and the seam holds it against the slot it really claims - which
\ turns "nothing moved the code pointer between the emission and the
\ publication" from an assumption into a refusal.
: EMIT-AT ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m ROUTINE A64RAV:ACCEPT
   NPUB:NEXT-SLOT A64EMIT:PLACE-AT
   CC m A64EMIT:EMIT ;

\ The module in which the sealed spill decisions are real stores and loads. The
\ emitter's binding is given back first because it was taken over the builder
\ selection wrote into, and everything from here on reads the builder this makes.
\ The reserve the rewrite emits is sized from A64RA:FRAME - the frame the walk
\ just proved this routine needs - which is the same number ROUTINE declares from
\ the same count, so the module and its contract agree by construction rather
\ than by the caller getting a guess right.
: LOWERED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   A64EMIT:RELEASE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC nb A64EMIT:BIND-DIALECT
   CC m nb TXT len A64SPILL:REWRITE ;

\ ---- the two stages, or the four ---------------------------------------------
\ A definition whose values all fit its registers is selected, allocated,
\ accepted and emitted - one walk, exactly as before this file knew what a spill
\ was.
\
\ ONE WHOSE VALUES DO NOT FIT IS NO LONGER REFUSED. The allocator does not hold a
\ walk to the frame its contract arrived with any more; it hands out the slots the
\ program needs and answers how many (habu-derive-a-routine-84ed36b6). So the
\ count is read off the walk, the contract above declares exactly it, and two
\ stages go in between: build the module those decisions are real operations in,
\ and allocate THAT - because the decisions the first walk sealed are not an
\ assignment for the module it read. Skipping either does not quietly emit the
\ wrong program; the validator refuses it.
\
\ WHAT THE SECOND WALK DECIDES IS NOTHING, because it reads a module whose
\ operations already are the ones the first walk assumed. It is still run, because
\ the claims the emitter reads have to be claims about the module being emitted.
\
\ A ROUTINE THAT CALLS STILL CANNOT SPILL, and it is refused rather than
\ mis-emitted. Its frame is built by the SELECTOR - src/compiler/native/select.f
\ PROLOGUE emits the reserve and the link save, sized from the contract selection
\ was handed - and selection has already happened by the time the count is known.
\ The lowering pass keeps that prologue rather than resizing it
\ (src/compiler/native/spill.f ONCE-CK), so the module reserves the frame a
\ spill-free routine needed while its contract declares the one the spills need,
\ and src/compiler/native/regalloc-verify.f refuses the difference by name. Making
\ a calling routine spill starts at the selector and is dot
\ habu-exercise-a-call-dda45093.
: EMITTED ( -- )
   TEXT-LEN {: len:n :}
   len SELECTED len COMBINED {: m:IR-BUILD:module :}
   CC m ROUTINE A64RA:ALLOCATE
   A64RA:SPILLS M-SPILLS !
   M-SPILLS @ 0= if
      A64SPILL:RELEASE
      m EMIT-AT
      exit
   then
   m len LOWERED {: m1:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   CC m1 ROUTINE A64RA:ALLOCATE
   m1 EMIT-AT ;

\ ---- one migration -----------------------------------------------------------
\ A held record is not in the name index, so asking whether its tail resolves to
\ it would ask whether an unpublished record can be found - which it cannot, by
\ construction. The question that check really answers, "the record about to be
\ rewritten is the one this source made", is answered for a held migration by the
\ publisher instead: src/compiler/native/publish.f HELD-CK refuses any index but
\ the one slot the engine can have withheld.
: RESOLUTION-CK ( n -- ) {: wid:n :}
   M-HELD @ 0<> if exit then
   LATEST-NAME$ wid RESOLVES-TO-LATEST ;

\ From here until the commit, a record exists that nothing will ever publish
\ unless this run finishes. RUN's failure path is what settles it.
: HELD-TAKEN ( -- )
   M-HELD @ 0= if exit then
   1 M-HELD-PENDING ! ;

: PUBLISH-IT ( n -- ) {: wid:n :}
   M-HELD @ 0<> if
      M-MEASURE @ 0<> if NPUB:VALIDATE-HELD exit then
      NPUB:COMMIT-HELD
      0 M-HELD-PENDING !
      exit
   then
   NAME-BUF NAME-U @ wid NPUB:REPUBLISH ;

\ THE MODEL IS BUILT AFTER THE TAPE AND NOT BEFORE IT. It used to be built first,
\ because everything in it was known before the body was read: the dialect's own
\ words, and whatever the caller had staged. Neither of those is the whole table
\ any more - the elaborator adds a row for each name the body writes that the
\ engine can answer for - so the table has to be sized from the body, and the
\ body is the tape. Nothing between the two ever needed the model: the engine's
\ own compilation of the source is what RECORD runs, and it knows nothing about
\ this chain's vocabulary.
: WORK ( -- )
   CC HIR-MOD 0 M-BLD !
   RECORD {: before:n :}
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   before SOURCE-PUBLICATION-CK
   LATEST-WID {: wid:n :}
   wid RESOLUTION-CK
   KEEP-NAME
   wid NAME-WID !
   HELD-TAKEN
   CC BB TAPE p r M-IN @ M-OUT @ NELAB:COLON drop
   r STAGE-BODY
   EMITTED
   SIZE-CK
   CLAIM-ROW
   wid PUBLISH-IT
   KEEP-BODY ;

\ The failure is caught INSIDE the context and carried out as a code, so the
\ context always leaves the ordinary way and gives its arenas back. A throw that
\ unwound past it would strand every arena the run had built, and the arena
\ registry - which is small and shared - would run out after a handful of refused
\ words. Nothing is decided here: the code is rethrown unchanged by RUN.
\ A refused run gives its bindings back, for the same reason it gives its arenas
\ back. Each of the chain's passes takes an identity binding over the module it
\ is about to read, and two of them refuse a second binding over a live one - so
\ a binding a refusal left behind would make every LATER migration fail for the
\ state this one left rather than for anything about itself, which is exactly
\ what RUN below refuses to let happen with the recorder.
\
\ EACH PASS IS ASKED ABOUT ITSELF, so this cannot get out of step with how far
\ the run got. Selection spends its binding first thing, allocation spends its
\ own, emission spends its own - so which are still live depends on which stage
\ refused, and no counter kept here could track that without being a second copy
\ of state the passes already hold. The validator is left out because its own
\ binding is documented as replaceable: a second one over a live one is not a
\ refusal there.
: RETURN-BINDINGS ( -- )
   A64SEL:BOUND? if A64SEL:RELEASE then
   A64RA:BOUND? if A64RA:RELEASE then
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   A64COMB:BOUND? if A64COMB:RELEASE then
   A64EMIT:BOUND? if A64EMIT:RELEASE then ;

: BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 M-CTX !
   [: WORK ;] catch M-RC !
   M-RC @ 0 <> if RETURN-BINDINGS then ;

\ The whole run, once. A migration inside a migration would record one
\ definition's tokens onto the other's tape, so the second is refused by name.
\
\ The failure is caught only to put the entry back where the next migration can
\ open, and is rethrown with its own code: a refused word must not make every
\ later migration refuse too, and it must not turn some stage's refusal into
\ this file's.
\ The run inside its own context, which gives the arenas the stages built back
\ when it leaves.
: IN-CONTEXT ( -- )
   NABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- what a refused HELD migration owes --------------------------------------
\ A refused held run leaves less behind than a refused ordinary one, because the
\ engine already took back everything it owns: the count never moved, the name
\ never entered the index, and the code space went back to the colon entry the
\ moment the hold was taken. The record at that slot is inert - the next
\ definition the engine compiles writes over it - so there is nothing to undo
\ there either.
\
\ WHAT IS LEFT IS THE CHECKER'S. The definition CERTIFIED; that is what made it
\ holdable. So the checker recorded a signature under the name, and unlike the
\ count and the code that signature has no owner that gives it back. Left in
\ place it is a certified effect for a word that does not exist: the next
\ definition of that name meets the certified-duplicate guard and is refused for
\ the state this run left rather than for anything about itself - the same
\ failure mode RECORD gives the recorder up to avoid.
\
\ THE NAME IS THE TAIL THE RECORD CARRIES, which is the checker's symbol for a
\ definition made at top level, where migrations run. A held migration inside an
\ open package would need the qualified spelling, and that arrives with the
\ package-scoped migration (dot habu-parse-a-migrated-b38a83d9); until then a
\ held run in a package would under-retract, so the fixture pins the global case
\ this file actually supports.
: HELD-RETRACT ( -- )
   M-HELD-PENDING @ 0= if exit then
   0 M-HELD-PENDING !
   NAME-BUF NAME-U @ CHECKER-USIGS-TRUNCATE-FROM-RAW ;

: RUN ( -- )
   M-OPEN @ 0<> if E-NMIGRATE-STATE throw then
   M-SRC-U @ TEXT-CAP > if E-NMIGRATE-TEXT throw then
   CALLEES-SCOPE-CK
   1 M-OPEN !
   0 M-RC !
   IN-CONTEXT
   0 M-OPEN !
   CALLEES-CLEAR
   NINL:STAGED? if NINL:STAGE-CLEAR then
   M-RC @ {: rc:n :}
   rc 0 <> if HELD-RETRACT rc throw then
   M-MEASURE @ 0<> if HELD-RETRACT then ;

: STAGE ( ptr u8 n n n n -- )
   {: sa su:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa keeps the ptr u8 byte-span role
   sa M-SRC ! su M-SRC-U !
   in M-IN ! out M-OUT ! regs M-REGS !
   0 M-DATA-U ! 0 M-SPILLS !
   0 M-HELD ! 0 M-HELD-PENDING ! 0 M-MEASURE ! ;

public

\ Compile the definition this source publishes, and republish it as the native
\ chain's code. `in` and `out` are its declared arities and `regs` the scratch
\ registers its routine may use.
: DEFINE ( ptr u8 n n n n -- )
   CALLEES-NONE-CK
   STAGE RUN ;

\ Compile the definition this source publishes WITHOUT publishing it: the engine
\ certifies it and withholds the record, the chain compiles the tape, and this
\ file's publisher commits that record. A refusal anywhere leaves the definition
\ uncompiled and unpublished, with the refusing stage's own error - the chain's
\ vocabulary refusal is still E-HIR-UNMODELED, its control refusal still
\ E-NELAB-CTRL - and nothing under the name at all, because there never was
\ anything under the name.
\
\ THIS IS THE ENTRY THE CUT NEEDS. Every other entry in this file compiles a word
\ the old emitter already published; this one is the first that does not, which
\ is what makes the old emitter's emission unnecessary rather than prerequisite.
: DEFINE-HELD ( ptr u8 n n n n -- )
   CALLEES-NONE-CK
   STAGE
   1 M-HELD !
   RUN ;

\ Ask whether the chain can compile the definition this source publishes, and
\ keep nothing whatever the answer is. It runs every stage DEFINE-HELD runs,
\ including every refusal the publication seam can make
\ (src/compiler/native/publish.f VALIDATE-HELD), and stops one step short of the
\ writes: no code in the arena, no dictionary record, no row in either
\ address-keyed record, no replacement-log row, and the checker's signature for
\ the name retracted on the way out. A refusal arrives exactly as it would have
\ from DEFINE-HELD, with the refusing stage's own code.
\
\ THIS IS WHAT A CENSUS NEEDS AND THE OTHER ENTRIES CANNOT GIVE IT. Every other
\ entry here publishes, and a publication is permanent: the two address-keyed
\ records may not drop a row to make space, so a caller that asks about a few
\ thousand definitions fills them and is refused for the tables rather than for
\ the definitions. What the caller wanted to know is answered before any of that
\ is written down.
: MEASURE-HELD ( ptr u8 n n n n -- )
   CALLEES-NONE-CK
   STAGE
   1 M-HELD !
   1 M-MEASURE !
   RUN ;

\ Stage one word the NEXT migration's definition calls: its spelling as the
\ definition writes it, the address its code starts at, and the effect it
\ declares. The spelling is COPIED, because it is a span of the caller's memory
\ and the migration reads it later, inside the recorded run.
\
\ THE SPELLING AND THE ADDRESS ARE HELD AGAINST EACH OTHER BEFORE EITHER IS
\ KEPT, so a staging that contradicts itself leaves no row behind for the next
\ migration to be refused for. The two capacity refusals come first because they
\ are about this list rather than about the callee, and the scope the list
\ belongs to is taken from the first row and held against every row after it.
: CALLEE ( ptr u8 n n n n -- )
   {: ca cu:n entry:n cin:n cout:n :} \ typed-local-lint: allow-bare-local - ca keeps the ptr u8 byte-span role
   CALLEE-N @ {: k:n :}
   k CALLEES-MAX >= if E-NMIGRATE-STATE throw then
   cu NAME-CAP > if E-NMIGRATE-TEXT throw then
   k 0= if CALLEES-SCOPE! else CALLEES-SCOPE-CK then
   ca cu entry RESOLVES-TO-ENTRY
   ca  CALLEE-BUF k NAME-CAP * +  cu STR-LEN BYTE-COPY-LEN
   cu k cells CALLEE-U + !
   entry k cells CALLEE-ADDR + !
   cin k cells CALLEE-IN + !
   cout k cells CALLEE-OUT + !
   k 1+ CALLEE-N ! ;

\ Migrate a definition that CALLS the words staged above it. A migration with no
\ callee staged is refused: it would be DEFINE.
\
\ NOTHING IS DECLARED ABOUT THE FRAME HERE ANY MORE. This entry used to raise a
\ flag saying "this definition calls", and a migration whose body called without
\ it was refused. The flag was never the fact: ROUTINE picks the frame from
\ NELAB:CALLED?, which is what the WALK found, and the refusal only checked that
\ the caller had predicted the walk correctly. Once a body's names resolve off
\ the dictionary a caller cannot predict it at all - any body may turn out to
\ call - and the prediction was redundant the whole time, because the frame was
\ already derived from the walk that knows.
: DEFINE-CALLING ( ptr u8 n n n n -- )
   {: sa su:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa keeps the ptr u8 byte-span role
   CALLEE-N @ 0= if E-NMIGRATE-STATE throw then
   sa su in out regs STAGE
   RUN ;

\ The same for a definition that names one `create`d data word. Its spelling as
\ the definition writes it is the whole of what the caller says about it: the
\ address that word pushes is the engine's to answer, and the word model asks
\ src/compiler/native/dict.f for it while it declares the row.
: DEFINE-DATA ( ptr u8 n ptr u8 n n n n -- )
   {: sa su:n da du:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa and da keep the ptr u8 byte-span role
   CALLEES-NONE-CK
   sa su in out regs STAGE
   da M-DATA ! du M-DATA-U !
   RUN ;

\ The name of the word the last migration published, and the wordlist it landed
\ in - the pair that names a record.
: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: WID ( -- n )
   NAME-WID @ ;

\ How many values the last migration could not keep in registers, and therefore
\ how many slots its routine's frame holds. Zero for every definition that fits,
\ which is nearly all of them. It is published because it is the only way to tell
\ from outside that a migration took the lowering path at all: the code a spilling
\ definition and a fitting one publish are both just code, and a test that only
\ checked the answers could not tell which route produced them.
: SPILLS ( -- n )
   M-SPILLS @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
