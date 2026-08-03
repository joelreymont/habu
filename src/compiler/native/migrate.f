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
\ AND, FOR A DEFINITION THAT CALLS ANOTHER WORD, WHICH WORD IT IS. Its spelling,
\ the address its code starts at, and its declared effect. All three are already
\ facts of the running engine - the address is the callee's dictionary record and
\ the effect is what the checker accepted for it - and reading them off those two
\ authorities instead of taking them from the caller is dot
\ habu-resolve-a-callee-0340dfde. Until it lands a caller that states them wrongly
\ compiles a routine that computes the wrong thing, which is exactly why the dot
\ exists and why the acceptance suite states them from the same publication the
\ callee was made by.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/abi.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/inline.f
require src/compiler/native/select.f
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
64 constant NAME-CAP                 \ the longest name a migration answers about

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
variable M-DATA-ADDR
variable M-IN
variable M-OUT
variable M-REGS
variable M-CALLS
variable M-OPEN                      \ a migration is running
variable M-RC                        \ the code the run inside the context reached
variable M-VERDICT                   \ the verdict the recorded scan reached

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
\ which data words a program names is the program's and not the dialect's; the
\ address is stated until the chain can ask the engine what a data word is (dot
\ habu-resolve-a-data-a1c8067f).
: EXTRA-ROWS ( -- n )
   CALLEE-N @
   M-DATA-U @ 0<> if 1+ then ;

: MODEL-ROWS ( -- n )
   HIR-WORD:WORDS EXTRA-ROWS + ;

: DECLARE-DATA ( IR-ARENA:arena -- ) {: r:IR-ARENA:arena :}
   M-DATA-U @ 0= if exit then
   CC BB r  CC BB DATA$ IR-BUILD:INTERN-SYMBOL  M-DATA-ADDR @ HIR-WORD:DECLARE-FIXED ;

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
: RECORD ( -- n )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   ndict@ {: before:n :}
   [: SCAN ;] catch {: rc:n :}
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

: LATEST-NAME$ ( -- ptr u8 n )
   ndict@ 1- XREF-REC XREF-NAME$ ;

\ Which wordlist the definition landed in. A word is a tail in a wordlist, and
\ where a definition lands is decided by the package scope open when the source
\ is evaluated, so the wordlist is read off the record rather than assumed.
: LATEST-WID ( -- n )
   ndict@ 1- XREF-REC XREF-WORDLIST ;

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
\ AND THE TOKENS ARE STAGED BEFORE THE ADDRESS IS KNOWN, WHICH IS WHY THE RECORD
\ HAS TWO STEPS. The spellings have to be read while the module is still being
\ built; the address is not the definition's until the publication seam has
\ written the routine there, and between the two a refusal is possible. A row
\ keyed to a slot no publication claimed would be a body waiting for whatever
\ word is published there next, so the staging is committed only after the seam
\ has answered - and thrown away by RUN if the run never got that far.
64 constant SPELL-CAP                \ the longest spelling one staged token may have

create SPELL-BUF SPELL-CAP allot

here CELL 1- and CELL swap - CELL 1- and allot
variable REC-OK                      \ the body staged so far is still one worth keeping

\ One body token, copied into the staging area. A token the elaborator could not
\ stage inside a copied body, and one whose spelling is longer than a record
\ holds, both end the recording: this definition is one its callers will call.
: REC-TOKEN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   REC-OK @ 0= if exit then
   TAPE MKEY r ix NELAB:SPLICEABLE? 0= if 0 REC-OK ! exit then
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
: STAGE-BODY ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   TAPE NTAPE:TOKENS {: n:n :}
   n 1- NINL:FITS? 0= if exit then
   M-IN @ M-OUT @ NINL:STAGE-BEGIN
   1 REC-OK !
   n 1 ?do
      r i REC-TOKEN
   loop
   REC-OK @ 0= if NINL:STAGE-CLEAR then ;

\ The size rule, asked of the emission the validator accepted. It is the last
\ thing that can disqualify a body, and it is asked here because this is the
\ first moment the definition's own instruction count exists.
: SIZE-CK ( -- )
   NINL:STAGED? 0= if exit then
   M-IN @ M-OUT @ A64EMIT:INSNS NINL:SMALL? 0= if NINL:STAGE-CLEAR then ;

: ROOM-CK ( -- )
   NINL:STAGED? 0= if exit then
   NINL:ROOM-CK ;

\ Key the staged body to the address the seam really wrote the routine at. The
\ emitter's own recorded placement is that address: the seam refuses to publish
\ an emission whose placement is not the slot it is claiming, so the two are one
\ answer by the time this runs.
: KEEP-BODY ( -- )
   NINL:STAGED? 0= if exit then
   A64EMIT:PLACEMENT NINL:COMMIT ;

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
\ CALLS-CK below keeps the other direction, which is still the caller's to get
\ wrong: a migration entered as one that calls nothing may not have staged one.
: ROUTINE ( -- A64EFF:routine )
   NELAB:CALLED? if 0 M-REGS @ M-IN @ M-OUT @ NABI:CALL exit then
   0 M-REGS @ M-IN @ M-OUT @ NABI:LEAF ;

: CALLS-CK ( -- )
   M-CALLS @ 0<> if exit then
   NELAB:CALLED? if E-NMIGRATE-STATE throw then ;

: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

\ The recorded length is read off the LIVE builder, before the freeze consumes
\ the handle, because selection is handed the text the unit kept and refuses it
\ unless it digests to the source the module was compiled from.
: SELECTED ( -- IR-BUILD:module )
   TEXT-LEN {: len:n :}
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC ab A64RAV:BIND-DIALECT
   CC ab A64EMIT:BIND-DIALECT
   CC m ab TXT len ROUTINE A64SEL:SELECT ;

\ The emission is made against the slot the publication seam is about to claim,
\ which is what a branch to another word is measured from. It is declared for
\ EVERY migration and not only for one that calls: a migration always publishes
\ through that seam at that slot, so stating it once here is one rule rather than
\ a condition, and the seam holds it against the slot it really claims - which
\ turns "nothing moved the code pointer between the emission and the
\ publication" from an assumption into a refusal.
: EMITTED ( -- )
   SELECTED {: m:IR-BUILD:module :}
   CC m ROUTINE A64RA:ALLOCATE
   m ROUTINE A64RAV:ACCEPT
   NPUB:NEXT-SLOT A64EMIT:PLACE-AT
   CC m A64EMIT:EMIT ;

\ ---- one migration -----------------------------------------------------------
: WORK ( -- )
   CC HIR-MOD 0 M-BLD !
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD {: before:n :}
   before PUBLISHED-ONE
   LATEST-WID {: wid:n :}
   LATEST-NAME$ wid RESOLVES-TO-LATEST
   KEEP-NAME
   wid NAME-WID !
   CC BB TAPE p r M-IN @ M-OUT @ NELAB:COLON drop
   CALLS-CK
   r STAGE-BODY
   EMITTED
   SIZE-CK
   ROOM-CK
   NAME-BUF NAME-U @ wid NPUB:REPUBLISH
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

: RUN ( -- )
   M-OPEN @ 0<> if E-NMIGRATE-STATE throw then
   M-SRC-U @ TEXT-CAP > if E-NMIGRATE-TEXT throw then
   1 M-OPEN !
   0 M-RC !
   IN-CONTEXT
   0 M-OPEN !
   CALLEES-CLEAR
   NINL:STAGED? if NINL:STAGE-CLEAR then
   M-RC @ {: rc:n :}
   rc 0 <> if rc throw then ;

: STAGE ( ptr u8 n n n n -- )
   {: sa su:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa keeps the ptr u8 byte-span role
   sa M-SRC ! su M-SRC-U !
   in M-IN ! out M-OUT ! regs M-REGS !
   0 M-DATA-U ! 0 M-DATA-ADDR ! 0 M-CALLS ! ;

public

\ Compile the definition this source publishes, and republish it as the native
\ chain's code. `in` and `out` are its declared arities and `regs` the scratch
\ registers its routine may use.
: DEFINE ( ptr u8 n n n n -- )
   CALLEES-NONE-CK
   STAGE RUN ;

\ The same for a definition that calls itself.
: DEFINE-CALL ( ptr u8 n n n n -- )
   CALLEES-NONE-CK
   STAGE
   1 M-CALLS !
   RUN ;

\ Stage one word the NEXT migration's definition calls: its spelling as the
\ definition writes it, the address its code starts at, and the effect it
\ declares. The spelling is COPIED, because it is a span of the caller's memory
\ and the migration reads it later, inside the recorded run.
: CALLEE ( ptr u8 n n n n -- )
   {: ca cu:n entry:n cin:n cout:n :} \ typed-local-lint: allow-bare-local - ca keeps the ptr u8 byte-span role
   CALLEE-N @ {: k:n :}
   k CALLEES-MAX >= if E-NMIGRATE-STATE throw then
   cu NAME-CAP > if E-NMIGRATE-TEXT throw then
   ca  CALLEE-BUF k NAME-CAP * +  cu STR-LEN BYTE-COPY-LEN
   cu k cells CALLEE-U + !
   entry k cells CALLEE-ADDR + !
   cin k cells CALLEE-IN + !
   cout k cells CALLEE-OUT + !
   k 1+ CALLEE-N ! ;

\ Migrate a definition that CALLS the words staged above it. The routine's
\ contract declares the direct call for the same reason DEFINE-CALL's does - the
\ first call destroys this routine's caller's return address, so it has to have a
\ frame to keep it in - and it is the SAME declaration, because a routine that
\ calls itself and a routine that calls somebody else pay exactly the same thing
\ for it. A migration with no callee staged is refused: it would be DEFINE.
: DEFINE-CALLING ( ptr u8 n n n n -- )
   {: sa su:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa keeps the ptr u8 byte-span role
   CALLEE-N @ 0= if E-NMIGRATE-STATE throw then
   sa su in out regs STAGE
   1 M-CALLS !
   RUN ;

\ The same for a definition that names one `create`d data word: its spelling as
\ the definition writes it, and the address that word pushes.
: DEFINE-DATA ( ptr u8 n ptr u8 n n n n n -- )
   {: sa su:n da du:n addr:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - sa and da keep the ptr u8 byte-span role
   CALLEES-NONE-CK
   sa su in out regs STAGE
   da M-DATA ! du M-DATA-U ! addr M-DATA-ADDR !
   RUN ;

\ The name of the word the last migration published, and the wordlist it landed
\ in - the pair that names a record.
: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: WID ( -- n )
   NAME-WID @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
