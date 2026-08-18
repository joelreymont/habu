\ migrate.f - the production entry: one checked colon definition, compiled by
\ the engine and then recompiled by the native chain, published as an ordinary
\ word. One concern: driving the whole chain for one definition.
\
\ The caller states the definition's SOURCE. The engine compiles it the ordinary
\ way, and the checker's own reader fills the tape while it consumes the tokens,
\ so the chain elaborates what the checker certified and not a second reading.
\
\ The NAME is not a parameter: it is read off the record the source published,
\ and a source that published none or several is refused. Nothing about a callee
\ is a parameter either - dict.f answers where its code starts and how many
\ cells a call to it moves, at the point those are used.
\
\ Nothing the chain refuses is caught here, so a word the chain cannot compile
\ keeps running the code the engine compiled for it, under the refuser's name.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/inline.f
require src/compiler/native/loop.f
require src/compiler/native/select.f
require src/compiler/native/spill.f
require src/compiler/native/combine.f
require src/compiler/native/emit.f
require src/compiler/native/publish.f

package NMIGRATE

private

\ ---- the one boundary this file needs ---------------------------------------
\ `evaluate` compiles whatever text it is handed, so it may never become a
\ `PRIM:` row: that would hand every checked body the arbitrary-source door.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

\ ---- what a recording unit is opened with ------------------------------------
\ The unit's text ceiling is the ENGINE's own body capture, which overflows with
\ an exit rather than a throw - so a longer source is refused before `evaluate`
\ sees it. The capture is at least three bytes shorter than its source.
BODYBUF-CAP constant TEXT-CAP

\ A name past it is refused rather than truncated into one that denotes another word.
64 constant NAME-CAP

create TXT TEXT-CAP allot
create NAME-BUF NAME-CAP allot
variable NAME-U
variable NAME-WID

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
variable M-OPEN                      \ a migration is running
variable M-RC                        \ the code the run inside the context reached
variable M-VERDICT                   \ the verdict the recorded scan reached
variable M-SPILLS                    \ frame slots this definition proved it needs
variable M-REMATS                    \ values it writes again instead of putting away

\ ---- compiling without publishing --------------------------------------------
\ A HELD migration asks the engine to certify and publish NOTHING; the chain's
\ own publisher commits the record `:` built, so nothing is reachable under the
\ name until code the validator accepted stands behind it. A MEASURED migration
\ is a held one that never commits. M-HELD-PENDING is the obligation to retract.
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

\ Which data words a program names is the program's and not the dialect's, and
\ the row's contents are the engine's answer rather than the caller's.
: EXTRA-ROWS ( -- n )
   M-DATA-U @ 0<> if 1 else 0 then ;

\ Read off the TAPE: the elaborator adds a row per name the body writes that the
\ dialect does not model, and which names those are is not known until it is read.
: MODEL-ROWS ( -- n )
   HIR-WORD:WORDS EXTRA-ROWS + TAPE NTAPE:TOKENS + ;

: DECLARE-DATA ( IR-ARENA:arena -- ) {: r:IR-ARENA:arena :}
   M-DATA-U @ 0= if exit then
   CC BB r  CC BB DATA$ IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED ;

: MODEL ( -- IR-ARENA:arena IR-ARENA:arena )
   CC BB IR-BUILD:MODULE-KEY MODEL-ROWS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB p r HIR-WORD:REGISTER-WORDS
   r DECLARE-DATA
   p r ;

\ ---- stage N0: the definition the engine compiles ----------------------------
\ Parked rather than left on the stack, because this runs inside the quotation
\ the recovery below catches.
: SCAN ( -- )
   SRC$ EV
   NFEED:END-UNIT M-VERDICT !  0 M-TAPE ! ;

\ A failure between open and close leaves the producer holding a half-recorded
\ unit, so it is caught only to release the recorder and rethrown unchanged.
\ The hold is closed on every path: left armed it would withhold the NEXT definition.
: HOLD-OPEN ( -- )
   M-HELD @ 0= if exit then
   CHECKER-TAPE:HOLD-ARM ;

: HOLD-CLOSE ( -- )
   M-HELD @ 0= if exit then
   CHECKER-TAPE:HOLD-DISARM ;

\ Read off the SOURCE: a token costs at least two bytes of capture, so n bytes
\ can never produce more than n/2 rows. A tape is a span of the shared mapping.
: TAPE-ROOM ( -- n )
   M-SRC-U @ 2 / 1 max ;

: RECORD ( -- n )
   CC BB IR-BUILD:MODULE-KEY TAPE-ROOM NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   HOLD-OPEN
   ndict@ {: before:n :}
   [: SCAN ;] catch {: rc:n :}
   HOLD-CLOSE
   rc 0 <> if NFEED:ABANDON-UNIT rc throw then
   M-VERDICT @ -1 <> if E-NMIGRATE-VERDICT throw then
   before ;

\ Read off the live builder because selection takes its binding before the
\ module freezes.
: TEXT-LEN ( -- n )
   CC BB  TAPE MKEY 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC
   IR-BUILD:SOURCE-LEN ;

\ ---- which word the source published -----------------------------------------
\ Exactly one record, and the name is that record's.
: PUBLISHED-ONE ( n -- ) {: before:n :}
   ndict@ before 1+ <> if E-NMIGRATE-NAME throw then ;

\ The opposite assertion: anything published under a hold means the record this
\ migration is about is not the one the count points at, so the count must not move.
: PUBLISHED-NONE ( n -- ) {: before:n :}
   ndict@ before <> if E-NMIGRATE-NAME throw then ;

: SOURCE-PUBLICATION-CK ( n -- ) {: before:n :}
   M-HELD @ 0<> if before PUBLISHED-NONE exit then
   before PUBLISHED-ONE ;

\ A published record is the newest; a held one is the unpublished slot the count
\ still points at, which is exactly the slot publish.f will commit.
: REC-INDEX ( -- n )
   M-HELD @ 0<> if ndict@ exit then
   ndict@ 1- ;

: LATEST-NAME$ ( -- ptr u8 n )
   REC-INDEX XREF-REC XREF-NAME$ ;

\ Where a definition lands is decided by the package scope open when the source
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

\ If an earlier record of the same tail wins the lookup, the republication would
\ rewrite the wrong one.
: RESOLVES-TO-LATEST ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   a u wid XREF-FIND-WL-INDEX ndict@ 1- <> if E-NMIGRATE-NAME throw then ;

\ ---- recording this definition's body for its callers ------------------------
\ Which body qualifies is the ELABORATOR's rule, asked token by token through
\ NELAB:SPLICEABLE?. What is written down is what the ROUTINE has: a call the
\ elaboration COPIED is staged as the row it was copied from, not as a call.
\ Tokens are staged while the module is alive, claimed while a refusal is still
\ free, and committed only after the seam wrote the routine at the claimed address.
64 constant SPELL-CAP                \ the longest spelling one staged token may have

create SPELL-BUF SPELL-CAP allot

here CELL 1- and CELL swap - CELL 1- and allot
variable REC-OK                      \ the body staged so far is still one worth keeping

\ The row is already flat - no row holds a call - so this adds operations only.
: REC-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix NELAB:COPIED-ENTRY {: entry:n :}
   entry NINL:TOKENS NINL:STAGE-FITS? 0= if 0 REC-OK ! exit then
   entry NINL:STAGE-RECORD ;

\ The copied call is asked about FIRST, because SPLICEABLE? answers about the
\ token as written and every call is written as a call.
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

\ Runs while the module is still being built. The ceiling is asked at each step,
\ because a copied call stages a whole row rather than one token.
: STAGE-BODY ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   TAPE NTAPE:TOKENS {: n:n :}
   M-IN @ M-OUT @ NINL:STAGE-BEGIN
   1 REC-OK !
   n 1 ?do
      r i REC-TOKEN
   loop
   REC-OK @ 0= if NINL:STAGE-CLEAR then ;

\ Asked of the BODY and not the whole emission, because the body is what a
\ caller copying this routine would write; the emitter measured it as it wrote.
: SIZE-CK ( -- )
   NINL:STAGED? 0= if exit then
   A64EMIT:LEAVES-BY-BRANCH? if NINL:STAGE-CLEAR exit then
   M-IN @ M-OUT @ A64EMIT:BODY-INSNS NINL:SMALL? 0= if NINL:STAGE-CLEAR then ;

\ The last moment a refusal is free. A record with no room declines the row
\ instead: the word publishes and its callers call it. A measured run claims nothing.
: CLAIM-ROW ( -- )
   NINL:STAGED? 0= if exit then
   M-MEASURE @ 0<> if exit then
   A64EMIT:PLACEMENT NINL:CLAIM ;

\ A staging that was declined a row left no claim, so this is the same question
\ as "is there still a body to keep".
: KEEP-BODY ( -- )
   NINL:CLAIMED? 0= if exit then
   NINL:COMMIT ;

\ ---- the chain ---------------------------------------------------------------
\ Asked of the checker by name, because every OTHER caller in the tree was
\ compiled against that same certificate. A wrong answer cannot publish a wrong
\ routine: either direction is refused by the validator or the memory-order rule.
: NO-RETURN? ( -- bool )
   NAME-BUF NAME-U @ NDICT:SPELL-DEAD? ;

\ The no-return arm comes first: the three below are shapes of a routine that
\ HAS a return, and a body whose every path ends has none.
: ROUTINE ( -- A64EFF:routine )
   NO-RETURN? if
      NELAB:CALLED? if
         NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:NORET-FRAMED exit
      then
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:NORET-LEAF-FRAMED exit
   then
   NELAB:TAIL-CALLED?  NELAB:TAIL-ENTRY@ NPUB:IN-REGION?  and if
      NELAB:CALLS-BACK? if
         NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-CALLING-FRAMED exit
      then
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-FRAMED exit
   then
   NELAB:CALLED? if
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:CALL-FRAMED exit
   then
   NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:LEAF-FRAMED ;

: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

: HIR-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC HIR:NEW-BUILDER ;

\ A module with no such loop is handed back UNTOUCHED: rebuilding renumbers
\ values, so a routine that gained nothing could still come out with other bytes.
: CLOSED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   m NLOOP:FOLDS {: n:n :}
   n 0= if NLOOP:RELEASE m exit then
   A64SEL:RELEASE
   HIR-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64SEL:BIND-SOURCE
   CC m nb TXT len NLOOP:REWRITE {: m1:IR-BUILD:module :}
   NLOOP:FOLDED n <> if E-NLOOP-PLAN throw then
   m IR-BUILD:RETIRE
   m1 ;

\ The recorded length is read off the LIVE builder, before the freeze consumes
\ the handle. The lowering pass is bound here because a module's symbols are its own.
: SELECTED ( n -- IR-BUILD:module )
   {: len:n :}
   CC BB NLOOP:BIND-DIALECT
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m0:IR-BUILD:module :}
   m0 len CLOSED {: m:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC ab A64RAV:BIND-DIALECT
   CC ab A64EMIT:BIND-DIALECT
   CC ab A64SPILL:BIND-DIALECT
   CC ab A64COMB:BIND-DIALECT
   CC m ab TXT len ROUTINE A64SEL:SELECT ;

\ A module with no such pair is handed back UNTOUCHED: rebuilding renumbers
\ values and the allocator breaks ties on those numbers.
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

\ Declared for EVERY migration, not only one that calls, so the seam can hold it
\ against the slot it really claims.
: EMIT-AT ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m ROUTINE A64RAV:ACCEPT
   NPUB:NEXT-SLOT A64EMIT:PLACE-AT
   CC m A64EMIT:EMIT ;

\ The reserve is sized from A64RA:FRAME, the same count ROUTINE declares from,
\ so the module and its contract agree by construction.
: LOWERED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   A64EMIT:RELEASE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC nb A64EMIT:BIND-DIALECT
   CC m nb TXT len A64SPILL:REWRITE ;

\ ---- the two stages, or the four ---------------------------------------------
\ Frame slots and DECISIONS are different counts: a value re-emitted where it is
\ read takes no slot, so a walk asked through the slot count looks like one that
\ decided nothing. A routine that calls still cannot spill; it is refused.
: EMITTED ( -- )
   TEXT-LEN {: len:n :}
   len SELECTED len COMBINED {: m:IR-BUILD:module :}
   CC m ROUTINE A64RA:ALLOCATE
   A64RA:SPILLS M-SPILLS !
   A64RA:REMATS M-REMATS !
   A64RA:PLAN-N 0= if
      A64SPILL:RELEASE
      m EMIT-AT
      exit
   then
   m len LOWERED {: m1:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   CC m1 ROUTINE A64RA:ALLOCATE
   m1 EMIT-AT ;

\ ---- one migration -----------------------------------------------------------
\ A held record is not in the name index, so publish.f HELD-CK answers this
\ instead - it refuses any index but the one slot the engine can have withheld.
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

\ The model is built AFTER the tape, because the table has to be sized from the
\ body and the body is the tape.
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
   NAME-BUF NAME-U @ NDICT:SPELL-GLUE NELAB:FRAME-GLUE!
   CC BB TAPE p r M-IN @ M-OUT @ NELAB:COLON drop
   r STAGE-BODY
   EMITTED
   SIZE-CK
   CLAIM-ROW
   wid PUBLISH-IT
   KEEP-BODY ;

\ Caught INSIDE the context so it always leaves the ordinary way and gives its
\ arenas back. Each pass is asked about ITSELF, so this cannot get out of step.
: RETURN-BINDINGS ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then
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

\ A migration inside a migration would record one definition's tokens onto the
\ other's tape. The failure is rethrown unchanged with its own code.
: IN-CONTEXT ( -- )
   NABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- what a refused HELD migration owes --------------------------------------
\ The engine gives the count and the code space back by itself; the certified
\ signature has no owner that does, so it is retracted here by the record's tail.
: HELD-RETRACT ( -- )
   M-HELD-PENDING @ 0= if exit then
   0 M-HELD-PENDING !
   NAME-BUF NAME-U @ CHECKER-USIGS-TRUNCATE-FROM-RAW ;

\ The length refusal comes first because a source past the engine's body capture
\ ends the process rather than throwing.
: RUN ( -- )
   M-OPEN @ 0<> if E-NMIGRATE-STATE throw then
   M-SRC-U @ TEXT-CAP > if E-NMIGRATE-TEXT throw then
   1 M-OPEN !
   0 M-RC !
   IN-CONTEXT
   0 M-OPEN !
   NINL:STAGED? if NINL:STAGE-CLEAR then
   M-RC @ {: rc:n :}
   rc 0 <> if HELD-RETRACT rc throw then
   M-MEASURE @ 0<> if HELD-RETRACT then ;

: STAGE ( ptr u8 n n n -- )
   {: sa su:n in:n out:n :} \ typed-local-lint: allow-bare-local - sa keeps the ptr u8 byte-span role
   sa M-SRC ! su M-SRC-U !
   in M-IN ! out M-OUT !
   0 M-DATA-U ! 0 M-SPILLS ! 0 M-REMATS !
   0 M-HELD ! 0 M-HELD-PENDING ! 0 M-MEASURE ! ;

public

\ `in` and `out` are the declared arities. The scratch pool is NABI:SCRATCH - the
\ machine's, not a caller's.
: DEFINE ( ptr u8 n n n -- )
   STAGE RUN ;

\ The first entry that compiles a word the old emitter never published, which is
\ what makes the old emitter unnecessary rather than prerequisite.
: DEFINE-HELD ( ptr u8 n n n -- )
   STAGE
   1 M-HELD !
   RUN ;

\ Stops one step short of every write, because a publication is permanent and
\ the two address-keyed records may not drop a row to make space.
: MEASURE-HELD ( ptr u8 n n n -- )
   STAGE
   1 M-HELD !
   1 M-MEASURE !
   RUN ;

\ The spelling is the whole of what the caller says: the address that word
\ pushes is the engine's to answer.
: DEFINE-DATA ( ptr u8 n ptr u8 n n n -- )
   {: sa su:n da du:n in:n out:n :} \ typed-local-lint: allow-bare-local - sa and da keep the ptr u8 byte-span role
   sa su in out STAGE
   da M-DATA ! du M-DATA-U !
   RUN ;

\ The name of the word the last migration published, and the wordlist it landed
\ in - the pair that names a record.
: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: WID ( -- n )
   NAME-WID @ ;

\ Both counts are published because a lowered definition and a fitting one
\ publish code that looks the same, so a test could not tell the routes apart.
: SPILLS ( -- n )
   M-SPILLS @ ;

\ How many values it re-emitted where they are read instead of putting them away.
: REMATS ( -- n )
   M-REMATS @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
