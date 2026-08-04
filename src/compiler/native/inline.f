\ inline.f - the body of a small routine the native chain published, kept against
\ the address its code starts at. One concern: the record a call site copies a
\ callee into itself from.
\
\ WHY THERE IS ANYTHING TO RECORD. A call to a word costs instructions that
\ compute nothing. The site writes each argument into a data-stack slot, branches
\ with link and reads each result back out; the routine it enters moves the
\ pointer down over those arguments, loads each one, stores each result, moves
\ the pointer back up and returns. For a routine that takes one value and leaves
\ one that is ten instructions and two round trips through memory before a single
\ addition happens - and the engine's own emitter does not pay them, because it
\ copies a small callee's body into its caller instead of calling it
\ (src/habu/habu2.f, C-CALL). This file is where the chain's answer to that
\ lives: what a published routine's body IS, between the publication that
\ compiled it and the call site that copies it.
\
\ WHAT A ROW HOLDS, AND WHY IT IS TOKENS AND NOT MACHINE CODE. The row holds the
\ callee's own SOURCE TOKENS - the ones the checker certified while it accepted
\ that definition - and its declared arity. It does not hold the callee's
\ instructions, and that is the whole design decision: a caller that copied
\ machine words would be putting instructions into its module that the module's
\ own dialect has no form for, so the register allocator could not say which
\ registers they read and write and the allocation validator would have nothing
\ to re-derive them from. Tokens go in at the top of the chain instead. The
\ caller's elaborator meets them exactly as it meets its own, and everything
\ downstream - selection, allocation, the validator, the emitter - sees one
\ ordinary module and learns no new concept at all.
\
\ THE KEY IS THE ADDRESS AND NOT THE NAME, for the reason
\ src/compiler/native/clobber.f gives in full: a call site branches to an
\ ADDRESS, and the code at an address is written once BETWEEN RECLAMATIONS,
\ because src/compiler/native/publish.f writes every emission at the engine's
\ free code slot and moves the pointer past it. A name can be redefined; a slot
\ cannot be claimed twice while the pointer only goes forward. So a row written
\ here is never contradicted, and a second row for one address is refused by
\ name rather than replacing a body some caller has already copied.
\
\ AND THE POINTER DOES GO BACK, SO A ROW DIES WITH ITS ROUTINE. A FORGET
\ (src/habu/xref.f) and a declaration rollback
\ (src/core/generated-declaration-dictionary.f) both hand the bytes above the
\ code pointer back to the engine, and the next definition is compiled over
\ them. A row left behind then holds the body of a routine that is gone, and
\ that is worse here than next door: a caller does not merely mis-save around
\ such a row, it SPLICES it, so the caller computes what the reclaimed routine
\ computed. It was reproduced end to end - a caller of a six-addition word
\ inherited a one-addition row at the same address, passed the arity
\ cross-check because both are one in and one out, and answered 6 where 26 was
\ right. So this file registers with src/habu/xref.f's CODE-RECLAIM, the one
\ word every checked reclamation of code space goes through, and gives up every
\ row at or above the floor before the bytes are released.
\
\ WHICH IS NOT THE EVICTION THE CEILING BELOW DECLINES TO MAKE. Turning a new
\ body away keeps every row a caller may have been compiled against; giving back
\ the rows of code that no longer exists takes away only rows whose callers went
\ with them. The first would make which words are inlined depend on migration
\ order, and the second is what stops it depending on migration HISTORY - on how
\ many times a program forgot and re-migrated before this one.
\
\ AND THE ROW HOLDS NO NAME, BECAUSE THE ADDRESS A CALLER STATES IS ALREADY THE
\ NAME'S OWN ANSWER. A call site does not search for its row: it STATES an
\ address, taken from what its own migration declared about the callee
\ (src/compiler/native/migrate.f's CALLEE), and reads whatever row is keyed
\ there. That statement used to be free, so an address one routine out landed on
\ a real row and only the arity was held against it - which agrees by coincidence
\ all the time, because `( n n -- n )` helpers are everywhere - and the row
\ carried the published name so that the two could be told apart. It is not free
\ any more: RESOLVES-TO-ENTRY refuses a staged address that is not where the
\ staged spelling's own word begins, so the address a row is read by is the
\ dictionary's answer for the name the site wrote. A recorded name held against
\ that spelling could then only repeat the dictionary, and where it did NOT it
\ was wrong: `EXPORT` publishes a second record over one routine's code, so an
\ alias names that routine as truly as its first name does and a name comparison
\ would refuse a legal copy. What remains held against the row is the ARITY,
\ which is still the caller's own statement and not yet anybody else's answer.
\
\ WHICH BODIES ARE RECORDED, AND BOTH HALVES OF THE RULE ARE DERIVED.
\
\   A ROW HOLDS STRAIGHT-LINE OPERATIONS. Every token of a row means a literal,
\   an operation, a constant-and-operation word, a fixed value or a compile-time
\   rename - so a body with a control structure, a locals group or a `RECURSE` is
\   not recorded at all. That is what makes splicing a body into a caller a splice
\   of OPERATIONS rather than of blocks, and it is also what makes the copying
\   terminate: NO ROW HOLDS A CALL, so nothing that is copied can contain a call
\   to copy in its turn and no body can reach itself. The rule is applied twice -
\   once when the row is written, against the callee's own word model, and again
\   at every call site against the CALLER's model, which has to admit each token
\   itself before it may splice it.
\
\   AND A CALL THE CALLEE'S OWN COMPILATION COPIED IS NOT A CALL IN THE ROW. This
\   is the one place where the rule is about the ROUTINE and not about the source
\   text, and it has to be. `: T-GET-N ( ptr a n -- r ) T-AT-N @ ;` WRITES a call,
\   and the routine published for it contains none: its own elaboration found a
\   row for T-AT-N and copied that row in, so what the publication seam wrote at
\   that address is `cells + @` and a return. Judging such a body by its source
\   token refuses a routine that is straight-line machine code, which is what kept
\   every kernel of the third comparison corpus paying a call per element. So the
\   token staged for a call the elaborator COPIED is not the call at all: it is
\   the callee's own row, spliced in whole by STAGE-RECORD below.
\
\   THE ROW THEREFORE DESCRIBES THE ROUTINE'S OPERATIONS AT ITS ADDRESS RATHER
\   THAN THE TEXT THAT WAS WRITTEN, and that is the honest way round. A row is
\   keyed by an address and read by a caller that is about to reproduce what
\   lives there; a row saying "call T-AT-N" about a routine with no call in it
\   would describe something nobody published. The flattened row is the same
\   operations in the same order, so it elaborates to the instructions the
\   emitter really wrote - and the no-call invariant above is then kept BY
\   INDUCTION rather than by refusing the source: the row spliced in has no call
\   in it, so neither has the row built out of it, and there is no depth to bound
\   and no counter to keep.
\
\   WHAT A ROW DOES NOT HOLD IS THE ROUTINE'S TWO CROSSINGS, and the splice owes
\   both. A routine reads its arguments out of data-stack cells and writes its
\   results back into them, and neither end is a token: the entry block's cell
\   arguments are the shape the recorded tokens were elaborated against, and the
\   exit crossing belonged to the EMIT-RETURN the callee's own compilation ended
\   in. So the caller reproducing what lives at that address crosses the argument
\   positions to cells before the tokens and the result positions to cells after
\   them (DO-INLINE in src/compiler/native/elaborate.f). A splice of the tokens
\   alone would be a splice of the routine's middle only.
\
\   AND THE SIZE RULE STAYS ONE MEASUREMENT RATHER THAN A SUM OVER A CHAIN.
\   Because a row is flattened while its routine is being compiled, the emission
\   the rule is asked about ALREADY contains those copies - it is what the
\   flattened tokens elaborate to. A chain of routines each copying the last is
\   therefore admitted or refused at every step by the emitter's own instruction
\   count, with no transitive arithmetic anywhere and no second authority that
\   could disagree with the emitter about what it emitted.
\
\   AND THE COPY MAY NOT BE BIGGER THAN THE CALL IT REPLACES. This is where the
\   size rule comes from. A call to a routine of arity (in -> out) costs the SITE
\   in stores, its branch, up to two pointer adjustments and out loads; the
\   ROUTINE pays the mirror of that - up to two pointer adjustments, in loads,
\   out stores and one return. So one interface is AT MOST `in + out + 3`
\   instructions, and a routine whose entire emission is within twice that is one
\   whose body is within a call site's own half. Copy such a body into a site and
\   the site does not grow by more than the interfaces that disappeared.
\
\   THE WORD "AT MOST" IS NEW AND IT IS THE HONEST WORD. The two pointer
\   adjustments used to be two instructions every time. Since dot
\   habu-place-the-data-9f128e58 the data-stack pointer stands where the fewest
\   adjustments are needed and each of the four is written only when it moves
\   the pointer at all - so a leaf routine of equal arity pays NONE of them, and
\   a call site whose place already is the callee's base pays none either. The
\   number below is therefore a bound on an interface rather than a count of one,
\   and it is deliberately the LOOSE direction: it admits every body it admitted
\   before, so nothing that was copied stops being copied. Tightening it is the
\   whole of dot habu-measure-inline-cost-031e817e, which already owns the other
\   half of the same question - the rule measures the callee's emission under the
\   CALLEE's register pressure and not the cost at the site - and that dot is
\   where the bound and its derivation are to be made exact together.
\
\ WHAT IS NOT DERIVED IS THE ROW'S CAPACITY. A rename is a token and no
\ instruction at all, so a body of nothing but `dup` and `swap` can hold more
\ tokens than its emission holds instructions, and a table of fixed rows has to
\ stop somewhere. BODY-MAX and SPELL-MAX are those stops, and they are CAPACITIES
\ rather than parts of the rule: STAGE-FITS? and SPELL-FITS? are asked BEFORE
\ each staging step, so a body that does not fit is one this file never claims to
\ hold and its callers call it, exactly as they call a word the engine compiled.
\ Asking per step rather than once up front is what a flattened row needs: the
\ tokens a splice adds are not the source's, so the only count that can be tested
\ is the one already staged against the one about to be. What is not allowed is
\ finding out halfway - a token staged past either ceiling is refused by name,
\ because a row half written is a body nobody could copy.
\
\ WHAT AN UNKNOWN ADDRESS ANSWERS. Nothing - KNOWN? is false, and the call site
\ emits the call it always emitted. A word the engine's own emitter compiled has
\ no row here and never will, so copying a body is something a chain-compiled
\ callee EARNS and the discipline is unchanged everywhere else.
\
\ THE STAGING IS SEPARATE FROM THE ROW, AND THAT IS A SOUNDNESS RULE RATHER THAN
\ A CONVENIENCE. The tokens have to be copied out while the callee's module is
\ still being built, because that is the only moment its interner will answer for
\ a spelling; the ADDRESS is not known to be the callee's until the publication
\ seam has written the routine there. Between those two moments a refusal is
\ possible, and a row keyed to an address the publication never claimed would be
\ a body waiting for whatever word is published there next. So the tokens are
\ staged first and keyed second, and a run that ends without committing throws
\ its staging away.
\
\ AND THE ROW IS CLAIMED BEFORE IT IS WRITTEN, WHICH IS THAT SAME RULE ONE STEP
\ FURTHER. A publication stands between the staging and the row, and it is the
\ one step in a migration that cannot be taken back. Every question this file
\ could answer with a refusal - is that an address at all, is that a name at all,
\ does the address already have a row, is there room for one more - can be asked
\ BEFORE that publication, and none of those answers changes across it: the
\ address a routine will occupy is the slot the emission was measured from, the
\ name it will be published under is the one its migration already kept, and no
\ row can be written between the two because a row is written only by the commit
\ that completes a claim and only one body is ever staged at a time. So they are
\ all asked by CLAIM, while a refusal still costs nothing but a compilation
\ nobody has committed to, and COMMIT asks only whether the claim it is
\ completing was made. A refusal on the far side of the publication would leave a
\ word running new code while the migration that published it reported that it
\ had failed.
\
\ AND A FULL TABLE IS NOT A REFUSAL OF THE MIGRATION. Of those questions all but
\ one are about this definition; the last is about this file, and this file
\ having no room for another body is not a reason to refuse a word the chain
\ compiled. So the row is DECLINED - the staging is given up, the word publishes,
\ and its callers call it, exactly as they call a body past the size rule or past
\ BODY-MAX - and the decline is counted rather than dropped in silence, because
\ silence is what would make which words are inlined depend on the order they
\ were migrated in.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/tape.f

package NINL

private

\ How many published routines this file can remember in one process, for the
\ reason src/compiler/native/clobber.f's table is fixed: this runs while the
\ engine is compiling and has nowhere to allocate from. A row can be dropped
\ without making anything wrong - a caller that finds no row emits a call - so
\ the ceiling turns a body away rather than evicting one that callers may already
\ have been compiled against, and it turns it away by declining the ROW and not
\ the routine. What dropping one silently would cost is stated at the head of
\ this file, and DECLINED below is what is kept instead of the silence. The rows
\ this file DOES give back are the ones whose code was reclaimed, which is the
\ opposite case: their callers went with them, so the ceiling is a ceiling on
\ LIVE bodies rather than on how many times a process may recompile a word.
64 constant ROWS-MAX

\ The capacity of one row, and the argument for the number is above: it is not
\ the size rule, which is derived from the callee's own interface, but the point
\ where a fixed row stops. The size rule admits a body of `in + out + 3`
\ instructions, so sixteen tokens is past every body it admits at every arity
\ this chain compiles except one made almost entirely of renames.
16 constant BODY-MAX

\ The longest spelling one recorded token may carry. Habu's own vocabulary is
\ short and a callee's body is written in it; a longer spelling is refused when
\ it is staged rather than truncated into a name that means something else.
24 constant SPELL-MAX

ROWS-MAX BODY-MAX * constant SLOTS-MAX

create R-ENTRY ROWS-MAX cells allot
create R-IN ROWS-MAX cells allot
create R-OUT ROWS-MAX cells allot
create R-N ROWS-MAX cells allot
SLOTS-MAX TYPED-BUFFER R-KIND NTAPE:kind
create R-LIT SLOTS-MAX cells allot
create R-SLEN SLOTS-MAX cells allot
create R-SPELL SLOTS-MAX SPELL-MAX * allot

here CELL 1- and CELL swap - CELL 1- and allot
variable ROWS-N
0 ROWS-N !

\ How many bodies this file had no room for. It is an event count and not a piece
\ of state: nothing reads it to decide anything, and giving rows back does not
\ un-decline a body that was already turned away.
variable DECLINED-N
0 DECLINED-N !

\ ---- the staging area --------------------------------------------------------
\ One body, filled while the definition that owns it is still being compiled and
\ keyed to an address only after that definition has been published.
BODY-MAX TYPED-BUFFER S-KIND NTAPE:kind
create S-LIT BODY-MAX cells allot
create S-SLEN BODY-MAX cells allot
create S-SPELL BODY-MAX SPELL-MAX * allot

here CELL 1- and CELL swap - CELL 1- and allot
variable S-OPEN
variable S-N
variable S-IN
variable S-OUT

\ The claim: the address this staging is to be keyed to, and the row that will
\ hold it. Both are answered by CLAIM, before the routine is published, so that
\ the commit which runs after the publication has nothing left to decide. The
\ ADDRESS is a claim's business and not the staging's, because it describes the
\ ROUTINE: which routine a staged body turned out to be is settled by the
\ publication it is claimed against, not by the tokens.
variable S-CLAIM
variable S-ENTRY
variable S-ROW
0 S-OPEN !
0 S-CLAIM !

: S-OPEN-CK ( -- )
   S-OPEN @ 0= if E-NINL-STATE throw then ;

\ A claim is only ever made over an open staging and is given up with it, so this
\ answers the open question too.
: CLAIM-CK ( -- )
   S-CLAIM @ 0= if E-NINL-STATE throw then ;

: S-SPELL-AT ( n -- ptr u8 )
   SPELL-MAX * S-SPELL + ;

\ Stage one token's fields. Every constructor below ends here, so the capacity of
\ a row and of a spelling is proved in one place.
: S-PUT ( ptr u8 n NTAPE:kind n -- )
   {: a:ptr u:n k:NTAPE:kind v:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   S-OPEN-CK
   S-N @ {: j:n :}
   j BODY-MAX >= if E-NINL-CAP throw then
   u SPELL-MAX > if E-NINL-CAP throw then
   k j S-KIND !
   v j cells S-LIT + !
   u j cells S-SLEN + !
   u 0<> if a  j S-SPELL-AT  u STR-LEN BYTE-COPY-LEN then
   j 1+ S-N ! ;

\ ---- rows --------------------------------------------------------------------
\ Which row this address has, or -1. Linear, because the table is small and the
\ answer has to be exact: a hash that collided would hand one routine's body to
\ another routine's callers.
: ROW-OF ( n -- n )
   {: entry:n :}
   -1
   ROWS-N @ 0 ?do
      i cells R-ENTRY + @ entry = if drop i leave then
   loop ;

: ROW-CK ( n -- n )
   ROW-OF dup 0 < if E-NINL-BOUND throw then ;

: SLOT-OF ( n n -- n )
   {: l:n k:n :}
   k 0 < k l cells R-N + @ >= or if E-NINL-BOUND throw then
   l BODY-MAX * k + ;

: R-SPELL-AT ( n -- ptr u8 )
   SPELL-MAX * R-SPELL + ;

\ ---- giving back the rows of code that was reclaimed --------------------------
\ The first row at or above this address, or the end of the table. What makes
\ this one number the whole answer is that the live table is in publication
\ order and a publication's slot is above every slot claimed before it -
\ src/compiler/native/publish.f holds that as a REFUSAL (E-NPUB-SLOT) rather
\ than an assumption - so the rows a reclamation takes away are a SUFFIX.
: FLOOR-ROW ( n -- n )
   {: floor:n :}
   ROWS-N @
   ROWS-N @ 0 ?do
      i cells R-ENTRY + @ floor >= if drop i leave then
   loop ;

\ ...and that the rest of the table really is above the floor. A row below it
\ after the cut would mean the table is not the sequence the cut rests on, which
\ is a defect in this file rather than anything a program can ask for: there is
\ no correct answer to give and no caller to give it to, so it dies here. A
\ watcher may not throw - the reclamation it is answering is already half done -
\ and this is the shape src/core/decl-event.f uses for the same class.
: ORDER-CK ( n n -- )
   {: floor:n k:n :}
   ROWS-N @ k ?do
      i cells R-ENTRY + @ floor < if
         s" ninl: recorded bodies out of publication order" 76 die
      then
   loop ;

\ Give back every body whose routine starts at or above this address. Dropping a
\ suffix is what keeps a MARK meaning what it meant - a mark is a prefix count,
\ and a prefix of a prefix is the same prefix - and it is why no column has to be
\ carried anywhere: the rows below the cut are untouched, and COMMIT writes every
\ column of the row it fills, so a reused slot cannot show a previous body's
\ tokens.
: DROP-FROM ( n -- )
   {: floor:n :}
   floor FLOOR-ROW {: k:n :}
   floor k ORDER-CK
   k ROWS-N ! ;

public

\ ---- what the size rule is ---------------------------------------------------
\ The most instructions one side of a call to a routine of this arity can be: the
\ site's stores, its branch, its two pointer adjustments and its loads - which is
\ the same bound as the routine's own two adjustments, loads, stores and return.
\ It is a bound and not a count because the placement writes an adjustment only
\ when the pointer really moves; the head of this file says which dot makes the
\ two exact again.
: INTERFACE-INSNS ( n n -- n )
   {: in:n out:n :}
   in out + 3 + ;

\ Is a routine of this arity, whose whole emission is this many instructions,
\ small enough that copying its body into a call site does not make the site
\ bigger? Its body is the emission less its own interface, and the site's own
\ interface is the same count again, so the question is whether the emission is
\ within twice one interface.
: SMALL? ( n n n -- bool )
   {: in:n out:n insns:n :}
   insns  in out INTERFACE-INSNS 2 *  <= ;

\ ---- what one row can hold ---------------------------------------------------
\ The two capacities, asked before anything is staged. They are not the size
\ rule - the head of this file says why - and they are asked rather than
\ discovered so that a body too big for a row is one nobody started to record.
: FITS? ( n -- bool )
   BODY-MAX <= ;

\ The same ceiling, asked about what is staged NOW plus what is about to be. A
\ recorder stages one token for a literal or a name and a whole row for a call
\ its callee's compilation copied, so the number that has to be tested is the
\ number of tokens the next step will add - which is why this and not FITS? is
\ what src/compiler/native/migrate.f asks before every step.
: STAGE-FITS? ( n -- bool )
   {: k:n :}
   S-OPEN-CK
   S-N @ k + FITS? ;

: SPELL-FITS? ( n -- bool )
   {: u:n :}
   u 1 >=  u SPELL-MAX <=  and ;

\ ---- staging one body --------------------------------------------------------
\ Open the staging area for a definition of this arity. A second one over a live
\ one is refused: one definition is compiled at a time, and a body left staged
\ under another definition's arity would be keyed to the wrong routine.
: STAGE-BEGIN ( n n -- )
   {: in:n out:n :}
   S-OPEN @ 0<> if E-NINL-STATE throw then
   in 0 < out 0 < or if E-NINL-STATE throw then
   1 S-OPEN !
   0 S-CLAIM !
   0 S-N !
   in S-IN !
   out S-OUT ! ;

\ Three constructors, one per token kind a recorded body may hold, so the literal
\ rule is structural exactly as it is on the tape these tokens come from: there
\ is no way to stage a name that carries a value and no way to stage a literal
\ without one.
: STAGE-NAME ( ptr u8 n -- )
   {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   u 1 < if E-NINL-STATE throw then
   a u NTAPE-KIND:NAME 0 S-PUT ;

: STAGE-INT ( n -- )
   {: v:n :}
   S-SPELL 0 NTAPE-KIND:INT-LITERAL v S-PUT ;

: STAGE-REAL ( n -- )
   {: v:n :}
   S-SPELL 0 NTAPE-KIND:REAL-LITERAL v S-PUT ;

\ A fourth constructor, and the only one that stages more than one token: the
\ whole of ANOTHER address's row. It is what a body writes in place of a call its
\ own compilation copied, and the head of this file argues why that is the right
\ thing to write down. The row is read through the same ROW-CK the public readers
\ use, so an address with no row is refused by name here rather than staging
\ nothing and leaving a body that quietly lost a call; and the caller has already
\ asked STAGE-FITS? about exactly this many tokens, so the capacity refusal
\ inside S-PUT is a backstop and not the path.
\
\ NOTHING IS RE-JUDGED WHILE IT IS COPIED. Every token of the row was admitted
\ against the callee's own word model when the row was written, and again against
\ THIS definition's model when its elaborator decided to copy the call - which is
\ the decision that brings a recorder here at all. A third judgement would be a
\ second opinion about a question already answered twice.
: STAGE-RECORD ( n -- )
   {: entry:n :}
   S-OPEN-CK
   entry ROW-CK {: l:n :}
   l cells R-N + @ 0 ?do
      l BODY-MAX * i + R-SPELL-AT
      l BODY-MAX * i + cells R-SLEN + @
      l BODY-MAX * i + R-KIND @
      l BODY-MAX * i + cells R-LIT + @
      S-PUT
   loop ;

\ Give the staging up without keying it to anything. A run that refused, a
\ definition whose body turned out not to qualify, and a body this file had no
\ room for all end here, and so does the commit that consumed one. A claim goes
\ with the staging it was made over: an address kept past the body it was
\ answered for would be a row waiting for the next definition's tokens.
: STAGE-CLEAR ( -- )
   0 S-OPEN !
   0 S-CLAIM !
   0 S-N ! ;

: STAGED? ( -- bool )
   S-OPEN @ 0<> ;

: STAGED-TOKENS ( -- n )
   S-OPEN-CK
   S-N @ ;

\ Is there room for a row this file does not have yet? Asking is free and decides
\ nothing: a body that meets a full table is declined by CLAIM below, and this is
\ how a caller - or a test walking the table up to its ceiling - can see the
\ ceiling coming rather than infer it from a count it would have to know.
: ROOM? ( -- bool )
   ROWS-N @ ROWS-MAX < ;

\ Claim the row this staging is to become: the address the routine is about to be
\ published at. Everything that can refuse refuses here, before the publication,
\ and the head of this file argues why every one of these questions belongs on
\ this side of it.
\
\ ALL BUT ONE ARE REFUSALS AND THE LAST IS A DECLINE. An address that is not one
\ is this file's caller getting the protocol wrong; a second row for one address
\ is refused rather than replacing a body some caller has already copied - it
\ cannot be reached through the publication seam, which never claims a code slot
\ twice, and it is held here because that is a property of another file and a
\ rule callers rest on should fail closed where it is used. A full table is
\ neither: the migration is sound, the word will publish, and only the row is
\ given up.
\
\ THE ADDRESS IS VALIDATED BEFORE THE TABLE IS ASKED FOR ROOM, so a malformed
\ claim is refused whether or not there was anywhere to put it. A decline that
\ swallowed a protocol error would make the caller's bug appear and disappear
\ with the table's fill level.
: CLAIM ( n -- )
   {: entry:n :}
   S-OPEN-CK
   entry 0 <= if E-NINL-STATE throw then
   entry ROW-OF 0 >= if E-NINL-DUP throw then
   ROOM? 0= if
      1 DECLINED-N +!
      STAGE-CLEAR exit
   then
   entry S-ENTRY !
   ROWS-N @ S-ROW !
   1 S-CLAIM ! ;

\ Is there a row waiting to be written? This is what a caller asks after the
\ claim: a claim that declined answers no, and the caller publishes its word and
\ never commits.
: CLAIMED? ( -- bool )
   S-CLAIM @ 0<> ;

\ Write the row the claim reserved. This runs on the far side of the publication,
\ where a refusal cannot be acted on, so it decides nothing: the address and the
\ row are the claim's answers, and neither can have changed since - the table
\ grows only here, and a commit is only reached through a claim, of which one
\ staging admits exactly one. What is left is the protocol itself, which fails
\ closed: a commit with no claim behind it is refused rather than writing a row
\ nobody asked for.
: COMMIT ( -- )
   CLAIM-CK
   S-ROW @ {: l:n :}
   S-ENTRY @ l cells R-ENTRY + !
   S-IN @ l cells R-IN + !
   S-OUT @ l cells R-OUT + !
   S-N @ l cells R-N + !
   S-N @ 0 ?do
      i S-KIND @  l BODY-MAX * i +  R-KIND !
      i cells S-LIT + @  l BODY-MAX * i + cells R-LIT + !
      i cells S-SLEN + @  l BODY-MAX * i + cells R-SLEN + !
      i cells S-SLEN + @ 0<> if
         i S-SPELL-AT  l BODY-MAX * i + R-SPELL-AT
         i cells S-SLEN + @ STR-LEN BYTE-COPY-LEN
      then
   loop
   l 1+ ROWS-N !
   STAGE-CLEAR ;

\ ---- reading a row -----------------------------------------------------------
\ Does this file hold the body of the routine at this address?
: KNOWN? ( n -- bool )
   ROW-OF 0 >= ;

: IN@ ( n -- n )
   ROW-CK cells R-IN + @ ;

: OUT@ ( n -- n )
   ROW-CK cells R-OUT + @ ;

: TOKENS ( n -- n )
   ROW-CK cells R-N + @ ;

: KIND@ ( n n -- NTAPE:kind )
   {: entry:n k:n :}
   entry ROW-CK k SLOT-OF R-KIND @ ;

\ The literal value. Probe the kind first: a token that carries no literal throws
\ rather than answering the zero the row stores, so a caller cannot mistake "no
\ literal" for "the value zero" - the rule NTAPE:LIT@ keeps over the same tokens.
: LIT@ ( n n -- n )
   {: entry:n k:n :}
   entry ROW-CK k SLOT-OF {: s:n :}
   s R-KIND @ NTAPE-KIND:NAME NTAPE-KIND:EQ if E-NINL-BOUND throw then
   s cells R-LIT + @ ;

\ The spelling, as the bytes the callee's body was written with. Only a name has
\ one, for the same reason only a literal has a value.
: SPELL$ ( n n -- ptr u8 n )
   {: entry:n k:n :}
   entry ROW-CK k SLOT-OF {: s:n :}
   s R-KIND @ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NINL-BOUND throw then
   s R-SPELL-AT  s cells R-SLEN + @ ;

\ How many bodies this file remembers, which is what a test measures a
\ publication against.
: ROWS ( -- n )
   ROWS-N @ ;

\ How many bodies this file had no room for. A process whose table filled up
\ compiles correct code and slower code, and this is the difference between the
\ two: nothing else in the system distinguishes a word that was called because
\ its body did not qualify from one that was called because the table was full.
: DECLINED ( -- n )
   DECLINED-N @ ;

\ ---- giving a run's rows back ------------------------------------------------
\ The table is written at its end and read by a scan over its whole length, so
\ what it holds is a SEQUENCE and a mark taken from it is a prefix. A run that
\ recorded bodies and wants them gone again - a suite that walks the table up to
\ its ceiling, a stage whose publications are being thrown away - releases back
\ to its mark, and every row written since is forgotten in one step.
\
\ IT IS ONLY EVER SAFE IN ONE DIRECTION. Losing a row costs a caller a call and
\ nothing else, which is the same thing a full table costs; what may never happen
\ is a row surviving that describes something nobody published, so this drops
\ rows and never adds one. An address it forgets can only come back if the code
\ space was reclaimed, and a reclamation is exactly what takes that row away
\ first. A release with a body staged is refused, because a claim already holds
\ the row index it was given and a table moved under it would key that body to
\ somebody else's row.
\
\ AND A RECLAMATION MOVES THE TABLE UNDER A MARK SOMEBODY IS HOLDING, which is
\ the one interplay worth writing down. RECLAIM below drops a SUFFIX, so a mark
\ is left in one of two states and there is no third. Either the table still
\ reaches the mark, and the rows below it are untouched - a prefix of a prefix is
\ the same prefix - so releasing to it means exactly what it meant. Or the
\ reclamation cut below the mark, and the mark now names rows that no longer
\ exist: the bound check above refuses it by name rather than raising ROWS-N back
\ over rows whose code is gone. A mark is never silently re-interpreted.
: MARK ( -- n )
   ROWS ;

: RELEASE ( n -- )
   {: k:n :}
   S-OPEN @ 0<> if E-NINL-STATE throw then
   k 0 < k ROWS-N @ > or if E-NINL-BOUND throw then
   k ROWS-N ! ;

private

\ ---- what a code reclamation does to this file --------------------------------
\ Two things, and the second is why this is a word of its own rather than the
\ drop above handed straight to the registration.
\
\ THE ROWS OF THE RECLAIMED CODE GO. That is DROP-FROM, and the head of this file
\ argues it.
\
\ AND A CLAIM OUTSTANDING OVER THAT RECLAMATION IS GIVEN UP. A claim holds the
\ address a routine is ABOUT to be published at and the row index that address is
\ about to occupy; the address is the free code slot at the moment the emission
\ was placed, so EVERY reclamation floor is at or below it and every reclamation
\ therefore invalidates every live claim. The staging goes with it, so the commit
\ on the far side of the publication cannot write a row for a slot that was taken
\ away - it finds no claim and refuses by name.
\
\ WHETHER ANY REAL PATH CAN GET HERE MID-MIGRATION, AND THE ANSWER IS NO. Between
\ the claim and the commit stands exactly one step, NPUB:REPUBLISH, and it moves
\ the code pointer only forward, evaluates nothing, and opens no declaration - so
\ the two words that lower the pointer (src/habu/xref.f FORGET-DEFS-FROM,
\ src/core/generated-declaration-dictionary.f ROLLBACK) are not reachable from
\ inside it. A migration's own `evaluate`, which is what a declaration rollback
\ would unwind, has already happened: it is stage N0, many steps before the claim.
\ The guard is kept anyway because it is one comparison and because the invariant
\ it rests on belongs to another file: if REPUBLISH ever grew a step that could
\ reclaim, this file would give the row up instead of writing it against a slot
\ that had moved. The seam refuses that publication too - the emission's recorded
\ placement would no longer be the slot being claimed, which is E-NPUB-PLACE - so
\ the two are independent and neither is the other's excuse.
: RECLAIM ( n -- )
   {: floor:n :}
   floor DROP-FROM
   S-CLAIM @ 0<> if STAGE-CLEAR then ;

\ One registration, and no way to undo it: a row that outlived its code is a body
\ a caller would splice in place of the routine it meant to call.
: WATCH-INSTALL ( -- )
   [: RECLAIM ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

get-current prot-wid-add

public
get-current prot-wid-add

;package
