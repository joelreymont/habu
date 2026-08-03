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
\ ADDRESS, and the code at an address is written once, because
\ src/compiler/native/publish.f writes every emission at the engine's free code
\ slot and moves the pointer past it. A name can be redefined; a slot cannot be
\ claimed twice. So a row written here is never contradicted, and a second row
\ for one address is refused by name rather than replacing a body some caller has
\ already copied.
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
\   THE ROW THEREFORE DESCRIBES THE ROUTINE AT ITS ADDRESS RATHER THAN THE TEXT
\   THAT WAS WRITTEN, and that is the honest way round. A row is keyed by an
\   address and read by a caller that is about to reproduce what lives there; a
\   row saying "call T-AT-N" about a routine with no call in it would describe
\   something nobody published. The flattened row is the same operations in the
\   same order, so it elaborates to the instructions the emitter really wrote -
\   and the no-call invariant above is then kept BY INDUCTION rather than by
\   refusing the source: the row spliced in has no call in it, so neither has the
\   row built out of it, and there is no depth to bound and no counter to keep.
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
\   size rule comes from, and there is no chosen number in it. A call to a
\   routine of arity (in -> out) costs the SITE in stores, three instructions of
\   branch and pointer adjustment, and out loads; the ROUTINE pays the mirror of
\   that - one pointer move down, in loads, out stores, one pointer move up and
\   one return. So the whole interface is `in + out + 3` instructions on each
\   side, and a routine whose entire emission is no longer than both sides
\   together is one whose BODY is no longer than the call site's own half. Copy
\   such a body into a site and the site does not grow; every instruction the
\   interface spent disappears, on both sides. That is INTERFACE-INSNS and SMALL?
\   below, measured on the emission the validator accepted rather than guessed
\   at from the source.
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

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/tape.f

package NINL

private

\ How many published routines this file can remember in one process, for the
\ reason src/compiler/native/clobber.f's table is fixed: this runs while the
\ engine is compiling and has nowhere to allocate from. A row can be dropped
\ without making anything wrong - a caller that finds no row emits a call - but
\ dropping one silently would make which words are inlined depend on the order
\ they were migrated in, so the ceiling is a refusal and not an eviction.
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
0 S-OPEN !

: S-OPEN-CK ( -- )
   S-OPEN @ 0= if E-NINL-STATE throw then ;

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

public

\ ---- what the size rule is ---------------------------------------------------
\ How many instructions one side of a call to a routine of this arity is: the
\ site's stores, its three instructions of branch and pointer adjustment, and its
\ loads - which is the same count as the routine's own pointer move down, loads,
\ stores, pointer move up and return.
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

\ Give the staging up without keying it to anything. A run that refused, and a
\ definition whose body turned out not to qualify, both end here.
: STAGE-CLEAR ( -- )
   0 S-OPEN !
   0 S-N ! ;

: STAGED? ( -- bool )
   S-OPEN @ 0<> ;

: STAGED-TOKENS ( -- n )
   S-OPEN-CK
   S-N @ ;

\ Is there room for a row this file does not have yet? The migration asks before
\ the routine is published, because a refusal afterwards would leave a published
\ word whose body was staged and thrown away for a reason nothing recorded.
: ROOM-CK ( -- )
   S-OPEN-CK
   ROWS-N @ ROWS-MAX >= if E-NINL-CAP throw then ;

\ Key the staged body to the address the routine was published at. A second row
\ for one address is refused rather than replacing a body a caller has already
\ copied; it cannot be reached through the publication seam, which never claims a
\ code slot twice, and it is held here because that is a property of another file
\ and a rule callers rest on should fail closed where it is used.
: COMMIT ( n -- )
   {: entry:n :}
   S-OPEN-CK
   entry 0 <= if E-NINL-STATE throw then
   entry ROW-OF 0 >= if E-NINL-DUP throw then
   ROWS-N @ {: l:n :}
   l ROWS-MAX >= if E-NINL-CAP throw then
   entry l cells R-ENTRY + !
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

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
