\ inline.f - the body of a small routine the native chain published, kept against
\ the address its code starts at. One concern: the record a call site copies a
\
\ A row holds the callee's own SOURCE TOKENS and its declared arity, never its
\ instructions: copied machine words would be forms the caller's module has no
\ dialect entry for, so nothing downstream could say what they read and write.
\
\ NO ROW HOLDS A CALL, which is what makes copying terminate. A call the
\ callee's own compilation copied is not a call in the row either - the row
\ carries that callee's row spliced in whole, so the invariant holds by
\ induction and there is no depth to bound.
\
\ The key is the ADDRESS and not the name: a slot is claimed once between
\ reclamations, so a row is never contradicted. Rows die with their routine,
\ through src/habu/xref.f CODE-RECLAIM.
\
\ A row does not hold the routine's two crossings. The splice owes both: the
\ caller crosses argument positions to cells before the tokens and result
\ positions to cells after them (DO-INLINE in elaborate.f).
\
\ The tokens are staged first and keyed second, because the interner answers
\ only while the callee's module is being built and the address is not known to
\ be the callee's until the seam has published there. Everything that can refuse
\ refuses in CLAIM, before that publication; COMMIT decides nothing.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/tape.f

package NINL

private

\ The ceiling turns a body away rather than evicting one callers may already
\ have been compiled against, and it declines the ROW and not the routine.
64 constant ROWS-MAX

\ The point where a fixed row stops, not the size rule: past every body the size
\ rule admits at every arity, bar one made almost entirely of renames.
16 constant BODY-MAX

\ A longer spelling is refused when staged rather than truncated into a name
\ that means something else.
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

\ An event count, not state: giving rows back does not un-decline a body.
variable DECLINED-N
0 DECLINED-N !

\ ---- the staging area --------------------------------------------------------
\ One body, filled while its definition is still being compiled and keyed to an
\ address only after that definition has been published.
BODY-MAX TYPED-BUFFER S-KIND NTAPE:kind
create S-LIT BODY-MAX cells allot
create S-SLEN BODY-MAX cells allot
create S-SPELL BODY-MAX SPELL-MAX * allot

here CELL 1- and CELL swap - CELL 1- and allot
variable S-OPEN
variable S-N
variable S-IN
variable S-OUT

\ The address describes the ROUTINE, so which routine a staged body turned out
\ to be is settled by the publication it is claimed against, not by the tokens.
variable S-CLAIM
variable S-ENTRY
variable S-ROW
0 S-OPEN !
0 S-CLAIM !

: S-OPEN-CK ( -- )
   S-OPEN @ 0= if E-NINL-STATE throw then ;

\ A claim is only ever made over an open staging and is given up with it.
: CLAIM-CK ( -- )
   S-CLAIM @ 0= if E-NINL-STATE throw then ;

: S-SPELL-AT ( n -- ptr u8 )
   SPELL-MAX * S-SPELL + ;

\ Every constructor below ends here, so both capacities are proved in one place.
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
\ Linear and exact: a hash that collided would hand one routine's body to
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
\ The live table is in publication order and publish.f refuses a slot below the
\ last routine's end (E-NPUB-SLOT), so the rows a reclamation drops are a SUFFIX.
: FLOOR-ROW ( n -- n )
   {: floor:n :}
   ROWS-N @
   ROWS-N @ 0 ?do
      i cells R-ENTRY + @ floor >= if drop i leave then
   loop ;

\ A row below the floor means the table is not the sequence the cut rests on: a
\ defect here with no correct answer to give, and a watcher may not throw.
: ORDER-CK ( n n -- )
   {: floor:n k:n :}
   ROWS-N @ k ?do
      i cells R-ENTRY + @ floor < if
         s" ninl: recorded bodies out of publication order" 76 die
      then
   loop ;

\ Dropping a SUFFIX is what keeps a MARK meaning what it meant, and COMMIT
\ writes every column, so a reused slot cannot show a previous body's tokens.
: DROP-FROM ( n -- )
   {: floor:n :}
   floor FLOOR-ROW {: k:n :}
   floor k ORDER-CK
   k ROWS-N ! ;

public

\ ---- what the size rule is ---------------------------------------------------
\ The site's MAXIMUM: one store per argument, one load per result, the branch,
\ and the two pointer adjustments. Every one but the branch can be nothing.
: SITE-INSNS ( n n -- n )
   {: in:n out:n :}
   in out + 3 + ;

\ Compares what a copy really costs - the body alone - with the most the call it
\ stands in for could have cost. The body is measured by A64EMIT:BODY-INSNS.
: SMALL? ( n n n -- bool )
   {: in:n out:n body:n :}
   body  in out SITE-INSNS  <= ;

\ ---- what one row can hold ---------------------------------------------------
\ Capacities, not the size rule, so a body too big is one nobody started to record.
: FITS? ( n -- bool )
   BODY-MAX <= ;

\ Asked about what is staged NOW plus what the next step adds, because a step
\ may stage one token or a whole row; a row half written is a body nobody copies.
: STAGE-FITS? ( n -- bool )
   {: k:n :}
   S-OPEN-CK
   S-N @ k + FITS? ;

: SPELL-FITS? ( n -- bool )
   {: u:n :}
   u 1 >=  u SPELL-MAX <=  and ;

\ ---- staging one body --------------------------------------------------------
\ A second staging over a live one is refused: one definition is compiled at a
\ time, and a body left staged under another arity would be keyed to the wrong routine.
: STAGE-BEGIN ( n n -- )
   {: in:n out:n :}
   S-OPEN @ 0<> if E-NINL-STATE throw then
   in 0 < out 0 < or if E-NINL-STATE throw then
   1 S-OPEN !
   0 S-CLAIM !
   0 S-N !
   in S-IN !
   out S-OUT ! ;

\ One constructor per token kind, so the literal rule is structural.
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

\ The only constructor that stages more than one token: the whole of another
\ address's row. Nothing is re-judged - every token was admitted twice already.
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

\ A claim goes with the staging it was made over: an address kept past the body
\ it was answered for would be a row waiting for the next definition's tokens.
: STAGE-CLEAR ( -- )
   0 S-OPEN !
   0 S-CLAIM !
   0 S-N ! ;

: STAGED? ( -- bool )
   S-OPEN @ 0<> ;

: STAGED-TOKENS ( -- n )
   S-OPEN-CK
   S-N @ ;

\ Asking is free and decides nothing; a body meeting a full table is declined.
: ROOM? ( -- bool )
   ROWS-N @ ROWS-MAX < ;

\ All but one of these are refusals and the last is a decline: a full table is
\ not a reason to refuse a word. The address is validated before room is asked
\ for, so a malformed claim cannot appear and disappear with the fill level.
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

\ A claim that declined answers no, and the caller publishes and never commits.
: CLAIMED? ( -- bool )
   S-CLAIM @ 0<> ;

\ Runs on the far side of the publication, where a refusal cannot be acted on,
\ so it decides nothing; a commit with no claim behind it is refused.
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

\ Probe the kind first: a token carrying no literal throws rather than answering
\ the zero the row stores - the rule NTAPE:LIT@ keeps over the same tokens.
: LIT@ ( n n -- n )
   {: entry:n k:n :}
   entry ROW-CK k SLOT-OF {: s:n :}
   s R-KIND @ NTAPE-KIND:NAME NTAPE-KIND:EQ if E-NINL-BOUND throw then
   s cells R-LIT + @ ;

\ Only a name has one, for the same reason only a literal has a value.
: SPELL$ ( n n -- ptr u8 n )
   {: entry:n k:n :}
   entry ROW-CK k SLOT-OF {: s:n :}
   s R-KIND @ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NINL-BOUND throw then
   s R-SPELL-AT  s cells R-SLEN + @ ;

: ROWS ( -- n )
   ROWS-N @ ;

\ A process whose table filled compiles correct code and slower code; nothing
\ else distinguishes a call made for size from a call made for a full table.
: DECLINED ( -- n )
   DECLINED-N @ ;

\ ---- giving a run's rows back ------------------------------------------------
\ The table is a SEQUENCE and a mark is a prefix. A release only ever drops rows.
\ A reclamation cutting below a mark leaves it refused by name, never re-read.
: MARK ( -- n )
   ROWS ;

: RELEASE ( n -- )
   {: k:n :}
   S-OPEN @ 0<> if E-NINL-STATE throw then
   k 0 < k ROWS-N @ > or if E-NINL-BOUND throw then
   k ROWS-N ! ;

private

\ ---- what a code reclamation does to this file --------------------------------
\ A reclamation floor is at or below every live claim's address, so the claim
\ and its staging go too and the commit finds none rather than keying a lost slot.
: RECLAIM ( n -- )
   {: floor:n :}
   floor DROP-FROM
   S-CLAIM @ 0<> if STAGE-CLEAR then ;

public

\ One registration, and no way to undo it: a row that outlived its code is a
\ body a caller would splice in place of the routine it meant to call.
\
\ PUBLIC BECAUSE A CAPTURED CHAIN HAS TO RE-RUN IT: the slot is CODE-RECLAIM's,
\ below the capture window, so a seeded engine would keep exactly the stale rows
\ this exists to drop, silently. Named on tools/aot-chain-capture.f's boot-run
\ list, which LFIND resolves - and it resolves no private word.
: WATCH-INSTALL ( -- )
   [: RECLAIM ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
