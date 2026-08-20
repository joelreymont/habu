\ arena.f - the one generic compiler IR arena: disposable append-only typed
\ cell storage with geometric growth under a committed ceiling.
\
\ docs/compiler-ir-design.md sections 6.2-6.3 and the section 10.2 arena
\ obligations. Every dialect table (function, block, operation, value, type,
\ attribute, span) is an instance of this arena, and this package's private
\ CAST: window is the single raw index conversion authority the design demands
\ (design line 341): no dialect repeats trusted casts, no raw converter and no
\ pointer is public, and frozen readers accept nominal indices only.
\
\ OWNERSHIP SHAPE. An arena is created against a live IR-CTX context and every
\ cell it stores lives in spans bump-allocated from that context's mapping by
\ IR-CTX:SCRATCH-TAKE. Growth takes a doubled span and copies; the abandoned
\ smaller span stays in the mapping and dies with it - the accepted arena
\ discipline, bounded by the committed ceiling. Whole-range release is the
\ owning context's WITH-CONTEXT teardown: this file adds no MEM:RELEASE-BYTES
\ call site, the same constraint as context.f. FREEZE consumes the builder
\ handle into an immutable view; ABORT consumes it without publishing.
\
\ STALE HANDLES. The same fail-closed generation discipline as IR-CTX: a
\ handle is a nonzero, monotonic, never-reused serial from this package's
\ atomic counter, resolved against a bounded registry of live arenas. The
\ registry persists each arena's owner as the context serial - the only
\ storable form, since handles are sealed nominals a raw cell cannot re-mint -
\ and every resolution probes IR-CTX:SERIAL-LIVE? so an arena whose context
\ tore down rejects with E-IR-ARENA-STALE before any pointer is touched.
\ Registry slots are reclaimed eagerly on ABORT and lazily (dead owner) on the
\ next creation; the capacity below bounds the live population with a named
\ error. The linear-ownership work removes the whole mechanism.
\
\ APPEND AT THE CEILING. E-IR-ARENA-FULL is thrown before any mutation, so a
\ full arena stays usable: its contents and indices remain valid and
\ FREEZE still publishes them. Exhaustion never kills the arena.
\
\ CONCURRENCY. The generation counter is atomic; the registry is one
\ process-wide table under the current single-task compilation discipline,
\ exactly like the IR-CTX registry it mirrors.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f

package IR-ARENA
public

NEWTYPE arena 0
NEWTYPE view 0
NEWTYPE cell-id 0

private

CAST: MINT-ARENA ( n -- IR-ARENA:arena )
CAST: ARENA>N ( IR-ARENA:arena -- n )
CAST: MINT-VIEW ( n -- IR-ARENA:view )
CAST: VIEW>N ( IR-ARENA:view -- n )
CAST: MINT-IDX ( n -- IR-ARENA:cell-id )
CAST: IDX>N ( IR-ARENA:cell-id -- n )

\ ---- capacities and packing --------------------------------------------------
64 constant SLOT-MAX                \ live + frozen registry slots
8 constant SEED-CELLS                \ first data span, before any doubling
$7FFFFFFF constant AGEN-MAX          \ arena generation ceiling
32 constant LOCAL-BITS
$FFFFFFFF constant LOCAL-MAX         \ ordinal range inside one arena
LOCAL-MAX constant CEIL-MAX          \ a ceiling may commit the full ordinal range
1 constant ST-LIVE
2 constant ST-FROZEN

\ An index packs (generation << 32 | ordinal), mirroring IR-ID PACK-N,
\ so every minted value names the arena that minted it.
: PACK ( n n -- n )
   {: g:n l:n :}
   g LOCAL-BITS lshift l or ;

: PACK-GEN ( n -- n )
   LOCAL-BITS rshift ;

: PACK-LOCAL ( n -- n )
   LOCAL-MAX and ;

\ ---- registry storage --------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable AGEN-CELL
0 AGEN-CELL !
create AGENS SLOT-MAX cells allot
create AOWNERS SLOT-MAX cells allot
create ADATAS SLOT-MAX cells allot
create ACOUNTS SLOT-MAX cells allot
create ACAPS SLOT-MAX cells allot
create ACEILS SLOT-MAX cells allot
create ASTATES SLOT-MAX cells allot

: AGEN@ ( n -- n )
   cells AGENS + @ ;

: AGEN! ( n n -- )
   cells AGENS + ! ;

: AOWNER@ ( n -- n )
   cells AOWNERS + @ ;

: AOWNER! ( n n -- )
   cells AOWNERS + ! ;

: ADATA-FIELD ( n -- ptr ptr u8 )
   cells ADATAS + 0 ptr-field ;

: ACOUNT@ ( n -- n )
   cells ACOUNTS + @ ;

: ACOUNT! ( n n -- )
   cells ACOUNTS + ! ;

: ACAP@ ( n -- n )
   cells ACAPS + @ ;

: ACAP! ( n n -- )
   cells ACAPS + ! ;

: ACEIL@ ( n -- n )
   cells ACEILS + @ ;

: ACEIL! ( n n -- )
   cells ACEILS + ! ;

: ASTATE@ ( n -- n )
   cells ASTATES + @ ;

: ASTATE! ( n n -- )
   cells ASTATES + ! ;

: SLOTS-CLEAR ( -- )
   SLOT-MAX 0 ?do
      0 i AGEN!
   loop ;
SLOTS-CLEAR

\ ---- generation serials ------------------------------------------------------
: AGEN-NEXT-N ( n -- n )
   dup 0 < over AGEN-MAX >= or if E-IR-ARENA-SERIALS throw then
   1+ ;

: TRY-AGEN ( -- n bool )
   AGEN-CELL atomic@ {: current:n :}
   current AGEN-NEXT-N {: next:n :}
   current next AGEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-AGEN ( -- n )
   begin
      TRY-AGEN dup 0=
   while
      2drop
   repeat
   drop ;

\ ---- handle resolution -------------------------------------------------------
: FIND-A ( n -- n )
   {: g:n :}
   -1
   SLOT-MAX 0 ?do
      g i AGEN@ = if drop i leave then
   loop ;

\ Resolve a generation to its registry slot, fail closed on both a consumed
\ handle and a dead owner: a slot whose context tore down is retired on touch,
\ before its dangling data pointer can be read.
: RESOLVE ( n -- n )
   FIND-A
   dup 0 < if E-IR-ARENA-STALE throw then
   dup AOWNER@ IR-CTX:SERIAL-LIVE? 0= if
      0 over AGEN! E-IR-ARENA-STALE throw
   then ;

: LIVE-SLOT ( IR-ARENA:arena -- n )
   ARENA>N RESOLVE
   dup ASTATE@ ST-LIVE <> if E-IR-ARENA-FROZEN throw then ;

: FROZEN-SLOT ( IR-ARENA:view -- n )
   VIEW>N RESOLVE
   dup ASTATE@ ST-FROZEN <> if E-IR-ARENA-STATE throw then ;

\ ---- cell access -------------------------------------------------------------
\ Cells are eight-byte little-endian slots in the current data span, read and
\ written with the canonical CDIGEST slot words, like the context header.
: CELL-AT ( n n -- n )
   {: slot:n l:n :}
   slot ADATA-FIELD @ l CDIGEST:SLOT@ ;

\ Validate a raw packed index against one resolved slot: minted by this arena,
\ ordinal inside the readable count.
: IDX-AT ( n n -- n )
   {: slot:n raw:n :}
   raw PACK-GEN slot AGEN@ <> if E-IR-ARENA-OWNER throw then
   raw PACK-LOCAL
   dup slot ACOUNT@ >= if E-IR-ARENA-BOUND throw then ;

: NTH-RAW ( n n -- IR-ARENA:cell-id )
   {: slot:n k:n :}
   k 0 < k slot ACOUNT@ >= or if E-IR-ARENA-BOUND throw then
   slot AGEN@ k PACK MINT-IDX ;

\ ---- growth ------------------------------------------------------------------
: COPY-CELLS ( ptr u8 ptr u8 n -- )
   {: src:ptr dst:ptr cnt:n :}
   cnt 0 ?do
      src i CDIGEST:SLOT@ dst i CDIGEST:SLOT!
   loop ;

\ The capacity the doubling series reaches for `need` cells. Growing to a whole
\ row's worth at once lands on exactly the capacity a cell-at-a-time growth
\ would have climbed to, so reserving changes how many spans are taken, never
\ which capacities exist.
: CAP-FOR ( n n n -- n )
   {: cap:n need:n ceil:n :}
   cap
   begin
      dup need <
   while
      2 * ceil min
   repeat ;

\ Grow the data span until it holds `need` cells. The ceiling refusal and the
\ one scratch take both come before any slot field is written, so a refusal
\ mutates nothing; the copy preserves every published ordinal.
\
\ The capacity is rechecked because the series above doubles it: a slot claiming
\ a capacity of zero would double to zero forever, so the one state that could
\ turn this into a hang is a named refusal instead. NEW cannot install it - a
\ ceiling below one is refused there and the seed is the smaller of the two -
\ so this is the recheck, not the check.
: GROW-TO ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx slot:n need:n :}
   need slot ACAP@ <= if exit then
   slot ACAP@ 1 < if E-IR-ARENA-STATE throw then
   need slot ACEIL@ > if E-IR-ARENA-FULL throw then
   slot ACAP@ need slot ACEIL@ CAP-FOR {: ncap:n :}
   c ncap CDIGEST:SLOT-BYTES * IR-CTX:SCRATCH-TAKE drop {: nbase:ptr :}
   slot ADATA-FIELD @ nbase slot ACOUNT@ COPY-CELLS
   nbase slot ADATA-FIELD !
   ncap slot ACAP! ;

: OWN-CHECK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c IR-CTX:SERIAL slot AOWNER@ <> if E-IR-ARENA-OWNER throw then ;

\ ---- creation ----------------------------------------------------------------
: CEIL-OK ( n -- )
   dup 1 < over CEIL-MAX > or if E-IR-ARENA-CEIL throw then
   drop ;

\ Retire every slot whose owning context tore down; their storage is already
\ unmapped and their generations can never resolve again.
: SWEEP ( -- )
   SLOT-MAX 0 ?do
      i AGEN@ 0 <> if
         i AOWNER@ IR-CTX:SERIAL-LIVE? 0= if
            0 i AGEN!
         then
      then
   loop ;

: FREE-SLOT ( -- n )
   -1
   SLOT-MAX 0 ?do
      i AGEN@ 0= if drop i leave then
   loop
   dup 0 < if E-IR-ARENA-SLOTS throw then ;

\ ---- allocation scopes --------------------------------------------------------
\ A caller that needs SEVERAL arenas for one object needs all of them or none.
\ IR-BUILD:NEW-BUILDER takes seventeen and publishes the builder's generation
\ only after the last one, so until this record existed a refusal part-way
\ through left every arena already taken with no handle anybody could ABORT: the
\ builder did not exist, so nothing could name them, and SWEEP only reclaims a
\ slot when the whole owning context tears down. Three builders and a fourth
\ that ran out took thirteen slots and held them for the rest of the context.
\
\ THE RECORD BELONGS HERE, WHERE THE SLOT IS TAKEN. Those seventeen tables are
\ made by seven other packages, and each of them takes two or three arenas of
\ its own and hands them back together. A record kept by the CALLER can only see
\ the arenas that were handed back; the ones a constructor took before throwing
\ half way through its own group are invisible to it - which is exactly the
\ residue a caller-side count left behind when this was first written. NEW is
\ the one word that takes a slot, so NEW is the one place that can see all of
\ them.
\
\ SCOPES DO NOT NEST. One is open at a time and a second is refused by name; the
\ one consumer builds one module at a time and nothing else allocates while it
\ does. A scope releases newest first, which is the reverse of the order the
\ arenas were taken.
variable SCOPE-OPEN
variable SCOPE-N
create SCOPE-SLOTS SLOT-MAX cells allot
0 SCOPE-OPEN !
0 SCOPE-N !

: SCOPE-CLOSE ( -- )
   0 SCOPE-N !
   0 SCOPE-OPEN ! ;

: SCOPE-LIVE-CK ( -- )
   SCOPE-OPEN @ 0= if E-IR-ARENA-STATE throw then ;

\ Note that a slot was taken while a scope is open. Called by NEW after the
\ generation is installed, which is the instant the slot becomes taken: a NEW
\ that throws before that line leaves the slot free and has nothing to record.
: SCOPE-RECORD ( n -- ) {: slot:n :}
   SCOPE-OPEN @ 0= if exit then
   SCOPE-N @ SLOT-MAX >= if E-IR-ARENA-STATE throw then
   slot SCOPE-N @ cells SCOPE-SLOTS + !
   SCOPE-N @ 1+ SCOPE-N ! ;

: SCOPE-POP ( -- )
   SCOPE-N @ 1- {: k:n :}
   0 k cells SCOPE-SLOTS + @ AGEN!
   k SCOPE-N ! ;

public

\ Open the scope this package records new arenas into. The caller releases or
\ commits it on every path out of the work it wraps.
: SCOPE-BEGIN ( -- )
   SCOPE-OPEN @ 0<> if E-IR-ARENA-STATE throw then
   0 SCOPE-N !
   1 SCOPE-OPEN ! ;

\ Retire every arena taken since SCOPE-BEGIN, newest first, and close the scope.
\ Retiring is what ABORT does to one arena - the registry slot is freed and
\ every index it minted goes stale - reached here by slot because a caller that
\ never received a handle cannot present one.
: SCOPE-RELEASE ( -- )
   SCOPE-LIVE-CK
   begin SCOPE-N @ 0 > while SCOPE-POP repeat
   SCOPE-CLOSE ;

\ Close the scope and keep every arena it recorded: the object they were taken
\ for exists now and owns them.
: SCOPE-COMMIT ( -- )
   SCOPE-LIVE-CK
   SCOPE-CLOSE ;

\ Create an arena owned by ctx with a committed ceiling of n cells; production
\ callers pass their table's named ceiling constant. The data span is taken
\ before any slot field is written and the generation is installed last, so a
\ scratch or serial throw leaves no half-installed slot.
: NEW ( IR-CTX:ctx n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx ceil:n :}
   ceil CEIL-OK
   SWEEP
   FREE-SLOT {: slot:n :}
   TAKE-AGEN {: g:n :}
   ceil SEED-CELLS min {: cap0:n :}
   c cap0 CDIGEST:SLOT-BYTES * IR-CTX:SCRATCH-TAKE drop {: base:ptr :}
   c IR-CTX:SERIAL slot AOWNER!
   base slot ADATA-FIELD !
   0 slot ACOUNT!
   cap0 slot ACAP!
   ceil slot ACEIL!
   ST-LIVE slot ASTATE!
   g slot AGEN!
   slot SCOPE-RECORD
   g MINT-ARENA ;

\ ---- append ------------------------------------------------------------------
\ A ROW IS SEVERAL PUSHES AND A PUSH CAN FAIL. Growth takes a span from the
\ owning context's mapping, and that mapping runs out - E-IR-CTX-SCRATCH is a
\ failure real compilations reach, which is why context.f's MAP-BYTES has been
\ doubled three times. A caller that writes a six-cell row as six PUSH calls
\ therefore has five places where the row can stop half written, and the tables
\ built on this arena read their rows by position: five cells of a six-cell row
\ leaves a count no row width divides, and every later read of that table fails
\ its shape recheck. The registry is not damaged data, it is unreadable.
\
\ RESERVE IS HOW A ROW BECOMES ONE COMMIT. It performs the whole row's growth up
\ front, so the pushes that follow have nothing left to allocate and cannot
\ fail. Every caller that appends more than one cell - to more than one arena,
\ if its row spans several - reserves all of them BEFORE writing the first cell,
\ and reserves AFTER its own capacity check so a table's own named ceiling error
\ still comes first.
\
\ ROLLBACK IS NOT THE ALTERNATIVE. Context scratch is a monotonic bump cursor
\ with no free, so restoring the cell count after a failed grow would still
\ leave the doubled span spent - a row that "did not happen" would consume the
\ mapping anyway, and a retry loop would exhaust it. The tree already deleted an
\ arena rollback for want of a consumer (lib/errors.f, -6654, 2026-08-05).
\
\ Append one cell and mint its nominal index. The ctx is the allocator for
\ growth and must be the arena's owner; a foreign context is a named reject.
: PUSH ( IR-CTX:ctx IR-ARENA:arena n -- IR-ARENA:cell-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena v:n :}
   a LIVE-SLOT {: slot:n :}
   c slot OWN-CHECK
   c slot slot ACOUNT@ 1+ GROW-TO
   slot ACOUNT@ {: at:n :}
   v slot ADATA-FIELD @ at CDIGEST:SLOT!
   at 1+ slot ACOUNT!
   slot AGEN@ at PACK MINT-IDX ;

\ Make the capacity for k more cells real, so the next k PUSH calls to this
\ arena allocate nothing and therefore cannot fail: after this word returns,
\ every check a PUSH makes - liveness, ownership, capacity - has already been
\ made here against state no push of this row can change.
\
\ The ceiling refusal and the scratch take are the only failures, and both
\ happen before any cell of the row is written. A reservation larger than the
\ arena's committed ceiling is E-IR-ARENA-FULL, exactly as the append that
\ overran it would have been.
: RESERVE ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena k:n :}
   k 0 < k LOCAL-MAX > or if E-IR-ARENA-CEIL throw then
   a LIVE-SLOT {: slot:n :}
   c slot OWN-CHECK
   c slot slot ACOUNT@ k + GROW-TO ;

\ ---- live readers ------------------------------------------------------------
: PEEK ( IR-ARENA:arena IR-ARENA:cell-id -- n )
   {: a:IR-ARENA:arena x:IR-ARENA:cell-id :}
   a LIVE-SLOT {: slot:n :}
   slot x IDX>N IDX-AT
   slot swap CELL-AT ;

: USED ( IR-ARENA:arena -- n )
   LIVE-SLOT ACOUNT@ ;

\ Mint the nominal index of an existing ordinal - the one sanctioned raw-to-
\ index crossing, validating bounds and stamping this arena's own generation.
: NTH ( IR-ARENA:arena n -- IR-ARENA:cell-id )
   swap LIVE-SLOT swap NTH-RAW ;

\ The one-way ordinal projection, for embedding an arena position into an
\ IR-ID pack; no public word re-mints an ordinal into an index.
: ORD ( IR-ARENA:cell-id -- n )
   IDX>N PACK-LOCAL ;

: LIVE? ( IR-ARENA:arena -- bool )
   ARENA>N FIND-A {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot AOWNER@ IR-CTX:SERIAL-LIVE?
   slot ASTATE@ ST-LIVE = and ;

\ ---- freeze and abort --------------------------------------------------------
\ FREEZE consumes the builder into an immutable view over the same storage:
\ published indices stay valid through the view, and every mutation word left
\ holding the old builder handle rejects with E-IR-ARENA-FROZEN.
: FREEZE ( IR-ARENA:arena -- IR-ARENA:view )
   LIVE-SLOT {: slot:n :}
   ST-FROZEN slot ASTATE!
   slot AGEN@ MINT-VIEW ;

\ ABORT consumes the builder without publishing: the registry slot is retired
\ at once, so the handle and every index it minted are stale; the
\ abandoned spans die with the owning context's mapping.
: ABORT ( IR-ARENA:arena -- )
   LIVE-SLOT {: slot:n :}
   0 slot AGEN! ;

\ RETIRE is ABORT's other half, for an arena that was published rather than
\ abandoned: the registry slot is freed and every index the view minted goes
\ stale, exactly as ABORT does, but it is reached through the FROZEN handle
\ because that is the only handle a published arena has left.
\
\ WHY A FROZEN ARENA MAY BE GIVEN BACK AT ALL. A frozen arena is immutable, not
\ immortal. It stays readable for as long as somebody can still read it, and
\ when a later pass has copied everything it needed out of a module, nothing
\ can. Until this word existed the only way a published slot came back was
\ SWEEP, which waits for the whole owning CONTEXT to tear down - so a chain of
\ passes that each supersede the last held every intermediate module's slots to
\ the end of the compilation, and peak pressure was the number of modules ever
\ BUILT rather than the number live at once.
\
\ NOTHING HERE DECIDES THAT THE ARENA IS DEAD, and that is the point. This word
\ frees the slot; the generation seal is what makes a mistake loud. A view or an
\ index minted before the retire carries the old generation, and IDX-AT compares
\ it against the slot's current one, so a read through a retired arena is
\ E-IR-ARENA-OWNER and not stale data - the same refusal a handle from another
\ context gets, for the same reason and through the same comparison.
: RETIRE ( IR-ARENA:view -- )
   FROZEN-SLOT {: slot:n :}
   0 slot AGEN! ;

\ ---- frozen readers ----------------------------------------------------------
: AT ( IR-ARENA:view IR-ARENA:cell-id -- n )
   {: f:IR-ARENA:view x:IR-ARENA:cell-id :}
   f FROZEN-SLOT {: slot:n :}
   slot x IDX>N IDX-AT
   slot swap CELL-AT ;

: SIZE ( IR-ARENA:view -- n )
   FROZEN-SLOT ACOUNT@ ;

: FROZEN-NTH ( IR-ARENA:view n -- IR-ARENA:cell-id )
   swap FROZEN-SLOT swap NTH-RAW ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
