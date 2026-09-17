\ arena.f - the one generic compiler IR arena: disposable append-only typed
\ cell storage with geometric growth under a committed ceiling.
\
\ docs/compiler-ir-design.md sections 6.2-6.3 and the section 10.2 arena
\ obligations. Every dialect table (function, block, operation, value, type,
\ attribute, span) is an instance of this arena, and this package's private
\ CAST: window is the single raw index conversion authority the design demands
\ (design line 341): no dialect repeats trusted casts, no raw converter and no
\ pointer is public, and readers validate their nominal arena or view.
\
\ OWNERSHIP SHAPE. An arena is created against a live IR-CTX context and both
\ its DESCRIPTOR and its cells are spans bump-allocated from that context's
\ scratch region by IR-CTX:SCRATCH-OFFSET. Growth takes a doubled span and
\ copies; the abandoned smaller span stays in the region and dies with the
\ context's mark - the accepted arena discipline, bounded by the committed
\ ceiling. Whole-range release is the owning context's teardown, which moves the
\ region cursor back over every span this arena was given, its descriptor
\ included: this file adds no MEM:RELEASE-BYTES call site, the same constraint
\ as context.f. FREEZE consumes the builder handle into an immutable view;
\ ABORT consumes it without publishing.
\
\ A HANDLE IS AN OFFSET AND THE DESCRIPTOR IS THE ROW. There is no registry and
\ no generation counter. An arena's whole state - count, capacity, ceiling,
\ owner, data span - is a record inside the region, and the handle is the region
\ offset of that record packed with the state it names and the release epoch it
\ was born in. The record's first cell holds exactly that packed value, so every
\ resolution and every scoped read is one indexed load and one compare against
\ what the caller presented, and the ways it can be wrong are told apart by
\ TOKEN-REFUSE, off the read path. A view and a reader are the same value in the
\ same layout, which is why opening one costs a validation and nothing else.
\
\ WHAT MAKES A STALE HANDLE LOUD, case by case, because the region hands the
\ same bytes out again:
\ - ABORT and RETIRE zero the descriptor's identity cell, so every handle, view,
\   index and reader that named it compares against zero: E-IR-ARENA-STALE.
\ - FREEZE rewrites that cell with the frozen state, so the live handle it
\   consumed compares unequal: E-IR-ARENA-FROZEN, and a view presented where a
\   live arena belongs is E-IR-ARENA-STATE.
\ - The owning context's teardown zeroes the identity cell of every descriptor
\   that context owns, BEFORE the region cursor moves back (CHAIN-DROP below),
\   so a handle or a reader that outlived its context meets a dead record rather
\   than live bytes the next compilation is about to be handed.
\ - A LATER arena whose descriptor lands on those same bytes is not mistaken for
\   the dead one: reuse takes a release, every release bumps IR-CTX:EPOCH, and
\   the epoch is inside the value being compared.
\ - A span cannot outlive its extent while its owner is alive, because IR-CTX
\   refuses a scratch take by any context a deeper live one encloses
\   (E-IR-CTX-NESTED): every span a context holds lies above its own mark, and
\   only that context's release takes it back.
\
\ NO READ ASKS THE REGION'S BOUND, AND NONE NEEDS TO. The three lines above are
\ what a bound check would be for: bytes the region has taken back are already a
\ zeroed identity, bytes handed out again carry a later epoch, and a span cannot
\ be released while its owner lives. A test of the cursor per read would restate
\ them from the other side and cost a call into IR-CTX on the hottest path in
\ the compiler - 1.7 percent of a trivial tier-1 definition when it was written
\ that way and measured.
\
\ WHAT IS NOT PROVED, stated because this is reused storage: a dead record whose
\ bytes are overwritten by ordinary table cells that happen to equal the exact
\ identity value would pass the compare. The epoch sits in the identity's high
\ bits and a released record is zeroed before its bytes can be handed out, so
\ what is left is a coincidence over one cell, not a class of reads.
\
\ APPEND AT THE CEILING. E-IR-ARENA-FULL is thrown before any mutation, so a
\ full arena stays usable: its contents and indices remain valid and
\ FREEZE still publishes them. Exhaustion never kills the arena.
\
\ CONCURRENCY. The chain of live descriptors is one process-wide list under the
\ current single-task compilation discipline, exactly like the IR-CTX registry
\ it is built on.

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
NEWTYPE reader 0

private

CAST: MINT-ARENA ( n -- IR-ARENA:arena )
CAST: ARENA>N ( IR-ARENA:arena -- n )
CAST: MINT-VIEW ( n -- IR-ARENA:view )
CAST: VIEW>N ( IR-ARENA:view -- n )
CAST: MINT-IDX ( n -- IR-ARENA:cell-id )
CAST: IDX>N ( IR-ARENA:cell-id -- n )
CAST: MINT-READER ( n -- IR-ARENA:reader )
CAST: READER>N ( IR-ARENA:reader -- n )

\ ---- capacities and packing --------------------------------------------------
8 constant SEED-CELLS                \ first data span, before any doubling
32 constant LOCAL-BITS
$FFFFFFFF constant LOCAL-MAX         \ ordinal range inside one arena
LOCAL-MAX constant CEIL-MAX          \ a ceiling may commit the full ordinal range
1 constant ST-LIVE
2 constant ST-FROZEN

\ An identity packs (epoch, state, offset): the descriptor's region offset in
\ the low bits, the state above it, and the region release epoch the descriptor
\ was born in above that. The offset width is the region's own, read from the
\ region that defines it, so the two can never disagree.
IR-CTX:SCRATCH-OFFSET-BITS constant OFF-BITS
1 OFF-BITS lshift 1- constant OFF-MASK
2 constant STATE-BITS
3 constant STATE-MASK
OFF-BITS constant STATE-SHIFT
OFF-BITS STATE-BITS + constant EPOCH-SHIFT

: TOK ( n n n -- n ) {: g:n st:n off:n :}
   g EPOCH-SHIFT lshift st STATE-SHIFT lshift or off or ;

: TOK-OFF ( n -- n )
   OFF-MASK and ;

: TOK-STATE ( n -- n )
   STATE-SHIFT rshift STATE-MASK and ;

: TOK-EPOCH ( n -- n )
   EPOCH-SHIFT rshift ;

\ An index packs (offset << 32 | ordinal), mirroring IR-ID PACK-N, so every
\ minted value names the arena that minted it. The arena is named by where its
\ descriptor lives, which is the same thing the handle beside it already names.
: PACK ( n n -- n )
   {: off:n l:n :}
   off LOCAL-BITS lshift l or ;

: PACK-OWNER ( n -- n )
   LOCAL-BITS rshift ;

: PACK-LOCAL ( n -- n )
   LOCAL-MAX and ;

\ ---- the descriptor ----------------------------------------------------------
\ One record per arena, in the region, in byte offsets from its own start. The
\ three the read path touches come first so a read stays inside one cache line.
0 constant D-TOK                     \ the identity: epoch, state, own offset
1 cells constant D-CNT               \ readable cells
2 cells constant D-DATA              \ region offset of the data span
3 cells constant D-CAP               \ cells the data span holds
4 cells constant D-CEIL              \ committed ceiling
5 cells constant D-SIDE              \ the one observer's cell
6 cells constant D-OWNER             \ the owning context's serial
7 cells constant D-PREV              \ the previous descriptor of any context
8 constant DESC-CELLS
DESC-CELLS cells constant DESC-BYTES

\ ---- where the region is -----------------------------------------------------
\ This package resolves an offset to an address once per arena read - millions
\ of times in one compilation - so it keeps the base itself and IR-CTX pushes it
\ here when the mapping appears and when it goes. THE CLOSED STATE IS A DEAD
\ RECORD AND NOT A NULL: a mask of zero sends every offset to a record that is
\ all zeros, so a handle presented when there is no region reads a dead identity
\ and is refused by name, where a null base would have been a fault. That record
\ is also what a captured image carries, since a capture is what closes the
\ region.
\
\ THE BASE IS A BARE MARKED CELL AND NOT A PTR-VARIABLE, which is a measured
\ exception to the rule in docs/forth.md. A PTR-VARIABLE is create-plus-does>,
\ so reading one is a call into the does> body and then a call to ptr-field, and
\ this cell is read on every arena read: with the definer, the does> body alone
\ was 8.9 percent of a trivial tier-1 definition's samples, half of them from
\ RD@. The cell is marked with ptr-cell-mark exactly as the definer marks its
\ own, so it relocates with the image the same way, and the reads below spell
\ out the one ptr-field the definer would have reached anyway.
here CELL 1- and CELL swap - CELL 1- and allot
create DEAD-DESC DESC-BYTES allot
create RBASE-CELL here ptr-cell-mark 0 ,
variable RMASK

: RBASE@ ( -- ptr u8 )
   RBASE-CELL 0 ptr-field @ ;

: RBASE! ( ptr u8 -- )
   RBASE-CELL 0 ptr-field ! ;

\ An identity names its own descriptor, and this is the whole of the lookup: the
\ mask is the region's when there is a region and zero when there is not.
: DESC ( n -- ptr u8 )
   RMASK @ and RBASE@ + ;

: D@ ( ptr u8 n -- n )
   + CELL-VIEW @ ;

: D! ( n ptr u8 n -- )
   + CELL-VIEW ! ;

: SIDE-CELL ( ptr u8 -- ptr ptr u8 )
   D-SIDE + 0 ptr-field ;

\ The dead record answers every field as zero, which no identity can equal, so
\ it is cleared here rather than assumed: dictionary storage does not arrive
\ zeroed, and this record is what stands between a handle presented with no
\ region and a read of whatever happened to be there.
: DEAD-CLEAR ( -- )
   DESC-CELLS 0 ?do
      0 DEAD-DESC BYTE-VIEW i cells D!
   loop ;

: DEAD-BASE ( -- )
   DEAD-DESC BYTE-VIEW RBASE!
   0 RMASK ! ;
DEAD-CLEAR
DEAD-BASE

: REBASE ( ptr u8 -- ) {: b:ptr :}
   b NULL-PTR = if DEAD-BASE exit then
   b RBASE!
   OFF-MASK RMASK ! ;

: INSTALL-REBASE ( -- )
   [: REBASE ;] IR-CTX:REGION-REBASE! ;
INSTALL-REBASE

\ The data span's address. The base is loaded again rather than carried, because
\ a load of a cell this package wrote is cheaper than keeping it live across the
\ checks in between.
: DATA-AT ( ptr u8 -- ptr u8 ) {: d:ptr :}
   RBASE-CELL 0 ptr-field @ d D-DATA + CELL-VIEW @ + ;

\ ---- the chain of live descriptors -------------------------------------------
\ A context's teardown has to end its arenas BEFORE the region cursor moves back
\ over them: a reader resolves once and has no later probe, so a descriptor left
\ standing would read live over storage the next compilation is about to be
\ handed. The descriptors of every live arena are therefore threaded onto one
\ list, newest first, and teardown walks it and zeroes the identity of each
\ record this serial owns.
\
\ EVERY NODE ON THE LIST BELONGS TO A LIVE CONTEXT, which is what makes walking
\ it safe: a node is unlinked when its owner dies, so no walk ever reads a
\ record in bytes the region has taken back. ABORT and RETIRE leave their node
\ linked and merely dead; its owner still holds the storage and the teardown
\ still unlinks it.
variable CHAIN-HEAD
0 CHAIN-HEAD !
variable WALK-AT
variable WALK-PREV

: ROW-RETIRE ( ptr u8 -- )
   0 swap D-TOK D! ;

: CHAIN-PUSH ( n -- ) {: off:n :}
   CHAIN-HEAD @ off DESC D-PREV D!
   off CHAIN-HEAD ! ;

: CHAIN-UNLINK ( n -- ) {: nxt:n :}
   WALK-PREV @ 0= if nxt CHAIN-HEAD ! exit then
   nxt WALK-PREV @ DESC D-PREV D! ;

\ Retire every arena this serial owns, called by the owning context from its own
\ teardown. Zero is not a node: the region's first allotment is always a context
\ header, so no descriptor ever lives at offset zero, and NEW refuses an offset
\ that claims to.
: CHAIN-DROP ( n -- ) {: serial:n :}
   0 WALK-PREV !
   CHAIN-HEAD @ WALK-AT !
   begin WALK-AT @ 0<> while
      WALK-AT @ DESC {: d:ptr :}
      d D-PREV D@ {: nxt:n :}
      d D-OWNER D@ serial = if
         d ROW-RETIRE
         nxt CHAIN-UNLINK
      else
         WALK-AT @ WALK-PREV !
      then
      nxt WALK-AT !
   repeat ;

: INSTALL-RETIRE ( -- )
   [: CHAIN-DROP ;] IR-CTX:RETIRE-CHILDREN! ;
INSTALL-RETIRE

\ ---- handle resolution -------------------------------------------------------
\ The refusal an identity mismatch takes. It is a separate word so the read path
\ is a load and a compare, and it names the four ways a value can stop being the
\ record it was: a record that was given back, one whose bytes are a later
\ arena's, and the two state crossings.
: TOKEN-REFUSE ( n -- ) {: t:n :}
   t DESC D-TOK D@ {: cur:n :}
   cur 0= if E-IR-ARENA-STALE throw then
   cur TOK-EPOCH t TOK-EPOCH <> if E-IR-ARENA-STALE throw then
   t TOK-STATE ST-LIVE = if E-IR-ARENA-FROZEN throw then
   E-IR-ARENA-STATE throw ;

\ THE WARM WORDS SPELL THEIR FIELD READS OUT rather than calling D@ and the
\ token projections above. A resolution happens millions of times in one
\ compilation, and a call frame around an add and a load is most of what such a
\ read costs: the accessors stay for the words that run once per arena.

\ Resolve a builder handle to its descriptor and fail closed on a consumed
\ handle, an ended context and a published arena - in ONE definition, not a
\ chain. The handle names its own record and the record's first cell holds the
\ handle, so the whole resolution is one indexed load and one compare.
\
\ FROZEN-DESC below is this word's twin for published views. THE TWO ASK THE
\ SAME QUESTION AND REFUSE IN THE SAME ORDER, which TOKEN-REFUSE above fixes: a
\ dead or replaced record is reported before any state is. A state test that ran
\ first would answer E-IR-ARENA-FROZEN for a record that now belongs to somebody
\ else, which tells the caller its handle still names something.
: LIVE-DESC ( IR-ARENA:arena -- ptr u8 )
   ARENA>N {: t:n :}
   RBASE-CELL 0 ptr-field @ t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   d ;

: FROZEN-DESC ( IR-ARENA:view -- ptr u8 )
   VIEW>N {: t:n :}
   RBASE-CELL 0 ptr-field @ t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   d ;

\ ---- cell access -------------------------------------------------------------
\ Cells are eight-byte little-endian slots in the current data span, written
\ with the canonical CDIGEST slot words.
\
\ READING THEM IS A NATIVE CELL LOAD ON THIS HOST, AND THE ALIGNMENT IS BY
\ CONSTRUCTION. Every span comes from IR-CTX:SCRATCH-OFFSET, whose cursor only
\ ever advances by aligned steps over a base that is a fresh mapping - so a span
\ address is always a multiple of CDIGEST:SLOT-BYTES and so is every cell in it.
\ That leaves only the host: a canonical slot IS a native cell exactly on an
\ eight-byte little-endian machine. NATIVE-CELLS? asks CDIGEST itself rather
\ than re-deriving the answer - it writes one canonical slot and checks whether
\ the host's own cell load reads it back - so CDIGEST stays the authority, and
\ anywhere it says no, the canonical slot words do the reading.
\
\ The descriptor itself is NOT canonical storage: it is this package's own
\ bookkeeping, written and read through one native cell accessor, exactly as the
\ registry arrays it replaced were.
here CELL 1- and CELL swap - CELL 1- and allot
create NATIVE-PROBE CDIGEST:SLOT-BYTES allot
$0123456789ABCDEF NATIVE-PROBE 0 CDIGEST:SLOT!
NATIVE-PROBE CELL-VIEW @ $0123456789ABCDEF = constant NATIVE-CELLS?

: CELL-AT ( ptr u8 n -- n )
   {: d:ptr k:n :}
   RBASE-CELL 0 ptr-field @ d D-DATA + CELL-VIEW @ +
   NATIVE-CELLS? if k CDIGEST:SLOT-BYTES * + CELL-VIEW @ exit then
   k CDIGEST:SLOT@ ;

\ Validate a raw packed index against one resolved descriptor: minted by this
\ arena, ordinal inside the readable count.
: IDX-AT ( ptr u8 n -- n )
   {: d:ptr raw:n :}
   raw PACK-OWNER d D-TOK + CELL-VIEW @ OFF-MASK and <>
   if E-IR-ARENA-OWNER throw then
   raw PACK-LOCAL
   dup d D-CNT + CELL-VIEW @ >= if E-IR-ARENA-BOUND throw then ;

: ORDINAL-CHECK ( ptr u8 n -- )
   {: d:ptr k:n :}
   k 0 < k d D-CNT + CELL-VIEW @ >= or if E-IR-ARENA-BOUND throw then ;

: NTH-RAW ( ptr u8 n -- IR-ARENA:cell-id )
   {: d:ptr k:n :}
   d k ORDINAL-CHECK
   d D-TOK + CELL-VIEW @ OFF-MASK and k PACK MINT-IDX ;

\ ---- growth ------------------------------------------------------------------
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
\ one scratch take both come before any descriptor field is written, so a
\ refusal mutates nothing; the copy preserves every published ordinal.
\
\ The capacity is rechecked because the series above doubles it: a record
\ claiming a capacity of zero would double to zero forever, so the one state
\ that could turn this into a hang is a named refusal instead. NEW cannot
\ install it - a ceiling below one is refused there and the seed is the smaller
\ of the two - so this is the recheck, not the check.
: GROW-TO ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx d:ptr need:n :}
   need d D-CAP D@ <= if exit then
   d D-CAP D@ 1 < if E-IR-ARENA-STATE throw then
   need d D-CEIL D@ > if E-IR-ARENA-FULL throw then
   d D-CAP D@ need d D-CEIL D@ CAP-FOR {: ncap:n :}
   c ncap CDIGEST:SLOT-BYTES * IR-CTX:SCRATCH-OFFSET {: noff:n :}
   d DATA-AT  noff RBASE@ +  d D-CNT D@ CDIGEST:SLOTS-COPY
   noff d D-DATA D!
   ncap d D-CAP D! ;

: OWN-CHECK ( IR-CTX:ctx ptr u8 -- )
   {: c:IR-CTX:ctx d:ptr :}
   c IR-CTX:SERIAL d D-OWNER + CELL-VIEW @ <>
   if E-IR-ARENA-OWNER throw then ;

\ ---- creation ----------------------------------------------------------------
: CEIL-OK ( n -- )
   dup 1 < over CEIL-MAX > or if E-IR-ARENA-CEIL throw then
   drop ;

\ NO DESCRIPTOR LIVES AT OFFSET ZERO, and the chain reads zero as "no node", so
\ the invariant is checked where the offset is taken rather than argued. It
\ holds because a context allots its own header before anything can ask this
\ package for storage, so the first offset an arena can be given is past it.
: OFF-OK ( n -- n )
   dup 0= if E-IR-ARENA-STATE throw then ;

public

\ Create an arena owned by ctx with a committed ceiling of n cells; production
\ callers pass their table's named ceiling constant. Both spans are taken before
\ any field is written and the identity is installed last, so a scratch throw
\ leaves no half-built record and nothing that names one.
: NEW ( IR-CTX:ctx n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx ceil:n :}
   ceil CEIL-OK
   ceil SEED-CELLS min {: cap0:n :}
   c DESC-BYTES IR-CTX:SCRATCH-OFFSET OFF-OK {: off:n :}
   c cap0 CDIGEST:SLOT-BYTES * IR-CTX:SCRATCH-OFFSET {: data:n :}
   off DESC {: d:ptr :}
   c IR-CTX:SERIAL d D-OWNER D!
   data d D-DATA D!
   0 d D-CNT D!
   cap0 d D-CAP D!
   ceil d D-CEIL D!
   NULL-PTR d SIDE-CELL !
   IR-CTX:EPOCH ST-LIVE off TOK {: t:n :}
   t d D-TOK D!
   off CHAIN-PUSH
   t MINT-ARENA ;

\ ---- append ------------------------------------------------------------------
\ A ROW IS SEVERAL PUSHES AND A PUSH CAN FAIL. Growth takes a span from the
\ owning context's scratch storage, and allocation can fail. A caller that
\ writes a six-cell row as six PUSH calls
\ therefore has five places where the row can stop half written, and the tables
\ built on this arena read their rows by position: five cells of a six-cell row
\ leaves a count no row width divides, and every later read of that table fails
\ its shape recheck. The result is not damaged data, it is unreadable data.
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
\ region anyway, and a retry loop would exhaust it. The tree already deleted an
\ arena rollback for want of a consumer (lib/errors.f, -6654, 2026-08-05).
\
\ Append one cell and mint its nominal index. The ctx is the allocator for
\ growth and must be the arena's owner; a foreign context is a named reject.
: PUSH ( IR-CTX:ctx IR-ARENA:arena n -- IR-ARENA:cell-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena v:n :}
   a LIVE-DESC {: d:ptr :}
   c d OWN-CHECK
   c d d D-CNT + CELL-VIEW @ 1+ GROW-TO
   d D-CNT + CELL-VIEW @ {: at:n :}
   v d DATA-AT at CDIGEST:SLOT!
   at 1+ d D-CNT + CELL-VIEW !
   d D-TOK + CELL-VIEW @ OFF-MASK and at PACK MINT-IDX ;

\ ---- bulk append -------------------------------------------------------------
\ Append `k` cells of `src`, from its ordinal `from`, to the end of `dst`, in
\ one commit. This is RESERVE followed by k PUSH calls and nothing else: the
\ same growth, the same ownership check on the destination, the same resulting
\ count and the same published ordinals. What it does not do is resolve two
\ handles, recheck liveness and ownership and mint an index PER CELL, when the
\ per-cell work is one load and one store - which is what a caller copying a
\ whole table into a fresh one was paying, and is why this word exists.
\
\ THE SOURCE IS READ AND NOT OWNED. A prototype built in one context is copied
\ into arenas of another, so only the destination is own-checked; reading a live
\ arena of another context is what OPEN-LIVE and RD@ already allow.
\
\ The run itself is CDIGEST:SLOTS-COPY, the canonical slot words asked once for
\ the whole span instead of once per cell; the bytes, the order and the result
\ are the loop it replaces.
\
\ The destination's data address is read AFTER the growth, because growth is
\ what moves it.
: APPEND-SPAN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n n -- )
   {: c:IR-CTX:ctx dst:IR-ARENA:arena src:IR-ARENA:arena from:n k:n :}
   k 0 < k LOCAL-MAX > or if E-IR-ARENA-CEIL throw then
   dst LIVE-DESC {: d:ptr :}
   src LIVE-DESC {: s:ptr :}
   c d OWN-CHECK
   s D-CNT D@ {: sc:n :}
   from 0 < from sc > or if E-IR-ARENA-BOUND throw then
   k sc from - > if E-IR-ARENA-BOUND throw then
   k 0= if exit then
   c d  d D-CNT D@ k +  GROW-TO
   d D-CNT D@ {: at:n :}
   s DATA-AT from CDIGEST:SLOT-BYTES * +
   d DATA-AT at CDIGEST:SLOT-BYTES * +
   k CDIGEST:SLOTS-COPY
   at k + d D-CNT D! ;

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
   a LIVE-DESC {: d:ptr :}
   c d OWN-CHECK
   c d d D-CNT D@ k + GROW-TO ;

\ ---- live readers ------------------------------------------------------------
\ Equivalent to NTH followed by PEEK, resolving the live handle once. The
\ resolution, the bound check and the cell load are one definition: a dialect
\ row reader calls this once per FIELD, so every call frame between the handle
\ and the cell is paid per field of per row of the whole IR.
: READ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a LIVE-DESC {: d:ptr :}
   k 0 < k d D-CNT + CELL-VIEW @ >= or if E-IR-ARENA-BOUND throw then
   d k CELL-AT ;

: PEEK ( IR-ARENA:arena IR-ARENA:cell-id -- n )
   {: a:IR-ARENA:arena x:IR-ARENA:cell-id :}
   a LIVE-DESC {: d:ptr :}
   d x IDX>N IDX-AT
   d swap CELL-AT ;

: USED ( IR-ARENA:arena -- n )
   LIVE-DESC D-CNT + CELL-VIEW @ ;

\ Mint the nominal index of an existing ordinal - the one sanctioned raw-to-
\ index crossing, validating bounds and stamping the arena that owns it.
: NTH ( IR-ARENA:arena n -- IR-ARENA:cell-id )
   swap LIVE-DESC swap NTH-RAW ;

\ The one-way ordinal projection, for embedding an arena position into an
\ IR-ID pack; no public word re-mints an ordinal into an index.
: ORD ( IR-ARENA:cell-id -- n )
   IDX>N PACK-LOCAL ;

: LIVE? ( IR-ARENA:arena -- bool )
   ARENA>N {: t:n :}
   t DESC D-TOK + CELL-VIEW @ t = ;

\ ---- freeze and abort --------------------------------------------------------
\ FREEZE consumes the builder into an immutable view over the same storage:
\ published indices stay valid through the view, and every mutation word left
\ holding the old builder handle rejects with E-IR-ARENA-FROZEN. The view keeps
\ the record's BIRTH epoch, because it is the same object under a new state and
\ not a new one.
: FREEZE ( IR-ARENA:arena -- IR-ARENA:view )
   LIVE-DESC {: d:ptr :}
   d D-TOK + CELL-VIEW @ {: t:n :}
   t EPOCH-SHIFT rshift ST-FROZEN t OFF-MASK and TOK {: f:n :}
   f d D-TOK + CELL-VIEW !
   f MINT-VIEW ;

\ ABORT consumes the builder without publishing: the record's identity is zeroed
\ at once, so the handle and every index it minted are stale; the abandoned
\ spans die with the owning context's mark.
: ABORT ( IR-ARENA:arena -- )
   LIVE-DESC ROW-RETIRE ;

\ RETIRE is ABORT's other half, for an arena that was published rather than
\ abandoned: the identity is zeroed and every index the view minted goes stale,
\ exactly as ABORT does, but it is reached through the FROZEN handle because
\ that is the only handle a published arena has left.
\
\ WHY A FROZEN ARENA MAY BE GIVEN BACK AT ALL. A frozen arena is immutable, not
\ immortal. It stays readable for as long as somebody can still read it, and
\ when a later pass has copied everything it needed out of a module, nothing
\ can. Retiring says so, and says it in a way that is loud when it is wrong.
\
\ NOTHING HERE DECIDES THAT THE ARENA IS DEAD, and that is the point. This word
\ ends the record; the zeroed identity is what makes a mistake loud. A view or
\ an index minted before the retire still names that record, and every read
\ through either compares against an identity that is now zero, so a read
\ through a retired arena is E-IR-ARENA-STALE and not stale data.
: RETIRE ( IR-ARENA:view -- )
   FROZEN-DESC ROW-RETIRE ;

\ ---- frozen readers ----------------------------------------------------------
\ Equivalent to FROZEN-NTH followed by AT, resolving the frozen view once, and
\ flat for the same reason READ is.
: FREAD ( IR-ARENA:view n -- n )
   {: f:IR-ARENA:view k:n :}
   f FROZEN-DESC {: d:ptr :}
   k 0 < k d D-CNT + CELL-VIEW @ >= or if E-IR-ARENA-BOUND throw then
   d k CELL-AT ;

: AT ( IR-ARENA:view IR-ARENA:cell-id -- n )
   {: f:IR-ARENA:view x:IR-ARENA:cell-id :}
   f FROZEN-DESC {: d:ptr :}
   d x IDX>N IDX-AT
   d swap CELL-AT ;

: SIZE ( IR-ARENA:view -- n )
   FROZEN-DESC D-CNT + CELL-VIEW @ ;

\ Identity comparison grants no read authority; readers still check liveness.
: VIEW-SAME? ( IR-ARENA:view IR-ARENA:view -- bool )
   {: a:IR-ARENA:view b:IR-ARENA:view :}
   a VIEW>N b VIEW>N = ;

: FROZEN-NTH ( IR-ARENA:view n -- IR-ARENA:cell-id )
   swap FROZEN-DESC swap NTH-RAW ;

\ ---- scoped readers ----------------------------------------------------------
\ A DIALECT ROW READER RESOLVES THE SAME VIEW ONCE PER FIELD. Reading one field
\ costs a row lookup and a cell read, and both are whole resolutions, so walking
\ a row of six fields resolves the same handle twelve times and asks the same
\ record the same questions twelve times. A reader is that resolution done once
\ and kept.
\
\ IT IS NOT A POINTER AND CANNOT BECOME ONE. The token carries the descriptor's
\ region offset, the epoch the record was born in and the state it was opened
\ against - no address and no count. Every read fetches the data offset and the
\ count from the record, and refuses BEFORE touching either unless the record's
\ identity still matches the token. So a reader is exactly as stale-safe as the
\ handle it came from: ABORT, RETIRE and the owning context's teardown zero the
\ identity, FREEZE changes its state, and each of those refuses with the error
\ the handle would have given.
\
\ WHY REVALIDATION AND NOT A SCOPING QUOTATION. A quotation scope would bound
\ the reader's lifetime syntactically and, under the single-task compilation
\ discipline, would be sound. It would also force every caller that walks a row
\ into a nested body, and the dialect readers that need this most read fields
\ from SEVERAL arenas inside one expression - the shape a scope cannot hold.
\ Revalidation costs one load and one compare; the chain of accessors was the
\ expense, never the checking.
\
\ NO READ ASKS THE REGION'S BOUND, and none needs to, for the reasons the file
\ header gives: a record whose bytes the region has taken back was zeroed by its
\ owner's teardown before the cursor moved, and a record born in those bytes
\ afterwards carries a later epoch. Asking the cursor per read would restate
\ that through a call into IR-CTX, which is more than every check here.
: OPEN ( IR-ARENA:view -- IR-ARENA:reader )
   dup FROZEN-DESC drop VIEW>N MINT-READER ;

: OPEN-LIVE ( IR-ARENA:arena -- IR-ARENA:reader )
   dup LIVE-DESC drop ARENA>N MINT-READER ;

\ Read one ordinal. Straight-line by construction: the identity, the bound and
\ the cell are three loads out of one record and no call, which is the whole
\ point of having resolved once. RD-SIZE and RD-FIND below repeat the identity
\ compare rather than calling a shared one, for the same reason.
: RD@ ( IR-ARENA:reader n -- n )
   {: r:IR-ARENA:reader k:n :}
   r READER>N {: t:n :}
   RBASE-CELL 0 ptr-field @ {: base:ptr :}
   base t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   k 0 < k d D-CNT + CELL-VIEW @ >= or if E-IR-ARENA-BOUND throw then
   base d D-DATA + CELL-VIEW @ +
   NATIVE-CELLS? if k CDIGEST:SLOT-BYTES * + CELL-VIEW @ exit then
   k CDIGEST:SLOT@ ;

\ FIND ONE CELL VALUE IN A STRIDED RUN, WITH THE IDENTITY ASKED ONCE. A table
\ that resolves a name by walking one field of every row - IR-SCHEMA:SCAN-NAME
\ is the one this exists for - asked RD@ per row, and RD@ re-reads the identity
\ and the bound and then reaches the data span again, per row, to read a cell it
\ has already proved is in range. The questions are the same here and are asked
\ once: the token exactly as RD@ checks it, then the LAST cell of the run
\ against the readable count, which bounds every cell before it. What the loop
\ then costs is a load and a compare.
\
\ It answers the INDEX of the first match in the stepped sequence, not a cell
\ ordinal, because that is the row a caller means; -1 when the run holds no
\ match, and an empty run is -1 rather than a refusal.
\
\ THE LAST CELL IS BOUNDED WITHOUT MULTIPLYING, because a ceiling may commit the
\ whole ordinal range and two values that each fit in it have a product that
\ does not. Computing first + (count-1) * stride and comparing it would let a
\ stride large enough to wrap answer an index inside the count and then read
\ whatever the loop's own multiply reached. Dividing the room that is left by
\ the number of steps asks the same question - is every cell of the run inside
\ the readable count - on operands that are all inside it.
: RD-FIND ( IR-ARENA:reader n n n n -- n )
   {: r:IR-ARENA:reader first:n stride:n count:n want:n :}
   r READER>N {: t:n :}
   RBASE-CELL 0 ptr-field @ {: base:ptr :}
   base t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   count 0 <= if -1 exit then
   first 0 < stride 1 < or if E-IR-ARENA-BOUND throw then
   d D-CNT + CELL-VIEW @ {: used:n :}
   first used >= if E-IR-ARENA-BOUND throw then
   count 1- {: steps:n :}
   steps 0 > if
      used 1- first - steps / stride < if E-IR-ARENA-BOUND throw then
   then
   base d D-DATA + CELL-VIEW @ + {: dbase:ptr :}
   NATIVE-CELLS? if
      count 0 ?do
         dbase first i stride * + CDIGEST:SLOT-BYTES * + CELL-VIEW @ want =
         if i unloop exit then
      loop
      -1 exit
   then
   count 0 ?do
      dbase first i stride * + CDIGEST:SLOT@ want = if i unloop exit then
   loop
   -1 ;

\ The readable count through the same token, checked the same way; see RD@.
: RD-SIZE ( IR-ARENA:reader -- n )
   READER>N {: t:n :}
   RBASE-CELL 0 ptr-field @ t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   d D-CNT + CELL-VIEW @ ;

\ Frozen dialect readers may retain row facts. Refuse a live token before
\ those facts can be reused, with the usual identity-before-state ordering.
: FROZEN-READER ( IR-ARENA:reader -- IR-ARENA:reader )
   dup RD-SIZE drop
   dup READER>N STATE-SHIFT rshift STATE-MASK and ST-FROZEN <>
   if E-IR-ARENA-STATE throw then ;

\ Exact token identity includes the record, its epoch and the state it was
\ opened in. Equality grants no read authority; RD@ and RD-SIZE still validate
\ each use.
: READER-SAME? ( IR-ARENA:reader IR-ARENA:reader -- bool )
   {: a:IR-ARENA:reader b:IR-ARENA:reader :}
   a READER>N b READER>N = ;

\ THE ONE OBSERVER'S CELL, in the record itself. A package that keeps a private
\ side structure per arena - IR-SYM's bucket index is the one - used to key it
\ by a registry slot and clear it when that slot was reused. There are no slots
\ now: the cell lives in the descriptor, so it is born empty with the arena and
\ dies with it, and reaching it is the same validated read as any other. It
\ grants no arena write access.
: SIDE-FIELD ( IR-ARENA:reader -- ptr ptr u8 )
   dup RD-SIZE drop
   READER>N DESC SIDE-CELL ;

\ Every descriptor belongs to a compilation context and every context ends its
\ own, so with no live context the chain is empty. THIS IS WHERE THAT INVARIANT
\ IS TESTED: a node still on the chain here is an arena that outlived its owner,
\ and it is a named refusal rather than something swept quietly away. The
\ records themselves need no clearing - they live in a region this image does
\ not carry.
: CAPTURE-PREPARE ( -- )
   CHAIN-HEAD @ 0<> if E-IR-ARENA-STATE throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
