\ arena.f - the compiler IR arena: append-only typed cells with geometric growth
\ under a committed ceiling. Every dialect table is an instance, and this
\ package's private CAST: window is the IR's only raw index conversion.
\ Descriptor and cells are spans bump-allocated from the IR-CTX context's scratch
\ region, released only by its teardown. The descriptor chain and the IR-CTX
\ registry are process-wide lists: single-task compilation only.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f

package IR-ARENA
public

NEWTYPE arena
NEWTYPE view
NEWTYPE cell-id
NEWTYPE reader

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

\ An identity packs (epoch, state, offset), the offset lowest; the offset width
\ is read from the region that defines it.
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
\ minted value names the arena that minted it.
: PACK ( n n -- n )
   {: off:n l:n :}
   off LOCAL-BITS lshift l or ;

: PACK-OWNER ( n -- n )
   LOCAL-BITS rshift ;

: PACK-LOCAL ( n -- n )
   LOCAL-MAX and ;

\ ---- the descriptor ----------------------------------------------------------
\ The three fields the read path touches come first, inside one cache line.
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
\ Every arena read resolves an offset, so this package keeps the region's base
\ and IR-CTX pushes it here when the mapping appears and goes. With no region
\ the mask is zero and every offset reaches the all-zero dead record, so a
\ handle is refused by name instead of faulting; a captured image carries that
\ state. Every read of the base is spelled out, not wrapped: a call per read
\ measured 8.9 percent of a trivial tier-1 definition's samples.
align
create DEAD-DESC DESC-BYTES allot
PERSISTED-PTR-VARIABLE RBASE
variable RMASK

: DESC ( n -- ptr u8 )
   RMASK @ and RBASE @ + ;

: D@ ( ptr u8 n -- n )
   + CELL-VIEW @ ;

: D! ( n ptr u8 n -- )
   + CELL-VIEW ! ;

: SIDE-CELL ( ptr u8 -- ptr ptr u8 )
   D-SIDE + 0 ptr-field ;

\ Dictionary storage does not arrive zeroed, and the dead record must answer
\ every field as zero, which no identity can equal.
: DEAD-CLEAR ( -- )
   DESC-CELLS 0 ?do
      0 DEAD-DESC BYTE-VIEW i cells D!
   loop ;

: DEAD-BASE ( -- )
   DEAD-DESC BYTE-VIEW RBASE !
   0 RMASK ! ;
DEAD-CLEAR
DEAD-BASE

: REBASE ( ptr u8 -- ) {: b:ptr :}
   b NULL-PTR = if DEAD-BASE exit then
   b RBASE !
   OFF-MASK RMASK ! ;

: INSTALL-REBASE ( -- )
   [: REBASE ;] IR-CTX:REGION-REBASE! ;
INSTALL-REBASE

\ The base is loaded again, not carried: a load is cheaper than keeping it live
\ across the checks.
: DATA-AT ( ptr u8 -- ptr u8 ) {: d:ptr :}
   RBASE @ d D-DATA + CELL-VIEW @ + ;

\ ---- the chain of live descriptors -------------------------------------------
\ A reader resolves once and has no later probe, so a context's teardown must
\ zero its arenas' identities before the region cursor moves back over them.
\ Every live descriptor is on this list, newest first; a node is unlinked when
\ its owner dies, so a walk never reads bytes the region took back. ABORT and
\ RETIRE leave their node linked and dead until the teardown.
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

\ Called by the owning context's teardown. Zero is not a node: a context header
\ is always the region's first allotment, and NEW refuses offset zero (OFF-OK).
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
\ A handle is its descriptor's region offset packed with the state and the
\ release epoch, and the descriptor's first cell holds that same value, so a
\ read is one load and one compare. A stale handle is loud: ABORT, RETIRE and
\ the context's teardown (before the cursor moves back, CHAIN-DROP) zero that
\ cell (E-IR-ARENA-STALE); FREEZE rewrites it (E-IR-ARENA-FROZEN, and a view
\ where a live arena belongs is E-IR-ARENA-STATE); a later arena on the same
\ bytes carries a later IR-CTX:EPOCH; and IR-CTX refuses a scratch take by a
\ context a deeper live one encloses (E-IR-CTX-NESTED). No read checks the
\ region's bound: those cases cover it, and a cursor test per read measured 1.7
\ percent of a trivial tier-1 definition. Unproved: a dead record overwritten by
\ table cells equal to the exact identity value passes the compare.
\
\ Kept out of line so the read path is a load and a compare.
: TOKEN-REFUSE ( n -- ) {: t:n :}
   t DESC D-TOK D@ {: cur:n :}
   cur 0= if E-IR-ARENA-STALE throw then
   cur TOK-EPOCH t TOK-EPOCH <> if E-IR-ARENA-STALE throw then
   t TOK-STATE ST-LIVE = if E-IR-ARENA-FROZEN throw then
   E-IR-ARENA-STATE throw ;

\ The warm words spell their field reads out rather than calling D@: a call
\ frame around an add and a load is most of a resolution's cost.
\
\ LIVE-DESC and FROZEN-DESC refuse in TOKEN-REFUSE's order: a dead or replaced
\ record before any state, so a record now another arena's never answers
\ E-IR-ARENA-FROZEN.
: LIVE-DESC ( IR-ARENA:arena -- ptr u8 )
   ARENA>N {: t:n :}
   RBASE @ t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   d ;

: FROZEN-DESC ( IR-ARENA:view -- ptr u8 )
   VIEW>N {: t:n :}
   RBASE @ t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   d ;

\ ---- cell access -------------------------------------------------------------
\ Cells are eight-byte little-endian slots written with the CDIGEST slot words.
\ Every span comes from IR-CTX:SCRATCH-OFFSET, whose cursor advances by aligned
\ steps over a fresh mapping, so every cell is slot-aligned; NATIVE-CELLS? asks
\ CDIGEST whether the host's cell load reads a canonical slot, and where it
\ does not the slot words read. The descriptor is not canonical storage: it is
\ read and written by native cell.
align
create NATIVE-PROBE CDIGEST:SLOT-BYTES allot
$0123456789ABCDEF NATIVE-PROBE 0 CDIGEST:SLOT!
NATIVE-PROBE CELL-VIEW @ $0123456789ABCDEF = constant NATIVE-CELLS?

: CELL-AT ( ptr u8 n -- n )
   {: d:ptr k:n :}
   RBASE @ d D-DATA + CELL-VIEW @ +
   NATIVE-CELLS? if k CDIGEST:SLOT-BYTES * + CELL-VIEW @ exit then
   k CDIGEST:SLOT@ ;

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
\ Growth copies into a doubled span and leaves the old one to the context's
\ mark; this file makes no MEM:RELEASE-BYTES call.
\
\ Reserving a whole row at once reaches exactly the capacity cell-at-a-time
\ growth would, so it changes how many spans are taken, never which capacities
\ exist.
: CAP-FOR ( n n n -- n )
   {: cap:n need:n ceil:n :}
   cap
   begin
      dup need <
   while
      2 * ceil min
   repeat ;

\ The ceiling refusal and the scratch take come before any field is written, so
\ a refusal mutates nothing: a full arena stays usable and FREEZE still
\ publishes it. A zero capacity would double to zero forever; NEW cannot install
\ one, and this recheck names it instead of hanging.
: GROW-TO ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx d:ptr need:n :}
   need d D-CAP D@ <= if exit then
   d D-CAP D@ 1 < if E-IR-ARENA-STATE throw then
   need d D-CEIL D@ > if E-IR-ARENA-FULL throw then
   d D-CAP D@ need d D-CEIL D@ CAP-FOR {: ncap:n :}
   c ncap CDIGEST:SLOT-BYTES * IR-CTX:SCRATCH-OFFSET {: noff:n :}
   d DATA-AT  noff RBASE @ +  d D-CNT D@ CDIGEST:SLOTS-COPY
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

\ The chain reads zero as no node; a context allots its own header before any
\ arena, so no descriptor lives at offset zero.
: OFF-OK ( n -- n )
   dup 0= if E-IR-ARENA-STATE throw then ;

public

\ Production callers pass their table's named ceiling. Both spans are taken
\ before any field is written and the identity is installed last, so a scratch
\ throw leaves no half-built record.
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
\ A row is several pushes, and growth can fail: a row stopped half written
\ leaves a count no row width divides, and the table's every later read fails
\ its shape recheck. A caller appending more than one cell, to one arena or
\ several, RESERVEs them all before the first write and after its own capacity
\ check, so its named ceiling error comes first. Rollback cannot replace this:
\ context scratch never frees, so a failed grow's span stays spent.
\
\ ctx is the growth allocator and must be the arena's owner.
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
\ RESERVE then k PUSHes, with the same growth, ownership check, count and
\ ordinals, but one resolution instead of one per cell. Only the destination is
\ own-checked: a prototype built in one context is copied into another's
\ arenas. The destination's data address is read after the growth, which moves
\ it.
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

\ After this returns, every check a PUSH makes has been made against state no
\ push of this row can change, so the next k PUSHes allocate nothing and cannot
\ fail. A reservation past the committed ceiling is E-IR-ARENA-FULL.
: RESERVE ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena k:n :}
   k 0 < k LOCAL-MAX > or if E-IR-ARENA-CEIL throw then
   a LIVE-DESC {: d:ptr :}
   c d OWN-CHECK
   c d d D-CNT D@ k + GROW-TO ;

\ ---- live readers ------------------------------------------------------------
\ NTH then PEEK in one definition: a dialect row reader calls this once per
\ field, so each call frame between handle and cell is paid per field of the IR.
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

\ The one sanctioned raw-to-index crossing.
: NTH ( IR-ARENA:arena n -- IR-ARENA:cell-id )
   swap LIVE-DESC swap NTH-RAW ;

\ No public word re-mints an ordinal into an index.
: ORD ( IR-ARENA:cell-id -- n )
   IDX>N PACK-LOCAL ;

: LIVE? ( IR-ARENA:arena -- bool )
   ARENA>N {: t:n :}
   t DESC D-TOK + CELL-VIEW @ t = ;

\ ---- freeze and abort --------------------------------------------------------
\ FREEZE consumes the builder into an immutable view; ABORT consumes it without
\ publishing. Published indices stay valid through the view; the old builder
\ handle is E-IR-ARENA-FROZEN. The view keeps the record's birth epoch: it is
\ the same object under a new state.
: FREEZE ( IR-ARENA:arena -- IR-ARENA:view )
   LIVE-DESC {: d:ptr :}
   d D-TOK + CELL-VIEW @ {: t:n :}
   t EPOCH-SHIFT rshift ST-FROZEN t OFF-MASK and TOK {: f:n :}
   f d D-TOK + CELL-VIEW !
   f MINT-VIEW ;

: ABORT ( IR-ARENA:arena -- )
   LIVE-DESC ROW-RETIRE ;

\ ABORT for a published arena, reached through the frozen handle, the only one
\ it has left. A later read through a view or index minted before is
\ E-IR-ARENA-STALE, never stale data.
: RETIRE ( IR-ARENA:view -- )
   FROZEN-DESC ROW-RETIRE ;

\ ---- frozen readers ----------------------------------------------------------
\ FROZEN-NTH then AT in one definition, flat for the same reason as READ.
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

\ Identity comparison grants no read authority; readers still check liveness.
: VIEW-SAME? ( IR-ARENA:view IR-ARENA:view -- bool )
   {: a:IR-ARENA:view b:IR-ARENA:view :}
   a VIEW>N b VIEW>N = ;

: FROZEN-NTH ( IR-ARENA:view n -- IR-ARENA:cell-id )
   swap FROZEN-DESC swap NTH-RAW ;

\ ---- scoped readers ----------------------------------------------------------
\ A reader is a view's resolution done once and kept, since a dialect row
\ reader reads several fields per row. It is no pointer: the token carries the
\ descriptor's offset, birth epoch and opened state, and every read refuses
\ unless the record's identity still matches, with the handle's error. A scoping
\ quotation cannot replace it: the dialect readers read fields from several
\ arenas inside one expression.
: OPEN ( IR-ARENA:view -- IR-ARENA:reader )
   dup FROZEN-DESC drop VIEW>N MINT-READER ;

: OPEN-LIVE ( IR-ARENA:arena -- IR-ARENA:reader )
   dup LIVE-DESC drop ARENA>N MINT-READER ;

\ Three loads out of one record and no call; RD-SIZE and RD-FIND repeat the
\ identity compare inline for the same reason.
: RD@ ( IR-ARENA:reader n -- n )
   {: r:IR-ARENA:reader k:n :}
   r READER>N {: t:n :}
   RBASE @ {: base:ptr :}
   base t RMASK @ and + {: d:ptr :}
   d D-TOK + CELL-VIEW @ t <> if t TOKEN-REFUSE then
   k 0 < k d D-CNT + CELL-VIEW @ >= or if E-IR-ARENA-BOUND throw then
   base d D-DATA + CELL-VIEW @ +
   NATIVE-CELLS? if k CDIGEST:SLOT-BYTES * + CELL-VIEW @ exit then
   k CDIGEST:SLOT@ ;

\ The token and the run's last cell are checked once, so the loop is a load and
\ a compare (IR-SCHEMA:SCAN-NAME). Answers the index of the first match in the
\ stepped sequence, -1 for none or an empty run. The last cell is bounded by
\ dividing the room left by the steps, not by multiplying: a ceiling may commit
\ the whole ordinal range, and first + (count-1) * stride can wrap.
: RD-FIND ( IR-ARENA:reader n n n n -- n )
   {: r:IR-ARENA:reader first:n stride:n count:n want:n :}
   r READER>N {: t:n :}
   RBASE @ {: base:ptr :}
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

: RD-SIZE ( IR-ARENA:reader -- n )
   READER>N {: t:n :}
   RBASE @ t RMASK @ and + {: d:ptr :}
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

\ A package's per-arena side structure (IR-SYM's bucket index) lives in this
\ cell, born empty with the arena and dying with it. It grants no write access.
: SIDE-FIELD ( IR-ARENA:reader -- ptr ptr u8 )
   dup RD-SIZE drop
   READER>N DESC SIDE-CELL ;

\ With no live context the chain is empty: a node left here is an arena that
\ outlived its owner, and it is refused by name. The records need no clearing:
\ the image does not carry their region.
: CAPTURE-PREPARE ( -- )
   CHAIN-HEAD @ 0<> if E-IR-ARENA-STATE throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
