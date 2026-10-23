\ context.f - the owned compiler context: one explicit owner per compilation.
\
\ Every compilation gets exactly one
\ context, and the context is the single owner of the compilation's target and
\ numerical policy, its module serial allocation, and its scratch memory. It
\ stores the validated binding in its own transient storage.
\
\ THE SCRATCH IS ONE REGION WITH A MARK AND A RELEASE, which is Forth's HERE
\ and ALLOT applied to a transient extent. One mapping serves the whole process;
\ SCRATCH-TAKE bumps its cursor and hands back a span, entering a context takes
\ a MARK of the cursor, and leaving one releases the cursor back to that mark.
\ A context's own header is the first thing it allots from the region, so a
\ compilation costs no mapping of its own on either path out. Handles pack a
\ generation and registry slot so direct lookup can reject stale handles before
\ touching their mappings. The registry stores complete handles; retirement
\ clears the row.
\
\ NOTHING IS FREED BETWEEN A MARK AND ITS RELEASE. Every span a context takes
\ stays at the same address, holding the same bytes, until that context leaves;
\ the release is the only thing that moves the cursor back, and it moves it back
\ over every span the context took at once. So no read inside a context can
\ reach storage that has been handed to anything else.
\
\ A SPAN IS TAKEN AS AN OFFSET, because that is the form a record living inside
\ the region may hold: a pointer names this process's mapping and a captured
\ image must carry none, while an offset names a position. SCRATCH-OFFSET
\ answers one, and EPOCH is the release counter a child stamps into a record so
\ an offset reused by the next compilation cannot pass for the one it replaced.
\ ONLY THE INNERMOST CONTEXT MAY TAKE, which is what keeps an offset's extent
\ the taker's own; TOP-CK is where that is refused.
\
\ CONTEXT STATE. The counters, mark and validated binding live in a typed
\ header in the scratch region. CAPTURE-PREPARE releases the entire mapping.
\
\ CONCURRENCY. The generation counter is atomic, so serials stay unique if
\ tasks race. The registry itself is one process-wide stack and assumes the
\ current single-task compilation discipline, like the digest state it builds
\ on (src/compiler/digest.f header); concurrent compilation needs per-task
\ registries first.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/memory.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/lib/image-lifecycle.f
require src/compiler/workspace.f

package IR-CTX
public

NEWTYPE ctx

private

CAST: MINT-CTX ( n -- IR-CTX:ctx )
CAST: CTX>N ( IR-CTX:ctx -- n )

\ ---- capacities and layout ---------------------------------------------------
$7FFFFFFF constant SERIAL-CEILING    \ production per-context module ceiling; the
                                     \ full IR-ID module serial range
$7FFFFFFF constant GEN-MAX           \ context generation ceiling
64 constant DEPTH-MAX                \ live + retired registry slots
6 constant SLOT-BITS
DEPTH-MAX 1- constant SLOT-MASK
\ THE REGION IS RESERVED, NOT COMMITTED. It is one anonymous mapping whose
\ pages cost nothing until a compilation touches them, so the reservation is
\ sized to dominate the measured high-water of the whole engine self-build
\ (2,885,144 bytes over every definition src/ contains, 2026-09-16) with room
\ for a corpus far larger, and the cursor running past it is a named refusal.
\ The size is stated as a WIDTH because that is what a child packing an offset
\ beside anything else has to know, and deriving the bytes from it is how the
\ two can never disagree.
26 constant OFFSET-BITS               \ the width of a region offset
1 OFFSET-BITS lshift constant REGION-BYTES   \ 64 MiB of reserved scratch address space
$7FFFFFFFF constant EPOCH-MAX         \ region release epoch ceiling

STRUCTURE header DERIVE addr
   FIELD minted n
   FIELD ceil n
   FIELD used n
   FIELD mark n
   FIELD binding CBIND:binding
;STRUCTURE

CAST: HEADER-PTR ( ptr u8 -- ptr header )

\ ---- registry storage --------------------------------------------------------
align
variable GEN-CELL
0 GEN-CELL !
variable DEPTH
0 DEPTH !
DEPTH-MAX CWORK-SPAN HANDLES n
\ A slot's base is the address of the header the context lives at, so the table
\ is a declared table of pointers and each slot is read as one.
DEPTH-MAX CWORK-BUFFER BASES ptr u8
DEPTH-MAX CWORK-SPAN ENTERED n   \ scope depth -> the slot that scope took
0 CWORK-VARIABLE ENTERED-N n

: HANDLE! ( n n -- )
   cells HANDLES + ! ;

: CNT-OK ( n -- n )
   dup 0 < if E-IR-CTX-STATE throw then ;

\ ---- the scratch region ------------------------------------------------------
\ One mapping for the process, one cursor into it. The mapping is taken on the
\ first take and given back only at capture, because a region whose pages are
\ already faulted in is the whole saving: a compilation that mapped its own
\ storage paid the kernel for the same pages again per definition.
align
PTR-VARIABLE REGION-BASE
NULL-PTR REGION-BASE !
variable REGION-HERE
0 REGION-HERE !

: REGION-ALLOC-LEN ( -- NUM:alloc-byte-len )
   REGION-BYTES MEM:BYTES-ALLOC-LEN ;

\ ---- the release epoch -------------------------------------------------------
\ THE EPOCH COUNTS RELEASES, and that is the whole of it. A release hands the
\ bytes above a mark to whatever compiles next, so an offset a child stored
\ names one object before the release and another after it. A child that stamps
\ this counter into the identity cell it keeps AT that offset can tell the two
\ apart, because the stamp it reads back is the one the object was born with.
\
\ IT IS NEVER COMPARED AGAINST ITSELF. The test a child makes is "the stamp in
\ my reference equals the stamp in the record it names", not "my stamp is the
\ current epoch": a session-lived object is born once and read for the whole
\ load, so its epoch is older than the counter by every definition since, and a
\ comparison against the counter would refuse the one thing the session exists
\ to hold.
variable REGION-EPOCH
0 REGION-EPOCH !

: EPOCH-BUMP ( -- )
   REGION-EPOCH @ dup EPOCH-MAX >= if E-IR-CTX-SERIALS throw then
   1+ REGION-EPOCH ! ;

\ ---- who is told where the region is -----------------------------------------
\ A child that resolves offsets to addresses reads the base once per read of
\ its own - millions of times in one compilation - so it keeps the base itself
\ and this package PUSHES it when the mapping appears and when it goes. A call
\ into here per read would cost more than every check such a child makes.
\
\ NULL IS THE CLOSED STATE, and it is announced rather than left to be noticed:
\ a child told the mapping is gone can point its own resolution at storage that
\ answers "nothing is here" instead of at an address that no longer exists.
\
\ ONE VECTOR, INSTALLED ONCE, for the reason RETIRE-CHILDREN! gives below.
defer REBASE-CHILD ( ptr u8 -- )

variable REBASE-SET
0 REBASE-SET !

: KEEP-BASE ( ptr u8 -- )
   drop ;

: REBASE-RESET ( -- )
   [: KEEP-BASE ;] is REBASE-CHILD ;
REBASE-RESET

public

: REGION-REBASE! ( [ ptr u8 -- ] -- )
   REBASE-SET @ 0<> if E-IR-CTX-STATE throw then
   1 REBASE-SET !
   is REBASE-CHILD ;

private

\ Map the region on demand. The cursor is at zero whenever there is no mapping,
\ because the only word that drops the mapping is the capture preparation below
\ and it runs with no context open.
: REGION-OPEN ( -- )
   REGION-BASE @ NULL-PTR = 0= if exit then
   REGION-ALLOC-LEN MEM:ALLOC-BYTES drop REGION-BASE !
   0 REGION-HERE !
   REGION-BASE @ REBASE-CHILD ;

: REGION-CLOSE ( -- )
   REGION-BASE @ NULL-PTR = if exit then
   REGION-BASE @ REGION-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR REGION-BASE !
   0 REGION-HERE !
   NULL-PTR REBASE-CHILD ;

: ALIGN8 ( n -- n ) 7 + 8 / 8 * ;

\ Take `need` aligned bytes from the region cursor and answer WHERE they are.
\ The refusal comes before the cursor moves, so a region that is full stays
\ exactly as usable as it was.
: REGION-ALLOT ( n -- n ) {: need:n :}
   REGION-OPEN
   need ALIGN8 {: step:n :}
   REGION-HERE @ {: off:n :}
   step REGION-BYTES off - > if E-IR-CTX-SCRATCH throw then
   off step + REGION-HERE !
   off ;

\ An offset is a position in the mapping; this is the only place that turns one
\ into an address. It asks no question, because every caller here holds an
\ offset the cursor has already passed: a take answers one and the taker uses it
\ inside the context that took it.
: REGION-AT ( n -- ptr u8 )
   REGION-BASE @ + ;

\ ---- generation serials ------------------------------------------------------
: GEN-NEXT-N ( n -- n )
   dup 0 < over GEN-MAX >= or if E-IR-CTX-SERIALS throw then
   1+ ;

: TRY-GEN ( -- n bool )
   GEN-CELL atomic@ {: current:n :}
   current GEN-NEXT-N {: next:n :}
   current next GEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-GEN ( -- n )
   begin
      TRY-GEN dup 0=
   while
      2drop
   repeat
   drop ;

\ ---- handle resolution -------------------------------------------------------
: PACK-HANDLE ( n n -- n )
   swap SLOT-BITS lshift or ;

\ A handle carries its own registry slot in its low bits and the row holds the
\ complete handle, so a lookup is one indexed load and one compare. The load is
\ spelled here rather than called: SERIAL-LIVE? below runs it once per IR arena
\ resolution - millions of times in one compilation - and a call frame around
\ three instructions was most of what the probe cost.
: FIND-SLOT ( n -- n )
   dup 0= if drop -1 exit then
   dup SLOT-MASK and tuck cells HANDLES + @ = if else drop -1 then ;

: RESOLVE ( IR-CTX:ctx -- ptr u8 )
   CTX>N FIND-SLOT
   dup 0 < if E-IR-CTX-STALE throw then
   BASES @ ;

\ THE CURSOR IS A STACK, SO ONLY THE INNERMOST CONTEXT MAY TAKE FROM IT. A span
\ taken for a context that a deeper live one encloses lies ABOVE that deeper
\ context's mark, so the deeper context's release - not the owner's - is what
\ takes the bytes back, and the owner would go on naming storage the next
\ compilation has been handed. Every production take is by the context being
\ compiled into, which is the innermost one; the mistake is refused where it is
\ made rather than at the read that finds the bytes gone.
: TOP-CK ( n -- )
   DEPTH @ 1- <> if E-IR-CTX-NESTED throw then ;

: RESOLVE-TOP ( IR-CTX:ctx -- ptr u8 )
   CTX>N FIND-SLOT
   dup 0 < if E-IR-CTX-STALE throw then
   dup TOP-CK
   BASES @ ;

\ ---- child registries --------------------------------------------------------
\ A context owns storage that OTHER packages hand out under its serial: the IR
\ arena registry keeps a row per arena whose owner cell is this context. Those
\ registries are built ON this one and cannot be named from here, so teardown
\ announces the dying serial and each owner retires its own rows.
\
\ WHY EAGERLY, WHEN A STALE PROBE ALREADY EXISTS. A child that revalidates its
\ owner on every touch is safe without this and pays for the probe on every
\ touch. A child that resolves ONCE and then reads many times - the arena's
\ scoped reader - has no later touch to probe on, so the only thing that can
\ tell it its storage is gone is its own registry row, and that row has to be
\ retired while this context is dying rather than at somebody's next allocation.
\ Retirement therefore runs BEFORE the chunks are unmapped: there is no instant
\ at which a row still looks live over storage that is already gone.
\
\ ONE VECTOR, INSTALLED ONCE. There is one child registry today. A second one
\ installing over the first would silently stop the first being retired, so a
\ second install is a named refusal and not a replacement: the next package that
\ needs this has to generalise it on purpose.
defer RETIRE-CHILDREN ( n -- )

variable CHILDREN-SET
0 CHILDREN-SET !

: KEEP-CHILDREN ( n -- )
   drop ;

: CHILDREN-RESET ( -- )
   [: KEEP-CHILDREN ;] is RETIRE-CHILDREN ;
CHILDREN-RESET

public

\ Install the child registry's teardown retirement. Takes the retirement itself,
\ so the installing package keeps its own word private and nothing outside it
\ can retire another context's rows by naming a serial.
: RETIRE-CHILDREN! ( [ n -- ] -- )
   CHILDREN-SET @ 0<> if E-IR-CTX-STATE throw then
   1 CHILDREN-SET !
   is RETIRE-CHILDREN ;

private

\ ---- context entry and teardown ----------------------------------------------
: CEIL-OK ( n -- )
   dup 1 < over SERIAL-CEILING > or if E-IR-CTX-CEILING throw then
   drop ;

: DEPTH-ROOM ( -- )
   DEPTH @ DEPTH-MAX >= if E-IR-CTX-DEPTH throw then ;

\ Install one registry slot in the region with the validated binding.
: CTX-INSTALL ( CBIND:binding n ptr u8 n n -- )
   {: binding:CBIND:binding ceil:n base:ptr slot:n mark:n :}
   base slot BASES !
   0 base HEADER-PTR HEADER-MINTED !
   ceil base HEADER-PTR HEADER-CEIL !
   0 base HEADER-PTR HEADER-USED !
   mark base HEADER-PTR HEADER-MARK !
   binding base HEADER-PTR HEADER-BINDING ! ;

\ ---- leaving, on both paths --------------------------------------------------
\ Retire ONE named slot: announce its dying serial to the child registries,
\ release the region back to the mark this context took, and clear its row. The
\ slot is the one its owner took and never "the deepest one" - a row taken
\ outside a scope is retired by whoever took it, so the deepest row is not
\ always the row the leaving scope owns.
\
\ DEPTH IS THE TOP OF THE SLOT STACK, so it falls back to this slot only when
\ this slot IS the top. A row retired below the top leaves its slot spent rather
\ than handing it out again, because rows above it are still live and name the
\ storage they were given: dropping DEPTH past them would call a live row free.
\
\ THE CURSOR OBEYS THE SAME CONDITION, and it must: a context below the top
\ marked the region before the contexts above it allotted from it, so releasing
\ to its mark would hand storage those rows are still reading to the next
\ caller. The slot and the storage come back together or neither does, and the
\ storage a non-top retirement leaves behind is reclaimed by the release of
\ whichever context IS the top, whose mark is below this one's.
\
\ A RELEASE IS ALSO AN EPOCH, and the two are one action: the bytes above this
\ mark become the next compilation's at the same instant, so the counter that
\ tells an offset into them apart from an offset into what comes next moves
\ here and nowhere else.
: CTX-RETIRE ( n -- ) {: at:n :}
   at cells HANDLES + @ RETIRE-CHILDREN
   0 at HANDLE!
   at DEPTH @ 1- = if
      at BASES @ HEADER-PTR HEADER-MARK @ REGION-HERE !
      EPOCH-BUMP
      at DEPTH !
   then ;

\ ---- which slot a scope took -------------------------------------------------
\ A cleanup quotation cannot read its word's locals, so a scope that must give
\ back the slot it took records it here and the cleanup reads it back. The stack
\ is the SCOPES', not the registry's: `finally` nests them strictly, while the
\ registry also holds the session's row, which no scope will ever leave.
\
\ Both bounds are rechecks and not checks. DEPTH-ROOM refuses an entry past the
\ registry before a slot is taken, so this stack is never deeper than that one;
\ and a cleanup runs only for an entry that pushed. They are here so a violation
\ of either is a named refusal rather than a write outside the array.
: ENTERED-PUSH ( n -- )
   ENTERED-N @ dup DEPTH-MAX >= if E-IR-CTX-DEPTH throw then
   {: at:n k:n :}
   at k cells ENTERED + !
   k 1+ ENTERED-N ! ;

: ENTERED-POP ( -- n )
   ENTERED-N @ 1- {: k:n :}
   k 0 < if E-IR-CTX-STATE throw then
   k ENTERED-N !
   k cells ENTERED + @ ;

: CE-CLEANUP ( -- )
   ENTERED-POP CTX-RETIRE ;

\ Install one context into the registry over a header it allots from the region,
\ and answer its handle and the slot it took. Entering a context is this and
\ nothing else; the two callers differ only in when they give the slot back, so
\ this is the one place that takes a registry slot and CTX-RETIRE is the one
\ place that gives it up - and each caller retires the slot it was handed here.
\
\ THE MARK IS READ BEFORE THE HEADER IS ALLOTTED, so releasing to it gives back
\ the header as well as everything the context took after it.
: CTX-TAKE ( CBIND:binding n -- IR-CTX:ctx n )
   {: binding:CBIND:binding ceil:n :}
   DEPTH-ROOM
   DEPTH @ TAKE-GEN {: at:n g:n :}
   REGION-OPEN
   REGION-HERE @ {: mark:n :}
   binding ceil HEADER-BYTES REGION-ALLOT REGION-AT
   at mark CTX-INSTALL
   g at PACK-HANDLE at HANDLE!
   at 1+ DEPTH !
   g at PACK-HANDLE MINT-CTX at ;

\ Build the context at the region's cursor, run the caller's quotation with the
\ minted handle, then retire the slot this entry took - which is also what
\ gives the region's cursor back, on the ordinary path and on a throw alike.
: CTX-ENTER ( R CBIND:binding [ R IR-CTX:ctx -- S ] n -- S )
   {: body ceil:n :}
   ceil CTX-TAKE ENTERED-PUSH body [: CE-CLEANUP ;] finally ;

public

\ ---- creation ----------------------------------------------------------------
\ WITH-CONTEXT-BOUND is the one entry point; the module-serial ceiling is a
\ creation parameter so the exhaustion path is testable, and WITH-CONTEXT is
\ the production spelling that passes the full serial range. The binding is
\ revalidated before anything is built on it.
: WITH-CONTEXT-BOUND ( R CBIND:binding n [ R IR-CTX:ctx -- S ] -- S )
   {: ceil:n body :}
   ceil CEIL-OK
   CBIND:VALIDATE body ceil CTX-ENTER ;

: WITH-CONTEXT ( R CBIND:binding [ R IR-CTX:ctx -- S ] -- S )
   SERIAL-CEILING swap WITH-CONTEXT-BOUND ;

\ ---- identity ----------------------------------------------------------------
\ The context's own serial. A pure projection: it still answers on a stale
\ handle, so a caught failure can always name the owning context.
: SERIAL ( IR-CTX:ctx -- n )
   CTX>N ;

: LIVE? ( IR-CTX:ctx -- bool )
   CTX>N FIND-SLOT 0 < 0= ;

\ Liveness observed through a raw serial. Child modules that outlive a single
\ call (the IR arena) can persist their owner only as the context serial,
\ because handles are sealed nominals a stored raw cell cannot re-mint; they
\ still must observe owner teardown fail-closed before touching context-owned
\ storage. A boolean probe mints no handle and exposes no pointer, so it adds
\ no forging or access power beyond what LIVE? already publishes.
\
\ LIVENESS IS THE REGISTRY COMPARE. The serial a child stored is a whole handle,
\ and it is live exactly while the row it names still holds it, so this answers
\ without building a slot number for FIND-SLOT to hand back and throw away. Zero
\ is still tested first and is still not live: it masks to slot 0, and a retired
\ row holds zero, so the bare compare would call an empty row 0 a live context.
: SERIAL-LIVE? ( n -- bool )
   dup 0= if drop 0 0 <> exit then
   dup SLOT-MASK and cells HANDLES + @ = ;

\ ---- bound target and policy -------------------------------------------------
: BINDING@ ( IR-CTX:ctx -- CBIND:binding )
   RESOLVE HEADER-PTR HEADER-BINDING @ ;

\ ---- module allocation -------------------------------------------------------
private

\ Reserve one mint against the context ceiling. The count is written before the
\ IR-ID take, so a global serial-exhaustion throw can never leave a module this
\ context took but did not account for.
: MINT-TAKE ( ptr u8 -- )
   dup HEADER-PTR HEADER-MINTED @ CNT-OK
   over HEADER-PTR HEADER-CEIL @ CNT-OK {: m:n c:n :}
   m c >= if E-IR-CTX-SERIALS throw then
   m 1+ swap HEADER-PTR HEADER-MINTED ! ;

public

\ Mint a module identity owned by this context. Serials come from the IR-ID
\ authority, so they are nonzero, monotonic, and never reused - across sibling
\ contexts as well as within one - and this context's ceiling bounds how many
\ it may take.
: NEW-MODULE ( IR-CTX:ctx -- IR-ID:ir-module-key IR-ID:ir-module-id )
   RESOLVE MINT-TAKE IR-ID:NEW-MODULE ;

\ ---- scratch -----------------------------------------------------------------
public

\ Every returned span stays where it is, holding the same bytes, until the
\ context that took it leaves - which is what makes an offset into it safe to
\ hand from one pass to the next without any liveness stamp of its own.
\
\ THE OFFSET IS THE PRIMITIVE AND THE ADDRESS IS DERIVED. A record kept inside
\ the region may hold an offset and may not hold a pointer: the pointer names a
\ mapping this process took and a captured image must carry neither, while the
\ offset names a position that means the same thing for as long as the extent
\ does. So the take answers an offset and the address form is one addition on
\ top of it, for the callers whose span never leaves their own hands.
: SCRATCH-OFFSET ( IR-CTX:ctx n -- n ) {: c:IR-CTX:ctx need:n :}
   need 1 < if E-IR-CTX-SIZE throw then
   need REGION-BYTES > if E-IR-CTX-SCRATCH throw then
   c RESOLVE-TOP {: base:ptr :}
   base HEADER-PTR HEADER-USED @ CNT-OK {: used:n :}
   need REGION-ALLOT {: off:n :}
   used need ALIGN8 + base HEADER-PTR HEADER-USED !
   off ;

: SCRATCH-TAKE ( IR-CTX:ctx n -- ptr u8 n ) {: c:IR-CTX:ctx need:n :}
   c need SCRATCH-OFFSET REGION-AT need ;

: SCRATCH-LIMIT ( -- n ) REGION-BYTES ;

\ How wide a region offset is, for a child that packs one beside other fields.
: SCRATCH-OFFSET-BITS ( -- n ) OFFSET-BITS ;

\ The release epoch, to be stamped into a record stored at a region offset; see
\ the account at REGION-EPOCH.
: EPOCH ( -- n ) REGION-EPOCH @ ;

\ A FILE range can carry retired context handles and region offsets in DATA.
\ Before importing it, preserve the highest identities the cold compilation
\ reached. Earlier or unrelated work may already have advanced either count.
: HISTORY ( -- n n ) GEN-CELL atomic@ REGION-EPOCH @ ;

private

: GEN-ENSURE ( n -- ) {: want:n :}
   begin
      GEN-CELL atomic@ {: old:n :}
      old want >= if exit then
      old want GEN-CELL atomic-cas old =
   until ;

: EPOCH-ENSURE ( n -- )
   REGION-EPOCH @ max REGION-EPOCH ! ;

public

: CHECK-HISTORY ( n n -- )
   {: gen:n epoch:n :}
   gen 0 < gen GEN-MAX > or epoch 0 < or epoch EPOCH-MAX > or
      if E-IR-CTX-SERIALS throw then ;

: REPLAY-HISTORY ( n n -- )
   {: gen:n epoch:n :}
   gen epoch CHECK-HISTORY
   gen GEN-ENSURE epoch EPOCH-ENSURE ;

public

\ No compilation context survives into a captured runtime. Registry payloads
\ share the transient workspace; only the generation and region epochs persist.
: CAPTURE-PREPARE ( -- )
   DEPTH @ 0<> if E-IR-CTX-STATE throw then
   ENTERED-N @ 0<> if E-IR-CTX-STATE throw then
   REGION-CLOSE ;

\ ---- the compilation session -------------------------------------------------
\ A context is scoped because a compilation is: WITH-CONTEXT marks the region,
\ runs the work, and gives the cursor back. One SOURCE LOAD is not that shape.
\ Everything a definition builds - its modules, its tape, its builders - dies
\ when the definition ends, while the things a load builds once and every
\ definition only reads - the dialect's interned opcode identities, the
\ source-word model, the interner those spellings live in - outlive each
\ definition and die with the load. A scoped context cannot hold the second
\ kind, so the load-lived half was rebuilt for all 3,079 definitions.
\
\ A SESSION IS AN UNSCOPED CONTEXT. SESSION-OPEN installs it through the same
\ CTX-TAKE that WITH-CONTEXT uses, so its mark is the bottom of the region;
\ SESSION-CLOSE runs the same CTX-RETIRE over the slot CTX-TAKE answered, which
\ releases the cursor back to that bottom mark. Entry and teardown keep one
\ implementation each, with WITH-CONTEXT and this pair as their two callers, and
\ each of the two retires the slot it was handed.
\
\ THE SESSION'S MARK DOMINATES EVERY DEFINITION'S. The session enters first, so
\ everything its owner builds lies below every later mark and no definition's
\ release can reach it; a definition's tables may hold offsets into the
\ session's storage and never the reverse.
\
\ A DEFINITION IS AN ORDINARY WITH-CONTEXT NESTED IN THE SESSION, not a
\ watermark inside it. A definition's builders live in a process-wide registry
\ whose rows are freed only when their owning context tears down, so the thing
\ that ends a definition has to be a context teardown: a mark inside one
\ context cannot reach them. The child costs one mapping per definition and
\ gives back everything the definition took, on the ordinary path and on a
\ throw alike.
\
\ WHAT LIVES IN THE SESSION is whatever its owner builds in the session context
\ before the first definition and only reads afterwards. A module minted there
\ carries the session's serial, so the owner check every child performs against
\ it holds for the whole load, and the storage behind it stays live because the
\ session context stays live.
\
\ THE SESSION LIVES UNTIL CAPTURE OR PROCESS EXIT. There is no load-end hook
\ and none is needed: each definition's own context is what bounds the memory,
\ so an idle session costs one header mapping and whatever its owner built once.
\
\ WHAT THE OWNER HOLDS GOES OUT WITH THE SESSION. An owner that keeps a flag
\ saying its session-lived state is readable installs its admission and
\ stand-down through SESSION-STAND-DOWN!, and SESSION-CLOSE runs the admission
\ before either the owner or the row is changed. The stand-down runs before
\ retirement -
\ so no reader can be handed such a flag over storage that is already gone, and
\ no ordering between a capture's entry points decides it.
\
\ A SECOND SESSION IS REFUSED, so is a session opened while ANY context is
\ already open - a session that a scope encloses would die with that scope while
\ still answering as live - and so is a close taken while a context is open
\ inside the session; all three are E-IR-CTX-STATE.
private

1 TYPED-BUFFER SESSION-CTX IR-CTX:ctx
variable SESSION-SLOT

: SESSION-FORGET ( -- )
   -1 SESSION-SLOT ! ;
SESSION-FORGET

: SESSION-CK ( -- )
   SESSION-SLOT @ 0 < if E-IR-CTX-STATE throw then ;

\ The slot is recorded from what CTX-TAKE answers, not from DEPTH read a second
\ time: the row the session gives back is the row it took.
: SESSION-INSTALL ( CBIND:binding -- )
   SERIAL-CEILING CTX-TAKE SESSION-SLOT !
   0 SESSION-CTX ! ;

: SESSION-RETIRE ( -- )
   SESSION-SLOT @ CTX-RETIRE
   SESSION-FORGET ;

\ A session outlives every scope, so it is the BOTTOM row of the registry or it
\ is nothing: one opened inside a scope would be a row that scope encloses and
\ outlives at the same time, and the close that the scope's exit is not would
\ never come.
: REGISTRY-EMPTY-CK ( -- )
   DEPTH @ 0<> if E-IR-CTX-STATE throw then ;

\ The session must be the deepest context there is when it goes, because a
\ context open inside it is reading what the session holds - its interner, its
\ modules, whatever its owner built there - and the storage behind those dies
\ with this row. A definition's context is deeper, so this is how a close taken
\ inside a definition is refused.
: SESSION-DEEPEST-CK ( -- )
   DEPTH @ 1- SESSION-SLOT @ <> if E-IR-CTX-STATE throw then ;

\ ---- what the session's owner stands down ------------------------------------
\ A session's owner builds state in the session context and holds FLAGS saying
\ that state is readable: a dialect's interned prototype, a registered
\ vocabulary. Those flags describe storage that dies with this row, so they have
\ to go out WITH it and not at some later point in a capture sequence. In between
\ them a reader answers from a flag that says yes over an arena that is already
\ gone - E-IR-ARENA-STALE from a reader that asked an ordinary question - and
\ which of the two runs first becomes load-bearing between two entry points.
\ SESSION-CLOSE therefore runs the owner's stand-down itself, before the row is
\ retired, so a capture's close and the owner's own close are one action.
\
\ ONE OWNER PAIR, INSTALLED ONCE, for the reason RETIRE-CHILDREN! gives above:
\ a second install would silently stop the first one from running. Admission
\ refuses while another root or task owns the compiler; only a successful
\ admission lets stand-down and retirement begin.
defer SESSION-ADMISSION ( -- )
defer SESSION-STAND-DOWN ( -- )

variable STAND-SET
0 STAND-SET !

: KEEP-STANDING ( -- ) ;

: STAND-RESET ( -- )
   [: KEEP-STANDING ;] is SESSION-ADMISSION
   [: KEEP-STANDING ;] is SESSION-STAND-DOWN ;
STAND-RESET

\ Once admitted, the row goes back whatever the stand-down did, so an owner
\ that throws cannot leave a live row over storage this close is about to
\ release; the first error is the one the caller is told.
: SESSION-END ( -- )
   [: SESSION-STAND-DOWN ;] catch {: rc:n :}
   SESSION-RETIRE
   rc 0<> if rc throw then ;

public

\ Install the admission and stand-down a session's owner runs when it closes.
\ The owner keeps both words private, so nothing else can stand its state down.
: SESSION-STAND-DOWN! ( [ -- ] [ -- ] -- )
   STAND-SET @ 0<> if E-IR-CTX-STATE throw then
   1 STAND-SET !
   is SESSION-STAND-DOWN
   is SESSION-ADMISSION ;

: SESSION-LIVE? ( -- bool )
   SESSION-SLOT @ 0 < 0= ;

\ A FILE import cannot alias saved handles against an unrelated live context.
\ A compiler session is the only permitted live row while importing its child.
: REGISTRY-IDLE? ( -- bool ) DEPTH @ 0= ;
: REGISTRY-SESSION-ONLY? ( -- bool )
   SESSION-LIVE? DEPTH @ 1 = and ;

\ The one open session occupies slot zero. FILE metadata stores this identity
\ so replay can distinguish a continuing session from a close and a new open.
: SESSION-SERIAL ( -- n )
   SESSION-LIVE? if 0 SESSION-CTX @ SERIAL else 0 then ;
: SESSION-GEN-OF ( n -- n )
   dup 0= if exit then
   dup 0 < over SLOT-MASK and 0<> or if E-IR-CTX-SERIALS throw then
   SLOT-BITS rshift dup 0 <= over GEN-MAX > or if E-IR-CTX-SERIALS throw then ;

\ Give the load's context back: what its owner holds, then the row this session
\ took - which is what releases the region's cursor back to the bottom mark
\ this session marked.
: SESSION-CLOSE ( -- )
   SESSION-CK
   SESSION-DEEPEST-CK
   SESSION-ADMISSION
   SESSION-END ;

private

variable HOOKED
0 HOOKED !

\ No session survives into a captured image: this runs before the capture
\ machinery asks IR-CTX to clear its registry, so an idle session is closed in
\ time for that check to pass, and a capture taken while a definition's context
\ is open is refused here first, by the session's own name. The flag is cleared
\ only when the close succeeded, because a callback that throws stays
\ registered for the retry.
: SESSION-AT-CAPTURE ( -- )
   SESSION-LIVE? if SESSION-CLOSE then
   0 HOOKED ! ;

\ Registered when a session is TAKEN and not when this file is loaded, the way
\ every other owner of process-local state registers its cleanup. A captured
\ image carries this flag but not the registry - PREPARE removes every callback
\ it runs - so a load-time registration would never happen again in an engine
\ built from an image, and the first capture after the first tier-1 definition
\ would find a live context and refuse.
: HOOK-CAPTURE ( -- )
   HOOKED @ 0<> if exit then
   1 HOOKED !
   [: SESSION-AT-CAPTURE ;] IMAGE-LIFECYCLE:REGISTER ;

public

\ Open the load's context. The binding is revalidated just as
\ WITH-CONTEXT-BOUND does, and the ceiling is the production one: a session is
\ the production spelling and has no test-only narrowing.
\
\ THE REGISTRY MUST BE EMPTY, which REGISTRY-EMPTY-CK states. A definition
\ compiled inside a caller's own context - which is how the first tier-1
\ definition of a load reaches here - is therefore refused by name rather than
\ served a session that dies with the caller's scope.
: SESSION-OPEN ( CBIND:binding -- IR-CTX:ctx )
   SESSION-LIVE? if E-IR-CTX-STATE throw then
   REGISTRY-EMPTY-CK
   SERIAL-CEILING CEIL-OK
   DEPTH-ROOM
   HOOK-CAPTURE
   CBIND:VALIDATE
   SESSION-INSTALL
   0 SESSION-CTX @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
