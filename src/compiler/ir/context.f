\ context.f - the owned compiler context: one explicit owner per compilation.
\
\ docs/compiler-ir-design.md section 6.2. Every compilation gets exactly one
\ context, and the context is the single owner of the compilation's target and
\ numerical policy, its module serial allocation, and its scratch memory. The
\ slots for the source registry, the diagnostic sink, and the witness allocator
\ exist from birth but stay in an explicit unbound state that rejects use until
\ those modules land; nothing is stubbed silently.
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
\ PERSISTED STATE. All per-context state lives in the context's own mapping as
\ eight-byte little-endian slots written with the canonical CDIGEST slot words:
\ the module-mint count and ceiling, scratch usage, the unbound module
\ slots, and the bound target/policy pair. The pair is persisted as the stable
\ wire codes read straight out of the components' canonical preimages
\ (CTARGET:ENCODE / CNUM:ENCODE), and reading it back reconstructs the value
\ through the validating constructors and CBIND:BIND, so a forged or corrupted
\ header can never produce an unvalidated binding. Creation round-trips the
\ staged codes through that same decoder and compares with CBIND:SAME?, so a
\ component schema change that renumbers a wire code fails closed at creation
\ instead of decoding a different machine later.
\
\ CONCURRENCY. The generation counter is atomic, so serials stay unique if
\ tasks race. The registry itself is one process-wide stack and assumes the
\ current single-task compilation discipline, like the digest state it builds
\ on (src/compiler/digest.f header); concurrent compilation needs per-task
\ registries first.

require lib/prelude.f
require lib/errors.f
require lib/memory.f
require src/compiler/digest.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require lib/image-lifecycle.f

package IR-CTX
public

NEWTYPE ctx 0

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
$4000000 constant REGION-BYTES        \ 64 MiB of reserved scratch address space

\ Header slots inside the mapping, one CDIGEST slot each.
0 constant HF-MINTED                 \ modules minted by this context
1 constant HF-CEIL                   \ this context's module ceiling
2 constant HF-USED                   \ aligned bytes handed to scratch callers
3 constant HF-CODE0                  \ first of the ten binding wire-code slots
13 constant HF-SOURCES               \ source registry slot (unbound)
14 constant HF-DIAG                  \ diagnostic sink slot (unbound)
15 constant HF-WITNESS               \ witness allocator slot (unbound)
16 constant HF-MARK                  \ region cursor when this context entered
17 constant HDR-SLOTS
HDR-SLOTS CDIGEST:SLOT-BYTES * constant HDR-BYTES
0 constant SLOT-UNBOUND

\ Wire-code slots relative to a ten-slot code window: five target fields then
\ five policy fields, in each component's canonical preimage order.
0 constant CS-ARCH
1 constant CS-ABI
2 constant CS-ENDIAN
3 constant CS-PTRW
4 constant CS-FEAT
5 constant CS-OVF
6 constant CS-FLOAT
7 constant CS-CONTR
8 constant CS-FAST
9 constant CS-CMP
10 constant CODES#
2 constant COMP-CODE0                \ first semantic slot of a component preimage
5 constant COMP-CODE#                \ semantic slots per component preimage
7 CDIGEST:SLOT-BYTES * constant COMP-PRE-BYTES

\ ---- registry storage --------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable GEN-CELL
0 GEN-CELL !
variable DEPTH
0 DEPTH !
create HANDLES DEPTH-MAX cells allot
: HANDLES-CLEAR ( -- )
   DEPTH-MAX 0 ?do 0 HANDLES i cells + ! loop ;
HANDLES-CLEAR
create BASES DEPTH-MAX cells allot
create ENTERED DEPTH-MAX cells allot   \ scope depth -> the slot that scope took
variable ENTERED-N
0 ENTERED-N !
create STAGE CODES# CDIGEST:SLOT-BYTES * allot

: HANDLE! ( n n -- )
   cells HANDLES + ! ;

: BASE-FIELD ( n -- ptr ptr u8 )
   cells BASES + 0 ptr-field ;

\ ---- header slot access ------------------------------------------------------
: HDR@ ( ptr u8 n -- n )
   CDIGEST:SLOT@ ;

: HDR! ( n ptr u8 n -- )
   CDIGEST:SLOT! ;

: CNT-OK ( n -- n )
   dup 0 < if E-IR-CTX-STATE throw then ;

\ ---- the scratch region ------------------------------------------------------
\ One mapping for the process, one cursor into it. The mapping is taken on the
\ first take and given back only at capture, because a region whose pages are
\ already faulted in is the whole saving: a compilation that mapped its own
\ storage paid the kernel for the same pages again per definition.
here CELL 1- and CELL swap - CELL 1- and allot
PTR-VARIABLE REGION-BASE
NULL-PTR REGION-BASE !
variable REGION-HERE
0 REGION-HERE !

: REGION-ALLOC-LEN ( -- NUM:alloc-byte-len )
   REGION-BYTES MEM:BYTES-ALLOC-LEN ;

\ Map the region on demand. The cursor is at zero whenever there is no mapping,
\ because the only word that drops the mapping is the capture preparation below
\ and it runs with no context open.
: REGION-OPEN ( -- )
   REGION-BASE @ NULL-PTR = 0= if exit then
   REGION-ALLOC-LEN MEM:ALLOC-BYTES drop REGION-BASE !
   0 REGION-HERE ! ;

: REGION-CLOSE ( -- )
   REGION-BASE @ NULL-PTR = if exit then
   REGION-BASE @ REGION-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR REGION-BASE !
   0 REGION-HERE ! ;

: ALIGN8 ( n -- n ) 7 + 8 / 8 * ;

\ Take `need` aligned bytes from the region cursor. The refusal comes before
\ the cursor moves, so a region that is full stays exactly as usable as it was.
: REGION-ALLOT ( n -- ptr u8 ) {: need:n :}
   REGION-OPEN
   need ALIGN8 {: step:n :}
   REGION-HERE @ {: off:n :}
   step REGION-BYTES off - > if E-IR-CTX-SCRATCH throw then
   off step + REGION-HERE !
   REGION-BASE @ off + ;

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
   BASE-FIELD @ ;

\ ---- binding wire codes: decode ----------------------------------------------
\ The codes are the components' stable canonical wire codes; a slot outside the
\ published vocabulary is corrupted context state.
: N>ARCH ( n -- CTARGET:arch )
   case
      0 of CTARGET-ARCH:AARCH64 endof
      1 of CTARGET-ARCH:PTX endof
      2 of CTARGET-ARCH:A32 endof
      3 of CTARGET-ARCH:THUMB2 endof
      4 of CTARGET-ARCH:C66X endof
      E-IR-CTX-STATE throw
   endcase ;

: N>ABI ( n -- CTARGET:abi )
   case
      0 of CTARGET-ABI:AAPCS64-DARWIN endof
      1 of CTARGET-ABI:AAPCS64-LINUX endof
      2 of CTARGET-ABI:PTX-KERNEL endof
      3 of CTARGET-ABI:AAPCS32 endof
      4 of CTARGET-ABI:C6000-EABI endof
      E-IR-CTX-STATE throw
   endcase ;

: N>ENDIAN ( n -- CTARGET:endian )
   case
      0 of CTARGET-ENDIAN:LITTLE endof
      1 of CTARGET-ENDIAN:BIG endof
      E-IR-CTX-STATE throw
   endcase ;

: N>PTRW ( n -- CTARGET:ptr-width )
   case
      0 of CTARGET-PTR--WIDTH:BITS32 endof
      1 of CTARGET-PTR--WIDTH:BITS64 endof
      E-IR-CTX-STATE throw
   endcase ;

: N>OVF ( n -- CNUM:overflow )
   case
      0 of CNUM-OVERFLOW:WRAP endof
      1 of CNUM-OVERFLOW:TRAP endof
      E-IR-CTX-STATE throw
   endcase ;

: N>FLOAT ( n -- CNUM:float-model )
   case
      0 of CNUM-FLOAT--MODEL:IEEE754 endof
      1 of CNUM-FLOAT--MODEL:FLUSH-DENORMAL endof
      E-IR-CTX-STATE throw
   endcase ;

: N>CONTR ( n -- CNUM:contraction )
   case
      0 of CNUM-CONTRACTION:FORBIDDEN endof
      1 of CNUM-CONTRACTION:ALLOWED endof
      E-IR-CTX-STATE throw
   endcase ;

: N>FAST ( n -- CNUM:fast-math )
   case
      0 of CNUM-FAST--MATH:BIT-EXACT endof
      1 of CNUM-FAST--MATH:REASSOCIATE endof
      2 of CNUM-FAST--MATH:APPROXIMATE endof
      E-IR-CTX-STATE throw
   endcase ;

: N>CMP ( n -- CNUM:compare )
   case
      0 of CNUM-COMPARE:IEEE754-UNORDERED endof
      1 of CNUM-COMPARE:TOTAL-ORDER endof
      2 of CNUM-COMPARE:ASSUME-ORDERED endof
      E-IR-CTX-STATE throw
   endcase ;

\ Reconstruct the target contract from a ten-slot code window. Every value goes
\ back through the validating constructors, so the result is always a coherent
\ contract or a named throw.
: CODES-TARGET@ ( ptr u8 -- CTARGET:contract )
   dup CS-ARCH HDR@ N>ARCH swap
   dup CS-ABI HDR@ N>ABI swap
   dup CS-ENDIAN HDR@ N>ENDIAN swap
   dup CS-PTRW HDR@ N>PTRW swap
   CS-FEAT HDR@ CTARGET:FEATURE-SET
   CTARGET:CONTRACT ;

: CODES-POLICY@ ( ptr u8 -- CNUM:numeric-policy )
   dup CS-OVF HDR@ N>OVF swap
   dup CS-FLOAT HDR@ N>FLOAT swap
   dup CS-CONTR HDR@ N>CONTR swap
   dup CS-FAST HDR@ N>FAST swap
   CS-CMP HDR@ N>CMP
   CNUM:POLICY ;

: CODES-BINDING@ ( ptr u8 -- CBIND:binding )
   dup CODES-TARGET@ swap CODES-POLICY@ CBIND:BIND ;

: CODES-AT ( ptr u8 n -- ptr u8 )
   CDIGEST:SLOT-BYTES * + ;

\ ---- binding wire codes: stage on creation -----------------------------------
\ Copy one component's five semantic preimage slots into the staging window.
: STAGE-CODES! ( ptr u8 n n -- )
   {: dst0:n :}
   COMP-PRE-BYTES <> if E-IR-CTX-STATE throw then
   COMP-CODE# 0 ?do
      dup COMP-CODE0 i + HDR@
      STAGE dst0 i + HDR!
   loop
   drop ;

: STAGE-TARGET! ( ptr u8 n -- )
   CS-ARCH STAGE-CODES! ;

: STAGE-POLICY! ( ptr u8 n -- )
   CS-OVF STAGE-CODES! ;

\ Persist a validated binding as wire codes and prove the codes decode back to
\ the same binding before any context is built on them.
: STAGE-BINDING ( CBIND:binding -- )
   dup CBIND:TARGET@ CTARGET:ENCODE STAGE-TARGET!
   dup CBIND:POLICY@ CNUM:ENCODE STAGE-POLICY!
   STAGE CODES-BINDING@ CBIND:SAME? 0= if E-IR-CTX-STATE throw then ;

: STAGE>HDR ( ptr u8 -- )
   CODES# 0 ?do
      STAGE i HDR@
      over HF-CODE0 i + HDR!
   loop
   drop ;

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

\ Install one registry slot: record the header's place in the region and the
\ mark the context's storage starts at, reset the counters, mark the
\ not-yet-landed module slots unbound, and copy the staged binding codes.
: CTX-INSTALL ( n ptr u8 n n -- )
   {: slot:n mark:n :}
   dup slot BASE-FIELD !
   swap over HF-CEIL HDR!
   0 over HF-MINTED HDR!
   0 over HF-USED HDR!
   mark over HF-MARK HDR!
   SLOT-UNBOUND over HF-SOURCES HDR!
   SLOT-UNBOUND over HF-DIAG HDR!
   SLOT-UNBOUND over HF-WITNESS HDR!
   STAGE>HDR ;

\ ---- leaving, on both paths ---------------------------------------------------
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
: CTX-RETIRE ( n -- ) {: at:n :}
   at cells HANDLES + @ RETIRE-CHILDREN
   0 at HANDLE!
   at DEPTH @ 1- = if
      at BASE-FIELD @ HF-MARK HDR@ REGION-HERE !
      at DEPTH !
   then ;

\ ---- which slot a scope took --------------------------------------------------
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
: CTX-TAKE ( n -- IR-CTX:ctx n )
   DEPTH-ROOM
   DEPTH @ TAKE-GEN {: at:n g:n :}
   REGION-OPEN
   REGION-HERE @ {: mark:n :}
   HDR-BYTES REGION-ALLOT
   at mark CTX-INSTALL
   g at PACK-HANDLE at HANDLE!
   at 1+ DEPTH !
   g at PACK-HANDLE MINT-CTX at ;

\ Build the context at the region's cursor, run the caller's quotation with the
\ minted handle, then retire the slot this entry took - which is also what
\ gives the region's cursor back, on the ordinary path and on a throw alike.
: CTX-ENTER ( R [ R IR-CTX:ctx -- S ] n -- S )
   CTX-TAKE ENTERED-PUSH swap [: CE-CLEANUP ;] finally ;

public

\ ---- creation ----------------------------------------------------------------
\ WITH-CONTEXT-BOUND is the one entry point; the module-serial ceiling is a
\ creation parameter so the exhaustion path is testable, and WITH-CONTEXT is
\ the production spelling that passes the full serial range. The binding is
\ revalidated before anything is built on it.
: WITH-CONTEXT-BOUND ( R CBIND:binding n [ R IR-CTX:ctx -- S ] -- S )
   {: ceil:n body :}
   ceil CEIL-OK
   CBIND:VALIDATE STAGE-BINDING
   body ceil CTX-ENTER ;

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
   RESOLVE HF-CODE0 CODES-AT CODES-BINDING@ ;

\ ---- module allocation -------------------------------------------------------
private

\ Reserve one mint against the context ceiling. The count is written before the
\ IR-ID take, so a global serial-exhaustion throw can never leave a module this
\ context took but did not account for.
: MINT-TAKE ( ptr u8 -- )
   dup HF-MINTED HDR@ CNT-OK
   over HF-CEIL HDR@ CNT-OK {: m:n c:n :}
   m c >= if E-IR-CTX-SERIALS throw then
   m 1+ swap HF-MINTED HDR! ;

public

\ Mint a module identity owned by this context. Serials come from the IR-ID
\ authority, so they are nonzero, monotonic, and never reused - across sibling
\ contexts as well as within one - and this context's ceiling bounds how many
\ it may take.
: NEW-MODULE ( IR-CTX:ctx -- IR-ID:ir-module-key IR-ID:ir-module-id )
   RESOLVE MINT-TAKE IR-ID:NEW-MODULE ;

: MINTED ( IR-CTX:ctx -- n )
   RESOLVE HF-MINTED HDR@ CNT-OK ;

\ ---- scratch -----------------------------------------------------------------
public

\ Every returned span stays at the same address, holding the same bytes, until
\ the context that took it leaves - which is what makes an offset into it safe
\ to hand from one pass to the next without any liveness stamp of its own.
: SCRATCH-TAKE ( IR-CTX:ctx n -- ptr u8 n ) {: c:IR-CTX:ctx need:n :}
   need 1 < if E-IR-CTX-SIZE throw then
   need REGION-BYTES > if E-IR-CTX-SCRATCH throw then
   c RESOLVE {: base:ptr :}
   base HF-USED HDR@ CNT-OK {: used:n :}
   need REGION-ALLOT {: span:ptr :}
   used need ALIGN8 + base HF-USED HDR!
   span need ;

: SCRATCH-USED ( IR-CTX:ctx -- n )
   RESOLVE HF-USED HDR@ CNT-OK ;

: SCRATCH-LIMIT ( -- n ) REGION-BYTES ;

\ ---- not-yet-landed module slots ---------------------------------------------
private

: SLOT-CHECK ( ptr u8 n -- )
   HDR@ SLOT-UNBOUND <> if E-IR-CTX-STATE throw then
   E-IR-CTX-UNBOUND throw ;

public

\ Fail-closed unbound slots: each throws E-IR-CTX-UNBOUND until its owning
\ module lands and binds it, and anything else in the slot is corrupted state.
: SOURCES@ ( IR-CTX:ctx -- )
   RESOLVE HF-SOURCES SLOT-CHECK ;

: DIAG@ ( IR-CTX:ctx -- )
   RESOLVE HF-DIAG SLOT-CHECK ;

: WITNESSES@ ( IR-CTX:ctx -- )
   RESOLVE HF-WITNESS SLOT-CHECK ;

public

\ No compilation context survives into a captured runtime.  The registry depth
\ is the lifecycle authority; once it is empty, clear the retired mapping
\ cells so the DATA image carries no host mapping pointers.
: CAPTURE-PREPARE ( -- )
   DEPTH @ 0<> if E-IR-CTX-STATE throw then
   DEPTH-MAX 0 ?do
      NULL-PTR i BASE-FIELD !
   loop
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
\ saying its session-lived state is readable installs its stand-down through
\ SESSION-STAND-DOWN!, and SESSION-CLOSE runs that before the row is retired -
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
: SESSION-INSTALL ( -- )
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
\ ONE VECTOR, INSTALLED ONCE, for the reason RETIRE-CHILDREN! gives above: a
\ second install would silently stop the first one from running.
defer SESSION-STAND-DOWN ( -- )

variable STAND-SET
0 STAND-SET !

: KEEP-STANDING ( -- ) ;

: STAND-RESET ( -- )
   [: KEEP-STANDING ;] is SESSION-STAND-DOWN ;
STAND-RESET

\ The row goes back whatever the stand-down did, so an owner that throws cannot
\ leave a live row over storage this close is about to release; the first error
\ is the one the caller is told.
: SESSION-END ( -- )
   [: SESSION-STAND-DOWN ;] catch {: rc:n :}
   SESSION-RETIRE
   rc 0<> if rc throw then ;

public

\ Install the stand-down a session's owner runs when the session closes. Takes
\ the word itself, so the owner keeps it private and nothing else can stand
\ another package's state down.
: SESSION-STAND-DOWN! ( [ -- ] -- )
   STAND-SET @ 0<> if E-IR-CTX-STATE throw then
   1 STAND-SET !
   is SESSION-STAND-DOWN ;

: SESSION-LIVE? ( -- bool )
   SESSION-SLOT @ 0 < 0= ;

\ Give the load's context back: what its owner holds, then the row this session
\ took - which is what releases the region's cursor back to the bottom mark
\ this session marked.
: SESSION-CLOSE ( -- )
   SESSION-CK
   SESSION-DEEPEST-CK
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

\ Open the load's context. The binding is revalidated and staged exactly as
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
   CBIND:VALIDATE STAGE-BINDING
   SESSION-INSTALL
   0 SESSION-CTX @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
