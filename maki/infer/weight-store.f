\ weight-store.f - policy-selectable weight residency for the GB10 UMA inference
\ engine (sealed package WSTORE; inference leaf S3d, epic habu-epic-gb10-uma-391d12e8).
\
\ WHY THIS MODULE EXISTS. A bound model owns exactly ONE immutable linear
\ residency value - the store - and every weight read goes through it. The store
\ has two arms because the GB10 plan forbids forcing a copy of multi-GiB
\ checkpoints: the MAPPED arm serves weights straight out of the checkpoint's
\ file mapping (the linear SAFET:mapping owner that DETACH-MAPPING hands out),
\ so a 14-25 GiB model needs no second resident copy, while the ALLOCATED arm
\ owns a device-friendly buffer filled once by the loader for the checkpoints
\ where copying pays (measured in docs/gb10-uma-residency.md). Which arm a model
\ uses is the caller's residency choice, named by the payload-free enum below.
\
\ MODEL-AGNOSTIC BY CONSTRUCTION. The store maps SLOT NUMBERS to byte ranges and
\ nothing else. What a slot MEANS - which layer, which role, which tensor - is
\ the consumer's pure function; WSTORE never learns roles, layers, or model
\ geometry, so any model whose weights can be numbered can live in a store. The
\ slot table is built once (TABLE-NEW / SLOT! / SEAL), fully populated or
\ refused, and immutable after SEAL: there is no word anywhere that writes a
\ sealed table.
\
\ NAMING NOTE (one deviation from the leaf contract's spelling). The contract
\ calls the residency enum "policy"; `policy` is a reserved token of the typed
\ declaration grammar (the POLICY header clause), so `ENUM policy ... ;ENUM`
\ rejects with 7110. The family is spelled `residency` instead - same two
\ variants, same role, only the tail differs.
\
\ OWNERSHIP. tbuilder, table, buffer, and the store bundle itself are linear:
\ the checker rejects duplicating, dropping, storing, or reusing any of them.
\ SEAL consumes the builder and publishes the table by retyping the same block;
\ the store constructors (the generated WSTORE-STORE:MAPPED / :ALLOCATED) consume
\ their arm's owner and the table and are TOTAL - no throw sits between a
\ DETACH-MAPPING and the mapped store's construction. DISPOSE is the exit for a
\ store: the mapped arm frees the table and gives the mapping back through
\ SAFET:UNMAP-MAPPING; the allocated arm frees the table and releases the buffer
\ through MEM:RELEASE-BYTES. Both report one result<n,n> (ok = bytes given back)
\ instead of throwing past owners a caller has not disposed of yet.
\
\ A SEALED TABLE HAS ITS OWN EXIT. A table does not have to become a store. A
\ caller can seal one and then decide not to commit - the bind transaction's
\ prepared value is exactly that: it owns a sealed table and no arm owner yet, and
\ its abort path must dispose the table on its own. Before TABLE-DISPOSE existed
\ the only route to the table's memory was through DISPOSE, so such a caller had
\ to FABRICATE a store around a mapping it did not want to consume just to reach a
\ free path, and a caller that could not do even that leaked the block with no
\ public word able to free it (the checker refuses a bare `drop` on a linear
\ table, correctly). That was a real leak in this module's surface, not merely an
\ inconvenience: the package that mints a linear owner must own its exit.
\ TABLE-DISPOSE is that exit, and it reports the same result<n,n> shape DISPOSE
\ does so a caller unwinding several owners handles one outcome type.
\
\ EVERY OWNER OWNS ITS EXIT. The same argument settles the other two owners, so all
\ four now have a public way out: BUILDER-DISPOSE for a builder that will not be
\ sealed, and BUFFER-DISPOSE for a filled buffer whose store never got built. Both
\ were reachable states with no exit, and this module minted the owner in each of
\ them. The four exits report one outcome type, and what ok carries is always the
\ memory THAT word gave back - a block's byte length for the two table exits, the
\ buffer's byte extent for BUFFER-DISPOSE, the arm's bytes for DISPOSE.
\
\ WHAT THE EXITS STILL CANNOT REACH, AND WHY THAT IS THE CHECKER'S GAP. An owner a
\ THROW strands is not recoverable by any of them: the throw unwinds past the token
\ itself, so the caller's catch has nothing left to hand to an exit. Which refusals
\ that leaves recoverable follows from the SHAPE of the step that refuses:
\   - SLOT! is stack-preserving ( tbuilder -- tbuilder ), so a caller can run it
\     inside a `catch` and a refusal leaves the builder exactly where it was (the
\     SAFET:LOAD discipline). Those four refusals - E-SLOT twice, E-SET, E-EXTENT -
\     are now disposable, and the suite disposes them;
\   - SEAL is ( tbuilder -- table ), so a `catch` over it would have to hold a
\     BUILDER on the refusal arm and a TABLE on the success arm. No effect can say
\     that today, so E-UNSET still strands its builder;
\   - WITH-SLOT's E-SLOT/E-EXTENT still strand the arm owner AND the table, because
\     MATCH had already deconstructed the store when the throw fired.
\ Both remaining residues are the same missing capability - the linear-scope
\ combinator named further down, which is what would let a scope dispose the owner it
\ holds on the throw path - and not a missing exit. The suite's leak accounting names
\ which of its strands is which.
\
\ A STORE HELD AS ONE CELL (the residency handle). A store is a bundle of two
\ owners, so it is three stack cells wide, and a field names ONE value: a bound
\ model cannot carry a three-cell bundle in a single field whatever the field is
\ allowed to contain. HOLD answers the same store as a single-cell linear
\ `resident`, which a model holds as one checked field, and RESIDENT-DISPOSE is its
\ only exit.
\
\ THE REASON HAS NARROWED, AND THE SHAPE IS STILL RIGHT. This paragraph used to say
\ that a declaration field may not name a compound that transitively contains a
\ linear owner. That restriction is gone: such a field is legal now, and it carries
\ the disposal obligation with it. What remains is the plainer fact above - a store
\ is three cells and a field is one - so the handle is still how a store becomes
\ something a record can hold.
\
\ The handle allocates nothing, and that is a requirement rather than a saving. Its
\ caller is a bind commit: it takes a census's file mapping through
\ SAFET:DETACH-MAPPING, an atomic and terminal step, and may run nothing fallible
\ afterwards, because a throw past a fresh mapping strands it beyond any catch. So
\ the handle's two cells are reserved in every table block by TABLE-NEW and HOLD
\ only writes them. Builder, table and resident are ONE allocation retyped three
\ times, and every transition is total - the same discipline SEAL already follows,
\ carried one step further. The price is two cells per table block, paid whether or
\ not the table is ever held; the alternative was a fallible step in the one place
\ the transaction cannot tolerate one.
\
\ NO PUBLIC WORD RETURNS A RAW POINTER. Weight bytes are reachable only inside
\ the WITH-SLOT quotation, whose declared effect is [ ptr u8 n -- n ]: the result
\ row is one scalar, so a quotation declared to return the span pointer (or any
\ pointer) fails the check statically - that is the escape negative pinned in
\ weight-store-test.f. The store is held OUTSIDE the quotation and returned
\ unchanged. The span itself is ADVISORY, exactly as for SAFET's WITH-TENSOR and
\ WITH-MAPPING: nothing stops a body from deliberately stashing the address and
\ reading it after DISPOSE; that reads freed memory and crashes, exactly as it
\ would in any language without region types. Escaping the scope takes a
\ deliberate stash - the checker cannot yet express "this pointer may not outlive
\ this call", so the guarantee here is the shape of the interface, not a proof.
\ Closing that gap needs the same pointer-lifetime / region-type checker
\ capability the SAFET header names, tracked by the capability dot the
\ orchestrator is queuing for it; the span here is claimed no more strongly than
\ WITH-TENSOR's.
\
\ THE PARKED FRAME. A quotation cannot read the enclosing word's locals, so the
\ mapped arm cannot hand the user body to SAFET:WITH-MAPPING's own quotation
\ directly. WITH-SLOT parks the body and the slot's byte range in package cells
\ (the lib/memory.f WB-CUR-* pattern). The frame is one set of cells, and
\ WITH-SLOT DOES nest: a body cannot bring the outer store in, but it can MINT a
\ second store, read it, and dispose it, re-entering WITH-SLOT mid-flight - so
\ the inner call rewrites every parked cell while the outer call is live. What
\ keeps that correct is not exclusion but ORDERING, and each fact is
\ load-bearing:
\   1. the serve helpers compute the slot sub-span from WS-OFF/WS-LEN BEFORE
\      running the body, so an inner call rewriting the parked range cannot
\      move the outer span;
\   2. RUN-PARKED reads WS-BODY exactly once, immediately before execute, so an
\      inner PARK cannot redirect the outer body;
\   3. WS-RES and WS-RAN are written only AFTER the body returns, so the outer
\      verdict overwrites whatever the inner left - including the zeroed
\      ran-flag of an inner refusal the body caught.
\ Reordering any of these breaks nesting silently; the nested regressions in
\ weight-store-test.f (including the aborted-inner-under-catch case) pin all
\ three.
\
\ THROWS AND STRANDED OWNERS. WITH-SLOT throws E-SLOT on an out-of-range slot
\ and E-EXTENT on a row outside the arm's bytes. A throw unwinds past the
\ deconstructed store, so the caller's catch cannot recover it. A WITH form that
\ disposes its owner on the throw path needs the tracked linear-scope checker
\ capability. LIVE makes such strands observable: it counts undisposed
\ WSTORE-owned blocks and decides nothing.

require lib/errors.f                     \ E-MEM-* via lib/memory.f load order
require lib/string.f                     \ BYTE+
require lib/cad-num-arithmetic.f         \ byte-off/byte-len roles + ADVANCE-BYTE-OFF
require lib/memory.f                     \ MEM: typed allocation and release
require lib/adt/option.f                 \ option<n> from SAFET:WITH-MAPPING
require lib/adt/result.f                 \ result<n,n> cleanup outcome
require maki/infer/safetensors.f         \ SAFET:mapping, WITH-MAPPING, UNMAP-MAPPING

package WSTORE

public

\ ---- named throw codes (this module owns -7710..-7714) ----------------------
-7710 constant E-SLOTS     \ TABLE-NEW slot count nonpositive or above MAX-SLOTS
-7711 constant E-SLOT      \ slot index outside the table (SLOT! or WITH-SLOT)
-7712 constant E-SET       \ SLOT! on a slot that is already set
-7713 constant E-UNSET     \ SEAL on a builder with an unset slot
-7714 constant E-EXTENT    \ row end overflows, or the row does not fit the arm's bytes
-7715 constant E-ARM       \ a resident's parked arm tag is neither mapped nor allocated

\ ---- the residency choice (the contract's "policy" enum; see the naming note) --
ENUM residency mapped allocated ;ENUM

\ ---- the four linear owners -------------------------------------------------
DEFLINEAR WSTORE:tbuilder      \ one table under construction
DEFLINEAR WSTORE:table         \ one sealed immutable slot table
DEFLINEAR WSTORE:buffer        \ one owned byte buffer (the allocated arm's bytes)
DEFLINEAR WSTORE:resident      \ one store, held as a single cell (see the header)

\ ---- the residency value. The generated WSTORE-STORE:MAPPED / :ALLOCATED are
\ the public store constructors for the two arms; both are total. --------------
ENUM store 0
   VARIANT mapped FIELD map SAFET:mapping FIELD mtbl WSTORE:table ;VARIANT
   VARIANT allocated FIELD buf WSTORE:buffer FIELD atbl WSTORE:table ;VARIANT
;ENUM

private

\ ---- capacities and block layout ----------------------------------------------
$10000 constant MAX-SLOTS      \ slots per table; GPT-2 needs 4 + 13*nlayer = 160
0 cells constant NSLOTS-OFF    \ slot count
1 cells constant NSET-OFF      \ how many slots SLOT! has populated

\ The residency handle's two cells, reserved in EVERY table block from the moment
\ TABLE-NEW allocates it. They are empty until HOLD fills them, and reserving them
\ up front is what makes HOLD allocation-free and therefore TOTAL - see the header.
2 cells constant RES-ARM-OFF   \ which arm HOLD parked here (RA-*)
3 cells constant RES-OWN-OFF   \ that arm's own owner, as one parked cell
4 cells constant ROWS-OFF      \ first row; rows are (off, len, set)
4 constant HDR-CELLS           \ the four cells above, before the first row
3 constant ROW-CELLS
0 constant C-OFF               \ row byte offset into the arm's bytes
1 constant C-LEN               \ row byte extent
2 constant C-SET               \ 1 once SLOT! wrote this row

\ The parked arm tag. RA-NONE is 0 so a block INIT-BLOCK cleared and HOLD never
\ touched cannot read back as a real arm: the tag a resident carries is one a HOLD
\ arm wrote, or the handle is refused.
0 constant RA-NONE             \ never held
1 constant RA-MAPPED           \ the mapped arm's SAFET:mapping is parked
2 constant RA-ALLOC            \ the allocated arm's WSTORE:buffer is parked

\ ---- one buffer record ----------------------------------------------------------
\ A buffer owner outlives the raw (base, extent) pair it was minted from, so it
\ owns a two-cell record of its own, allocated by BUFFER and freed by DISPOSE.
\ Cell 0 holds the base pointer (`ptr-field`, a cell INDEX); the extent is a raw
\ byte offset cell, exactly like the SAFET mapping record.
0 constant RB-BASE-IDX         \ ptr u8: first byte of the owned buffer
1 cells constant RB-LEN-OFF    \ owned byte extent (raw cell; re-typed at release)
2 cells constant RB-BYTES

\ ---- audited representation boundary --------------------------------------------
\ The owner tokens ARE their blocks, so every leaf is an identity or a one-cell
\ duplication; the checker cannot express "this pointer is a live WSTORE block".
\ All of these stay package-private behind the seal (proved in weight-store-test.f).
TRUSTED: MINT-TBUILDER ( ptr u8 -- WSTORE:tbuilder ) ;

TRUSTED: TB>BLOCK ( WSTORE:tbuilder -- WSTORE:tbuilder ptr n )
   dup ;

TRUSTED: TB>TABLE ( WSTORE:tbuilder -- WSTORE:table ) ;

TRUSTED: TBL>BLOCK ( WSTORE:table -- WSTORE:table ptr n )
   dup ;

TRUSTED: TAKE-TABLE ( WSTORE:table -- ptr n ) ;

TRUSTED: MINT-BUFFER ( ptr u8 -- WSTORE:buffer ) ;

TRUSTED: BUF>REC ( WSTORE:buffer -- WSTORE:buffer ptr n )
   dup ;

TRUSTED: TAKE-BUFFER ( WSTORE:buffer -- ptr n ) ;

\ The residency handle is the store's own table block retyped once more, so its
\ mint and its consume are the same identities SEAL and TAKE-TABLE already are.
TRUSTED: MINT-RESIDENT ( ptr u8 -- WSTORE:resident ) ;

TRUSTED: TAKE-RESIDENT ( WSTORE:resident -- ptr n ) ;

\ The held arm's own owner, parked in the block as one raw cell. A DEFLINEAR carries
\ no fields, so a single-cell owner cannot be stored as a typed value; these four
\ leaves are the only crossing, they are package-private with no public inverse, and
\ between a park and its recovery the checker cannot see the owner. What holds
\ "owned exactly once" over that stretch is this file's structure - one park site per
\ arm inside HOLD, one recovery site inside RESIDENT-DISPOSE, no accessor - plus the
\ LIVE and SAFET:LIVE-OWNERS counters the suite asserts around every transaction.
\ They retire with the linear-scope combinator, habu-checker-linear-scope-6218899c.
TRUSTED: MAP>N ( SAFET:mapping -- n ) ;
TRUSTED: N>MAP ( n -- SAFET:mapping ) ;
TRUSTED: BUF>N ( WSTORE:buffer -- n ) ;
TRUSTED: N>BUF ( n -- WSTORE:buffer ) ;

\ Byte view of a table block or buffer record, applied only to an address one of
\ the leaves above just produced; the release path still consumes `ptr u8`.
TRUSTED: BLK>BYTES ( ptr n -- ptr u8 ) ;

\ ---- audited proof-erasure projections (the byte-buffer.f BLEN>N discipline) ----
\ Read a validated role's raw cell for row storage and pointer arithmetic, which
\ still consume a bare `n`. Package-private, no public inverse, so offset and
\ extent roles cannot round-trip through a raw cell by accident.
TRUSTED: BOFF>N ( CAD-NUM:byte-off -- n ) ;
TRUSTED: BLEN>N ( CAD-NUM:byte-len -- n ) ;
TRUSTED: ABLEN>N ( CAD-NUM:alloc-byte-len -- n ) ;

\ ---- the parked access frame (nesting-safe by ordering; see the header note) ----
variable WS-BODY               \ the user body xt for the innermost WITH-SLOT call
variable WS-OFF                \ the served row's byte offset
variable WS-LEN                \ the served row's byte extent
variable WS-RES                \ what the body returned
variable WS-RAN                \ 1 once the body actually ran

\ The two quotation-plumbing leaves (the lib/memory.f WB-RUN-CUR pattern): a
\ quotation cannot read the caller's locals, so the body xt crosses through a
\ package cell, and executing an xt read back from a cell is unchecked.
\ RUN-PARKED's line order IS the nesting contract (header facts 2 and 3):
\ WS-BODY is read once immediately before execute, and WS-RES/WS-RAN are
\ written only after the body returns - a nested WITH-SLOT inside that execute
\ rewrites every frame cell, and the two writes after it are what restore the
\ outer call's verdict.
TRUSTED: PARK ( [ ptr u8 n -- n ] -- )
   WS-BODY ! ;

TRUSTED: RUN-PARKED ( ptr u8 n -- )
   WS-BODY @ execute WS-RES !
   1 WS-RAN ! ;

\ ---- block accessors -------------------------------------------------------------
variable LIVE-N                \ undisposed WSTORE-owned blocks (accounting only)

: ROW@ ( ptr n n n -- n ) {: blk:ptr row:n col:n :}
   blk ROWS-OFF + row ROW-CELLS * col + cells + @ ;

: ROW! ( ptr n n n n -- ) {: blk:ptr row:n col:n value:n :}
   value  blk ROWS-OFF + row ROW-CELLS * col + cells +  ! ;

: NSLOTS@ ( ptr n -- n ) {: blk:ptr :}
   blk NSLOTS-OFF + @ ;

: TB-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len )
   ROW-CELLS * HDR-CELLS + cells MEM:BYTES-ALLOC-LEN ;

: RB-ALLOC ( -- CAD-NUM:alloc-byte-len )
   RB-BYTES MEM:BYTES-ALLOC-LEN ;

\ The record allocation BUFFER-NEW runs under `catch`, parked because a caught quotation
\ must be stack-neutral and cannot hand a value back out (the lib/memory.f WB-CUR pattern).
PTR-VARIABLE RB-PEND

: RB-STEP ( -- )
   RB-ALLOC MEM:ALLOC-BYTES drop RB-PEND ! ;

: SLOT-GUARD ( ptr n n -- ) {: blk:ptr slot:n :}
   slot 0 <  slot blk NSLOTS@ >=  or if E-SLOT throw then ;

: INIT-BLOCK ( ptr n n -- ) {: blk:ptr nslots:n :}
   nslots blk NSLOTS-OFF + !
   0 blk NSET-OFF + !
   RA-NONE blk RES-ARM-OFF + !                 \ no residency parked yet
   0 blk RES-OWN-OFF + !
   nslots 0 ?do blk i C-SET 0 ROW! loop ;

\ ---- SLOT! helpers ------------------------------------------------------------
\ The row end (off + len) is proven overflow-free HERE, on the validated roles,
\ so WITH-SLOT's extent comparison below can use raw cells safely.
: END-OK ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- )
   MATCH CAD-NUM:numeric-result
      ok OF drop ENDOF                        negative OF E-EXTENT throw ENDOF
      zero OF E-EXTENT throw ENDOF             overflow OF E-EXTENT throw ENDOF
      underflow OF E-EXTENT throw ENDOF        bad-alignment OF E-EXTENT throw ENDOF
      misaligned OF E-EXTENT throw ENDOF
   ;MATCH ;

: CHECK-NSLOTS ( n -- ) {: nslots:n :}
   nslots 0 <=  nslots MAX-SLOTS >  or if E-SLOTS throw then ;

\ ---- WITH-SLOT internals ---------------------------------------------------------
\ ROW-ARGS parks one validated row; both arms then serve the same parked range,
\ which is what makes access identical over both arms. A serve helper that finds
\ the row outside the arm's bytes runs nothing, so TAKE-RESULT reports E-EXTENT.
: ROW-ARGS ( WSTORE:table n -- WSTORE:table ) {: slot:n :}
   TBL>BLOCK {: blk:ptr :}
   blk slot SLOT-GUARD
   blk slot C-OFF ROW@ WS-OFF !
   blk slot C-LEN ROW@ WS-LEN !
   0 WS-RAN ! ;

: MAP-BODY ( SAFET:mapping ptr u8 n -- SAFET:mapping ) {: ba:ptr blen:n :}
   WS-OFF @ WS-LEN @ + blen > if exit then
   ba WS-OFF @ BYTE+  WS-LEN @  RUN-PARKED ;

: BUF-SERVE ( WSTORE:buffer -- WSTORE:buffer )
   BUF>REC {: rec:ptr :}
   WS-OFF @ WS-LEN @ +  rec RB-LEN-OFF + @  > if exit then
   rec RB-BASE-IDX ptr-field @  WS-OFF @ BYTE+  WS-LEN @  RUN-PARKED ;

: OPT-DROP ( option<n> -- )
   MATCH option
      none OF ENDOF
      some OF drop ENDOF
   ;MATCH ;

: TAKE-RESULT ( -- n )
   WS-RAN @ 0= if E-EXTENT throw then
   WS-RES @ ;

: MAPPED-SLOT ( n SAFET:mapping WSTORE:table -- WSTORE:store )
   rot ROW-ARGS
   swap [: MAP-BODY ;] SAFET:WITH-MAPPING OPT-DROP
   swap WSTORE-STORE:MAPPED ;

: ALLOC-SLOT ( n WSTORE:buffer WSTORE:table -- WSTORE:store )
   rot ROW-ARGS
   swap BUF-SERVE
   swap WSTORE-STORE:ALLOCATED ;

\ ---- DISPOSE internals -------------------------------------------------------------
\ One block free for BOTH block owners. A builder and a sealed table are the same
\ allocation - SEAL retypes it, it does not copy it - so there is exactly one place
\ that computes a block's byte length from its slot count and hands it back, and the
\ length it computed is left on the stack because every exit that reports a result
\ needs it. Recomputing that arithmetic per exit is how two callers drift apart.
: BLK-FREE ( ptr n -- n )                      \ frees one builder/table block; n = bytes given back
   {: blk:ptr :}
   blk NSLOTS@ TB-ALLOC-LEN ABLEN>N {: blen:n :}
   blk BLK>BYTES blen MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   -1 LIVE-N +!
   blen ;

: TBL-FREE ( WSTORE:table -- )
   TAKE-TABLE BLK-FREE drop ;

: MAPPED-DISPOSE ( SAFET:mapping WSTORE:table -- result<n,n> )
   TBL-FREE
   SAFET:UNMAP-MAPPING ;

: BUF-REL ( ptr u8 n -- ptr u8 n ) {: base:ptr blen:n :}   \ stack-preserving for catch
   base blen MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   base blen ;

\ Reads the record and frees it BEFORE the fallible release, so a failing munmap
\ cannot leak the record; the conversion to a result happens here for the same
\ reason SAFET:UNMAP-MAPPING does it - a caller disposing several owners must see
\ a failed release without unwinding past the owners it has not disposed of yet.
: BUF-FREE ( WSTORE:buffer -- result<n,n> )
   TAKE-BUFFER {: rec:ptr :}
   rec RB-BASE-IDX ptr-field @ {: base:ptr :}
   rec RB-LEN-OFF + @ {: blen:n :}
   rec BLK>BYTES RB-ALLOC MEM:RELEASE-BYTES
   -1 LIVE-N +!
   base blen [: BUF-REL ;] catch {: code:n :}
   2drop
   code 0= if blen RESULT:OK else code RESULT:ERR then ;

: ALLOC-DISPOSE ( WSTORE:buffer WSTORE:table -- result<n,n> )
   TBL-FREE
   BUF-FREE ;

\ ---- HOLD internals: park one arm in the table's own block ---------------------------
\ Consumes the table token and answers the same block as bytes, ready to be minted as
\ a resident. Nothing here can fail: the cells were reserved by TABLE-NEW, the tag is
\ a constant, and the owner cell is a single-cell erasure. That totality is the whole
\ reason the cells are reserved rather than allocated on demand.
: PARK-ARM ( WSTORE:table n n -- ptr u8 ) {: arm:n own:n :}
   TAKE-TABLE {: blk:ptr :}
   arm blk RES-ARM-OFF + !
   own blk RES-OWN-OFF + !
   blk BLK>BYTES ;

: HOLD-MAPPED ( SAFET:mapping WSTORE:table -- WSTORE:resident )
   swap MAP>N {: own:n :}
   RA-MAPPED own PARK-ARM MINT-RESIDENT ;

: HOLD-ALLOC ( WSTORE:buffer WSTORE:table -- WSTORE:resident )
   swap BUF>N {: own:n :}
   RA-ALLOC own PARK-ARM MINT-RESIDENT ;

\ ---- RESIDENT-DISPOSE internals ------------------------------------------------------
\ The block is the table's, so the table token comes back by the same retype SEAL
\ performs, and the arm owner comes back out of its parked cell. Rebuilding the store
\ and handing it to DISPOSE is deliberate: the arm disposal sequence stays written
\ once, in DISPOSE, instead of being copied here where the two could drift apart.
: UNPARK-TABLE ( ptr n -- WSTORE:table )
   BLK>BYTES MINT-TBUILDER TB>TABLE ;

public

\ ---- table construction ------------------------------------------------------------
: TABLE-NEW ( n -- WSTORE:tbuilder )           \ begin a table of n empty slots
   {: nslots:n :}
   nslots CHECK-NSLOTS
   nslots TB-ALLOC-LEN MEM:ALLOC-BYTES drop MINT-TBUILDER
   1 LIVE-N +!
   TB>BLOCK nslots INIT-BLOCK ;

: SLOT! ( WSTORE:tbuilder n CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:tbuilder )
   2dup CAD-NUM:ADVANCE-BYTE-OFF END-OK        \ prove off+len cannot overflow
   BLEN>N {: len:n :}
   BOFF>N {: off:n :}
   {: slot:n :}
   TB>BLOCK {: blk:ptr :}
   blk slot SLOT-GUARD
   blk slot C-SET ROW@ 0 <> if E-SET throw then
   blk slot C-OFF off ROW!
   blk slot C-LEN len ROW!
   blk slot C-SET 1 ROW!
   blk NSET-OFF + @ 1 +  blk NSET-OFF +  ! ;

: SEAL ( WSTORE:tbuilder -- WSTORE:table )     \ full population or a named refusal
   TB>BLOCK {: blk:ptr :}
   blk NSET-OFF + @  blk NSLOTS@  <> if E-UNSET throw then
   TB>TABLE ;

\ ---- the allocated arm's owner -------------------------------------------------------
\ Consumes the exact (base, extent) pair a MEM:ALLOC-BYTES allocation minted,
\ after the loader has filled it. The pair must be that allocation's own; the
\ checker cannot yet bind a pointer to its allocation (the same pointer-lifetime
\ capability gap named in the header), so this crossing is advisory exactly like
\ SAFET:ADOPT's.
: BUFFER ( ptr u8 CAD-NUM:alloc-byte-len -- WSTORE:buffer )
   ABLEN>N {: blen:n :}
   {: base:ptr :}
   RB-ALLOC MEM:ALLOC-BYTES drop MINT-BUFFER
   1 LIVE-N +!
   BUF>REC {: rec:ptr :}
   base rec RB-BASE-IDX ptr-field !
   blen rec RB-LEN-OFF + ! ;

\ ---- allocate the bytes and own them in ONE step -----------------------------------------
\ BUFFER above adopts bytes the caller already owns, which leaves the caller holding raw
\ memory between its own MEM:ALLOC-BYTES and the adoption. That window is small and it is
\ real: every caller has to carry a release-by-hand path for a failure inside it, and such a
\ path is reached only when the second allocation fails, so it is the one branch a test
\ cannot force and review cannot trust. This word removes the window instead of asking each
\ caller to handle it - the bytes and the record are acquired together, and if the record
\ cannot be allocated the bytes go straight back before the failure surfaces, so there is no
\ state in which this package has handed out unowned memory.
\
\ It is the same owner-owns-its-exit argument BUILDER-DISPOSE and BUFFER-DISPOSE make, taken
\ one step earlier: the package that owns the exit should also own the entry.
: BUFFER-NEW ( CAD-NUM:alloc-byte-len -- WSTORE:buffer )
   MEM:ALLOC-BYTES                              \ ( base alen ) owned by nobody yet
   [: RB-STEP ;] catch {: code:n :}
   code 0 <> if
      MEM:RELEASE-BYTES                         \ straight back, before anything is reported
      code throw
   then
   ABLEN>N {: blen:n :}
   {: base:ptr :}
   RB-PEND @ MINT-BUFFER
   1 LIVE-N +!
   BUF>REC {: rec:ptr :}
   base rec RB-BASE-IDX ptr-field !
   blen rec RB-LEN-OFF + ! ;

\ ---- scoped weight access --------------------------------------------------------------
\ Identical over both arms: the slot's bytes exist only inside the body, the
\ result row is one scalar, and the store comes back whole. E-SLOT for a slot the
\ table does not have; E-EXTENT when the row does not fit inside the arm's bytes.
\ typed-local-lint: allow-bare-local - `body` carries the quotation effect
\ [ ptr u8 n -- n ], which a local annotation cannot express.
: WITH-SLOT ( WSTORE:store n [ ptr u8 n -- n ] -- WSTORE:store n )
   {: slot:n body :}
   body PARK
   slot swap
   MATCH store
      mapped    OF MAPPED-SLOT ENDOF
      allocated OF ALLOC-SLOT ENDOF
   ;MATCH
   TAKE-RESULT ;

\ ---- disposal ----------------------------------------------------------------------------
\ The single exit for a store. ok carries the bytes given back (the mapping's
\ extent, or the buffer's; 0 for a mapping that owned nothing), err the named
\ release code.
: DISPOSE ( WSTORE:store -- result<n,n> )
   MATCH store
      mapped    OF MAPPED-DISPOSE ENDOF
      allocated OF ALLOC-DISPOSE ENDOF
   ;MATCH ;

\ ---- disposal of a sealed table that never became a store -------------------------
\ The exit for the table alone (see the header note on why it exists). It frees
\ the same block by the same private path DISPOSE's two arms use, so a table
\ disposed here and a table disposed inside a store cost identically; ok carries
\ the table block's own byte length, which is the only memory this word gives
\ back. The result is the shape, not a wider failure model: releasing the block is
\ the unguarded package-memory free both DISPOSE arms already perform on it, so a
\ failing munmap throws here exactly as it throws through DISPOSE today. What the
\ union buys is that a caller disposing several owners in sequence reads one
\ outcome type across all of them.
: TABLE-DISPOSE ( WSTORE:table -- result<n,n> )
   TAKE-TABLE BLK-FREE RESULT:OK ;

\ ---- disposal of a builder that never got sealed ------------------------------------------
\ The exit for a builder that is not going to become a table. Before this word the
\ only route out of a builder was SEAL, so a caller holding one it could not seal -
\ because a SLOT! refused, or because it decided against the transaction - held a
\ linear token with no exit at all: it could not seal it, it could not drop it (the
\ checker refuses that, correctly), and it could not free the block by hand because
\ the block is package-private. There was not even the fabricate-a-store hack the
\ table's exit retired, since that shape needs a SEALED table. That was the same real
\ leak in this module's surface TABLE-DISPOSE closed for tables, and the module's own
\ suite documented it as six stranded builder blocks.
\
\ It frees the block through the SAME private path both DISPOSE arms and
\ TABLE-DISPOSE use, reached by the audited identity retype SEAL itself uses. That
\ retype is honest here rather than a shortcut around the seal: the two owners ARE
\ one allocation, and the free path reads only the slot count TABLE-NEW wrote before
\ any SLOT! could run, so what it gives back does not depend on how far population
\ got. A builder with no slots populated and a fully populated one cost identically,
\ and ok carries that byte length. The result is the shape, not a wider failure
\ model, for the reason TABLE-DISPOSE gives: a caller unwinding several owners in
\ sequence reads one outcome type across all of them.
: BUILDER-DISPOSE ( WSTORE:tbuilder -- result<n,n> )
   TB>TABLE TAKE-TABLE BLK-FREE RESULT:OK ;

\ ---- disposal of a buffer that never became a store ---------------------------------------
\ The exit for the allocated arm's owner alone. A caller that has filled a buffer and
\ then cannot build the store around it - because the table it needed was refused, or
\ because a later step in its own transaction ran out of memory - owns a buffer and
\ nothing else, and the same argument applies: the package that mints a linear owner
\ owns its exit. The bind transaction's commit phase is exactly that caller, which is
\ why this word exists before the arena copy is written.
\
\ Unlike the two table exits this one can genuinely report err, and it is the same
\ err the allocated arm's DISPOSE reports: the buffer's bytes go back through the
\ guarded MEM release, so a failed release becomes err(code) instead of a throw past
\ owners the caller has not disposed of yet. ok carries the buffer's byte extent -
\ the bytes it owned, not the two-cell record - which is the same number
\ DISPOSE answers for the allocated arm.
: BUFFER-DISPOSE ( WSTORE:buffer -- result<n,n> )
   BUF-FREE ;

\ ---- holding a store as one cell -----------------------------------------------------
\ A store is a two-owner bundle, so it is three stack cells wide, and a field names one
\ value: no record can carry it, whatever a field is allowed to contain. A bound model
\ has to own its residency as ONE field, so this word answers the same store as a
\ single-cell linear handle. (What a field may CONTAIN is no longer the constraint - a
\ field naming a record that transitively owns a linear value is legal today. The width
\ is the reason this handle exists; the old prohibition is not.)
\
\ WHY IT ALLOCATES NOTHING, AND WHY THAT IS THE POINT. HOLD is TOTAL. Its caller is a
\ bind commit, which takes a census's file mapping away through SAFET:DETACH-MAPPING -
\ an atomic, terminal step - and may not run anything that can fail afterwards, because
\ a throw past a fresh mapping strands it where no catch can reach it. A HOLD that
\ allocated would be exactly such a step. So the handle's two cells are reserved in
\ every table block by TABLE-NEW, and HOLD only writes them: builder, table and
\ resident are ONE allocation retyped three times, each transition total, which is the
\ same discipline SEAL already follows.
\
\ WHAT KEEPS IT HONEST AS THE STORE GROWS. The arm is read by MATCH, not by any
\ assumption about the store's width, and MATCH is exhaustiveness-checked: a third
\ residency arm makes this word fail to compile until it is handled here. Nothing in
\ this file depends on how many cells a store happens to occupy.
: HOLD ( WSTORE:store -- WSTORE:resident )
   MATCH store
      mapped    OF HOLD-MAPPED ENDOF
      allocated OF HOLD-ALLOC ENDOF
   ;MATCH ;

\ ---- the residency handle's exit ------------------------------------------------------
\ The single exit for a resident, and the only way back to the store it holds. It
\ reports what the underlying DISPOSE reported - the arm's bytes for ok, the named
\ release code for err - because that is the memory this call actually gave back.
\
\ The tag is validated before any owner is rebuilt. No caller can reach a bad tag: the
\ only writers are HOLD's two arms and INIT-BLOCK's RA-NONE, and a resident can only be
\ minted by HOLD. The test is kept for the reason the module checks a slot at the point
\ it indexes memory - the value is read out of a block and decides which owner is
\ reconstructed, so a wrong answer would hand a mapping to the buffer path. Refusing
\ before anything is rebuilt means the refusal strands no owner.
: RESIDENT-DISPOSE ( WSTORE:resident -- result<n,n> )
   TAKE-RESIDENT {: blk:ptr :}
   blk RES-ARM-OFF + @ {: arm:n :}
   arm RA-MAPPED <>  arm RA-ALLOC <>  and if E-ARM throw then
   blk RES-OWN-OFF + @ {: own:n :}
   blk UNPARK-TABLE                              \ ( table ), the same block retyped back
   arm RA-MAPPED = if
      own N>MAP swap WSTORE-STORE:MAPPED DISPOSE exit
   then
   own N>BUF swap WSTORE-STORE:ALLOCATED DISPOSE ;

\ ---- leak accounting (decides nothing; the SAFET:LIVE-OWNERS pattern) --------------------
: LIVE ( -- n )                                \ undisposed builder/table blocks + buffer records
   LIVE-N @ ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package
