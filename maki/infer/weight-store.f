\ weight-store.f - policy-selectable weight residency for the GB10 UMA inference
\ engine (sealed package WSTORE; epic habu-epic-gb10-uma-391d12e8).
\
\ WHY THIS MODULE EXISTS. A loaded model owns exactly ONE immutable linear
\ residency value - the store - and every weight read goes through it. The store
\ has two arms because the GB10 plan forbids forcing a copy of multi-GiB
\ checkpoints: the MAPPED arm serves weights straight out of the checkpoint's
\ file mapping (the linear SAFET:mapping owner that DETACH-MAPPING hands out),
\ so a 14-25 GiB model needs no second resident copy, while the ALLOCATED arm
\ owns a device-friendly buffer filled once by the loader for the checkpoints
\ where copying pays (measured in docs/gb10-uma-residency.md). Which arm a model
\ uses is the caller's residency choice, named by the payload-free enum below.
\
\ MODEL-AGNOSTIC BY CONSTRUCTION. The store maps typed slot ordinals to byte ranges and
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
\ caller can seal one and then stop before creating a store. A prepared load is
\ exactly that: it owns a sealed table and no storage owner yet, so
\ DISCARD-PREPARED must dispose the table directly. Before TABLE-DISPOSE existed
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
\ WHAT THE EXITS STILL CANNOT REACH, AND WHY THAT IS THE CHECKER'S GAP. SLOT!
\ is stack-preserving, so a caught refusal leaves its builder available for
\ BUILDER-DISPOSE. SEAL changes the owner type, so E-UNSET still strands its
\ builder until the checker can express a catch whose two arms hold different
\ linear types. U32-LE@? refuses as an option value and always preserves its
\ store, so reads add no throw-stranded owner state.
\
\ A MODEL HOLDS THE STORE ITSELF. A declaration field may name a family that
\ transitively contains linear owners, so `FIELD weights WSTORE:store` carries
\ the store's disposal obligation directly. The former one-cell residency wrapper
\ has no remaining purpose; DISPOSE stays the store's single exit.
\
\ NO PUBLIC WORD RETURNS A RAW POINTER. U32-LE@? accepts a nominal slot and a
\ slot-relative nominal byte offset, proves the four-byte window against the
\ slot and the selected arm, and returns option<n> with the unchanged store.
\ Invalid values return none before any backing byte is read. The mapped arm
\ delegates the validated absolute offset to SAFET:U32-LE@?; the allocated arm
\ reads the same four little-endian bytes directly.

require lib/errors.f                     \ E-MEM-* via lib/memory.f load order
require lib/cad-num-arithmetic.f         \ byte-off/byte-len roles + ADVANCE-BYTE-OFF
require lib/memory.f                     \ MEM: typed allocation and release
require lib/adt/option.f                 \ option<n> bounded read result
require lib/adt/result.f                 \ result<n,n> cleanup outcome
require maki/infer/safetensors.f         \ SAFET:mapping, U32-LE@?, UNMAP-MAPPING

package WSTORE

public

\ ---- named throw codes (this module owns -7710..-7714) ----------------------
-7710 constant E-SLOTS     \ TABLE-NEW slot count nonpositive or above MAX-SLOTS
-7711 constant E-SLOT      \ SLOT! index outside the table
-7712 constant E-SET       \ SLOT! on a slot that is already set
-7713 constant E-UNSET     \ SEAL on a builder with an unset slot
-7714 constant E-EXTENT    \ SLOT! row end is not representable

\ ---- the residency choice (the contract's "policy" enum; see the naming note) --
ENUM residency mapped allocated ;ENUM

\ ---- the three linear owners ------------------------------------------------
DEFLINEAR WSTORE:tbuilder      \ one table under construction
DEFLINEAR WSTORE:table         \ one sealed immutable slot table
DEFLINEAR WSTORE:buffer        \ one owned byte buffer (the allocated arm's bytes)

\ ---- the residency value. The generated WSTORE-STORE:MAPPED / :ALLOCATED are
\ the public store constructors for the two arms; both are total. --------------
ENUM store 0
   VARIANT mapped FIELD map SAFET:mapping FIELD mtbl WSTORE:table ;VARIANT
   VARIANT allocated FIELD buf WSTORE:buffer FIELD atbl WSTORE:table ;VARIANT
;ENUM

private

\ ---- capacities and block layout ----------------------------------------------
$10000 constant MAX-SLOTS      \ slots per table; GPT-2 needs 4 + 13*nlayer = 160
4 constant U32-BYTES           \ wire width of one little-endian u32
0 cells constant NSLOTS-OFF    \ slot count
1 cells constant NSET-OFF      \ how many slots SLOT! has populated

2 cells constant ROWS-OFF      \ first row; rows are (off, len, set)
2 constant HDR-CELLS           \ the two cells above, before the first row
3 constant ROW-CELLS
0 constant C-OFF               \ row byte offset into the arm's bytes
1 constant C-LEN               \ row byte extent
2 constant C-SET               \ 1 once SLOT! wrote this row

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

\ Symmetric with MINT-BUFFER on purpose: the token IS the allocation MINT-BUFFER
\ was handed, so consuming it gives back the same `ptr u8`. Declaring the consume
\ as `ptr n` made the pair retype the block's element silently, which only ever
\ certified because a pointee used to admit `n` where `u8` was required. Callers
\ that need the record's CELLS take them from BUF>REC, the audited cell view.
TRUSTED: TAKE-BUFFER ( WSTORE:buffer -- ptr u8 ) ;

\ Byte view of a table block or buffer record, applied only to an address one of
\ the leaves above just produced; the release path still consumes `ptr u8`.
TRUSTED: BLK>BYTES ( ptr n -- ptr u8 ) ;

\ ---- audited proof-erasure projections (the byte-buffer.f BLEN>N discipline) ----
\ Read a validated role's raw cell for row or owner-record storage. Package-private,
\ no public inverse, so offset and extent roles cannot round-trip by accident.
TRUSTED: BOFF>N ( CAD-NUM:byte-off -- n ) ;
TRUSTED: BLEN>N ( CAD-NUM:byte-len -- n ) ;
TRUSTED: ABLEN>N ( CAD-NUM:alloc-byte-len -- n ) ;

\ ---- block accessors -------------------------------------------------------------
variable LIVE-N                \ undisposed WSTORE-owned blocks (accounting only)

: NEED-INDEX ( CAD-NUM:numeric-result<CAD-NUM:index> -- CAD-NUM:index )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-SLOT throw ENDOF
      zero OF E-SLOT throw ENDOF                overflow OF E-SLOT throw ENDOF
      underflow OF E-SLOT throw ENDOF           bad-alignment OF E-SLOT throw ENDOF
      misaligned OF E-SLOT throw ENDOF
   ;MATCH ;

: NEED-COUNT ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- CAD-NUM:item-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-SLOTS throw ENDOF
      zero OF E-SLOTS throw ENDOF               overflow OF E-SLOTS throw ENDOF
      underflow OF E-SLOTS throw ENDOF          bad-alignment OF E-SLOTS throw ENDOF
      misaligned OF E-SLOTS throw ENDOF
   ;MATCH ;

: NEED-OFF ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-EXTENT throw ENDOF
      zero OF E-EXTENT throw ENDOF              overflow OF E-EXTENT throw ENDOF
      underflow OF E-EXTENT throw ENDOF         bad-alignment OF E-EXTENT throw ENDOF
      misaligned OF E-EXTENT throw ENDOF
   ;MATCH ;

: NEED-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-EXTENT throw ENDOF
      zero OF E-EXTENT throw ENDOF              overflow OF E-EXTENT throw ENDOF
      underflow OF E-EXTENT throw ENDOF         bad-alignment OF E-EXTENT throw ENDOF
      misaligned OF E-EXTENT throw ENDOF
   ;MATCH ;

: ROW-WIDTH ( -- CAD-NUM:byte-len )
   ROW-CELLS cells CAD-NUM:BYTE-LEN NEED-LEN ;

: ROW-OFF ( CAD-NUM:index -- CAD-NUM:byte-off )
   ROW-WIDTH CAD-NUM:INDEX-BYTE-OFF NEED-OFF ;

: ROW-ADDR ( ptr n CAD-NUM:index -- ptr n ) {: blk:ptr row:CAD-NUM:index :}
   blk ROWS-OFF + row ROW-OFF BOFF>N + ;

: ROW@ ( ptr n CAD-NUM:index n -- n ) {: blk:ptr row:CAD-NUM:index col:n :}
   blk row ROW-ADDR col cells + @ ;

: ROW! ( ptr n CAD-NUM:index n n -- ) {: blk:ptr row:CAD-NUM:index col:n value:n :}
   value blk row ROW-ADDR col cells + ! ;

: NSLOTS@ ( ptr n -- n ) {: blk:ptr :}
   blk NSLOTS-OFF + @ ;

: NSLOTS ( ptr n -- CAD-NUM:item-count )
   NSLOTS@ CAD-NUM:ITEM-COUNT NEED-COUNT ;

: TB-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len )
   ROW-CELLS * HDR-CELLS + cells MEM:BYTES-ALLOC-LEN ;

: RB-ALLOC ( -- CAD-NUM:alloc-byte-len )
   RB-BYTES MEM:BYTES-ALLOC-LEN ;

\ The record allocation BUFFER-NEW runs under `catch`, parked because a caught quotation
\ must be stack-neutral and cannot hand a value back out (the lib/memory.f WB-CUR pattern).
PTR-VARIABLE RB-PEND

: RB-STEP ( -- )
   RB-ALLOC MEM:ALLOC-BYTES drop RB-PEND ! ;

: SLOT? ( ptr n CAD-NUM:index -- bool ) {: blk:ptr slot:CAD-NUM:index :}
   slot blk NSLOTS CAD-NUM:INDEX-IN-COUNT? ;

: SLOT-GUARD ( ptr n CAD-NUM:index -- ) {: blk:ptr slot:CAD-NUM:index :}
   blk slot SLOT? 0= if E-SLOT throw then ;

: INIT-BLOCK ( ptr n n -- ) {: blk:ptr nslots:n :}
   nslots blk NSLOTS-OFF + !
   0 blk NSET-OFF + !
   nslots 0 ?do blk i CAD-NUM:INDEX NEED-INDEX C-SET 0 ROW! loop ;

\ ---- SLOT! helpers ------------------------------------------------------------
\ The row end is proven representable before its validated roles are stored.
: END-OK ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- )
   MATCH CAD-NUM:numeric-result
      ok OF drop ENDOF                        negative OF E-EXTENT throw ENDOF
      zero OF E-EXTENT throw ENDOF             overflow OF E-EXTENT throw ENDOF
      underflow OF E-EXTENT throw ENDOF        bad-alignment OF E-EXTENT throw ENDOF
      misaligned OF E-EXTENT throw ENDOF
   ;MATCH ;

: CHECK-NSLOTS ( n -- ) {: nslots:n :}
   nslots 0 <=  nslots MAX-SLOTS >  or if E-SLOTS throw then ;

\ ---- bounded u32 reads -----------------------------------------------------------
U32-BYTES 1- constant U32-LAST

: NO ( -- bool )
   0 0 > ;

using CAD-NUM

: U32-REACH ( -- CAD-NUM:byte-len )
   U32-LAST BYTE-LEN NEED-LEN ;

: ZERO-OFF ( -- CAD-NUM:byte-off )
   0 BYTE-OFF NEED-OFF ;

: WINDOW-IN? ( CAD-NUM:byte-off CAD-NUM:byte-len -- bool )
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   off U32-REACH ADVANCE-BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF len BYTE-OFF-IN-LEN? ENDOF
      negative OF NO ENDOF                      zero OF NO ENDOF
      overflow OF NO ENDOF                      underflow OF NO ENDOF
      bad-alignment OF NO ENDOF                 misaligned OF NO ENDOF
   ;MATCH ;

: REL-ABS? ( CAD-NUM:byte-off CAD-NUM:byte-len CAD-NUM:byte-off -- option<CAD-NUM:byte-off> )
   {: start:CAD-NUM:byte-off len:CAD-NUM:byte-len rel:CAD-NUM:byte-off :}
   rel len WINDOW-IN? 0= if OPTION:NONE exit then
   rel ZERO-OFF BYTE-DISTANCE
   MATCH CAD-NUM:numeric-result
      ok OF start swap ADVANCE-BYTE-OFF
            MATCH CAD-NUM:numeric-result
               ok OF OPTION:SOME ENDOF
               negative OF OPTION:NONE ENDOF       zero OF OPTION:NONE ENDOF
               overflow OF OPTION:NONE ENDOF       underflow OF OPTION:NONE ENDOF
               bad-alignment OF OPTION:NONE ENDOF  misaligned OF OPTION:NONE ENDOF
            ;MATCH
      ENDOF
      negative OF OPTION:NONE ENDOF             zero OF OPTION:NONE ENDOF
      overflow OF OPTION:NONE ENDOF             underflow OF OPTION:NONE ENDOF
      bad-alignment OF OPTION:NONE ENDOF        misaligned OF OPTION:NONE ENDOF
   ;MATCH ;

: ROW-OFF? ( WSTORE:table CAD-NUM:index CAD-NUM:byte-off -- WSTORE:table option<CAD-NUM:byte-off> )
   {: slot:CAD-NUM:index rel:CAD-NUM:byte-off :}
   TBL>BLOCK {: blk:ptr :}
   blk slot SLOT? 0= if OPTION:NONE exit then
   blk slot C-OFF ROW@ BYTE-OFF NEED-OFF
   blk slot C-LEN ROW@ BYTE-LEN NEED-LEN
   rel REL-ABS? ;

: BUF-U32? ( WSTORE:buffer CAD-NUM:byte-off -- WSTORE:buffer option<n> )
   {: off:CAD-NUM:byte-off :}
   BUF>REC {: rec:ptr :}
   rec RB-LEN-OFF + @ BYTE-LEN NEED-LEN off swap WINDOW-IN?
   if rec RB-BASE-IDX ptr-field @ off CAD-NUM:BYTE+ RD32 OPTION:SOME
   else OPTION:NONE then ;

: MAPPED-READ ( SAFET:mapping WSTORE:table CAD-NUM:byte-off -- WSTORE:store option<n> )
   {: off:CAD-NUM:byte-off :}
   swap off SAFET:U32-LE@?
   MATCH option
      none OF swap WSTORE-STORE:MAPPED OPTION:NONE ENDOF
      some OF {: value:n :} swap WSTORE-STORE:MAPPED value OPTION:SOME ENDOF
   ;MATCH ;

: MAPPED-U32? ( CAD-NUM:index CAD-NUM:byte-off SAFET:mapping WSTORE:table -- WSTORE:store option<n> )
   2swap ROW-OFF?
   MATCH option
      none OF WSTORE-STORE:MAPPED OPTION:NONE ENDOF
      some OF MAPPED-READ ENDOF
   ;MATCH ;

: ALLOC-READ ( WSTORE:buffer WSTORE:table CAD-NUM:byte-off -- WSTORE:store option<n> )
   {: off:CAD-NUM:byte-off :}
   swap off BUF-U32?
   MATCH option
      none OF swap WSTORE-STORE:ALLOCATED OPTION:NONE ENDOF
      some OF {: value:n :} swap WSTORE-STORE:ALLOCATED value OPTION:SOME ENDOF
   ;MATCH ;

: ALLOC-U32? ( CAD-NUM:index CAD-NUM:byte-off WSTORE:buffer WSTORE:table -- WSTORE:store option<n> )
   2swap ROW-OFF?
   MATCH option
      none OF WSTORE-STORE:ALLOCATED OPTION:NONE ENDOF
      some OF ALLOC-READ ENDOF
   ;MATCH ;

;using

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
   BUF>REC {: rec:ptr :}                       \ the record's CELLS, token still held
   rec RB-BASE-IDX ptr-field @ {: base:ptr :}
   rec RB-LEN-OFF + @ {: blen:n :}
   TAKE-BUFFER RB-ALLOC MEM:RELEASE-BYTES      \ the allocation's BYTES, token consumed
   -1 LIVE-N +!
   base blen [: BUF-REL ;] catch {: code:n :}
   2drop
   code 0= if blen RESULT:OK else code RESULT:ERR then ;

: ALLOC-DISPOSE ( WSTORE:buffer WSTORE:table -- result<n,n> )
   TBL-FREE
   BUF-FREE ;

public

\ ---- table construction ------------------------------------------------------------
: TABLE-NEW ( n -- WSTORE:tbuilder )           \ begin a table of n empty slots
   {: nslots:n :}
   nslots CHECK-NSLOTS
   nslots TB-ALLOC-LEN MEM:ALLOC-BYTES drop MINT-TBUILDER
   1 LIVE-N +!
   TB>BLOCK nslots INIT-BLOCK ;

: SLOT! ( WSTORE:tbuilder CAD-NUM:index CAD-NUM:byte-off CAD-NUM:byte-len -- WSTORE:tbuilder )
   2dup CAD-NUM:ADVANCE-BYTE-OFF END-OK        \ prove off+len cannot overflow
   BLEN>N {: len:n :}
   BOFF>N {: off:n :}
   {: slot:CAD-NUM:index :}
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

\ ---- bounded weight access -------------------------------------------------------------
\ `off` is relative to the selected slot. NONE covers an absent slot, numeric
\ overflow, a four-byte window crossing the slot, or a row/window crossing the
\ residency arm. No backing byte is touched until all applicable bounds pass.
: U32-LE@? ( WSTORE:store CAD-NUM:index CAD-NUM:byte-off -- WSTORE:store option<n> )
   {: slot:CAD-NUM:index off:CAD-NUM:byte-off :}
   slot off rot
   MATCH store
      mapped    OF MAPPED-U32? ENDOF
      allocated OF ALLOC-U32? ENDOF
   ;MATCH ;

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
\ because a SLOT! refused, or because it decided against the load - held a
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
\ because a later step in its own load ran out of memory - owns a buffer and
\ nothing else, and the same argument applies: the package that mints a linear owner
\ owns its exit. LOAD-COPIED is exactly that caller, which is
\ why this word exists before the copy buffer is filled.
\
\ Unlike the two table exits this one can genuinely report err, and it is the same
\ err the allocated arm's DISPOSE reports: the buffer's bytes go back through the
\ guarded MEM release, so a failed release becomes err(code) instead of a throw past
\ owners the caller has not disposed of yet. ok carries the buffer's byte extent -
\ the bytes it owned, not the two-cell record - which is the same number
\ DISPOSE answers for the allocated arm.
: BUFFER-DISPOSE ( WSTORE:buffer -- result<n,n> )
   BUF-FREE ;

\ ---- leak accounting (decides nothing; the SAFET:LIVE-OWNERS pattern) --------------------
: LIVE ( -- n )                                \ undisposed builder/table blocks + buffer records
   LIVE-N @ ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package
