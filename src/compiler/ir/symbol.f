\ symbol.f - the compiler symbol interner: deterministic module-local symbol
\ identities backed by content bytes, with byte equality and no pointer
\ identity.
\
\ docs/compiler-ir-design.md section 6.3 (the symbol table), section 6.6 (the
\ string and symbol tables in the canonical serialization order), and plan
\ item IR-0.3. One interner serves one module as two coupled stores, both
\ IR-ARENA arenas owned by the compilation context: a byte pool holding the
\ symbol bytes and a row table holding one record per distinct symbol. Both
\ die with their context, freeze with their arenas, geometric growth under the
\ committed ceilings is the arena's own doubling, and symbol identities are
\ the existing IR-ID ir-symbol-id family packed under the module key; this
\ file mints no parallel identity family and no raw converter.
\
\ STORE SHAPES. Each arena carries a three-cell header (format tag, owning
\ module serial, committed capacity). The pool's data cells hold symbol bytes
\ packed eight per cell little-endian; every symbol starts on a fresh cell and
\ the last cell's tail is zero-padded. That keeps the arena the single storage
\ owner - no side allocation, no SCRATCH-TAKE bookkeeping - and keeps appends
\ whole, at a cost of at most seven padding bytes per symbol; the committed
\ byte capacity is therefore accounted in whole cells. The row table stores
\ one three-cell row per symbol: content filter, starting pool data cell, and
\ byte length. Every row access rechecks the header shape and revalidates that
\ row's start and length against the pool's live cells fail-closed
\ (SPAN-CK-N, E-IR-SYM-STATE), so a holder who bypasses this package and
\ appends raw cells cannot make a reader touch cells outside the pool's live
\ range. The third cell, the content filter, is not rechecked; see THE FILTER
\ CELL IS TRUSTED below.
\
\ INTERNING IS BYTE EQUALITY. INTERN answers the existing identity when the
\ presented bytes equal a stored symbol's bytes, and mints the next
\ module-local ordinal otherwise. The row's filter cell - the low sixteen
\ bits of the bytes' SHA-256 word 0 (CDIGEST:COMPUTE) - is a cheap reject
\ filter, never identity: every filter-and-length match is confirmed by
\ comparing the stored bytes cell by cell, so two different strings sharing a
\ filter stay distinct. Pointer identity plays no part; equal bytes presented
\ from different buffers intern to one identity. A duplicate intern allocates
\ nothing and therefore does not consult the context: the ctx argument is
\ allocation authority for the miss path, exactly as the readers below take
\ no ctx at all.
\
\ THE FILTER CELL IS TRUSTED. Of a row's three cells only the start and the
\ length are revalidated on access. Nothing ever recomputes the stored filter
\ from the bytes it stands for, so ROW-MATCH? reads that cell on faith, and it
\ is the one row cell whose protection is IR-ARENA ownership - only ROW-ADD
\ writes it, into an arena checked code cannot forge cells into - rather than
\ a recheck at read time. The consequence splits in two, and
\ formal/Common/Interning.v proves the halves separately. Soundness does not
\ depend on the cell: Symbols.sym_row_match_sound holds for an arbitrary
\ stored filter value, because the byte comparison runs behind the filter
\ test, so two different symbols never merge into one identity however wrong
\ that cell is. Completeness does depend on it:
\ Symbols.sym_row_match_is_byte_equality needs the stored cell to be the
\ honest filter of the stored bytes before the three-part test is byte
\ equality. A wrong cell makes the scan walk past a row that does hold the
\ presented bytes and mint a second identity for them, and duplicate rows
\ would also break the induction over ordinals type.f relies on, which assumes
\ two rows of one table are structurally equal exactly when they are the same
\ row.
\
\ OWNERSHIP. The same possession discipline as source.f: holding the context,
\ the two store handles, and the module key IS interner ownership, and each is
\ a sealed nominal checked code cannot forge from raw cells. Both headers bind
\ to the key's module serial and every operation rechecks that the two stores
\ carry the same serial (E-IR-SYM-OWNER), so a foreign key, a foreign
\ module's symbol-id, and a cross-module store pairing all reject before any
\ row is touched. Creating a second store pair under one key is the owner's
\ misuse, as it is for the source registry.
\
\ INSERTION ORDER AND CANONICALIZATION. Local ordinals are insertion-ordered
\ and stable; the section 6.6 canonical encoding must not depend on that
\ order. What canonicalization needs from this file is a deterministic,
\ byte-faithful iteration surface, and that is SYMBOLS/FSYMBOLS plus the
\ ordinal-packed identities resolved through LEN@ and COPY: the encoder sorts
\ symbols by their bytes and emits its own permutation. The filter cell is an
\ in-memory scan aid only and never serializes.
\
\ NO POINTER ESCAPES. Readers copy bytes into a caller span or compare bytes
\ in place; no public word returns a pointer into the context mapping, so
\ nothing a caller can hold outlives the context but the sealed identities,
\ which fail closed once the stores are gone.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-SYM
private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner
\ comparison. Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$53594231 constant SYB-MAGIC         \ "SYB1": the byte-pool header format tag
$53594D31 constant SYM-MAGIC         \ "SYM1": the row-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-FLT
1 constant OFF-START
2 constant OFF-LEN
3 constant ROW-CELLS
8 constant CELL-BYTES
$FFFF constant FILTER-MASK
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CELL-MAX
POOL-CELL-MAX CELL-BYTES * constant BYTE-MAX

: BYTES>CELLS ( n -- n )
   CELL-BYTES 1- + CELL-BYTES / ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-SYM-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-SYM-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-SYM-STATE throw then ;

: PMAGIC-CK ( n -- )
   SYB-MAGIC <> if E-IR-SYM-STATE throw then ;

: RMAGIC-CK ( n -- )
   SYM-MAGIC <> if E-IR-SYM-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a HC-MAGIC LCELL@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r IR-ARENA:USED RSHAPE-CK
   r HC-MAGIC LCELL@ RMAGIC-CK ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE PSHAPE-CK
   v HC-MAGIC FCELL@ PMAGIC-CK ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE RSHAPE-CK
   v HC-MAGIC FCELL@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED USED>CNT ;

: FCNT ( IR-ARENA:view -- n )
   IR-ARENA:SIZE USED>CNT ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ;

: FPCELLS ( IR-ARENA:view -- n )
   IR-ARENA:SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-SYM-OWNER throw then ;

\ The pair coupling: both stores are what their tags claim and both carry the
\ same owning module serial, so a cross-module pairing rejects before any row
\ span is trusted against the wrong pool.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a PHDR-CK
   r RHDR-CK
   a HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FPAIR-CK ( IR-ARENA:view IR-ARENA:view -- )
   {: pv:IR-ARENA:view rv:IR-ARENA:view :}
   pv FPHDR-CK
   rv FRHDR-CK
   pv HC-SERIAL FCELL@ rv HC-SERIAL FCELL@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a r PAIR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

\ Validate a presented symbol-id against a resolved (header serial, count):
\ minted under this interner's module, ordinal below the interned count.
: ID-CK-N ( n n IR-ID:ir-symbol-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-symbol-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:SYMBOL-LOCAL
   dup cnt >= if E-IR-SYM-BOUND throw then ;

: ID-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ r CNT id ID-CK-N ;

: FID-CK ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-symbol-id :}
   v FRHDR-CK
   v HC-SERIAL FCELL@ v FCNT id ID-CK-N ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: DC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

: FDC@ ( IR-ARENA:view n -- n )
   HDR-CELLS + FCELL@ ;

\ A row's byte span revalidates against the pool's live cell range on every
\ access, so a forged or bypass-appended row rejects fail-closed instead of
\ reading cells the pool never stored.
: SPAN-CK-N ( n n n -- )
   {: pc:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SYM-STATE throw then
   st ln BYTES>CELLS + pc > if E-IR-SYM-STATE throw then ;

: ROW-START ( IR-ARENA:arena IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-START RC@ {: st:n :}
   a PCELLS st r l OFF-LEN RC@ SPAN-CK-N
   st ;

: FROW-START ( IR-ARENA:view IR-ARENA:view n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n :}
   rv l OFF-START FRC@ {: st:n :}
   pv FPCELLS st rv l OFF-LEN FRC@ SPAN-CK-N
   st ;

\ ---- byte packing ------------------------------------------------------------
\ Build the pool cell for the presented bytes at byte offset j: up to eight
\ bytes little-endian, missing tail bytes zero. Packing is deterministic, so
\ byte equality of two symbols is exactly cell equality of their packed spans.
: PACK-CELL ( ptr u8 n n -- n )
   {: p u:n j:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   0
   CELL-BYTES 0 ?do
      j i + u < if
         p j i + + c@  i 8 * lshift  or
      then
   loop ;

: CELL-BYTE ( n n -- n )
   8 * rshift $FF and ;

: PBYTE@ ( IR-ARENA:arena n n -- n )
   {: a:IR-ARENA:arena st:n i:n :}
   a st i CELL-BYTES / + DC@  i CELL-BYTES mod CELL-BYTE ;

: FPBYTE@ ( IR-ARENA:view n n -- n )
   {: pv:IR-ARENA:view st:n i:n :}
   pv st i CELL-BYTES / + FDC@  i CELL-BYTES mod CELL-BYTE ;

\ The verify step of interning and of every equality probe: compare the
\ stored packed cells with the presented bytes packed the same way.
: BYTES-EQ ( IR-ARENA:arena n ptr u8 n -- bool )
   {: a:IR-ARENA:arena st:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   u BYTES>CELLS 0 ?do
      a st i + DC@  p u i CELL-BYTES * PACK-CELL <> if
         false unloop exit
      then
   loop
   true ;

: FBYTES-EQ ( IR-ARENA:view n ptr u8 n -- bool )
   {: pv:IR-ARENA:view st:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   u BYTES>CELLS 0 ?do
      pv st i + FDC@  p u i CELL-BYTES * PACK-CELL <> if
         false unloop exit
      then
   loop
   true ;

public

\ The deterministic content filter a symbol row stores: the low sixteen bits
\ of the bytes' SHA-256 word 0. A pure function of the bytes and never
\ identity - a filter match is always confirmed by comparing bytes - it is
\ public so fixtures can force two different strings through one filter and
\ prove the verify step discriminates. It never serializes.
: FILTER ( ptr u8 n -- n )
   CDIGEST:COMPUTE CDIGEST-DIGEST:UNMAKE
   drop drop drop FILTER-MASK and ;

private

\ ---- scan --------------------------------------------------------------------
: ROW-MATCH? ( IR-ARENA:arena IR-ARENA:arena n ptr u8 n n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n p u:n f:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   r l OFF-FLT RC@ f <> if false exit then
   r l OFF-LEN RC@ u <> if false exit then
   a  a r l ROW-START  p u BYTES-EQ ;

: SCAN ( IR-ARENA:arena IR-ARENA:arena ptr u8 n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena p u:n f:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   -1
   r CNT 0 ?do
      a r i p u f ROW-MATCH? if drop i leave then
   loop ;

\ ---- creation ----------------------------------------------------------------
: SYM-CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-SYM-CAP throw then
   drop ;

: BYTE-CAP-OK ( n -- )
   dup 1 < over BYTE-MAX > or if E-IR-SYM-BYTES throw then
   drop ;

\ ---- append ------------------------------------------------------------------
\ Both room checks run before the first cell is written and each arena's
\ ceiling equals its committed capacity exactly, so an intern either appends
\ its bytes and its row whole or mutates nothing.
: ROOM-CK ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena u:n :}
   r CNT r HC-CAP LCELL@ >= if E-IR-SYM-CAP throw then
   a PCELLS u BYTES>CELLS + a HC-CAP LCELL@ > if E-IR-SYM-BYTES throw then ;

\ An intern writes BOTH arenas, so both are reserved here, before either is
\ touched: a row whose bytes went in and whose row did not is a symbol table
\ that no longer divides into rows. Reserving after the room checks keeps this
\ table's own named capacity errors ahead of the arena's.
: ROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena u:n :}
   c a u BYTES>CELLS IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE ;

: POOL-ADD ( IR-CTX:ctx IR-ARENA:arena ptr u8 n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   a PCELLS {: st:n :}
   u BYTES>CELLS 0 ?do
      c a  p u i CELL-BYTES * PACK-CELL  IR-ARENA:PUSH drop
   loop
   st ;

: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n -- n )
   {: c:IR-CTX:ctx r:IR-ARENA:arena f:n st:n u:n :}
   r CNT {: l:n :}
   c r f IR-ARENA:PUSH drop
   c r st IR-ARENA:PUSH drop
   c r u IR-ARENA:PUSH drop
   l ;

public

\ Create a module's symbol interner: the byte pool committed to at least bcap
\ bytes (accounted in whole cells) and the row table committed to exactly
\ scap symbols, both headers bound to key's module serial. The two handles
\ plus the key are the interner; all three stay with the module owner, and
\ the interner dies with the owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key scap:n bcap:n :}
   scap SYM-CAP-OK
   bcap BYTE-CAP-OK
   c bcap BYTES>CELLS HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a HDR-CELLS IR-ARENA:RESERVE
   c a SYB-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a bcap BYTES>CELLS IR-ARENA:PUSH drop
   c scap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r SYM-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r scap IR-ARENA:PUSH drop
   a r ;

\ Intern the presented bytes: equal bytes answer the identity they already
\ hold - the same bytes twice, from any buffer, are one symbol - and new
\ bytes mint the next module-local identity under key. A hit allocates
\ nothing; a miss appends bytes and row whole or throws with nothing written.
: INTERN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   u 0 < if E-IR-SYM-LEN throw then
   a r key KEY-CK
   p u FILTER {: f:n :}
   a r p u f SCAN {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-SYMBOL exit then
   a r u ROOM-CK
   c a r u ROOM-TAKE
   c a p u POOL-ADD {: st:n :}
   c r f st u ROW-ADD
   key swap IR-ID:PACK-SYMBOL ;

\ ---- live readers ------------------------------------------------------------
: SYMBOLS ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

: LEN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ ;

\ Byte equality between a symbol and a presented span - the observable form
\ of the interning invariant, with no pointer crossing the boundary.
: EQ? ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   a r PAIR-CK
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ u <> if false exit then
   a  a r l ROW-START  p u BYTES-EQ ;

\ Copy a symbol's bytes into the caller's span and answer the byte length; a
\ span smaller than the symbol rejects with a named error before any write.
: COPY ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   a r PAIR-CK
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ {: u:n :}
   u cap > if E-IR-SYM-RANGE throw then
   a r l ROW-START {: st:n :}
   u 0 ?do
      a st i PBYTE@  q i + c!
   loop
   u ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its symbols through the two arena views; the retired
\ builder handles reject every touch with E-IR-ARENA-FROZEN.
: FSYMBOLS ( IR-ARENA:view -- n )
   dup FRHDR-CK FCNT ;

: FLEN@ ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-symbol-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-LEN FRC@ ;

: FEQ? ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-symbol-id p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   rv l OFF-LEN FRC@ u <> if false exit then
   pv  pv rv l FROW-START  p u FBYTES-EQ ;

: FCOPY ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-symbol-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   rv l OFF-LEN FRC@ {: u:n :}
   u cap > if E-IR-SYM-RANGE throw then
   pv rv l FROW-START {: st:n :}
   u 0 ?do
      pv st i FPBYTE@  q i + c!
   loop
   u ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
