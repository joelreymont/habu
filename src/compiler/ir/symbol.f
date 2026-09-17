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
\ the last cell's tail is zero-padded. Appends stay whole, at a cost of at
\ most seven padding bytes per symbol; the committed
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
\ module-local ordinal otherwise. The row's filter cell - FNV-1a over the
\ bytes, folded to sixteen bits - is a cheap reject filter, never identity:
\ every filter-and-length match is confirmed by comparing the stored bytes
\ cell by cell, so two different strings sharing a filter stay distinct.
\ Pointer identity plays no part; equal bytes presented from different buffers
\ intern to one identity. A duplicate intern allocates
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
\ equality. A wrong cell makes lookup pass over a row that does hold the
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
\ in-memory lookup filter only and never serializes. Mutable buckets are private
\ context scratch, cleared on arena retirement and copied independently on clone.
\
\ NO POINTER ESCAPES. Readers copy bytes into a caller span or compare bytes
\ in place; no public word returns a pointer into the context mapping, so
\ nothing a caller can hold outlives the context but the sealed identities,
\ which fail closed once the stores are gone.

require lib/prelude.f
require lib/errors.f
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
\ FNV-1a's 64-bit offset basis and prime, the content filter's hash.
$CBF29CE484222325 constant FNV-OFFSET
$100000001B3 constant FNV-PRIME
public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CELL-MAX
POOL-CELL-MAX CELL-BYTES * constant BYTE-MAX
private

: BYTES>CELLS ( n -- n )
   CELL-BYTES 1- + CELL-BYTES / ;

\ ---- cell access -------------------------------------------------------------
\ Every read below goes through an IR-ARENA reader: a store is resolved ONCE, at
\ the public word, and the helpers take the resolved readers. The live/frozen
\ twins that used to run down this file collapse into one set, because a reader
\ carries the state it was opened against and refuses the other with the error
\ the handle would have given - the only thing the two entry points still differ
\ in is OPEN-LIVE against OPEN. INTERN resolves the two stores before looking up
\ a bucket, so a stale generation or frozen builder rejects before index access.

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

: PHDR-CK ( IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader :}
   pr IR-ARENA:RD-SIZE PSHAPE-CK
   pr HC-MAGIC IR-ARENA:RD@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:reader -- )
   {: rr:IR-ARENA:reader :}
   rr IR-ARENA:RD-SIZE RSHAPE-CK
   rr HC-MAGIC IR-ARENA:RD@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE USED>CNT ;

: PCELLS ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-SYM-OWNER throw then ;

\ The pair coupling: both stores are what their tags claim and both carry the
\ same owning module serial, so a cross-module pairing rejects before any row
\ span is trusted against the wrong pool.
: PAIR-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr PHDR-CK
   rr RHDR-CK
   pr HC-SERIAL IR-ARENA:RD@ rr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   pr rr PAIR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

\ Validate a presented symbol-id against a resolved (header serial, count):
\ minted under this interner's module, ordinal below the interned count.
: ID-CK-N ( n n IR-ID:ir-symbol-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-symbol-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:SYMBOL-LOCAL
   dup cnt >= if E-IR-SYM-BOUND throw then ;

: ID-CK ( IR-ARENA:reader IR-ID:ir-symbol-id -- n )
   {: rr:IR-ARENA:reader id:IR-ID:ir-symbol-id :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ rr CNT id ID-CK-N ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

: DC@ ( IR-ARENA:reader n -- n )
   HDR-CELLS + IR-ARENA:RD@ ;

\ A row's byte span revalidates against the pool's live cell range on every
\ access, so a forged or bypass-appended row rejects fail-closed instead of
\ reading cells the pool never stored.
: SPAN-CK-N ( n n n -- )
   {: pc:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SYM-STATE throw then
   st ln BYTES>CELLS + pc > if E-IR-SYM-STATE throw then ;

: ROW-START ( IR-ARENA:reader IR-ARENA:reader n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n :}
   rr l OFF-START RC@ {: st:n :}
   pr PCELLS st rr l OFF-LEN RC@ SPAN-CK-N
   st ;

\ ---- byte packing ------------------------------------------------------------
\ Build the pool cell for the presented bytes at byte offset j: up to eight
\ bytes little-endian, missing tail bytes zero. Packing is deterministic, so
\ byte equality of two symbols is exactly cell equality of their packed spans.
: PACK-CELL ( ptr u8 n n -- n )
   {: p u:n j:n :}
   0
   CELL-BYTES 0 ?do
      j i + u < if
         p j i + + c@  i 8 * lshift  or
      then
   loop ;

: CELL-BYTE ( n n -- n )
   8 * rshift $FF and ;

: PBYTE@ ( IR-ARENA:reader n n -- n )
   {: pr:IR-ARENA:reader st:n i:n :}
   pr st i CELL-BYTES / + DC@  i CELL-BYTES mod CELL-BYTE ;

\ The verify step of interning and of every equality probe: compare the
\ stored packed cells with the presented bytes packed the same way.
: BYTES-EQ ( IR-ARENA:reader n ptr u8 n -- bool )
   {: pr:IR-ARENA:reader st:n p u:n :}
   u BYTES>CELLS 0 ?do
      pr st i + DC@  p u i CELL-BYTES * PACK-CELL <> if
         false unloop exit
      then
   loop
   true ;

: HASH ( ptr u8 n -- n )
   {: p u:n :}
   FNV-OFFSET
   u 0 ?do
      p i + c@ xor  FNV-PRIME *
   loop
   dup 32 rshift xor
   dup 16 rshift xor ;

public

\ The deterministic content filter a symbol row stores: FNV-1a over the bytes
\ (64-bit, offset basis FNV-OFFSET, prime FNV-PRIME), the resulting hash
\ XOR-folded down onto FILTER-MASK bits. It is public so fixtures can force
\ two different strings through one filter and prove the verify step
\ discriminates.
\
\ WHAT THE VALUE OWES, AND WHAT IT DOES NOT. It is a pure function of the
\ bytes and nothing else - no address, no ordinal, no interning order - taken
\ one byte at a time, so it is independent of host word order and identical in
\ every process and every run. It is never identity: ROW-ADD is the one writer
\ of the cell and ROW-MATCH? the one reader, and the match ROW-MATCH? allows is
\ always confirmed behind it by BYTES-EQ, so two different strings that share a
\ filter stay two symbols.
\
\ WHY A NONCRYPTOGRAPHIC HASH IS THE RIGHT ONE. The value never leaves this
\ file: it does not serialize, no canonical preimage or persisted digest
\ contains it, and the section 6.6 encoder reaches symbols through
\ SYMBOLS/FSYMBOLS, LEN@ and COPY (see INSERTION ORDER AND CANONICALIZATION
\ above). Nothing outside ROW-MATCH? can observe which function produced it, so
\ changing it moves no digest anyone has stored. It cheaply rejects candidates;
\ a cryptographic digest per intern was a steep price for sixteen bits of
\ reject power that FNV-1a delivers in a few instructions per byte.
: FILTER ( ptr u8 n -- n )
   HASH FILTER-MASK and ;

private

\ ---- matching --------------------------------------------------------------------
: ROW-MATCH? ( IR-ARENA:reader IR-ARENA:reader n ptr u8 n n -- bool )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n p u:n f:n :}
   rr l OFF-FLT RC@ f <> if false exit then
   rr l OFF-LEN RC@ u <> if false exit then
   pr  pr rr l ROW-START  p u BYTES-EQ ;

\ Buckets and their control record live in context scratch, and the pointer to
\ that record lives in the arena's own descriptor - IR-ARENA:SIDE-FIELD, the one
\ observer's cell. It is born empty with the arena and dies with it, so nothing
\ here has to notice a retirement: the record and the cell naming it are spans
\ of one region and go back together.
\ Each bucket keeps the full hash for growth and ordinal+1 (zero means empty).
\ Load stays at most one half; the insertion-ordered rows remain authoritative.
8 constant INDEX-SEED
0 constant IX-CAP
1 constant IX-COUNT
2 constant IX-BUCKETS
3 cells constant INDEX-BYTES
2 constant BUCKET-CELLS
0 constant BK-HASH
1 constant BK-ORD

: BUCKETS-FIELD ( ptr u8 -- ptr ptr u8 )
   IX-BUCKETS ptr-field ;


: INDEX@ ( IR-ARENA:reader -- ptr u8 )
   IR-ARENA:SIDE-FIELD @
   dup NULL-PTR = if E-IR-SYM-STATE throw then ;


: BUCKET@ ( ptr u8 n n -- n )
   swap BUCKET-CELLS * + CDIGEST:SLOT@ ;


: BUCKET! ( n ptr u8 n n -- )
   swap BUCKET-CELLS * + CDIGEST:SLOT! ;


: BUCKET-NEXT ( n n -- n )
   1- swap 1+ and ;


: EMPTY-BUCKET ( ptr u8 n n -- n )
   {: buckets:ptr cap:n hash:n :}
   hash cap 1- and
   begin buckets over BK-ORD BUCKET@ 0<> while
      cap BUCKET-NEXT
   repeat ;


: BUCKET-ADD ( ptr u8 n n n -- )
   {: buckets:ptr cap:n hash:n ord:n :}
   buckets cap hash EMPTY-BUCKET {: at:n :}
   hash buckets at BK-HASH BUCKET!
   ord 1+ buckets at BK-ORD BUCKET! ;


: BUCKETS-TAKE ( IR-CTX:ctx n -- ptr u8 )
   BUCKET-CELLS * cells IR-CTX:SCRATCH-TAKE drop ;


\ A bucket table is a run of canonical slots, so both of these ask the canonical
\ slot words once for the whole run - same cells, same order, same result as the
\ per-slot loop each replaces.
: BUCKETS-ZERO ( ptr u8 n -- )
   {: buckets:ptr cap:n :}
   buckets cap BUCKET-CELLS * CDIGEST:SLOTS-ZERO ;


: BUCKETS-CLONE ( ptr u8 ptr u8 n -- )
   {: src:ptr dst:ptr cap:n :}
   src dst cap BUCKET-CELLS * CDIGEST:SLOTS-COPY ;

\ All allocation precedes publishing the control pointer in the registry.
: INDEX-TAKE ( IR-CTX:ctx n n -- ptr u8 )
   {: c:IR-CTX:ctx cap:n count:n :}
   c INDEX-BYTES IR-CTX:SCRATCH-TAKE drop {: ix:ptr :}
   c cap BUCKETS-TAKE {: buckets:ptr :}
   cap ix IX-CAP CDIGEST:SLOT!
   count ix IX-COUNT CDIGEST:SLOT!
   buckets ix BUCKETS-FIELD !
   ix ;


: INDEX-NEW ( IR-CTX:ctx IR-ARENA:reader -- )
   {: c:IR-CTX:ctx rr:IR-ARENA:reader :}
   c INDEX-SEED 0 INDEX-TAKE {: ix:ptr :}
   ix BUCKETS-FIELD @ INDEX-SEED BUCKETS-ZERO
   ix rr IR-ARENA:SIDE-FIELD ! ;


: BUCKETS-REHASH ( ptr u8 n ptr u8 n -- )
   {: old:ptr oldcap:n fresh:ptr cap:n :}
   oldcap 0 ?do
      old i BK-ORD BUCKET@ {: entry:n :}
      entry 0<> if
         fresh cap old i BK-HASH BUCKET@ entry 1- BUCKET-ADD
      then
   loop ;


: INDEX-ROOM ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx ix:ptr need:n :}
   ix IX-CAP CDIGEST:SLOT@ {: oldcap:n :}
   need oldcap 2 / <= if exit then
   oldcap 2 * {: cap:n :}
   c cap BUCKETS-TAKE {: fresh:ptr :}
   fresh cap BUCKETS-ZERO
   ix BUCKETS-FIELD @ oldcap fresh cap BUCKETS-REHASH
   fresh ix BUCKETS-FIELD !
   cap ix IX-CAP CDIGEST:SLOT! ;


: INDEX-ADD ( ptr u8 n n -- )
   {: ix:ptr hash:n ord:n :}
   ix BUCKETS-FIELD @ ix IX-CAP CDIGEST:SLOT@ hash ord BUCKET-ADD
   ord 1+ ix IX-COUNT CDIGEST:SLOT! ;


: INDEX-CK ( IR-ARENA:reader -- ptr u8 )
   {: rr:IR-ARENA:reader :}
   rr INDEX@ {: ix:ptr :}
   rr CNT ix IX-COUNT CDIGEST:SLOT@ <> if E-IR-SYM-STATE throw then
   ix ;


: LOOKUP ( IR-ARENA:reader IR-ARENA:reader ptr u8 n n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader p u:n hash:n :}
   rr INDEX-CK {: ix:ptr :}
   ix BUCKETS-FIELD @ {: buckets:ptr :}
   ix IX-CAP CDIGEST:SLOT@ {: cap:n :}
   hash cap 1- and
   begin
      buckets over BK-ORD BUCKET@ {: entry:n :}
      entry 0= if drop -1 exit then
      pr rr entry 1- p u hash FILTER-MASK and ROW-MATCH? if
         drop entry 1- exit
      then
      cap BUCKET-NEXT
   again ;


: INDEX-CLONE ( IR-CTX:ctx IR-ARENA:reader IR-ARENA:reader -- )
   {: c:IR-CTX:ctx rr:IR-ARENA:reader proto:IR-ARENA:reader :}
   proto INDEX-CK {: src:ptr :}
   src IX-CAP CDIGEST:SLOT@ {: cap:n :}
   c cap proto CNT INDEX-TAKE {: dst:ptr :}
   src BUCKETS-FIELD @ dst BUCKETS-FIELD @ cap BUCKETS-CLONE
   dst rr IR-ARENA:SIDE-FIELD ! ;

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
: ROOM-CK ( IR-ARENA:reader IR-ARENA:reader n -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader u:n :}
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-SYM-CAP throw then
   pr PCELLS u BYTES>CELLS + pr HC-CAP IR-ARENA:RD@ > if E-IR-SYM-BYTES throw then ;

\ An intern writes BOTH arenas, so both are reserved here, before either is
\ touched: a row whose bytes went in and whose row did not is a symbol table
\ that no longer divides into rows. Reserving after the room checks keeps this
\ table's own named capacity errors ahead of the arena's.
: ROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena u:n :}
   c a u BYTES>CELLS IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE ;

\ The readers are opened before the reservation and outlive it: a RESERVE or a
\ PUSH changes neither registry generation nor state, and a reader re-reads its
\ row's pointer and count on every call, so it follows the new span.
: POOL-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader ptr u8 n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena pr:IR-ARENA:reader p u:n :}
   pr PCELLS {: st:n :}
   u BYTES>CELLS 0 ?do
      c a  p u i CELL-BYTES * PACK-CELL  IR-ARENA:PUSH drop
   loop
   st ;

: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader n n n -- n )
   {: c:IR-CTX:ctx r:IR-ARENA:arena rr:IR-ARENA:reader f:n st:n u:n :}
   rr CNT {: l:n :}
   c r f IR-ARENA:PUSH drop
   c r st IR-ARENA:PUSH drop
   c r u IR-ARENA:PUSH drop
   l ;

\ One store of the interner: its committed span and its three header cells.
\ NEW and NEW-FROM differ in what goes into the store afterwards, never in how
\ the store is made or how its header is written.
: PART-NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cells:n magic:n cap:n :}
   c cells HDR-CELLS + IR-ARENA:NEW {: x:IR-ARENA:arena :}
   c x HDR-CELLS IR-ARENA:RESERVE
   c x magic IR-ARENA:PUSH drop
   c x key KEY-SERIAL IR-ARENA:PUSH drop
   c x cap IR-ARENA:PUSH drop
   x ;

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
   c key bcap BYTES>CELLS SYB-MAGIC bcap BYTES>CELLS PART-NEW {: a:IR-ARENA:arena :}
   c key scap ROW-CELLS * SYM-MAGIC scap PART-NEW {: r:IR-ARENA:arena :}
   c r IR-ARENA:OPEN-LIVE INDEX-NEW
   a r ;

\ ---- an interner that starts where another one left off ----------------------
\ A module's symbols are its own ordinals, so a fresh module starts with an
\ empty interner and every name a dialect needs is interned into it again. For
\ the dialect's own vocabulary that is the same hundred-odd names every time,
\ and each one costs a content hash over its bytes, a bucket lookup and an
\ append - per module, for every module a definition builds.
\
\ A CLONE IS THE SAME TABLE UNDER A NEW KEY. Ordinals are positions in the row
\ table, so copying the prototype's pool bytes and rows verbatim gives the new
\ module the same spelling at the same ordinal: a caller that recorded an
\ ordinal against the prototype can mint the identity it names in this module
\ with IR-ID:PACK-SYMBOL and no lookup at all. The filter value a row stores is
\ a pure function of the bytes (FILTER above), so it copies verbatim too - no
\ row is recomputed and nothing here can disagree with what INTERN would have
\ written.
\
\ THE PROTOTYPE IS READ AND NOT TOUCHED. Its own header keeps its own key, so
\ the clone is not a second name for it: interning into either afterwards
\ appends to that one alone, and the two agree only about the ordinals that
\ existed when the copy was taken.
\
\ The committed ceilings belong to the new module. The caller supplies them
\ just as it does to NEW; both are validated against the prototype's live
\ occupancy before either destination arena is allocated. A clone may therefore
\ start exactly full, and any later miss is still governed by its own plan.
\
\ THE TWO SPANS ARE COPIED IN BULK. A clone holds every symbol the prototype
\ holds, so the copy is the whole committed prefix of each arena and never a
\ row at a time: appending it cell by cell through PUSH resolved both handles,
\ rechecked ownership and minted an index for each of some hundreds of cells,
\ per module, for every module a definition builds. IR-ARENA:APPEND-SPAN is
\ that same append with the per-cell work reduced to the load and the store it
\ always was.
: NEW-FROM ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key pa:IR-ARENA:arena pr:IR-ARENA:arena scap:n bcap:n :}
   scap SYM-CAP-OK
   bcap BYTE-CAP-OK
   pa IR-ARENA:OPEN-LIVE {: par:IR-ARENA:reader :}
   pr IR-ARENA:OPEN-LIVE {: prr:IR-ARENA:reader :}
   par prr PAIR-CK
   prr INDEX-CK drop
   prr CNT scap > if E-IR-SYM-CAP throw then
   bcap BYTES>CELLS {: poolcap:n :}
   par PCELLS poolcap > if E-IR-SYM-BYTES throw then
   c key poolcap SYB-MAGIC poolcap PART-NEW {: a:IR-ARENA:arena :}
   c key scap ROW-CELLS * SYM-MAGIC scap PART-NEW {: r:IR-ARENA:arena :}
   c a pa HDR-CELLS par PCELLS IR-ARENA:APPEND-SPAN
   c r pr HDR-CELLS prr CNT ROW-CELLS * IR-ARENA:APPEND-SPAN
   c r IR-ARENA:OPEN-LIVE prr INDEX-CLONE
   a r ;

\ Intern the presented bytes: equal bytes answer the identity they already
\ hold - the same bytes twice, from any buffer, are one symbol - and new
\ bytes mint the next module-local identity under key. A hit allocates
\ nothing; a miss appends bytes and row whole or throws with nothing written.
: INTERN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :}
   u 0 < if E-IR-SYM-LEN throw then
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   p u HASH {: hash:n :}
   pr rr p u hash LOOKUP {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-SYMBOL exit then
   pr rr u ROOM-CK
   c a r u ROOM-TAKE
   rr INDEX-CK {: ix:ptr :}
   c ix rr CNT 1+ INDEX-ROOM
   c a pr p u POOL-ADD {: st:n :}
   c r rr hash FILTER-MASK and st u ROW-ADD {: ord:n :}
   ix hash ord INDEX-ADD
   key ord IR-ID:PACK-SYMBOL ;

\ ---- live readers ------------------------------------------------------------
\ Total bucket visits for one successful lookup of every interned symbol.
\ This derives deterministic work from placement without instrumenting INTERN.
: LOOKUP-PROBES ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK INDEX-CK {: ix:ptr :}
   ix IX-CAP CDIGEST:SLOT@ {: cap:n :}
   ix BUCKETS-FIELD @ {: buckets:ptr :}
   0
   cap 0 ?do
      buckets i BK-ORD BUCKET@ 0<> if
         i buckets i BK-HASH BUCKET@ - cap 1- and 1+ +
      then
   loop ;

: SYMBOLS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK CNT ;

: LEN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ ;

\ Bind a batch of existing ordinals under this table's module. Resolve the
\ live table once, and check the entire input before writing any identity.
\ Nothing calls back or appends while the checked header/count are in use.
\ Copy direction also covers overlapping caller spans; the common pointer-field
\ views compare addresses only and are never dereferenced.
: IDS! ( IR-ARENA:arena IR-ID:ir-module-key ptr n n ptr IR-ID:ir-symbol-id n -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key src:ptr n:n dst:ptr cap:n :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK
   key 0 IR-ID:PACK-SYMBOL drop
   rr CNT {: cnt:n :}
   n 0 < cap n < or
   n $7FFFFFFFFFFFFFFF CELL-BYTES / > or
   cap $7FFFFFFFFFFFFFFF CELL-BYTES / > or
   if E-IR-SYM-RANGE throw then
   src n cells + src < dst cap cells + dst < or
   if E-IR-SYM-RANGE throw then
   n 0 ?do
      src i cells + @ dup 0 < swap cnt >= or
      if E-IR-SYM-BOUND throw then
   loop
   src 0 ptr-field dst 0 ptr-field < if
      n begin dup 0 > while
         1- {: ix:n :}
         key src ix cells + @ IR-ID:PACK-SYMBOL dst ix cells + !
         ix
      repeat drop
   else
      n 0 ?do
         key src i cells + @ IR-ID:PACK-SYMBOL dst i cells + !
      loop
   then ;

\ Byte equality between a symbol and a presented span - the observable form
\ of the interning invariant, with no pointer crossing the boundary.
: EQ? ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id p u:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ u <> if false exit then
   pr  pr rr l ROW-START  p u BYTES-EQ ;

\ Copy a symbol's bytes into the caller's span and answer the byte length; a
\ span smaller than the symbol rejects with a named error before any write.
: COPY ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id q cap:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ {: u:n :}
   u cap > if E-IR-SYM-RANGE throw then
   pr rr l ROW-START {: st:n :}
   u 0 ?do
      pr st i PBYTE@  q i + c!
   loop
   u ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its symbols through the two arena views; the retired
\ builder handles reject every touch with E-IR-ARENA-FROZEN.
: FSYMBOLS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup RHDR-CK CNT ;

: FLEN@ ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ ;

: FEQ? ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-symbol-id p u:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ u <> if false exit then
   pr  pr rr l ROW-START  p u BYTES-EQ ;

: FCOPY ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-symbol-id q cap:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ {: u:n :}
   u cap > if E-IR-SYM-RANGE throw then
   pr rr l ROW-START {: st:n :}
   u 0 ?do
      pr st i PBYTE@  q i + c!
   loop
   u ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
