\ symbol.f - the compiler symbol interner: module-local symbol identities by
\ byte equality, never pointer identity. Two IR-ARENA stores per module: a byte
\ pool (eight bytes per cell little-endian, each symbol cell-aligned and
\ zero-padded) and a row table (filter, start cell, byte length). Ordinals are
\ insertion-ordered and stable.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-SYM
private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
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
\ Each store is resolved once at the public word into a reader, which refuses
\ the wrong live/frozen state itself. INTERN resolves both stores before the
\ bucket lookup, so a stale or frozen store rejects before index access.

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

\ A cross-module pairing rejects before a row span is trusted against the pool.
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

\ Revalidated on every access, so a forged row cannot read past the pool.
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
\ Up to eight bytes at offset j, little-endian, missing tail bytes zero.
\ Deterministic, so byte equality is cell equality of the packed spans.
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

: BYTES-EQ ( IR-ARENA:reader n ptr u8 n -- bool )
   {: pr:IR-ARENA:reader st:n p u:n :}
   u BYTES>CELLS 0 ?do
      pr st i + DC@  p u i CELL-BYTES * PACK-CELL <> if
         false unloop exit
      then
   loop
   true ;

\ A pure function of the bytes, taken a byte at a time, so it is identical across
\ hosts, processes and runs. It never leaves this file (no serialization, no
\ persisted digest), so the function may change freely; a cryptographic digest
\ per intern cost far more than sixteen bits of reject power need.
: HASH ( ptr u8 n -- n )
   {: p u:n :}
   FNV-OFFSET
   u 0 ?do
      p i + c@ xor  FNV-PRIME *
   loop
   dup 32 rshift xor
   dup 16 rshift xor ;

\ ---- matching ----------------------------------------------------------------
\ The row filter (FNV-1a folded to sixteen bits) is a reject filter only: every
\ match is confirmed by BYTES-EQ. It is the one row cell not rechecked on read
\ (only ROW-ADD writes it); a wrong filter would mint a duplicate row for equal
\ bytes and break type.f's assumption that structurally equal rows are one row.
: ROW-MATCH? ( IR-ARENA:reader IR-ARENA:reader n ptr u8 n n -- bool )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n p u:n f:n :}
   rr l OFF-FLT RC@ f <> if false exit then
   rr l OFF-LEN RC@ u <> if false exit then
   pr  pr rr l ROW-START  p u BYTES-EQ ;

\ Buckets live in context scratch; the pointer to their control record lives in
\ the arena descriptor (IR-ARENA:SIDE-FIELD), so both die with the arena and no
\ retirement needs noticing. A bucket holds the full hash (for growth) and
\ ordinal+1 (zero is empty); load stays at most one half.
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
\ Refused before HASH reads the bytes: a length near the maximum cell would run
\ off the caller's span.
: BYTES-CK ( IR-ARENA:reader n -- )
   {: pr:IR-ARENA:reader u:n :}
   u pr HC-CAP IR-ARENA:RD@ CELL-BYTES * > if E-IR-SYM-BYTES throw then ;

\ The bytes are compared with the room left in bytes: rounded up to cells, a
\ length near the maximum cell wraps negative and its sum would pass.
: ROOM-CK ( IR-ARENA:reader IR-ARENA:reader n -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader u:n :}
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-SYM-CAP throw then
   u  pr HC-CAP IR-ARENA:RD@ pr PCELLS -  CELL-BYTES *  > if E-IR-SYM-BYTES throw then ;

\ Both arenas are reserved before either is written, after the room checks so
\ this table's capacity errors come first.
: ROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena u:n :}
   c a u BYTES>CELLS IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE ;

\ The readers survive RESERVE and PUSH: they re-read the row's pointer and count
\ on every call.
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

\ NEW and NEW-FROM differ only in what goes into the store afterwards.
: PART-NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cells:n magic:n cap:n :}
   c cells HDR-CELLS + IR-ARENA:NEW {: x:IR-ARENA:arena :}
   c x HDR-CELLS IR-ARENA:RESERVE
   c x magic IR-ARENA:PUSH drop
   c x key KEY-SERIAL IR-ARENA:PUSH drop
   c x cap IR-ARENA:PUSH drop
   x ;

public

\ The pool's byte capacity is accounted in whole cells. Both headers are bound to
\ key's module serial; the interner dies with the owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key scap:n bcap:n :}
   scap SYM-CAP-OK
   bcap BYTE-CAP-OK
   c key bcap BYTES>CELLS SYB-MAGIC bcap BYTES>CELLS PART-NEW {: a:IR-ARENA:arena :}
   c key scap ROW-CELLS * SYM-MAGIC scap PART-NEW {: r:IR-ARENA:arena :}
   c r IR-ARENA:OPEN-LIVE INDEX-NEW
   a r ;

\ ---- an interner that starts where another one left off ----------------------
\ A clone copies the prototype's pool and rows verbatim under a new key, so each
\ dialect name keeps its ordinal and a caller mints identities with
\ IR-ID:PACK-SYMBOL instead of re-interning a hundred-odd names per module. The
\ filter is a pure function of the bytes, so it copies too. The prototype is
\ only read; the two diverge on later interns. The new module's ceilings are
\ checked against the prototype's occupancy before allocation. The copy is one
\ IR-ARENA:APPEND-SPAN per arena: per-cell PUSH re-resolved both handles per
\ cell.
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

\ A hit allocates nothing; a miss appends bytes and row whole or throws with
\ nothing written.
: INTERN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :}
   u 0 < if E-IR-SYM-LEN throw then
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   pr u BYTES-CK
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
\ No public word returns a pointer into the context's mapping.
: SYMBOLS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK CNT ;

: LEN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ ;

\ The whole input is checked before any identity is written. The copy direction
\ handles overlapping caller spans; pointer-field views compare addresses only.
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

: EQ? ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id p u:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   rr l OFF-LEN RC@ u <> if false exit then
   pr  pr rr l ROW-START  p u BYTES-EQ ;

\ A span smaller than the symbol rejects before any write.
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

\ The bucket index lives in the arena descriptor and outlives the freeze, so a
\ reader in another context can look bytes up without interning.
: FFIND ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id bool )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key p u:n :}
   u 0 < if E-IR-SYM-LEN throw then
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   pr rr p u  p u HASH  LOOKUP {: hit:n :}
   hit 0 < if key 0 IR-ID:PACK-SYMBOL false exit then
   key hit IR-ID:PACK-SYMBOL true ;

\ ---- carrying a symbol between modules ---------------------------------------
\ The bytes cross through a scratch span sized to the symbol, so no caller
\ buffer bounds a name's length.
: CARRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key
      pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-symbol-id :}
   rv id FLEN@ {: u:n :}
   c u 1 max IR-CTX:SCRATCH-TAKE drop {: q:ptr :}
   pv rv id q u FCOPY drop
   c a r key q u INTERN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
