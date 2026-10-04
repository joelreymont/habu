\ pool.f - fixed-slot memory pools, checked reference counts and bounded
\ reclamation, for the browser runtime (docs/browser-runtime.md §3.1, §3.4,
\ §26.1).
\
\ STORAGE CLASS. CALLER-OWNED for pools: a set of pools keeps its state in the
\ caller's cells, POOLS-CELLS of them (`n TYPED-BUFFER P n`), so any number of
\ tasks use their own pools at once. The state is a private record: INIT writes
\ it into the cells and answers the pools, an opaque token for them. A pools
\ cell INIT has not filled holds the zero token, which every word refuses
\ before reading a cell. PROCESS-WIDE for schemas: SCHEMA adds to one registry
\ for the image while files load, and every set of pools reads it.
\
\ There are five pools, one per class of §3.4 (`kind`): immutable pages,
\ transient transaction and build storage, job states, packets and emergency
\ diagnostics. Each holds slots of one size and grows by one 64 KiB chunk from
\ MEM:ALLOC-64K at a time, while the chunks of the pools of its §26.1 class stay
\ within that class's ceiling: 48 MiB for pages, 24 MiB shared by transient
\ storage and job states, 16 MiB for packets and 4 MiB for the emergency pool.
\ INIT maps the whole emergency pool, so that an emergency reservation makes
\ no mapping calls, and only RESERVE-EMERGENCY reserves from it. No chunk
\ moves, and none goes back to the OS before SHUTDOWN.
\
\ An object is one slot: a header of four cells, then a payload of cells,
\ zeroed when the slot is reserved. Its ID is an RT-HANDLE handle, the slot
\ number and the slot's generation, and no word answers its address: a payload
\ cell is read and written by handle and index, a child's handle through REF@
\ and REF!. Each pool numbers its slots from just past the pool before it, the
\ first from 1, so no two pools issue one handle. FREEZE makes an object
\ read-only: CELL! and REF! refuse it from then until RECLAIM-STEP returns its
\ slot, whose next generation starts writable. A frozen object is still read,
\ retained and released.
\
\ RESERVE checks the pool's quota, then takes a free slot, a slot of a mapped
\ chunk never issued, or one of a chunk it maps now. When the quota, the class
\ ceiling or the OS (E-MEM-MAP) refuses, it answers oom, which is
\ RecoverableOOM, and nothing has changed. A pool's ledger charges a slot's
\ bytes once, at RESERVE, however many references hold the object, until
\ RECLAIM-STEP returns the slot.
\
\ An object starts held once. Its reference count is a checked u64: RETAIN of
\ an object held 2^64 - 1 times is refused, and the count never wraps. A
\ payload cell REF! fills holds its own reference to the child, and REF! of
\ the child it already holds changes nothing. The pools do not tell such a
\ cell from one CELL! writes, so a schema's reference cells are written by
\ REF! alone for the object's whole lifetime, and its enumerator answers each
\ non-null reference they hold exactly once: a child that two cells hold,
\ twice. The last RELEASE links the object onto the retirement queue through
\ its own header and frees nothing. RECLAIM-STEP takes objects off the queue
\ and, through each one's schema's enumerator, visits at most its budget of
\ edges: it releases each child, which queues a child whose count reaches
\ zero, then returns the slot to its pool for the slot's next generation.
\ Nothing but its enumerator, while it runs, reads an object released for
\ good. Generations run 1 .. 2^32 - 1, and returning a slot at its last
\ retires the slot, which is never issued again. Owning edges form a DAG
\ (§3.1); the pools look for no cycle, and the objects of one are never
\ reclaimed.
\
\ BOUNDARY. Outside the package no typed route leads from pools to their state,
\ and none from a number to pools or a schema: their converters are private,
\ and a cast from a number to either is E-CAST-OWNER. Raw storage passes them: a
\ private pointer mint (docs/forth.md, CAST:), such as any package's own
\ `CAST: ( RT-POOL:pools -- ptr n )`, reads and writes the state and, through
\ it, the chunks, and the public byte and cell views over a cell that holds
\ pools store any number there. The cells also stay the caller's buffer, which
\ the package does not guard: writing them can revive a returned handle. Each
\ set of pools owns its cells exclusively: no two sets, and no set and a table
\ or counter, share a cell, and INIT is never handed cells that others use. A
\ handle names no set of pools, so two sets cannot tell their handles apart:
\ the runtime context holds one. Nor is a handle a capability: RT-HANDLE:HANDLE
\ makes one of any slot and generation, the halves of another package's token
\ cast to a number included, so whoever holds the pools reaches each live
\ object through these words. A frozen one they refuse to write, however its
\ handle was made: only a private pointer mint writes it. What follows holds
\ while only this package's words write a set of pools' cells and each set's
\ cells are its own.

require lib/errors.f
require lib/memory.f
require lib/type/deftype.f
require lib/runtime/handle.f

package RT-POOL

public

\ The decade -9520..-9529.
-9520 constant E-RT-POOL-FIRST
-9529 constant E-RT-POOL-LAST
-9520 constant E-RT-POOL-NULL       \ INIT over null cells, or any other word given the zero pools token
-9521 constant E-RT-POOL-HELD       \ INIT over cells that hold pools, open or shut
-9522 constant E-RT-POOL-SHUT       \ a word given pools SHUTDOWN has closed
-9523 constant E-RT-POOL-SCHEMA     \ the zero schema token, or SCHEMA with every schema taken
-9524 constant E-RT-POOL-EMERGENCY  \ RESERVE of an emergency schema, or RESERVE-EMERGENCY of another
-9525 constant E-RT-POOL-DEAD       \ RETAIN, RELEASE, FREEZE, REF! or CELL! of an object released for good, or REF@ or CELL@ of one while its enumerator is not running
-9526 constant E-RT-POOL-OVERFLOW   \ RETAIN of an object held 2^64 - 1 times
-9527 constant E-RT-POOL-CELL       \ a payload cell index outside the object's payload
-9528 constant E-RT-POOL-RANGE      \ RECLAIM-STEP with a budget below 1, or QUOTA! below zero or above the pool's class ceiling
-9529 constant E-RT-POOL-FROZEN     \ CELL! or REF! of an object FREEZE has frozen

DEFTYPE POOLS
undefine >POOLS
undefine POOLS>N

DEFTYPE SCHEMA
undefine >SCHEMA
undefine SCHEMA>N

ENUM kind pages transient jobs packets emergency ;ENUM

ENUM reservation 0
   VARIANT granted FIELD id RT-HANDLE:handle ;VARIANT
   VARIANT oom ;VARIANT
;ENUM

\ The chunk a pool grows by and the §26.1 ceilings, in bytes.
$10000 constant CHUNK-BYTES
$3000000 constant PAGES-CEILING
$1800000 constant SCRATCH-CEILING
$1000000 constant INGRESS-CEILING
$400000 constant EMERGENCY-CEILING

64 constant MAX-SCHEMAS

private

\ mode: 0 in the cells before INIT, then OPEN, then SHUT; queue: the bits of
\ the handle of the object released for good last, 0 while the queue is empty;
\ dying: those of the object RECLAIM-STEP is visiting, 0 when none; edge: that
\ object's next edge to visit; reading: the dying object's bits while its
\ enumerator runs, else 0.
STRUCTURE state 0 DERIVE addr
   FIELD mode n
   FIELD queue n
   FIELD dying n
   FIELD edge n
   FIELD reading n
;STRUCTURE

\ fresh: how many slots the pool has issued at least once; free: the index of
\ its first free slot + 1, 0 when none; chunks: how many chunks it has mapped;
\ charged: the bytes of the slots its objects hold; quota: the most bytes it
\ may charge.
STRUCTURE pool 0 DERIVE addr
   FIELD fresh n
   FIELD free n
   FIELD chunks n
   FIELD charged n
   FIELD quota n
;STRUCTURE

\ stamp: the slot's last generation in bits 0-31, LIVE while that
\ generation's object is held or queued, and FROZEN once FREEZE has frozen
\ it, until CLAIM or RETURN-SLOT writes a fresh stamp; count: the object's
\ reference count, 0 once it is released for good; schema: its schema's
\ number; link: the bits of the next object on the queue, or the index + 1 of
\ the next free slot.
STRUCTURE head 0 DERIVE addr
   FIELD stamp n
   FIELD count n
   FIELD schema n
   FIELD link n
;STRUCTURE

CAST: >POOLS ( ptr n -- pools )
CAST: POOLS>CELLS ( pools -- ptr n )
CAST: >STATE ( ptr n -- ptr state )
CAST: >POOL ( ptr n -- ptr pool )
CAST: >ENTRY ( ptr n -- ptr ptr u8 )
CAST: >HEAD ( ptr u8 -- ptr head )
CAST: >SCHEMA ( n -- schema )
CAST: SCHEMA>N ( schema -- n )
CAST: KIND>N ( kind -- n )

\ A handle's slot in the low 32 bits and its generation in the high 32, read
\ through RT-HANDLE's public words.
: HANDLE>BITS ( RT-HANDLE:handle -- n )
   {: h:RT-HANDLE:handle :}
   h RT-HANDLE:GENERATION 32 lshift h RT-HANDLE:SLOT or ;

1 constant OPEN
2 constant SHUT
$FFFFFFFF constant U32-MAX
U32-MAX constant LAST-GEN
$100000000 constant LIVE
$200000000 constant FROZEN
-1 constant LAST-COUNT             \ 2^64 - 1

RT--POOL-KIND:emergency KIND>N constant EMERGENCY#
EMERGENCY# 1 + constant KINDS

\ Each pool's slot bytes and §26.1 class, in the order of `kind`, and each
\ class's ceiling. A page holds a tree page or a record (§3.1), and a control
\ packet is at most one (§4.3); a job state holds §4.1's JobHeader and its
\ cursor; a transient entry and an emergency record are small fixed records.
create SLOT-SIZES 4096 , 128 , 256 , 4096 , 128 ,
create CLASSES 0 , 1 , 1 , 2 , 3 ,
create CEILINGS PAGES-CEILING , SCRATCH-CEILING , INGRESS-CEILING , EMERGENCY-CEILING ,

: SLOT-SIZE ( n -- n )
   SLOT-SIZES swap cells + @ ;

: CLASS-OF ( n -- n )
   CLASSES swap cells + @ ;

: CEILING ( n -- n )
   CLASS-OF CEILINGS swap cells + @ ;

: PER-CHUNK ( n -- n )
   CHUNK-BYTES swap SLOT-SIZE / ;

: MAX-CHUNKS ( n -- n )
   CEILING CHUNK-BYTES / ;

\ A pool may hold its whole class's ceiling of slots.
: CAPACITY ( n -- n )
   dup MAX-CHUNKS swap PER-CHUNK * ;

\ FIRSTS holds the slot number of each pool's index 0, CHUNK-BASES the place
\ of its first entry in the chunk table.
KINDS TYPED-BUFFER FIRSTS n
KINDS TYPED-BUFFER CHUNK-BASES n

: PLAN ( -- )
   1 0 FIRSTS !
   0 0 CHUNK-BASES !
   KINDS 1 do
      i 1 - FIRSTS @ i 1 - CAPACITY + i FIRSTS !
      i 1 - CHUNK-BASES @ i 1 - MAX-CHUNKS + i CHUNK-BASES !
   loop ;

PLAN

KINDS 1 - FIRSTS @ KINDS 1 - CAPACITY + constant END-SLOT
KINDS 1 - CHUNK-BASES @ KINDS 1 - MAX-CHUNKS + constant ALL-CHUNKS

\ The cells hold the state, then each pool's record, then the chunk table.
STATE-CELLS KINDS POOL-CELLS * + constant ENTRY-BASE

public

ENTRY-BASE ALL-CHUNKS + constant POOLS-CELLS

private

\ ---- state and slots ------------------------------------------------------

\ The words below take the pools' cells.

\ The pools' cells, refusing the zero token before anything reads through it.
: CELLS-OF ( pools -- ptr n )
   POOLS>CELLS {: cs:ptr :}
   cs 0= if E-RT-POOL-NULL throw then
   cs ;

\ The cells of pools INIT opened and SHUTDOWN has not closed.
: OPEN-CELLS ( pools -- ptr n )
   CELLS-OF {: cs:ptr :}
   cs >STATE STATE-MODE @ OPEN <> if E-RT-POOL-SHUT throw then
   cs ;

: POOL-OF ( n ptr n -- ptr pool )
   {: k:n cs:ptr :}
   cs STATE-CELLS k POOL-CELLS * + cells + >POOL ;

\ The cell of the chunk table that holds a pool's chunk.
: ENTRY ( n n ptr n -- ptr ptr u8 )
   {: k:n c:n cs:ptr :}
   cs ENTRY-BASE k CHUNK-BASES @ + c + cells + >ENTRY ;

\ The first byte of a pool's slot.
: SLOT-AT ( n n ptr n -- ptr u8 )
   {: k:n ix:n cs:ptr :}
   ix k PER-CHUNK /mod {: off:n c:n :}
   k c cs ENTRY @ off k SLOT-SIZE * + ;

\ The handle whose bits these are.
: BITS>HANDLE ( n -- RT-HANDLE:handle )
   {: v:n :}
   v U32-MAX and v 32 rshift RT-HANDLE:HANDLE ;

\ The pool whose slot numbers hold the slot.
: KIND-AT ( n -- n )
   {: s:n :}
   0 KINDS 1 do s i FIRSTS @ >= if drop i then loop ;

\ The pool and index of the slot a handle's bits name: the null handle and a
\ slot past every pool are foreign, and a slot never issued is stale.
: WHERE ( n ptr n -- n n )
   {: v:n cs:ptr :}
   v U32-MAX and {: s:n :}
   s 1 < s END-SLOT >= or if RT-HANDLE:E-RT-HANDLE-FOREIGN throw then
   s KIND-AT {: k:n :}
   s k FIRSTS @ - {: ix:n :}
   ix k cs POOL-OF POOL-FRESH @ >= if RT-HANDLE:E-RT-HANDLE-STALE throw then
   k ix ;

\ The slot, pool and index whose live generation the handle's bits name; any
\ other generation is stale.
: LOCATE ( n ptr n -- ptr u8 n n )
   {: v:n cs:ptr :}
   v cs WHERE {: k:n ix:n :}
   k ix cs SLOT-AT {: sl:ptr :}
   sl >HEAD HEAD-STAMP @ {: t:n :}
   t LIVE and 0= if RT-HANDLE:E-RT-HANDLE-STALE throw then
   t U32-MAX and v 32 rshift <> if RT-HANDLE:E-RT-HANDLE-STALE throw then
   sl k ix ;

\ ---- chunks -----------------------------------------------------------------

\ Map a chunk into the cell. Every chunk the pools hold comes from here.
: MAP-CHUNK ( ptr ptr u8 -- ptr ptr u8 )
   dup MEM:ALLOC-64K drop swap ! ;

: MAP-WITH ( ptr ptr u8 [ ptr ptr u8 -- ptr ptr u8 ] -- ptr ptr u8 [ ptr ptr u8 -- ptr ptr u8 ] )
   {: at m :}
   at m execute m ;

\ Run the mapper on the cell: true once it has mapped a chunk there, false
\ when the OS refused the mapping. Any other throw goes on.
: MAPPED? ( ptr ptr u8 [ ptr ptr u8 -- ptr ptr u8 ] -- bool )
   [: MAP-WITH ;] catch {: at m code:n :}
   code 0= if true exit then
   code E-MEM-MAP <> if code throw then
   false ;

\ The chunks the pools of the pool's class have mapped.
: CLASS-CHUNKS ( n ptr n -- n )
   {: k:n cs:ptr :}
   k CLASS-OF {: c:n :}
   0 KINDS 0 do i CLASS-OF c = if i cs POOL-OF POOL-CHUNKS @ + then loop ;

\ Map the pool its next chunk: false, with nothing changed, when that would
\ pass its class's ceiling or the OS refuses the mapping.
: GROW ( n ptr n [ ptr ptr u8 -- ptr ptr u8 ] -- bool )
   {: k:n cs:ptr m :}
   k cs CLASS-CHUNKS 1 + CHUNK-BYTES * k CEILING > if false exit then
   k cs POOL-OF {: p:ptr :}
   k p POOL-CHUNKS @ cs ENTRY m MAPPED? 0= if false exit then
   1 p POOL-CHUNKS +! true ;

: UNMAP-POOL ( n ptr n -- )
   {: k:n cs:ptr :}
   k cs POOL-OF {: p:ptr :}
   p POOL-CHUNKS @ 0 ?do
      k i cs ENTRY @ CHUNK-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   loop
   0 p POOL-CHUNKS ! ;

\ Give every chunk of the pools back to the OS.
: UNMAP ( ptr n -- )
   {: cs:ptr :}
   KINDS 0 do i cs UNMAP-POOL loop ;

\ ---- lifecycle and schemas --------------------------------------------------

\ Map the whole emergency pool.
: MAP-EMERGENCY ( ptr n [ ptr ptr u8 -- ptr ptr u8 ] -- ptr n [ ptr ptr u8 -- ptr ptr u8 ] )
   {: cs:ptr m :}
   EMERGENCY# cs POOL-OF {: p:ptr :}
   EMERGENCY# MAX-CHUNKS 0 do
      EMERGENCY# i cs ENTRY m execute drop
      1 p POOL-CHUNKS +!
   loop
   cs m ;

: INIT-WITH ( ptr n [ ptr ptr u8 -- ptr ptr u8 ] -- pools )
   {: cs:ptr m :}
   cs 0= if E-RT-POOL-NULL throw then
   cs >STATE {: st:ptr :}
   st STATE-MODE @ 0<> if E-RT-POOL-HELD throw then
   0 0 0 0 0 STATE-MAKE st !
   KINDS 0 do 0 0 0 0 i CEILING POOL-MAKE i cs POOL-OF ! loop
   cs m [: MAP-EMERGENCY ;] catch {: c q code:n :}
   code 0<> if cs UNMAP code throw then
   OPEN st STATE-MODE !
   cs >POOLS ;

\ The registry: each schema's pool and enumerator, by schema number - 1.
MAX-SCHEMAS TYPED-BUFFER SCHEMA-KINDS n
MAX-SCHEMAS TYPED-BUFFER ENUMERATORS [ RT-HANDLE:handle n pools -- RT-HANDLE:handle ]
variable SCHEMAS

\ The schema's number - 1, refusing the zero token and any number SCHEMA has
\ not answered before the registry is read.
: SCHEMA# ( schema -- n )
   SCHEMA>N {: s:n :}
   s 1 < s SCHEMAS @ > or if E-RT-POOL-SCHEMA throw then
   s 1 - ;

public

\ Open pools over the POOLS-CELLS cells at the address and map the whole
\ emergency pool, so that an emergency reservation makes no mapping calls. A
\ refused mapping is thrown with every chunk given back and the cells left to
\ INIT again.
: INIT ( ptr n -- pools )
   [: MAP-CHUNK ;] INIT-WITH ;

\ Give every chunk back and close the pools. Each handle they issued then
\ dangles, and only the runtime's epoch tells it from one that other pools
\ issue (§2.1).
: SHUTDOWN ( pools -- )
   OPEN-CELLS {: cs:ptr :}
   cs UNMAP
   SHUT cs >STATE STATE-MODE ! ;

\ A schema of objects of the pool. Its enumerator answers an object's child at
\ an edge, the edges numbered from 0 with no gap, one for each non-null
\ reference the object's reference cells hold, and the null handle past the
\ last. It reads the object, which RECLAIM-STEP is reclaiming, through REF@
\ and CELL@ while it runs, and changes nothing.
: SCHEMA ( kind [ RT-HANDLE:handle n pools -- RT-HANDLE:handle ] -- schema )
   {: k:kind e :}
   SCHEMAS @ {: s:n :}
   s MAX-SCHEMAS >= if E-RT-POOL-SCHEMA throw then
   e s ENUMERATORS !
   k KIND>N s SCHEMA-KINDS !
   s 1 + SCHEMAS !
   s 1 + >SCHEMA ;

private

\ ---- reserving --------------------------------------------------------------

: PAYLOAD# ( n -- n )
   SLOT-SIZE HEAD-BYTES - 1 cells / ;

\ Whether the pool can issue a slot within its quota: a free one, one of a
\ mapped chunk never issued, or one of a chunk mapped for it now.
: ROOM? ( n ptr n [ ptr ptr u8 -- ptr ptr u8 ] -- bool )
   {: k:n cs:ptr m :}
   k cs POOL-OF {: p:ptr :}
   p POOL-CHARGED @ k SLOT-SIZE + p POOL-QUOTA @ > if false exit then
   p POOL-FREE @ 0<> if true exit then
   p POOL-FRESH @ p POOL-CHUNKS @ k PER-CHUNK * < if true exit then
   k cs m GROW ;

\ The index and generation of the slot to issue: the first free one at its
\ next generation, else the first never issued, whose chunk ROOM? has mapped.
: TAKE ( n ptr n -- n n )
   {: k:n cs:ptr :}
   k cs POOL-OF {: p:ptr :}
   p POOL-FREE @ {: f:n :}
   f 0<> if
      f 1 - {: ix:n :}
      k ix cs SLOT-AT >HEAD {: hd:ptr :}
      hd HEAD-LINK @ p POOL-FREE !
      ix hd HEAD-STAMP @ U32-MAX and 1 + exit
   then
   p POOL-FRESH @ {: ix:n :}
   ix 1 + p POOL-FRESH !
   ix 1 ;

\ Issue a slot of the pool to an object of the schema, held once, its payload
\ zeroed, and charge the slot's bytes.
: CLAIM ( n n ptr n -- RT-HANDLE:handle )
   {: s:n k:n cs:ptr :}
   k cs TAKE {: ix:n g:n :}
   k ix cs SLOT-AT {: sl:ptr :}
   k PAYLOAD# 0 ?do 0 sl HEAD-BYTES + i cells + CELL-VIEW ! loop
   g LIVE or 1 s 0 HEAD-MAKE sl >HEAD !
   k SLOT-SIZE k cs POOL-OF POOL-CHARGED +!
   k FIRSTS @ ix + g RT-HANDLE:HANDLE ;

: RESERVE-IN ( n ptr n [ ptr ptr u8 -- ptr ptr u8 ] -- reservation )
   {: s:n cs:ptr m :}
   s SCHEMA-KINDS @ {: k:n :}
   k cs m ROOM? 0= if RT--POOL-RESERVATION:oom exit then
   s k cs CLAIM RT--POOL-RESERVATION:granted ;

public

\ A slot of the schema's pool for an object held once, its payload zeroed;
\ oom, with nothing changed, when the pool's quota, its class's ceiling or the
\ OS refuses. The emergency pool is not reserved from here.
: RESERVE ( schema pools -- reservation )
   {: sc:schema p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   sc SCHEMA# {: s:n :}
   s SCHEMA-KINDS @ EMERGENCY# = if E-RT-POOL-EMERGENCY throw then
   s cs [: MAP-CHUNK ;] RESERVE-IN ;

\ RESERVE from the emergency pool, which nothing else reserves from.
: RESERVE-EMERGENCY ( schema pools -- reservation )
   {: sc:schema p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   sc SCHEMA# {: s:n :}
   s SCHEMA-KINDS @ EMERGENCY# <> if E-RT-POOL-EMERGENCY throw then
   s cs [: MAP-CHUNK ;] RESERVE-IN ;

private

\ ---- references -------------------------------------------------------------

\ Release the object whose handle's bits these are; the last release queues it.
: DROP-REF ( n ptr n -- )
   {: v:n cs:ptr :}
   v cs LOCATE {: sl:ptr k:n ix:n :}
   sl >HEAD {: hd:ptr :}
   hd HEAD-COUNT @ {: c:n :}
   c 0= if E-RT-POOL-DEAD throw then
   c 1 - hd HEAD-COUNT !
   c 1 = if
      cs >STATE {: st:ptr :}
      st STATE-QUEUE @ hd HEAD-LINK !
      v st STATE-QUEUE !
   then ;

\ The slot and pool of an object held at least once.
: HELD ( n ptr n -- ptr u8 n )
   {: v:n cs:ptr :}
   v cs LOCATE {: sl:ptr k:n ix:n :}
   sl >HEAD HEAD-COUNT @ 0= if E-RT-POOL-DEAD throw then
   sl k ;

\ The slot and pool of an object held at least once that FREEZE has not
\ frozen.
: WRITABLE ( n ptr n -- ptr u8 n )
   {: v:n cs:ptr :}
   v cs HELD {: sl:ptr k:n :}
   sl >HEAD HEAD-STAMP @ FROZEN and 0<> if E-RT-POOL-FROZEN throw then
   sl k ;

\ The slot and pool of an object held at least once, or of the one whose
\ enumerator RECLAIM-STEP is running.
: READABLE ( n ptr n -- ptr u8 n )
   {: v:n cs:ptr :}
   v cs LOCATE {: sl:ptr k:n ix:n :}
   sl >HEAD HEAD-COUNT @ 0= v cs >STATE STATE-READING @ <> and if E-RT-POOL-DEAD throw then
   sl k ;

\ A payload cell of an object of the pool.
: PAYLOAD ( ptr u8 n n -- ptr n )
   {: sl:ptr k:n j:n :}
   j 0 < j k PAYLOAD# >= or if E-RT-POOL-CELL throw then
   sl HEAD-BYTES + j cells + CELL-VIEW ;

public

: RETAIN ( RT-HANDLE:handle pools -- )
   {: h p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs HELD {: sl:ptr k:n :}
   sl >HEAD {: hd:ptr :}
   hd HEAD-COUNT @ {: c:n :}
   c LAST-COUNT = if E-RT-POOL-OVERFLOW throw then
   c 1 + hd HEAD-COUNT ! ;

\ The last release links the object onto the retirement queue through its own
\ header, and frees nothing.
: RELEASE ( RT-HANDLE:handle pools -- )
   {: h p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs DROP-REF ;

: CELL@ ( RT-HANDLE:handle n pools -- n )
   {: h j:n p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs READABLE {: sl:ptr k:n :}
   sl k j PAYLOAD @ ;

: CELL! ( n RT-HANDLE:handle n pools -- )
   {: v:n h j:n p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs WRITABLE {: sl:ptr k:n :}
   v sl k j PAYLOAD ! ;

\ The child in a payload cell REF! filled, or the null handle.
: REF@ ( RT-HANDLE:handle n pools -- RT-HANDLE:handle )
   {: h j:n p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs READABLE {: sl:ptr k:n :}
   sl k j PAYLOAD @ BITS>HANDLE ;

\ Hold the child, or the null handle, in a payload cell of the object. The cell
\ holds its own reference: the child is retained and the one it replaces
\ released, and the child the cell already holds is neither.
: REF! ( RT-HANDLE:handle RT-HANDLE:handle n pools -- )
   {: c h j:n p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs WRITABLE {: sl:ptr k:n :}
   sl k j PAYLOAD {: at:ptr :}
   at @ {: old:n :}
   c HANDLE>BITS {: new:n :}
   old new = if exit then
   new 0<> if c p RETAIN then
   new at !
   old 0<> if old cs DROP-REF then ;

\ Freeze the object: CELL! and REF! refuse it from now until RECLAIM-STEP
\ returns its slot. An object frozen already stays so.
: FREEZE ( RT-HANDLE:handle pools -- )
   {: h p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   h HANDLE>BITS cs HELD {: sl:ptr k:n :}
   sl >HEAD {: hd:ptr :}
   hd HEAD-STAMP @ FROZEN or hd HEAD-STAMP ! ;

private

\ ---- reclamation ------------------------------------------------------------

\ Return the slot of the object whose bits these are to its pool for its next
\ generation, or retire the slot at its last, and uncharge its bytes.
: RETURN-SLOT ( n ptr n -- )
   {: v:n cs:ptr :}
   v cs LOCATE {: sl:ptr k:n ix:n :}
   k cs POOL-OF {: p:ptr :}
   v 32 rshift {: g:n :}
   g LAST-GEN = if
      g 0 0 0 HEAD-MAKE sl >HEAD !
   else
      g 0 0 p POOL-FREE @ HEAD-MAKE sl >HEAD !
      ix 1 + p POOL-FREE !
   then
   k SLOT-SIZE negate p POOL-CHARGED +! ;

\ Whether an object is left to visit: the one being visited, else the next off
\ the queue, which becomes it.
: DYING? ( ptr n -- bool )
   {: cs:ptr :}
   cs >STATE {: st:ptr :}
   st STATE-DYING @ 0<> if true exit then
   st STATE-QUEUE @ {: v:n :}
   v 0= if false exit then
   v cs LOCATE {: sl:ptr k:n ix:n :}
   sl >HEAD HEAD-LINK @ st STATE-QUEUE !
   v st STATE-DYING !
   0 st STATE-EDGE !
   true ;

\ Visit the dying object's next edge through its schema's enumerator, which
\ may read the object while it runs: release the child there, or, past the
\ last, return the object's slot.
: VISIT-EDGE ( ptr n pools -- ptr n pools )
   {: cs:ptr p:pools :}
   cs >STATE {: st:ptr :}
   st STATE-DYING @ {: v:n :}
   v cs LOCATE {: sl:ptr k:n ix:n :}
   v st STATE-READING !
   v BITS>HANDLE st STATE-EDGE @ p sl >HEAD HEAD-SCHEMA @ ENUMERATORS @ execute
   0 st STATE-READING !
   HANDLE>BITS {: c:n :}
   c 0= if v cs RETURN-SLOT 0 st STATE-DYING ! cs p exit then
   c cs DROP-REF
   1 st STATE-EDGE +!
   cs p ;

\ VISIT-EDGE, closing the object to reads again when it throws.
: VISIT ( ptr n pools -- )
   {: cs:ptr p:pools :}
   cs p [: VISIT-EDGE ;] catch {: c q code:n :}
   code 0<> if 0 cs >STATE STATE-READING ! code throw then ;

public

\ Visit at most the budget's edges of objects released for good, and answer
\ how many: fewer than the budget only once the queue is empty. Each call of
\ an enumerator is one edge, the one past an object's last included.
: RECLAIM-STEP ( n pools -- n )
   {: budget:n p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   budget 1 < if E-RT-POOL-RANGE throw then
   0 begin
      dup budget < while
      cs DYING? 0= if exit then
      cs p VISIT
      1 +
   repeat ;

\ ---- quotas and ledgers -----------------------------------------------------

\ The most bytes the pool may charge, from zero up to its class's ceiling,
\ where INIT sets it. A reserve that would charge past it answers oom.
: QUOTA! ( n kind pools -- )
   {: q:n k:kind p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   k KIND>N {: kn:n :}
   q 0 < q kn CEILING > or if E-RT-POOL-RANGE throw then
   q kn cs POOL-OF POOL-QUOTA ! ;

\ The bytes the pool charges: a slot's for each object it holds, however many
\ references hold the object, until RECLAIM-STEP returns the slot.
: CHARGED ( kind pools -- n )
   {: k:kind p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   k KIND>N cs POOL-OF POOL-CHARGED @ ;

\ The bytes of the chunks the pool has mapped.
: MAPPED ( kind pools -- n )
   {: k:kind p:pools :}
   p OPEN-CELLS {: cs:ptr :}
   k KIND>N cs POOL-OF POOL-CHUNKS @ CHUNK-BYTES * ;

\ The payload cells of an object of the pool.
: PAYLOAD-CELLS ( kind -- n )
   KIND>N PAYLOAD# ;

;package
