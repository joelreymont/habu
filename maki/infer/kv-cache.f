\ maki/infer/kv-cache.f - the UMA-simplified paged KV cache allocator (phase 1 of
\ the GB10 UMA inference engine, dot habu-infer-paged-kv-53b72853).
\
\ CONCERN: the host-side page bookkeeping for a vLLM-class KV cache tailored to the
\ GB10's ONE coherent memory pool. There is no swap tier and no GPU<->host block
\ migration (UMA deletes them), so the cache collapses to: a fixed-size page pool over
\ one MEM:ALLOC-BYTES mapping, a per-page free stack, a per-page refcount for prefix
\ sharing, and a per-sequence BLOCK TABLE (a lib/vector.f cell vector of page ids).
\ The device decode kernel later reads pages via the block table; nothing here touches
\ the device.
\
\ PAGE GEOMETRY (no magic numbers - sized from model dims through a config record).
\ A page holds KV-P tokens. One token's KV footprint is
\   tok-bytes  = n_kv_heads * head_dim * 2 (K and V) * dtype-bytes
\ and a page is
\   page-bytes = KV-P * tok-bytes.
\ KV-P is the ONE fixed page granularity (16 tokens); every other size is derived from
\ the `kvcfg` record (n_kv_heads, head_dim, dtype-bytes, num-pages, max-seqs) by
\ KV-CALC-TOK / KV-SIZE with an overflow-checked multiply (KV-MUL -> E-KV-CONFIG).
\
\ STORAGE / OWNERSHIP (the lib/vector.f + lib/byte-buffer.f discipline). The cache owns
\ exactly TWO OS mappings, both installed leak-free:
\   - the POOL: num-pages*page-bytes through MEM:ALLOC-BYTES (the coherent KV bytes);
\   - the META block: one MEM:ALLOC-CELLS mapping partitioned into the free stack
\     [num-pages], the refcounts [num-pages], the per-seq lengths [max-seqs], the
\     per-seq live flags [max-seqs], and the per-seq block-table VEC HEADERS
\     [max-seqs * VEC-HEADER-CELLS].
\ Each per-sequence block table owns its OWN growable mapping (VEC:INIT allocates it,
\ VEC:DISPOSE releases it). POOLCAP is the capacity-as-ownership token: nonzero means
\ both mappings are owned (fresh/disposed header reads zero). KV-INIT installs the two
\ mappings under a catch so ANY failure releases what was installed and leaves the
\ header dead (KV-INIT-CLEANUP), and sets the POOLCAP token LAST after a full install.
\ KV-DISPOSE captures the mappings, voids the token BEFORE the releases (consume-on-
\ release: a second dispose or a throwing release cannot double-free on retry), and
\ releases pool + meta. Pool byte extent is always recomputed from num-pages*page-bytes,
\ so the token is a pure liveness cell, never conflated with the release size.
\
\ REFCOUNTS + PREFIX SHARING. refc[pid] == the number of block-table slots (across all
\ live sequences) that point at pid; a page is live iff refc[pid] > 0. KV-FORK-SEQ
\ copies the parent block table byte-identically and bumps every shared page's
\ refcount, so parent and child address the SAME pages until one diverges. Divergence
\ is copy-on-write of the TAIL page only: KV-APPEND-TOKEN, when it writes into a
\ partially-filled tail page whose refc > 1, allocates a fresh page, copies the tail's
\ page-bytes, drops the old refc and sets the new page's refc to 1, and rewrites the
\ last block-table entry. A token that starts a fresh page (pos mod KV-P == 0) never
\ COWs - it allocates exactly one new page. Because a page returns to the free list
\ ONLY on the refc 1->0 transition inside KV-FREE-SEQ, and refc equals the exact number
\ of references, the last reference frees it once and no page can be double-freed;
\ KV-CHECK proves refc[]==recomputed refs, free-top==#(refc==0), every free entry has
\ refc==0, and no page id is on the free stack twice.
\
\ WATERMARK. KV-WATERMARK = num-pages - free-top = live pages, the scheduler's O(1)
\ residency query. Under churn it stays exactly equal to the number of distinct pages
\ referenced across all block tables (== #(refc>0)); the property test recomputes both
\ independently and asserts equality every round.
\
\ Fail closed: non-positive/overflowing model dims (E-KV-CONFIG), a disposed/re-inited
\ cache (E-KV-STATE), an exhausted page pool (E-KV-POOL), a full sequence table
\ (E-KV-SEQS), a stale/out-of-range/dead sequence handle (E-KV-SEQ), an out-of-range
\ token/page index (E-KV-BOUNDS), a broken internal invariant (E-KV-INVARIANT).
\ maki -> habu only; the KV cache owns -5622..-5628.

require lib/prelude.f
require lib/string.f            \ BYTE-COPY: copy-on-write tail-page byte copy
require lib/memory.f            \ MEM:ALLOC-BYTES / MEM:ALLOC-CELLS / MEM:RELEASE-BYTES
require lib/vector.f            \ the landed block-table vector discipline
require lib/type/deftype.f      \ DEFTYPE KVSEQ: a sequence handle is its own type

-5622 constant E-KV-CONFIG      \ non-positive model dim, or a page/pool size that overflows a cell
-5623 constant E-KV-STATE       \ touch of a disposed/uninitialized cache, or re-init of a live one
-5624 constant E-KV-POOL        \ the page pool is exhausted (no free page)
-5625 constant E-KV-SEQS        \ the sequence table is full (no free sequence slot)
-5626 constant E-KV-SEQ         \ stale, out-of-range, or dead (double-freed) sequence handle
-5627 constant E-KV-BOUNDS      \ token index / page id outside the sequence or pool
-5628 constant E-KV-INVARIANT   \ recomputed refcount / free-list invariant broken (KV-CHECK)

package KV
public

\ ---- the config record: model dims, page size derived (no field is a magic number) --
\ Fields (declaration order = KV-KVCFG:UNMAKE order): nkv = n_kv_heads, hdim = head_dim,
\ dbytes = bytes per element (fp16/bf16 -> 2), npages = pool size in pages, nseq = max
\ concurrent sequences. (Inline `\` notes inside a PRODUCT block are a parse error.)
PRODUCT kvcfg 0
   FIELD nkv n
   FIELD hdim n
   FIELD dbytes n
   FIELD npages n
   FIELD nseq n
;PRODUCT

private

\ ---- a sequence handle is a validated slot index, kept distinct from a page id -------
DEFTYPE KVSEQ                    \ >KVSEQ ( n -- kvseq ) / KVSEQ>N ( kvseq -- n )

16 constant KV-P                \ fixed page granularity: tokens per page
8  constant KV-BLK-CAP0         \ initial block-table capacity (pages) per sequence

\ ---- header layout: two pointer fields + ten scalar cells --------------------------
0  constant POOL-CX             \ cell index of the pool pointer (ptr u8) for ptr-field
2  constant META-CX             \ cell index of the meta pointer (ptr a) for ptr-field
1  cells constant POOLCAP-OFF   \ pool byte capacity == the liveness/ownership token
3  cells constant METAC-OFF     \ meta mapping cell count (for release)
4  cells constant NPAGES-OFF
5  cells constant NSEQ-OFF
6  cells constant PAGEB-OFF     \ page-bytes
7  cells constant TOKB-OFF      \ tok-bytes
8  cells constant FREETOP-OFF   \ free-stack depth (free pages)
9  cells constant NKV-OFF
10 cells constant HDIM-OFF
11 cells constant DBYTES-OFF
12 cells constant HDR-SIZE      \ header byte footprint the caller allots

\ ---- pointer fields (ptr-field preserves the nested ptr u8 / ptr a role) ------------
: POOL-FIELD ( ptr a -- ptr ptr u8 )  POOL-CX ptr-field ;
: POOL@ ( ptr a -- ptr u8 )  POOL-FIELD @ ;
: POOL! ( ptr u8 ptr a -- ) {: p:ptr h:ptr :}  p h POOL-FIELD ! ;
: META-FIELD ( ptr a -- ptr ptr a )  META-CX ptr-field ;
: META@ ( ptr a -- ptr a )  META-FIELD @ ;
: META! ( ptr a ptr a -- ) {: m:ptr h:ptr :}  m h META-FIELD ! ;

\ ---- scalar cell fields (one generic byte-offset accessor pair) ---------------------
: H@ ( ptr a n -- n )  + @ ;
: H! ( n ptr a n -- ) {: v:n h:ptr off:n :}  v h off + ! ;

\ ---- config sizing: overflow-checked multiply, then the derived page geometry -------
: KV-POS ( n -- )  0 <= if E-KV-CONFIG throw then ;
: KV-MUL ( n n -- n ) {: a:n b:n :}     \ a*b with a divide-back overflow guard
   a KV-POS  b KV-POS
   a b *  {: r:n :}
   r a / b <> if E-KV-CONFIG throw then
   r ;
: KV-CALC-TOK ( n n n -- n )  {: nkv:n hdim:n dbytes:n :}   \ bytes for one token's KV
   nkv hdim KV-MUL  2 KV-MUL  dbytes KV-MUL ;
: KV-SIZE ( n n n n -- n n )  {: nkv:n hdim:n dbytes:n npages:n :}
   nkv hdim dbytes KV-CALC-TOK {: tokb:n :}
   KV-P tokb KV-MUL {: pageb:n :}
   npages pageb KV-MUL drop             \ probe: the whole pool fits one cell
   pageb tokb ;
: KV-META-CELLS ( n n -- n ) {: np:n ns:n :}    \ free + refc + len + live + block headers
   np 2 *  ns 2 * +  ns VEC-HEADER-CELLS *  + ;
: KV-POOL-BYTES ( ptr a -- n ) {: h:ptr :}  h NPAGES-OFF H@  h PAGEB-OFF H@ * ;

\ ---- n -> CAD-NUM role bridges for the block-table VEC surface (sched-key idiom) -----
\ A block-table capacity is a CAD-NUM:item-count, a page position a CAD-NUM:index, so a
\ count/index swap at a VEC call is a checker reject. The refusal arms are unreachable
\ invariants (a capacity and a live index are nonnegative); an impossible negative
\ surfaces the vector's own capacity / bounds code.
: KV>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: KV>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- meta region bases (each a ptr a into the one meta mapping) ---------------------
: FREE-BASE ( ptr a -- ptr a )  META@ ;
: REFC-BASE ( ptr a -- ptr a ) {: h:ptr :}  h META@  h NPAGES-OFF H@ cells + ;
: SEQLEN-BASE ( ptr a -- ptr a ) {: h:ptr :}  h META@  h NPAGES-OFF H@ 2 * cells + ;
: SEQLIVE-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h META@  h NPAGES-OFF H@ 2 *  h NSEQ-OFF H@ +  cells + ;
: SEQBLK-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h META@  h NPAGES-OFF H@ 2 *  h NSEQ-OFF H@ 2 * +  cells + ;

\ ---- meta element access ------------------------------------------------------------
: FREE@ ( ptr a n -- n ) {: h:ptr i:n :}  h FREE-BASE i cells + @ ;
: FREE! ( n ptr a n -- ) {: pid:n h:ptr i:n :}  pid  h FREE-BASE i cells +  ! ;
: REFC@ ( ptr a n -- n ) {: h:ptr pid:n :}  h REFC-BASE pid cells + @ ;
: REFC! ( n ptr a n -- ) {: rc:n h:ptr pid:n :}  rc  h REFC-BASE pid cells +  ! ;
: SEQLEN@ ( ptr a n -- n ) {: h:ptr s:n :}  h SEQLEN-BASE s cells + @ ;
: SEQLEN! ( n ptr a n -- ) {: v:n h:ptr s:n :}  v  h SEQLEN-BASE s cells +  ! ;
: SEQLIVE@ ( ptr a n -- n ) {: h:ptr s:n :}  h SEQLIVE-BASE s cells + @ ;
: SEQLIVE! ( n ptr a n -- ) {: v:n h:ptr s:n :}  v  h SEQLIVE-BASE s cells +  ! ;
: SEQBLK ( ptr a n -- ptr a ) {: h:ptr s:n :}  h SEQBLK-BASE  s VEC-HEADER-CELLS * cells + ;

\ ---- refcount helpers ---------------------------------------------------------------
: KV-REF+ ( ptr a n -- ) {: h:ptr pid:n :}  h pid REFC@ 1+  h pid REFC! ;
: KV-REF- ( ptr a n -- n ) {: h:ptr pid:n :}  h pid REFC@ 1-  dup h pid REFC! ;   \ -> new refc

\ ---- free stack ---------------------------------------------------------------------
: KV-POP-FREE ( ptr a -- n ) {: h:ptr :}     \ take a free page id or reject exhaustion
   h FREETOP-OFF H@ {: top:n :}
   top 0 <= if E-KV-POOL throw then
   top 1- {: t:n :}
   h t FREE@ {: pid:n :}
   t h FREETOP-OFF H!
   pid ;
: KV-PUSH-FREE ( n ptr a -- ) {: pid:n h:ptr :}   \ return a page to the free stack
   h FREETOP-OFF H@ {: top:n :}
   pid h top FREE!
   top 1+ h FREETOP-OFF H! ;
: KV-FILL-FREE ( ptr a -- ) {: h:ptr :}      \ every page free, in id order
   h NPAGES-OFF H@ {: np:n :}
   np 0 ?do  i h i FREE!  loop
   np h FREETOP-OFF H! ;

\ ---- pool byte addressing -----------------------------------------------------------
: KV-PAGE-ADDR ( ptr a n -- ptr u8 ) {: h:ptr pid:n :}  h POOL@  pid h PAGEB-OFF H@ *  + ;

\ ---- block-table access -------------------------------------------------------------
: KV-BLK-LEN ( ptr a n -- n ) {: h:ptr s:n :}  h s SEQBLK VEC-LEN@ LEN>N ;
: KV-BLK@ ( ptr a n n -- n ) {: h:ptr s:n i:n :}  h s SEQBLK  i KV>INDEX VEC:@ ;
: KV-BLK! ( n ptr a n n -- ) {: pid:n h:ptr s:n i:n :}  pid  h s SEQBLK  i KV>INDEX VEC:! ;
: KV-BLK-PUSH ( n ptr a n -- ) {: pid:n h:ptr s:n :}  pid  h s SEQBLK  VEC:PUSH drop ;

\ ---- liveness / handle validation ---------------------------------------------------
: KV-LIVE-CK ( ptr a -- ) {: h:ptr :}  h POOLCAP-OFF H@ 0= if E-KV-STATE throw then ;
: KV-SEQ-CK ( ptr a kvseq -- n ) {: h:ptr sq:kvseq :}   \ live cache + valid live slot -> index
   h KV-LIVE-CK
   sq KVSEQ>N {: s:n :}
   s 0 < if E-KV-SEQ throw then
   s h NSEQ-OFF H@ >= if E-KV-SEQ throw then
   h s SEQLIVE@ 0= if E-KV-SEQ throw then
   s ;

\ ---- append: grow a fresh page, or copy-on-write a shared tail page ------------------
: KV-GROW-PAGE ( ptr a n -- ) {: h:ptr s:n :}   \ pos crossed a page boundary: one new page
   h KV-POP-FREE {: pid:n :}
   1 h pid REFC!
   pid h s KV-BLK-PUSH ;
: KV-COW-TAIL ( ptr a n -- ) {: h:ptr s:n :}    \ writing into the tail page: COW it if shared
   h s KV-BLK-LEN 1- {: last:n :}
   h s last KV-BLK@ {: tail:n :}
   h tail REFC@ 1 > if
      h KV-POP-FREE {: np:n :}
      h tail KV-PAGE-ADDR  h np KV-PAGE-ADDR  h PAGEB-OFF H@  BYTE-COPY
      h tail REFC@ 1-  h tail REFC!
      1 h np REFC!
      np h s last KV-BLK!
   then ;
: KV-APPEND-TOKEN ( ptr a kvseq -- ) {: h:ptr sq:kvseq :}
   h sq KV-SEQ-CK {: s:n :}
   h s SEQLEN@ {: len:n :}
   len KV-P mod 0= if h s KV-GROW-PAGE else h s KV-COW-TAIL then
   len 1+ h s SEQLEN! ;

\ ---- sequence slots -----------------------------------------------------------------
: KV-FIND-SLOT ( ptr a -- n ) {: h:ptr :}    \ first dead slot, else the table is full
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0= if i unloop exit then
   loop  E-KV-SEQS throw ;
: KV-OPEN-SLOT ( ptr a -- n ) {: h:ptr :}    \ claim a dead slot: live, empty, fresh block table
   h KV-FIND-SLOT {: s:n :}
   1 h s SEQLIVE!
   0 h s SEQLEN!
   h s SEQBLK  KV-BLK-CAP0 KV>ITEM VEC:INIT
   s ;
: KV-COPY-BLK ( ptr a n n -- ) {: h:ptr c:n p:n :}   \ share parent p's pages into child c
   h p KV-BLK-LEN {: n:n :}
   h c SEQBLK  n KV-BLK-CAP0 max KV>ITEM VEC:ENSURE
   n 0 ?do
      h p i KV-BLK@ {: pid:n :}
      pid h c KV-BLK-PUSH
      h pid KV-REF+
   loop ;

: KV-ALLOC-SEQ ( ptr a -- kvseq ) {: h:ptr :}
   h KV-LIVE-CK
   h KV-OPEN-SLOT >KVSEQ ;
: KV-FORK-SEQ ( ptr a kvseq -- kvseq ) {: h:ptr sq:kvseq :}
   h sq KV-SEQ-CK {: p:n :}
   h KV-OPEN-SLOT {: c:n :}
   h c p KV-COPY-BLK
   h p SEQLEN@  h c SEQLEN!
   c >KVSEQ ;
: KV-FREE-SEQ ( ptr a kvseq -- ) {: h:ptr sq:kvseq :}
   h sq KV-SEQ-CK {: s:n :}
   h s KV-BLK-LEN 0 ?do
      h s i KV-BLK@ {: pid:n :}
      h pid KV-REF- 0= if pid h KV-PUSH-FREE then
   loop
   h s SEQBLK VEC:DISPOSE
   0 h s SEQLIVE!
   0 h s SEQLEN! ;

\ ---- watermark + read-only introspection --------------------------------------------
: KV-WATERMARK ( ptr a -- n ) {: h:ptr :}
   h KV-LIVE-CK  h NPAGES-OFF H@  h FREETOP-OFF H@ - ;
: KV-SEQ-LEN ( ptr a kvseq -- n ) {: h:ptr sq:kvseq :}  h  h sq KV-SEQ-CK  SEQLEN@ ;
: KV-SEQ-PAGES ( ptr a kvseq -- n ) {: h:ptr sq:kvseq :}  h  h sq KV-SEQ-CK  KV-BLK-LEN ;
: KV-SEQ-PAGE ( ptr a kvseq n -- n ) {: h:ptr sq:kvseq i:n :}  h  h sq KV-SEQ-CK  i KV-BLK@ ;
: KV-PAGE-REFC ( ptr a n -- n ) {: h:ptr pid:n :}
   h KV-LIVE-CK
   pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-BOUNDS throw then
   h pid REFC@ ;
: KV-TOKEN-PTR ( ptr a kvseq n -- ptr u8 ) {: h:ptr sq:kvseq tok:n :}
   h sq KV-SEQ-CK {: s:n :}
   tok 0 < tok h s SEQLEN@ >= or if E-KV-BOUNDS throw then
   tok KV-P / {: pageix:n :}
   tok KV-P mod {: slot:n :}
   h s pageix KV-BLK@ {: pid:n :}
   h pid KV-PAGE-ADDR  slot h TOKB-OFF H@ *  + ;
: KV-LIVE? ( ptr a kvseq -- bool ) {: h:ptr sq:kvseq :}
   h POOLCAP-OFF H@ 0= if false exit then
   sq KVSEQ>N {: s:n :}
   s 0 < s h NSEQ-OFF H@ >= or if false exit then
   h s SEQLIVE@ 0<> ;

\ ---- invariant checker: the double-free-impossible / watermark-exact proof ----------
: KV-SEQ-REFS ( ptr a n n -- n ) {: h:ptr s:n pid:n :}   \ occurrences of pid in seq s block table
   0
   h s KV-BLK-LEN 0 ?do
      h s i KV-BLK@ pid = if 1+ then
   loop ;
: KV-COUNT-REFS ( ptr a n -- n ) {: h:ptr pid:n :}       \ references to pid across all live seqs
   0
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0<> if  h i pid KV-SEQ-REFS +  then
   loop ;
: KV-CHECK-REFS ( ptr a -- ) {: h:ptr :}     \ stored refc equals the recomputed reference count
   h NPAGES-OFF H@ 0 ?do
      h i REFC@  h i KV-COUNT-REFS  <> if E-KV-INVARIANT throw then
   loop ;
: KV-CHECK-FREE ( ptr a -- ) {: h:ptr :}     \ free-top == #(refc==0); every free entry has refc==0
   0  h NPAGES-OFF H@ 0 ?do  h i REFC@ 0= if 1+ then  loop
   h FREETOP-OFF H@ <> if E-KV-INVARIANT throw then
   h FREETOP-OFF H@ 0 ?do
      h i FREE@ {: pid:n :}
      pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-INVARIANT throw then
      h pid REFC@ 0<> if E-KV-INVARIANT throw then
   loop ;
: KV-CHECK-NODUP ( ptr a -- ) {: h:ptr :}    \ no page id sits on the free stack twice
   h FREETOP-OFF H@ {: top:n :}
   top 0 ?do
      h i FREE@ {: pid:n :}
      top i 1+ ?do
         h i FREE@ pid = if E-KV-INVARIANT throw then
      loop
   loop ;
: KV-CHECK ( ptr a -- ) {: h:ptr :}
   h KV-LIVE-CK
   h KV-CHECK-REFS
   h KV-CHECK-FREE
   h KV-CHECK-NODUP ;

\ ---- storage install / release (leak-free, consume-on-release) ----------------------
PTR-VARIABLE KV-IH              \ header under construction, parked for the catch cleanup

: KV-ALLOC-META ( ptr a -- ) {: h:ptr :}
   h METAC-OFF H@ MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS  h META! ;
: KV-ALLOC-POOL ( ptr a -- ) {: h:ptr :}
   h KV-POOL-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop  h POOL! ;
: KV-RELEASE-META ( ptr a -- ) {: h:ptr :}
   h META@  h METAC-OFF H@ >COUNT MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;
: KV-RELEASE-POOL ( ptr a -- ) {: h:ptr :}
   h POOL@  h KV-POOL-BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;
: KV-INSTALL-BODY ( -- )        \ read the parked header; install both mappings + free stack
   KV-IH @ {: h:ptr :}
   h KV-ALLOC-META
   h KV-ALLOC-POOL
   h KV-FILL-FREE ;
: KV-INIT-CLEANUP ( -- )        \ release whatever the aborted install owned; leave header dead
   KV-IH @ {: h:ptr :}
   h META@ 0= 0= if h KV-RELEASE-META then     \ 0= 0= is the non-null test over a typed pointer
   h POOL@ 0= 0= if h KV-RELEASE-POOL then
   0 h POOLCAP-OFF H! ;

: KV-STORE-DIMS ( ptr a n n n n n -- ) {: h:ptr nkv:n hdim:n dbytes:n npages:n nseq:n :}
   nkv hdim dbytes npages KV-SIZE {: pageb:n tokb:n :}
   nkv    h NKV-OFF H!   hdim   h HDIM-OFF H!   dbytes h DBYTES-OFF H!
   npages h NPAGES-OFF H!   nseq h NSEQ-OFF H!
   pageb  h PAGEB-OFF H!   tokb h TOKB-OFF H!
   npages nseq KV-META-CELLS h METAC-OFF H! ;

\ typed-local-lint: allow-bare-local - `cfg` is a KV:kvcfg product; a colon-qualified
\ family type cannot be spelled in a local annotation, so it is inferred from the stack.
: KV-INIT ( ptr a KV:kvcfg -- ) {: h:ptr cfg :}
   h POOLCAP-OFF H@ 0<> if E-KV-STATE throw then       \ reject re-init of a live cache
   cfg KV-KVCFG:UNMAKE {: nkv:n hdim:n dbytes:n npages:n nseq:n :}
   h nkv hdim dbytes npages nseq KV-STORE-DIMS
   h KV-IH !
   [: KV-INSTALL-BODY ;] catch {: code:n :}
   code 0<> if KV-INIT-CLEANUP code throw then
   h KV-POOL-BYTES h POOLCAP-OFF H! ;                  \ ownership token set LAST

: KV-DISPOSE-SEQS ( ptr a -- ) {: h:ptr :}   \ release every live block-table mapping
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0<> if  h i SEQBLK VEC:DISPOSE  0 h i SEQLIVE!  then
   loop ;
: KV-DISPOSE ( ptr a -- ) {: h:ptr :}
   h POOLCAP-OFF H@ {: pcap:n :}
   pcap 0= if exit then                       \ dead: proved no-op
   h POOL@ {: pool:ptr :}
   h META@ {: meta:ptr :}
   h METAC-OFF H@ {: mc:n :}
   h KV-POOL-BYTES {: pbytes:n :}
   h KV-DISPOSE-SEQS
   0 h POOLCAP-OFF H!                          \ consume ownership BEFORE the releases
   meta  mc >COUNT MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES
   pool  pbytes MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;

\ ---- config constructor (positivity + page/pool overflow probe) ---------------------
: KV-CONFIG ( n n n n n -- KV:kvcfg ) {: nkv:n hdim:n dbytes:n npages:n nseq:n :}
   nkv KV-POS  hdim KV-POS  dbytes KV-POS  npages KV-POS  nseq KV-POS
   nkv hdim dbytes npages KV-SIZE 2drop
   nkv hdim dbytes npages nseq KV-KVCFG:MAKE ;

public

\ ---- fixed geometry + caller-allotted header size -----------------------------------
: PAGE-TOKENS ( -- n )  KV-P ;
: HDR-BYTES ( -- n )  HDR-SIZE ;

\ ---- config record ------------------------------------------------------------------
: CONFIG ( n n n n n -- KV:kvcfg )  KV-CONFIG ;

\ ---- lifecycle ----------------------------------------------------------------------
: INIT ( ptr a KV:kvcfg -- )  KV-INIT ;
: DISPOSE ( ptr a -- )  KV-DISPOSE ;

\ ---- sequences ----------------------------------------------------------------------
: ALLOC-SEQ ( ptr a -- kvseq )  KV-ALLOC-SEQ ;
: APPEND-TOKEN ( ptr a kvseq -- )  KV-APPEND-TOKEN ;
: FORK-SEQ ( ptr a kvseq -- kvseq )  KV-FORK-SEQ ;
: FREE-SEQ ( ptr a kvseq -- )  KV-FREE-SEQ ;

\ ---- queries ------------------------------------------------------------------------
: WATERMARK ( ptr a -- n )  KV-WATERMARK ;
: SEQ-LEN ( ptr a kvseq -- n )  KV-SEQ-LEN ;
: SEQ-PAGES ( ptr a kvseq -- n )  KV-SEQ-PAGES ;
: SEQ-PAGE ( ptr a kvseq n -- n )  KV-SEQ-PAGE ;
: TOKEN-PTR ( ptr a kvseq n -- ptr u8 )  KV-TOKEN-PTR ;
: PAGE-REFC ( ptr a n -- n )  KV-PAGE-REFC ;
: LIVE? ( ptr a kvseq -- bool )  KV-LIVE? ;
: NUM-PAGES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK  h NPAGES-OFF H@ ;
: PAGE-BYTES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK  h PAGEB-OFF H@ ;
: TOK-BYTES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK  h TOKB-OFF H@ ;

\ ---- invariant proof (throws E-KV-INVARIANT on a broken accounting) ------------------
: CHECK ( ptr a -- )  KV-CHECK ;

;package
