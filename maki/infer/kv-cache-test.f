\ maki/infer/kv-cache-test.f - checked paged KV allocator contract.
\ Run: bin/hb --load maki/infer/kv-cache-test.f
\
\ The suite pins the host allocator only. Device-visible snapshots and publication
\ leases belong to habu-lease-kv-snapshot-9ef40f19 and are intentionally absent.

require lib/test.f
require lib/property.f
require maki/infer/kv-cache.f

package KV
private

create KVT-CACHE HDR-BYTES allot
create KVT-OTHER HDR-BYTES allot

: KVT-INIT/P ( n n n n n n -- )
   {: nkv:n hdim:n dbytes:n npages:n nseq:n ptok:n :}
   KVT-CACHE DISPOSE
   nkv hdim dbytes npages nseq npages ptok KV-MUL0 ptok CONFIG/P
   KVT-CACHE swap INIT ;

: KVT-INIT ( n n n n n -- )
   {: nkv:n hdim:n dbytes:n npages:n nseq:n :}
   KVT-CACHE DISPOSE
   nkv hdim dbytes npages nseq npages PAGE-TOKENS KV-MUL0 CONFIG
   KVT-CACHE swap INIT ;

: KVT-INIT/MAX/P ( n n n n n n n -- )
   KVT-CACHE DISPOSE
   CONFIG/P KVT-CACHE swap INIT ;

\ ---- structured sequence-handle storage for caught calls and churn ----------------
create KVT-H-CACHE 9 cells allot
create KVT-H-SLOT  9 cells allot
create KVT-H-GEN   9 cells allot

256 constant KVT-SNAP-CELLS
create KVT-SNAP KVT-SNAP-CELLS cells allot
variable KVT-SNAP-N
PTR-VARIABLE KVT-META-SAVE

: KVT-H! ( kvseq n -- ) {: i:n :}
   KV-KVSEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N i cells KVT-H-CACHE + !
   slot KV-SEQ-SLOT>N i cells KVT-H-SLOT + !
   gen KV-SEQ-GEN>N i cells KVT-H-GEN + ! ;

: KVT-H@ ( n -- kvseq ) {: i:n :}
   i cells KVT-H-CACHE + @ >KV-CACHE-ID
   i cells KVT-H-SLOT + @ >KV-SEQ-SLOT
   i cells KVT-H-GEN + @ >KV-SEQ-GEN
   KV-KVSEQ:MAKE ;

: KVT-SNAPSHOT ( -- )
   HDR-BYTES 1 cells / {: hdrcells:n :}
   KVT-CACHE METAC-OFF H@ {: metacells:n :}
   hdrcells metacells KV-ADD0 dup KVT-SNAP-CELLS > if E-KV-BOUNDS throw then
   KVT-SNAP-N !
   hdrcells 0 ?do KVT-CACHE i cells + @ KVT-SNAP i cells + ! loop
   metacells 0 ?do
      KVT-CACHE META@ i cells + @
      KVT-SNAP hdrcells i + cells + !
   loop ;

: KVT-SNAPSHOT= ( -- bool )
   HDR-BYTES 1 cells / KVT-CACHE METAC-OFF H@ +
   KVT-SNAP-N @ <> if false exit then
   true
   HDR-BYTES 1 cells / 0 ?do
      KVT-CACHE i cells + @ KVT-SNAP i cells + @ = and
   loop
   KVT-CACHE METAC-OFF H@ 0 ?do
      KVT-CACHE META@ i cells + @
      KVT-SNAP HDR-BYTES 1 cells / i + cells + @ = and
   loop ;

\ ---- geometry and checked arithmetic ----------------------------------------------
: KVT-GEOMETRY ( -- )
   2 4 2 8 4 65 16 KVT-INIT/MAX/P
   KVT-CACHE PAGE-SIZE 16 T=
   KVT-CACHE MAX-CONTEXT 65 T=
   KVT-CACHE BLOCK-CAPACITY 5 T=
   KVT-CACHE TOK-BYTES 32 T=
   KVT-CACHE PAGE-BYTES 512 T=
   KVT-CACHE NUM-PAGES 8 T=
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE HIGH-WATER 0 T=
   KVT-CACHE BYTES-PER-TOKEN 32 T=
   KVT-CACHE CHECK ;

: KVT-GENERATED-OPS ( -- )
   KVT-CACHE DISPOSE
   2 4 2 8 4 65 16 CONFIG/P
   KV-KVCFG:UNMAKE {: nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n :}
   nkv 2 T=  hdim 4 T=  dbytes 2 T=  npages 8 T=
   nseq 4 T=  maxctx 65 T=  ptok 16 T=
   nkv hdim dbytes npages nseq maxctx ptok KV-KVCFG:MAKE
   KVT-CACHE swap INIT
   KVT-CACHE 1 ALLOC-SEQ
   KV-KVSEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N 0 > TTRUE
   slot KV-SEQ-SLOT>N 0 T=
   gen KV-SEQ-GEN>N 1 T=
   cid slot gen KV-KVSEQ:MAKE 0 KVT-H!
   KVT-CACHE 0 KVT-H@ SEQ-LEN 0 T=
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE CHECK ;

: KVT-OVERFLOW-TOK ( -- )
   KV-MAX-N 2 2 8 4 16 16 CONFIG/P drop ;

: KVT-OVERFLOW-META ( -- )
   1 1 1 1 KV-MAX-N 1 16 CONFIG/P drop ;

: KVT-OVERFLOW-META-BYTES ( -- )
   1 1 KV-MAX-N 1 cells / 1+ META-CELLS drop ;

: KVT-OVERFLOW-TABLE-META ( -- )
   1 2 KV-MAX-N 2 / 1+ META-CELLS drop ;

: KVT-BAD-BLOCK-CAPACITY ( -- )
   1 1 1 1 1 2 1 CONFIG/P drop ;

: KVT-BAD-PAGE ( -- )
   1 1 1 8 4 1 0 CONFIG/P drop ;

: KVT-CEIL-SAFE ( -- )
   1 1 1 8 4 64 KVT-INIT/P
   KVT-CACHE 0 PAGES-FOR 0 T=
   KVT-CACHE 64 PAGES-FOR 1 T=
   KVT-CACHE 65 PAGES-FOR 2 T=
   KVT-CACHE KV-MAX-N PAGES-FOR
   KV-MAX-N 64 / KV-MAX-N 64 mod 0<> if 1+ then T= ;

\ Four sequence slots, six pages, and a five-page block capacity, so the metadata
\ partition is 6*3 page cells + 4*6 sequence cells + 4*5 block-table cells = 62. All
\ three terms differ at this geometry, so losing or swapping any one of the multipliers
\ moves the total that CONFIG/P and INIT actually allocate.
: KVT-FIXED-TABLES ( -- )
   2 4 2 6 4 65 16 KVT-INIT/MAX/P
   KVT-CACHE META@ KVT-META-SAVE !
   KVT-CACHE METAC-OFF H@ 62 T=
   KVT-CACHE 65 ALLOC-SEQ 0 KVT-H!
   65 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ SEQ-LEN 65 T=
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 5 T=
   KVT-CACHE 0 4 KV-BLK@ KVT-CACHE 0 KVT-H@ 4 SEQ-PAGE T=
   KVT-SNAPSHOT
   [: KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;] catch E-KV-ADMIT T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 0 SEQBLEN@ 0 T=
   KVT-CACHE BLOCK-CAPACITY 0 ?do KVT-CACHE 0 i KV-BLK@ 0 T= loop
   KVT-CACHE 1 ALLOC-SEQ 1 KVT-H!
   KVT-CACHE META@ KVT-META-SAVE @ = TTRUE
   KVT-CACHE METAC-OFF H@ 62 T=
   KVT-CACHE BLOCK-CAPACITY 0 ?do KVT-CACHE 0 i KV-BLK@ 0 T= loop
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   KVT-CACHE CHECK ;

: KVT-HUGE-POOL-INIT ( -- )
   KV-MAX-N 2 / 1 1 1 1 1 1 CONFIG/P KVT-CACHE swap INIT ;

: KVT-INIT-FAIL-CLEANUP ( -- )
   KVT-CACHE DISPOSE
   [: KVT-HUGE-POOL-INIT ;] catch E-MEM-MAP T=
   KVT-CACHE LIVE-CACHE? TFALSE
   2 4 2 8 1 KVT-INIT
   KVT-CACHE LIVE-CACHE? TTRUE
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE CHECK ;

\ ---- token indices where page-index arithmetic can break ---------------------------
\ Six indices carry the page arithmetic for one page size: the first two tokens, the
\ last token of the first page, the first two tokens of the second page, and the final
\ token. A wrong divisor, a wrong remainder, or an off-by-one page index shows up at
\ one of those six; the tokens between them repeat the same arithmetic.
6 constant KVT-BOUNDS-N
create KVT-BOUND-TAB KVT-BOUNDS-N cells allot

: KVT-BOUND! ( n n -- ) {: v:n k:n :}  v k cells KVT-BOUND-TAB + ! ;
: KVT-BOUND@ ( n -- n ) {: k:n :}  k cells KVT-BOUND-TAB + @ ;

: KVT-BOUND-PLAN ( n n -- ) {: ptok:n last:n :}
   0 0 KVT-BOUND!
   1 1 KVT-BOUND!
   ptok 1- 2 KVT-BOUND!
   ptok 3 KVT-BOUND!
   ptok 1+ 4 KVT-BOUND!
   last 5 KVT-BOUND! ;

: KVT-BOUND? ( n -- bool ) {: tok:n :}
   false
   KVT-BOUNDS-N 0 ?do
      i KVT-BOUND@ tok = if drop true then
   loop ;

\ ---- page-token parameterization: exact addressing at 8/16/32/64 ------------------
129 constant KVT-LANE-TOKENS

: KVT-LANE-BYTE ( n -- n ) {: tok:n :}  tok 17 * 3 + 255 and ;

: KVT-LANE-FILL ( kvseq -- )
   8 KVT-H!
   KVT-LANE-TOKENS 0 ?do
      KVT-CACHE 8 KVT-H@ APPEND-TOKEN
      i KVT-LANE-BYTE KVT-CACHE 8 KVT-H@ i TOKEN-PTR c!
   loop ;

: KVT-LANE-SHAPE ( n -- ) {: ptok:n :}
   KVT-CACHE PAGE-SIZE ptok T=
   KVT-CACHE 8 KVT-H@ SEQ-PAGES KVT-LANE-TOKENS ptok / KVT-LANE-TOKENS ptok mod 0<> if 1+ then T= ;

: KVT-LANE-TOK ( n -- ) {: tok:n :}
   KVT-CACHE 8 KVT-H@ tok TOKEN-PTR c@ tok KVT-LANE-BYTE T= ;

\ The exhaustive sweep, for the one page size that gets it.
: KVT-LANE-CHECK ( kvseq n -- ) {: ptok:n :}
   8 KVT-H!
   ptok KVT-LANE-SHAPE
   KVT-LANE-TOKENS 0 ?do i KVT-LANE-TOK loop ;

\ The boundary sweep, for the remaining page sizes.
: KVT-LANE-BOUNDS ( kvseq n -- ) {: ptok:n :}
   8 KVT-H!
   ptok KVT-LANE-SHAPE
   KVT-BOUNDS-N 0 ?do i KVT-BOUND@ KVT-LANE-TOK loop ;

: KVT-LANE-SETUP ( n -- ) {: ptok:n :}
   2 4 2 64 4 ptok KVT-INIT/P
   ptok KVT-LANE-TOKENS 1- KVT-BOUND-PLAN
   KVT-CACHE KVT-LANE-TOKENS ALLOC-SEQ 0 KVT-H!
   0 KVT-H@ KVT-LANE-FILL ;

: KVT-LANE-FULL ( n -- ) {: ptok:n :}
   ptok KVT-LANE-SETUP
   0 KVT-H@ ptok KVT-LANE-CHECK
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   1 KVT-H@ ptok KVT-LANE-CHECK
   KVT-CACHE CHECK ;

: KVT-LANE ( n -- ) {: ptok:n :}
   ptok KVT-LANE-SETUP
   0 KVT-H@ ptok KVT-LANE-BOUNDS
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   1 KVT-H@ ptok KVT-LANE-BOUNDS
   KVT-CACHE CHECK ;

\ The production page size proves every token index on the parent and on the fork;
\ the other three page sizes prove the six indices that can break, on both handles.
: KVT-PAGE-TOKENS ( -- )
   8 KVT-LANE  16 KVT-LANE-FULL  32 KVT-LANE  64 KVT-LANE ;

\ ---- generation-bearing handles ----------------------------------------------------
: KVT-STALE-REUSE ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H! KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 1 ALLOC-SEQ 1 KVT-H! KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE 1 KVT-H@ SEQ-LEN 1 T=
   0 KVT-H@ KV-KVSEQ:UNMAKE {: oc:kv-cache-id os:kv-seq-slot og:kv-seq-gen :}
   1 KVT-H@ KV-KVSEQ:UNMAKE {: nc:kv-cache-id ns:kv-seq-slot ng:kv-seq-gen :}
   oc KV-CACHE-ID>N nc KV-CACHE-ID>N = TTRUE
   os KV-SEQ-SLOT>N ns KV-SEQ-SLOT>N = TTRUE
   og KV-SEQ-GEN>N ng KV-SEQ-GEN>N <> TTRUE ;

: KVT-OLD-USE ( -- )
   KVT-CACHE 0 KVT-H@ SEQ-LEN drop ;

: KVT-CROSS-CACHE ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 1 KVT-H!
   KVT-OTHER DISPOSE
   2 4 2 8 1 128 CONFIG KVT-OTHER swap INIT
   KVT-OTHER 1 ALLOC-SEQ 8 KVT-H!
   [: KVT-OTHER 1 KVT-H@ APPEND-TOKEN ;] catch E-KV-SEQ T=
   KVT-OTHER RESERVED-PAGES 1 T=
   KVT-OTHER 8 KVT-H@ SEQ-RESERVED 1 T=
   KVT-OTHER 8 KVT-H@ SEQ-LEN 0 T=
   KVT-OTHER CHECK ;

: KVT-STALE-SPEND ( -- )
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;

: KVT-STALE-RESERVATION ( -- )
   2 4 2 2 1 KVT-INIT
   KVT-CACHE 32 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 32 ALLOC-SEQ 1 KVT-H!
   KVT-SNAPSHOT
   [: KVT-STALE-SPEND ;] catch E-KV-SEQ T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE RESERVED-PAGES 2 T=
   KVT-CACHE 1 KVT-H@ SEQ-RESERVED 2 T=
   KVT-CACHE 1 KVT-H@ SEQ-LEN 0 T=
   KVT-CACHE CHECK ;

: KVT-REBUILD-CACHE ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE DISPOSE
   2 4 2 8 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 1 KVT-H! ;

: KVT-OLD-CACHE-USE ( -- )
   KVT-CACHE 0 KVT-H@ SEQ-LEN drop ;

variable KVT-ID-SAVE

\ Narrow test-only ceiling injection. These helpers are the only test reach into
\ allocator identity state; production exposes no raw identity or generation setter.
: KVT-CACHE-ID-LIMIT! ( -- )
   KV-NEXT-CACHE-ID @ KVT-ID-SAVE !
   KV-ID-MAX KV-NEXT-CACHE-ID ! ;

: KVT-CACHE-ID-RESTORE ( -- )
   KVT-ID-SAVE @ KV-NEXT-CACHE-ID ! ;

: KVT-SLOT-GEN-LIMIT! ( -- )
   KV-ID-MAX KVT-CACHE 0 SEQGEN! ;

: KVT-SLOT-GEN-LIMIT-ALL! ( -- )
   KVT-CACHE NSEQ-OFF H@ 0 ?do KV-ID-MAX KVT-CACHE i SEQGEN! loop ;

: KVT-INIT-AT-ID-LIMIT ( -- )
   2 4 2 8 1 128 CONFIG KVT-CACHE swap INIT ;

: KVT-CACHE-ID-EXHAUST ( -- )
   KVT-CACHE DISPOSE
   KVT-CACHE-ID-LIMIT!
   [: KVT-INIT-AT-ID-LIMIT ;] catch {: code:n :}
   KVT-CACHE-ID-RESTORE
   code E-KV-ID T=
   KVT-CACHE LIVE-CACHE? TFALSE ;

: KVT-SLOT-GEN-EXHAUST-CALL ( -- )
   KVT-CACHE 1 ALLOC-SEQ 8 KVT-H! ;

: KVT-SLOT-GEN-EXHAUST ( -- )
   2 4 2 8 2 KVT-INIT
   KVT-SLOT-GEN-LIMIT-ALL!
   KVT-SNAPSHOT
   [: KVT-SLOT-GEN-EXHAUST-CALL ;] catch E-KV-ID T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE 0 SEQLIVE@ 0 T=
   KVT-CACHE 1 SEQLIVE@ 0 T=
   KVT-CACHE 0 SEQGEN@ KV-ID-MAX T=
   KVT-CACHE 1 SEQGEN@ KV-ID-MAX T=
   KVT-CACHE 0 SEQBLEN@ 0 T=
   KVT-CACHE 1 SEQBLEN@ 0 T=
   KVT-CACHE BLOCK-CAPACITY 0 ?do
      KVT-CACHE 0 i KV-BLK@ 0 T=
      KVT-CACHE 1 i KV-BLK@ 0 T=
   loop
   KVT-CACHE CHECK ;

: KVT-SLOT-GEN-SKIP-FIRST ( -- )
   2 4 2 8 2 KVT-INIT
   KVT-SLOT-GEN-LIMIT!
   KVT-CACHE 1 ALLOC-SEQ 8 KVT-H!
   8 KVT-H@ KV-KVSEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N 0 > TTRUE
   slot KV-SEQ-SLOT>N 1 T=
   gen KV-SEQ-GEN>N 1 T=
   KVT-CACHE CHECK ;

: KVT-SLOT-GEN-SKIP-LAST ( -- )
   2 4 2 8 2 KVT-INIT
   KV-ID-MAX KVT-CACHE 1 SEQGEN!
   KVT-CACHE 1 ALLOC-SEQ 8 KVT-H!
   8 KVT-H@ KV-KVSEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N 0 > TTRUE
   slot KV-SEQ-SLOT>N 0 T=
   gen KV-SEQ-GEN>N 1 T=
   KVT-CACHE CHECK ;

\ ---- boundary append, recycling, and failure atomicity -----------------------------
: KVT-BOUNDARY ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE PAGE-TOKENS 1+ ALLOC-SEQ 0 KVT-H!
   PAGE-TOKENS 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 2 T=
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK ;

: KVT-OWNED-POP ( -- )
   2 4 2 2 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   0 KVT-CACHE FREETOP-OFF H!                 \ inject impossible physical state
   KVT-SNAPSHOT
   [: KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;] catch E-KV-INVARIANT T=
   KVT-SNAPSHOT= TTRUE ;

: KVT-RECYCLE ( -- )
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE 1 ALLOC-SEQ 1 KVT-H!
   KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE CHECK ;

: KVT-SEQS-FULL ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-SNAPSHOT
   KVT-CACHE 1 ALLOC-SEQ 8 KVT-H! ;

: KVT-SEQS-FAIL-ATOMIC ( -- )
   [: KVT-SEQS-FULL ;] catch E-KV-SEQS T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE 0 SEQLIVE@ 1 T=
   KVT-CACHE 0 SEQGEN@ 1 T=
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE RESERVED-PAGES 1 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ SEQ-LEN 1 T=
   KVT-CACHE CHECK ;

\ ---- lifecycle and bounds reject by their owning error -----------------------------
: KVT-BAD-CONFIG ( -- )
   0 4 2 8 4 128 CONFIG drop ;

: KVT-DOUBLE-CANCEL ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ ;

: KVT-USE-AFTER-CANCEL ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 0 KVT-H@ SEQ-LEN drop ;

: KVT-DISPOSED-USE ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE DISPOSE
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H! ;

: KVT-REINIT ( -- )
   2 4 2 8 4 KVT-INIT
   2 4 2 8 4 128 CONFIG KVT-CACHE swap INIT ;

: KVT-BAD-TOKEN ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ 5 TOKEN-PTR drop ;

: KVT-BAD-SEQ-PAGE ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 1 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ 1 SEQ-PAGE drop ;

\ ---- sharing and physical tail-waste accounting ------------------------------------
: KVT-COW-BYTES ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 4 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN 11 KVT-CACHE 0 KVT-H@ 0 TOKEN-PTR c!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN 22 KVT-CACHE 0 KVT-H@ 1 TOKEN-PTR c!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN 33 KVT-CACHE 0 KVT-H@ 2 TOKEN-PTR c!
   KVT-CACHE 0 KVT-H@ 0 SEQ-PAGE {: tail:n :}
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   KVT-CACHE 1 KVT-H@ 0 SEQ-PAGE tail T=
   KVT-CACHE tail PAGE-REFC 2 T=
   KVT-CACHE 1 KVT-H@ 0 TOKEN-PTR c@ 11 T=
   KVT-CACHE 1 KVT-H@ 1 TOKEN-PTR c@ 22 T=
   KVT-CACHE 1 KVT-H@ 2 TOKEN-PTR c@ 33 T=
   KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE 1 KVT-H@ 0 SEQ-PAGE {: newp:n :}
   newp tail <> TTRUE
   KVT-CACHE tail PAGE-REFC 1 T=
   KVT-CACHE newp PAGE-REFC 1 T=
   99 KVT-CACHE 1 KVT-H@ 3 TOKEN-PTR c!
   55 KVT-CACHE 0 KVT-H@ 0 TOKEN-PTR c!
   KVT-CACHE 0 KVT-H@ 0 TOKEN-PTR c@ 55 T=
   KVT-CACHE 1 KVT-H@ 0 TOKEN-PTR c@ 11 T=
   KVT-CACHE 1 KVT-H@ 3 TOKEN-PTR c@ 99 T=
   KVT-CACHE 0 KVT-H@ SEQ-LEN 3 T=
   KVT-CACHE 1 KVT-H@ SEQ-LEN 4 T=
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK ;

: KVT-SHARED-WASTE ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE 5 ALLOC-SEQ 0 KVT-H!
   4 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   s" unique tail waste" T-LABEL KVT-CACHE TAIL-WASTE 12 T=
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   s" shared physical tail waste" T-LABEL KVT-CACHE TAIL-WASTE 12 T=
   s" shared page metric" T-LABEL KVT-CACHE SHARED-PAGES 1 T=
   KVT-CACHE 1 KVT-H@ APPEND-TOKEN          \ copy-on-write makes two physical tails
   s" split physical tail waste" T-LABEL KVT-CACHE TAIL-WASTE 23 T=
   s" split shared page metric" T-LABEL KVT-CACHE SHARED-PAGES 0 T=
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK ;

\ ---- peak physical occupancy -------------------------------------------------------
\ One token per page makes the peak exactly the number of live tokens. The peak is the
\ highest occupancy the cache ever reached, so it has to survive a release back to
\ empty and must not follow a lower second peak back down.
5 constant KVT-HW-PEAK
3 constant KVT-HW-LATER

: KVT-HW-GROW ( n n -- ) {: slot:n toks:n :}
   KVT-CACHE toks ALLOC-SEQ slot KVT-H!
   toks 0 ?do KVT-CACHE slot KVT-H@ APPEND-TOKEN loop ;

: KVT-HIGH-WATER ( -- )
   2 4 2 8 4 1 KVT-INIT/P
   s" fresh cache has no peak" T-LABEL KVT-CACHE HIGH-WATER 0 T=
   0 KVT-HW-PEAK KVT-HW-GROW
   s" first peak occupancy" T-LABEL KVT-CACHE WATERMARK KVT-HW-PEAK T=
   s" first peak recorded" T-LABEL KVT-CACHE HIGH-WATER KVT-HW-PEAK T=
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   s" released back to empty" T-LABEL KVT-CACHE WATERMARK 0 T=
   s" peak survives the release" T-LABEL KVT-CACHE HIGH-WATER KVT-HW-PEAK T=
   1 KVT-HW-LATER KVT-HW-GROW
   s" lower second peak" T-LABEL KVT-CACHE WATERMARK KVT-HW-LATER T=
   s" peak is the maximum, not the latest" T-LABEL KVT-CACHE HIGH-WATER KVT-HW-PEAK T=
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   s" peak survives the last cancel" T-LABEL KVT-CACHE HIGH-WATER KVT-HW-PEAK T=
   KVT-CACHE CHECK ;

\ ---- real max-context reservations --------------------------------------------------
: KVT-RESERVATION-CONSUME ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 17 ALLOC-SEQ 0 KVT-H!             \ two page guarantee
   KVT-CACHE RESERVED-PAGES 2 T=
   KVT-CACHE 0 KVT-H@ SEQ-RESERVED 2 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE RESERVED-PAGES 1 T=
   KVT-CACHE 0 KVT-H@ SEQ-RESERVED 1 T=
   15 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE RESERVED-PAGES 1 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE 0 KVT-H@ SEQ-RESERVED 0 T=
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK ;

\ Every token of the declared maximum is appended, and the remaining reservation is
\ checked at the six indices where the page arithmetic changes: reservation only moves
\ when an append crosses a page boundary, so those are the appends that can get it
\ wrong.
: KVT-BOUNDARY-STEP ( n -- ) {: tok:n :}
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   tok KVT-BOUND? 0= if exit then
   KVT-CACHE RESERVED-PAGES
   KVT-CACHE 65 PAGES-FOR KVT-CACHE tok 1+ PAGES-FOR - T=
   KVT-CACHE CHECK ;

: KVT-BOUNDARY-RESERVATION ( -- )
   2 4 2 5 2 65 16 KVT-INIT/MAX/P
   16 64 KVT-BOUND-PLAN
   KVT-CACHE 65 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE RESERVED-PAGES 5 T=
   65 0 ?do i KVT-BOUNDARY-STEP loop
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 5 T=
   KVT-CACHE 0 KVT-H@ SEQ-LEN 65 T= ;

: KVT-EXACT-FIT ( -- )
   2 4 2 4 2 KVT-INIT
   KVT-CACHE 64 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE RESERVED-PAGES 4 T=
   KVT-CACHE 0 KVT-H@ SEQ-RESERVED 4 T=
   KVT-CACHE CHECK ;

: KVT-ONE-OVER-CALL ( -- )
   KVT-CACHE 1 ALLOC-SEQ 8 KVT-H! ;

: KVT-ONE-OVER-ATOMIC ( -- )
   2 4 2 4 2 KVT-INIT
   KVT-CACHE 64 ALLOC-SEQ 0 KVT-H!
   KVT-SNAPSHOT
   [: KVT-ONE-OVER-CALL ;] catch E-KV-ADMIT T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE RESERVED-PAGES 4 T=
   KVT-CACHE 1 SEQLIVE@ 0 T=
   KVT-CACHE 1 SEQGEN@ 0 T=
   KVT-CACHE CHECK ;

: KVT-OVER-MAX-CALL ( -- )
   KVT-CACHE 65 ALLOC-SEQ 8 KVT-H! ;

: KVT-OVER-MAX-ATOMIC ( -- )
   2 4 2 8 2 64 16 KVT-INIT/MAX/P
   KVT-SNAPSHOT
   [: KVT-OVER-MAX-CALL ;] catch E-KV-ADMIT T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE 0 SEQLIVE@ 0 T=
   KVT-CACHE 0 SEQGEN@ 0 T=
   KVT-CACHE CHECK ;

: KVT-SEQ-CEILING ( -- )
   2 4 2 8 1 64 16 KVT-INIT/MAX/P
   KVT-CACHE 17 ALLOC-SEQ 0 KVT-H!
   17 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-SNAPSHOT
   [: KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;] catch E-KV-ADMIT T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE 0 KVT-H@ SEQ-LEN 17 T=
   KVT-CACHE 0 KVT-H@ SEQ-RESERVED 0 T=
   KVT-CACHE CHECK ;

: KVT-ZERO-MAX ( -- )
   2 4 2 4 2 KVT-INIT
   KVT-SNAPSHOT
   [: KVT-CACHE 0 ALLOC-SEQ 8 KVT-H! ;] catch E-KV-ADMIT T=
   KVT-SNAPSHOT= TTRUE
   KVT-CACHE CHECK ;

: KVT-MULTI-RESERVATION ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 48 ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 16 ALLOC-SEQ 1 KVT-H!
   KVT-CACHE RESERVED-PAGES 4 T=                    \ exact fit across admissions
   [: KVT-CACHE 1 ALLOC-SEQ 8 KVT-H! ;] catch E-KV-ADMIT T=
   KVT-CACHE RESERVED-PAGES 4 T=                    \ no oversubscription
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 1 T=                    \ unused reservation released
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE CHECK ;

: KVT-CANCEL-RESERVATION ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 64 ALLOC-SEQ 0 KVT-H!
   s" cancel initial sequence reservation" T-LABEL KVT-CACHE 0 KVT-H@ SEQ-RESERVED 4 T=
   s" cancel initial global reservation" T-LABEL KVT-CACHE RESERVED-PAGES 4 T=
   s" cancel initial free" T-LABEL KVT-CACHE FREE-PAGES 4 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE RESERVED-PAGES 3 T=
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE CHECK ;

: KVT-FORK-RESERVE-FAIL ( -- )
   2 4 2 2 4 KVT-INIT
   KVT-CACHE 32 ALLOC-SEQ 0 KVT-H!
   4 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop    \ free=1, reserved=1
   KVT-CACHE 0 KVT-H@ FORK-SEQ 8 KVT-H! ;           \ needs child future page + COW

: KVT-FORK-FAIL-ATOMIC ( -- )
   [: KVT-FORK-RESERVE-FAIL ;] catch E-KV-ADMIT T=
   KVT-CACHE 0 KVT-H@ 0 SEQ-PAGE {: page:n :}
   KVT-CACHE page PAGE-REFC 1 T=
   KVT-CACHE 0 KVT-H@ SEQ-LEN 4 T=
   KVT-CACHE RESERVED-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

: KVT-FORK-RESERVE-SUCCESS ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 32 ALLOC-SEQ 0 KVT-H!
   4 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop    \ remaining parent page=1
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   KVT-CACHE RESERVED-PAGES 3 T=                    \ parent page + child page + one COW
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE RESERVED-PAGES 2 T=
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 1 T=
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE CHECK ;

: KVT-FORK-MULTI-SHARE ( -- )
   2 4 2 6 4 KVT-INIT
   KVT-CACHE 32 ALLOC-SEQ 0 KVT-H!
   4 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ 0 SEQ-PAGE {: shared:n :}
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H!
   KVT-CACHE 0 KVT-H@ FORK-SEQ 2 KVT-H!
   KVT-CACHE RESERVED-PAGES 5 T=                 \ three future pages + two physical COWs
   KVT-CACHE shared PAGE-REFC 3 T=
   KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE RESERVED-PAGES 4 T=
   KVT-CACHE shared PAGE-REFC 2 T=
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 3 T=
   KVT-CACHE 2 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 1 T=                 \ parent future page only
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE CHECK ;

\ ---- copy-on-write reservation, counted from the sharing this test built -----------
\ The allocator checks its own copy-on-write reservation against KV-COW-DESIRED, and
\ the rebalance that writes that reservation is computed from KV-COW-DESIRED too, so
\ the allocator cannot notice a mistake inside it. This test states the expected
\ reservation from what the test itself constructed instead.
\
\ Every sequence here declares 32 tokens, which is two 16-token pages, and holds one
\ partially filled page, so every one of them still owns exactly one future page. They
\ all share that single page. Any sharer may later be forced to copy the page
\ privately, except the last one left, which keeps the original: so the allocator must
\ hold one copy page for every sharer beyond the first.
32 constant KVT-SHARE-MAX
4 constant KVT-SHARE-FILL
5 constant KVT-SHARE-SEQS

: KVT-SHARE-COW ( n -- n ) {: seqs:n :}  seqs 1- 0 max ;
: KVT-SHARE-TOTAL ( n -- n ) {: seqs:n :}  seqs seqs KVT-SHARE-COW KV-ADD0 ;

: KVT-COW-HELD ( -- n )
   0 KVT-CACHE NUM-PAGES 0 ?do KVT-CACHE i COWRES@ KV-ADD0 loop ;

: KVT-SHARE-STAGE ( n n -- ) {: page:n seqs:n :}
   s" sharers of the partial page" T-LABEL KVT-CACHE page PAGE-REFC seqs T=
   s" copy pages held for that page" T-LABEL KVT-CACHE page COWRES@ seqs KVT-SHARE-COW T=
   s" no other page holds copy pages" T-LABEL KVT-COW-HELD seqs KVT-SHARE-COW T=
   s" future pages plus copy pages" T-LABEL KVT-CACHE RESERVED-PAGES seqs KVT-SHARE-TOTAL T= ;

: KVT-SHARE-FORK ( n -- ) {: slot:n :}
   KVT-CACHE 0 KVT-H@ FORK-SEQ slot KVT-H! ;

: KVT-COW-SHARE ( -- )
   2 4 2 10 KVT-SHARE-SEQS KVT-INIT
   KVT-CACHE KVT-SHARE-MAX ALLOC-SEQ 0 KVT-H!
   KVT-SHARE-FILL 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ 0 SEQ-PAGE {: page:n :}
   page 1 KVT-SHARE-STAGE                        \ nothing shares it yet
   KVT-SHARE-SEQS 1 ?do
      i KVT-SHARE-FORK
      page i 1+ KVT-SHARE-STAGE                  \ one fork, then two, then more
   loop
   KVT-SHARE-SEQS 1 ?do
      KVT-SHARE-SEQS i - {: dead:n :}
      KVT-CACHE dead KVT-H@ CANCEL-SEQ
      page dead KVT-SHARE-STAGE                  \ each cancel releases one copy page
   loop
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE CHECK ;

\ ---- churn: every caught capacity failure leaves ownership accounting exact --------
8 constant KVT-ACT-CAP
variable KVT-NACT
variable KVT-FAILURES
variable KVT-PICK

: KVT-ACT@ ( n -- kvseq )  KVT-H@ ;
: KVT-ACT! ( kvseq n -- )  KVT-H! ;
: KVT-ACT-PUSH ( kvseq -- )  KVT-NACT @ KVT-ACT! 1 KVT-NACT +! ;
: KVT-ACT-DROP ( n -- ) {: i:n :}
   KVT-NACT @ 1- {: last:n :}
   last KVT-ACT@ i KVT-ACT!
   -1 KVT-NACT +! ;

: KVT-SEQ-HAS? ( kvseq n -- bool ) {: pid:n :}
   8 KVT-H!
   false
   KVT-CACHE 8 KVT-H@ SEQ-PAGES 0 ?do
      KVT-CACHE 8 KVT-H@ i SEQ-PAGE pid = if drop true then
   loop ;

: KVT-CONTAINS? ( n -- bool ) {: pid:n :}
   false
   KVT-NACT @ 0 ?do
      i KVT-ACT@ pid KVT-SEQ-HAS? if drop true then
   loop ;

: KVT-DISTINCT ( -- n )
   0
   KVT-CACHE NUM-PAGES 0 ?do i KVT-CONTAINS? if 1+ then loop ;

: KVT-TRY-APPEND-BODY ( -- )
   KVT-CACHE 8 KVT-ACT@ APPEND-TOKEN ;

: KVT-DO-APPEND ( -- )
   KVT-NACT @ 0= if exit then
   KVT-NACT @ PROP:RND% KVT-ACT@ 8 KVT-H!
   [: KVT-TRY-APPEND-BODY ;] catch {: code:n :}
   code 0= if exit then
   code E-KV-ADMIT = if 1 KVT-FAILURES +! exit then
   code throw ;

: KVT-TRY-ALLOC-BODY ( -- )
   KVT-CACHE PAGE-TOKENS 2 * PROP:RND% 1+ ALLOC-SEQ 8 KVT-H! ;

: KVT-DO-ALLOC ( -- )
   KVT-NACT @ KVT-ACT-CAP >= if exit then
   [: KVT-TRY-ALLOC-BODY ;] catch {: code:n :}
   code 0= if 8 KVT-H@ KVT-ACT-PUSH exit then
   code E-KV-ADMIT = if 1 KVT-FAILURES +! exit then
   code throw ;

: KVT-TRY-FORK-BODY ( -- )
   KVT-CACHE KVT-PICK @ KVT-ACT@ FORK-SEQ 8 KVT-H! ;

: KVT-DO-FORK ( -- )
   KVT-NACT @ 0= KVT-NACT @ KVT-ACT-CAP >= or if exit then
   KVT-NACT @ PROP:RND% KVT-PICK !
   [: KVT-TRY-FORK-BODY ;] catch {: code:n :}
   code 0= if 8 KVT-H@ KVT-ACT-PUSH exit then
   code E-KV-ADMIT = if 1 KVT-FAILURES +! exit then
   code throw ;

: KVT-DO-CANCEL ( -- )
   KVT-NACT @ 0= if exit then
   KVT-NACT @ PROP:RND% {: i:n :}
   KVT-CACHE i KVT-ACT@ CANCEL-SEQ
   i KVT-ACT-DROP ;

: KVT-CHURN-ROUND ( -- )
   5 PROP:RND% case
      0 of KVT-DO-ALLOC endof
      1 of KVT-DO-APPEND endof
      2 of KVT-DO-APPEND endof
      3 of KVT-DO-FORK endof
      4 of KVT-DO-CANCEL endof
   endcase
   KVT-CACHE CHECK
   KVT-CACHE WATERMARK KVT-DISTINCT T= ;

: KVT-EXHAUST-ADMISSION ( -- )
   KVT-CACHE 1 ALLOC-SEQ 6 KVT-H! ;

: KVT-INJECT-EXHAUSTION ( -- )
   KVT-CACHE PAGE-TOKENS 7 * ALLOC-SEQ 8 KVT-H!
   KVT-CACHE PAGE-TOKENS ALLOC-SEQ 7 KVT-H!
   KVT-SNAPSHOT
   [: KVT-EXHAUST-ADMISSION ;] catch {: code:n :}
   code E-KV-ADMIT <> if code throw then
   KVT-SNAPSHOT= TTRUE
   1 KVT-FAILURES +!
   KVT-CACHE CHECK
   KVT-CACHE 7 KVT-H@ CANCEL-SEQ
   KVT-CACHE 8 KVT-H@ CANCEL-SEQ ;

: KVT-CHURN ( -- )
   2 4 2 8 KVT-ACT-CAP KVT-INIT
   0 KVT-NACT ! 0 KVT-FAILURES !
   KVT-INJECT-EXHAUSTION
   11 300 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do KVT-CHURN-ROUND loop
   KVT-FAILURES @ 0 > TTRUE
   begin KVT-NACT @ 0 > while 0 KVT-ACT@ KVT-CACHE swap CANCEL-SEQ 0 KVT-ACT-DROP repeat
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE CHECK ;

\ ---- run ----------------------------------------------------------------------------
T-RESET

s" geometry" T-LABEL ' KVT-GEOMETRY 0 TTHROWS
s" generated structure operations" T-LABEL ' KVT-GENERATED-OPS 0 TTHROWS
' KVT-OVERFLOW-TOK E-KV-CONFIG TTHROWS
' KVT-OVERFLOW-META E-KV-CONFIG TTHROWS
' KVT-OVERFLOW-META-BYTES E-KV-CONFIG TTHROWS
' KVT-OVERFLOW-TABLE-META E-KV-CONFIG TTHROWS
' KVT-BAD-BLOCK-CAPACITY E-KV-CONFIG TTHROWS
' KVT-BAD-PAGE E-KV-CONFIG TTHROWS
s" overflow-free ceiling" T-LABEL ' KVT-CEIL-SAFE 0 TTHROWS
s" fixed block tables" T-LABEL ' KVT-FIXED-TABLES 0 TTHROWS
s" failed install cleanup" T-LABEL ' KVT-INIT-FAIL-CLEANUP 0 TTHROWS
s" page-token parameterization" T-LABEL ' KVT-PAGE-TOKENS 0 TTHROWS

s" stale slot reuse" T-LABEL ' KVT-STALE-REUSE 0 TTHROWS
' KVT-OLD-USE E-KV-SEQ TTHROWS
s" stale reservation ownership" T-LABEL ' KVT-STALE-RESERVATION 0 TTHROWS
s" cross-cache reservation ownership" T-LABEL ' KVT-CROSS-CACHE 0 TTHROWS
KVT-OTHER DISPOSE
s" cache rebuild setup" T-LABEL ' KVT-REBUILD-CACHE 0 TTHROWS
' KVT-OLD-CACHE-USE E-KV-SEQ TTHROWS
s" cache identity exhaustion" T-LABEL ' KVT-CACHE-ID-EXHAUST 0 TTHROWS
s" slot generation exhaustion" T-LABEL ' KVT-SLOT-GEN-EXHAUST 0 TTHROWS
s" skip first exhausted slot" T-LABEL ' KVT-SLOT-GEN-SKIP-FIRST 0 TTHROWS
s" skip last exhausted slot" T-LABEL ' KVT-SLOT-GEN-SKIP-LAST 0 TTHROWS

s" page boundary" T-LABEL ' KVT-BOUNDARY 0 TTHROWS
s" page recycling" T-LABEL ' KVT-RECYCLE 0 TTHROWS
s" owned physical exhaustion" T-LABEL ' KVT-OWNED-POP 0 TTHROWS
s" sequence failure atomic" T-LABEL ' KVT-SEQS-FAIL-ATOMIC 0 TTHROWS
s" invalid config" T-LABEL ' KVT-BAD-CONFIG E-KV-CONFIG TTHROWS
s" double cancel" T-LABEL ' KVT-DOUBLE-CANCEL E-KV-SEQ TTHROWS
s" use after cancel" T-LABEL ' KVT-USE-AFTER-CANCEL E-KV-SEQ TTHROWS
s" disposed use" T-LABEL ' KVT-DISPOSED-USE E-KV-STATE TTHROWS
s" live reinitialization" T-LABEL ' KVT-REINIT E-KV-STATE TTHROWS
s" token bounds" T-LABEL ' KVT-BAD-TOKEN E-KV-BOUNDS TTHROWS
s" sequence page bounds" T-LABEL ' KVT-BAD-SEQ-PAGE E-KV-BOUNDS TTHROWS
s" copy-on-write bytes" T-LABEL ' KVT-COW-BYTES 0 TTHROWS
s" physical tail waste" T-LABEL ' KVT-SHARED-WASTE 0 TTHROWS
s" peak physical occupancy" T-LABEL ' KVT-HIGH-WATER 0 TTHROWS

s" reservation consumption" T-LABEL ' KVT-RESERVATION-CONSUME 0 TTHROWS
s" reservation at page boundaries" T-LABEL ' KVT-BOUNDARY-RESERVATION 0 TTHROWS
s" exact-fit admission" T-LABEL ' KVT-EXACT-FIT 0 TTHROWS
s" capacity oversubscription atomicity" T-LABEL ' KVT-ONE-OVER-ATOMIC 0 TTHROWS
s" one-page-over maximum atomicity" T-LABEL ' KVT-OVER-MAX-ATOMIC 0 TTHROWS
s" per-sequence append ceiling" T-LABEL ' KVT-SEQ-CEILING 0 TTHROWS
s" positive declared maximum" T-LABEL ' KVT-ZERO-MAX 0 TTHROWS
s" multiple reservations" T-LABEL ' KVT-MULTI-RESERVATION 0 TTHROWS
s" cancel reservation" T-LABEL ' KVT-CANCEL-RESERVATION 0 TTHROWS
s" fork reservation rollback" T-LABEL ' KVT-FORK-FAIL-ATOMIC 0 TTHROWS
s" fork reservation guarantee" T-LABEL ' KVT-FORK-RESERVE-SUCCESS 0 TTHROWS
s" multi-fork reservation" T-LABEL ' KVT-FORK-MULTI-SHARE 0 TTHROWS
s" copy-on-write sharing reservation" T-LABEL ' KVT-COW-SHARE 0 TTHROWS

s" failure churn" T-LABEL ' KVT-CHURN 0 TTHROWS

KVT-CACHE DISPOSE
KVT-OTHER DISPOSE
T-REPORT

;package
