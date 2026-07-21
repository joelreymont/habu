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

: KVT-INIT/P ( n n n n n n -- )             \ nkv hdim dbytes npages nseq ptok
   KVT-CACHE DISPOSE
   CONFIG/P KVT-CACHE swap INIT ;

: KVT-INIT ( n n n n n -- )                 \ default page-token count
   KVT-CACHE DISPOSE
   CONFIG KVT-CACHE swap INIT ;

\ ---- structured sequence-handle storage for caught calls and churn ----------------
create KVT-H-CACHE 9 cells allot
create KVT-H-SLOT  9 cells allot
create KVT-H-GEN   9 cells allot

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

\ ---- geometry and checked arithmetic ----------------------------------------------
: KVT-GEOMETRY ( -- )
   2 4 2 8 4 16 KVT-INIT/P
   KVT-CACHE PAGE-SIZE 16 T=
   KVT-CACHE TOK-BYTES 32 T=
   KVT-CACHE PAGE-BYTES 512 T=
   KVT-CACHE NUM-PAGES 8 T=
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE HIGH-WATER 0 T=
   KVT-CACHE BYTES-PER-TOKEN 32 T=
   KVT-CACHE CHECK ;

: KVT-OVERFLOW-TOK ( -- )
   KV-MAX-N 2 2 8 4 16 CONFIG/P drop ;

: KVT-OVERFLOW-META ( -- )
   1 1 1 1 KV-MAX-N 16 CONFIG/P drop ;

: KVT-OVERFLOW-META-BYTES ( -- )
   1 1 1 1 KV-MAX-N 8 / 16 CONFIG/P drop ;

: KVT-BAD-PAGE ( -- )
   1 1 1 8 4 0 CONFIG/P drop ;

: KVT-CEIL-SAFE ( -- )
   1 1 1 8 4 64 KVT-INIT/P
   KVT-CACHE 0 PAGES-FOR 0 T=
   KVT-CACHE 64 PAGES-FOR 1 T=
   KVT-CACHE 65 PAGES-FOR 2 T=
   KVT-CACHE KV-MAX-N PAGES-FOR
   KV-MAX-N 64 / KV-MAX-N 64 mod 0<> if 1+ then T= ;

: KVT-HUGE-POOL-INIT ( -- )
   KV-MAX-N 2 / 1 1 1 1 1 CONFIG/P KVT-CACHE swap INIT ;

: KVT-INIT-FAIL-CLEANUP ( -- )
   KVT-CACHE DISPOSE
   [: KVT-HUGE-POOL-INIT ;] catch E-MEM-MAP T=
   KVT-CACHE LIVE-CACHE? TFALSE
   2 4 2 8 1 KVT-INIT
   KVT-CACHE LIVE-CACHE? TTRUE
   KVT-CACHE FREE-PAGES 8 T=
   KVT-CACHE CHECK ;

\ ---- page-token parameterization: exact addressing at 8/16/32/64 ------------------
129 constant KVT-LANE-TOKENS

: KVT-LANE-FILL ( kvseq -- )
   8 KVT-H!
   KVT-LANE-TOKENS 0 ?do
      KVT-CACHE 8 KVT-H@ APPEND-TOKEN
      i 17 * 3 + 255 and KVT-CACHE 8 KVT-H@ i TOKEN-PTR c!
   loop ;

: KVT-LANE-CHECK ( kvseq n -- ) {: ptok:n :}
   8 KVT-H!
   KVT-CACHE PAGE-SIZE ptok T=
   KVT-CACHE 8 KVT-H@ SEQ-PAGES KVT-LANE-TOKENS ptok / KVT-LANE-TOKENS ptok mod 0<> if 1+ then T=
   KVT-LANE-TOKENS 0 ?do
      KVT-CACHE 8 KVT-H@ i TOKEN-PTR c@ i 17 * 3 + 255 and T=
   loop ;

: KVT-LANE ( n -- ) {: ptok:n :}
   2 4 2 64 4 ptok KVT-INIT/P
   KVT-CACHE ALLOC-SEQ 0 KVT-H! 0 KVT-H@ KVT-LANE-FILL
   0 KVT-H@ ptok KVT-LANE-CHECK
   KVT-CACHE 0 KVT-H@ FORK-SEQ 1 KVT-H! 1 KVT-H@ ptok KVT-LANE-CHECK
   KVT-CACHE CHECK ;

: KVT-PAGE-TOKENS ( -- )
   8 KVT-LANE  16 KVT-LANE  32 KVT-LANE  64 KVT-LANE ;

\ ---- generation-bearing handles ----------------------------------------------------
: KVT-STALE-REUSE ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H! KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE ALLOC-SEQ 1 KVT-H! KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE 1 KVT-H@ SEQ-LEN 1 T=
   0 KVT-H@ KV-KVSEQ:UNMAKE {: oc:kv-cache-id os:kv-seq-slot og:kv-seq-gen :}
   1 KVT-H@ KV-KVSEQ:UNMAKE {: nc:kv-cache-id ns:kv-seq-slot ng:kv-seq-gen :}
   oc KV-CACHE-ID>N nc KV-CACHE-ID>N = TTRUE
   os KV-SEQ-SLOT>N ns KV-SEQ-SLOT>N = TTRUE
   og KV-SEQ-GEN>N ng KV-SEQ-GEN>N <> TTRUE ;

: KVT-OLD-USE ( -- )
   KVT-CACHE 0 KVT-H@ SEQ-LEN drop ;

: KVT-CROSS-CACHE ( -- )
   KVT-OTHER DISPOSE
   2 4 2 8 1 CONFIG KVT-OTHER swap INIT
   KVT-OTHER 1 KVT-H@ SEQ-LEN drop ;

: KVT-REBUILD-CACHE ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE DISPOSE
   2 4 2 8 1 KVT-INIT
   KVT-CACHE ALLOC-SEQ 1 KVT-H! ;

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

: KVT-INIT-AT-ID-LIMIT ( -- )
   2 4 2 8 1 CONFIG KVT-CACHE swap INIT ;

: KVT-CACHE-ID-EXHAUST ( -- )
   KVT-CACHE DISPOSE
   KVT-CACHE-ID-LIMIT!
   [: KVT-INIT-AT-ID-LIMIT ;] catch {: code:n :}
   KVT-CACHE-ID-RESTORE
   code E-KV-ID T=
   KVT-CACHE LIVE-CACHE? TFALSE ;

: KVT-SLOT-GEN-EXHAUST-CALL ( -- )
   KVT-CACHE ALLOC-SEQ 8 KVT-H! ;

: KVT-SLOT-GEN-EXHAUST ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-SLOT-GEN-LIMIT!
   [: KVT-SLOT-GEN-EXHAUST-CALL ;] catch E-KV-ID T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE CHECK ;

\ ---- boundary append, recycling, and failure atomicity -----------------------------
: KVT-BOUNDARY ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   PAGE-TOKENS 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 2 T=
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK ;

: KVT-POOL-FAIL ( -- )
   2 4 2 1 2 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   PAGE-TOKENS 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;

: KVT-APPEND-FAIL-ATOMIC ( -- )
   [: KVT-POOL-FAIL ;] catch E-KV-POOL T=
   KVT-CACHE 0 KVT-H@ SEQ-LEN PAGE-TOKENS T=
   KVT-CACHE 0 KVT-H@ SEQ-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE CHECK ;

: KVT-RECYCLE ( -- )
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE ALLOC-SEQ 1 KVT-H!
   KVT-CACHE 1 KVT-H@ APPEND-TOKEN
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE CHECK ;

: KVT-SEQS-FULL ( -- )
   2 4 2 8 1 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE ALLOC-SEQ 8 KVT-H! ;

: KVT-SEQS-FAIL-ATOMIC ( -- )
   [: KVT-SEQS-FULL ;] catch E-KV-SEQS T=
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ SEQ-LEN 1 T=
   KVT-CACHE CHECK ;

\ ---- lifecycle and bounds reject by their owning error -----------------------------
: KVT-BAD-CONFIG ( -- )
   0 4 2 8 4 CONFIG drop ;

: KVT-DOUBLE-CANCEL ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ ;

: KVT-USE-AFTER-CANCEL ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE 0 KVT-H@ SEQ-LEN drop ;

: KVT-DISPOSED-USE ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE DISPOSE
   KVT-CACHE ALLOC-SEQ 0 KVT-H! ;

: KVT-REINIT ( -- )
   2 4 2 8 4 KVT-INIT
   2 4 2 8 4 CONFIG KVT-CACHE swap INIT ;

: KVT-BAD-TOKEN ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ 5 TOKEN-PTR drop ;

: KVT-BAD-SEQ-PAGE ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN
   KVT-CACHE 0 KVT-H@ 1 SEQ-PAGE drop ;

\ ---- sharing and physical tail-waste accounting ------------------------------------
: KVT-COW-BYTES ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
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
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
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

\ ---- real max-context reservations --------------------------------------------------
: KVT-RESERVATION-CONSUME ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 17 ALLOC-SEQ/MAX 0 KVT-H!             \ two page guarantee
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

: KVT-RESERVE-ONE-OVER ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 65 ALLOC-SEQ/MAX 8 KVT-H! ;

: KVT-RESERVE-FAIL-ATOMIC ( -- )
   [: KVT-RESERVE-ONE-OVER ;] catch E-KV-ADMIT T=
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE ALLOC-SEQ 0 KVT-H!                      \ rejected admission consumed no slot
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE CHECK ;

: KVT-MULTI-RESERVATION ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 48 ALLOC-SEQ/MAX 0 KVT-H!
   KVT-CACHE 16 ALLOC-SEQ/MAX 1 KVT-H!
   KVT-CACHE RESERVED-PAGES 4 T=                    \ exact fit across admissions
   [: KVT-CACHE 1 ALLOC-SEQ/MAX 8 KVT-H! ;] catch E-KV-ADMIT T=
   KVT-CACHE RESERVED-PAGES 4 T=                    \ no oversubscription
   KVT-CACHE 0 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 1 T=                    \ unused reservation released
   KVT-CACHE 1 KVT-H@ CANCEL-SEQ
   KVT-CACHE RESERVED-PAGES 0 T=
   KVT-CACHE CHECK ;

: KVT-INCREMENTAL-STEAL ( -- )
   2 4 2 3 4 KVT-INIT
   KVT-CACHE 32 ALLOC-SEQ/MAX 2 KVT-H!              \ protects two pages
   KVT-CACHE ALLOC-SEQ 0 KVT-H!
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN                  \ consumes the only unreserved page
   PAGE-TOKENS 1- 0 ?do KVT-CACHE 0 KVT-H@ APPEND-TOKEN loop
   KVT-CACHE 0 KVT-H@ APPEND-TOKEN ;                \ cannot steal a reserved page

: KVT-INCREMENTAL-FAIL-ATOMIC ( -- )
   [: KVT-INCREMENTAL-STEAL ;] catch E-KV-ADMIT T=
   KVT-CACHE 0 KVT-H@ SEQ-LEN PAGE-TOKENS T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE RESERVED-PAGES 2 T=
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

: KVT-CANCEL-RESERVATION ( -- )
   2 4 2 4 4 KVT-INIT
   KVT-CACHE 64 ALLOC-SEQ/MAX 0 KVT-H!
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
   KVT-CACHE 32 ALLOC-SEQ/MAX 0 KVT-H!
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
   KVT-CACHE 32 ALLOC-SEQ/MAX 0 KVT-H!
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
   KVT-CACHE 32 ALLOC-SEQ/MAX 0 KVT-H!
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

\ ---- churn: every caught capacity failure leaves ownership accounting exact --------
8 constant KVT-ACT-CAP
variable KVT-NACT
variable KVT-FAILURES

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
   code E-KV-POOL = code E-KV-ADMIT = or if 1 KVT-FAILURES +! exit then
   code throw ;

: KVT-DO-ALLOC ( -- )
   KVT-NACT @ KVT-ACT-CAP < if KVT-CACHE ALLOC-SEQ KVT-ACT-PUSH then ;

: KVT-DO-FORK ( -- )
   KVT-NACT @ 0 > KVT-NACT @ KVT-ACT-CAP < and if
      KVT-NACT @ PROP:RND% KVT-ACT@ KVT-CACHE swap FORK-SEQ KVT-ACT-PUSH
   then ;

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

: KVT-INJECT-EXHAUSTION ( -- )
   KVT-CACHE ALLOC-SEQ 8 KVT-H!
   KVT-CACHE NUM-PAGES PAGE-TOKENS * 0 ?do
      KVT-CACHE 8 KVT-H@ APPEND-TOKEN
   loop
   [: KVT-CACHE 8 KVT-H@ APPEND-TOKEN ;] catch {: code:n :}
   code E-KV-POOL <> if code throw then
   1 KVT-FAILURES +!
   KVT-CACHE CHECK
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
' KVT-OVERFLOW-TOK E-KV-CONFIG TTHROWS
' KVT-OVERFLOW-META E-KV-CONFIG TTHROWS
' KVT-OVERFLOW-META-BYTES E-KV-CONFIG TTHROWS
' KVT-BAD-PAGE E-KV-CONFIG TTHROWS
s" overflow-free ceiling" T-LABEL ' KVT-CEIL-SAFE 0 TTHROWS
s" failed install cleanup" T-LABEL ' KVT-INIT-FAIL-CLEANUP 0 TTHROWS
s" page-token parameterization" T-LABEL ' KVT-PAGE-TOKENS 0 TTHROWS

s" stale slot reuse" T-LABEL ' KVT-STALE-REUSE 0 TTHROWS
' KVT-OLD-USE E-KV-SEQ TTHROWS
' KVT-CROSS-CACHE E-KV-SEQ TTHROWS
KVT-OTHER DISPOSE
s" cache rebuild setup" T-LABEL ' KVT-REBUILD-CACHE 0 TTHROWS
' KVT-OLD-CACHE-USE E-KV-SEQ TTHROWS
s" cache identity exhaustion" T-LABEL ' KVT-CACHE-ID-EXHAUST 0 TTHROWS
s" slot generation exhaustion" T-LABEL ' KVT-SLOT-GEN-EXHAUST 0 TTHROWS

s" page boundary" T-LABEL ' KVT-BOUNDARY 0 TTHROWS
s" append failure atomic" T-LABEL ' KVT-APPEND-FAIL-ATOMIC 0 TTHROWS
s" page recycling" T-LABEL ' KVT-RECYCLE 0 TTHROWS
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

s" reservation consumption" T-LABEL ' KVT-RESERVATION-CONSUME 0 TTHROWS
s" reservation failure atomic" T-LABEL ' KVT-RESERVE-FAIL-ATOMIC 0 TTHROWS
s" multiple reservations" T-LABEL ' KVT-MULTI-RESERVATION 0 TTHROWS
s" protected reservation" T-LABEL ' KVT-INCREMENTAL-FAIL-ATOMIC 0 TTHROWS
s" cancel reservation" T-LABEL ' KVT-CANCEL-RESERVATION 0 TTHROWS
s" fork reservation rollback" T-LABEL ' KVT-FORK-FAIL-ATOMIC 0 TTHROWS
s" fork reservation guarantee" T-LABEL ' KVT-FORK-RESERVE-SUCCESS 0 TTHROWS
s" multi-fork reservation" T-LABEL ' KVT-FORK-MULTI-SHARE 0 TTHROWS

s" failure churn" T-LABEL ' KVT-CHURN 0 TTHROWS

KVT-CACHE DISPOSE
KVT-OTHER DISPOSE
T-REPORT

;package
