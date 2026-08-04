\ maki/infer/kv-cache.f - host paged KV cache allocator for the GB10 engine.
\
\ This module owns the mutable HOST allocator: one coherent byte pool, page
\ refcounts, cache-owned fixed block-table slices, generation-bearing handles,
\ copy-on-write prefix forks, physical allocator metrics, and max-context page
\ reservations. The device never reads these mutable tables. Immutable device
\ publication is the separate habu-lease-kv-snapshot-9ef40f19 child.
\
\ A sequence handle is the structured value (cache identity, slot, slot
\ generation). Cache identities and slot generations advance monotonically and
\ reject at their ceiling instead of wrapping. Reusing a slot, rebuilding a cache
\ in the same header, or passing a handle to another cache therefore fails closed.
\
\ Reservation is owned state, not a fit predicate. ALLOC-SEQ requires a declared
\ maximum token count and reserves its exact ceiling page count before admission.
\ Every page-boundary append consumes one page from that sequence's reservation;
\ cancellation releases every unused page.
\ Forking a partially occupied reserved tail first reserves the possible copy-on-
\ write page, so the parent's maximum-context guarantee survives sharing.
\ The scheduler is the sole mutation owner; it serializes allocator calls while
\ any number of admitted sequences retain independent outstanding reservations.
\
\ All configuration, metadata, page, and ceiling calculations use pre-multiply /
\ pre-add checks against KV-MAX-N. Ceiling division is quotient-plus-remainder and
\ never forms n+d-1. Allocation, fork and append perform every fallible preflight
\ before ownership mutation. Cancellation commits logical death first, returns
\ page ownership, then clears its fixed table slice; cleanup cannot revive a stale
\ handle.

require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/type/deftype.f
require maki/gpu-buffer.f

-5622 constant E-KV-CONFIG      \ invalid or overflowing configuration
-5625 constant E-KV-SEQS        \ no sequence slot is free
-5626 constant E-KV-SEQ         \ stale, dead, reused, or cross-cache handle
-5627 constant E-KV-BOUNDS      \ token/page/index outside the owned range
-5628 constant E-KV-INVARIANT   \ allocator accounting is internally inconsistent
-5629 constant E-KV-ADMIT       \ reservation or protected-capacity admission failure
-5630 constant E-KV-ID          \ cache identity or per-slot generation exhausted

package KV

private

DEFTYPE KV-CACHE-ID
DEFTYPE KV-SEQ-SLOT
DEFTYPE KV-SEQ-GEN

public

-5624 constant E-KV-BATCH       \ another batch is active, or this owner does not match

DEFLINEAR KV:cache
DEFLINEAR KV:batch

ENUM cancel-result 0
   VARIANT cancelled FIELD cache KV:cache ;VARIANT
   VARIANT refused FIELD cache KV:cache FIELD batch KV:batch FIELD code n ;VARIANT
;ENUM

\ The nominal fields are private representation roles. The structure is public so
\ callers can transport a sequence value, but only allocator operations mint the
\ three field values from raw state.
STRUCTURE seq 0
   FIELD cache kv-cache-id
   FIELD slot kv-seq-slot
   FIELD gen kv-seq-gen
;STRUCTURE

STRUCTURE config 0
   FIELD nlayer n
   FIELD nkv n
   FIELD hdim n
   FIELD dbytes n
   FIELD npages n
   FIELD nseq n
   FIELD maxctx n
   FIELD ptok n
;STRUCTURE

private

\ KV:cache is the KV-owned host mapping. Cell zero stores the consumed
\ GPU:buffer owner. The checker cannot yet bind those owners to that mapping.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: KC-MINT ( GPU:buffer ptr u8 -- KV:cache )
   swap over ! ;

TRUSTED: KC-TAKE ( KV:cache -- GPU:buffer ptr u8 )
   dup @ swap ;

\ The checker cannot bind a raw generation to its opaque linear batch owner.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: KB-MINT ( n -- KV:batch ) ;

\ The checker cannot recover a raw generation from its opaque linear batch owner.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: KB-TAKE ( KV:batch -- n ) ;

0  constant BUF-OFF
1  cells constant HOSTB-OFF
2  cells constant DEVB-OFF
3  cells constant NPAGES-OFF
4  cells constant NSEQ-OFF
5  cells constant PAGEB-OFF
6  cells constant TOKB-OFF
7  cells constant FREETOP-OFF
8  cells constant NLAYER-OFF
9  cells constant NKV-OFF
10 cells constant HDIM-OFF
11 cells constant DBYTES-OFF
12 cells constant PTOK-OFF
13 cells constant MAXCTX-OFF
14 cells constant BLKCAP-OFF
15 cells constant HIWATER-OFF
16 cells constant CACHEID-OFF
17 cells constant RESERVED-OFF
18 cells constant BATCH-OFF
19 cells constant HEADER-SIZE

$7FFFFFFFFFFFFFFF constant KV-MAX-N
KV-MAX-N constant KV-ID-MAX
16 constant KV-P

variable KV-NEXT-CACHE-ID
variable NEXT-BATCH-GEN

: META@ ( ptr a -- ptr a )  HEADER-SIZE + ;

: H@ ( ptr a n -- n )  + @ ;
: H! ( n ptr a n -- ) {: v:n h:ptr off:n :}  v h off + ! ;

\ ---- checked nonnegative arithmetic -----------------------------------------------
: KV-NONNEG ( n -- n )
   dup 0 < if E-KV-CONFIG throw then ;

: KV-POS ( n -- n )
   dup 0 <= if E-KV-CONFIG throw then ;

: KV-ADD0 ( n n -- n ) {: a:n b:n :}
   a KV-NONNEG drop  b KV-NONNEG drop
   a KV-MAX-N b - > if E-KV-CONFIG throw then
   a b + ;

: KV-MUL0 ( n n -- n ) {: a:n b:n :}
   a KV-NONNEG drop  b KV-NONNEG drop
   a 0= b 0= or if 0 exit then
   a KV-MAX-N b / > if E-KV-CONFIG throw then
   a b * ;

: CALC-ROW ( n n n -- n ) {: nkv:n hdim:n dbytes:n :}
   2 nkv KV-MUL0 hdim KV-MUL0 dbytes KV-MUL0 ;

: SIZE ( n n n n n n -- n n n )
   {: nlayer:n nkv:n hdim:n dbytes:n npages:n ptok:n :}
   nkv hdim dbytes CALC-ROW {: rowb:n :}
   nlayer rowb KV-MUL0 {: tokb:n :}
   ptok tokb KV-MUL0 {: pageb:n :}
   npages pageb KV-MUL0 {: devb:n :}
   pageb tokb devb ;

: META-CELLS ( n n n -- n ) {: np:n ns:n blkcap:n :}
   np 3 KV-MUL0
   ns 6 KV-MUL0 KV-ADD0
   ns blkcap KV-MUL0 KV-ADD0
   dup 1 cells KV-MUL0 drop ;

: KV-PAGES-FOR-RAW ( n n -- n ) {: toks:n ptok:n :}
   toks KV-NONNEG drop  ptok KV-POS drop
   toks ptok /  toks ptok mod 0<> if 1 KV-ADD0 then ;

\ ---- metadata partition ------------------------------------------------------------
\ [free np][refcount np][COW reservation np][len ns][live ns]
\ [generation ns][future-page reservation ns][maximum tokens ns][block len ns]
\ [nseq * block-cap fixed page ids].
: FREE-BASE ( ptr a -- ptr a )  META@ ;

: REFC-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h META@ h NPAGES-OFF H@ cells + ;

: COWRES-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h REFC-BASE h NPAGES-OFF H@ cells + ;

: SEQLEN-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h COWRES-BASE h NPAGES-OFF H@ cells + ;

: SEQLIVE-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQLEN-BASE h NSEQ-OFF H@ cells + ;

: SEQGEN-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQLIVE-BASE h NSEQ-OFF H@ cells + ;

: SEQRES-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQGEN-BASE h NSEQ-OFF H@ cells + ;

: SEQMAX-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQRES-BASE h NSEQ-OFF H@ cells + ;

: SEQBLEN-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQMAX-BASE h NSEQ-OFF H@ cells + ;

: SEQBLK-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQBLEN-BASE h NSEQ-OFF H@ cells + ;

: FREE@ ( ptr a n -- n ) {: h:ptr i:n :}  h FREE-BASE i cells + @ ;
: FREE! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h FREE-BASE i cells + ! ;
: REFC@ ( ptr a n -- n ) {: h:ptr i:n :}  h REFC-BASE i cells + @ ;
: REFC! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h REFC-BASE i cells + ! ;
: COWRES@ ( ptr a n -- n ) {: h:ptr i:n :}  h COWRES-BASE i cells + @ ;
: COWRES! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h COWRES-BASE i cells + ! ;
: SEQLEN@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQLEN-BASE i cells + @ ;
: SEQLEN! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQLEN-BASE i cells + ! ;
: SEQLIVE@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQLIVE-BASE i cells + @ ;
: SEQLIVE! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQLIVE-BASE i cells + ! ;
: SEQGEN@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQGEN-BASE i cells + @ ;
: SEQGEN! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQGEN-BASE i cells + ! ;
: SEQRES@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQRES-BASE i cells + @ ;
: SEQRES! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQRES-BASE i cells + ! ;
: SEQMAX@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQMAX-BASE i cells + @ ;
: SEQMAX! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQMAX-BASE i cells + ! ;
: SEQBLEN@ ( ptr a n -- n ) {: h:ptr i:n :}  h SEQBLEN-BASE i cells + @ ;
: SEQBLEN! ( n ptr a n -- ) {: v:n h:ptr i:n :}  v h SEQBLEN-BASE i cells + ! ;

: SEQBLK ( ptr a n -- ptr a ) {: h:ptr i:n :}
   h SEQBLK-BASE i h BLKCAP-OFF H@ KV-MUL0 cells + ;

\ ---- block-table operations --------------------------------------------------------
: KV-BLK-LEN ( ptr a n -- n ) {: h:ptr s:n :}
   h s SEQBLEN@ ;

: KV-BLK@ ( ptr a n n -- n ) {: h:ptr s:n i:n :}
   h s SEQBLK i cells + @ ;

: KV-BLK! ( n ptr a n n -- ) {: v:n h:ptr s:n i:n :}
   v h s SEQBLK i cells + ! ;

: BLK-ROOM ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-BLK-LEN h BLKCAP-OFF H@ >= if E-KV-ADMIT throw then ;

: KV-BLK-PUSH ( n ptr a n -- ) {: v:n h:ptr s:n :}
   h s BLK-ROOM
   h s KV-BLK-LEN {: len:n :}
   v h s len KV-BLK!
   len 1 KV-ADD0 h s SEQBLEN! ;

: BLK-CLEAR ( ptr a n -- ) {: h:ptr s:n :}
   h BLKCAP-OFF H@ 0 ?do 0 h s i KV-BLK! loop
   0 h s SEQBLEN! ;

\ ---- cache and structured-handle validation ---------------------------------------
: MAKE-HANDLE ( ptr a n -- seq ) {: h:ptr s:n :}
   h CACHEID-OFF H@ >KV-CACHE-ID
   s >KV-SEQ-SLOT
   h s SEQGEN@ >KV-SEQ-GEN
   KV-SEQ:MAKE ;

: SEQ-PARTS ( seq -- n n n )
   KV-SEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N slot KV-SEQ-SLOT>N gen KV-SEQ-GEN>N ;

: SEQ-CK ( ptr a seq -- ptr a n )
   SEQ-PARTS {: h:ptr cid:n s:n gen:n :}
   cid h CACHEID-OFF H@ <> if E-KV-SEQ throw then
   s 0 < s h NSEQ-OFF H@ >= or if E-KV-SEQ throw then
   h s SEQLIVE@ 0= if E-KV-SEQ throw then
   gen h s SEQGEN@ <> if E-KV-SEQ throw then
   h s ;

\ ---- free pages and reservation accounting ----------------------------------------
: KV-FILL-FREE ( ptr a -- ) {: h:ptr :}
   h NPAGES-OFF H@ {: np:n :}
   np 0 ?do i h i FREE! loop
   np h FREETOP-OFF H! ;

: KV-POP-PREFLIGHT ( ptr a -- ) {: h:ptr :}
   h FREETOP-OFF H@ 0 <= if E-KV-INVARIANT throw then ;

: KV-POP-COMMIT ( ptr a -- n ) {: h:ptr :}
   h FREETOP-OFF H@ 1- {: top:n :}
   top h FREETOP-OFF H!
   h NPAGES-OFF H@ top - h HIWATER-OFF H@ max h HIWATER-OFF H!
   h top FREE@ ;

: KV-PUSH-FREE ( n ptr a -- ) {: pid:n h:ptr :}
   h FREETOP-OFF H@ {: top:n :}
   pid h top FREE!
   top 1+ h FREETOP-OFF H! ;

: KV-UNRESERVED ( ptr a -- n ) {: h:ptr :}
   h FREETOP-OFF H@ h RESERVED-OFF H@ - ;

: KV-RESERVE-PREFLIGHT ( ptr a n -- ) {: h:ptr pages:n :}
   pages KV-NONNEG drop
   pages h KV-UNRESERVED > if E-KV-ADMIT throw then ;

: KV-GLOBAL-RESERVE+ ( ptr a n -- ) {: h:ptr pages:n :}
   h RESERVED-OFF H@ pages KV-ADD0 h RESERVED-OFF H! ;

: KV-GLOBAL-RESERVE- ( ptr a n -- ) {: h:ptr pages:n :}
   h RESERVED-OFF H@ pages - h RESERVED-OFF H! ;

: KV-SEQ-RESERVE-ONE ( ptr a n -- ) {: h:ptr s:n :}
   h s SEQRES@ 1- h s SEQRES!
   h 1 KV-GLOBAL-RESERVE- ;

: KV-COW-RESERVE+ ( ptr a n -- ) {: h:ptr pid:n :}
   h 1 KV-GLOBAL-RESERVE+
   h pid COWRES@ 1+ h pid COWRES! ;

: KV-GROW-PREFLIGHT ( ptr a n -- ) {: h:ptr s:n :}
   h s SEQRES@ 0 <= if E-KV-INVARIANT throw then
   h KV-POP-PREFLIGHT ;

: KV-TAKE-GROW-COMMIT ( ptr a n -- n ) {: h:ptr s:n :}
   h KV-POP-COMMIT {: pid:n :}
   h s KV-SEQ-RESERVE-ONE
   pid ;

: KV-COW-GUARANTEED? ( ptr a n -- bool ) {: h:ptr s:n :}
   h s SEQMAX@ h s SEQLEN@ > ;

\ ---- page references and physical copy-on-write reservation -----------------------
: KV-REF+ ( ptr a n -- ) {: h:ptr pid:n :}
   h pid REFC@ 1+ h pid REFC! ;

: KV-PARTIAL? ( ptr a n -- bool ) {: h:ptr s:n :}
   h s SEQLEN@ h PTOK-OFF H@ mod 0<> ;

: KV-TAIL-PAGE ( ptr a n -- n ) {: h:ptr s:n :}
   h s h s KV-BLK-LEN 1- KV-BLK@ ;

: KV-GUARANTEED-TAIL? ( ptr a n n -- bool ) {: h:ptr s:n pid:n :}
   h s SEQLIVE@ 0= if false exit then
   h s KV-COW-GUARANTEED? 0= if false exit then
   h s KV-PARTIAL? 0= if false exit then
   h s KV-TAIL-PAGE pid = ;

: KV-GUARANTEED-TAILS ( ptr a n -- n ) {: h:ptr pid:n :}
   0
   h NSEQ-OFF H@ 0 ?do
      h i pid KV-GUARANTEED-TAIL? if 1+ then
   loop ;

: KV-COW-DESIRED ( ptr a n -- n ) {: h:ptr pid:n :}
   h pid KV-GUARANTEED-TAILS
   h pid REFC@ 1- 0 max min ;

: KV-REBALANCE-COW ( ptr a n -- ) {: h:ptr pid:n :}
   h pid KV-COW-DESIRED {: want:n :}
   h pid COWRES@ {: have:n :}
   want have > if E-KV-INVARIANT throw then
   have want - {: release:n :}
   want h pid COWRES!
   h release KV-GLOBAL-RESERVE- ;

: KV-REF-RETURN ( ptr a n -- ) {: h:ptr pid:n :}
   h pid REFC@ 1- {: rc:n :}
   rc h pid REFC!
   h pid KV-REBALANCE-COW
   rc 0= if pid h KV-PUSH-FREE then ;

\ ---- page addressing and append ----------------------------------------------------
: PAGE-OFF ( ptr a n -- n ) {: h:ptr pid:n :}
   pid h PAGEB-OFF H@ KV-MUL0 ;

: KV-GROW-PAGE ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-GROW-PREFLIGHT
   h s BLK-ROOM
   h s KV-TAKE-GROW-COMMIT {: pid:n :}
   1 h pid REFC!
   pid h s KV-BLK-PUSH ;

: COW-PLAN ( ptr a n n -- n n n n n n n n n ) {: h:ptr s:n len:n :}
   h s KV-BLK-LEN 1- {: last:n :}
   h s last KV-BLK@ {: old:n :}
   h s KV-COW-GUARANTEED? 0= if E-KV-INVARIANT throw then
   h old REFC@ {: oldref:n :}
   oldref 1 <= if E-KV-INVARIANT throw then
   h old COWRES@ {: cow:n :}
   cow 0 <= if E-KV-INVARIANT throw then
   h RESERVED-OFF H@ {: res:n :}
   res 0 <= if E-KV-INVARIANT throw then
   h FREETOP-OFF H@ {: top:n :}
   top 0 <= if E-KV-INVARIANT throw then
   top 1- {: newtop:n :}
   h newtop FREE@ {: new:n :}
   new 0 < new h NPAGES-OFF H@ >= or if E-KV-INVARIANT throw then
   h new REFC@ 0<> h new COWRES@ 0<> or if E-KV-INVARIANT throw then
   h old KV-COW-DESIRED cow <> if E-KV-INVARIANT throw then
   h NPAGES-OFF H@ newtop - h HIWATER-OFF H@ max {: hi:n :}
   len 1 KV-ADD0
   last old new newtop hi res 1- cow 1- oldref 1- ;

: COW-COMMIT ( ptr a n n n n n n n n n n -- )
   {: h:ptr s:n newlen:n last:n old:n new:n top:n hi:n res:n cow:n oldref:n :}
   top h FREETOP-OFF H!
   hi h HIWATER-OFF H!
   res h RESERVED-OFF H!
   cow h old COWRES!
   1 h new REFC!
   new h s last KV-BLK!
   oldref h old REFC!
   newlen h s SEQLEN! ;

: KV-APPEND-LIMIT-CK ( n n -- ) {: len:n maxtoks:n :}
   maxtoks 0 > len maxtoks >= and if E-KV-ADMIT throw then ;

: APPEND-PREFLIGHT ( ptr a seq -- ptr a n n )
   SEQ-CK {: h:ptr s:n :}
   h s SEQLEN@ {: len:n :}
   len h MAXCTX-OFF H@ KV-APPEND-LIMIT-CK
   len h s SEQMAX@ KV-APPEND-LIMIT-CK
   h s len ;

\ ---- sequence allocation and fork transaction -------------------------------------
: KV-FIND-SLOT ( ptr a -- n ) {: h:ptr :}
   false
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0= if
         h i SEQGEN@ KV-ID-MAX < if drop i unloop exit then
         drop true
      then
   loop
   if E-KV-ID else E-KV-SEQS then throw ;

: KV-NEXT-SLOT-GEN ( ptr a n -- n ) {: h:ptr s:n :}
   h s SEQGEN@ dup KV-ID-MAX >= if drop E-KV-ID throw then 1+ ;

: DEAD-SLOT-CK ( ptr a n -- ) {: h:ptr s:n :}
   h s SEQLEN@ 0<> h s SEQRES@ 0<> or
   h s SEQMAX@ 0<> or h s SEQBLEN@ 0<> or if E-KV-INVARIANT throw then
   h BLKCAP-OFF H@ 0 ?do
      h s i KV-BLK@ 0<> if E-KV-INVARIANT throw then
   loop ;

: PREP-SLOT ( ptr a -- n n ) {: h:ptr :}
   h KV-FIND-SLOT {: s:n :}
   h s DEAD-SLOT-CK
   h s KV-NEXT-SLOT-GEN {: gen:n :}
   s gen ;

: COMMIT-SLOT ( ptr a n n n n -- seq ) {: h:ptr s:n gen:n pages:n maxtoks:n :}
   0 h s SEQLEN!
   pages h s SEQRES!
   maxtoks h s SEQMAX!
   h pages KV-GLOBAL-RESERVE+
   gen h s SEQGEN!
   1 h s SEQLIVE!                           \ commit last
   h s MAKE-HANDLE ;

: ADMIT-PAGES ( ptr a n -- n ) {: h:ptr toks:n :}
   toks 0 <= if E-KV-ADMIT throw then
   toks h MAXCTX-OFF H@ > if E-KV-ADMIT throw then
   toks h PTOK-OFF H@ KV-PAGES-FOR-RAW ;

: ADMIT-SEQ ( ptr a n -- seq ) {: h:ptr toks:n :}
   h toks ADMIT-PAGES {: pages:n :}
   h pages KV-RESERVE-PREFLIGHT
   h PREP-SLOT {: s:n gen:n :}
   h s gen pages toks COMMIT-SLOT ;

: KV-FORK-COW-EXTRA ( ptr a n -- n ) {: h:ptr p:n :}
   h p KV-COW-GUARANTEED? 0= if 0 exit then
   h p KV-PARTIAL? 0= if 0 exit then
   1 ;

: KV-FORK-EXTRA ( ptr a n -- n ) {: h:ptr p:n :}
   h p SEQRES@ h p KV-FORK-COW-EXTRA KV-ADD0 ;

: KV-COPY-BLK ( ptr a n n -- ) {: h:ptr c:n p:n :}
   h p KV-BLK-LEN 0 ?do
      h p i KV-BLK@ {: pid:n :}
      pid h c KV-BLK-PUSH
      h pid KV-REF+
   loop ;

: FORK-INNER ( ptr a seq -- seq )
   SEQ-CK {: h:ptr p:n :}
   h p KV-FORK-COW-EXTRA {: cowextra:n :}
   h p KV-FORK-EXTRA {: extra:n :}
   h extra KV-RESERVE-PREFLIGHT
   h p KV-BLK-LEN h BLKCAP-OFF H@ > if E-KV-INVARIANT throw then
   h PREP-SLOT {: c:n gen:n :}
   h c p KV-COPY-BLK
   h p SEQLEN@ h c SEQLEN!
   h p SEQRES@ {: childres:n :}
   childres h c SEQRES!
   h p SEQMAX@ h c SEQMAX!
   h childres KV-GLOBAL-RESERVE+
   gen h c SEQGEN!
   cowextra 0<> if
      h h p KV-TAIL-PAGE KV-COW-RESERVE+
   then
   1 h c SEQLIVE!                            \ child commit last
   h c MAKE-HANDLE ;

\ ---- cancellation: logical ownership is consumed before fixed-table clear ----------
: KV-CANCEL-RESERVATION ( ptr a n -- ) {: h:ptr s:n :}
   h s SEQRES@ {: pages:n :}
   h pages KV-GLOBAL-RESERVE-
   0 h s SEQRES!
   0 h s SEQMAX! ;

: KV-CANCEL-PAGES ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-BLK-LEN 0 ?do h h s i KV-BLK@ KV-REF-RETURN loop ;

: KV-CANCEL-SLOT ( ptr a n -- ) {: h:ptr s:n :}
   0 h s SEQLIVE!                             \ stale immediately; scans ignore it
   h s KV-CANCEL-RESERVATION
   h s KV-CANCEL-PAGES
   0 h s SEQLEN!
   h s BLK-CLEAR ;

: CANCEL-INNER ( ptr a seq -- )
   SEQ-CK KV-CANCEL-SLOT ;

\ ---- queries and physical-page metrics --------------------------------------------
: KV-WATERMARK ( ptr a -- n ) {: h:ptr :}
   h NPAGES-OFF H@ h FREETOP-OFF H@ - ;

: SEQ-LEN-INNER ( ptr a seq -- n )
   SEQ-CK SEQLEN@ ;

: SEQ-PAGES-INNER ( ptr a seq -- n )
   SEQ-CK KV-BLK-LEN ;

: SEQ-PAGE-INNER ( ptr a seq n -- n ) {: i:n :}
   SEQ-CK {: h:ptr s:n :}
   i 0 < i h s KV-BLK-LEN >= or if E-KV-BOUNDS throw then
   h s i KV-BLK@ ;

: KV-PAGE-REFC ( ptr a n -- n ) {: h:ptr pid:n :}
   pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-BOUNDS throw then
   h pid REFC@ ;

: ROW-BYTES ( ptr a -- n ) {: h:ptr :}
   h NKV-OFF H@ h HDIM-OFF H@ h DBYTES-OFF H@ CALC-ROW ;

: LAYER-SPAN ( ptr a -- n ) {: h:ptr :}
   h PTOK-OFF H@ h ROW-BYTES KV-MUL0 ;

: LAYER-CK ( ptr a n -- ) {: h:ptr layer:n :}
   layer 0 < layer h NLAYER-OFF H@ >= or if E-KV-BOUNDS throw then ;

: KV-SHARED-PAGES ( ptr a -- n ) {: h:ptr :}
   0 h NPAGES-OFF H@ 0 ?do h i REFC@ 1 > if 1+ then loop ;

: KV-PAGE-OCC ( ptr a n n -- n ) {: h:ptr s:n pageix:n :}
   h s SEQLEN@ pageix h PTOK-OFF H@ KV-MUL0 -
   0 max h PTOK-OFF H@ min ;

: KV-PAGE-MAX-OCC ( ptr a n -- n ) {: h:ptr pid:n :}
   0
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0<> if
         h i KV-BLK-LEN 0 ?do
            h j i KV-BLK@ pid = if h j i KV-PAGE-OCC max then
         loop
      then
   loop ;

: KV-TAIL-WASTE ( ptr a -- n ) {: h:ptr :}
   0
   h NPAGES-OFF H@ 0 ?do
      h i REFC@ 0 > if h PTOK-OFF H@ h i KV-PAGE-MAX-OCC - KV-ADD0 then
   loop ;

\ ---- invariant proof ---------------------------------------------------------------
: KV-SEQ-REFS ( ptr a n n -- n ) {: h:ptr s:n pid:n :}
   0 h s KV-BLK-LEN 0 ?do h s i KV-BLK@ pid = if 1+ then loop ;

: KV-COUNT-REFS ( ptr a n -- n ) {: h:ptr pid:n :}
   0 h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0<> if h i pid KV-SEQ-REFS + then
   loop ;

: KV-CHECK-REFS ( ptr a -- ) {: h:ptr :}
   h NPAGES-OFF H@ 0 ?do
      h i REFC@ h i KV-COUNT-REFS <> if E-KV-INVARIANT throw then
   loop ;

: KV-CHECK-FREE-DUP ( ptr a n n -- ) {: h:ptr start:n pid:n :}
   h FREETOP-OFF H@ start ?do
      h i FREE@ pid = if E-KV-INVARIANT throw then
   loop ;

: KV-CHECK-FREE-AT ( ptr a n -- ) {: h:ptr i:n :}
   h i FREE@ {: pid:n :}
   pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-INVARIANT throw then
   h pid REFC@ 0<> if E-KV-INVARIANT throw then
   h i 1+ pid KV-CHECK-FREE-DUP ;

: KV-CHECK-FREE ( ptr a -- ) {: h:ptr :}
   0 h NPAGES-OFF H@ 0 ?do h i REFC@ 0= if 1+ then loop
   h FREETOP-OFF H@ <> if E-KV-INVARIANT throw then
   h FREETOP-OFF H@ 0 ?do h i KV-CHECK-FREE-AT loop ;

: KV-CHECK-SEQ ( ptr a n -- n ) {: h:ptr s:n :}
   h s SEQLIVE@ 0= if
      h s DEAD-SLOT-CK
      0 exit
   then
   h s SEQGEN@ 0 <= if E-KV-INVARIANT throw then
   h s KV-BLK-LEN h BLKCAP-OFF H@ > if E-KV-INVARIANT throw then
   h s SEQLEN@ h s KV-BLK-LEN h PTOK-OFF H@ KV-MUL0 > if E-KV-INVARIANT throw then
   h s SEQLEN@ h MAXCTX-OFF H@ > if E-KV-INVARIANT throw then
   h s SEQMAX@ {: maxtoks:n :}
   maxtoks 0 <= maxtoks h MAXCTX-OFF H@ > or if E-KV-INVARIANT throw then
   h s SEQLEN@ maxtoks > if E-KV-INVARIANT throw then
   maxtoks h PTOK-OFF H@ KV-PAGES-FOR-RAW h s KV-BLK-LEN -
   h s SEQRES@ <> if E-KV-INVARIANT throw then
   h s SEQRES@ ;

: KV-CHECK-COW ( ptr a -- n ) {: h:ptr :}
   0
   h NPAGES-OFF H@ 0 ?do
      h i COWRES@ h i KV-COW-DESIRED <> if E-KV-INVARIANT throw then
      h i COWRES@ KV-ADD0
   loop ;

: KV-CHECK-RESERVATIONS ( ptr a -- ) {: h:ptr :}
   0 h NSEQ-OFF H@ 0 ?do h i KV-CHECK-SEQ KV-ADD0 loop
   h KV-CHECK-COW KV-ADD0
   dup h RESERVED-OFF H@ <> if drop E-KV-INVARIANT throw then drop
   h RESERVED-OFF H@ h FREETOP-OFF H@ > if E-KV-INVARIANT throw then ;

: KV-CHECK ( ptr a -- ) {: h:ptr :}
   h CACHEID-OFF H@ 0 <= if E-KV-INVARIANT throw then
   h KV-CHECK-REFS
   h KV-CHECK-FREE
   h KV-CHECK-RESERVATIONS ;

: KV-ID-PREFLIGHT ( -- )
   KV-NEXT-CACHE-ID @ KV-ID-MAX >= if E-KV-ID throw then ;

: KV-ID-COMMIT ( ptr a -- ) {: h:ptr :}
   KV-NEXT-CACHE-ID @ 1+ dup KV-NEXT-CACHE-ID ! h CACHEID-OFF H! ;

: BATCH-COMMIT ( ptr a n -- ) {: h:ptr gen:n :}
   gen NEXT-BATCH-GEN !
   gen h BATCH-OFF H! ;

: STORE-DIMS ( ptr a n n n n n n n n n n n n n -- )
   {: h:ptr nlayer:n nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n pageb:n tokb:n blkcap:n hostb:n devb:n :}
   0 h BUF-OFF H!
   hostb h HOSTB-OFF H!  devb h DEVB-OFF H!
   nlayer h NLAYER-OFF H!
   nkv h NKV-OFF H!  hdim h HDIM-OFF H!  dbytes h DBYTES-OFF H!
   npages h NPAGES-OFF H!  nseq h NSEQ-OFF H!  ptok h PTOK-OFF H!
   maxctx h MAXCTX-OFF H!  blkcap h BLKCAP-OFF H!
   pageb h PAGEB-OFF H!  tokb h TOKB-OFF H!
   0 h HIWATER-OFF H!  0 h RESERVED-OFF H!  0 h BATCH-OFF H! ;

: CONFIG-VALUES ( n n n n n n n n -- n n n n n n )
   {: nlayer:n nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n :}
   nlayer KV-POS drop  nkv KV-POS drop  hdim KV-POS drop  dbytes KV-POS drop
   npages KV-POS drop  nseq KV-POS drop  maxctx KV-POS drop  ptok KV-POS drop
   nlayer nkv hdim dbytes npages ptok SIZE {: pageb:n tokb:n devb:n :}
   maxctx ptok KV-PAGES-FOR-RAW {: blkcap:n :}
   blkcap npages > if E-KV-CONFIG throw then
   npages nseq blkcap META-CELLS {: metac:n :}
   metac 1 cells KV-MUL0 HEADER-SIZE KV-ADD0 {: hostb:n :}
   pageb tokb blkcap metac hostb devb ;

: ALLOC-HOST-REAL ( ptr u8 n -- ptr u8 n )
   {: old:ptr bytes:n :}
   old drop
   bytes MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop bytes ;

: RELEASE-HOST-REAL ( ptr u8 n -- )
   MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

defer HOST-ALLOC ( ptr u8 n -- ptr u8 n )
defer HOST-RELEASE ( ptr u8 n -- )

: HOST-USE-REAL ( -- )
   [: ALLOC-HOST-REAL ;] is HOST-ALLOC
   [: RELEASE-HOST-REAL ;] is HOST-RELEASE ;

HOST-USE-REAL

: ALLOC-HOST ( n -- ptr u8 n n )
   NULL$ drop swap [: HOST-ALLOC ;] catch ;

: RELEASE-HOST ( ptr u8 -- ) {: h:ptr :}
   h h HOSTB-OFF H@ HOST-RELEASE ;

: RES-CODE ( result<n,n> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: CAD-OFF ( n -- CAD-NUM:byte-off )
   CAD-NUM:BYTE-OFF MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-KV-INVARIANT throw ENDOF
      zero OF E-KV-INVARIANT throw ENDOF
      overflow OF E-KV-INVARIANT throw ENDOF
      underflow OF E-KV-INVARIANT throw ENDOF
      bad-alignment OF E-KV-INVARIANT throw ENDOF
      misaligned OF E-KV-INVARIANT throw ENDOF
   ;MATCH ;

: CAD-LEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-KV-INVARIANT throw ENDOF
      zero OF E-KV-INVARIANT throw ENDOF
      overflow OF E-KV-INVARIANT throw ENDOF
      underflow OF E-KV-INVARIANT throw ENDOF
      bad-alignment OF E-KV-INVARIANT throw ENDOF
      misaligned OF E-KV-INVARIANT throw ENDOF
   ;MATCH ;

: LOAD-SEQ ( n n n -- seq )
   {: cid:n slot:n gen:n :}
   cid >KV-CACHE-ID slot >KV-SEQ-SLOT gen >KV-SEQ-GEN KV-SEQ:MAKE ;

: ALLOC-TRY ( ptr a n n n n -- ptr a n n n n )
   {: h:ptr max:n xcid:n xslot:n xgen:n :}
   xcid drop xslot drop xgen drop
   h max ADMIT-SEQ SEQ-PARTS {: cid:n slot:n gen:n :}
   h max cid slot gen ;

: FORK-TRY ( ptr a n n n -- ptr a n n n )
   {: h:ptr cid:n slot:n gen:n :}
   h cid slot gen LOAD-SEQ FORK-INNER SEQ-PARTS
   {: ncid:n nslot:n ngen:n :}
   h ncid nslot ngen ;

: CANCEL-TRY ( ptr a n n n -- ptr a n n n )
   {: h:ptr cid:n slot:n gen:n :}
   h cid slot gen LOAD-SEQ CANCEL-INNER
   h cid slot gen ;

: SEQ-LEN-TRY ( ptr a n n n n -- ptr a n n n n )
   {: h:ptr cid:n slot:n gen:n x:n :} x drop
   h cid slot gen LOAD-SEQ SEQ-LEN-INNER {: value:n :}
   h cid slot gen value ;

: SEQ-RES-TRY ( ptr a n n n n -- ptr a n n n n )
   {: h:ptr cid:n slot:n gen:n x:n :} x drop
   h cid slot gen LOAD-SEQ SEQ-CK SEQRES@ {: value:n :}
   h cid slot gen value ;

: PAGES-FOR-TRY ( ptr a n n -- ptr a n n )
   {: h:ptr toks:n x:n :} x drop
   toks h PTOK-OFF H@ KV-PAGES-FOR-RAW {: pages:n :}
   h toks pages ;

: APPEND-NOCOPY ( ptr a n n n -- ptr a n n n n n n n n n n n n n n )
   0 0 0 0 0 0 0 0 0 0 0 ;

: APPEND-TRY
   ( ptr a n n n n n n n n n n n n n n -- ptr a n n n n n n n n n n n n n n )
   {: h:ptr cid:n slot:n gen:n xcopy:n xs:n xlen:n xlast:n xold:n xnew:n xtop:n xhi:n xres:n xcow:n xref:n :}
   xcopy drop xs drop xlen drop xlast drop xold drop xnew drop
   xtop drop xhi drop xres drop xcow drop xref drop
   h cid slot gen LOAD-SEQ APPEND-PREFLIGHT
   {: hp:ptr s:n len:n :}
   len h PTOK-OFF H@ mod 0= if
      hp s KV-GROW-PAGE
      len 1+ hp s SEQLEN!
      hp cid slot gen APPEND-NOCOPY exit
   then
   hp s KV-TAIL-PAGE {: old:n :}
   hp old REFC@ 1 <= if
      len 1+ hp s SEQLEN!
      hp cid slot gen APPEND-NOCOPY exit
   then
   hp cid slot gen 1 s
   hp s len COW-PLAN ;

: CONFIG-INNER ( n n n n n n n n -- config )
   {: nlayer:n nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n :}
   nlayer nkv hdim dbytes npages nseq maxctx ptok CONFIG-VALUES 2drop 2drop 2drop
   nlayer nkv hdim dbytes npages nseq maxctx ptok KV-CONFIG:MAKE ;

: OPEN-PLAN
   ( n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n )
   {: nlayer:n nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n
      xpageb:n xtokb:n xblkcap:n xmetac:n xhostb:n xdevb:n :}
   xpageb drop xtokb drop xblkcap drop xmetac drop xhostb drop xdevb drop
   nlayer nkv hdim dbytes npages nseq maxctx ptok CONFIG-VALUES
   {: pageb:n tokb:n blkcap:n metac:n hostb:n devb:n :}
   KV-ID-PREFLIGHT
   nlayer nkv hdim dbytes npages nseq maxctx ptok
   pageb tokb blkcap metac hostb devb ;

: OPEN-OK ( GPU:buffer ptr u8 -- result<KV:cache,n> ) {: h:ptr :}
   h KV-ID-COMMIT
   h KC-MINT RESULT:OK ;

: OPEN-ERR ( n ptr u8 -- result<KV:cache,n> ) {: code:n h:ptr :}
   h RELEASE-HOST
   code RESULT:ERR ;

: OPEN-PUBLISH
   ( GPU:session result<GPU:buffer,n> ptr u8 -- GPU:session result<KV:cache,n> )
   {: h:ptr :}
   MATCH result
      ok OF h OPEN-OK ENDOF
      err OF h OPEN-ERR ENDOF
   ;MATCH ;

: OPEN-INNER ( GPU:session config -- GPU:session result<KV:cache,n> )
   KV-CONFIG:UNMAKE
   0 0 0 0 0 0 [: OPEN-PLAN ;] catch
   {: nlayer:n nkv:n hdim:n dbytes:n npages:n nseq:n maxctx:n ptok:n
      pageb:n tokb:n blkcap:n metac:n hostb:n devb:n code:n :}
   code 0<> if
      nlayer drop nkv drop hdim drop dbytes drop npages drop nseq drop
      maxctx drop ptok drop pageb drop tokb drop blkcap drop metac drop
      hostb drop devb drop
      code RESULT:ERR exit
   then
   metac drop
   hostb ALLOC-HOST {: h:ptr gotb:n hcode:n :}
   hcode 0<> if hcode RESULT:ERR exit then
   gotb drop
   h nlayer nkv hdim dbytes npages nseq maxctx ptok
   pageb tokb blkcap hostb devb STORE-DIMS
   h KV-FILL-FREE
   devb MEM:BYTES-ALLOC-LEN GPU:ALLOC h OPEN-PUBLISH ;

: CLOSE-INNER ( GPU:session KV:cache -- GPU:session result<n,n> )
   KC-TAKE {: h:ptr :}
   GPU:FREE RES-CODE {: code:n :}
   h RELEASE-HOST
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

public

: PAGE-TOKENS ( -- n )  KV-P ;
: CONFIG ( n n n n n n n -- config )  KV-P CONFIG-INNER ;
: CONFIG/P ( n n n n n n n n -- config )  CONFIG-INNER ;
: OPEN ( GPU:session config -- GPU:session result<KV:cache,n> )  OPEN-INNER ;
: CLOSE ( GPU:session KV:cache -- GPU:session result<n,n> )  CLOSE-INNER ;

: BEGIN-BATCH ( KV:cache -- KV:cache result<KV:batch,n> )
   KC-TAKE {: h:ptr :}
   h BATCH-OFF H@ 0<> if h KC-MINT E-KV-BATCH RESULT:ERR exit then
   NEXT-BATCH-GEN @ KV-ID-MAX >= if
      h KC-MINT E-KV-ID RESULT:ERR exit
   then
   NEXT-BATCH-GEN @ 1+ {: gen:n :}
   h gen BATCH-COMMIT
   h KC-MINT gen KB-MINT RESULT:OK ;

: CANCEL-BATCH ( KV:cache KV:batch -- cancel-result )
   KB-TAKE {: gen:n :}
   KC-TAKE {: h:ptr :}
   h BATCH-OFF H@ {: active:n :}
   active 0<> active gen = and if
      0 h BATCH-OFF H!
      h KC-MINT KV-CANCEL--RESULT:CANCELLED exit
   then
   h KC-MINT gen KB-MINT E-KV-BATCH KV-CANCEL--RESULT:REFUSED ;

: ALLOC-SEQ ( KV:cache n -- KV:cache result<seq,n> ) {: n:n :}
   KC-TAKE {: h:ptr :}
   h n 0 0 0 [: ALLOC-TRY ;] catch
   {: rh:ptr max:n cid:n slot:n gen:n code:n :}
   rh drop max drop
   h KC-MINT
   code 0= if cid slot gen LOAD-SEQ RESULT:OK else code RESULT:ERR then ;

: FORK-SEQ ( KV:cache seq -- KV:cache result<seq,n> )
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen [: FORK-TRY ;] catch
   {: rh:ptr ncid:n nslot:n ngen:n code:n :} rh drop
   h KC-MINT
   code 0= if ncid nslot ngen LOAD-SEQ RESULT:OK else code RESULT:ERR then ;

: CANCEL-SEQ ( KV:cache seq -- KV:cache result<n,n> )
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen [: CANCEL-TRY ;] catch
   {: rh:ptr rcid:n rslot:n rgen:n code:n :}
   rh drop rcid drop rslot drop rgen drop
   h KC-MINT
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

: APPEND-TOKEN
   ( GPU:session KV:cache seq -- GPU:session KV:cache result<n,n> )
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen 0 0 0 0 0 0 0 0 0 0 0
   [: APPEND-TRY ;] catch
   {: rh:ptr rcid:n rslot:n rgen:n copy:n s:n newlen:n last:n old:n new:n top:n hi:n res:n cow:n oldref:n code:n :}
   rh drop rcid drop rslot drop rgen drop
   code 0<> if
      h KC-MINT code RESULT:ERR exit
   then
   copy 0= if
      h KC-MINT 0 RESULT:OK exit
   then
   h new PAGE-OFF CAD-OFF
   h old PAGE-OFF CAD-OFF
   h PAGEB-OFF H@ CAD-LEN
   GPU:COPY RES-CODE {: copycode:n :}
   copycode 0<> if
      h KC-MINT copycode RESULT:ERR exit
   then
   h s newlen last old new top hi res cow oldref COW-COMMIT
   h KC-MINT 0 RESULT:OK ;

: WATERMARK ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h KV-WATERMARK {: n:n :}
   h KC-MINT n ;
: FREE-PAGES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h FREETOP-OFF H@ {: n:n :}
   h KC-MINT n ;
: RESERVED-PAGES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h RESERVED-OFF H@ {: n:n :}
   h KC-MINT n ;
: SHARED-PAGES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h KV-SHARED-PAGES {: n:n :}
   h KC-MINT n ;
: TAIL-WASTE ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h KV-TAIL-WASTE {: n:n :}
   h KC-MINT n ;
: HIGH-WATER ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h HIWATER-OFF H@ {: n:n :}
   h KC-MINT n ;
: NUM-PAGES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h NPAGES-OFF H@ {: n:n :}
   h KC-MINT n ;
: PAGE-SIZE ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h PTOK-OFF H@ {: n:n :}
   h KC-MINT n ;
: PAGE-BYTES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h PAGEB-OFF H@ {: n:n :}
   h KC-MINT n ;
: TOK-BYTES ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h TOKB-OFF H@ {: n:n :}
   h KC-MINT n ;
: MAX-CONTEXT ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h MAXCTX-OFF H@ {: n:n :}
   h KC-MINT n ;
: BLOCK-CAPACITY ( KV:cache -- KV:cache n )
   KC-TAKE {: h:ptr :} h BLKCAP-OFF H@ {: n:n :}
   h KC-MINT n ;

: FOOTPRINT
   ( KV:cache -- KV:cache CAD-NUM:alloc-byte-len CAD-NUM:alloc-byte-len )
   KC-TAKE {: h:ptr :}
   h HOSTB-OFF H@ MEM:BYTES-ALLOC-LEN
   h DEVB-OFF H@ MEM:BYTES-ALLOC-LEN
   >r >r h KC-MINT r> r> ;

: SEQ-LEN ( KV:cache seq -- KV:cache result<n,n> )
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen 0 [: SEQ-LEN-TRY ;] catch
   {: rh:ptr rcid:n rslot:n rgen:n value:n code:n :}
   rh drop rcid drop rslot drop rgen drop
   h KC-MINT
   code 0= if value RESULT:OK else code RESULT:ERR then ;

: SEQ-RESERVED ( KV:cache seq -- KV:cache result<n,n> )
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen 0 [: SEQ-RES-TRY ;] catch
   {: rh:ptr rcid:n rslot:n rgen:n value:n code:n :}
   rh drop rcid drop rslot drop rgen drop
   h KC-MINT
   code 0= if value RESULT:OK else code RESULT:ERR then ;

: PAGES-FOR ( KV:cache n -- KV:cache result<n,n> ) {: n:n :}
   KC-TAKE {: h:ptr :}
   h n 0 [: PAGES-FOR-TRY ;] catch
   {: rh:ptr toks:n value:n code:n :} rh drop toks drop
   h KC-MINT
   code 0= if value RESULT:OK else code RESULT:ERR then ;

private

: HEAD-BYTES ( ptr a -- n ) {: h:ptr :}
   h HDIM-OFF H@ h DBYTES-OFF H@ KV-MUL0 ;

: HEAD-OFF ( ptr a seq n n n n -- n n )
   {: layer:n tok:n head:n kind:n :}
   SEQ-CK {: h:ptr s:n :}
   h layer LAYER-CK
   tok 0 < tok h s SEQLEN@ >= or if E-KV-BOUNDS throw then
   head 0 < head h NKV-OFF H@ >= or if E-KV-BOUNDS throw then
   h PTOK-OFF H@ {: ptok:n :}
   tok ptok / {: pageix:n :}
   tok ptok mod {: slot:n :}
   pageix 0 < pageix h s KV-BLK-LEN >= or if E-KV-BOUNDS throw then
   h s pageix KV-BLK@ {: pid:n :}
   pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-BOUNDS throw then
   h HEAD-BYTES {: headb:n :}
   h pid PAGE-OFF
   layer h LAYER-SPAN KV-MUL0 KV-ADD0
   slot h ROW-BYTES KV-MUL0 KV-ADD0
   kind h NKV-OFF H@ KV-MUL0 head KV-ADD0 headb KV-MUL0 KV-ADD0
   {: off:n :}
   off h DEVB-OFF H@ > if E-KV-BOUNDS throw then
   headb h DEVB-OFF H@ off - > if E-KV-BOUNDS throw then
   off headb ;

: SPAN-TRY
   ( ptr a n n n n n n n n n -- ptr a n n n n n n n n n )
   {: h:ptr cid:n slot:n gen:n layer:n tok:n head:n kind:n xoff:n xlen:n :}
   xoff drop xlen drop
   h cid slot gen LOAD-SEQ layer tok head kind HEAD-OFF
   {: off:n len:n :}
   h cid slot gen layer tok head kind off len ;

: SPAN-RESULT
   ( GPU:session GPU:buffer result<cuda-devptr,n> ptr u8 -- GPU:session KV:cache result<cuda-devptr,n> )
   {: h:ptr :}
   MATCH result
      ok OF >r h KC-MINT r> RESULT:OK ENDOF
      err OF >r h KC-MINT r> RESULT:ERR ENDOF
   ;MATCH ;

: SPAN-INNER
   ( GPU:session KV:cache seq n n n n -- GPU:session KV:cache result<cuda-devptr,n> )
   {: layer:n tok:n head:n kind:n :}
   SEQ-PARTS {: cid:n slot:n gen:n :}
   KC-TAKE {: h:ptr :}
   h cid slot gen layer tok head kind 0 0 [: SPAN-TRY ;] catch
   {: rh:ptr rcid:n rslot:n rgen:n rlayer:n rtok:n rhead:n rkind:n off:n len:n code:n :}
   rh drop rcid drop rslot drop rgen drop
   rlayer drop rtok drop rhead drop rkind drop
   code 0<> if h KC-MINT code RESULT:ERR exit then
   off CAD-OFF len CAD-LEN GPU:SPAN h SPAN-RESULT ;

: K-SPAN
   ( GPU:session KV:cache seq n n n -- GPU:session KV:cache result<cuda-devptr,n> )
   0 SPAN-INNER ;

: V-SPAN
   ( GPU:session KV:cache seq n n n -- GPU:session KV:cache result<cuda-devptr,n> )
   1 SPAN-INNER ;

;package
