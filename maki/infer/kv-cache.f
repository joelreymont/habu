\ maki/infer/kv-cache.f - host paged KV cache allocator for the GB10 engine.
\
\ This module owns the mutable HOST allocator: one coherent byte pool, page
\ refcounts, per-sequence block tables, generation-bearing sequence handles,
\ copy-on-write prefix forks, physical allocator metrics, and max-context page
\ reservations. The device never reads these mutable tables. Immutable device
\ publication is the separate habu-lease-kv-snapshot-9ef40f19 child.
\
\ A sequence handle is the structured product (cache identity, slot, slot
\ generation). Cache identities and slot generations advance monotonically and
\ reject at their ceiling instead of wrapping. Reusing a slot, rebuilding a cache
\ in the same header, or passing a handle to another cache therefore fails closed.
\
\ Reservation is owned state, not a fit predicate. ALLOC-SEQ/MAX reserves the
\ physical pages needed by the declared maximum context. Ordinary appends may use
\ only free pages not reserved by another sequence. A reserved append consumes its
\ own reservation when it takes a page; cancellation releases every unused page.
\ Forking a partially occupied reserved tail first reserves the possible copy-on-
\ write page, so the parent's maximum-context guarantee survives sharing.
\ The scheduler is the sole mutation owner; it serializes allocator calls while
\ any number of admitted sequences retain independent outstanding reservations.
\
\ All configuration, metadata, page, and ceiling calculations use pre-multiply /
\ pre-add checks against KV-MAX-N. Ceiling division is quotient-plus-remainder and
\ never forms n+d-1. Allocation, fork and append perform every fallible preflight
\ before ownership mutation. Cancellation commits logical death first, then uses
\ consume-before-release vector disposal; a release failure cannot make a stale
\ handle live again or double-release pages on retry.

require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/type/deftype.f

-5622 constant E-KV-CONFIG      \ invalid or overflowing configuration
-5623 constant E-KV-STATE       \ uninitialized/disposed cache or live re-init
-5624 constant E-KV-POOL        \ no physical page is free
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

\ The nominal fields are private representation roles. The product is public so
\ callers can transport a sequence value, but only allocator operations mint the
\ three field values from raw state.
PRODUCT kvseq 0
   FIELD cache kv-cache-id
   FIELD slot kv-seq-slot
   FIELD gen kv-seq-gen
;PRODUCT

PRODUCT kvcfg 0
   FIELD nkv n
   FIELD hdim n
   FIELD dbytes n
   FIELD npages n
   FIELD nseq n
   FIELD ptok n
;PRODUCT

private

$7FFFFFFFFFFFFFFF constant KV-MAX-N
KV-MAX-N constant KV-ID-MAX
16 constant KV-P
8 constant KV-BLK-CAP0

\ Header: pointer fields occupy one cell. POOLCAP is the single ownership token
\ for the complete cache and is written last after both mappings and the cache id.
0  constant POOL-CX
2  constant META-CX
1  cells constant POOLCAP-OFF
3  cells constant METAC-OFF
4  cells constant NPAGES-OFF
5  cells constant NSEQ-OFF
6  cells constant PAGEB-OFF
7  cells constant TOKB-OFF
8  cells constant FREETOP-OFF
9  cells constant NKV-OFF
10 cells constant HDIM-OFF
11 cells constant DBYTES-OFF
12 cells constant PTOK-OFF
13 cells constant HIWATER-OFF
14 cells constant CACHEID-OFF
15 cells constant RESERVED-OFF
16 cells constant HDR-SIZE

variable KV-NEXT-CACHE-ID

: POOL-FIELD ( ptr a -- ptr ptr u8 )  POOL-CX ptr-field ;
: POOL@ ( ptr a -- ptr u8 )  POOL-FIELD @ ;
: POOL! ( ptr u8 ptr a -- ) {: p:ptr h:ptr :}  p h POOL-FIELD ! ;
: META-FIELD ( ptr a -- ptr ptr a )  META-CX ptr-field ;
: META@ ( ptr a -- ptr a )  META-FIELD @ ;
: META! ( ptr a ptr a -- ) {: m:ptr h:ptr :}  m h META-FIELD ! ;
: KV-NULL ( -- ptr a )  NULL$ drop ;

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

: KV-CALC-TOK ( n n n -- n ) {: nkv:n hdim:n dbytes:n :}
   nkv hdim KV-MUL0 2 KV-MUL0 dbytes KV-MUL0 ;

: KV-SIZE ( n n n n n -- n n ) {: nkv:n hdim:n dbytes:n npages:n ptok:n :}
   nkv hdim dbytes KV-CALC-TOK {: tokb:n :}
   ptok tokb KV-MUL0 {: pageb:n :}
   npages pageb KV-MUL0 drop
   npages ptok KV-MUL0 drop
   pageb tokb ;

: KV-META-CELLS ( n n -- n ) {: np:n ns:n :}
   np 3 KV-MUL0
   ns 5 KV-MUL0 KV-ADD0
   ns VEC-HEADER-CELLS KV-MUL0 KV-ADD0
   dup 1 cells KV-MUL0 drop ;

: KV-PAGES-FOR-RAW ( n n -- n ) {: toks:n ptok:n :}
   toks KV-NONNEG drop  ptok KV-POS drop
   toks ptok /  toks ptok mod 0<> if 1 KV-ADD0 then ;

: KV-POOL-BYTES ( ptr a -- n ) {: h:ptr :}
   h NPAGES-OFF H@ h PAGEB-OFF H@ KV-MUL0 ;

\ ---- typed vector count/index adapters ---------------------------------------------
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

\ ---- metadata partition ------------------------------------------------------------
\ [free np][refcount np][COW reservation np][len ns][live ns]
\ [generation ns][future-page reservation ns][maximum tokens ns][VEC headers ns].
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

: SEQBLK-BASE ( ptr a -- ptr a ) {: h:ptr :}
   h SEQMAX-BASE h NSEQ-OFF H@ cells + ;

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

: SEQBLK ( ptr a n -- ptr a ) {: h:ptr i:n :}
   h SEQBLK-BASE i VEC-HEADER-CELLS KV-MUL0 cells + ;

\ ---- block-table operations --------------------------------------------------------
: KV-BLK-LEN ( ptr a n -- n ) {: h:ptr s:n :}
   h s SEQBLK VEC-LEN@ LEN>N ;

: KV-BLK@ ( ptr a n n -- n ) {: h:ptr s:n i:n :}
   h s SEQBLK i KV>INDEX VEC:@ ;

: KV-BLK! ( n ptr a n n -- ) {: v:n h:ptr s:n i:n :}
   v h s SEQBLK i KV>INDEX VEC:! ;

: KV-BLK-PUSH ( n ptr a n -- ) {: v:n h:ptr s:n :}
   v h s SEQBLK VEC:PUSH drop ;

: KV-BLK-ENSURE+ ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-BLK-LEN 1 KV-ADD0 KV>ITEM h s SEQBLK swap VEC:ENSURE ;

\ ---- cache and structured-handle validation ---------------------------------------
: KV-LIVE-CK ( ptr a -- ) {: h:ptr :}
   h POOLCAP-OFF H@ 0= if E-KV-STATE throw then ;

: KV-MAKE-HANDLE ( ptr a n -- kvseq ) {: h:ptr s:n :}
   h CACHEID-OFF H@ >KV-CACHE-ID
   s >KV-SEQ-SLOT
   h s SEQGEN@ >KV-SEQ-GEN
   KV-KVSEQ:MAKE ;

: KV-SEQ-PARTS ( kvseq -- n n n )
   KV-KVSEQ:UNMAKE {: cid:kv-cache-id slot:kv-seq-slot gen:kv-seq-gen :}
   cid KV-CACHE-ID>N slot KV-SEQ-SLOT>N gen KV-SEQ-GEN>N ;

: KV-SEQ-CK ( ptr a kvseq -- ptr a n )
   KV-SEQ-PARTS {: h:ptr cid:n s:n gen:n :}
   h KV-LIVE-CK
   cid h CACHEID-OFF H@ <> if E-KV-SEQ throw then
   s 0 < s h NSEQ-OFF H@ >= or if E-KV-SEQ throw then
   h s SEQLIVE@ 0= if E-KV-SEQ throw then
   gen h s SEQGEN@ <> if E-KV-SEQ throw then
   h s ;

: KV-LIVE-HANDLE? ( ptr a kvseq -- bool )
   KV-SEQ-PARTS {: h:ptr cid:n s:n gen:n :}
   h POOLCAP-OFF H@ 0= if false exit then
   cid h CACHEID-OFF H@ <> if false exit then
   s 0 < s h NSEQ-OFF H@ >= or if false exit then
   h s SEQLIVE@ 0<> gen h s SEQGEN@ = and ;

\ ---- free pages and reservation accounting ----------------------------------------
: KV-FILL-FREE ( ptr a -- ) {: h:ptr :}
   h NPAGES-OFF H@ {: np:n :}
   np 0 ?do i h i FREE! loop
   np h FREETOP-OFF H! ;

: KV-POP-PREFLIGHT ( ptr a -- ) {: h:ptr :}
   h FREETOP-OFF H@ 0 <= if E-KV-POOL throw then ;

: KV-POP-COMMIT ( ptr a -- n ) {: h:ptr :}
   h FREETOP-OFF H@ 1- {: top:n :}
   top h FREETOP-OFF H!
   h NPAGES-OFF H@ top - h HIWATER-OFF H@ max h HIWATER-OFF H!
   h top FREE@ ;

: KV-POP-FREE ( ptr a -- n ) {: h:ptr :}
   h KV-POP-PREFLIGHT
   h KV-POP-COMMIT ;

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

: KV-COW-RESERVE-ONE ( ptr a n -- ) {: h:ptr pid:n :}
   h pid COWRES@ 1- h pid COWRES!
   h 1 KV-GLOBAL-RESERVE- ;

: KV-TAKE-UNRESERVED ( ptr a -- n ) {: h:ptr :}
   h KV-POP-PREFLIGHT
   h KV-UNRESERVED 0 <= if E-KV-ADMIT throw then
   h KV-POP-FREE ;

: KV-GROW-PREFLIGHT ( ptr a n -- ) {: h:ptr s:n :}
   h KV-POP-PREFLIGHT
   h s SEQMAX@ 0 > if
      h s SEQRES@ 0 <= if E-KV-INVARIANT throw then
   else
      h KV-UNRESERVED 0 <= if E-KV-ADMIT throw then
   then ;

: KV-TAKE-GROW-COMMIT ( ptr a n -- n ) {: h:ptr s:n :}
   h KV-POP-COMMIT {: pid:n :}
   h s SEQMAX@ 0 > if h s KV-SEQ-RESERVE-ONE then
   pid ;

: KV-COW-GUARANTEED? ( ptr a n -- bool ) {: h:ptr s:n :}
   h s SEQMAX@ h s SEQLEN@ > ;

: KV-TAKE-RESERVED-COW ( ptr a n -- n ) {: h:ptr old:n :}
   h KV-POP-FREE
   h old KV-COW-RESERVE-ONE ;

: KV-TAKE-COW ( ptr a n n -- n ) {: h:ptr s:n old:n :}
   h s KV-COW-GUARANTEED? h old COWRES@ 0 > and if
      h old KV-TAKE-RESERVED-COW
   else
      h KV-TAKE-UNRESERVED
   then
   ;

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
: KV-PAGE-ADDR ( ptr a n -- ptr u8 ) {: h:ptr pid:n :}
   h POOL@ pid h PAGEB-OFF H@ KV-MUL0 + ;

: KV-GROW-PAGE ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-GROW-PREFLIGHT
   h s KV-BLK-ENSURE+
   h s KV-TAKE-GROW-COMMIT {: pid:n :}
   1 h pid REFC!
   pid h s KV-BLK-PUSH ;

: KV-COW-TAIL ( ptr a n -- ) {: h:ptr s:n :}
   h s KV-BLK-LEN 1- {: last:n :}
   h s last KV-BLK@ {: old:n :}
   h old REFC@ 1 <= if exit then
   h s old KV-TAKE-COW {: new:n :}
   h old KV-PAGE-ADDR h new KV-PAGE-ADDR h PAGEB-OFF H@ BYTE-COPY
   1 h new REFC!
   new h s last KV-BLK!
   h old KV-REF-RETURN ;

: KV-APPEND-LIMIT-CK ( n n -- ) {: len:n maxtoks:n :}
   maxtoks 0 > len maxtoks >= and if E-KV-ADMIT throw then ;

: KV-APPEND-TOKEN ( ptr a kvseq -- )
   KV-SEQ-CK {: h:ptr s:n :}
   h s SEQLEN@ {: len:n :}
   len h s SEQMAX@ KV-APPEND-LIMIT-CK
   len h PTOK-OFF H@ mod 0= if h s KV-GROW-PAGE else h s KV-COW-TAIL then
   len 1 KV-ADD0 h s SEQLEN! ;

\ ---- sequence allocation and fork transaction -------------------------------------
: KV-FIND-SLOT ( ptr a -- n ) {: h:ptr :}
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0= if i unloop exit then
   loop
   E-KV-SEQS throw ;

: KV-NEXT-SLOT-GEN ( ptr a n -- n ) {: h:ptr s:n :}
   h s SEQGEN@ dup KV-ID-MAX >= if drop E-KV-ID throw then 1+ ;

: KV-PREP-SLOT ( ptr a n -- n n ) {: h:ptr cap:n :}
   h KV-FIND-SLOT {: s:n :}
   h s KV-NEXT-SLOT-GEN {: gen:n :}
   h s SEQBLK cap KV>ITEM VEC:INIT
   s gen ;

: KV-COMMIT-SLOT ( ptr a n n n n -- kvseq ) {: h:ptr s:n gen:n pages:n maxtoks:n :}
   0 h s SEQLEN!
   pages h s SEQRES!
   maxtoks h s SEQMAX!
   h pages KV-GLOBAL-RESERVE+
   gen h s SEQGEN!
   1 h s SEQLIVE!                           \ commit last
   h s KV-MAKE-HANDLE ;

: KV-ALLOC-SEQ/RES ( ptr a n n -- kvseq ) {: h:ptr pages:n maxtoks:n :}
   h KV-LIVE-CK
   h pages KV-RESERVE-PREFLIGHT
   h KV-BLK-CAP0 KV-PREP-SLOT {: s:n gen:n :}
   h s gen pages maxtoks KV-COMMIT-SLOT ;

: KV-ALLOC-SEQ ( ptr a -- kvseq )
   0 0 KV-ALLOC-SEQ/RES ;

: KV-ALLOC-SEQ/MAX ( ptr a n -- kvseq ) {: h:ptr toks:n :}
   toks 0 <= if E-KV-ADMIT throw then
   toks h PTOK-OFF H@ KV-PAGES-FOR-RAW h swap toks KV-ALLOC-SEQ/RES ;

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

: KV-FORK-SEQ ( ptr a kvseq -- kvseq )
   KV-SEQ-CK {: h:ptr p:n :}
   h p KV-FORK-COW-EXTRA {: cowextra:n :}
   h p KV-FORK-EXTRA {: extra:n :}
   h extra KV-RESERVE-PREFLIGHT
   h p KV-BLK-LEN KV-BLK-CAP0 max {: cap:n :}
   h cap KV-PREP-SLOT {: c:n gen:n :}        \ only fallible allocation
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
   h c KV-MAKE-HANDLE ;

\ ---- cancellation: logical ownership is consumed before vector release ------------
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
   h s SEQBLK VEC:DISPOSE ;                   \ consume-before-release owner

: KV-CANCEL-SEQ ( ptr a kvseq -- )
   KV-SEQ-CK KV-CANCEL-SLOT ;

\ ---- queries and physical-page metrics --------------------------------------------
: KV-WATERMARK ( ptr a -- n ) {: h:ptr :}
   h KV-LIVE-CK h NPAGES-OFF H@ h FREETOP-OFF H@ - ;

: KV-SEQ-LEN ( ptr a kvseq -- n )
   KV-SEQ-CK SEQLEN@ ;

: KV-SEQ-PAGES ( ptr a kvseq -- n )
   KV-SEQ-CK KV-BLK-LEN ;

: KV-SEQ-PAGE ( ptr a kvseq n -- n ) {: i:n :}
   KV-SEQ-CK {: h:ptr s:n :}
   i 0 < i h s KV-BLK-LEN >= or if E-KV-BOUNDS throw then
   h s i KV-BLK@ ;

: KV-PAGE-REFC ( ptr a n -- n ) {: h:ptr pid:n :}
   h KV-LIVE-CK
   pid 0 < pid h NPAGES-OFF H@ >= or if E-KV-BOUNDS throw then
   h pid REFC@ ;

: KV-TOKEN-PTR ( ptr a kvseq n -- ptr u8 ) {: tok:n :}
   KV-SEQ-CK {: h:ptr s:n :}
   tok 0 < tok h s SEQLEN@ >= or if E-KV-BOUNDS throw then
   h PTOK-OFF H@ {: ptok:n :}
   tok ptok / {: pageix:n :}
   tok ptok mod {: slot:n :}
   h s pageix KV-BLK@ {: pid:n :}
   h pid KV-PAGE-ADDR slot h TOKB-OFF H@ KV-MUL0 + ;

: KV-SHARED-PAGES ( ptr a -- n ) {: h:ptr :}
   h KV-LIVE-CK
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
   h KV-LIVE-CK
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
      h s SEQRES@ 0<> h s SEQMAX@ 0<> or if E-KV-INVARIANT throw then
      0 exit
   then
   h s SEQGEN@ 0 <= if E-KV-INVARIANT throw then
   h s SEQLEN@ h s KV-BLK-LEN h PTOK-OFF H@ KV-MUL0 > if E-KV-INVARIANT throw then
   h s SEQMAX@ {: maxtoks:n :}
   maxtoks 0 > if
      h s SEQLEN@ maxtoks > if E-KV-INVARIANT throw then
      maxtoks h PTOK-OFF H@ KV-PAGES-FOR-RAW h s KV-BLK-LEN -
      h s SEQRES@ <> if E-KV-INVARIANT throw then
   then
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
   h KV-LIVE-CK
   h CACHEID-OFF H@ 0 <= if E-KV-INVARIANT throw then
   h KV-CHECK-REFS
   h KV-CHECK-FREE
   h KV-CHECK-RESERVATIONS ;

\ ---- cache install and cleanup ------------------------------------------------------
PTR-VARIABLE KV-IH
variable KV-CLEAN-RC

: KV-CLEAR-POINTERS ( ptr a -- ) {: h:ptr :}
   NULL$ drop h POOL!
   KV-NULL h META! ;

: KV-ALLOC-META ( ptr a -- ) {: h:ptr :}
   h METAC-OFF H@ MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS h META! ;

: KV-ALLOC-POOL ( ptr a -- ) {: h:ptr :}
   h KV-POOL-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop h POOL! ;

: KV-RELEASE-META ( ptr a -- ) {: h:ptr :}
   h META@ h METAC-OFF H@ >COUNT MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: KV-RELEASE-POOL ( ptr a -- ) {: h:ptr :}
   h POOL@ h KV-POOL-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: KV-INSTALL-BODY ( -- )
   KV-IH @ {: h:ptr :}
   h KV-ALLOC-META
   h KV-ALLOC-POOL
   h KV-FILL-FREE ;

: KV-CLEAN-META ( -- )  KV-IH @ KV-RELEASE-META ;
: KV-CLEAN-POOL ( -- )  KV-IH @ KV-RELEASE-POOL ;

: KV-RECORD-CLEAN ( n -- ) {: code:n :}
   code 0<> KV-CLEAN-RC @ 0= and if code KV-CLEAN-RC ! then ;

: KV-INIT-CLEANUP ( -- n )
   0 KV-CLEAN-RC !
   KV-IH @ {: h:ptr :}
   h POOL@ 0= 0= if [: KV-CLEAN-POOL ;] catch KV-RECORD-CLEAN then
   h META@ 0= 0= if [: KV-CLEAN-META ;] catch KV-RECORD-CLEAN then
   h KV-CLEAR-POINTERS
   0 h POOLCAP-OFF H!
   KV-CLEAN-RC @ ;

: KV-ID-PREFLIGHT ( -- )
   KV-NEXT-CACHE-ID @ KV-ID-MAX >= if E-KV-ID throw then ;

: KV-ID-COMMIT ( ptr a -- ) {: h:ptr :}
   KV-NEXT-CACHE-ID @ 1+ dup KV-NEXT-CACHE-ID ! h CACHEID-OFF H! ;

: KV-STORE-DIMS ( ptr a n n n n n n n n n -- )
   {: h:ptr nkv:n hdim:n dbytes:n npages:n nseq:n ptok:n pageb:n tokb:n metac:n :}
   nkv h NKV-OFF H!  hdim h HDIM-OFF H!  dbytes h DBYTES-OFF H!
   npages h NPAGES-OFF H!  nseq h NSEQ-OFF H!  ptok h PTOK-OFF H!
   pageb h PAGEB-OFF H!  tokb h TOKB-OFF H!  metac h METAC-OFF H!
   0 h HIWATER-OFF H!  0 h RESERVED-OFF H! ;

: KV-CONFIG-VALUES ( n n n n n n -- n n n )
   {: nkv:n hdim:n dbytes:n npages:n nseq:n ptok:n :}
   nkv KV-POS drop  hdim KV-POS drop  dbytes KV-POS drop
   npages KV-POS drop  nseq KV-POS drop  ptok KV-POS drop
   nkv hdim dbytes npages ptok KV-SIZE {: pageb:n tokb:n :}
   npages nseq KV-META-CELLS {: metac:n :}
   pageb tokb metac ;

\ typed-local-lint: allow-bare-local - cfg is the multi-cell KV:kvcfg product.
: KV-INIT ( ptr a KV:kvcfg -- ) {: h:ptr cfg :}
   h POOLCAP-OFF H@ 0<> if E-KV-STATE throw then
   cfg KV-KVCFG:UNMAKE {: nkv:n hdim:n dbytes:n npages:n nseq:n ptok:n :}
   nkv hdim dbytes npages nseq ptok KV-CONFIG-VALUES {: pageb:n tokb:n metac:n :}
   KV-ID-PREFLIGHT
   h KV-CLEAR-POINTERS
   h nkv hdim dbytes npages nseq ptok pageb tokb metac KV-STORE-DIMS
   h KV-IH !
   [: KV-INSTALL-BODY ;] catch {: code:n :}
   code 0<> if KV-INIT-CLEANUP drop code throw then
   h KV-ID-COMMIT
   h KV-POOL-BYTES h POOLCAP-OFF H! ;            \ ownership commit last

\ Dispose retains the first release error while consuming all logical owners.
PTR-VARIABLE KV-DH
PTR-VARIABLE KV-DPOOL
PTR-VARIABLE KV-DMETA
variable KV-DMC
variable KV-DPB
variable KV-DSLOT
variable KV-DISPOSE-RC

: KV-RECORD-DISPOSE ( n -- ) {: code:n :}
   code 0<> KV-DISPOSE-RC @ 0= and if code KV-DISPOSE-RC ! then ;

: KV-DISPOSE-SLOT-CUR ( -- )
   KV-DH @ KV-DSLOT @ KV-CANCEL-SLOT ;

: KV-DISPOSE-SEQS ( ptr a -- ) {: h:ptr :}
   h KV-DH !
   h NSEQ-OFF H@ 0 ?do
      h i SEQLIVE@ 0<> if
         i KV-DSLOT !
         [: KV-DISPOSE-SLOT-CUR ;] catch KV-RECORD-DISPOSE
      then
   loop ;

: KV-DISPOSE-META-CUR ( -- )
   KV-DMETA @ KV-DMC @ >COUNT MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: KV-DISPOSE-POOL-CUR ( -- )
   KV-DPOOL @ KV-DPB @ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: KV-DISPOSE ( ptr a -- ) {: h:ptr :}
   h POOLCAP-OFF H@ 0= if exit then
   0 KV-DISPOSE-RC !
   h KV-DISPOSE-SEQS
   h POOL@ KV-DPOOL !  h META@ KV-DMETA !
   h METAC-OFF H@ KV-DMC !  h KV-POOL-BYTES KV-DPB !
   0 h POOLCAP-OFF H!
   h KV-CLEAR-POINTERS
   [: KV-DISPOSE-POOL-CUR ;] catch KV-RECORD-DISPOSE
   [: KV-DISPOSE-META-CUR ;] catch KV-RECORD-DISPOSE
   KV-DISPOSE-RC @ dup 0<> if throw then drop ;

: KV-CONFIG ( n n n n n n -- KV:kvcfg )
   {: nkv:n hdim:n dbytes:n npages:n nseq:n ptok:n :}
   nkv hdim dbytes npages nseq ptok KV-CONFIG-VALUES drop drop drop
   nkv hdim dbytes npages nseq ptok KV-KVCFG:MAKE ;

public

: PAGE-TOKENS ( -- n )  KV-P ;
: HDR-BYTES ( -- n )  HDR-SIZE ;
: CONFIG ( n n n n n -- KV:kvcfg )  KV-P KV-CONFIG ;
: CONFIG/P ( n n n n n n -- KV:kvcfg )  KV-CONFIG ;

: INIT ( ptr a KV:kvcfg -- )  KV-INIT ;
: DISPOSE ( ptr a -- )  KV-DISPOSE ;

: ALLOC-SEQ ( ptr a -- kvseq )  KV-ALLOC-SEQ ;
: ALLOC-SEQ/MAX ( ptr a n -- kvseq )  KV-ALLOC-SEQ/MAX ;
: APPEND-TOKEN ( ptr a kvseq -- )  KV-APPEND-TOKEN ;
: FORK-SEQ ( ptr a kvseq -- kvseq )  KV-FORK-SEQ ;
: CANCEL-SEQ ( ptr a kvseq -- )  KV-CANCEL-SEQ ;

: WATERMARK ( ptr a -- n )  KV-WATERMARK ;
: FREE-PAGES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h FREETOP-OFF H@ ;
: RESERVED-PAGES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h RESERVED-OFF H@ ;
: SHARED-PAGES ( ptr a -- n )  KV-SHARED-PAGES ;
: TAIL-WASTE ( ptr a -- n )  KV-TAIL-WASTE ;
: HIGH-WATER ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h HIWATER-OFF H@ ;
: BYTES-PER-TOKEN ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h TOKB-OFF H@ ;

: SEQ-LEN ( ptr a kvseq -- n )  KV-SEQ-LEN ;
: SEQ-RESERVED ( ptr a kvseq -- n )
   KV-SEQ-CK SEQRES@ ;
: NUM-PAGES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h NPAGES-OFF H@ ;
: PAGE-SIZE ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h PTOK-OFF H@ ;
: PAGE-BYTES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h PAGEB-OFF H@ ;
: TOK-BYTES ( ptr a -- n ) {: h:ptr :}  h KV-LIVE-CK h TOKB-OFF H@ ;
: PAGES-FOR ( ptr a n -- n ) {: h:ptr toks:n :}
   h KV-LIVE-CK toks h PTOK-OFF H@ KV-PAGES-FOR-RAW ;

private

\ These views exist only for the owner-package invariant suite. In particular,
\ TOKEN-PTR cannot escape the cache lifetime. The later immutable publication
\ module reopens KV and reads the private ownership tables under its own lease.
: LIVE-CACHE? ( ptr a -- bool )  POOLCAP-OFF H@ 0<> ;
: SEQ-PAGES ( ptr a kvseq -- n )  KV-SEQ-PAGES ;
: SEQ-PAGE ( ptr a kvseq n -- n )  KV-SEQ-PAGE ;
: TOKEN-PTR ( ptr a kvseq n -- ptr u8 )  KV-TOKEN-PTR ;
: PAGE-REFC ( ptr a n -- n )  KV-PAGE-REFC ;
: CHECK ( ptr a -- )  KV-CHECK ;

;package
