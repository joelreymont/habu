\ maki/infer/kv-cache-test.f - the paged KV cache allocator, red-first.
\ Run: bin/hb --load maki/infer/kv-cache-test.f
\
\ Proves, both ways: page geometry derives from the config record (no magic numbers);
\ append fills a page then allocates EXACTLY ONE page across a boundary; the pool
\ rejects exhaustion by name (E-KV-POOL); free returns pages and they recycle;
\ double-free / use-after-free / disposed-use / re-init are named rejects; FORK shares
\ pages byte-identically and diverges ONLY on a copy-on-write of the tail page; and a
\ random alloc/append/fork/free churn keeps the watermark exact (== distinct referenced
\ pages) with the refcount / free-list invariant intact every round.

require lib/test.f
require lib/property.f
require maki/infer/kv-cache.f

package KV

create KVT-CACHE HDR-BYTES allot        \ one caller-embedded cache header, reused per case

\ ---- shared setup: dispose (no-op if dead) then config + init ----------------------
: KVT-INIT ( n n n n n -- )             \ nkv hdim dbytes npages nseq
   KVT-CACHE DISPOSE
   CONFIG  KVT-CACHE swap INIT ;

\ ---- geometry: every size is derived from the config dims --------------------------
: KVT-GEOM ( -- )
   2 4 2 8 4 KVT-INIT                    \ tok = 2*4*2*2 = 32 ; page = 16*32 = 512
   PAGE-TOKENS 16 T=
   KVT-CACHE TOK-BYTES 32 T=
   KVT-CACHE PAGE-BYTES 512 T=
   KVT-CACHE NUM-PAGES 8 T=
   KVT-CACHE WATERMARK 0 T=
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

\ ---- append fills a page, then a boundary allocates EXACTLY ONE page ----------------
: KVT-BOUNDARY ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   16 0 ?do  KVT-CACHE s APPEND-TOKEN  loop      \ fill one page exactly
   KVT-CACHE s SEQ-LEN 16 T=
   KVT-CACHE s SEQ-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE s APPEND-TOKEN                       \ token 17 crosses the page boundary
   KVT-CACHE s SEQ-PAGES 2 T=
   KVT-CACHE WATERMARK 2 T=                       \ exactly ONE new page
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

\ ---- free returns pages; a later alloc recycles them --------------------------------
: KVT-RECYCLE ( -- )
   3 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   17 0 ?do  KVT-CACHE s APPEND-TOKEN  loop       \ 2 pages
   KVT-CACHE WATERMARK 2 T=
   KVT-CACHE CHECK
   KVT-CACHE s FREE-SEQ
   KVT-CACHE WATERMARK 0 T=                        \ both pages returned
   KVT-CACHE CHECK
   KVT-CACHE ALLOC-SEQ {: s2:kvseq :}
   KVT-CACHE s2 APPEND-TOKEN
   KVT-CACHE WATERMARK 1 T=                        \ recycled page reused
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

\ ---- FORK shares byte-identically, then copy-on-write of the tail page diverges -----
: KVT-COW ( -- )
   2 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: p:kvseq :}
   KVT-CACHE p APPEND-TOKEN  11 KVT-CACHE p 0 TOKEN-PTR c!
   KVT-CACHE p APPEND-TOKEN  22 KVT-CACHE p 1 TOKEN-PTR c!
   KVT-CACHE p APPEND-TOKEN  33 KVT-CACHE p 2 TOKEN-PTR c!
   KVT-CACHE p SEQ-PAGES 1 T=
   KVT-CACHE WATERMARK 1 T=
   KVT-CACHE p 0 SEQ-PAGE {: tail:n :}
   \ fork: child shares the one (partially-filled) tail page byte-identically
   KVT-CACHE p FORK-SEQ {: c:kvseq :}
   KVT-CACHE c SEQ-LEN 3 T=
   KVT-CACHE c SEQ-PAGES 1 T=
   KVT-CACHE c 0 SEQ-PAGE tail T=                  \ SAME page id
   KVT-CACHE tail PAGE-REFC 2 T=                   \ shared -> refc 2
   KVT-CACHE WATERMARK 1 T=                        \ fork allocates NO new page
   KVT-CACHE c 0 TOKEN-PTR c@ 11 T=               \ child sees the parent's bytes
   KVT-CACHE c 1 TOKEN-PTR c@ 22 T=
   KVT-CACHE c 2 TOKEN-PTR c@ 33 T=
   \ child appends into the shared tail page -> copy-on-write of exactly that page
   KVT-CACHE c APPEND-TOKEN
   KVT-CACHE c 0 SEQ-PAGE {: newp:n :}
   newp tail <> TTRUE                              \ child's tail page was copied
   KVT-CACHE p 0 SEQ-PAGE tail T=                  \ parent stays on the old page
   KVT-CACHE tail PAGE-REFC 1 T=                   \ old page now uniquely the parent's
   KVT-CACHE newp PAGE-REFC 1 T=
   KVT-CACHE WATERMARK 2 T=                        \ COW allocated exactly one page
   KVT-CACHE c 0 TOKEN-PTR c@ 11 T=               \ copied prefix is byte-identical
   KVT-CACHE c 1 TOKEN-PTR c@ 22 T=
   KVT-CACHE c 2 TOKEN-PTR c@ 33 T=
   99 KVT-CACHE c 3 TOKEN-PTR c!                   \ child's new token
   \ divergence: overwrite the parent's slot 0; the child's copy is unaffected
   55 KVT-CACHE p 0 TOKEN-PTR c!
   KVT-CACHE p 0 TOKEN-PTR c@ 55 T=
   KVT-CACHE c 0 TOKEN-PTR c@ 11 T=               \ still the copied value
   KVT-CACHE c 3 TOKEN-PTR c@ 99 T=
   KVT-CACHE p SEQ-LEN 3 T=                        \ parent length frozen at fork point
   KVT-CACHE c SEQ-LEN 4 T=
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

\ ---- named-reject probes (each re-inits, then provokes exactly one throw) -----------
: KVT-BAD-CONFIG ( -- )   0 4 2 8 4 CONFIG drop ;              \ nkv 0 -> E-KV-CONFIG
: KVT-EXHAUST ( -- )                                          \ 2-page pool, force a 3rd
   3 4 2 2 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   33 0 ?do  KVT-CACHE s APPEND-TOKEN  loop ;                 \ token 33 needs page 3 -> E-KV-POOL
: KVT-SEQS-FULL ( -- )                                        \ 2 seq slots, ask for a 3rd
   3 4 2 8 2 KVT-INIT
   KVT-CACHE ALLOC-SEQ drop  KVT-CACHE ALLOC-SEQ drop
   KVT-CACHE ALLOC-SEQ drop ;                                  \ -> E-KV-SEQS
: KVT-DOUBLE-FREE ( -- )
   3 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   KVT-CACHE s APPEND-TOKEN
   KVT-CACHE s FREE-SEQ
   KVT-CACHE s FREE-SEQ ;                                      \ second free -> E-KV-SEQ
: KVT-USE-AFTER-FREE ( -- )
   3 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   KVT-CACHE s FREE-SEQ
   KVT-CACHE s SEQ-LEN drop ;                                  \ -> E-KV-SEQ
: KVT-DISPOSED-USE ( -- )
   3 4 2 8 4 KVT-INIT
   KVT-CACHE DISPOSE
   KVT-CACHE ALLOC-SEQ drop ;                                  \ -> E-KV-STATE
: KVT-REINIT ( -- )
   3 4 2 8 4 KVT-INIT
   3 4 2 8 4 CONFIG  KVT-CACHE swap INIT ;                     \ re-init live -> E-KV-STATE
: KVT-BAD-TOKEN ( -- )
   3 4 2 8 4 KVT-INIT
   KVT-CACHE ALLOC-SEQ {: s:kvseq :}
   KVT-CACHE s APPEND-TOKEN
   KVT-CACHE s 5 TOKEN-PTR drop ;                              \ token 5 of a 1-token seq -> E-KV-BOUNDS

\ ---- churn: watermark exactness + invariant under random rounds --------------------
8 constant KVT-ACT-CAP                   \ == nseq below; active-handle capacity
create KVT-ACT KVT-ACT-CAP cells allot   \ live handles (raw slot ids), test-managed
variable KVT-NACT

: KVT-ACT@ ( n -- kvseq )  cells KVT-ACT + @ >KVSEQ ;
: KVT-ACT! ( kvseq n -- ) {: sq:kvseq i:n :}  sq KVSEQ>N  i cells KVT-ACT +  ! ;
: KVT-ACT-PUSH ( kvseq -- )  KVT-NACT @ KVT-ACT!  1 KVT-NACT +! ;
: KVT-ACT-DROP ( n -- ) {: i:n :}                            \ swap the last handle into slot i
   KVT-NACT @ 1- {: last:n :}
   last KVT-ACT@  i KVT-ACT!
   -1 KVT-NACT +! ;

: KVT-SEQ-HAS? ( kvseq n -- bool ) {: sq:kvseq pid:n :}      \ does seq's block table hold pid
   false
   KVT-CACHE sq SEQ-PAGES 0 ?do
      KVT-CACHE sq i SEQ-PAGE pid = if drop true then
   loop ;
: KVT-CONTAINS? ( n -- bool ) {: pid:n :}                    \ any active seq references pid
   false
   KVT-NACT @ 0 ?do
      i KVT-ACT@ pid KVT-SEQ-HAS? if drop true then
   loop ;
: KVT-DISTINCT ( -- n )                                       \ distinct referenced pages, refc-independent
   0
   KVT-CACHE NUM-PAGES 0 ?do
      i KVT-CONTAINS? if 1+ then
   loop ;

: KVT-DO-ALLOC ( -- )
   KVT-NACT @ KVT-ACT-CAP < if KVT-CACHE ALLOC-SEQ KVT-ACT-PUSH then ;
: KVT-DO-APPEND ( -- )
   KVT-NACT @ 0 > if
      KVT-NACT @ PROP:RND% KVT-ACT@ {: sq:kvseq :}
      KVT-CACHE sq APPEND-TOKEN
   then ;
: KVT-DO-FORK ( -- )
   KVT-NACT @ 0 >  KVT-NACT @ KVT-ACT-CAP <  and if
      KVT-NACT @ PROP:RND% KVT-ACT@ {: sq:kvseq :}
      KVT-CACHE sq FORK-SEQ KVT-ACT-PUSH
   then ;
: KVT-DO-FREE ( -- )
   KVT-NACT @ 0 > if
      KVT-NACT @ PROP:RND% {: i:n :}
      KVT-CACHE i KVT-ACT@ FREE-SEQ
      i KVT-ACT-DROP
   then ;
: KVT-CHURN-OP ( -- )
   4 PROP:RND% case
      0 of KVT-DO-ALLOC endof
      1 of KVT-DO-APPEND endof
      2 of KVT-DO-FORK endof
      3 of KVT-DO-FREE endof
   endcase ;
: KVT-CHURN-ROUND ( -- )
   KVT-CHURN-OP
   KVT-CACHE CHECK                                            \ refc == recomputed refs, free-list sane
   KVT-CACHE WATERMARK  KVT-DISTINCT  T= ;                    \ watermark == distinct referenced pages
: KVT-DRAIN ( -- )                                            \ free every remaining handle
   begin KVT-NACT @ 0 > while
      KVT-CACHE 0 KVT-ACT@ FREE-SEQ
      0 KVT-ACT-DROP
   repeat ;
: KVT-CHURN ( -- )
   3 4 2 256 KVT-ACT-CAP KVT-INIT
   0 KVT-NACT !
   1 150 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do  KVT-CHURN-ROUND  loop
   KVT-CACHE CHECK
   KVT-DRAIN
   KVT-CACHE WATERMARK 0 T=                                   \ every page returned
   KVT-CACHE CHECK
   KVT-CACHE DISPOSE ;

\ ---- run -----------------------------------------------------------------------------
T-RESET

KVT-GEOM
KVT-BOUNDARY
KVT-RECYCLE
KVT-COW

' KVT-BAD-CONFIG    E-KV-CONFIG TTHROWS
' KVT-EXHAUST       E-KV-POOL   TTHROWS
' KVT-SEQS-FULL     E-KV-SEQS   TTHROWS
' KVT-DOUBLE-FREE   E-KV-SEQ    TTHROWS
' KVT-USE-AFTER-FREE E-KV-SEQ   TTHROWS
' KVT-DISPOSED-USE  E-KV-STATE  TTHROWS
' KVT-REINIT        E-KV-STATE  TTHROWS
' KVT-BAD-TOKEN     E-KV-BOUNDS TTHROWS

KVT-CHURN

T-REPORT

;package
