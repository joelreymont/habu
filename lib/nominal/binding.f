\ binding.f - binding records and sorted chunks for the immutable nominal
\ collection (dot habu-add-immutable-nominal-9290a81f).
\
\ A binding is an append-only record [path-node-index, slot-cell] in the binding
\ pool; its public value is the opaque nominal `binding` over the pool index. The
\ slot is an opaque content cell the substrate does not interpret. A CHUNK is a
\ contiguous run of binding indices in the chunk arena, kept sorted-unique by the
\ canonical (path, slot) order. CHUNK-MERGE streams two sorted chunks into one -
\ this is the sorted-chunk streaming behind UNION: equal (path, slot) bindings
\ collapse to one (idempotent) and a shared path with disagreeing slots fails
\ closed with E-NOM-CONFLICT. Nothing here interns or publishes rows; that is the
\ row arena's concern.
\
\ Load after lib/nominal/path.f. Reopens package NOM.

require lib/nominal/path.f

package NOM
public

\ The published opaque nominal binding handle. External consumers name
\ NOM:binding directly in signatures (dot habu-export-public-nom-20170121). The
\ pool index and the audited mint/erase boundary stay private below.
NEWTYPE binding 0

private

create NOM-BIND-AR 4 cells allot           \ binding i -> cells [2i]=path-index [2i+1]=slot
create NOM-CHUNK-AR 4 cells allot           \ contiguous binding-index runs

variable MRG-SA  variable MRG-CA
variable MRG-SB  variable MRG-CB
variable MRG-I   variable MRG-J

TRUSTED: MINT-BINDING ( n -- binding ) ;    \ audited pool-index -> handle boundary
TRUSTED: BIND-IDX ( binding -- n ) ;        \ proof-erasure: handle -> pool index

: BIND-COUNT ( -- n )   NOM-BIND-AR AR-USED@ 2 / ;
: BIND-PATH@ ( n -- n ) {: b:n :}   NOM-BIND-AR b 2 * AR-CELL@ ;
: BIND-SLOT@ ( n -- a ) {: b:n :}   NOM-BIND-AR b 2 * 1 + AR-CELL@ ;

: BIND-CHECK-IDX ( n -- n ) {: b:n :}       \ bounds-check a raw pool index (for sibling files)
   b 0 < b BIND-COUNT >= or if E-NOM-HANDLE throw then
   b ;

: BIND-CHECK ( binding -- n )
   BIND-IDX BIND-CHECK-IDX ;

: BIND+ ( n a -- n ) {: pidx:n slot:n :}    \ ( path-index slot -- binding-index )
   pidx NOM-BIND-AR AR-PUSH {: c0:n :}
   slot NOM-BIND-AR AR-PUSH drop
   NOM-BIND-AR AR-SEAL-COMMIT
   c0 2 / ;

\ ---- canonical binding order (path root-first, then slot) --------------------
: BIND-CMP ( n n -- n ) {: ba:n bb:n :}
   ba BIND-PATH@ bb BIND-PATH@ PATH-CMP {: pc:n :}
   pc 0 <> if pc exit then
   ba BIND-SLOT@ bb BIND-SLOT@ {: sa:n sb:n :}
   sa sb < if -1 exit then
   sa sb > if 1 exit then
   0 ;

: BIND-LESS? ( a a -- bool )   BIND-CMP 0 < ;
: BIND-PATH-EQ? ( n n -- bool ) {: ba:n bb:n :}
   ba BIND-PATH@ bb BIND-PATH@ PATH-CMP 0= ;
: BIND-SLOT-EQ? ( n n -- bool ) {: ba:n bb:n :}
   ba BIND-SLOT@ bb BIND-SLOT@ = ;

\ ---- chunk arena -------------------------------------------------------------
: CHUNK-USED ( -- n )   NOM-CHUNK-AR AR-USED@ ;
: CHUNK-MARK ( -- n )   CHUNK-USED ;
: CHUNK@ ( n -- n ) {: c:n :}   NOM-CHUNK-AR c AR-CELL@ ;
: CHUNK+ ( n -- n )     NOM-CHUNK-AR AR-PUSH ;
: CHUNK-COMMIT ( -- )   NOM-CHUNK-AR AR-SEAL-COMMIT ;
: CHUNK-TRUNC ( n -- )  NOM-CHUNK-AR swap AR-TRUNC ;

\ ---- streaming two-way merge (the sorted-chunk stream behind UNION) -----------
: MERGE-A@ ( -- n )   MRG-SA @ MRG-I @ + CHUNK@ ;
: MERGE-B@ ( -- n )   MRG-SB @ MRG-J @ + CHUNK@ ;
: MERGE-TAKE-A ( -- )    MERGE-A@ CHUNK+ drop  MRG-I @ 1+ MRG-I ! ;
: MERGE-TAKE-B ( -- )    MERGE-B@ CHUNK+ drop  MRG-J @ 1+ MRG-J ! ;
: MERGE-TAKE-BOTH ( -- ) MERGE-A@ CHUNK+ drop  MRG-I @ 1+ MRG-I !  MRG-J @ 1+ MRG-J ! ;

: MERGE-ONE ( -- )
   MRG-I @ MRG-CA @ >= if MERGE-TAKE-B exit then
   MRG-J @ MRG-CB @ >= if MERGE-TAKE-A exit then
   MERGE-A@ MERGE-B@ BIND-PATH-EQ? if
      MERGE-A@ MERGE-B@ BIND-SLOT-EQ? 0= if E-NOM-CONFLICT throw then
      MERGE-TAKE-BOTH exit
   then
   MERGE-A@ MERGE-B@ BIND-CMP 0 < if MERGE-TAKE-A else MERGE-TAKE-B then ;

: CHUNK-MERGE ( n n n n -- n n )            \ ( startA cntA startB cntB -- newstart newcount )
   MRG-CB !  MRG-SB !  MRG-CA !  MRG-SA !
   0 MRG-I !  0 MRG-J !
   CHUNK-USED {: start:n :}
   begin MRG-I @ MRG-CA @ < MRG-J @ MRG-CB @ < or while
      MERGE-ONE
   repeat
   start  CHUNK-USED start - ;

: NOM-BIND-INIT ( -- )
   NOM-BIND-AR AR-INIT
   NOM-CHUNK-AR AR-INIT ;

public

: BIND ( path a -- binding )               \ intern a (path, slot) content record
   swap PATH-CHECK swap
   BIND+ MINT-BINDING ;

private
NOM-BIND-INIT
;package
