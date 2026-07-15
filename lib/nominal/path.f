\ path.f - persistent consed lexical paths for the immutable nominal collection
\ (dot habu-add-immutable-nominal-9290a81f).
\
\ A path is a persistent list of opaque lexical atom cells with NO fixed depth:
\ CONS(parent, atom) appends one node [atom, parent-index] to the append-only path
\ arena and returns a fresh one-cell `path` handle over the new node's index. The
\ parent index IS the structural share - reusing a parent handle shares its whole
\ prefix, so building A/B/C then A/B/D shares the A/B spine. The substrate assigns
\ NO meaning to an atom (CAD-EFFECT defines atoms and legality separately); an atom
\ is just an opaque content cell, and the canonical order over paths is the raw
\ atom-cell order compared ROOT-FIRST, which is content-derived and therefore
\ allocation-order independent.
\
\ The public value is the opaque nominal `path`; the node index is never exposed.
\ MINT-PATH is the sole (private, audited) index->handle boundary and PATH-IDX its
\ proof-erasure inverse; a raw cell can never forge a path through the public API,
\ and a stale/out-of-range handle fails closed with E-NOM-HANDLE.
\
\ Load after lib/nominal/arena.f. Reopens package NOM.

require lib/nominal/arena.f

package NOM
public

\ The published opaque nominal path handle. External consumers name NOM:path
\ directly in signatures (dot habu-export-public-nom-20170121). The node index
\ and the audited mint/erase boundary stay private below.
TYPEFAMILY path 0

private

create NOM-PATH-AR 4 cells allot          \ node i -> cells [2i]=atom [2i+1]=parent-index
create NOM-CMPA 4 cells allot              \ scratch: path A materialised leaf-first
create NOM-CMPB 4 cells allot              \ scratch: path B materialised leaf-first
create NOM-RMP 4 cells allot              \ scratch: remap path-materialise
variable RMP-K

TRUSTED: MINT-PATH ( n -- path ) ;         \ audited node-index -> handle boundary
TRUSTED: PATH-IDX ( path -- n ) ;          \ proof-erasure: handle -> node index

: PATH-NODES ( -- n )   NOM-PATH-AR AR-USED@ 2 / ;
: PATH-ATOM@ ( n -- a ) {: nd:n :}   NOM-PATH-AR nd 2 * AR-CELL@ ;
: PATH-PARENT@ ( n -- n ) {: nd:n :}   NOM-PATH-AR nd 2 * 1 + AR-CELL@ ;

: PATH-NODE+ ( a n -- n )                  \ ( atom-cell parent-nodeidx -- new-nodeidx )
   {: parent:n :}
   NOM-PATH-AR AR-PUSH {: c0:n :}
   parent NOM-PATH-AR AR-PUSH drop
   NOM-PATH-AR AR-SEAL-COMMIT
   c0 2 / ;

: PATH-CHECK ( path -- n )                 \ project + bounds-check a handle
   PATH-IDX {: nd:n :}
   nd 0 < nd PATH-NODES >= or if E-NOM-HANDLE throw then
   nd ;

: PATH-MAT ( n ptr a -- ) {: ctl:ptr :}    \ materialise leaf-first into a scratch arena
   ctl AR-RESET
   begin dup 0 <> while
      dup PATH-ATOM@ ctl AR-PUSH drop
      PATH-PARENT@
   repeat
   drop ;

: MAT-CMP ( ptr a ptr a -- n ) {: ca:ptr cb:ptr :}   \ compare scratches root-first
   ca AR-USED@ {: da:n :}  cb AR-USED@ {: db:n :}
   da db min {: m:n :}
   0 begin dup m < while
      dup {: k:n :}
      ca da 1- k - AR-CELL@ {: xa:n :}
      cb db 1- k - AR-CELL@ {: xb:n :}
      xa xb < if drop -1 exit then
      xa xb > if drop 1 exit then
      1+
   repeat
   drop
   da db < if -1 exit then
   da db > if 1 exit then
   0 ;

: PATH-CMP ( n n -- n ) {: na:n nb:n :}    \ total canonical order over path node indices
   na NOM-CMPA PATH-MAT
   nb NOM-CMPB PATH-MAT
   NOM-CMPA NOM-CMPB MAT-CMP ;

: NOM-PATH-INIT ( -- )
   NOM-PATH-AR AR-INIT
   0 0 PATH-NODE+ drop                     \ node 0 = root sentinel
   NOM-CMPA AR-INIT
   NOM-CMPB AR-INIT
   NOM-RMP AR-INIT ;

public

: ROOT ( -- path )   0 MINT-PATH ;

: CONS ( path a -- path )                  \ extend a path by one opaque outer atom
   swap PATH-CHECK
   PATH-NODE+ MINT-PATH ;

: SEGS ( path -- n )                      \ segment count (diagnostic; content, not identity)
   PATH-CHECK
   0 begin over 0 <> while
      swap PATH-PARENT@ swap 1+
   repeat
   nip ;

private

\ Prefix one stable outer atom onto a node's path (row.f REMAP uses it); defined
\ after ROOT/CONS since it rebuilds through them, kept private to confine MINT-PATH.
: REMAP-PATH ( n a -- path )
   {: atom:n :}
   NOM-RMP PATH-MAT
   NOM-RMP AR-USED@ {: d:n :}
   ROOT atom CONS
   0 RMP-K !
   begin RMP-K @ d < while
      NOM-RMP d 1- RMP-K @ - AR-CELL@ CONS
      RMP-K @ 1+ RMP-K !
   repeat ;

NOM-PATH-INIT
;package
