\ layout-box.f - runtime record arena for the boxed layout policy (TFAM 16 boxed
\ sub-slice 2; docs/type-families.md §22.4/§22.2). A boxed ADT value is a POINTER
\ to a public record laid out as [ tag | payload cell 0 .. payload cell (M-1) ] —
\ tag first, so a deref is a single load. One hidden capacity cell immediately
\ precedes that public pointer and makes every payload access bounds-checked.
\ Records are bump-allocated from a
\ grow-only arena of mmap chunks (the cell-typed MEM-ALLOC-CELLS member of the
\ MEM-ALLOC mmap family). There is no per-node free: the platform has no MEM-FREE,
\ so ownership is arena / free-all (BOX-ARENA-RESET drops every box by leaking the
\ chunks and forcing a fresh one). That keeps the boxed policy decoupled from the
\ linear/destructor system. This is the reusable RUNTIME half of boxed lowering:
\ the later coupled ctor/MATCH codegen emits calls to BOX-ALLOC / BOX-TAG! /
\ BOX-PAY! / BOX-DEREF-TAG / BOX-PAY@. It performs NO POLICY accept, NO checker
\ width change, and NO codegen.
\
\ Load after lib/errors.f and lib/memory.f.

require lib/errors.f
require lib/memory.f

0 constant BOX-TAG-CELL                 \ the tag occupies public record cell 0
1 constant BOX-PAY-CELL0                \ payload cell i occupies public record cell 1+i
-1 constant BOX-CAP-CELL                \ hidden payload capacity precedes the public record
2 constant BOX-HEAD-CELLS               \ hidden capacity + public tag
MEM-64K 1 cells / constant BOX-CHUNK-CELLS   \ default arena chunk = one 64K page of cells

\ --- arena state. CUR is a pointer (held via ptr-field, json-write's idiom);
\ OFF/CAP are plain cell counts.
variable BOX-CUR                        \ current chunk base pointer (0 until first alloc)
variable BOX-OFF                        \ next free cell offset within the current chunk
variable BOX-CAP                        \ current chunk capacity in cells

: BOX-CUR-FIELD ( -- ptr ptr a )   BOX-CUR 0 ptr-field ;
: BOX-CUR@ ( -- ptr a )   BOX-CUR-FIELD @ ;
: BOX-CUR! ( ptr a -- )   BOX-CUR-FIELD ! ;

\ BOX-ARENA-RESET drops every live box: cap 0 forces the next allocation to grow a
\ fresh (zero-filled) chunk; the old chunks leak (grow-only arena, no MEM-FREE).
: BOX-ARENA-RESET ( -- )
   0 BOX-OFF !   0 BOX-CAP ! ;
BOX-ARENA-RESET

: BOX-CELL-COUNT-CHECK ( n -- )
   >COUNT MEM-CHECK-CELL-COUNT ;

: BOX-CHUNK ( n -- )                    \ grow a fresh chunk holding >= n cells (>= default)
   {: need:n :}
   need BOX-CELL-COUNT-CHECK
   need BOX-CHUNK-CELLS max {: n:n :}
   n >COUNT MEM-ALLOC-CELLS BOX-CUR!
   0 BOX-OFF !   n BOX-CAP ! ;

: BOX-ROOM? ( n -- bool )               \ do n more cells fit in the current chunk?
   {: n:n :}
   n BOX-CELL-COUNT-CHECK
   n BOX-CAP @ BOX-OFF @ - <= ;

: BOX-BUMP ( n -- ptr a )               \ take n cells off the current chunk, return the base
   {: n:n :}
   n BOX-ROOM? 0= IF E-MEM-SIZE throw THEN
   BOX-CUR@ BOX-OFF @ cells +  {: p:ptr :}
   BOX-OFF @ n +  BOX-OFF !
   p ;

: BOX-CELLS ( n -- ptr a )              \ allocate n contiguous zero-filled cells from the arena
   {: n:n :}
   n BOX-CELL-COUNT-CHECK
   n BOX-ROOM? 0= IF n BOX-CHUNK THEN
   n BOX-BUMP ;

: BOX-PAY-COUNT-CHECK ( n -- )
   dup 0 < IF E-MEM-SIZE throw THEN
   dup MEM-MAX-CELLS BOX-HEAD-CELLS - > IF E-MEM-SIZE throw THEN
   drop ;

: BOX-RECORD-CELLS ( n -- n )           \ allocation size: hidden capacity + tag + payload
   dup BOX-PAY-COUNT-CHECK
   BOX-HEAD-CELLS + ;

: BOX-ALLOC ( n -- ptr a )              \ a fresh box record with room for m payload cells
   {: m:n :}
   m BOX-RECORD-CELLS BOX-CELLS {: base:ptr :}
   m base !
   base 1 cells + ;

\ --- typed field access through a box pointer.
: BOX-TAG! ( n ptr a -- )               \ write the variant tag
   BOX-TAG-CELL cells +  ! ;
: BOX-DEREF-TAG ( ptr a -- n )          \ read the variant tag
   BOX-TAG-CELL cells +  @ ;

: BOX-PAY-CAP ( ptr a -- n )            \ payload capacity stored before the public record
   BOX-CAP-CELL cells +  @ ;

: BOX-PAY-ADDR ( n ptr a -- ptr a )     \ address of payload cell i
   {: i:n p:ptr :}
   i 0 < IF E-TBL-BOUNDS throw THEN
   i p BOX-PAY-CAP >= IF E-TBL-BOUNDS throw THEN
   p  BOX-PAY-CELL0 i +  cells + ;
: BOX-PAY! ( n n ptr a -- )             \ store val into payload cell i
   {: val:n i:n p:ptr :}
   val  i p BOX-PAY-ADDR  ! ;
: BOX-PAY@ ( n ptr a -- n )             \ read payload cell i
   BOX-PAY-ADDR @ ;
