\ layout-box.f - runtime record arena for the boxed layout policy (TFAM 16 boxed
\ sub-slice 2; docs/type-families.md §22.4/§22.2). A boxed ADT value is a POINTER
\ to a heap record laid out as [ tag | payload cell 0 .. payload cell (M-1) ] —
\ tag first, so a deref is a single load. Records are bump-allocated from a
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

0 constant BOX-TAG-CELL                 \ the tag occupies record cell 0
1 constant BOX-PAY-CELL0                \ payload cell i occupies record cell 1+i
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

: BOX-CHUNK ( n -- )                    \ grow a fresh chunk holding >= n cells (>= default)
   {: need:n :}
   need BOX-CHUNK-CELLS max {: n:n :}
   n >COUNT MEM-ALLOC-CELLS BOX-CUR!
   0 BOX-OFF !   n BOX-CAP ! ;

: BOX-ROOM? ( n -- bool )               \ do n more cells fit in the current chunk?
   BOX-OFF @ +  BOX-CAP @  <= ;

: BOX-BUMP ( n -- ptr a )               \ take n cells off the current chunk, return the base
   {: n:n :}
   BOX-CUR@ BOX-OFF @ cells +  {: p:ptr :}
   BOX-OFF @ n +  BOX-OFF !
   p ;

: BOX-CELLS ( n -- ptr a )              \ allocate n contiguous zero-filled cells from the arena
   {: n:n :}
   n BOX-ROOM? 0= IF n BOX-CHUNK THEN
   n BOX-BUMP ;

: BOX-RECORD-CELLS ( n -- n )           \ record size for a variant with m payload cells (+ tag)
   BOX-PAY-CELL0 + ;

: BOX-ALLOC ( n -- ptr a )              \ a fresh box record with room for m payload cells
   BOX-RECORD-CELLS BOX-CELLS ;

\ --- typed field access through a box pointer.
: BOX-TAG! ( n ptr a -- )               \ write the variant tag
   BOX-TAG-CELL cells +  ! ;
: BOX-DEREF-TAG ( ptr a -- n )          \ read the variant tag
   BOX-TAG-CELL cells +  @ ;

: BOX-PAY-ADDR ( n ptr a -- ptr a )     \ address of payload cell i
   {: i:n p:ptr :}
   p  BOX-PAY-CELL0 i +  cells + ;
: BOX-PAY! ( n n ptr a -- )             \ store val into payload cell i
   {: val:n i:n p:ptr :}
   val  i p BOX-PAY-ADDR  ! ;
: BOX-PAY@ ( n ptr a -- n )             \ read payload cell i
   BOX-PAY-ADDR @ ;
