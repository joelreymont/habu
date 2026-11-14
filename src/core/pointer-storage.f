\ pointer-storage.f - scratch storage and the global cells that hold an address.

\ Reserves n bytes at a cell-aligned `here`, rounded up to cells, as scratch
\ storage (docs/forth-card.md § 4). allot takes the cells before any is marked, so a size
\ that does not fit marks nothing; a negative size is asked of allot as the largest one.
: SCRATCH-ALLOT ( n -- )
   dup 0 < if drop -1 1 rshift then
   7 + 3 rshift  here over cells allot
   swap 0 ?do dup i cells + scratch-cell-mark loop drop ;

\ The clauses are empty on purpose: an empty clause declares the effect only, and
\ the engine leaves the word the body and DKIND:ADDR stamp `create` gave it
\ (src/habu/t0.f EMPTY-CLAUSE), so a read costs one load (`0 ptr-field` cost a
\ call, a branch and a frame).
: SCRATCH-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ptr a ) ;

: PTR-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ptr ptr a ) ;

\ The DATA cell at an offset the engine layout reserves, declared as a cell that
\ holds a byte pointer. `ptr-field` (src/habu/prims.f) is the declared door
\ from an address to a pointer cell, so no cast stands behind this word. It is
\ a public colon word with no checker row, so the sealed checker refuses a
\ checked call of it, and checked source reaches it only through the
\ RESERVED-PTR-U8-CELL definer below. The null, NULL-PTR, is an
\ engine primitive (src/habu/prims.f) and needs no cell.
package POINTER-STORAGE
public
: DATA-CELL ( n -- ptr ptr u8 ) data-base swap + 0 ptr-field ;
\ The DATA cell holding the newest datum's address, 0 before the first.
$3820 constant DATUM-CELL
;package

\ Starts a nameless row laid at top level: a build cuts the data from here to
\ the next definition or datum as an extent of its own. The scratch first cell
\ holds the previous datum's address and DATUM-CELL the newest, so a build
\ finds each datum it lays.
: datum ( -- )
   align here CELL-VIEW {: at:ptr :}
   1 cells SCRATCH-ALLOT
   data-base POINTER-STORAGE:DATUM-CELL + 0 ptr-field {: newest:ptr :}
   newest @  at 0 ptr-field !
   at newest ! ;

\ Scratch slots (PTR-VARIABLE, PTR-U8-TABLE, PTR-U8-TABLE-VARIABLE) are saved as null; a slot
\ whose value must survive an image or capture uses this definer.
: PERSISTED-PTR-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) ;

\ ---- a cell whose pointee the declaration chooses -----------------------------
\ The clause names the pointee. `create` publishes through `trust-raw`, which
\ mints every type variable TVK-RAW, and a raw pointee absorbs a scalar and
\ refuses a pointer: the definers above never hold the address of an address, so
\ neither can hold a pointer table's base. A spelled-out pointee has no variable,
\ so the cell's type is exactly what the clause says.
\
\ These live here, not beside TYPED-VARIABLE (src/core/layout-buffer.f), because
\ that form lays its words through the engine's child cell, filled only after the
\ boot prefix; a clause is compiled, so it works in the boot prefix,
\ where the checker's own pointer tables are declared. A new definer needs two
\ rows, because a definer written before the checker exists is invisible to it:
\ src/core/cell-effects.f states its own effect (without it the sealed checker
\ refuses a checked call of the name), and
\ src/habu/verify-source.f states what it publishes for the created word.

\ A table of byte pointers: `n` cells, each holding a `ptr u8`. `n PTR-U8-TABLE
\ NAME` publishes NAME as the table's BASE, so `i cells NAME + 0 ptr-field` takes
\ the field of a declared pointer cell rather than of raw dictionary storage.
: PTR-U8-TABLE ( n -- )
   create cells SCRATCH-ALLOT does> ( -- ptr ptr u8 ) ;

\ A PTR-U8-TABLE whose cells keep their addresses across a save.
: PERSISTED-PTR-U8-TABLE ( n -- )
   create dup 0 ?do here i cells + ptr-cell-mark loop cells allot does> ( -- ptr ptr u8 ) ;

\ A persisted cell holding a PTR-U8-TABLE's base: the head of an arena that
\ starts in a boot table and is repointed at a grown allocation.
: PERSISTED-PTR-U8-TABLE-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr ptr u8 ) ;

: PTR-U8-TABLE-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ptr ptr ptr u8 ) ;

\ `off RESERVED-PTR-U8-CELL NAME` publishes NAME as the address of the DATA cell
\ the engine layout reserves at `off`, declared `ptr ptr u8`. The offset goes
\ through POINTER-STORAGE:DATA-CELL on every read, so no absolute address is
\ baked. This definer has both rows, so the seal leaves its name alone;
\ test/pointer-storage-test.f measures its effect.
: RESERVED-PTR-U8-CELL ( n -- )
   create , does> ( -- ptr ptr u8 ) @ POINTER-STORAGE:DATA-CELL ;
