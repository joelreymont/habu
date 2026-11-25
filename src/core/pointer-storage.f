\ pointer-storage.f - scratch storage and the global cells that hold an address.

\ Scratch storage (docs/forth-card.md § 4): n bytes, rounded up to cells, at an aligned `here`.
\ allot runs before any mark, so an unfit size marks nothing; a negative size asks allot for the largest.
: SCRATCH-ALLOT ( n -- )
   dup 0 < if drop -1 1 rshift then
   7 + 3 rshift  align here over cells allot
   swap 0 ?do dup i cells + scratch-cell-mark loop drop ;

\ The clauses are empty on purpose: an empty clause declares the effect only, and
\ the engine leaves the word the body and DKIND:ADDR stamp `create` gave it
\ (src/habu/t0.f EMPTY-CLAUSE), so a read costs one load (`0 ptr-field` cost a
\ call, a branch and a frame).
: SCRATCH-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ^a ) ;

: PTR-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ^^a ) ;

\ The DATA cell at an offset the engine layout reserves, declared as a cell that
\ holds a byte pointer. `ptr-field` (src/habu/prims.f) is the declared door
\ from an address to a pointer cell, so no cast stands behind this word. The
\ null, NULL-PTR, is an engine primitive (src/habu/prims.f) and needs no cell.
package POINTER-STORAGE
private
EXPORT BOOT:data-base
: DATA-CELL ( n -- ^^u8 ) data-base swap + 0 ptr-field ;
\ `off RESERVED-PTR-U8-CELL NAME`: NAME gives the DATA cell the engine layout
\ reserves at `off`, found from data-base on each read, never baked absolute.
: RESERVED-PTR-U8-CELL ( n -- )
   create , does> ( -- ^^u8 ) @ DATA-CELL ;
public
\ The DATA cell holding the newest datum's address, 0 before the first.
$3820 constant DATUM-CELL
\ Aligns here past one byte, so the datum laid next never starts where the data
\ before it ends: an address where data ends lies in that data's extent.
: ALIGN-APART ( -- ) 1 allot align ;
global
\ Starts a nameless row at top level, an extent of its own up to the next
\ definition or datum; its scratch first cell links the datums from DATUM-CELL.
: datum ( -- )
   POINTER-STORAGE:ALIGN-APART here CELL-VIEW {: at:^ :}
   1 cells SCRATCH-ALLOT
   data-base POINTER-STORAGE:DATUM-CELL + 0 ptr-field {: newest:^ :}
   newest @  at 0 ptr-field !
   at newest ! ;
;package

\ Scratch slots (PTR-VARIABLE, PTR-U8-TABLE, PTR-U8-TABLE-VARIABLE) are saved as null; a slot
\ whose value must survive an image or capture uses this definer.
: PERSISTED-PTR-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ^^a ) ;

\ ---- a cell whose pointee the declaration chooses -----------------------------
\ The clause names the pointee. `create` publishes through `trust-raw`, which
\ mints every type variable TVK-RAW, and a raw pointee absorbs a scalar and
\ rejects a pointer: the definers above never hold the address of an address, so
\ neither can hold a pointer table's base. A spelled-out pointee has no variable,
\ so the cell's type is exactly what the clause says.
\
\ These live here, not beside TYPED-VARIABLE (src/core/layout-buffer.f), because
\ that form lays its words through the engine's child cell, filled only after the
\ boot prefix; a clause is compiled, so it works in the boot prefix, where the
\ checker's own pointer tables are declared.

\ A table of byte pointers: `n` cells, each holding a `^u8`. `n PTR-U8-TABLE
\ NAME` publishes NAME as the table's BASE, so `i cells NAME + 0 ptr-field` takes
\ the field of a declared pointer cell rather than of raw dictionary storage.
: PTR-U8-TABLE ( n -- )
   create cells SCRATCH-ALLOT does> ( -- ^^u8 ) ;

\ A PTR-U8-TABLE whose cells keep their addresses across a save.
: PERSISTED-PTR-U8-TABLE ( n -- )
   create dup 0 ?do here i cells + ptr-cell-mark loop cells allot does> ( -- ^^u8 ) ;

\ A persisted cell holding a PTR-U8-TABLE's base: the head of an arena that
\ starts in a boot table and is repointed at a grown allocation.
: PERSISTED-PTR-U8-TABLE-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ^^^u8 ) ;

: PTR-U8-TABLE-VARIABLE ( -- )
   create 1 cells SCRATCH-ALLOT does> ( -- ^^^u8 ) ;
