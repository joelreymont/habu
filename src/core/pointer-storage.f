\ pointer-storage.f - pointer-valued global storage definer.

\ The clauses are empty on purpose: an empty clause declares the effect only, and
\ the engine leaves the word the body and DKIND:ADDR stamp `create` gave it
\ (habu2.f DOES-REC:ELIDE-EMPTY), so a read costs one load (`0 ptr-field` cost a
\ call, a branch and a frame).
: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) ;

\ The DATA cell at an offset the engine layout reserves, declared as a cell that
\ holds a byte pointer. `ptr-field` (src/habu/prims.f) is the declared door
\ from an address to a pointer cell, so no cast stands behind this word. It is
\ a public colon word with no checker row, so the seal (internal-mark.f
\ IMK-CLASSIFY-PUB) marks it DNAME-INT, and checked source reaches it only
\ through the RESERVED-PTR-U8-CELL definer below. The null, NULL-PTR, is an
\ engine primitive (src/habu/prims.f) and needs no cell.
package POINTER-STORAGE
public
: DATA-CELL ( n -- ptr ptr u8 ) data-base swap + 0 ptr-field ;
;package

\ Persistence is explicit: scratch pointer slots use PTR-VARIABLE and never join
\ the relocation table; only a slot whose value must survive an image/capture uses
\ this definer.
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
\ that form computes its signature and throws E-LAYOUT-BUFFER until include.f
\ arms TDECL-EVAL-ARMED; a clause is compiled, so it works in the boot prefix,
\ where the checker's own pointer tables are declared. A new definer needs two
\ rows, because a definer written before the checker exists is invisible to it:
\ src/core/cell-effects.f states its own effect (without it the seal marks the
\ name DNAME-INT and `hb --load` answers `internal engine word`), and
\ src/habu/verify-source.f states what it publishes for the created word.

\ A table of byte pointers: `n` cells, each holding a `ptr u8`. `n PTR-U8-TABLE
\ NAME` publishes NAME as the table's BASE, so `i cells NAME + 0 ptr-field` takes
\ the field of a declared pointer cell rather than of raw dictionary storage.
: PTR-U8-TABLE ( n -- )
   create cells allot does> ( -- ptr ptr u8 ) ;

\ A persisted cell holding a PTR-U8-TABLE's base: the head of an arena that
\ starts in a boot table and is repointed at a grown allocation.
: PERSISTED-PTR-U8-TABLE-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr ptr u8 ) ;

: PTR-U8-TABLE-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr ptr u8 ) ;

\ `off RESERVED-PTR-U8-CELL NAME` publishes NAME as the address of the DATA cell
\ the engine layout reserves at `off`, declared `ptr ptr u8`. The offset goes
\ through POINTER-STORAGE:DATA-CELL on every read, so no absolute address is
\ baked. This definer has both rows, so the seal leaves its name alone; DATA-CELL
\ has neither and does not resolve outside the engine (test/internal-word-gate.f).
\ test/pointer-storage-test.f measures the pair.
: RESERVED-PTR-U8-CELL ( n -- )
   create , does> ( -- ptr ptr u8 ) @ POINTER-STORAGE:DATA-CELL ;
