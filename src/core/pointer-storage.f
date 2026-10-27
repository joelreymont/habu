\ pointer-storage.f - pointer-valued global storage definer.

\ THE CLAUSES ARE EMPTY ON PURPOSE. `create` already leaves a word that pushes
\ its own data address, and all a pointer definer adds is the type to read that
\ address at: `0 ptr-field` was the identity on it, so spelling it cost every
\ read a call, a branch and a frame. An empty clause declares the effect and
\ nothing else, and the engine answers by leaving the word the body and the
\ DKIND:ADDR stamp `create` gave it (habu2.f DOES-REC:ELIDE-EMPTY), so a read
\ costs the one load a bare cell costs.
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
\ THE CLAUSE NAMES THE POINTEE, AND THAT IS THE WHOLE MECHANISM. `create`
\ registers a published effect through `trust-raw`, which mints every type
\ VARIABLE in it TVK-RAW, and a raw pointee absorbs a scalar and refuses a
\ pointer (dot habu-refuse-a-ptr-5ad2734e): the two definers above therefore
\ declare a cell that holds an address but never the address of an address, so
\ neither can hold the base of a pointer TABLE. A clause that spells its pointee
\ out has no variable to seal, so the cell's type is exactly what it says and the
\ raw discipline never applies to it. Nothing is minted and nothing is trusted.
\
\ This is why the forms below live here rather than beside TYPED-VARIABLE. The
\ general parsed form (`TYPED-VARIABLE NAME ptr t`, src/core/layout-buffer.f)
\ computes its accessor's signature, so it needs the generative armed window and
\ throws E-LAYOUT-BUFFER until src/core/include.f arms TDECL-EVAL-ARMED - after
\ checker.f, layout-buffer.f and env-base.f. Include.f arms the evaluator before
\ its first typed cell. A clause
\ is compiled, not computed, so it asks nothing of the checker and works in the
\ boot prefix, which is where the checker's own pointer tables are declared.
\ Naming a definer per pointee is the price of that: add one when the prefix
\ needs a pointee these two do not spell - AND GIVE IT ITS TWO ROWS, because a
\ definer written before the checker exists is invisible to it. src/core/
\ cell-effects.f states the definer's own effect (without it the seal marks the
\ name DNAME-INT and `hb --load` answers `internal engine word`), and
\ src/habu/verify-source.f states what it publishes for the created word, so the
\ source scanner agrees with what `trust-raw` records on the native path.

\ A table of byte pointers: `n` cells, each holding a `ptr u8`. `n PTR-U8-TABLE
\ NAME` publishes NAME as the table's BASE, so `i cells NAME + 0 ptr-field` takes
\ the field of a declared pointer cell rather than of raw dictionary storage.
: PTR-U8-TABLE ( n -- )
   create cells allot does> ( -- ptr ptr u8 ) ;

\ A persisted cell holding a PTR-U8-TABLE's base: the head of an arena that
\ starts in a boot table and is repointed at a grown allocation. Persisted for
\ the same reason PERSISTED-PTR-VARIABLE is - the value must survive an image.
: PERSISTED-PTR-U8-TABLE-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr ptr u8 ) ;

\ Its scratch sibling: a cell holding a PTR-U8-TABLE's base that never joins the
\ relocation table, as PTR-VARIABLE is PERSISTED-PTR-VARIABLE's.
: PTR-U8-TABLE-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr ptr u8 ) ;

\ A cell the ENGINE LAYOUT reserves, not the dictionary: `off
\ RESERVED-PTR-U8-CELL NAME` publishes NAME as the address of the DATA cell at
\ `off`, declared as a `ptr ptr u8` the way the table definers above declare
\ theirs. The offset lives in the created body and goes through
\ POINTER-STORAGE:DATA-CELL on every read, so no absolute address is baked and
\ `ptr-field` is the one door from an offset to a pointer cell; there is no
\ cast.
\
\ THIS DEFINER IS THE TWO-ROW RULE'S OWN EXAMPLE. It has both rows, so the seal
\ leaves the name alone; POINTER-STORAGE:DATA-CELL, in this same file and
\ this same phase, has neither, is sealed, and does not resolve outside the
\ engine (test/internal-word-gate.f).
\ test/pointer-storage-test.f measures the pair.
: RESERVED-PTR-U8-CELL ( n -- )
   create , does> ( -- ptr ptr u8 ) @ POINTER-STORAGE:DATA-CELL ;
