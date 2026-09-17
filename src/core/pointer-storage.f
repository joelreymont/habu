\ pointer-storage.f - pointer-valued global storage definer.

\ THE CLAUSES ARE EMPTY ON PURPOSE. `create` already leaves a word that pushes
\ its own data address, and all a pointer definer adds is the type to read that
\ address at: `0 ptr-field` was the identity on it, so spelling it cost every
\ read a call, a branch and a frame. An empty clause declares the effect and
\ nothing else, and the engine answers by leaving the word the body and the
\ DKIND:ADDR stamp `create` gave it (habu2.f DOES-REC:ELIDE-EMPTY), so a read
\ costs the one load a bare cell costs. test/does-empty-clause.f pins it.
: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) ;

\ Typed reset code needs a null value without pretending numeric zero inhabits
\ every pointer family. A reserved DATA header cell, not a `create`d body whose
\ absolute address chain the stripped AOT linker refuses; src/habu/layout.f
\ NULL-PTR-CELL-OFF reserves the offset, says why, and internal-word-gate.f
\ refuses a drift from this copy. Keep this file inside pointer-storage-test.f's
\ SOURCE-CAP: it reads the whole file in.
$3800 constant NULL-PTR-OFF
: NULL-PTR-CELL ( -- ptr n )
   data-base NULL-PTR-OFF + ;
REG-PROTECT
: NULL-PTR ( -- ptr a )
   NULL-PTR-CELL 0 ptr-field @ ;

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
\ throws E-LAYOUT-BUFFER until src/core/include.f arms TDECL-EVAL-ARMED - later
\ than checker.f, layout-buffer.f, env-base.f and include.f's own head. A clause
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
