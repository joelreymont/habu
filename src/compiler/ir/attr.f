\ attr.f - the compiler attribute table: typed canonical attribute values
\ shared by every dialect, structurally interned per module.
\
\ docs/compiler-ir-design.md section 6.3 (the attribute table, lines 462-479),
\ sections 5.4/5.5 (target and numeric-policy authority), and plan item
\ IR-0.1's identity substrate. One table serves one module as two coupled
\ stores, both IR-ARENA arenas owned by the compilation context: a payload
\ pool holding string bytes, integer-list cells, and record pairs, and a row
\ table holding one fixed-shape record per distinct attribute. Attribute
\ identities are the existing IR-ID ir-attr-id family packed under the module
\ key; this file mints no parallel identity family and no raw converter.
\
\ CLOSED WORLD. The kinds are exactly the section 6.3 attribute list, one
\ closed ENUM member per design line:
\   int       - "integer"        (design line 467)
\   boolean   - "boolean"        (design line 468)
\   text      - "string"         (design line 469)
\   sym       - "symbol"         (design line 470)
\   type-ref  - "type"           (design line 471)
\   int-list  - "integer list"   (design line 473)
\   enum-val  - "enum"           (design line 474)
\   record    - "nested record"  (design line 475)
\   digest    - "digest"         (design line 476)
\ The remaining section 6.3 kind, "value list" (design line 472), references
\ ir-value-ids and therefore needs the operation/value pools of the table
\ stage (dot habu-store-compiler-ops-10440e3e); it lands with that owning
\ stage and its wire code 9 is reserved here, not half-built. The attr-window
\ rows that attach attribute lists to functions and operations (design lines
\ 388/421) likewise belong to the function/operation table stage. Unknown
\ kinds are structurally impossible in checked code (there is no raw-integer
\ path into a row) and design line 479's "unknown attributes fail" holds
\ against forgery too: every decoder is an exact case whose unmatched wire
\ code throws E-IR-ATTR-STATE.
\
\ ONE MEMBER IS SPELLED DIFFERENTLY FROM THE DESIGN WORD. The design calls
\ the byte-string kind "string", but `str` is a checker built-in type token
\ (src/core/checker.f, CC-STR), so an enum variant of that name is rejected
\ as reserved before the file can load. The member is therefore spelled
\ `text` and its constructor is TEXT, the same accommodation IR-TYPE made
\ when its float formats could not be spelled f32/f64. Only the Habu
\ spelling moves: the wire code, the payload, and the rendered form are the
\ design's string attribute unchanged.
\
\ TYPED PAYLOADS, VALIDATED AT CONSTRUCTION. A boolean is a real bool; a
\ symbol reference must be an id the module's own symbol interner vouches for
\ (IR-SYM:LEN@ is the authority); a type reference must be an id the module's
\ own type table vouches for (IR-TYPE:KIND@); record keys are symbol
\ references and record values are already constructed attributes of this
\ same table. A foreign module's or context's id rejects E-IR-ATTR-OWNER
\ before any row is touched.
\
\ TARGET AND NUMERIC OWNERSHIP. The enum-val kind's families are exactly the
\ compiler's closed vocabularies: the five numeric-policy families of CNUM
\ (design section 5.5 - overflow, float-model, contraction, fast-math,
\ compare) and the four target-contract families of CTARGET (design section
\ 5.4 - arch, abi, endian, ptr-width). Numeric-policy members are validated
\ against CNUM's vocabulary by construction: the constructors consume the
\ typed CNUM values themselves, so an out-of-vocabulary member cannot be
\ presented, and a forged stored code rejects at every decode. Target members
\ are validated against the context's bound contract (IR-CTX:BINDING@ ->
\ CBIND:TARGET@): section 5.4 makes the binding the single owner of target
\ facts, so an attribute stating a target fact the binding contradicts - a
\ ptx arch attribute in an aarch64 compilation - rejects E-IR-ATTR-TARGET.
\ A later dialect stage may add a family as a new wire code; existing codes
\ are never renumbered without a schema bump in the canonical encoder.
\
\ STORE SHAPES. Each arena carries a three-cell header (format tag, owning
\ module serial, committed capacity). The row table stores one five-cell row
\ per attribute: kind code and four kind-specific field cells. The pool's
\ data cells hold string bytes packed eight per cell little-endian (each
\ string starting on a fresh cell, tail zero-padded, the IR-SYM discipline),
\ integer-list elements one per cell, and record pairs as (key ordinal, value
\ ordinal) cell pairs. Every access rechecks shape, window, and stored
\ references fail-closed (E-IR-ATTR-STATE), so a holder who bypasses this
\ package and appends raw cells cannot make a reader touch cells outside the
\ live ranges.
\
\ INTERNING IS STRUCTURAL FIELD EQUALITY. Every constructor scans for an
\ existing row whose complete canonical field set equals the candidate and
\ answers that identity; only a miss appends. Pointer identity plays no part;
\ the pool start offset is storage, not identity - equality of windowed kinds
\ is equality of the window's cells. Every semantic field participates:
\ changing an integer's value, a boolean's truth, one string byte or its
\ length, a referenced symbol or type ordinal, one list element or the list
\ order or length, an enum family or member, one record key, value, or pair
\ count, or one digest word each mints a distinct identity.
\
\ DETERMINISTIC REFERENCE IDENTITY. Ids are module-local insertion ordinals
\ under the module key. Record pairs are sorted by key ordinal at
\ construction and duplicate keys reject, so the pair presentation order
\ cannot change a record's identity fields: one set of pairs is one record
\ however it was written. That in-memory order is by symbol ordinal, which is
\ enough to make identity presentation-independent inside one module but is
\ not yet the design line 479 canonical order, because symbol ordinals are
\ themselves insertion-ordered; the section 6.6 encoder sorts symbols by
\ their bytes and emits its own permutation, which is one of the permutations
\ the next paragraph has to renumber under. Record values must already be
\ constructed, so a value ordinal is strictly below its record's; a forward
\ reference dies E-IR-ATTR-BOUND before anything is written, cycles are
\ impossible by construction, and the renderer re-verifies the strict decrease
\ on every stored reference, so recursive walks terminate on any table state.
\
\ CANONICAL ORDER MUST RENUMBER STORED REFERENCES, INCLUDING FOREIGN ONES.
\ Insertion-ordered ordinals are stable but not canonical, so the section 6.6
\ encoder may emit the rows in a structural order of its own choosing, and
\ sorting them is not enough. An attribute row can store another row's
\ module-local ordinal: a symbol attribute stores a symbol ordinal, a type
\ attribute stores a type ordinal, and a record pair stores a key symbol
\ ordinal beside a value attribute ordinal (ORD-OK below admits exactly these
\ foreign-table ordinals). Permuting the attribute table changes stored row
\ content, and so does permuting the symbol table or the type table underneath
\ it. A canonical attribute encoder must therefore renumber every embedded
\ reference under all three permutations it chose - its own, the symbol
\ table's and the type table's - and emitting sorted rows unchanged is not
\ canonicalization. What two orders agree on is the denotation of a row, never
\ its stored content. The same obligation is machine-checked for the type
\ table in formal/Common/Interning.v (Types.ty_both_orders_admissible,
\ Types.structural_rows_not_permutation and
\ Types.ty_denotation_order_independent, with the general statement restricted
\ to reference-free keys by MODEL GAP 8); this table stores ordinals the same
\ way, so it carries the same obligation. As there, the build orders that
\ exist at all are exactly the topological orders of the reference graph,
\ because a record value must already be constructed when its record is.
\
\ FOREIGN ORDINALS ARE VALIDATED WHERE THEY ARE OWNED. A symbol or type
\ reference is checked against its owning table at construction, and a reader
\ hands back an id under this module's key whose ordinal only has to be
\ non-negative here: the symbol interner and the type table each revalidate
\ the id they are given, so a corrupted row cannot make either table read a
\ row it never built.
\
\ STAGED LISTS. Habu words cannot pass variable-length lists on the stack, so
\ integer lists and records are built through the staged protocol IR-TYPE
\ established: IL-BEGIN / IL-ADD ... INT-LIST and REC-BEGIN / REC-PAIR ...
\ RECORD. One package-owned stage under the single-task compilation
\ discipline; a begin while any stage is open, an end without its begin, and
\ a list past STAGE-MAX all reject E-IR-ATTR-STAGE, and a rejected end
\ consumes the stage so no half-staged list leaks into the next build.
\
\ RENDER IS DIAGNOSTIC TEXT. RENDER writes a deterministic spelling into a
\ caller span - int(-7), bool(true), "bytes", sym#3, type#2, ints(1 2),
\ fast-math:reassociate, rec(sym#0=int(1) sym#1=bool(true)), digest(64 hex
\ digits) - depending only on structural content, never on interning history,
\ and is never parsed by the compiler (design section 6.6). Reference kinds
\ spell the module-local ordinal because the referenced table is not part of
\ this store. Insertion-ordered ordinals are stable but not canonical; the
\ section 6.6 encoder orders structurally, exactly as for symbols and types.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f

package IR-ATTR
public

\ The exhaustive attribute vocabulary. Closed ENUM families make an unknown
\ kind or family unrepresentable in checked code; the wire codes below
\ persist them.
ENUM kind DERIVE eq
   int
   boolean
   text
   sym
   type-ref
   int-list
   enum-val
   record
   digest
;ENUM

ENUM efam DERIVE eq
   overflow
   float-model
   contraction
   fast-math
   compare
   arch
   abi
   endian
   ptr-width
;ENUM

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner
\ comparison. Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$41544C31 constant ATL-MAGIC         \ "ATL1": the payload-pool header format tag
$41545231 constant ATR-MAGIC         \ "ATR1": the row-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-KIND
1 constant OFF-A
2 constant OFF-B
3 constant OFF-C
4 constant OFF-D
5 constant ROW-CELLS
8 constant CELL-BYTES
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-MAX
32 constant STAGE-MAX                \ committed per-list stage ceiling
$8000000000000000 constant INT-MIN

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family, mirroring IR-TYPE: a code may be added but
\ never renumbered without a schema bump in the canonical encoder. Code 9 is
\ reserved for the value-list kind that lands with the value pools.
0 constant K-INT
1 constant K-BOOL
2 constant K-TXT
3 constant K-SYM
4 constant K-TYPE
5 constant K-ILIST
6 constant K-ENUM
7 constant K-REC
8 constant K-DIG

0 constant F-OVF
1 constant F-FLO
2 constant F-CON
3 constant F-FAS
4 constant F-CMP
5 constant F-ARCH
6 constant F-ABI
7 constant F-END
8 constant F-PTRW

: N>KIND ( n -- IR-ATTR:kind )
   case
      K-INT   of IR--ATTR-KIND:INT endof
      K-BOOL  of IR--ATTR-KIND:BOOLEAN endof
      K-TXT   of IR--ATTR-KIND:TEXT endof
      K-SYM   of IR--ATTR-KIND:SYM endof
      K-TYPE  of IR--ATTR-KIND:TYPE-REF endof
      K-ILIST of IR--ATTR-KIND:INT-LIST endof
      K-ENUM  of IR--ATTR-KIND:ENUM-VAL endof
      K-REC   of IR--ATTR-KIND:RECORD endof
      K-DIG   of IR--ATTR-KIND:DIGEST endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>EFAM ( n -- IR-ATTR:efam )
   case
      F-OVF  of IR--ATTR-EFAM:OVERFLOW endof
      F-FLO  of IR--ATTR-EFAM:FLOAT-MODEL endof
      F-CON  of IR--ATTR-EFAM:CONTRACTION endof
      F-FAS  of IR--ATTR-EFAM:FAST-MATH endof
      F-CMP  of IR--ATTR-EFAM:COMPARE endof
      F-ARCH of IR--ATTR-EFAM:ARCH endof
      F-ABI  of IR--ATTR-EFAM:ABI endof
      F-END  of IR--ATTR-EFAM:ENDIAN endof
      F-PTRW of IR--ATTR-EFAM:PTR-WIDTH endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>BOOL ( n -- bool )
   case
      0 of false endof
      1 of true endof
      E-IR-ATTR-STATE throw
   endcase ;

\ ---- enum member wire codes --------------------------------------------------
\ The member codes are the components' stable canonical wire codes (the
\ CTARGET/CNUM preimage codes), matched value for value; the IR-CTX binding
\ persistence mirrors them the same way.
: OVF-CODE ( CNUM:overflow -- n )
   MATCH CNUM:overflow
      wrap OF 0 ENDOF
      trap OF 1 ENDOF
   ;MATCH ;

: FLO-CODE ( CNUM:float-model -- n )
   MATCH CNUM:float-model
      ieee754        OF 0 ENDOF
      flush-denormal OF 1 ENDOF
   ;MATCH ;

: CON-CODE ( CNUM:contraction -- n )
   MATCH CNUM:contraction
      forbidden OF 0 ENDOF
      allowed   OF 1 ENDOF
   ;MATCH ;

: FAS-CODE ( CNUM:fast-math -- n )
   MATCH CNUM:fast-math
      bit-exact   OF 0 ENDOF
      reassociate OF 1 ENDOF
      approximate OF 2 ENDOF
   ;MATCH ;

: CMP-CODE ( CNUM:compare -- n )
   MATCH CNUM:compare
      ieee754-unordered OF 0 ENDOF
      total-order       OF 1 ENDOF
      assume-ordered    OF 2 ENDOF
   ;MATCH ;

: ARCH-CODE ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
   ;MATCH ;

: ABI-CODE ( CTARGET:abi -- n )
   MATCH CTARGET:abi
      aapcs64-darwin OF 0 ENDOF
      aapcs64-linux  OF 1 ENDOF
      ptx-kernel     OF 2 ENDOF
   ;MATCH ;

: ENDN-CODE ( CTARGET:endian -- n )
   MATCH CTARGET:endian
      little OF 0 ENDOF
      big    OF 1 ENDOF
   ;MATCH ;

: PTRW-CODE ( CTARGET:ptr-width -- n )
   MATCH CTARGET:ptr-width
      bits32 OF 0 ENDOF
      bits64 OF 1 ENDOF
   ;MATCH ;

\ ---- enum member wire-code decoders ------------------------------------------
\ A stored code outside a family's vocabulary is a corrupted or forged row;
\ every decoder rejects it named, so malformed records cannot read as values.
: N>OVF ( n -- CNUM:overflow )
   case
      0 of CNUM-OVERFLOW:WRAP endof
      1 of CNUM-OVERFLOW:TRAP endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>FLO ( n -- CNUM:float-model )
   case
      0 of CNUM-FLOAT--MODEL:IEEE754 endof
      1 of CNUM-FLOAT--MODEL:FLUSH-DENORMAL endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>CON ( n -- CNUM:contraction )
   case
      0 of CNUM-CONTRACTION:FORBIDDEN endof
      1 of CNUM-CONTRACTION:ALLOWED endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>FAS ( n -- CNUM:fast-math )
   case
      0 of CNUM-FAST--MATH:BIT-EXACT endof
      1 of CNUM-FAST--MATH:REASSOCIATE endof
      2 of CNUM-FAST--MATH:APPROXIMATE endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>CMP ( n -- CNUM:compare )
   case
      0 of CNUM-COMPARE:IEEE754-UNORDERED endof
      1 of CNUM-COMPARE:TOTAL-ORDER endof
      2 of CNUM-COMPARE:ASSUME-ORDERED endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>ARCH ( n -- CTARGET:arch )
   case
      0 of CTARGET-ARCH:AARCH64 endof
      1 of CTARGET-ARCH:PTX endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>ABI ( n -- CTARGET:abi )
   case
      0 of CTARGET-ABI:AAPCS64-DARWIN endof
      1 of CTARGET-ABI:AAPCS64-LINUX endof
      2 of CTARGET-ABI:PTX-KERNEL endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>ENDN ( n -- CTARGET:endian )
   case
      0 of CTARGET-ENDIAN:LITTLE endof
      1 of CTARGET-ENDIAN:BIG endof
      E-IR-ATTR-STATE throw
   endcase ;

: N>PTRW ( n -- CTARGET:ptr-width )
   case
      0 of CTARGET-PTR--WIDTH:BITS32 endof
      1 of CTARGET-PTR--WIDTH:BITS64 endof
      E-IR-ATTR-STATE throw
   endcase ;

\ The member vocabulary size per family code; the shared validity check for
\ stored enum rows.
: FAM-N ( n -- n )
   case
      F-OVF  of 2 endof
      F-FLO  of 2 endof
      F-CON  of 2 endof
      F-FAS  of 3 endof
      F-CMP  of 3 endof
      F-ARCH of 2 endof
      F-ABI  of 3 endof
      F-END  of 2 endof
      F-PTRW of 2 endof
      E-IR-ATTR-STATE throw
   endcase ;

: ENUM-CK ( n n -- )
   {: f:n m:n :}
   m 0 < m f FAM-N >= or if E-IR-ATTR-STATE throw then ;

\ ---- target legality ---------------------------------------------------------
\ The legality authority is the context's validated binding; nothing here
\ re-derives a target table.
: CTX-TARGET ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:TARGET@ ;

: TGT-CK ( n n -- )
   <> if E-IR-ATTR-TARGET throw then ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-ATTR-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-ATTR-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-ATTR-STATE throw then ;

: PMAGIC-CK ( n -- )
   ATL-MAGIC <> if E-IR-ATTR-STATE throw then ;

: RMAGIC-CK ( n -- )
   ATR-MAGIC <> if E-IR-ATTR-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a HC-MAGIC LCELL@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r IR-ARENA:USED RSHAPE-CK
   r HC-MAGIC LCELL@ RMAGIC-CK ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE PSHAPE-CK
   v HC-MAGIC FCELL@ PMAGIC-CK ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE RSHAPE-CK
   v HC-MAGIC FCELL@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED USED>CNT ;

: FCNT ( IR-ARENA:view -- n )
   IR-ARENA:SIZE USED>CNT ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ;

: FPCELLS ( IR-ARENA:view -- n )
   IR-ARENA:SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-ATTR-OWNER throw then ;

\ The pair coupling: both stores are what their tags claim and both carry the
\ same owning module serial, so a cross-module pairing rejects before any row
\ window is trusted against the wrong pool.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a PHDR-CK
   r RHDR-CK
   a HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FPAIR-CK ( IR-ARENA:view IR-ARENA:view -- )
   {: pv:IR-ARENA:view rv:IR-ARENA:view :}
   pv FPHDR-CK
   rv FRHDR-CK
   pv HC-SERIAL FCELL@ rv HC-SERIAL FCELL@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a r PAIR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: RKEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: FRKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key :}
   rv FRHDR-CK
   rv HC-SERIAL FCELL@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-attr-id -- n )
   IR-ID:ATTR-OWNER MID-SERIAL ;

\ Validate a presented attr-id against a resolved (header serial, count):
\ minted under this table's module, ordinal below the constructed count.
: ID-CK-N ( n n IR-ID:ir-attr-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-attr-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:ATTR-LOCAL
   dup cnt >= if E-IR-ATTR-BOUND throw then ;

: ID-CK ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ r CNT id ID-CK-N ;

: FID-CK ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-attr-id :}
   v FRHDR-CK
   v HC-SERIAL FCELL@ v FCNT id ID-CK-N ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

: FPC@ ( IR-ARENA:view n -- n )
   HDR-CELLS + FCELL@ ;

\ A row's pool window revalidated against the pool's live cells on every
\ access, so a forged or bypass-appended row rejects fail-closed.
: WIN-CK-N ( n n n -- )
   {: pc:n st:n cells:n :}
   st 0 < cells 0 < or if E-IR-ATTR-STATE throw then
   st cells + pc > if E-IR-ATTR-STATE throw then ;

\ A stored reference re-verified at the point of use: inside the table and
\ strictly below its referer, so recursive walks terminate on any state.
: REF-OK ( n n -- n )
   {: l:n raw:n :}
   raw 0 < raw l >= or if E-IR-ATTR-STATE throw then
   raw ;

\ A stored foreign-table ordinal (symbol, type): non-negative, upper bound
\ owned by the referenced table's own validation.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-ATTR-STATE throw then ;

\ ---- byte packing ------------------------------------------------------------
\ String payload cells hold up to eight bytes little-endian, missing tail
\ bytes zero, exactly the IR-SYM pool discipline: byte equality of two
\ strings is cell equality of their packed spans.
: BYTES>CELLS ( n -- n )
   CELL-BYTES 1- + CELL-BYTES / ;

: PACK-CELL ( ptr u8 n n -- n )
   {: p u:n j:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   0
   CELL-BYTES 0 ?do
      j i + u < if
         p j i + + c@  i 8 * lshift  or
      then
   loop ;

: CELL-BYTE ( n n -- n )
   8 * rshift $FF and ;

: PBYTE@ ( IR-ARENA:arena n n -- n )
   {: a:IR-ARENA:arena st:n i:n :}
   a st i CELL-BYTES / + PC@  i CELL-BYTES mod CELL-BYTE ;

: FPBYTE@ ( IR-ARENA:view n n -- n )
   {: pv:IR-ARENA:view st:n i:n :}
   pv st i CELL-BYTES / + FPC@  i CELL-BYTES mod CELL-BYTE ;

: BYTES-EQ ( IR-ARENA:arena n ptr u8 n -- bool )
   {: a:IR-ARENA:arena st:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   u BYTES>CELLS 0 ?do
      a st i + PC@  p u i CELL-BYTES * PACK-CELL <> if
         false unloop exit
      then
   loop
   true ;

\ ---- kind windows ------------------------------------------------------------
: KND-CK ( n n -- )
   <> if E-IR-ATTR-KIND throw then ;

: TXT-WIN ( IR-ARENA:arena IR-ARENA:arena n -- n n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-A RC@ r l OFF-B RC@ {: st:n u:n :}
   u 0 < if E-IR-ATTR-STATE throw then
   a PCELLS st u BYTES>CELLS WIN-CK-N
   st u ;

: FTXT-WIN ( IR-ARENA:view IR-ARENA:view n -- n n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n :}
   rv l OFF-A FRC@ rv l OFF-B FRC@ {: st:n u:n :}
   u 0 < if E-IR-ATTR-STATE throw then
   pv FPCELLS st u BYTES>CELLS WIN-CK-N
   st u ;

: IL-WIN ( IR-ARENA:arena IR-ARENA:arena n -- n n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-A RC@ r l OFF-B RC@ {: st:n cnt:n :}
   a PCELLS st cnt WIN-CK-N
   st cnt ;

: FIL-WIN ( IR-ARENA:view IR-ARENA:view n -- n n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n :}
   rv l OFF-A FRC@ rv l OFF-B FRC@ {: st:n cnt:n :}
   pv FPCELLS st cnt WIN-CK-N
   st cnt ;

: REC-WIN ( IR-ARENA:arena IR-ARENA:arena n -- n n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-A RC@ r l OFF-B RC@ {: st:n cnt:n :}
   a PCELLS st cnt 2 * WIN-CK-N
   st cnt ;

: FREC-WIN ( IR-ARENA:view IR-ARENA:view n -- n n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n :}
   rv l OFF-A FRC@ rv l OFF-B FRC@ {: st:n cnt:n :}
   pv FPCELLS st cnt 2 * WIN-CK-N
   st cnt ;

\ ---- fixed-field interning ---------------------------------------------------
: ROW5-MATCH? ( IR-ARENA:arena n n n n n n -- bool )
   {: r:IR-ARENA:arena l:n k:n x:n y:n z:n w:n :}
   r l OFF-KIND RC@ k <> if false exit then
   r l OFF-A RC@ x <> if false exit then
   r l OFF-B RC@ y <> if false exit then
   r l OFF-C RC@ z <> if false exit then
   r l OFF-D RC@ w = ;

: SCAN5 ( IR-ARENA:arena n n n n n -- n )
   {: r:IR-ARENA:arena k:n x:n y:n z:n w:n :}
   -1
   r CNT 0 ?do
      r i k x y z w ROW5-MATCH? if drop i leave then
   loop ;

: ROOM-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r CNT r HC-CAP LCELL@ >= if E-IR-ATTR-CAP throw then ;

: POOL-ROOM-CK ( IR-ARENA:arena n -- )
   {: a:IR-ARENA:arena cells:n :}
   a PCELLS cells + a HC-CAP LCELL@ > if E-IR-ATTR-CAP throw then ;

: ROW-ADD5 ( IR-CTX:ctx IR-ARENA:arena n n n n n -- n )
   {: c:IR-CTX:ctx r:IR-ARENA:arena k:n x:n y:n z:n w:n :}
   r CNT {: l:n :}
   c r k IR-ARENA:PUSH drop
   c r x IR-ARENA:PUSH drop
   c r y IR-ARENA:PUSH drop
   c r z IR-ARENA:PUSH drop
   c r w IR-ARENA:PUSH drop
   l ;

\ The one fixed-field intern path: answer the structurally equal row's
\ identity or append the five validated cells whole. Capacity is checked
\ before the first write, so a full table stays usable and duplicate
\ construction still answers.
: INTERN5 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n n n n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key k:n x:n y:n z:n w:n :}
   a r key KEY-CK
   r k x y z w SCAN5 {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-ATTR exit then
   r ROOM-CK
   c r k x y z w ROW-ADD5
   key swap IR-ID:PACK-ATTR ;

\ ---- string interning --------------------------------------------------------
: ROWTXT-MATCH? ( IR-ARENA:arena IR-ARENA:arena n ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   r l OFF-KIND RC@ K-TXT <> if false exit then
   r l OFF-B RC@ u <> if false exit then
   a  a r l TXT-WIN drop  p u BYTES-EQ ;

: SCANTXT ( IR-ARENA:arena IR-ARENA:arena ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   -1
   r CNT 0 ?do
      a r i p u ROWTXT-MATCH? if drop i leave then
   loop ;

\ ---- staged lists ------------------------------------------------------------
\ One package-owned stage under the single-task compilation discipline, mode
\ tagged so an integer-list end cannot close a record stage or the reverse.
0 constant MODE-NONE
1 constant MODE-IL
2 constant MODE-REC

here CELL 1- and CELL swap - CELL 1- and allot
variable STG-MODE
0 STG-MODE !
variable STG-N
0 STG-N !
create STG-V STAGE-MAX cells allot
create STG-VO STAGE-MAX cells allot
create STG-K STAGE-MAX cells allot
create STG-KO STAGE-MAX cells allot

: SGV@ ( n -- n )
   cells STG-V + @ ;

: SGV! ( n n -- )
   cells STG-V + ! ;

: SGVO@ ( n -- n )
   cells STG-VO + @ ;

: SGVO! ( n n -- )
   cells STG-VO + ! ;

: SGK@ ( n -- n )
   cells STG-K + @ ;

: SGK! ( n n -- )
   cells STG-K + ! ;

: SGKO@ ( n -- n )
   cells STG-KO + @ ;

: SGKO! ( n n -- )
   cells STG-KO + ! ;

: STAGE-OPEN ( n -- )
   STG-MODE @ MODE-NONE <> if E-IR-ATTR-STAGE throw then
   STG-MODE !
   0 STG-N ! ;

\ An end always ends the stage, whatever its outcome: the stage exists only
\ between one begin and the next end, so neither a wrong-protocol end nor a
\ rejected end can leave a half-staged list for the next build to close.
: STAGE-TAKE ( n -- )
   {: want:n :}
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have want <> if E-IR-ATTR-STAGE throw then ;

: STAGE-ROOM ( n -- )
   STG-MODE @ <> if E-IR-ATTR-STAGE throw then
   STG-N @ STAGE-MAX >= if E-IR-ATTR-STAGE throw then ;

\ ---- integer-list interning --------------------------------------------------
: ROWIL-MATCH? ( IR-ARENA:arena IR-ARENA:arena n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-KIND RC@ K-ILIST <> if false exit then
   r l OFF-B RC@ STG-N @ <> if false exit then
   a r l IL-WIN drop {: st:n :}
   STG-N @ 0 ?do
      a st i + PC@ i SGV@ <> if false unloop exit then
   loop
   true ;

: SCANIL ( IR-ARENA:arena IR-ARENA:arena -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   -1
   r CNT 0 ?do
      a r i ROWIL-MATCH? if drop i leave then
   loop ;

\ ---- record staging: validation, canonical order, interning ------------------
\ Every staged key must be a symbol the module's own interner vouches for and
\ every staged value an already constructed attribute of this table, so the
\ pair set is validated whole before anything is sorted or written.
: KEYS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: syr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   STG-N @ 0 ?do
      i SGKO@ key KEY-SERIAL SERIAL-CK
      syr key i SGK@ IR-ID:PACK-SYMBOL IR-SYM:LEN@ drop
   loop ;

: VALS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   STG-N @ 0 ?do
      i SGVO@ key KEY-SERIAL SERIAL-CK
      i SGV@ dup 0 < over r CNT >= or if E-IR-ATTR-BOUND throw then
      drop
   loop ;

: PAIR-SWAP ( n n -- )
   {: x:n y:n :}
   x SGK@ y SGK@ x SGK! y SGK!
   x SGV@ y SGV@ x SGV! y SGV! ;

\ Canonical key order: sort the validated pairs by key ordinal, so the
\ presentation order cannot reach a record's identity fields.
: REC-SORT ( -- )
   STG-N @ 1 ?do
      i
      begin
         dup 0 > if
            dup SGK@ over 1- SGK@ <
         else
            false
         then
      while
         dup dup 1- PAIR-SWAP
         1-
      repeat
      drop
   loop ;

: REC-DUP-CK ( -- )
   STG-N @ 1 ?do
      i SGK@ i 1- SGK@ = if E-IR-ATTR-VALUE throw then
   loop ;

: ROWREC-MATCH? ( IR-ARENA:arena IR-ARENA:arena n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-KIND RC@ K-REC <> if false exit then
   r l OFF-B RC@ STG-N @ <> if false exit then
   a r l REC-WIN drop {: st:n :}
   STG-N @ 0 ?do
      a st i 2 * + PC@ i SGK@ <> if false unloop exit then
      a st i 2 * + 1+ PC@ i SGV@ <> if false unloop exit then
   loop
   true ;

: SCANREC ( IR-ARENA:arena IR-ARENA:arena -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   -1
   r CNT 0 ?do
      a r i ROWREC-MATCH? if drop i leave then
   loop ;

\ ---- creation checks ---------------------------------------------------------
: ROW-CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-ATTR-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 1 < over POOL-MAX > or if E-IR-ATTR-CAP throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Create a module's attribute table: the payload pool committed to exactly
\ pcap cells and the row table committed to exactly rcap attributes, both
\ headers bound to key's module serial. The two handles plus the key are the
\ table; all three stay with the module owner, and the table dies with the
\ owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a ATL-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a pcap IR-ARENA:PUSH drop
   c rcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r ATR-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r rcap IR-ARENA:PUSH drop
   a r ;

\ ---- constructors ------------------------------------------------------------
\ Every constructor interns: the same fields twice answer one identity. The
\ ctx is the allocation and target-legality authority; a hit allocates
\ nothing.
: INT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   c a r key K-INT v 0 0 0 INTERN5 ;

: BOOLEAN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key bool -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:bool :}
   c a r key K-BOOL v if 1 else 0 then 0 0 0 INTERN5 ;

\ String bytes intern by content from any buffer; the bytes are copied into
\ the pool packed, so no caller pointer is retained.
: TEXT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   u 0 < if E-IR-ATTR-VALUE throw then
   a r key KEY-CK
   a r p u SCANTXT {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-ATTR exit then
   r ROOM-CK
   a u BYTES>CELLS POOL-ROOM-CK
   a PCELLS {: st:n :}
   u BYTES>CELLS 0 ?do
      c a  p u i CELL-BYTES * PACK-CELL  IR-ARENA:PUSH drop
   loop
   c r K-TXT st u 0 0 ROW-ADD5
   key swap IR-ID:PACK-ATTR ;

\ A symbol reference: the id must be minted under this module and vouched for
\ by the module's own symbol interner row table, the validation authority.
: SYMBOL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key syr:IR-ARENA:arena sid:IR-ID:ir-symbol-id :}
   a r key KEY-CK
   sid IR-ID:SYMBOL-OWNER MID-SERIAL key KEY-SERIAL SERIAL-CK
   syr sid IR-SYM:LEN@ drop
   c a r key K-SYM sid IR-ID:SYMBOL-LOCAL 0 0 0 INTERN5 ;

\ A type reference: the id must be minted under this module and vouched for
\ by the module's own type table.
: TYPE-REF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ID:ir-type-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key tyr:IR-ARENA:arena tid:IR-ID:ir-type-id :}
   a r key KEY-CK
   tid IR-ID:TYPE-OWNER MID-SERIAL key KEY-SERIAL SERIAL-CK
   tyr tid IR-TYPE:KIND@ drop
   c a r key K-TYPE tid IR-ID:TYPE-LOCAL 0 0 0 INTERN5 ;

: DIGEST ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CDIGEST:digest -- IR-ID:ir-attr-id )
   CDIGEST-DIGEST:UNMAKE
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key w0:n w1:n w2:n w3:n :}
   c a r key K-DIG w0 w1 w2 w3 INTERN5 ;

\ ---- numeric-policy enum attributes (design section 5.5) ---------------------
\ The typed CNUM member IS the vocabulary validation: no raw member code can
\ be presented.
: OVERFLOW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CNUM:overflow -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CNUM:overflow :}
   c a r key K-ENUM F-OVF v OVF-CODE 0 0 INTERN5 ;

: FLOAT-MODEL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CNUM:float-model -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CNUM:float-model :}
   c a r key K-ENUM F-FLO v FLO-CODE 0 0 INTERN5 ;

: CONTRACTION ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CNUM:contraction -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CNUM:contraction :}
   c a r key K-ENUM F-CON v CON-CODE 0 0 INTERN5 ;

: FAST-MATH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CNUM:fast-math -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CNUM:fast-math :}
   c a r key K-ENUM F-FAS v FAS-CODE 0 0 INTERN5 ;

: COMPARE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CNUM:compare -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CNUM:compare :}
   c a r key K-ENUM F-CMP v CMP-CODE 0 0 INTERN5 ;

\ ---- target enum attributes (design section 5.4) -----------------------------
\ Target facts have one owner, the context's bound contract; an attribute
\ stating a different value describes another machine and rejects.
: ARCH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CTARGET:arch -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CTARGET:arch :}
   v ARCH-CODE {: mc:n :}
   c CTX-TARGET CTARGET:ARCH@ ARCH-CODE mc TGT-CK
   c a r key K-ENUM F-ARCH mc 0 0 INTERN5 ;

: ABI ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CTARGET:abi -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CTARGET:abi :}
   v ABI-CODE {: mc:n :}
   c CTX-TARGET CTARGET:ABI@ ABI-CODE mc TGT-CK
   c a r key K-ENUM F-ABI mc 0 0 INTERN5 ;

: ENDIAN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CTARGET:endian -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CTARGET:endian :}
   v ENDN-CODE {: mc:n :}
   c CTX-TARGET CTARGET:ENDIAN@ ENDN-CODE mc TGT-CK
   c a r key K-ENUM F-END mc 0 0 INTERN5 ;

: PTR-WIDTH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key CTARGET:ptr-width -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key v:CTARGET:ptr-width :}
   v PTRW-CODE {: mc:n :}
   c CTX-TARGET CTARGET:PTR-WIDTH@ PTRW-CODE mc TGT-CK
   c a r key K-ENUM F-PTRW mc 0 0 INTERN5 ;

\ ---- integer-list stage protocol ---------------------------------------------
: IL-BEGIN ( -- )
   MODE-IL STAGE-OPEN ;

: IL-ADD ( n -- )
   {: v:n :}
   MODE-IL STAGE-ROOM
   v STG-N @ SGV!
   STG-N @ 1+ STG-N ! ;

\ Close the stage and intern the staged list. The stage is consumed first, so
\ a rejected end never leaks a half-staged list; all validation and both room
\ checks run before the first cell is written.
: INT-LIST ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   MODE-IL STAGE-TAKE
   a r key KEY-CK
   a r SCANIL {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-ATTR exit then
   r ROOM-CK
   a STG-N @ POOL-ROOM-CK
   a PCELLS {: st:n :}
   STG-N @ 0 ?do
      c a i SGV@ IR-ARENA:PUSH drop
   loop
   c r K-ILIST st STG-N @ 0 0 ROW-ADD5
   key swap IR-ID:PACK-ATTR ;

\ ---- record stage protocol ---------------------------------------------------
: REC-BEGIN ( -- )
   MODE-REC STAGE-OPEN ;

: REC-PAIR ( IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: kid:IR-ID:ir-symbol-id vid:IR-ID:ir-attr-id :}
   MODE-REC STAGE-ROOM
   kid IR-ID:SYMBOL-OWNER MID-SERIAL STG-N @ SGKO!
   kid IR-ID:SYMBOL-LOCAL STG-N @ SGK!
   vid ID-OWNER-SERIAL STG-N @ SGVO!
   vid IR-ID:ATTR-LOCAL STG-N @ SGV!
   STG-N @ 1+ STG-N ! ;

\ Close the stage and intern the staged record against the module's symbol
\ interner (syr) and this table. Keys are sorted canonically and duplicates
\ reject, so pair order cannot change identity; values must already be
\ constructed, so references strictly decrease.
: RECORD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key syr:IR-ARENA:arena :}
   MODE-REC STAGE-TAKE
   a r key KEY-CK
   syr key KEYS-CK
   r key VALS-CK
   REC-SORT
   REC-DUP-CK
   a r SCANREC {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-ATTR exit then
   r ROOM-CK
   a STG-N @ 2 * POOL-ROOM-CK
   a PCELLS {: st:n :}
   STG-N @ 0 ?do
      c a i SGK@ IR-ARENA:PUSH drop
      c a i SGV@ IR-ARENA:PUSH drop
   loop
   c r K-REC st STG-N @ 0 0 ROW-ADD5
   key swap IR-ID:PACK-ATTR ;

\ ---- live readers ------------------------------------------------------------
: ATTRS ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

: KIND@ ( IR-ARENA:arena IR-ID:ir-attr-id -- IR-ATTR:kind )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ N>KIND ;

: INT@ ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-INT KND-CK
   r l OFF-A RC@ ;

: BOOLEAN@ ( IR-ARENA:arena IR-ID:ir-attr-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-BOOL KND-CK
   r l OFF-A RC@ N>BOOL ;

: TEXT-LEN@ ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-TXT KND-CK
   r l OFF-B RC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

\ Copy a string attribute's bytes into the caller's span and answer the byte
\ length; a span smaller than the string rejects named before any write.
: TEXT-COPY ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-attr-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-attr-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   a r PAIR-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-TXT KND-CK
   a r l TXT-WIN {: st:n u:n :}
   u cap > if E-IR-ATTR-RANGE throw then
   u 0 ?do
      a st i PBYTE@  q i + c!
   loop
   u ;

: SYM@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   r key RKEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-SYM KND-CK
   key r l OFF-A RC@ ORD-OK IR-ID:PACK-SYMBOL ;

: TYPE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-type-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   r key RKEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-TYPE KND-CK
   key r l OFF-A RC@ ORD-OK IR-ID:PACK-TYPE ;

: DIGEST@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CDIGEST:digest )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-DIG KND-CK
   r l OFF-A RC@ r l OFF-B RC@ r l OFF-C RC@ r l OFF-D RC@
   CDIGEST-DIGEST:MAKE ;

: ITEMS@ ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-ILIST KND-CK
   r l OFF-B RC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

: ITEM@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-attr-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-attr-id i:n :}
   a r PAIR-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-ILIST KND-CK
   a r l IL-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   a st i + PC@ ;

: PAIRS@ ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-REC KND-CK
   r l OFF-B RC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

: KEY@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-attr-id n -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-attr-id i:n :}
   a r key KEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-REC KND-CK
   a r l REC-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   key a st i 2 * + PC@ ORD-OK IR-ID:PACK-SYMBOL ;

: VAL@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-attr-id n -- IR-ID:ir-attr-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-attr-id i:n :}
   a r key KEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-REC KND-CK
   a r l REC-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   key l a st i 2 * + 1+ PC@ REF-OK IR-ID:PACK-ATTR ;

: EFAM@ ( IR-ARENA:arena IR-ID:ir-attr-id -- IR-ATTR:efam )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-ENUM KND-CK
   r l OFF-A RC@ r l OFF-B RC@ {: f:n m:n :}
   f m ENUM-CK
   f N>EFAM ;

private

\ Project an enum row's member code under a required family: wrong kind or
\ family is a caller error, an out-of-vocabulary stored code a state error.
: EMEM ( IR-ARENA:arena IR-ID:ir-attr-id n -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id f:n :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-ENUM KND-CK
   r l OFF-A RC@ r l OFF-B RC@ {: rf:n m:n :}
   rf m ENUM-CK
   rf f KND-CK
   m ;

: FEMEM ( IR-ARENA:view IR-ID:ir-attr-id n -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id f:n :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-ENUM KND-CK
   rv l OFF-A FRC@ rv l OFF-B FRC@ {: rf:n m:n :}
   rf m ENUM-CK
   rf f KND-CK
   m ;

public

: OVERFLOW@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CNUM:overflow )
   F-OVF EMEM N>OVF ;

: FLOAT-MODEL@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CNUM:float-model )
   F-FLO EMEM N>FLO ;

: CONTRACTION@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CNUM:contraction )
   F-CON EMEM N>CON ;

: FAST-MATH@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CNUM:fast-math )
   F-FAS EMEM N>FAS ;

: COMPARE@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CNUM:compare )
   F-CMP EMEM N>CMP ;

: ARCH@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CTARGET:arch )
   F-ARCH EMEM N>ARCH ;

: ABI@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CTARGET:abi )
   F-ABI EMEM N>ABI ;

: ENDIAN@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CTARGET:endian )
   F-END EMEM N>ENDN ;

: PTR-WIDTH@ ( IR-ARENA:arena IR-ID:ir-attr-id -- CTARGET:ptr-width )
   F-PTRW EMEM N>PTRW ;

\ ---- render ------------------------------------------------------------------
private

\ Byte emitters over the caller span: cur rides on top so emission chains
\ read left to right without juggling.
: EMIT-B ( n ptr u8 n n -- n )
   {: cur:n q cap:n b:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur cap >= if E-IR-ATTR-RANGE throw then
   b q cur + c!
   cur 1+ ;

: EMIT-S ( n ptr u8 n ptr u8 n -- n )
   {: cur:n q cap:n p u:n :} \ typed-local-lint: allow-bare-local - q and p keep the ptr u8 byte-span roles
   cur u + cap > if E-IR-ATTR-RANGE throw then
   u 0 ?do
      p i + c@ q cur + i + c!
   loop
   cur u + ;

\ Decimal digits for the integer arms, built reversed in a package scratch
\ used transiently under the single-task discipline.
here CELL 1- and CELL swap - CELL 1- and allot
variable DVAL
variable DLEN
create DBUF 24 allot

: DEC-BUILD ( n -- )
   DVAL !
   0 DLEN !
   DVAL @ 0= if
      $30 DBUF c!
      1 DLEN !
      exit
   then
   begin
      DVAL @ 0 >
   while
      DVAL @ 10 mod $30 + DBUF DLEN @ + c!
      DVAL @ 10 / DVAL !
      DLEN @ 1+ DLEN !
   repeat ;

: EMIT-U ( n ptr u8 n n -- n )
   {: cur:n q cap:n v:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   v DEC-BUILD
   cur
   DLEN @ 0 ?do
      q cap DBUF DLEN @ 1- i - + c@ EMIT-B
   loop ;

: EMIT-N ( n ptr u8 n n -- n )
   {: cur:n q cap:n v:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   v INT-MIN = if cur q cap s" -9223372036854775808" EMIT-S exit then
   v 0 < if
      cur q cap $2D EMIT-B
      q cap 0 v - EMIT-U
      exit
   then
   cur q cap v EMIT-U ;

: HEXDIG ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: EMIT-H16 ( n ptr u8 n n -- n )
   {: cur:n q cap:n v:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur
   16 0 ?do
      q cap  v 15 i - 4 * rshift $F and HEXDIG  EMIT-B
   loop ;

\ Diagnostic spellings for the closed vocabularies. A stored code outside
\ them is a corrupted row, exactly as in the decoders.
: FAM-STR ( n -- ptr u8 n )
   case
      F-OVF  of s" overflow" endof
      F-FLO  of s" float-model" endof
      F-CON  of s" contraction" endof
      F-FAS  of s" fast-math" endof
      F-CMP  of s" compare" endof
      F-ARCH of s" arch" endof
      F-ABI  of s" abi" endof
      F-END  of s" endian" endof
      F-PTRW of s" ptr-width" endof
      E-IR-ATTR-STATE throw
   endcase ;

: OVF-STR ( n -- ptr u8 n )
   case
      0 of s" wrap" endof
      1 of s" trap" endof
      E-IR-ATTR-STATE throw
   endcase ;

: FLO-STR ( n -- ptr u8 n )
   case
      0 of s" ieee754" endof
      1 of s" flush-denormal" endof
      E-IR-ATTR-STATE throw
   endcase ;

: CON-STR ( n -- ptr u8 n )
   case
      0 of s" forbidden" endof
      1 of s" allowed" endof
      E-IR-ATTR-STATE throw
   endcase ;

: FAS-STR ( n -- ptr u8 n )
   case
      0 of s" bit-exact" endof
      1 of s" reassociate" endof
      2 of s" approximate" endof
      E-IR-ATTR-STATE throw
   endcase ;

: CMP-STR ( n -- ptr u8 n )
   case
      0 of s" ieee754-unordered" endof
      1 of s" total-order" endof
      2 of s" assume-ordered" endof
      E-IR-ATTR-STATE throw
   endcase ;

: ARCH-STR ( n -- ptr u8 n )
   case
      0 of s" aarch64" endof
      1 of s" ptx" endof
      E-IR-ATTR-STATE throw
   endcase ;

: ABI-STR ( n -- ptr u8 n )
   case
      0 of s" aapcs64-darwin" endof
      1 of s" aapcs64-linux" endof
      2 of s" ptx-kernel" endof
      E-IR-ATTR-STATE throw
   endcase ;

: ENDN-STR ( n -- ptr u8 n )
   case
      0 of s" little" endof
      1 of s" big" endof
      E-IR-ATTR-STATE throw
   endcase ;

: PTRW-STR ( n -- ptr u8 n )
   case
      0 of s" bits32" endof
      1 of s" bits64" endof
      E-IR-ATTR-STATE throw
   endcase ;

: MEM-STR ( n n -- ptr u8 n )
   swap case
      F-OVF  of OVF-STR endof
      F-FLO  of FLO-STR endof
      F-CON  of CON-STR endof
      F-FAS  of FAS-STR endof
      F-CMP  of CMP-STR endof
      F-ARCH of ARCH-STR endof
      F-ABI  of ABI-STR endof
      F-END  of ENDN-STR endof
      F-PTRW of PTRW-STR endof
      E-IR-ATTR-STATE throw
   endcase ;

: EMIT-ENUM ( n ptr u8 n n n -- n )
   {: cur:n q cap:n f:n m:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   f m ENUM-CK
   cur q cap f FAM-STR EMIT-S
   q cap $3A EMIT-B
   q cap f m MEM-STR EMIT-S ;

: EMIT-BOOL ( n ptr u8 n n -- n )
   {: cur:n q cap:n v:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur q cap
   v N>BOOL if s" bool(true)" else s" bool(false)" then
   EMIT-S ;

\ Render one row by local ordinal. The record arm recurses on stored value
\ references; RECURSE binds the containing word, so that arm stays inline
\ here while every non-recursive step is a named helper. Each stored value
\ reference passes REF-OK's strict decrease, so recursion depth is bounded by
\ the ordinal itself on any table state.
: R-EMIT ( IR-ARENA:arena IR-ARENA:arena ptr u8 n n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena q cap:n cur:n l:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   r l OFF-KIND RC@ {: k:n :}
   k K-INT = if
      cur q cap s" int(" EMIT-S
      q cap r l OFF-A RC@ EMIT-N
      q cap $29 EMIT-B
      exit
   then
   k K-BOOL = if cur q cap r l OFF-A RC@ EMIT-BOOL exit then
   k K-TXT = if
      a r l TXT-WIN {: st:n u:n :}
      cur q cap $22 EMIT-B
      u 0 ?do
         q cap a st i PBYTE@ EMIT-B
      loop
      q cap $22 EMIT-B
      exit
   then
   k K-SYM = if
      cur q cap s" sym#" EMIT-S
      q cap r l OFF-A RC@ ORD-OK EMIT-U
      exit
   then
   k K-TYPE = if
      cur q cap s" type#" EMIT-S
      q cap r l OFF-A RC@ ORD-OK EMIT-U
      exit
   then
   k K-ILIST = if
      a r l IL-WIN {: st:n cnt:n :}
      cur q cap s" ints(" EMIT-S
      cnt 0 ?do
         i 0 > if q cap $20 EMIT-B then
         q cap a st i + PC@ EMIT-N
      loop
      q cap $29 EMIT-B
      exit
   then
   k K-ENUM = if
      cur q cap r l OFF-A RC@ r l OFF-B RC@ EMIT-ENUM exit
   then
   k K-DIG = if
      cur q cap s" digest(" EMIT-S
      q cap r l OFF-A RC@ EMIT-H16
      q cap r l OFF-B RC@ EMIT-H16
      q cap r l OFF-C RC@ EMIT-H16
      q cap r l OFF-D RC@ EMIT-H16
      q cap $29 EMIT-B
      exit
   then
   k K-REC <> if E-IR-ATTR-STATE throw then
   a r l REC-WIN {: st:n cnt:n :}
   cur q cap s" rec(" EMIT-S
   cnt 0 ?do
      i 0 > if q cap $20 EMIT-B then
      q cap s" sym#" EMIT-S
      q cap a st i 2 * + PC@ ORD-OK EMIT-U
      q cap $3D EMIT-B
      {: cm:n :}
      a r q cap cm  l a st i 2 * + 1+ PC@ REF-OK  recurse
   loop
   q cap $29 EMIT-B ;

\ The frozen twin of R-EMIT over the arena views.
: FR-EMIT ( IR-ARENA:view IR-ARENA:view ptr u8 n n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view q cap:n cur:n l:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   rv l OFF-KIND FRC@ {: k:n :}
   k K-INT = if
      cur q cap s" int(" EMIT-S
      q cap rv l OFF-A FRC@ EMIT-N
      q cap $29 EMIT-B
      exit
   then
   k K-BOOL = if cur q cap rv l OFF-A FRC@ EMIT-BOOL exit then
   k K-TXT = if
      pv rv l FTXT-WIN {: st:n u:n :}
      cur q cap $22 EMIT-B
      u 0 ?do
         q cap pv st i FPBYTE@ EMIT-B
      loop
      q cap $22 EMIT-B
      exit
   then
   k K-SYM = if
      cur q cap s" sym#" EMIT-S
      q cap rv l OFF-A FRC@ ORD-OK EMIT-U
      exit
   then
   k K-TYPE = if
      cur q cap s" type#" EMIT-S
      q cap rv l OFF-A FRC@ ORD-OK EMIT-U
      exit
   then
   k K-ILIST = if
      pv rv l FIL-WIN {: st:n cnt:n :}
      cur q cap s" ints(" EMIT-S
      cnt 0 ?do
         i 0 > if q cap $20 EMIT-B then
         q cap pv st i + FPC@ EMIT-N
      loop
      q cap $29 EMIT-B
      exit
   then
   k K-ENUM = if
      cur q cap rv l OFF-A FRC@ rv l OFF-B FRC@ EMIT-ENUM exit
   then
   k K-DIG = if
      cur q cap s" digest(" EMIT-S
      q cap rv l OFF-A FRC@ EMIT-H16
      q cap rv l OFF-B FRC@ EMIT-H16
      q cap rv l OFF-C FRC@ EMIT-H16
      q cap rv l OFF-D FRC@ EMIT-H16
      q cap $29 EMIT-B
      exit
   then
   k K-REC <> if E-IR-ATTR-STATE throw then
   pv rv l FREC-WIN {: st:n cnt:n :}
   cur q cap s" rec(" EMIT-S
   cnt 0 ?do
      i 0 > if q cap $20 EMIT-B then
      q cap s" sym#" EMIT-S
      q cap pv st i 2 * + FPC@ ORD-OK EMIT-U
      q cap $3D EMIT-B
      {: cm:n :}
      pv rv q cap cm  l pv st i 2 * + 1+ FPC@ REF-OK  recurse
   loop
   q cap $29 EMIT-B ;

public

\ Render an attribute's deterministic diagnostic spelling into the caller's
\ span and answer the byte length; a span too small rejects named before the
\ overflowing write. The text depends only on structural content, never on
\ interning history.
: RENDER ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-attr-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-attr-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   a r PAIR-CK
   r id ID-CK {: l:n :}
   a r q cap 0 l R-EMIT ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its attributes through the two arena views; the
\ retired builder handles reject every touch with E-IR-ARENA-FROZEN.
: FATTRS ( IR-ARENA:view -- n )
   dup FRHDR-CK FCNT ;

: FKIND@ ( IR-ARENA:view IR-ID:ir-attr-id -- IR-ATTR:kind )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ N>KIND ;

: FINT@ ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-INT KND-CK
   rv l OFF-A FRC@ ;

: FBOOLEAN@ ( IR-ARENA:view IR-ID:ir-attr-id -- bool )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-BOOL KND-CK
   rv l OFF-A FRC@ N>BOOL ;

: FTEXT-LEN@ ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-TXT KND-CK
   rv l OFF-B FRC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

: FTEXT-COPY ( IR-ARENA:view IR-ARENA:view IR-ID:ir-attr-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-attr-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-TXT KND-CK
   pv rv l FTXT-WIN {: st:n u:n :}
   u cap > if E-IR-ATTR-RANGE throw then
   u 0 ?do
      pv st i FPBYTE@  q i + c!
   loop
   u ;

: FSYM@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-SYM KND-CK
   key rv l OFF-A FRC@ ORD-OK IR-ID:PACK-SYMBOL ;

: FTYPE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-type-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-TYPE KND-CK
   key rv l OFF-A FRC@ ORD-OK IR-ID:PACK-TYPE ;

: FDIGEST@ ( IR-ARENA:view IR-ID:ir-attr-id -- CDIGEST:digest )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-DIG KND-CK
   rv l OFF-A FRC@ rv l OFF-B FRC@ rv l OFF-C FRC@ rv l OFF-D FRC@
   CDIGEST-DIGEST:MAKE ;

: FITEMS@ ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-ILIST KND-CK
   rv l OFF-B FRC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

: FITEM@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-attr-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-attr-id i:n :}
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-ILIST KND-CK
   pv rv l FIL-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   pv st i + FPC@ ;

: FPAIRS@ ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-REC KND-CK
   rv l OFF-B FRC@
   dup 0 < if E-IR-ATTR-STATE throw then ;

: FKEY@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-attr-id n -- IR-ID:ir-symbol-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-attr-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-REC KND-CK
   pv rv l FREC-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   key pv st i 2 * + FPC@ ORD-OK IR-ID:PACK-SYMBOL ;

: FVAL@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-attr-id n -- IR-ID:ir-attr-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-attr-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-REC KND-CK
   pv rv l FREC-WIN {: st:n cnt:n :}
   i 0 < i cnt >= or if E-IR-ATTR-BOUND throw then
   key l pv st i 2 * + 1+ FPC@ REF-OK IR-ID:PACK-ATTR ;

: FEFAM@ ( IR-ARENA:view IR-ID:ir-attr-id -- IR-ATTR:efam )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-ENUM KND-CK
   rv l OFF-A FRC@ rv l OFF-B FRC@ {: f:n m:n :}
   f m ENUM-CK
   f N>EFAM ;

: FOVERFLOW@ ( IR-ARENA:view IR-ID:ir-attr-id -- CNUM:overflow )
   F-OVF FEMEM N>OVF ;

: FFLOAT-MODEL@ ( IR-ARENA:view IR-ID:ir-attr-id -- CNUM:float-model )
   F-FLO FEMEM N>FLO ;

: FCONTRACTION@ ( IR-ARENA:view IR-ID:ir-attr-id -- CNUM:contraction )
   F-CON FEMEM N>CON ;

: FFAST-MATH@ ( IR-ARENA:view IR-ID:ir-attr-id -- CNUM:fast-math )
   F-FAS FEMEM N>FAS ;

: FCOMPARE@ ( IR-ARENA:view IR-ID:ir-attr-id -- CNUM:compare )
   F-CMP FEMEM N>CMP ;

: FARCH@ ( IR-ARENA:view IR-ID:ir-attr-id -- CTARGET:arch )
   F-ARCH FEMEM N>ARCH ;

: FABI@ ( IR-ARENA:view IR-ID:ir-attr-id -- CTARGET:abi )
   F-ABI FEMEM N>ABI ;

: FENDIAN@ ( IR-ARENA:view IR-ID:ir-attr-id -- CTARGET:endian )
   F-END FEMEM N>ENDN ;

: FPTR-WIDTH@ ( IR-ARENA:view IR-ID:ir-attr-id -- CTARGET:ptr-width )
   F-PTRW FEMEM N>PTRW ;

: FRENDER ( IR-ARENA:view IR-ARENA:view IR-ID:ir-attr-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-attr-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   pv rv q cap 0 l FR-EMIT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
