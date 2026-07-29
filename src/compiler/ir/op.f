\ op.f - the append-only operation and value store: one row per operation, one
\ row per SSA value, and one shared cell pool holding every operation's operand,
\ result, successor, and attribute window.
\
\ docs/compiler-ir-design.md section 6.3 (the operation table and the value
\ table), section 6.4 (the ADD-OP/ADD-OPERAND/ADD-RESULT/ADD-SUCCESSOR/ADD-ATTR
\ builder API), section 6.5 (the freeze checks that consume these rows), and
\ section 7.2 (the stage that fills them). This file stores operations and
\ values; it does not define what any dialect's operations mean, which section
\ 5.3 leaves to the dialect's schema table.
\
\ ONE CONCERN, TWO ROW TABLES. Operations and values are one responsibility, not
\ two, because they are defined by the same act: appending an operation is what
\ brings its result values into existence, and a value's whole identity is the
\ operation and result position that defined it. Splitting them across two files
\ would make each depend on the other's live count - the value table needs the
\ operation count to validate a defining operation, the operation table needs the
\ value count to validate an operand - and would break the single all-or-nothing
\ append this file guarantees. They therefore share one package and one file, and
\ the file keeps them in separate row tables so a later canonical encoder can
\ order and renumber each independently.
\
\ IDENTITY COMES FROM IR-ID. Operations are the existing IR-ID ir-op-id family
\ and values the ir-value-id family, both packed under this module's key. This
\ file mints no identity family of its own, no raw converter, and no parallel
\ numbering. An operation names its opcode the way IR-SCHEMA names a schema: by
\ the module's interned symbol for that opcode, resolved through the dialect's
\ schema table. There is no second opcode numbering to keep in step.
\
\ WHAT AN OPERATION ROW RECORDS (design lines 412-426).
\   opcode name symbol   - design line 417's dialect-opcode half; the dialect
\                          half belongs to the schema table the opcode resolves
\                          in, exactly as IR-SCHEMA decided
\   operand window       - design line 419
\   result window        - design line 420
\   successor window     - design line 422
\   attribute window     - design line 421
\   source span          - design line 423, as the (source, start, length)
\                          triple IR-SOURCE:span already defines
\ Three section 6.3 fields are deliberately absent, each because another stage
\ owns it and duplicating it here would create a second authority:
\   parent-block (design line 418) belongs to the function and block structure
\     stage, which owns parents and their windows;
\   effect-class (design line 424) is the schema's field (design line 238) and is
\     read through the opcode's schema, so an operation cannot contradict the
\     closed world its dialect declared;
\   flags (design line 425) names no member anywhere in the design, and an empty
\     flag word is not a capability: the stage that needs a flag adds it together
\     with the meaning that makes it checkable.
\
\ WHAT A VALUE ROW RECORDS (design lines 428-438). Its type, its definition
\ kind, the identity that defined it, and the position within that definition.
\ Section 6.3 fixes the two definition kinds - block argument or operation
\ result - so they are a closed ENUM here. Only the operation-result kind has a
\ minting path in this stage, because a block argument cannot exist before the
\ block table does; the block-argument code is stored, decoded, and rejected by
\ the operation-result readers with E-IR-OP-KIND, in the same shape IR-SCHEMA
\ uses for a field its effect shape does not carry.
\
\ APPENDING IS ALL-OR-NOTHING. Every check - the stores' headers and module
\ binding, the staged fields, the opcode's schema, the arity rules, the SSA rule,
\ each referenced type, attribute, block, and source span, and all three
\ committed ceilings - runs before the first IR-ARENA:PUSH. A rejected append
\ therefore leaves the store exactly as it found it: no operation row, no value
\ row, and no pool cell.
\
\ OPERANDS CANNOT REACH FORWARD. An operand must name a value that is already
\ defined, so its ordinal is strictly below the live value count at the moment
\ the operation is appended - the same discipline the type table uses for its
\ references. An operation's own results are appended after its operands have
\ been validated, so an operation cannot take its own result, and a use-before-
\ definition cycle needs some value to name an ordinal at or past the count and
\ dies with E-IR-OP-SSA before anything is written. Dominance is therefore a
\ property of the construction order rather than something a later pass has to
\ detect. Block successors are the one reference that legitimately points
\ forward - a branch to a block that is still being built is ordinary SSA
\ construction - so this stage validates a successor's owning module and leaves
\ the existence check to the block table and the section 6.5 freeze verifier.
\
\ ARITY AGREES WITH THE SCHEMA. The opcode's schema declares how many operands
\ and results the operation has and whether the last listed type is a variadic
\ tail (design lines 234-235). Without a tail the counts must match exactly;
\ with one, the fixed part is one shorter and there is no upper bound, because
\ the tail type describes every further entry and may describe none. A
\ terminator's successor count is an exact match, since the schema records it as
\ a plain count. Attribute counts are not constrained here: the schema records
\ required attribute KEYS while an operation carries attribute VALUES, and
\ matching one against the other needs the attribute table's key reader, which
\ is the section 6.5 freeze verifier's job together with the operand and result
\ type rules.
\
\ WINDOWS TILE THE POOL EXACTLY. All four windows live in one cell pool, and an
\ append writes them in a fixed order: operands, results, successors, then
\ attributes. Every row's windows therefore continue exactly where the row
\ before it ended, with no gap and no overlap, and that tiling is what every
\ window read revalidates: the first window must start where the previous row
\ finished, each following window must start where the previous one finished,
\ and the last must end inside the live pool. Non-overlap becomes a constant-cost
\ check on the row itself instead of a search over every other row, and a row
\ appended past this package's constructors cannot claim a window that overlaps
\ another operation's or reaches past the cells the pool holds.
\
\ STORE SHAPES. Three IR-ARENA arenas owned by the compilation context: the cell
\ pool, the value row table, and the operation row table. Each carries the usual
\ three-cell header - format tag, owning module serial, committed capacity - so
\ presenting one store where another belongs is a format-tag reject rather than a
\ misread, and every access rechecks shape, window, and stored ordinals
\ fail-closed with E-IR-OP-STATE.
\
\ CEILINGS AND THE CONTEXT MAPPING. Capacities are creation parameters bounded
\ by the ordinal range a row or pool cell can address. The practical limit today
\ is smaller and belongs to the context: IR-CTX maps 64K per context, so all of a
\ module's tables together have about eight thousand cells to share, which is a
\ test-scale module rather than a production one. Enlarging or chunking that
\ mapping changes the memory profile of every table that shares it, so it is its
\ own designed capacity decision and its own dot, not a side effect of this
\ store. Hitting a committed ceiling here is a named error thrown before any
\ mutation, so a full store stays readable and keeps every identity it issued.
\
\ NOTHING PUBLIC MUTATES A FROZEN STORE. Every word here that writes takes an
\ IR-ARENA:arena builder handle, and the checker rejects an IR-ARENA:view in that
\ position, so a mutation cannot even be spelled against a frozen store. Once
\ IR-ARENA:FREEZE consumes a handle, the retired handle rejects every live word
\ with E-IR-ARENA-FROZEN. There is no publication, retirement, or lifecycle word
\ in this file: the module freeze belongs to the builder stage, and this stage
\ leaves it nothing to retract.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f

package IR-OP
public

\ Design line 434: a value is defined either as a block argument or as an
\ operation result. The vocabulary is closed, so an unknown definition kind is
\ unrepresentable in checked code; the wire codes below persist it.
ENUM def-kind DERIVE eq
   op-result
   blk-arg
;ENUM

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner comparison.
\ Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$4F504C31 constant OPL-MAGIC         \ "OPL1": the cell-pool header format tag
$4F505631 constant OPV-MAGIC         \ "OPV1": the value-table header format tag
$4F505231 constant OPR-MAGIC         \ "OPR1": the operation-table header format tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-OPC                   \ design line 417: the opcode name symbol
1 constant OFF-OPST                  \ design line 419: the operand window
2 constant OFF-OPN
3 constant OFF-RSST                  \ design line 420: the result window
4 constant OFF-RSN
5 constant OFF-SCST                  \ design line 422: the successor window
6 constant OFF-SCN
7 constant OFF-ATST                  \ design line 421: the attribute window
8 constant OFF-ATN
9 constant OFF-SRC                   \ design line 423: the source span
10 constant OFF-SBEG
11 constant OFF-SLEN
12 constant ROW-CELLS

0 constant OFF-VTYP                  \ design line 433: the value's type
1 constant OFF-VKIND                 \ design line 434: block argument or result
2 constant OFF-VDEF                  \ design line 435: what defined it
3 constant OFF-VPOS                  \ design line 436: where in that definition
4 constant VROW-CELLS

$FFFFFFFF HDR-CELLS - ROW-CELLS / constant ROW-CAP-MAX
$FFFFFFFF HDR-CELLS - VROW-CELLS / constant VAL-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX
64 constant ARITY-MAX                \ committed staged entries per list

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump in the canonical encoder.
0 constant DK-RESULT
1 constant DK-BLOCK

: KIND-CODE ( IR-OP:def-kind -- n )
   MATCH def-kind
      op-result OF DK-RESULT ENDOF
      blk-arg   OF DK-BLOCK ENDOF
   ;MATCH ;

\ A stored code outside the family's vocabulary is a corrupted or forged row, so
\ the decoder rejects it named instead of reading as some other definition.
: N>KIND ( n -- IR-OP:def-kind )
   case
      DK-RESULT of IR--OP-DEF--KIND:OP-RESULT endof
      DK-BLOCK  of IR--OP-DEF--KIND:BLK-ARG endof
      E-IR-OP-STATE throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-OP-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-OP-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-OP-STATE throw then ;

: VSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-OP-STATE throw then
   HDR-CELLS - VROW-CELLS mod 0 <> if E-IR-OP-STATE throw then ;

: MAGIC-CK ( n n -- )
   <> if E-IR-OP-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: p:IR-ARENA:arena :}
   p IR-ARENA:USED PSHAPE-CK
   OPL-MAGIC p HC-MAGIC LCELL@ MAGIC-CK ;

: VHDR-CK ( IR-ARENA:arena -- )
   {: v:IR-ARENA:arena :}
   v IR-ARENA:USED VSHAPE-CK
   OPV-MAGIC v HC-MAGIC LCELL@ MAGIC-CK ;

: RHDR-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r IR-ARENA:USED RSHAPE-CK
   OPR-MAGIC r HC-MAGIC LCELL@ MAGIC-CK ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: p:IR-ARENA:view :}
   p IR-ARENA:SIZE PSHAPE-CK
   OPL-MAGIC p HC-MAGIC FCELL@ MAGIC-CK ;

: FVHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE VSHAPE-CK
   OPV-MAGIC v HC-MAGIC FCELL@ MAGIC-CK ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: r:IR-ARENA:view :}
   r IR-ARENA:SIZE RSHAPE-CK
   OPR-MAGIC r HC-MAGIC FCELL@ MAGIC-CK ;

: CNT ( IR-ARENA:arena -- n )
   dup RHDR-CK IR-ARENA:USED HDR-CELLS - ROW-CELLS / ;

: FCNT ( IR-ARENA:view -- n )
   dup FRHDR-CK IR-ARENA:SIZE HDR-CELLS - ROW-CELLS / ;

: VCNT ( IR-ARENA:arena -- n )
   dup VHDR-CK IR-ARENA:USED HDR-CELLS - VROW-CELLS / ;

: FVCNT ( IR-ARENA:view -- n )
   dup FVHDR-CK IR-ARENA:SIZE HDR-CELLS - VROW-CELLS / ;

: PCELLS ( IR-ARENA:arena -- n )
   dup PHDR-CK IR-ARENA:USED HDR-CELLS - ;

: FPCELLS ( IR-ARENA:view -- n )
   dup FPHDR-CK IR-ARENA:SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-OP-OWNER throw then ;

\ The three stores are one table only when all three carry the same owning
\ module serial, so a cross-module trio rejects before a row window is trusted
\ against the wrong pool.
: TRIO-CK ( IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   p PHDR-CK
   v VHDR-CK
   r RHDR-CK
   p HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK
   v HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FTRIO-CK ( IR-ARENA:view IR-ARENA:view IR-ARENA:view -- )
   {: p:IR-ARENA:view v:IR-ARENA:view r:IR-ARENA:view :}
   p FPHDR-CK
   v FVHDR-CK
   r FRHDR-CK
   p HC-SERIAL FCELL@ r HC-SERIAL FCELL@ SERIAL-CK
   v HC-SERIAL FCELL@ r HC-SERIAL FCELL@ SERIAL-CK ;

: RKEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: VKEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: v:IR-ARENA:arena key:IR-ID:ir-module-key :}
   v VHDR-CK
   v HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

\ A window reader holds only the pool and the row table, so it checks that
\ pairing itself: both stores are what their tags claim, both carry the same
\ owning module serial, and that serial is the presented key's. A pool from
\ another module can therefore never be read through this module's windows.
: PR-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   p PHDR-CK
   r key RKEY-CK
   p HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FRKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: r:IR-ARENA:view key:IR-ID:ir-module-key :}
   r FRHDR-CK
   r HC-SERIAL FCELL@ key KEY-SERIAL SERIAL-CK ;

: FVKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key :}
   v FVHDR-CK
   v HC-SERIAL FCELL@ key KEY-SERIAL SERIAL-CK ;

: FPR-CK ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key -- )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key :}
   p FPHDR-CK
   r key FRKEY-CK
   p HC-SERIAL FCELL@ r HC-SERIAL FCELL@ SERIAL-CK ;

\ ---- identity projections ----------------------------------------------------
: SYM-ORD ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-LOCAL ;

: SYM-OWNER ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

: TYP-OWNER ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

: ATTR-OWNER ( IR-ID:ir-attr-id -- n )
   IR-ID:ATTR-OWNER MID-SERIAL ;

: VAL-OWNER ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-OWNER MID-SERIAL ;

: BLK-OWNER ( IR-ID:ir-block-id -- n )
   IR-ID:BLOCK-OWNER MID-SERIAL ;

: SRC-OWNER ( IR-ID:ir-source-id -- n )
   IR-ID:SOURCE-OWNER MID-SERIAL ;

\ A stored ordinal only has to be non-negative here: the table that owns the
\ reference revalidates the id it is handed, so a corrupted row cannot make the
\ symbol interner, the type table, or the attribute table read a row it never
\ built.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-OP-STATE throw then ;

: ROW-ORD ( n n IR-ID:ir-op-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-op-id :}
   id IR-ID:OP-OWNER MID-SERIAL hs SERIAL-CK
   id IR-ID:OP-LOCAL
   dup 0 < over cnt >= or if E-IR-OP-BOUND throw then ;

: VAL-ORD ( n n IR-ID:ir-value-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-value-id :}
   id VAL-OWNER hs SERIAL-CK
   id IR-ID:VALUE-LOCAL
   dup 0 < over cnt >= or if E-IR-OP-BOUND throw then ;

: ROW-AT ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-op-id :}
   r HC-SERIAL LCELL@ r CNT id ROW-ORD ;

: FROW-AT ( IR-ARENA:view IR-ID:ir-op-id -- n )
   {: r:IR-ARENA:view id:IR-ID:ir-op-id :}
   r HC-SERIAL FCELL@ r FCNT id ROW-ORD ;

: VAL-AT ( IR-ARENA:arena IR-ID:ir-value-id -- n )
   {: v:IR-ARENA:arena id:IR-ID:ir-value-id :}
   v HC-SERIAL LCELL@ v VCNT id VAL-ORD ;

: FVAL-AT ( IR-ARENA:view IR-ID:ir-value-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-value-id :}
   v HC-SERIAL FCELL@ v FVCNT id VAL-ORD ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: VROW-CELL ( n n -- n )
   swap VROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: VC@ ( IR-ARENA:arena n n -- n )
   VROW-CELL LCELL@ ;

: FVC@ ( IR-ARENA:view n n -- n )
   VROW-CELL FCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

: FPC@ ( IR-ARENA:view n -- n )
   HDR-CELLS + FCELL@ ;

\ ---- the window tiling -------------------------------------------------------
: LEN-OK ( n -- n )
   dup 0 < if E-IR-OP-STATE throw then ;

: STEP-CK ( n n -- )
   <> if E-IR-OP-WINDOW throw then ;

: ROW-END ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena l:n :}
   r l OFF-ATST RC@ LEN-OK  r l OFF-ATN RC@ LEN-OK + ;

: FROW-END ( IR-ARENA:view n -- n )
   {: r:IR-ARENA:view l:n :}
   r l OFF-ATST FRC@ LEN-OK  r l OFF-ATN FRC@ LEN-OK + ;

\ Revalidate one row's four windows as an exact tiling of the pool: the operand
\ window continues where the row before it ended, each following window
\ continues where the previous one ended, and the last ends inside the live
\ cells. Overlap and gaps are both impossible once this holds.
: TILE-CK ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   l 0= if 0 else r l 1- ROW-END then {: at:n :}
   at r l OFF-OPST RC@ STEP-CK
   at r l OFF-OPN RC@ LEN-OK + {: a1:n :}
   a1 r l OFF-RSST RC@ STEP-CK
   a1 r l OFF-RSN RC@ LEN-OK + {: a2:n :}
   a2 r l OFF-SCST RC@ STEP-CK
   a2 r l OFF-SCN RC@ LEN-OK + {: a3:n :}
   a3 r l OFF-ATST RC@ STEP-CK
   a3 r l OFF-ATN RC@ LEN-OK + p PCELLS > if E-IR-OP-STATE throw then ;

: FTILE-CK ( IR-ARENA:view IR-ARENA:view n -- )
   {: p:IR-ARENA:view r:IR-ARENA:view l:n :}
   l 0= if 0 else r l 1- FROW-END then {: at:n :}
   at r l OFF-OPST FRC@ STEP-CK
   at r l OFF-OPN FRC@ LEN-OK + {: a1:n :}
   a1 r l OFF-RSST FRC@ STEP-CK
   a1 r l OFF-RSN FRC@ LEN-OK + {: a2:n :}
   a2 r l OFF-SCST FRC@ STEP-CK
   a2 r l OFF-SCN FRC@ LEN-OK + {: a3:n :}
   a3 r l OFF-ATST FRC@ STEP-CK
   a3 r l OFF-ATN FRC@ LEN-OK + p FPCELLS > if E-IR-OP-STATE throw then ;

\ One element of a stored window, read only after the row's tiling holds.
: WIN@ ( IR-ARENA:arena IR-ARENA:arena n n n n -- n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena l:n stoff:n lenoff:n i:n :}
   p r l TILE-CK
   r l lenoff RC@ {: ln:n :}
   i 0 < i ln >= or if E-IR-OP-BOUND throw then
   p  r l stoff RC@ i +  PC@ ORD-OK ;

: FWIN@ ( IR-ARENA:view IR-ARENA:view n n n n -- n )
   {: p:IR-ARENA:view r:IR-ARENA:view l:n stoff:n lenoff:n i:n :}
   p r l FTILE-CK
   r l lenoff FRC@ {: ln:n :}
   i 0 < i ln >= or if E-IR-OP-BOUND throw then
   p  r l stoff FRC@ i +  FPC@ ORD-OK ;

\ ---- the staged operation ----------------------------------------------------
\ One package-owned stage under the single-task compilation discipline, in the
\ shape IR-TYPE, IR-ATTR, and IR-SCHEMA established: BEGIN-OP opens it, the ADD-
\ and SET- words fill it, and END-OP validates everything and appends. Any end
\ consumes the stage, so neither a rejected append nor an end without a begin
\ can leave a half-staged operation behind.
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

\ The four staged lists share one pair of arrays, addressed by a list index and
\ a fixed segment base, because a Habu word cannot take a storage array as an
\ argument: one indexed pair keeps the append and validate helpers shared
\ instead of written out four times.
4 constant LIST#
0 constant L-OP                      \ operand value ordinals
1 constant L-RS                      \ result type ordinals
2 constant L-SC                      \ successor block ordinals
3 constant L-AT                      \ attribute ordinals

here CELL 1- and CELL swap - CELL 1- and allot
variable STG-MODE
MODE-NONE STG-MODE !
variable STG-OPC
variable STG-OPCO
variable STG-SRC
variable STG-SRCO
variable STG-SBEG
variable STG-SLEN
create STG-V LIST# ARITY-MAX * cells allot
create STG-O LIST# ARITY-MAX * cells allot
create STG-N LIST# cells allot

: SV@ ( n -- n )
   cells STG-V + @ ;

: SV! ( n n -- )
   cells STG-V + ! ;

: SO@ ( n -- n )
   cells STG-O + @ ;

: SO! ( n n -- )
   cells STG-O + ! ;

: SN@ ( n -- n )
   cells STG-N + @ ;

: SN! ( n n -- )
   cells STG-N + ! ;

: SEG ( n -- n )
   ARITY-MAX * ;

: STG-RESET ( -- )
   LIST# 0 ?do
      0 i SN!
   loop
   UNSET STG-SRC !  0 STG-SRCO !  0 STG-SBEG !  0 STG-SLEN ! ;

: STG-OPEN-CK ( -- )
   STG-MODE @ MODE-OPEN <> if E-IR-OP-STAGE throw then ;

: STG-TAKE ( -- )
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have MODE-OPEN <> if E-IR-OP-STAGE throw then ;

: LIST-ROOM ( n -- )
   SN@ ARITY-MAX >= if E-IR-OP-ARITY throw then ;

: ORD+ ( n n n -- )
   {: l:n ord:n owner:n :}
   l LIST-ROOM
   l SN@ {: n:n :}
   ord l SEG n + SV!
   owner l SEG n + SO!
   n 1+ l SN! ;

\ ---- staged validation -------------------------------------------------------
: OWNED-CK ( n n -- )
   {: hs:n owner:n :}
   owner hs SERIAL-CK ;

\ Design lines 432-437 with the acyclicity rule above: an operand names a value
\ that is already defined, so its ordinal is strictly below the live count.
: OPERANDS-CK ( n n -- )
   {: hs:n vcnt:n :}
   L-OP SN@ 0 ?do
      hs L-OP SEG i + SO@ OWNED-CK
      L-OP SEG i + SV@ dup 0 < swap vcnt >= or if E-IR-OP-SSA throw then
   loop ;

\ Every staged result type is a real type of this module's type table.
: RESULTS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: tr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   L-RS SN@ 0 ?do
      key KEY-SERIAL L-RS SEG i + SO@ OWNED-CK
      tr key L-RS SEG i + SV@ IR-ID:PACK-TYPE IR-TYPE:KIND@ drop
   loop ;

\ Every staged attribute is a real attribute of this module's attribute table.
: ATTRS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: ar:IR-ARENA:arena key:IR-ID:ir-module-key :}
   L-AT SN@ 0 ?do
      key KEY-SERIAL L-AT SEG i + SO@ OWNED-CK
      ar key L-AT SEG i + SV@ IR-ID:PACK-ATTR IR-ATTR:KIND@ drop
   loop ;

\ A successor names a block of this module. Its existence is the block table's
\ fact and the section 6.5 freeze verifier's check, because a branch to a block
\ that is still being built is ordinary SSA construction.
: SUCCS-CK ( n -- )
   {: hs:n :}
   L-SC SN@ 0 ?do
      hs L-SC SEG i + SO@ OWNED-CK
      L-SC SEG i + SV@ 0 < if E-IR-OP-BOUND throw then
   loop ;

\ Design lines 234-235: an exact count without a variadic tail; with one, the
\ fixed part is one shorter and the tail describes every further entry, of which
\ there may be none.
: LIST-ARITY-CK ( n n bool -- )
   {: want:n have:n tail:bool :}
   tail if
      have want 1- < if E-IR-OP-ARITY throw then
   else
      have want <> if E-IR-OP-ARITY throw then
   then ;

: EXACT-ARITY-CK ( n n -- )
   <> if E-IR-OP-ARITY throw then ;

: SCHEMA-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: sr:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   sr op IR-SCHEMA:OPERANDS L-OP SN@ sr op IR-SCHEMA:OPERAND-TAIL? LIST-ARITY-CK
   sr op IR-SCHEMA:RESULTS L-RS SN@ sr op IR-SCHEMA:RESULT-TAIL? LIST-ARITY-CK
   sr op IR-SCHEMA:SUCCESSORS L-SC SN@ EXACT-ARITY-CK ;

: FIELDS-CK ( -- )
   STG-SRC @ UNSET = if E-IR-OP-FIELD throw then ;

: SPAN-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL STG-SRCO @ OWNED-CK
   sa  key STG-SRC @ IR-ID:PACK-SOURCE STG-SBEG @ STG-SLEN @ IR--SOURCE-SPAN:MAKE
   IR-SOURCE:SPAN-CK ;

\ ---- room and append ---------------------------------------------------------
: STAGED-CELLS ( -- n )
   L-OP SN@ L-RS SN@ + L-SC SN@ + L-AT SN@ + ;

: ROOM-CK ( IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   r CNT r HC-CAP LCELL@ >= if E-IR-OP-CAP throw then
   v VCNT L-RS SN@ + v HC-CAP LCELL@ > if E-IR-OP-CAP throw then
   p PCELLS STAGED-CELLS + p HC-CAP LCELL@ > if E-IR-OP-CAP throw then ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: LIST-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena l:n :}
   l SN@ 0 ?do
      c p l SEG i + SV@ CELL+
   loop ;

\ The result window holds the value ordinals this append is about to mint, so
\ the pool records results the same way it records operands: as value ordinals.
: RESULTS-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena base:n :}
   L-RS SN@ 0 ?do
      c p base i + CELL+
   loop ;

\ The four window starts of a row whose cells begin at st, in the order the
\ append writes them. This is the tiling TILE-CK later revalidates.
: WIN-STARTS ( n -- n n n n )
   {: st:n :}
   st
   st L-OP SN@ +
   st L-OP SN@ + L-RS SN@ +
   st L-OP SN@ + L-RS SN@ + L-SC SN@ + ;

: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena st:n :}
   st WIN-STARTS {: sop:n srs:n ssc:n sat:n :}
   c r STG-OPC @ CELL+
   c r sop CELL+   c r L-OP SN@ CELL+
   c r srs CELL+   c r L-RS SN@ CELL+
   c r ssc CELL+   c r L-SC SN@ CELL+
   c r sat CELL+   c r L-AT SN@ CELL+
   c r STG-SRC @ CELL+
   c r STG-SBEG @ CELL+
   c r STG-SLEN @ CELL+ ;

: VROWS-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx v:IR-ARENA:arena l:n :}
   L-RS SN@ 0 ?do
      c v L-RS SEG i + SV@ CELL+
      c v IR--OP-DEF--KIND:OP-RESULT KIND-CODE CELL+
      c v l CELL+
      c v i CELL+
   loop ;

\ ---- creation checks ---------------------------------------------------------
: ROW-CAP-OK ( n -- )
   dup 1 < over ROW-CAP-MAX > or if E-IR-OP-CAP throw then
   drop ;

: VAL-CAP-OK ( n -- )
   dup 0 < over VAL-CAP-MAX > or if E-IR-OP-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 0 < over POOL-CAP-MAX > or if E-IR-OP-CAP throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Create one module's operation store: a cell pool committed to exactly pcap
\ cells, a value table committed to exactly vcap values, and an operation table
\ committed to exactly ocap operations, all three headers bound to key's module
\ serial. The three handles plus the key are the store; they stay with the
\ module owner and die with the owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key ocap:n vcap:n pcap:n :}
   ocap ROW-CAP-OK
   vcap VAL-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p OPL-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c vcap VROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: v:IR-ARENA:arena :}
   c v OPV-MAGIC CELL+
   c v key KEY-SERIAL CELL+
   c v vcap CELL+
   c ocap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r OPR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r ocap CELL+
   p v r ;

\ ---- the operation builder (design lines 497-521) ----------------------------
\ Open one operation, named by the module's interned symbol for its opcode.
: BEGIN-OP ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   STG-MODE @ MODE-NONE <> if E-IR-OP-STAGE throw then
   MODE-OPEN STG-MODE !
   STG-RESET
   op SYM-ORD STG-OPC !
   op SYM-OWNER STG-OPCO ! ;

\ Abandon an open operation without appending it. The stage exists only between
\ a begin and an end, so this is the explicit end for a caller that decides not
\ to append after all.
: ABANDON ( -- )
   STG-TAKE ;

\ Design line 510: one operand, naming a value this module has already defined.
: ADD-OPERAND ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   STG-OPEN-CK
   L-OP val IR-ID:VALUE-LOCAL val VAL-OWNER ORD+ ;

\ Design line 511: one result of this type. Its value identity is minted by the
\ append, because a result cannot name its defining operation before that
\ operation exists.
: ADD-RESULT ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   STG-OPEN-CK
   L-RS t IR-ID:TYPE-LOCAL t TYP-OWNER ORD+ ;

\ Design line 512: one control-flow successor of this terminator.
: ADD-SUCCESSOR ( IR-ID:ir-block-id -- )
   {: b:IR-ID:ir-block-id :}
   STG-OPEN-CK
   L-SC b IR-ID:BLOCK-LOCAL b BLK-OWNER ORD+ ;

\ Design line 513: one attribute this operation carries.
: ADD-ATTR ( IR-ID:ir-attr-id -- )
   {: a:IR-ID:ir-attr-id :}
   STG-OPEN-CK
   L-AT a IR-ID:ATTR-LOCAL a ATTR-OWNER ORD+ ;

\ Design line 518: where this operation came from.
: SET-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   STG-OPEN-CK
   STG-SRC @ UNSET <> if E-IR-OP-STAGE throw then
   src IR-ID:SOURCE-LOCAL STG-SRC !
   src SRC-OWNER STG-SRCO !
   st STG-SBEG !
   ln STG-SLEN ! ;

\ Close the staged operation: validate it whole against these stores, the
\ opcode's schema, the module's type, attribute, and source tables, and all
\ three committed ceilings, then append its pool cells, its operation row, and
\ one value row per result. The stage is consumed whatever the outcome, and
\ every check runs before the first cell is written, so an append either lands
\ whole or changes nothing.
: END-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   STG-TAKE
   p v r TRIO-CK
   r key RKEY-CK
   FIELDS-CK
   key KEY-SERIAL STG-OPCO @ OWNED-CK
   sr key STG-OPC @ IR-ID:PACK-SYMBOL SCHEMA-CK
   key KEY-SERIAL v VCNT OPERANDS-CK
   tr key RESULTS-CK
   ar key ATTRS-CK
   key KEY-SERIAL SUCCS-CK
   sa key SPAN-CK
   p v r ROOM-CK
   p PCELLS {: st:n :}
   r CNT {: l:n :}
   v VCNT {: base:n :}
   c p L-OP LIST-ADD
   c p base RESULTS-ADD
   c p L-SC LIST-ADD
   c p L-AT LIST-ADD
   c r st ROW-ADD
   c v l VROWS-ADD
   key l IR-ID:PACK-OP ;

\ ---- table readers -----------------------------------------------------------
: OPS ( IR-ARENA:arena -- n )
   CNT ;

: VALUES ( IR-ARENA:arena -- n )
   VCNT ;

: POOL-CELLS ( IR-ARENA:arena -- n )
   PCELLS ;

private

: FLD ( IR-ARENA:arena IR-ID:ir-op-id n -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-op-id off:n :}
   r  r id ROW-AT  off RC@ ;

: FFLD ( IR-ARENA:view IR-ID:ir-op-id n -- n )
   {: r:IR-ARENA:view id:IR-ID:ir-op-id off:n :}
   r  r id FROW-AT  off FRC@ ;

: VFLD ( IR-ARENA:arena IR-ID:ir-value-id n -- n )
   {: v:IR-ARENA:arena id:IR-ID:ir-value-id off:n :}
   v  v id VAL-AT  off VC@ ;

: FVFLD ( IR-ARENA:view IR-ID:ir-value-id n -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-value-id off:n :}
   v  v id FVAL-AT  off FVC@ ;

public

\ ---- operation readers -------------------------------------------------------
: OPCODE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   r key RKEY-CK
   key r id OFF-OPC FLD ORD-OK IR-ID:PACK-SYMBOL ;

: OPERANDS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   OFF-OPN FLD LEN-OK ;

: RESULTS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   OFF-RSN FLD LEN-OK ;

: SUCCESSORS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   OFF-SCN FLD LEN-OK ;

: ATTRS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   OFF-ATN FLD LEN-OK ;

\ The span this operation came from. A span is a value, so the consumer
\ revalidates it against the registry it names with IR-SOURCE:SPAN-CK, exactly
\ as IR-SOURCE requires of every holder.
: SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id -- IR-SOURCE:span )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   r key RKEY-CK
   r id ROW-AT {: l:n :}
   key r l OFF-SRC RC@ ORD-OK IR-ID:PACK-SOURCE
   r l OFF-SBEG RC@ LEN-OK
   r l OFF-SLEN RC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-OPST OFF-OPN i WIN@ IR-ID:PACK-VALUE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-RSST OFF-RSN i WIN@ IR-ID:PACK-VALUE ;

: SUCCESSOR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-SCST OFF-SCN i WIN@ IR-ID:PACK-BLOCK ;

: ATTR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-attr-id )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-ATST OFF-ATN i WIN@ IR-ID:PACK-ATTR ;

\ ---- value readers -----------------------------------------------------------
: VALUE-KIND@ ( IR-ARENA:arena IR-ID:ir-value-id -- IR-OP:def-kind )
   OFF-VKIND VFLD N>KIND ;

: VALUE-TYPE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: v:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   v key VKEY-CK
   key v id OFF-VTYP VFLD ORD-OK IR-ID:PACK-TYPE ;

private

\ A value's defining identity means different things in the two definition
\ kinds, so asking an operation-result reader about a block argument is a caller
\ error rather than a misread. A stored code outside the vocabulary dies in the
\ decoder first.
: RESULT-KIND-CK ( n -- )
   N>KIND IR--OP-DEF--KIND:OP-RESULT IR--OP-DEF--KIND:EQ
   0= if E-IR-OP-KIND throw then ;

public

\ The operation that defined this value, checked against the operation table
\ that owns the reference.
: VALUE-OP@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: v:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   v key VKEY-CK
   r key RKEY-CK
   v id OFF-VKIND VFLD RESULT-KIND-CK
   v id OFF-VDEF VFLD ORD-OK
   dup r CNT >= if E-IR-OP-BOUND throw then
   key swap IR-ID:PACK-OP ;

\ Which result of that operation this value is.
: VALUE-POS@ ( IR-ARENA:arena IR-ID:ir-value-id -- n )
   {: v:IR-ARENA:arena id:IR-ID:ir-value-id :}
   v id OFF-VKIND VFLD RESULT-KIND-CK
   v id OFF-VPOS VFLD LEN-OK ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its operations and values through the three arena views;
\ the retired builder handles reject every touch with E-IR-ARENA-FROZEN.
: FOPS ( IR-ARENA:view -- n )
   FCNT ;

: FVALUES ( IR-ARENA:view -- n )
   FVCNT ;

: FPOOL-CELLS ( IR-ARENA:view -- n )
   FPCELLS ;

: FOPCODE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   r key FRKEY-CK
   key r id OFF-OPC FFLD ORD-OK IR-ID:PACK-SYMBOL ;

: FOPERANDS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   OFF-OPN FFLD LEN-OK ;

: FRESULTS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   OFF-RSN FFLD LEN-OK ;

: FSUCCESSORS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   OFF-SCN FFLD LEN-OK ;

: FATTRS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   OFF-ATN FFLD LEN-OK ;

: FSPAN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id -- IR-SOURCE:span )
   {: r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   r key FRKEY-CK
   r id FROW-AT {: l:n :}
   key r l OFF-SRC FRC@ ORD-OK IR-ID:PACK-SOURCE
   r l OFF-SBEG FRC@ LEN-OK
   r l OFF-SLEN FRC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: FOPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key FPR-CK
   key p r  r id FROW-AT  OFF-OPST OFF-OPN i FWIN@ IR-ID:PACK-VALUE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key FPR-CK
   key p r  r id FROW-AT  OFF-RSST OFF-RSN i FWIN@ IR-ID:PACK-VALUE ;

: FSUCCESSOR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key FPR-CK
   key p r  r id FROW-AT  OFF-SCST OFF-SCN i FWIN@ IR-ID:PACK-BLOCK ;

: FATTR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-attr-id )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key FPR-CK
   key p r  r id FROW-AT  OFF-ATST OFF-ATN i FWIN@ IR-ID:PACK-ATTR ;

: FVALUE-KIND@ ( IR-ARENA:view IR-ID:ir-value-id -- IR-OP:def-kind )
   OFF-VKIND FVFLD N>KIND ;

: FVALUE-TYPE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   v key FVKEY-CK
   key v id OFF-VTYP FVFLD ORD-OK IR-ID:PACK-TYPE ;

: FVALUE-OP@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: v:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   v key FVKEY-CK
   r key FRKEY-CK
   v id OFF-VKIND FVFLD RESULT-KIND-CK
   v id OFF-VDEF FVFLD ORD-OK
   dup r FCNT >= if E-IR-OP-BOUND throw then
   key swap IR-ID:PACK-OP ;

: FVALUE-POS@ ( IR-ARENA:view IR-ID:ir-value-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-value-id :}
   v id OFF-VKIND FVFLD RESULT-KIND-CK
   v id OFF-VPOS FVFLD LEN-OK ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
