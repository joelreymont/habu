\ schema.f - the closed-world operation schema table: one validated record per
\ opcode, and the single authority on the shape an operation of that opcode may
\ have.
\
\ docs/compiler-ir-design.md section 5.3 (closed-world operation schemas, lines
\ 227-245), section 6.4 (the builder API, lines 497-521), section 6.5 (the
\ freeze checks that consume a schema, lines 523-545), section 6.7 (the witness
\ schema digest, line 602), and section 10.6 (schema name, major/minor version,
\ and schema digest, lines 1710-1719). This file defines the schema MACHINERY.
\ The dialect packages that fill a table with their own opcodes stay separate,
\ exactly as design line 317 says the shared substrate "does not define the
\ meaning of dialect-specific operations".
\
\ ONE TABLE IS ONE DIALECT. Design line 229 says "Each dialect has an
\ exhaustive operation family and one schema table", so the dialect is a
\ property of the table, not of each row: the row-table header carries the
\ dialect's interned name symbol together with the major and minor schema
\ version that design line 1714 requires. A row therefore records the opcode
\ half of design line 233 ("dialect and opcode") and inherits the dialect half
\ from the table it lives in. That is also what makes duplicate detection
\ simple and exact: two rows may not carry the same opcode name symbol.
\
\ IDENTITY IS THE INTERNED OPCODE NAME. A schema is named by the module's own
\ interned symbol for its opcode, an existing IR-ID ir-symbol-id. This file
\ mints no identity family of its own and no raw converter, and it does not
\ duplicate IR-ID's module-key packing. Every public reader takes that symbol,
\ resolves it through LOOKUP against this table, and throws E-IR-SCHEMA-OPCODE
\ when no row defines it: a name that is not an opcode of this dialect can name
\ nothing here. Combined with the exact decoders below, that is design line
\ 229's closed world - there is no path, forged rows included, by which a
\ reader hands back a value for an operation the table never defined.
\
\ WHAT A ROW RECORDS. One fixed-shape row per opcode, one cell per design line:
\   opcode name symbol                      - design line 233
\   operand type list, length, tail flag    - design lines 234, 235
\   result type list, length, tail flag     - design lines 234, 235
\   successor count and region count        - design line 236 (control-flow shape)
\   terminator flag                         - design line 237
\   effect class                            - design line 238
\   effect domain                           - design line 238, section 7.3 lines 753-762
\   memory space and alias behavior         - design line 239
\   may-trap flag                           - design line 240
\   required architecture and features      - design line 241
\   required attribute key list and count   - design line 479
\   extension-set flag                      - design line 479
\   semantic rule identifier symbol         - design line 242
\   renderer identifier symbol              - design line 243
\   tied operand list and count             - see the note below
\ Design line 234 asks for arity RULES rather than a bare count, so an operand
\ or result list is a fixed sequence of types optionally ending in a variadic
\ tail: the last listed type then describes every further operand or result.
\ Design line 235's type constraints are those listed ir-type-ids, each one
\ validated against the module's own type table.
\
\ THE CLOSED VOCABULARIES. The effect class and the alias behavior are ENUM
\ families declared here, because design lines 238 and 239 name the fields but
\ no section fixes their members. The effect classes follow section 7.3 line
\ 763 - "Pure arithmetic has no token" - so an operation either has no effect
\ token at all or reads, writes, or does both in exactly one domain. The alias
\ behaviors follow design line 779 ("a proven alias class") and line 1177 ("alias
\ group"). The effect domain and the memory space are NOT redeclared here: the
\ six domains of section 7.3 and the address spaces of the section 6.3 type
\ table already exist as IR-TYPE:domain and IR-TYPE:space, and reusing them
\ keeps one vocabulary per concept. Rows persist all four families as stable
\ wire codes, and every decoder is an exact case whose unmatched code throws
\ E-IR-SCHEMA-STATE, so a bypass-forged row rejects at first touch instead of
\ decoding as some other operation.
\
\ THE THREE EFFECT SHAPES. Design line 239 pairs "memory space and alias
\ behavior" with the effect class, and only the data-memory domain has an
\ address space and an alias class at all; the dictionary, code-publication,
\ I/O, process, and external-call domains are ordered by their token alone.
\ There are therefore exactly three ways to declare an effect, and any other
\ combination throws E-IR-SCHEMA-EFFECT:
\   SET-PURE     no token, no domain, no space, no alias
\   SET-MEMORY   the data-memory domain, with a space and an alias behavior
\   SET-TOKEN    one of the five non-memory domains, with neither
\ A pure operation presented to SET-MEMORY or SET-TOKEN, a non-memory domain
\ presented to SET-MEMORY, and the data-memory domain presented to SET-TOKEN
\ all reject. Readers mirror the shapes: DOMAIN@ rejects on a pure schema and
\ SPACE@/ALIAS@ reject on anything but a memory schema, with E-IR-SCHEMA-KIND.
\
\ THE TERMINATOR RULES. Design line 237 records terminator status and design
\ line 236 records the control-flow shape; design line 531 requires that
\ "every block ends in exactly one terminator" and design line 532 requires
\ that "successor argument counts and types match destination block arguments".
\ Two rules follow and both reject with E-IR-SCHEMA-TERM:
\   a non-terminator declares no successors - a block leaves only through its
\     one terminator (design lines 236, 531), so control-flow edges belong to
\     terminator schemas alone;
\   a terminator declares no results - section 7.2's block-parameter SSA passes
\     values to a successor as its block arguments (design lines 706-708) and
\     design line 532 makes those arguments the matched interface, so a
\     terminator has nowhere to put a result of its own.
\
\ TIED OPERANDS BELONG TO THE FORM. Some instruction forms name one register
\ field twice: the move-wide overwrite keeps the bits of its destination it does
\ not write, so the value it keeps and the value it produces are one register. In
\ SSA those are two values, and a register allocator has to put them in the same
\ physical register or the instruction means something else. That is a property
\ of the form, exactly as its operand types are, so a schema declares it the way
\ an instruction descriptor does: a list of ties, each naming one result ordinal
\ and the operand ordinal it shares a register with. The default is no tie, so a
\ form that declares none has none - by declaration, not by a reader's guess.
\ A tie is checked whole at definition and rejects with E-IR-SCHEMA-TIE: both
\ ordinals must name a fixed entry of their list, never a variadic tail, because
\ a tail stands for a run of operands and "the tail" names no single register;
\ the tied result and operand must have the same type, since one register holds
\ one value; and no result and no operand may be tied twice, because one field
\ cannot hold two values and two fields cannot be one. A terminator therefore
\ cannot tie anything at all - it has no results to tie.
\
\ TARGET LEGALITY COMES FROM THE BINDING. Design line 241 records the legal
\ target capabilities and design line 541 makes the freeze check "target-specific
\ operations are legal for the target contract". A schema declares the
\ architecture and the feature set its operation needs, and definition consumes
\ the owning context's validated binding (IR-CTX:BINDING@ -> CBIND:TARGET@) -
\ never a rederived legality table. A required architecture other than the
\ bound one, a required feature the bound contract does not have, and a feature
\ the required architecture cannot have at all each reject
\ E-IR-SCHEMA-TARGET, so a table never holds a schema its target cannot run.
\
\ DEFINITION IS A STAGED BUILDER. Design lines 497-521 ask for a small builder
\ API of separate ADD-* words rather than one wide constructor, and Habu words
\ cannot pass variable-length lists on the stack anyway, so a schema is built
\ through the staged protocol IR-TYPE and IR-ATTR established: BEGIN-OP opens
\ the stage, the ADD-* and SET-* words fill it, and DEFINE validates everything
\ and appends. One package-owned stage under the single-task compilation
\ discipline; a begin while one is open, a field declared twice, an end without
\ a begin, and a list past ARITY-MAX all reject E-IR-SCHEMA-STAGE or
\ E-IR-SCHEMA-ARITY, and any end consumes the stage, so no half-staged schema
\ leaks into the next definition. DEFINE requires every field design line 5.3
\ names that has no meaningful default - the effect, the control-flow shape and
\ terminator status, the trap flag, the target requirement, the semantic rule,
\ and the renderer - and a missing one throws E-IR-SCHEMA-FIELD.
\
\ STORE SHAPES. Two IR-ARENA arenas owned by the compilation context: a list
\ pool holding operand type ordinals, result type ordinals, and required
\ attribute key ordinals, and a row table holding one fixed-shape record per
\ opcode. The pool carries the usual three-cell header (format tag, owning
\ module serial, committed capacity); the row table carries three more (the
\ dialect name symbol ordinal, the major version, the minor version). Every
\ access rechecks shape, window, and stored references fail-closed
\ (E-IR-SCHEMA-STATE), so a holder who bypasses this package and appends raw
\ cells cannot make a reader touch cells outside the live ranges.
\
\ DIGESTS DETECT ANY CHANGE. Design line 602 binds a witness to a schema digest
\ and design line 1716 gives a dialect's schema table one. DIGEST computes a
\ record's digest over a canonical preimage in the CDIGEST slot form: the
\ domain-separation tag, this file's preimage version, the dialect and opcode
\ ordinals, every fixed field, and then the operand, result, and attribute-key
\ ordinals, with each list's length already fixed earlier in the preimage so the
\ encoding stays injective. TABLE-DIGEST chains the record digests: it seeds
\ with the table header and the row count and folds one record digest at a time,
\ so the table digest is deterministic, covers every row, and needs no buffer
\ that grows with the table. VERIFY recomputes the table digest and throws
\ E-IR-SCHEMA-DIGEST when a presented digest differs, which is how a caller
\ detects that the schema table it is holding is not the one an artifact was
\ built against.
\
\ WHAT THE DIGEST IS OVER, AND WHAT IT IS NOT. Every reference a schema records
\ - its opcode name, its dialect, its semantic rule and renderer, its required
\ attribute keys, its operand and result types - is stored and digested as the
\ module-local ordinal the owning table gave it, exactly as the type table
\ stores its own references (src/compiler/ir/type.f, the note on canonical
\ order). Two schemas of one module therefore digest differently whenever any
\ field differs, which is what VERIFY needs. What the digest is NOT is a
\ module-independent name for a dialect: those ordinals are insertion-ordered,
\ so the same dialect registered against two modules whose symbol interners saw
\ different strings first digests differently. A dialect that needs a schema
\ digest stable across compilations - which is what design line 1716's "schema
\ digest" and design line 602's witness binding ultimately want - must give its
\ schema table an interner and a type table of its own, so the ordinals are a
\ pure function of its registration sequence. Making that stability structural
\ instead of a usage rule is the section 6.6 canonical encoder's job, the same
\ boundary the type table draws: the encoder sorts structurally and renumbers
\ every embedded reference under the permutation it chose. This file must not
\ duplicate that authority, and the residual work is tracked as its own dot.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f

package IR-SCHEMA
public

\ The effect class an operation has (design line 238). Section 7.3 line 763
\ says pure arithmetic carries no effect token; everything else takes and
\ returns one in exactly one domain, reading it, writing it, or both.
ENUM effect DERIVE eq
   pure
   read
   write
   read-write
;ENUM

\ How an operation's memory access relates to other accesses (design line 239),
\ in the vocabulary of design line 779's proven alias class and design line
\ 1177's alias group. `unaliased` reaches only locations no other operation can
\ reach, `grouped` only the locations of its declared alias group, and
\ `unrestricted` any location in its memory space.
ENUM alias DERIVE eq
   unaliased
   grouped
   unrestricted
;ENUM

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner
\ comparison. Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$53434C31 constant SCL-MAGIC         \ "SCL1": the list-pool header format tag
$53435231 constant SCR-MAGIC         \ "SCR1": the row-table header format tag

0 constant PC-MAGIC
1 constant PC-SERIAL
2 constant PC-CAP
3 constant PHDR-CELLS

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HC-DIALECT                \ design lines 229, 233: this table's dialect
4 constant HC-MAJOR                  \ design line 1714: schema major version
5 constant HC-MINOR                  \ design line 1714: schema minor version
6 constant RHDR-CELLS

0 constant OFF-NAME                  \ design line 233
1 constant OFF-OPST                  \ design lines 234, 235
2 constant OFF-OPN
3 constant OFF-OPTAIL
4 constant OFF-RSST                  \ design lines 234, 235
5 constant OFF-RSN
6 constant OFF-RSTAIL
7 constant OFF-SUCC                  \ design line 236
8 constant OFF-REGN                  \ design line 236
9 constant OFF-ATST                  \ design line 479
10 constant OFF-ATN
11 constant OFF-ATEXT
12 constant OFF-EFF                  \ design line 238
13 constant OFF-DOM                  \ design line 238
14 constant OFF-SPC                  \ design line 239
15 constant OFF-ALI                  \ design line 239
16 constant OFF-TRAP                 \ design line 240
17 constant OFF-ARCH                 \ design line 241
18 constant OFF-FEAT                 \ design line 241
19 constant OFF-TERM                 \ design line 237
20 constant OFF-RULE                 \ design line 242
21 constant OFF-REND                 \ design line 243
22 constant OFF-TIST                 \ the tied-operand list
23 constant OFF-TIN
24 constant ROW-CELLS

\ A tie is stored as a pair of cells, the result ordinal then the operand
\ ordinal, so a stored tie list is twice as many cells as it holds ties.
2 constant TIE-PAIR
0 constant TIE-RS                    \ the result half of a stored pair
1 constant TIE-OP                    \ the operand half

: TIE-CELLS ( n -- n )
   TIE-PAIR * ;

256 constant CAP-MAX                 \ committed opcodes per dialect table
$FFFFFFFF PHDR-CELLS - constant POOL-MAX
32 constant ARITY-MAX                \ committed per-list stage ceiling
32 constant SUCC-MAX                 \ committed successors per terminator
8 constant REGION-MAX                \ committed regions per operation
$FFFF constant VERSION-MAX           \ committed schema major/minor ceiling
-1 constant UNUSED                   \ a field this schema's effect shape does not carry

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump in the canonical encoder.
0 constant EF-PURE
1 constant EF-READ
2 constant EF-WRITE
3 constant EF-RW

0 constant AL-UNALIASED
1 constant AL-GROUPED
2 constant AL-UNRESTRICTED

0 constant DM-DATA
1 constant DM-DICT
2 constant DM-CODE
3 constant DM-IO
4 constant DM-PROC
5 constant DM-FFI

0 constant SP-GENERIC
1 constant SP-GLOBAL
2 constant SP-SHARED
3 constant SP-LOCAL
4 constant SP-PARAM
5 constant SP-CONST

0 constant AR-AARCH64
1 constant AR-PTX

: EFF-CODE ( IR-SCHEMA:effect -- n )
   MATCH effect
      pure       OF EF-PURE ENDOF
      read       OF EF-READ ENDOF
      write      OF EF-WRITE ENDOF
      read-write OF EF-RW ENDOF
   ;MATCH ;

: ALI-CODE ( IR-SCHEMA:alias -- n )
   MATCH alias
      unaliased    OF AL-UNALIASED ENDOF
      grouped      OF AL-GROUPED ENDOF
      unrestricted OF AL-UNRESTRICTED ENDOF
   ;MATCH ;

: DOM-CODE ( IR-TYPE:domain -- n )
   MATCH IR-TYPE:domain
      data-mem OF DM-DATA ENDOF
      dict     OF DM-DICT ENDOF
      code-pub OF DM-CODE ENDOF
      io       OF DM-IO ENDOF
      process  OF DM-PROC ENDOF
      ffi      OF DM-FFI ENDOF
   ;MATCH ;

: SPC-CODE ( IR-TYPE:space -- n )
   MATCH IR-TYPE:space
      generic OF SP-GENERIC ENDOF
      global  OF SP-GLOBAL ENDOF
      shared  OF SP-SHARED ENDOF
      local   OF SP-LOCAL ENDOF
      param   OF SP-PARAM ENDOF
      const   OF SP-CONST ENDOF
   ;MATCH ;

: ARCH-CODE ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF AR-AARCH64 ENDOF
      ptx     OF AR-PTX ENDOF
   ;MATCH ;

\ ---- wire-code decoders ------------------------------------------------------
\ A stored code outside a family's vocabulary is a corrupted or forged row;
\ every decoder rejects it named, so a malformed record cannot read as an
\ operation the dialect never declared.
: N>EFF ( n -- IR-SCHEMA:effect )
   case
      EF-PURE  of IR--SCHEMA-EFFECT:PURE endof
      EF-READ  of IR--SCHEMA-EFFECT:READ endof
      EF-WRITE of IR--SCHEMA-EFFECT:WRITE endof
      EF-RW    of IR--SCHEMA-EFFECT:READ-WRITE endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: N>ALI ( n -- IR-SCHEMA:alias )
   case
      AL-UNALIASED    of IR--SCHEMA-ALIAS:UNALIASED endof
      AL-GROUPED      of IR--SCHEMA-ALIAS:GROUPED endof
      AL-UNRESTRICTED of IR--SCHEMA-ALIAS:UNRESTRICTED endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: N>DOM ( n -- IR-TYPE:domain )
   case
      DM-DATA of IR--TYPE-DOMAIN:DATA-MEM endof
      DM-DICT of IR--TYPE-DOMAIN:DICT endof
      DM-CODE of IR--TYPE-DOMAIN:CODE-PUB endof
      DM-IO   of IR--TYPE-DOMAIN:IO endof
      DM-PROC of IR--TYPE-DOMAIN:PROCESS endof
      DM-FFI  of IR--TYPE-DOMAIN:FFI endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: N>SPC ( n -- IR-TYPE:space )
   case
      SP-GENERIC of IR--TYPE-SPACE:GENERIC endof
      SP-GLOBAL  of IR--TYPE-SPACE:GLOBAL endof
      SP-SHARED  of IR--TYPE-SPACE:SHARED endof
      SP-LOCAL   of IR--TYPE-SPACE:LOCAL endof
      SP-PARAM   of IR--TYPE-SPACE:PARAM endof
      SP-CONST   of IR--TYPE-SPACE:CONST endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: N>ARCH ( n -- CTARGET:arch )
   case
      AR-AARCH64 of CTARGET-ARCH:AARCH64 endof
      AR-PTX     of CTARGET-ARCH:PTX endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: N>BOOL ( n -- bool )
   case
      0 of false endof
      1 of true endof
      E-IR-SCHEMA-STATE throw
   endcase ;

: BOOL>N ( bool -- n )
   if 1 else 0 then ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   PHDR-CELLS < if E-IR-SCHEMA-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup RHDR-CELLS < if E-IR-SCHEMA-STATE throw then
   RHDR-CELLS - ROW-CELLS mod 0 <> if E-IR-SCHEMA-STATE throw then ;

: PMAGIC-CK ( n -- )
   SCL-MAGIC <> if E-IR-SCHEMA-STATE throw then ;

: RMAGIC-CK ( n -- )
   SCR-MAGIC <> if E-IR-SCHEMA-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a PC-MAGIC LCELL@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r IR-ARENA:USED RSHAPE-CK
   r HC-MAGIC LCELL@ RMAGIC-CK ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE PSHAPE-CK
   v PC-MAGIC FCELL@ PMAGIC-CK ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE RSHAPE-CK
   v HC-MAGIC FCELL@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   RHDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED USED>CNT ;

: FCNT ( IR-ARENA:view -- n )
   IR-ARENA:SIZE USED>CNT ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED PHDR-CELLS - ;

: FPCELLS ( IR-ARENA:view -- n )
   IR-ARENA:SIZE PHDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-SCHEMA-OWNER throw then ;

\ The pair coupling: both stores are what their tags claim and both carry the
\ same owning module serial, so a cross-module pairing rejects before any row
\ window is trusted against the wrong pool.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a PHDR-CK
   r RHDR-CK
   a PC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FPAIR-CK ( IR-ARENA:view IR-ARENA:view -- )
   {: pv:IR-ARENA:view rv:IR-ARENA:view :}
   pv FPHDR-CK
   rv FRHDR-CK
   pv PC-SERIAL FCELL@ rv HC-SERIAL FCELL@ SERIAL-CK ;

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

\ ---- symbol and type projections ---------------------------------------------
: SYM-ORD ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-LOCAL ;

: SYM-OWNER ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

: TYP-ORD ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-LOCAL ;

: TYP-OWNER ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

\ A stored ordinal only has to be non-negative here: the owning table
\ revalidates the id it is handed, so a corrupted row cannot make the symbol
\ interner or the type table read a row it never built.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-SCHEMA-STATE throw then ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * RHDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   PHDR-CELLS + LCELL@ ;

: FPC@ ( IR-ARENA:view n -- n )
   PHDR-CELLS + FCELL@ ;

\ Every stored window revalidates against the pool's live cell range on every
\ access, so a forged or bypass-appended row rejects fail-closed instead of
\ reading cells the pool never stored.
: WIN-CK-N ( n n n -- )
   {: pc:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SCHEMA-STATE throw then
   st ln + pc > if E-IR-SCHEMA-STATE throw then ;

\ ---- opcode resolution -------------------------------------------------------
\ A schema is named by its opcode name symbol. LOOKUP is the only way a
\ reader reaches a row, so an opcode this dialect never defined names nothing.
: SCAN-NAME ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena ord:n :}
   -1
   r CNT 0 ?do
      r i OFF-NAME RC@ ord = if drop i leave then
   loop ;

: FSCAN-NAME ( IR-ARENA:view n -- n )
   {: rv:IR-ARENA:view ord:n :}
   -1
   rv FCNT 0 ?do
      rv i OFF-NAME FRC@ ord = if drop i leave then
   loop ;

: ROW-OF ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ op SYM-OWNER SERIAL-CK
   r op SYM-ORD SCAN-NAME
   dup 0 < if E-IR-SCHEMA-OPCODE throw then ;

: FROW-OF ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv FRHDR-CK
   rv HC-SERIAL FCELL@ op SYM-OWNER SERIAL-CK
   rv op SYM-ORD FSCAN-NAME
   dup 0 < if E-IR-SCHEMA-OPCODE throw then ;

\ ---- effect-shape projections ------------------------------------------------
\ A row carries a domain only when it is not pure, and a space and an alias
\ behaviour only in the data-memory domain (design line 239). Asking a schema
\ for a field its effect shape does not have is a caller error.
: SHAPE-CK ( bool -- )
   0= if E-IR-SCHEMA-KIND throw then ;

: HAS-DOM? ( n -- bool )
   EF-PURE <> ;

: HAS-MEM? ( n n -- bool )
   {: eff:n dom:n :}
   eff HAS-DOM? if dom DM-DATA = else false then ;

\ ---- the staged schema -------------------------------------------------------
\ One package-owned stage under the single-task compilation discipline. Each
\ single-valued field starts UNSET and refuses a second declaration, so a
\ schema is described exactly once and DEFINE can tell "never declared" from
\ "declared as zero".
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

\ The four staged lists share one pair of arrays, addressed by a list index and
\ a fixed segment base, because a Habu word cannot take a storage array as an
\ argument: one indexed pair keeps the append, validate, and copy helpers shared
\ instead of written out four times. Each entry occupies one slot of each array:
\ the first holds the entry's ordinal, and the second its companion cell - the
\ owning module serial for the three identity lists, and the tied operand
\ ordinal for the tie list, whose entries are two ordinals of this schema rather
\ than an identity of the module.
4 constant LIST#
0 constant L-OP
1 constant L-RS
2 constant L-AT
3 constant L-TI

here CELL 1- and CELL swap - CELL 1- and allot
variable STG-MODE
MODE-NONE STG-MODE !
variable STG-NAME
variable STG-NAMEO
variable STG-ATEXT
variable STG-EFF
variable STG-DOM
variable STG-SPC
variable STG-ALI
variable STG-TRAP
variable STG-CTRL
variable STG-TERM
variable STG-SUCC
variable STG-REGN
variable STG-ARCH
variable STG-FEAT
variable STG-RULE
variable STG-RULEO
variable STG-REND
variable STG-RENDO
create STG-V LIST# ARITY-MAX * cells allot
create STG-O LIST# ARITY-MAX * cells allot
create STG-N LIST# cells allot
create STG-T LIST# cells allot

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

: ST@ ( n -- n )
   cells STG-T + @ ;

: ST! ( n n -- )
   cells STG-T + ! ;

: SEG ( n -- n )
   ARITY-MAX * ;

: STG-RESET ( -- )
   LIST# 0 ?do
      0 i SN!
      0 i ST!
   loop
   0 STG-ATEXT !
   UNSET STG-EFF !  UNSET STG-DOM !  UNSET STG-SPC !  UNSET STG-ALI !
   UNSET STG-TRAP !  UNSET STG-CTRL !  UNSET STG-TERM !
   0 STG-SUCC !  0 STG-REGN !
   UNSET STG-ARCH !  0 STG-FEAT !
   UNSET STG-RULE !  0 STG-RULEO !
   UNSET STG-REND !  0 STG-RENDO ! ;

: STG-OPEN-CK ( -- )
   STG-MODE @ MODE-OPEN <> if E-IR-SCHEMA-STAGE throw then ;

\ Any end consumes the stage, whatever its outcome, so neither an end without
\ a begin nor a rejected end can leave a half-staged schema behind.
: STG-TAKE ( -- )
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have MODE-OPEN <> if E-IR-SCHEMA-STAGE throw then ;

: ONCE-CK ( ptr ptr u8 -- )
   @ UNSET <> if E-IR-SCHEMA-STAGE throw then ;

: LIST-ROOM ( n -- )
   SN@ ARITY-MAX >= if E-IR-SCHEMA-ARITY throw then ;

\ Design line 234's variadic tail describes every operand or result past the
\ fixed ones, so it is necessarily the last entry of its list: appending after
\ a tail, or declaring a second tail, is an arity-rule violation.
: TAIL-CK ( n -- )
   ST@ 0<> if E-IR-SCHEMA-ARITY throw then ;

: ORD+ ( n n n -- )
   {: l:n ord:n owner:n :}
   l LIST-ROOM
   l SN@ {: n:n :}
   ord l SEG n + SV!
   owner l SEG n + SO!
   n 1+ l SN! ;

: TYPE+ ( n IR-ID:ir-type-id -- )
   {: l:n t:IR-ID:ir-type-id :}
   STG-OPEN-CK
   l TAIL-CK
   l t TYP-ORD t TYP-OWNER ORD+ ;

\ ---- validation of a staged schema -------------------------------------------
: STG-SYM-CK ( IR-ARENA:arena IR-ID:ir-module-key n n -- )
   {: syr:IR-ARENA:arena key:IR-ID:ir-module-key ord:n owner:n :}
   owner key KEY-SERIAL SERIAL-CK
   syr key ord IR-ID:PACK-SYMBOL IR-SYM:LEN@ drop ;

: STG-TYPES-CK ( IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: tyr:IR-ARENA:arena key:IR-ID:ir-module-key l:n :}
   l SN@ 0 ?do
      l SEG i + SO@ key KEY-SERIAL SERIAL-CK
      tyr key l SEG i + SV@ IR-ID:PACK-TYPE IR-TYPE:KIND@ drop
   loop ;

: STG-ATTRS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: syr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   L-AT SN@ 0 ?do
      syr key L-AT SEG i + SV@ L-AT SEG i + SO@ STG-SYM-CK
   loop ;

\ Every field design line 5.3 names that has no meaningful default must have
\ been declared; a zero count means "no operands", but a missing effect class
\ means the schema was never finished.
: FIELDS-CK ( -- )
   STG-EFF @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-CTRL @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-TRAP @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-ARCH @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-RULE @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-REND @ UNSET = if E-IR-SCHEMA-FIELD throw then ;

\ Design line 236's control-flow shape stays inside its committed ceilings, and
\ a variadic tail is only meaningful when its list has an entry to be.
: ARITY-CK ( -- )
   STG-SUCC @ dup 0 < swap SUCC-MAX > or if E-IR-SCHEMA-ARITY throw then
   STG-REGN @ dup 0 < swap REGION-MAX > or if E-IR-SCHEMA-ARITY throw then
   L-OP ST@ 0<> L-OP SN@ 0= and if E-IR-SCHEMA-ARITY throw then
   L-RS ST@ 0<> L-RS SN@ 0= and if E-IR-SCHEMA-ARITY throw then ;

\ A tied ordinal names one fixed entry of its list. A variadic tail stands for
\ every further operand or result, so it names no single register field and
\ cannot be tied.
: FIXED-CK ( n n -- )
   {: l:n ord:n :}
   ord 0 < ord l SN@ >= or if E-IR-SCHEMA-TIE throw then
   l ST@ 0<> ord l SN@ 1- = and if E-IR-SCHEMA-TIE throw then ;

\ One register holds one value, so the tied result and the tied operand have to
\ be the same type of the same module.
: TIE-TYPE-CK ( n n -- )
   {: rs:n op:n :}
   L-RS SEG rs + SV@  L-OP SEG op + SV@  <> if E-IR-SCHEMA-TIE throw then
   L-RS SEG rs + SO@  L-OP SEG op + SO@  <> if E-IR-SCHEMA-TIE throw then ;

\ No result and no operand may be tied twice: one register field cannot hold two
\ values, and two register fields cannot be one.
: TIE-ONCE-CK ( n -- )
   {: k:n :}
   k 0 ?do
      L-TI SEG k + SV@  L-TI SEG i + SV@  = if E-IR-SCHEMA-TIE throw then
      L-TI SEG k + SO@  L-TI SEG i + SO@  = if E-IR-SCHEMA-TIE throw then
   loop ;

: TIES-CK ( -- )
   L-TI SN@ 0 ?do
      L-TI SEG i + SV@ {: rs:n :}
      L-TI SEG i + SO@ {: op:n :}
      L-RS rs FIXED-CK
      L-OP op FIXED-CK
      rs op TIE-TYPE-CK
      i TIE-ONCE-CK
   loop ;

\ Design lines 236, 237, 531, 532: control-flow edges belong to the block's one
\ terminator, and a terminator hands its values to the successor as block
\ arguments rather than producing results of its own.
: TERM-CK ( -- )
   STG-TERM @ 0= STG-SUCC @ 0<> and if E-IR-SCHEMA-TERM throw then
   STG-TERM @ 0<> L-RS SN@ 0<> and if E-IR-SCHEMA-TERM throw then ;

\ Design line 241, enforced against the context's validated binding rather
\ than a rederived legality table (design line 541).
: WANT-ARCH ( -- CTARGET:arch )
   STG-ARCH @ N>ARCH ;

: WANT-FEAT ( -- CTARGET:features )
   STG-FEAT @ CTARGET:FEATURE-SET ;

: BOUND ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:TARGET@ ;

\ A required feature the required architecture cannot have at all is an
\ incoherent target requirement, whatever the bound contract says.
: ARCH-FEAT-CK ( -- )
   WANT-ARCH CTARGET:ARCH-MASK WANT-FEAT CTARGET:HAS?
   0= if E-IR-SCHEMA-TARGET throw then ;

: BOUND-ARCH-CK ( IR-CTX:ctx -- )
   BOUND CTARGET:ARCH@ WANT-ARCH CTARGET-ARCH:EQ
   0= if E-IR-SCHEMA-TARGET throw then ;

: BOUND-FEAT-CK ( IR-CTX:ctx -- )
   BOUND CTARGET:FEATURES@ WANT-FEAT CTARGET:HAS?
   0= if E-IR-SCHEMA-TARGET throw then ;

: TARGET-CK ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   ARCH-FEAT-CK
   c BOUND-ARCH-CK
   c BOUND-FEAT-CK ;

: DUP-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r STG-NAME @ SCAN-NAME 0 < 0= if E-IR-SCHEMA-DUP throw then ;

\ ---- room and append ---------------------------------------------------------
: ROOM-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   r CNT r HC-CAP LCELL@ >= if E-IR-SCHEMA-CAP throw then
   a PCELLS L-OP SN@ + L-RS SN@ + L-AT SN@ + L-TI SN@ TIE-CELLS +
   a PC-CAP LCELL@ > if E-IR-SCHEMA-CAP throw then ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: LIST-ADD ( IR-CTX:ctx IR-ARENA:arena n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena l:n :}
   a PCELLS {: st:n :}
   l SN@ 0 ?do
      c a l SEG i + SV@ CELL+
   loop
   st ;

\ The tie list is two cells per entry, so it has an appender of its own rather
\ than the shared one.
: TIE-LIST-ADD ( IR-CTX:ctx IR-ARENA:arena -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena :}
   a PCELLS {: st:n :}
   L-TI SN@ 0 ?do
      c a L-TI SEG i + SV@ CELL+
      c a L-TI SEG i + SO@ CELL+
   loop
   st ;

: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena opst:n rsst:n atst:n tist:n :}
   c r STG-NAME @ CELL+
   c r opst CELL+   c r L-OP SN@ CELL+   c r L-OP ST@ CELL+
   c r rsst CELL+   c r L-RS SN@ CELL+   c r L-RS ST@ CELL+
   c r STG-SUCC @ CELL+   c r STG-REGN @ CELL+
   c r atst CELL+   c r L-AT SN@ CELL+   c r STG-ATEXT @ CELL+
   c r STG-EFF @ CELL+   c r STG-DOM @ CELL+
   c r STG-SPC @ CELL+   c r STG-ALI @ CELL+
   c r STG-TRAP @ CELL+
   c r STG-ARCH @ CELL+   c r STG-FEAT @ CELL+
   c r STG-TERM @ CELL+
   c r STG-RULE @ CELL+   c r STG-REND @ CELL+
   c r tist CELL+   c r L-TI SN@ CELL+ ;

\ ---- creation checks ---------------------------------------------------------
: ROW-CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-SCHEMA-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 1 < over POOL-MAX > or if E-IR-SCHEMA-CAP throw then
   drop ;

: VERSION-OK ( n -- )
   dup 0 < over VERSION-MAX > or if E-IR-SCHEMA-VERSION throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Create one dialect's schema table: the list pool committed to exactly pcap
\ cells and the row table committed to exactly rcap opcodes, both headers bound
\ to key's module serial, and the row header carrying the dialect's interned
\ name symbol with its major and minor schema version (design lines 229, 1714).
\ The two handles plus the key are the table; all three stay with the module
\ owner, and the table dies with the owning context.
: NEW ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n n n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx syr:IR-ARENA:arena key:IR-ID:ir-module-key dia:IR-ID:ir-symbol-id major:n minor:n rcap:n pcap:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   major VERSION-OK
   minor VERSION-OK
   dia SYM-OWNER key KEY-SERIAL SERIAL-CK
   syr dia IR-SYM:LEN@ drop
   c pcap PHDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a SCL-MAGIC CELL+
   c a key KEY-SERIAL CELL+
   c a pcap CELL+
   c rcap ROW-CELLS * RHDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r SCR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r rcap CELL+
   c r dia SYM-ORD CELL+
   c r major CELL+
   c r minor CELL+
   a r ;

\ ---- the schema builder (design lines 497-521) -------------------------------
\ Open a schema for one opcode, named by the module's interned symbol for it.
: BEGIN-OP ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   STG-MODE @ MODE-NONE <> if E-IR-SCHEMA-STAGE throw then
   MODE-OPEN STG-MODE !
   STG-RESET
   op SYM-ORD STG-NAME !
   op SYM-OWNER STG-NAMEO ! ;

\ Abandon an open schema without defining it. The stage exists only between a
\ begin and an end, so this is the explicit end for a caller that decides not
\ to define after all.
: ABANDON ( -- )
   STG-TAKE ;

\ Design lines 234 and 235: one fixed operand of this type, then optionally a
\ variadic tail whose type describes every further operand.
: ADD-OPERAND ( IR-ID:ir-type-id -- )
   L-OP swap TYPE+ ;

: ADD-OPERAND-TAIL ( IR-ID:ir-type-id -- )
   L-OP swap TYPE+
   1 L-OP ST! ;

: ADD-RESULT ( IR-ID:ir-type-id -- )
   L-RS swap TYPE+ ;

: ADD-RESULT-TAIL ( IR-ID:ir-type-id -- )
   L-RS swap TYPE+
   1 L-RS ST! ;

\ This result and this operand are one register field of the instruction form,
\ named by their ordinals in the two lists above - the result's first, then the
\ operand's, the order the pair reads in. Declaring the tie here is what
\ lets a register allocator read the constraint instead of knowing which opcode
\ has it; the ordinals are checked against the finished lists at DEFINE, so the
\ two lists may be declared in any order around this.
: ADD-TIE ( n n -- )
   {: rs:n op:n :}
   STG-OPEN-CK
   L-TI rs op ORD+ ;

\ Design line 479: an attribute key this opcode requires.
: ADD-ATTR ( IR-ID:ir-symbol-id -- )
   {: k:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   L-AT k SYM-ORD k SYM-OWNER ORD+ ;

\ Design line 479: this opcode also admits the dialect's extension attribute
\ set, so an attribute outside the required keys is not automatically unknown.
: ADD-ATTR-EXT ( -- )
   STG-OPEN-CK
   STG-ATEXT @ 0<> if E-IR-SCHEMA-STAGE throw then
   1 STG-ATEXT ! ;

\ Design lines 236 and 237: the control-flow shape and the terminator status.
: SET-CONTROL ( bool n n -- )
   {: term:bool succ:n regn:n :}
   STG-OPEN-CK
   STG-CTRL ONCE-CK
   1 STG-CTRL !
   term BOOL>N STG-TERM !
   succ STG-SUCC !
   regn STG-REGN ! ;

\ Design line 238 with section 7.3 line 763: no effect token at all.
: SET-PURE ( -- )
   STG-OPEN-CK
   STG-EFF ONCE-CK
   EF-PURE STG-EFF ! ;

\ Design lines 238 and 239: an effect in the data-memory domain, which is the
\ one domain that has an address space and an alias behaviour.
: SET-MEMORY ( IR-TYPE:space IR-SCHEMA:alias IR-SCHEMA:effect -- )
   {: spc:IR-TYPE:space ali:IR-SCHEMA:alias eff:IR-SCHEMA:effect :}
   STG-OPEN-CK
   STG-EFF ONCE-CK
   eff EFF-CODE {: e:n :}
   e EF-PURE = if E-IR-SCHEMA-EFFECT throw then
   e STG-EFF !
   DM-DATA STG-DOM !
   spc SPC-CODE STG-SPC !
   ali ALI-CODE STG-ALI ! ;

\ Design line 238 with section 7.3 lines 753-762: an effect in one of the five
\ domains that are ordered by their token alone and have no address space.
: SET-TOKEN ( IR-TYPE:domain IR-SCHEMA:effect -- )
   {: dom:IR-TYPE:domain eff:IR-SCHEMA:effect :}
   STG-OPEN-CK
   STG-EFF ONCE-CK
   eff EFF-CODE {: e:n :}
   e EF-PURE = if E-IR-SCHEMA-EFFECT throw then
   dom DOM-CODE {: d:n :}
   d DM-DATA = if E-IR-SCHEMA-EFFECT throw then
   e STG-EFF !
   d STG-DOM ! ;

\ Design line 240.
: SET-TRAP ( bool -- )
   {: t:bool :}
   STG-OPEN-CK
   STG-TRAP ONCE-CK
   t BOOL>N STG-TRAP ! ;

\ Design line 241: the architecture and the features this operation needs.
: SET-TARGET ( CTARGET:arch CTARGET:features -- )
   STG-OPEN-CK
   STG-ARCH ONCE-CK
   CTARGET:FEATURES-N STG-FEAT !
   ARCH-CODE STG-ARCH ! ;

\ Design line 242.
: SET-RULE ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   STG-RULE ONCE-CK
   s SYM-ORD STG-RULE !
   s SYM-OWNER STG-RULEO ! ;

\ Design line 243.
: SET-RENDERER ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   STG-REND ONCE-CK
   s SYM-ORD STG-REND !
   s SYM-OWNER STG-RENDO ! ;

\ Close the staged schema: validate it whole against this table, the module's
\ symbol interner, the module's type table, and the context's bound target
\ contract, then append its lists and its row. The stage is consumed whatever
\ the outcome, and every check runs before the first cell is written, so a
\ definition either lands whole or changes nothing.
: DEFINE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key syr:IR-ARENA:arena tyr:IR-ARENA:arena :}
   STG-TAKE
   a r key KEY-CK
   FIELDS-CK
   ARITY-CK
   TERM-CK
   TIES-CK
   syr key STG-NAME @ STG-NAMEO @ STG-SYM-CK
   syr key STG-RULE @ STG-RULEO @ STG-SYM-CK
   syr key STG-REND @ STG-RENDO @ STG-SYM-CK
   syr key STG-ATTRS-CK
   tyr key L-OP STG-TYPES-CK
   tyr key L-RS STG-TYPES-CK
   c TARGET-CK
   r DUP-CK
   a r ROOM-CK
   c a L-OP LIST-ADD {: opst:n :}
   c a L-RS LIST-ADD {: rsst:n :}
   c a L-AT LIST-ADD {: atst:n :}
   c a TIE-LIST-ADD {: tist:n :}
   c r opst rsst atst tist ROW-ADD ;

\ ---- table readers -----------------------------------------------------------
: SCHEMAS ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

: DIALECT@ ( IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r key RKEY-CK
   key r HC-DIALECT LCELL@ ORD-OK IR-ID:PACK-SYMBOL ;

: MAJOR@ ( IR-ARENA:arena -- n )
   dup RHDR-CK HC-MAJOR LCELL@ ;

: MINOR@ ( IR-ARENA:arena -- n )
   dup RHDR-CK HC-MINOR LCELL@ ;

: DEFINED? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ op SYM-OWNER SERIAL-CK
   r op SYM-ORD SCAN-NAME 0 < 0= ;

\ ---- schema readers ----------------------------------------------------------
private

: FLD ( IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id off:n :}
   r  r op ROW-OF  off RC@ ;

: FFLD ( IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id off:n :}
   rv  rv op FROW-OF  off FRC@ ;

public

: OPERANDS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-OPN FLD ;

: OPERAND-TAIL? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-OPTAIL FLD N>BOOL ;

: RESULTS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-RSN FLD ;

: RESULT-TAIL? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-RSTAIL FLD N>BOOL ;

: SUCCESSORS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-SUCC FLD ;

: REGIONS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-REGN FLD ;

: ATTRS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-ATN FLD ;

\ How many ties this form declares. Zero is the default and the common answer:
\ most forms name every register field once.
: TIES ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-TIN FLD ;

: ATTR-EXT? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-ATEXT FLD N>BOOL ;

: TRAPS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-TRAP FLD N>BOOL ;

: TERMINATOR? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-TERM FLD N>BOOL ;

: EFFECT@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-SCHEMA:effect )
   OFF-EFF FLD N>EFF ;

: DOMAIN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-TYPE:domain )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r op ROW-OF {: l:n :}
   r l OFF-EFF RC@ HAS-DOM? SHAPE-CK
   r l OFF-DOM RC@ N>DOM ;

: SPACE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-TYPE:space )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r op ROW-OF {: l:n :}
   r l OFF-EFF RC@ r l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   r l OFF-SPC RC@ N>SPC ;

: ALIAS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-SCHEMA:alias )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r op ROW-OF {: l:n :}
   r l OFF-EFF RC@ r l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   r l OFF-ALI RC@ N>ALI ;

: ARCH@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- CTARGET:arch )
   OFF-ARCH FLD N>ARCH ;

: FEATURES@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- CTARGET:features )
   OFF-FEAT FLD CTARGET:FEATURE-SET ;

: RULE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   r key RKEY-CK
   key r op OFF-RULE FLD ORD-OK IR-ID:PACK-SYMBOL ;

: RENDERER@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   r key RKEY-CK
   key r op OFF-REND FLD ORD-OK IR-ID:PACK-SYMBOL ;

private

\ One element of a stored list window, revalidated against the pool's live
\ range before the cell is read.
: WIN@ ( IR-ARENA:arena IR-ARENA:arena n n n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n stoff:n lenoff:n i:n :}
   r l stoff RC@ {: st:n :}
   r l lenoff RC@ {: ln:n :}
   a PCELLS st ln WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   a st i + PC@ ORD-OK ;

\ One half of one stored tie, revalidated the same way: the whole tie list is
\ TIE-PAIR cells per entry, and the index counts ties rather than cells.
: TWIN@ ( IR-ARENA:arena IR-ARENA:arena n n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n i:n col:n :}
   r l OFF-TIST RC@ {: st:n :}
   r l OFF-TIN RC@ {: ln:n :}
   a PCELLS st ln TIE-CELLS WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   a st i TIE-CELLS + col + PC@ ORD-OK ;

: FWIN@ ( IR-ARENA:view IR-ARENA:view n n n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n stoff:n lenoff:n i:n :}
   rv l stoff FRC@ {: st:n :}
   rv l lenoff FRC@ {: ln:n :}
   pv FPCELLS st ln WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   pv st i + FPC@ ORD-OK ;

: FTWIN@ ( IR-ARENA:view IR-ARENA:view n n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n i:n col:n :}
   rv l OFF-TIST FRC@ {: st:n :}
   rv l OFF-TIN FRC@ {: ln:n :}
   pv FPCELLS st ln TIE-CELLS WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   pv st i TIE-CELLS + col + FPC@ ORD-OK ;

public

: OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a r key KEY-CK
   key a r  r op ROW-OF  OFF-OPST OFF-OPN i WIN@ IR-ID:PACK-TYPE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a r key KEY-CK
   key a r  r op ROW-OF  OFF-RSST OFF-RSN i WIN@ IR-ID:PACK-TYPE ;

: ATTR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a r key KEY-CK
   key a r  r op ROW-OF  OFF-ATST OFF-ATN i WIN@ IR-ID:PACK-SYMBOL ;

\ The two halves of one tie. Both are ordinals into this schema's own result and
\ operand lists rather than identities of the module, so neither takes the key.
: TIE-RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a r PAIR-CK
   a r  r op ROW-OF  i TIE-RS TWIN@ ;

: TIE-OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a r PAIR-CK
   a r  r op ROW-OF  i TIE-OP TWIN@ ;

\ ---- digests (design lines 602, 1716) ----------------------------------------
private

2 constant PRE-VER                \ this file's canonical preimage version

0 constant DS-TAG
1 constant DS-VER
2 constant DS-DIA
3 constant DS-NAME
4 constant DS-OPN
5 constant DS-OPTAIL
6 constant DS-RSN
7 constant DS-RSTAIL
8 constant DS-SUCC
9 constant DS-REGN
10 constant DS-ATN
11 constant DS-ATEXT
12 constant DS-EFF
13 constant DS-DOM
14 constant DS-SPC
15 constant DS-ALI
16 constant DS-TRAP
17 constant DS-ARCH
18 constant DS-FEAT
19 constant DS-TERM
20 constant DS-RULE
21 constant DS-REND
22 constant DS-TIN
23 constant DS-FIX
\ Three lists of one cell per entry, and the tie list of TIE-PAIR cells each.
DS-FIX ARITY-MAX 3 * + ARITY-MAX TIE-CELLS + constant DS-SLOTS
DS-SLOTS CDIGEST:SLOT-BYTES * constant DS-BYTES
create DPRE DS-BYTES allot

10 constant TS-SLOTS
TS-SLOTS CDIGEST:SLOT-BYTES * constant TS-BYTES
create TPRE TS-BYTES allot

: DP! ( n n -- )
   DPRE swap CDIGEST:SLOT! ;

: TP! ( n n -- )
   TPRE swap CDIGEST:SLOT! ;

\ Write the fixed head of a record preimage: the domain-separation tag, this
\ file's preimage version, the dialect, and every fixed field, so a byte's
\ position determines which field it belongs to.
: DPRE-FIX ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena dia:n l:n :}
   CDIGEST:TAG-SCHEMA DS-TAG DP!
   PRE-VER DS-VER DP!
   dia DS-DIA DP!
   r l OFF-NAME RC@ DS-NAME DP!
   r l OFF-OPN RC@ DS-OPN DP!
   r l OFF-OPTAIL RC@ DS-OPTAIL DP!
   r l OFF-RSN RC@ DS-RSN DP!
   r l OFF-RSTAIL RC@ DS-RSTAIL DP!
   r l OFF-SUCC RC@ DS-SUCC DP!
   r l OFF-REGN RC@ DS-REGN DP!
   r l OFF-ATN RC@ DS-ATN DP!
   r l OFF-ATEXT RC@ DS-ATEXT DP!
   r l OFF-EFF RC@ DS-EFF DP!
   r l OFF-DOM RC@ DS-DOM DP!
   r l OFF-SPC RC@ DS-SPC DP!
   r l OFF-ALI RC@ DS-ALI DP!
   r l OFF-TRAP RC@ DS-TRAP DP!
   r l OFF-ARCH RC@ DS-ARCH DP!
   r l OFF-FEAT RC@ DS-FEAT DP!
   r l OFF-TERM RC@ DS-TERM DP!
   r l OFF-RULE RC@ DS-RULE DP!
   r l OFF-REND RC@ DS-REND DP!
   r l OFF-TIN RC@ DS-TIN DP! ;

\ Append one stored list to the preimage and answer the next free slot. The
\ list's length is already fixed earlier in the preimage, so the concatenation
\ stays injective.
: DPRE-LIST ( IR-ARENA:arena IR-ARENA:arena n n n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n stoff:n lenoff:n at:n :}
   r l lenoff RC@ {: ln:n :}
   at ln + DS-SLOTS > if E-IR-SCHEMA-STATE throw then
   ln 0 ?do
      a r l stoff lenoff i WIN@  at i + DP!
   loop
   at ln + ;

\ The tie list, both halves of every tie in declaration order. Its length is
\ already fixed earlier in the preimage, so this stays injective too.
: DPRE-TIES ( IR-ARENA:arena IR-ARENA:arena n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n at:n :}
   r l OFF-TIN RC@ {: ln:n :}
   at ln TIE-CELLS + DS-SLOTS > if E-IR-SCHEMA-STATE throw then
   ln 0 ?do
      a r l i TIE-RS TWIN@  at i TIE-CELLS + DP!
      a r l i TIE-OP TWIN@  at i TIE-CELLS + TIE-OP + DP!
   loop
   at ln TIE-CELLS + ;

: ROW-DIGEST ( IR-ARENA:arena IR-ARENA:arena n n -- CDIGEST:digest )
   {: a:IR-ARENA:arena r:IR-ARENA:arena dia:n l:n :}
   r dia l DPRE-FIX
   a r l OFF-OPST OFF-OPN DS-FIX DPRE-LIST
   {: at:n :}
   a r l OFF-RSST OFF-RSN at DPRE-LIST
   {: at2:n :}
   a r l OFF-ATST OFF-ATN at2 DPRE-LIST
   {: at3:n :}
   a r l at3 DPRE-TIES
   {: at4:n :}
   DPRE at4 CDIGEST:SLOT-BYTES * CDIGEST:COMPUTE ;

: FDPRE-FIX ( IR-ARENA:view n n -- )
   {: rv:IR-ARENA:view dia:n l:n :}
   CDIGEST:TAG-SCHEMA DS-TAG DP!
   PRE-VER DS-VER DP!
   dia DS-DIA DP!
   rv l OFF-NAME FRC@ DS-NAME DP!
   rv l OFF-OPN FRC@ DS-OPN DP!
   rv l OFF-OPTAIL FRC@ DS-OPTAIL DP!
   rv l OFF-RSN FRC@ DS-RSN DP!
   rv l OFF-RSTAIL FRC@ DS-RSTAIL DP!
   rv l OFF-SUCC FRC@ DS-SUCC DP!
   rv l OFF-REGN FRC@ DS-REGN DP!
   rv l OFF-ATN FRC@ DS-ATN DP!
   rv l OFF-ATEXT FRC@ DS-ATEXT DP!
   rv l OFF-EFF FRC@ DS-EFF DP!
   rv l OFF-DOM FRC@ DS-DOM DP!
   rv l OFF-SPC FRC@ DS-SPC DP!
   rv l OFF-ALI FRC@ DS-ALI DP!
   rv l OFF-TRAP FRC@ DS-TRAP DP!
   rv l OFF-ARCH FRC@ DS-ARCH DP!
   rv l OFF-FEAT FRC@ DS-FEAT DP!
   rv l OFF-TERM FRC@ DS-TERM DP!
   rv l OFF-RULE FRC@ DS-RULE DP!
   rv l OFF-REND FRC@ DS-REND DP!
   rv l OFF-TIN FRC@ DS-TIN DP! ;

: FDPRE-LIST ( IR-ARENA:view IR-ARENA:view n n n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n stoff:n lenoff:n at:n :}
   rv l lenoff FRC@ {: ln:n :}
   at ln + DS-SLOTS > if E-IR-SCHEMA-STATE throw then
   ln 0 ?do
      pv rv l stoff lenoff i FWIN@  at i + DP!
   loop
   at ln + ;

: FDPRE-TIES ( IR-ARENA:view IR-ARENA:view n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n at:n :}
   rv l OFF-TIN FRC@ {: ln:n :}
   at ln TIE-CELLS + DS-SLOTS > if E-IR-SCHEMA-STATE throw then
   ln 0 ?do
      pv rv l i TIE-RS FTWIN@  at i TIE-CELLS + DP!
      pv rv l i TIE-OP FTWIN@  at i TIE-CELLS + TIE-OP + DP!
   loop
   at ln TIE-CELLS + ;

: FROW-DIGEST ( IR-ARENA:view IR-ARENA:view n n -- CDIGEST:digest )
   {: pv:IR-ARENA:view rv:IR-ARENA:view dia:n l:n :}
   rv dia l FDPRE-FIX
   pv rv l OFF-OPST OFF-OPN DS-FIX FDPRE-LIST
   {: at:n :}
   pv rv l OFF-RSST OFF-RSN at FDPRE-LIST
   {: at2:n :}
   pv rv l OFF-ATST OFF-ATN at2 FDPRE-LIST
   {: at3:n :}
   pv rv l at3 FDPRE-TIES
   {: at4:n :}
   DPRE at4 CDIGEST:SLOT-BYTES * CDIGEST:COMPUTE ;

\ The table digest is a chain: seed over the header and the row count, then one
\ fold step per record digest. Deterministic, covers every row, and needs no
\ buffer that grows with the table.
: CHAIN-HEAD ( -- )
   CDIGEST:TAG-SCHEMA-TABLE 0 TP!
   PRE-VER 1 TP! ;

: CHAIN-SEED ( n n n n -- CDIGEST:digest )
   {: dia:n major:n minor:n cnt:n :}
   CHAIN-HEAD
   dia 2 TP!  major 3 TP!  minor 4 TP!  cnt 5 TP!
   0 6 TP!  0 7 TP!  0 8 TP!  0 9 TP!
   TPRE TS-BYTES CDIGEST:COMPUTE ;

: CHAIN-STEP ( CDIGEST:digest CDIGEST:digest -- CDIGEST:digest )
   CDIGEST-DIGEST:UNMAKE {: v0:n v1:n v2:n v3:n :}
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   CHAIN-HEAD
   w0 2 TP!  w1 3 TP!  w2 4 TP!  w3 5 TP!
   v0 6 TP!  v1 7 TP!  v2 8 TP!  v3 9 TP!
   TPRE TS-BYTES CDIGEST:COMPUTE ;

public

\ One schema record's digest: a mutation of any field it records, of any
\ listed operand, result, or attribute key, or of the dialect it belongs to
\ moves it.
: DIGEST ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- CDIGEST:digest )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   a r PAIR-CK
   a r  r HC-DIALECT LCELL@ ORD-OK  r op ROW-OF  ROW-DIGEST ;

\ The whole table's digest (design lines 602, 1716).
: TABLE-DIGEST ( IR-ARENA:arena IR-ARENA:arena -- CDIGEST:digest )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a r PAIR-CK
   r HC-DIALECT LCELL@ ORD-OK {: dia:n :}
   dia r HC-MAJOR LCELL@ r HC-MINOR LCELL@ r CNT CHAIN-SEED
   r CNT 0 ?do
      a r dia i ROW-DIGEST CHAIN-STEP
   loop ;

\ Recompute the table digest and reject a presented one that differs, which is
\ how a caller detects that the schema table it holds is not the one an
\ artifact was built against.
: VERIFY ( IR-ARENA:arena IR-ARENA:arena CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: a:IR-ARENA:arena r:IR-ARENA:arena w0:n w1:n w2:n w3:n :}
   a r TABLE-DIGEST  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   0= if E-IR-SCHEMA-DIGEST throw then ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its schemas through the two arena views; the retired
\ builder handles reject every touch with E-IR-ARENA-FROZEN.
: FSCHEMAS ( IR-ARENA:view -- n )
   dup FRHDR-CK FCNT ;

: FDIALECT@ ( IR-ARENA:view IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key :}
   rv key FRKEY-CK
   key rv HC-DIALECT FCELL@ ORD-OK IR-ID:PACK-SYMBOL ;

: FMAJOR@ ( IR-ARENA:view -- n )
   dup FRHDR-CK HC-MAJOR FCELL@ ;

: FMINOR@ ( IR-ARENA:view -- n )
   dup FRHDR-CK HC-MINOR FCELL@ ;

: FDEFINED? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv FRHDR-CK
   rv HC-SERIAL FCELL@ op SYM-OWNER SERIAL-CK
   rv op SYM-ORD FSCAN-NAME 0 < 0= ;

: FOPERANDS ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-OPN FFLD ;

: FOPERAND-TAIL? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   OFF-OPTAIL FFLD N>BOOL ;

: FRESULTS ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-RSN FFLD ;

: FRESULT-TAIL? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   OFF-RSTAIL FFLD N>BOOL ;

: FSUCCESSORS ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-SUCC FFLD ;

: FREGIONS ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-REGN FFLD ;

: FATTRS ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-ATN FFLD ;

: FTIES ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-TIN FFLD ;

: FATTR-EXT? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   OFF-ATEXT FFLD N>BOOL ;

: FTRAPS? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   OFF-TRAP FFLD N>BOOL ;

: FTERMINATOR? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   OFF-TERM FFLD N>BOOL ;

: FEFFECT@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-SCHEMA:effect )
   OFF-EFF FFLD N>EFF ;

: FDOMAIN@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-TYPE:domain )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv op FROW-OF {: l:n :}
   rv l OFF-EFF FRC@ HAS-DOM? SHAPE-CK
   rv l OFF-DOM FRC@ N>DOM ;

: FSPACE@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-TYPE:space )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv op FROW-OF {: l:n :}
   rv l OFF-EFF FRC@ rv l OFF-DOM FRC@ HAS-MEM? SHAPE-CK
   rv l OFF-SPC FRC@ N>SPC ;

: FALIAS@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-SCHEMA:alias )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv op FROW-OF {: l:n :}
   rv l OFF-EFF FRC@ rv l OFF-DOM FRC@ HAS-MEM? SHAPE-CK
   rv l OFF-ALI FRC@ N>ALI ;

: FARCH@ ( IR-ARENA:view IR-ID:ir-symbol-id -- CTARGET:arch )
   OFF-ARCH FFLD N>ARCH ;

: FFEATURES@ ( IR-ARENA:view IR-ID:ir-symbol-id -- CTARGET:features )
   OFF-FEAT FFLD CTARGET:FEATURE-SET ;

: FRULE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   rv key FRKEY-CK
   key rv op OFF-RULE FFLD ORD-OK IR-ID:PACK-SYMBOL ;

: FRENDERER@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   rv key FRKEY-CK
   key rv op OFF-REND FFLD ORD-OK IR-ID:PACK-SYMBOL ;

: FOPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   key pv rv  rv op FROW-OF  OFF-OPST OFF-OPN i FWIN@ IR-ID:PACK-TYPE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   key pv rv  rv op FROW-OF  OFF-RSST OFF-RSN i FWIN@ IR-ID:PACK-TYPE ;

: FATTR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-symbol-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   key pv rv  rv op FROW-OF  OFF-ATST OFF-ATN i FWIN@ IR-ID:PACK-SYMBOL ;

: FTIE-RESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv rv FPAIR-CK
   pv rv  rv op FROW-OF  i TIE-RS FTWIN@ ;

: FTIE-OPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv rv FPAIR-CK
   pv rv  rv op FROW-OF  i TIE-OP FTWIN@ ;

: FDIGEST ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id -- CDIGEST:digest )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   pv rv FPAIR-CK
   pv rv  rv HC-DIALECT FCELL@ ORD-OK  rv op FROW-OF  FROW-DIGEST ;

: FTABLE-DIGEST ( IR-ARENA:view IR-ARENA:view -- CDIGEST:digest )
   {: pv:IR-ARENA:view rv:IR-ARENA:view :}
   pv rv FPAIR-CK
   rv HC-DIALECT FCELL@ ORD-OK {: dia:n :}
   dia rv HC-MAJOR FCELL@ rv HC-MINOR FCELL@ rv FCNT CHAIN-SEED
   rv FCNT 0 ?do
      pv rv dia i FROW-DIGEST CHAIN-STEP
   loop ;

: FVERIFY ( IR-ARENA:view IR-ARENA:view CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: pv:IR-ARENA:view rv:IR-ARENA:view w0:n w1:n w2:n w3:n :}
   pv rv FTABLE-DIGEST  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   0= if E-IR-SCHEMA-DIGEST throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
