\ frozen.f - the reader plumbing every native pass over a frozen module shares:
\ one cursor onto the module being read, and the row accessors that read it.
\
\ MECHANICS ONLY. Every word here reads a row of the frozen module or compares two
\ identities; none of them decides anything, refuses anything, or throws a code of
\ its own. Each pass keeps every judgement it makes - which shapes it accepts,
\ which operations it has a rule for, which registers it may use - and the error
\ code it refuses under. The independence that makes
\ src/compiler/native/regalloc-verify.f worth having is about judgements, not about
\ how a row is fetched: it still measures every live range itself, still reads the
\ ties out of the module's own schema table, and still compares its answer against
\ the allocator's. What it no longer carries is its own copy of "read operand i of
\ operation id" - never a judgement, and one expression a view-API change had to
\ edit in five places.
\
\ ONE MODULE AT A TIME. The native chain reads one frozen module at a time, which
\ is the single-task discipline every pass in it already declares in its own
\ header, so the views are one set of package-owned slots here rather than five. A
\ pass wires the cursor with VIEWS! at the start of its own run and reads it only
\ within that run; the allocator's sealed claims, the validator's accepted answer
\ and the emitter's sealed bytes all live in those passes' own tables, so an answer
\ stays true after the next pass has moved the cursor on. VIEWS! wires every table
\ any of these passes reads rather than a per-pass subset: wiring one a given pass
\ never looks at costs a store and reads nothing wrong.
\
\ WHAT STAYED IN THE PASSES. The spent-binding state machine, because four of those
\ bindings are live at the same instant in the real chain - the allocator, the
\ validator and the emitter are all bound over one builder before any of them runs
\ (test/compiler/native-chain-fixture.f) - so the state cannot be one set of slots.
\ The dialect opcode families, because each pass maps them to its own slots and
\ refuses an unmodelled form under its own name. The module rebuilding that
\ src/compiler/native/select.f and src/compiler/native/spill.f share, because two
\ passes are a pair, not a substrate.

require lib/prelude.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/attr.f
require src/compiler/ir/source.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f

package NFROZEN
private

12 constant VIEWS-N

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view

public

\ ---- how much of one block a native pass holds -------------------------------
\ Values in one block. Two hundred and fifty-six is far past anything a
\ straight-line body reaches, and it is one ceiling rather than one per pass: the
\ selector, the allocator, the validator, the lowering pass and the emitter all
\ have to agree on it, so a block a pass produced always fits the next one. A
\ block that wants more is a capability to raise here, not a ceiling to widen
\ silently in one pass and not another.
256 constant VMAX

\ ---- how many blocks one routine of the native chain has ---------------------
\ Blocks in one function. A colon body's blocks come from its control words, and
\ sixty-four of them is far past anything hand-written Forth reaches; it is one
\ ceiling rather than one per pass for the same reason VMAX is - the elaborator,
\ the selector, the allocator, the validator, the lowering pass and the emitter
\ all have to agree on it, so a function a pass produced always fits the next
\ one. A body that wants more is a capability to raise here.
64 constant BMAX

\ ---- how many functions one module of the native chain has -------------------
\ Functions in one module. A module used to hold exactly one - a colon
\ definition's own routine - and it now holds that one plus a routine per
\ quotation the body makes, because a quotation body is a routine with its own
\ entry and its own return and nothing else in this chain is that shape. The
\ measured tree needs twenty: src/core/type-family.f TFAM-HOOK-INSTALL makes
\ nineteen quotations and no definition makes more. Sixty-four is the same
\ number src/compiler/ir/build.f already plans a module's function table at, so
\ a module this ceiling admits is one the store admits too, and it is a ceiling
\ rather than one per pass for the reason VMAX and BMAX are.
\
\ RAISING IT IS A CAPABILITY AND NOT A NUMBER, because the values of every
\ function share one module-wide table (see E-A64RA-CAP in
\ src/compiler/native/regalloc.f): more functions in a module spend the same
\ value budget, so a raise here without a look at that one moves the refusal
\ rather than removing it.
64 constant FMAX

\ ---- the frozen tables of the module being read ------------------------------
\ One indexed slot per view keeps every accessor below to a signature a reader
\ can hold in their head.
0 constant V-SYMP                    \ symbol pool
1 constant V-SYMR                    \ symbol rows
2 constant V-TYPR                    \ type rows
3 constant V-ATTR                    \ attribute rows
4 constant V-SRC                     \ source registry
5 constant V-SCHP                    \ schema list pool
6 constant V-SCHR                    \ schema rows
7 constant V-OPP                     \ operation pool
8 constant V-OPR                     \ operation rows
9 constant V-VALR                    \ value rows
10 constant V-FUNR                   \ function rows
11 constant V-BLKR                   \ block rows

\ ---- the cursor --------------------------------------------------------------
\ The module key every row read is made against, and one view by slot.
: MKEY ( -- IR-ID:ir-module-key )    0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

\ Point the cursor at one frozen module. A pass calls this once at the start of
\ its own run, before it reads a single row.
: VIEWS! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 S-KEY !
   m IR-BUILD:FSYM-POOL    V-SYMP S-VIEW !
   m IR-BUILD:FSYM-ROWS    V-SYMR S-VIEW !
   m IR-BUILD:FTYPE-ROWS   V-TYPR S-VIEW !
   m IR-BUILD:FATTR-ROWS   V-ATTR S-VIEW !
   m IR-BUILD:FSOURCES     V-SRC  S-VIEW !
   m IR-BUILD:FSCHEMA-POOL V-SCHP S-VIEW !
   m IR-BUILD:FSCHEMA-ROWS V-SCHR S-VIEW !
   m IR-BUILD:FOP-POOL     V-OPP  S-VIEW !
   m IR-BUILD:FOP-ROWS     V-OPR  S-VIEW !
   m IR-BUILD:FVALUE-ROWS  V-VALR S-VIEW !
   m IR-BUILD:FFUN-ROWS    V-FUNR S-VIEW !
   m IR-BUILD:FBLOCK-ROWS  V-BLKR S-VIEW ! ;

\ ---- identity ----------------------------------------------------------------
\ Two symbols, or two types, are the same when they are the same ordinal of the
\ same module. Nothing here compares spellings: a dialect's spellings are the
\ dialect's, and a pass holds identities it was given rather than bytes it
\ decided on.
: SAME-SYM? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   {: x:IR-ID:ir-symbol-id y:IR-ID:ir-symbol-id :}
   x IR-ID:SYMBOL-LOCAL y IR-ID:SYMBOL-LOCAL <> if false exit then
   x IR-ID:SYMBOL-OWNER y IR-ID:SYMBOL-OWNER IR-ID:MODULE-SAME? ;

: SAME-TYPE? ( IR-ID:ir-type-id IR-ID:ir-type-id -- bool )
   {: x:IR-ID:ir-type-id y:IR-ID:ir-type-id :}
   x IR-ID:TYPE-LOCAL y IR-ID:TYPE-LOCAL <> if false exit then
   x IR-ID:TYPE-OWNER y IR-ID:TYPE-OWNER IR-ID:MODULE-SAME? ;

\ Two values are the same when they are the same ordinal of the same module. A
\ pass that has to ask whether the value one operation defined is the value
\ another one reads goes through here rather than comparing ordinals, so an
\ identity of another module can never answer yes.
: SAME-VALUE? ( IR-ID:ir-value-id IR-ID:ir-value-id -- bool )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   x IR-ID:VALUE-LOCAL y IR-ID:VALUE-LOCAL <> if false exit then
   x IR-ID:VALUE-OWNER y IR-ID:VALUE-OWNER IR-ID:MODULE-SAME? ;

\ ---- the functions and blocks of the module ----------------------------------
: FUN-COUNT ( -- n )
   V-FUNR VW IR-FUN:FFUNS ;

: BLOCK-COUNT ( IR-ID:ir-fun-id -- n )
   V-FUNR VW swap IR-FUN:FBLOCK-COUNT ;

: BLOCK-AT ( IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ID:ir-fun-id i:n :}
   V-FUNR VW V-BLKR VW MKEY f i IR-FUN:FBLOCK@ ;

: ARG-COUNT ( IR-ID:ir-block-id -- n )
   V-BLKR VW swap IR-FUN:FARG-COUNT ;

: ARG-AT ( IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR VW V-VALR VW MKEY bk i IR-FUN:FARG@ ;

: OP-COUNT ( IR-ID:ir-block-id -- n )
   V-BLKR VW swap IR-FUN:FOP-COUNT ;

: OP-AT ( IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR VW V-OPR VW MKEY bk i IR-FUN:FOP@ ;

\ The operation control leaves a block through, read off the block's own row
\ rather than taken as the last operation: which operation terminates a block is
\ the block's recorded fact.
: TERM-AT ( IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW V-OPR VW MKEY bk IR-FUN:FTERMINATOR@ ;

\ ---- one operation's own rows ------------------------------------------------
: OPCODE-AT ( IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   V-OPR VW MKEY rot IR-OP:FOPCODE@ ;

: OPERANDS-OF ( IR-ID:ir-op-id -- n )
   V-OPR VW swap IR-OP:FOPERANDS ;

: OPERAND-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW MKEY id i IR-OP:FOPERAND@ ;

: RESULTS-OF ( IR-ID:ir-op-id -- n )
   V-OPR VW swap IR-OP:FRESULTS ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW MKEY id i IR-OP:FRESULT@ ;

\ The blocks a terminator hands control to. A non-terminator names none, which is
\ the schema's rule and not this file's, so the count answers zero for one rather
\ than refusing: what a caller does with an operation that names no successor is
\ the caller's judgement.
: SUCCS-OF ( IR-ID:ir-op-id -- n )
   V-OPR VW swap IR-OP:FSUCCESSORS ;

: SUCC-AT ( IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW MKEY id i IR-OP:FSUCCESSOR@ ;

: ATTRS-OF ( IR-ID:ir-op-id -- n )
   V-OPR VW swap IR-OP:FATTRS ;

: ATTR-KEY-AT ( IR-ID:ir-op-id n -- IR-ID:ir-symbol-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW MKEY id i IR-OP:FATTR-KEY@ ;

\ The integer an attribute carries. Which key it is under, and whether the
\ operation was allowed to carry it at all, is the reading pass's question.
: ATTR-INT-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-ATTR VW  V-OPP VW V-OPR VW MKEY id i IR-OP:FATTR@  IR-ATTR:FINT@ ;

\ The span of the source text this operation came from. What a pass does with it
\ - rebuild it into a second module, or record it in a source map - is the pass's.
: SPAN-AT ( IR-ID:ir-op-id -- IR-SOURCE:span )
   V-OPR VW MKEY rot IR-OP:FSPAN@ ;

\ ---- one value's own row -----------------------------------------------------
: VALUE-TYPE-AT ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   V-VALR VW MKEY rot IR-OP:FVALUE-TYPE@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
