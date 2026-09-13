\ frozen.f - the reader plumbing every native pass over a frozen module shares:
\ one cursor onto the module being read, and the row accessors that read it.

require lib/prelude.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/attr.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/verify.f
require src/compiler/ir/build.f

package NFROZEN
private

14 constant VIEWS-N

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view
VIEWS-N TYPED-BUFFER S-READ IR-ARENA:reader

: RD ( n -- IR-ARENA:reader ) S-READ @ ;

public

\ ---- the frozen tables of the module being read ------------------------------
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
12 constant V-EDGEP                  \ frozen predecessor pool
13 constant V-EDGER                  \ frozen edge rows

\ ---- the cursor --------------------------------------------------------------
: MKEY ( -- IR-ID:ir-module-key )    0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

: VALUE-COUNT ( -- n ) V-VALR RD IR-OP:RVALUES ;
: TOTAL-BLOCKS ( -- n ) V-BLKR RD IR-FUN:RBLOCKS ;
: TOTAL-FUNS ( -- n ) V-FUNR RD IR-FUN:RFUNS ;
: TOTAL-OPS ( -- n ) V-OPR RD IR-OP:ROPS ;


\ A pass rebinds all readers at the start of its run, including every input
\ module switch. Tokens retain no pointer or count; every read still validates
\ generation, state and bounds through the arena reader API.
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
   m IR-BUILD:FBLOCK-ROWS  V-BLKR S-VIEW !
   m IR-BUILD:FEDGE-POOL   V-EDGEP S-VIEW !
   m IR-BUILD:FEDGE-ROWS   V-EDGER S-VIEW !
   VIEWS-N 0 ?do i S-VIEW @ IR-ARENA:OPEN i S-READ ! loop ;

\ ---- identity ----------------------------------------------------------------
\ Same ordinal of the same module. Nothing here compares spellings.
: SAME-SYM? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   {: x:IR-ID:ir-symbol-id y:IR-ID:ir-symbol-id :}
   x IR-ID:SYMBOL-LOCAL y IR-ID:SYMBOL-LOCAL <> if false exit then
   x IR-ID:SYMBOL-OWNER y IR-ID:SYMBOL-OWNER IR-ID:MODULE-SAME? ;

: SAME-TYPE? ( IR-ID:ir-type-id IR-ID:ir-type-id -- bool )
   {: x:IR-ID:ir-type-id y:IR-ID:ir-type-id :}
   x IR-ID:TYPE-LOCAL y IR-ID:TYPE-LOCAL <> if false exit then
   x IR-ID:TYPE-OWNER y IR-ID:TYPE-OWNER IR-ID:MODULE-SAME? ;

: SAME-VALUE? ( IR-ID:ir-value-id IR-ID:ir-value-id -- bool )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   x IR-ID:VALUE-LOCAL y IR-ID:VALUE-LOCAL <> if false exit then
   x IR-ID:VALUE-OWNER y IR-ID:VALUE-OWNER IR-ID:MODULE-SAME? ;

\ ---- the functions and blocks of the module ----------------------------------
: FUN-COUNT ( -- n )
   V-FUNR RD IR-FUN:RFUNS ;

\ Arity is a fact of the FUNCTION and not of the emission: a module holds a
\ routine per quotation, and a routine contract states one arity for them all.
: FUN-ARITY ( IR-ID:ir-fun-id -- n n )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR RD  V-FUNR RD MKEY f IR-FUN:RSIGNATURE@  IR-TYPE:RARITY@ ;

: BLOCK-COUNT ( IR-ID:ir-fun-id -- n )
   V-FUNR RD swap IR-FUN:RBLOCK-COUNT ;

: BLOCK-AT ( IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ID:ir-fun-id i:n :}
   V-FUNR RD V-BLKR RD MKEY f i IR-FUN:RBLOCK@ ;

: ARG-COUNT ( IR-ID:ir-block-id -- n )
   V-BLKR RD swap IR-FUN:RARG-COUNT ;

: ARG-AT ( IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR RD V-VALR RD MKEY bk i IR-FUN:RARG@ ;

: OP-COUNT ( IR-ID:ir-block-id -- n )
   V-BLKR RD swap IR-FUN:ROP-COUNT ;

: OP-AT ( IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR RD V-OPR RD MKEY bk i IR-FUN:ROP@ ;

\ Read off the block's own row rather than taken as the last operation.
: TERM-AT ( IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR RD V-OPR RD MKEY bk IR-FUN:RTERMINATOR@ ;

\ Freeze derives these rows from the checked terminators. Repeated edges may
\ repeat a predecessor; a dataflow meet is idempotent over those entries.
: PRED-COUNT ( IR-ID:ir-block-id -- n )
   V-EDGER RD swap IR-VERIFY:RPRED-COUNT ;

: PRED-AT ( IR-ID:ir-block-id n -- IR-ID:ir-block-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-EDGEP RD V-EDGER RD MKEY bk i IR-VERIFY:RPRED@ ;

\ ---- one operation's own rows ------------------------------------------------
: OPCODE-AT ( IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   V-OPR RD MKEY rot IR-OP:ROPCODE@ ;

: OPERANDS-OF ( IR-ID:ir-op-id -- n )
   V-OPR RD swap IR-OP:ROPERANDS ;

: OPERAND-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP RD V-OPR RD MKEY id i IR-OP:ROPERAND@ ;

: RESULTS-OF ( IR-ID:ir-op-id -- n )
   V-OPR RD swap IR-OP:RRESULTS ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP RD V-OPR RD MKEY id i IR-OP:RRESULT@ ;

\ A non-terminator names none and answers zero; that is the schema's rule.
: SUCCS-OF ( IR-ID:ir-op-id -- n )
   V-OPR RD swap IR-OP:RSUCCESSORS ;

: SUCC-AT ( IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP RD V-OPR RD MKEY id i IR-OP:RSUCCESSOR@ ;

: ATTRS-OF ( IR-ID:ir-op-id -- n )
   V-OPR RD swap IR-OP:RATTRS ;

: ATTR-KEY-AT ( IR-ID:ir-op-id n -- IR-ID:ir-symbol-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP RD V-OPR RD MKEY id i IR-OP:RATTR-KEY@ ;

\ Which key it is under, and whether it was allowed, is the reading pass's.
: ATTR-INT-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-ATTR RD  V-OPP RD V-OPR RD MKEY id i IR-OP:RATTR@  IR-ATTR:RINT@ ;

: SPAN-AT ( IR-ID:ir-op-id -- IR-SOURCE:span )
   V-OPR RD MKEY rot IR-OP:RSPAN@ ;

\ ---- one value's own row -----------------------------------------------------
: VALUE-TYPE-AT ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   V-VALR RD MKEY rot IR-OP:RVALUE-TYPE@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
