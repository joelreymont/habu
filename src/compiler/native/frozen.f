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
require src/compiler/ir/build.f

package NFROZEN
private

12 constant VIEWS-N

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view

public

\ ---- how much of one block a native pass holds -------------------------------
\ One ceiling for every pass, so a block one pass produced always fits the next.
256 constant VMAX

\ ---- how many blocks one routine of the native chain has ---------------------
\ One ceiling for every pass, for the same reason VMAX is one.
64 constant BMAX

\ ---- how many functions one module of the native chain has -------------------
\ One ceiling for every pass. Raising it is a capability and not a number: every
\ function of a module spends one shared value budget (E-A64RA-CAP, regalloc.f).
64 constant FMAX

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

\ ---- the cursor --------------------------------------------------------------
: MKEY ( -- IR-ID:ir-module-key )    0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

\ A pass calls this once at the start of its own run, before it reads a row.
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
   V-FUNR VW IR-FUN:FFUNS ;

\ Arity is a fact of the FUNCTION and not of the emission: a module holds a
\ routine per quotation, and a routine contract states one arity for them all.
: FUN-ARITY ( IR-ID:ir-fun-id -- n n )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@ ;

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

\ Read off the block's own row rather than taken as the last operation.
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

\ A non-terminator names none and answers zero; that is the schema's rule.
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

\ Which key it is under, and whether it was allowed, is the reading pass's.
: ATTR-INT-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-ATTR VW  V-OPP VW V-OPR VW MKEY id i IR-OP:FATTR@  IR-ATTR:FINT@ ;

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
