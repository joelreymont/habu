\ fun.f - the append-only function and block store: one row per function, one
\ row per basic block, and one shared cell pool holding every function's
\ attribute window.
\
\ docs/compiler-ir-design.md section 6.3 (the function table and the block
\ table), section 6.4 (the BEGIN-FUN/END-FUN and BEGIN-BLOCK/ADD-BLOCK-ARG/
\ END-BLOCK builder API), section 6.5 (the freeze checks that consume these
\ rows), and section 7.2, whose stage builds one function per colon definition.
\ This file stores the control structure of a module; it does not define what
\ any dialect's operations mean, which section 5.3 leaves to the schema table.
\
\ ONE CONCERN, TWO ROW TABLES. Functions and blocks are one responsibility for
\ the same reason IR-OP's operations and values are: they are defined by the
\ same act. A block cannot exist without the function that is being built around
\ it, and a function's record is not complete until the last of its blocks is
\ in. Each table needs the other's live count - a function row's block window is
\ read off the block table's count, and a block row's parent is read off the
\ function table's count - so splitting them across two files would make each
\ depend on the other and break the single all-or-nothing append each of them
\ guarantees. They therefore share one package and one file, and the file keeps
\ them in separate row tables so a later canonical encoder can order and
\ renumber each independently.
\
\ IDENTITY COMES FROM IR-ID. Functions are the existing IR-ID ir-fun-id family
\ and blocks the ir-block-id family, both packed under this module's key. This
\ file mints no identity family of its own, no raw converter, and no parallel
\ numbering.
\
\ WHAT A FUNCTION ROW RECORDS (design lines 384-392).
\   symbol name          - design line 384, the module's interned symbol, whose
\                          existence the symbol interner confirms
\   signature type       - design line 385, a code-reference type of this
\                          module's type table: design line 456 is the type
\                          table's name for "a callable with this effect", and a
\                          signature that is anything else is E-IR-FUN-SIG
\   block window         - design lines 386-387, first-block and block-count
\   attribute window     - design line 388
\   source span          - design line 389, as the (source, start, length)
\                          triple IR-SOURCE:span already defines
\   linkage              - design line 390
\   visibility           - the second half of design line 390's link-time
\                          question, see LINKAGE AND VISIBILITY below
\   calling convention   - design line 391
\ One section 6.3 field is deliberately absent: flags (design line 392) names no
\ member anywhere in the design, and an empty flag word is not a capability -
\ the stage that needs a flag adds it together with the meaning that makes it
\ checkable. This is the same judgment IR-OP recorded for the same field.
\
\ WHAT A BLOCK ROW RECORDS (design lines 400-407).
\   parent function      - design line 400
\   argument window      - design line 401, into the value table, see BLOCK
\                          ARGUMENTS below
\   operation window     - design line 402, into IR-OP's operation table
\   terminator operation - design line 403
\   source span          - design line 406
\ Three section 6.3 fields are deliberately absent:
\   predecessor-count and successor-count (design lines 404-405) are not the
\     builder's to maintain: design line 410 says in as many words that the
\     predecessor and successor tables are derived at freeze time rather than
\     maintained through every builder mutation. Storing a count here during
\     construction would publish a number that is wrong for as long as any
\     branch to the block is still unwritten, so the section 6.5 freeze verifier
\     derives them together with the tables they belong to;
\   flags (design line 407), for the reason given above.
\
\ AN OPERATION'S PARENT BLOCK LIVES HERE (design line 418). IR-OP deliberately
\ omits the parent-block field from an operation row, and this file supplies it
\ as the block's operation window rather than as a back-reference on every
\ operation. The window is the stronger of the two. Blocks are built in order
\ and each block's operations are appended consecutively, so block l's window
\ starts exactly where block l-1's ended: the windows tile IR-OP's operation
\ table, every operation from zero to the last block's end therefore belongs to
\ exactly one block by construction, and no scan or agreement check is needed to
\ prove it. A stored back-reference would be a second authority that could
\ disagree with the window, and section 6.5's "every operation belongs to
\ exactly one block" would then be a cross-table search rather than an O(1) read
\ of one row against the row before it. Operations appended while no block is
\ open are the one case the tiling has to catch, and BEGIN-BLOCK is what catches
\ it: the block captures the operation count when it opens, so an operation
\ appended outside a block leaves a gap the tiling check rejects.
\
\ TERMINATOR PLACEMENT (design line 403). A block's terminator is its last
\ operation. END-BLOCK reads every operation in the window, asks the opcode's
\ schema whether it is a terminator (design line 240's terminator flag), and
\ requires exactly one - at the end. A block with no operation has no terminator
\ and is rejected too. The ordinal is then stored, so the freeze verifier and
\ every later pass read the terminator in constant time instead of rescanning
\ the window, and a forged row whose terminator is not its last operation is a
\ named reject rather than a misread.
\
\ BLOCK ARGUMENTS ARE VALUES (design lines 401 and 434). A block argument is
\ minted through IR-OP's value store, because that store owns value rows
\ whatever defines them; this file records only the window they fill. The window
\ is a run of value ordinals rather than a pool of copies, and it does not have
\ to tile the value table - operation results are interleaved between one
\ block's arguments and the next block's - because each value row itself names
\ the block and the argument position it belongs to. END-BLOCK revalidates the
\ whole run against those rows, and every window read revalidates one element
\ the same way: the value must be a block argument, of this block, at this
\ position. Two blocks therefore cannot share an argument value at all, which is
\ a stronger fact than non-overlap, and it stays an O(1) check on the element.
\ An argument added after the block's first operation breaks the run and is
\ rejected by the same check, which is what makes out-of-order construction
\ fail.
\
\ LINKAGE AND VISIBILITY are the two independent link-time questions an object
\ file asks about a defined name, and a single enum cannot answer both:
\   linkage    - where the body comes from and how duplicate definitions
\                resolve: defined, replaceable, or imported;
\   visibility - who outside this module may name it: hidden or exported.
\ The combinations are not free, and the illegal ones are rejected rather than
\ stored: an imported function has no body here, so its block window must be
\ empty, and it must be exported, because a name nothing outside can see can
\ never be resolved from outside; a replaceable definition must likewise be
\ exported, because nothing hidden inside this module can be replaced from
\ outside; and a function that supplies a body must have at least an entry
\ block. The one hidden shape is a plain hidden definition.
\
\ THE CALLING CONVENTION IS CHECKED AGAINST THE BOUND TARGET (design line 391
\ with section 5.4). Each convention names the architecture that can implement
\ it - the section 7.6 Habu word ABI and the target contract's C ABI are
\ AArch64 facts, a kernel entry point is a PTX fact - and END-FUN rejects a
\ convention the context's bound contract cannot provide, exactly as IR-SCHEMA
\ rejects an operation the bound contract cannot execute.
\
\ APPENDING IS ALL-OR-NOTHING. Every check - the stores' headers and module
\ binding, the staged fields, each referenced symbol, type, attribute, value,
\ operation and source span, the window tiling, the linkage rules, the target
\ contract, and both committed ceilings - runs before the first IR-ARENA:PUSH.
\ A rejected append therefore leaves the stores exactly as it found them. The
\ one mutation this file performs outside an end is ADD-BLOCK-ARG, which mints a
\ value row through IR-OP: an argument must exist before the operations that
\ take it, and those operations must exist before their block's row can record
\ its window, so the value row is necessarily first. A block whose END-BLOCK is
\ then rejected leaves its argument values behind, and that is what the section
\ 6.2 builder ABORT is for - a structural append that fails discards the module
\ rather than being retried, and the freeze verifier rejects any value whose
\ defining block does not exist.
\
\ STORE SHAPES. Three IR-ARENA arenas owned by the compilation context: the
\ attribute cell pool, the function row table, and the block row table. Each
\ carries the usual three-cell header - format tag, owning module serial,
\ committed capacity - so presenting one store where another belongs is a
\ format-tag reject rather than a misread, and every access rechecks shape,
\ window, and stored ordinals fail-closed with E-IR-FUN-STATE.
\
\ CEILINGS AND THE CONTEXT MAPPING. Capacities are creation parameters bounded
\ by the ordinal range a row or pool cell can address, and the practical limit
\ belongs to the context mapping, exactly as recorded in op.f. Hitting a
\ committed ceiling is a named error thrown before any mutation, so a full store
\ stays readable and keeps every identity it issued.
\
\ NOTHING PUBLIC MUTATES A FROZEN STORE. Every word here that writes takes an
\ IR-ARENA:arena builder handle, and the checker rejects an IR-ARENA:view in
\ that position, so a mutation cannot even be spelled against a frozen store.
\ Once IR-ARENA:FREEZE consumes a handle, the retired handle rejects every live
\ word with E-IR-ARENA-FROZEN. There is no publication, retirement, or lifecycle
\ word in this file: the module freeze belongs to the builder stage.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f

package IR-FUN
public

\ Design line 390: where a function's body comes from and how two definitions of
\ one name resolve. The vocabulary is closed, so an unknown linkage is
\ unrepresentable in checked code; the wire codes below persist it.
ENUM linkage DERIVE eq
   defined
   replaceable
   imported
;ENUM

\ Who outside this module may name the function.
ENUM visibility DERIVE eq
   hidden
   exported
;ENUM

\ Design line 391: how arguments and results cross a call boundary. Each member
\ belongs to exactly one architecture, which is what binds it to the context's
\ target contract.
ENUM convention DERIVE eq
   habu
   c-abi
   kernel
;ENUM

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner comparison.
\ Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$464E5031 constant FNP-MAGIC         \ "FNP1": the attribute-pool header tag
$464E5231 constant FNR-MAGIC         \ "FNR1": the function-table header tag
$424C5231 constant BLR-MAGIC         \ "BLR1": the block-table header tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-SYM                   \ design line 384: the symbol name
1 constant OFF-SIG                   \ design line 385: the signature type
2 constant OFF-BST                   \ design line 386: first-block
3 constant OFF-BN                    \ design line 387: block-count
4 constant OFF-ATST                  \ design line 388: the attribute window
5 constant OFF-ATN
6 constant OFF-SRC                   \ design line 389: the source span
7 constant OFF-SBEG
8 constant OFF-SLEN
9 constant OFF-LNK                   \ design line 390: linkage
10 constant OFF-VIS                  \ the visibility half of the same question
11 constant OFF-CC                   \ design line 391: the calling convention
12 constant FNROW-CELLS

0 constant OFF-PAR                   \ design line 400: the parent function
1 constant OFF-AGST                  \ design line 401: the argument window
2 constant OFF-AGN
3 constant OFF-OPST                  \ design line 402: the operation window
4 constant OFF-OPN
5 constant OFF-TERM                  \ design line 403: the terminator operation
6 constant OFF-BSRC                  \ design line 406: the source span
7 constant OFF-BBEG
8 constant OFF-BLEN
9 constant BROW-CELLS

public
$FFFFFFFF HDR-CELLS - FNROW-CELLS / constant FNROW-CAP-MAX
$FFFFFFFF HDR-CELLS - BROW-CELLS / constant BLK-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX
private
32 constant ATTR-MAX                 \ committed staged attributes per function

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump in the canonical encoder.
0 constant LK-DEFINED
1 constant LK-REPLACEABLE
2 constant LK-IMPORTED
0 constant VS-HIDDEN
1 constant VS-EXPORTED
0 constant CV-HABU
1 constant CV-C
2 constant CV-KERNEL

: LNK-CODE ( IR-FUN:linkage -- n )
   MATCH linkage
      defined     OF LK-DEFINED ENDOF
      replaceable OF LK-REPLACEABLE ENDOF
      imported    OF LK-IMPORTED ENDOF
   ;MATCH ;

: VIS-CODE ( IR-FUN:visibility -- n )
   MATCH visibility
      hidden   OF VS-HIDDEN ENDOF
      exported OF VS-EXPORTED ENDOF
   ;MATCH ;

: CC-CODE ( IR-FUN:convention -- n )
   MATCH convention
      habu   OF CV-HABU ENDOF
      c-abi  OF CV-C ENDOF
      kernel OF CV-KERNEL ENDOF
   ;MATCH ;

\ A stored code outside the family's vocabulary is a corrupted or forged row, so
\ each decoder rejects it named instead of reading as some other member.
: N>LNK ( n -- IR-FUN:linkage )
   case
      LK-DEFINED     of IR--FUN-LINKAGE:DEFINED endof
      LK-REPLACEABLE of IR--FUN-LINKAGE:REPLACEABLE endof
      LK-IMPORTED    of IR--FUN-LINKAGE:IMPORTED endof
      E-IR-FUN-STATE throw
   endcase ;

: N>VIS ( n -- IR-FUN:visibility )
   case
      VS-HIDDEN   of IR--FUN-VISIBILITY:HIDDEN endof
      VS-EXPORTED of IR--FUN-VISIBILITY:EXPORTED endof
      E-IR-FUN-STATE throw
   endcase ;

: N>CC ( n -- IR-FUN:convention )
   case
      CV-HABU   of IR--FUN-CONVENTION:HABU endof
      CV-C      of IR--FUN-CONVENTION:C-ABI endof
      CV-KERNEL of IR--FUN-CONVENTION:KERNEL endof
      E-IR-FUN-STATE throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
\ Every read below goes through an IR-ARENA reader: each store is resolved ONCE,
\ at the public word, and the helpers take the resolved readers. The live/frozen
\ twins that used to run down this file collapse into one set, because a reader
\ carries the state it was opened against and refuses the other with the error
\ the handle would have given - the only thing the two entry points still differ
\ in is OPEN-LIVE against OPEN. The three readers are named for the header tags
\ they carry: fnp the attribute pool, fnr the function table, blr the blocks.

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-FUN-STATE throw then ;

: FNSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-FUN-STATE throw then
   HDR-CELLS - FNROW-CELLS mod 0 <> if E-IR-FUN-STATE throw then ;

: BSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-FUN-STATE throw then
   HDR-CELLS - BROW-CELLS mod 0 <> if E-IR-FUN-STATE throw then ;

: MAGIC-CK ( n n -- )
   <> if E-IR-FUN-STATE throw then ;

: PHDR-CK ( IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader :}
   fnp IR-ARENA:RD-SIZE PSHAPE-CK
   FNP-MAGIC fnp HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: FNHDR-CK ( IR-ARENA:reader -- )
   {: fnr:IR-ARENA:reader :}
   fnr IR-ARENA:RD-SIZE FNSHAPE-CK
   FNR-MAGIC fnr HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: BHDR-CK ( IR-ARENA:reader -- )
   {: blr:IR-ARENA:reader :}
   blr IR-ARENA:RD-SIZE BSHAPE-CK
   BLR-MAGIC blr HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: FNCNT ( IR-ARENA:reader -- n )
   dup FNHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - FNROW-CELLS / ;

: BCNT ( IR-ARENA:reader -- n )
   dup BHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - BROW-CELLS / ;

: PCELLS ( IR-ARENA:reader -- n )
   dup PHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-FUN-OWNER throw then ;

\ The three stores are one table only when all three carry the same owning
\ module serial, so a cross-module trio rejects before a row window is trusted
\ against the wrong pool.
: TRIO-CK ( IR-ARENA:reader IR-ARENA:reader IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader blr:IR-ARENA:reader :}
   fnp PHDR-CK
   fnr FNHDR-CK
   blr BHDR-CK
   fnp HC-SERIAL IR-ARENA:RD@ fnr HC-SERIAL IR-ARENA:RD@ SERIAL-CK
   blr HC-SERIAL IR-ARENA:RD@ fnr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: FNKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: fnr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   fnr FNHDR-CK
   fnr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: BKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   blr BHDR-CK
   blr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

\ An attribute-window lookup holds only the pool and the function table, so it
\ checks that pairing itself: both stores are what their tags claim, both carry
\ the same owning module serial, and that serial is the presented key's.
: PFN-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   fnp PHDR-CK
   fnr key FNKEY-CK
   fnp HC-SERIAL IR-ARENA:RD@ fnr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

\ A block-table lookup that also names the function table checks the two are one
\ module's pair, so a foreign function table can never answer a parent lookup.
: BFN-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: blr:IR-ARENA:reader fnr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   blr key BKEY-CK
   fnr key FNKEY-CK ;

\ ---- identity projections ----------------------------------------------------
: SYM-ORD ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-LOCAL ;

: SYM-OWNER ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

: TYP-OWNER ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

: ATTR-OWNER ( IR-ID:ir-attr-id -- n )
   IR-ID:ATTR-OWNER MID-SERIAL ;

: SRC-OWNER ( IR-ID:ir-source-id -- n )
   IR-ID:SOURCE-OWNER MID-SERIAL ;

\ A stored ordinal only has to be non-negative here: the table that owns the
\ reference revalidates the id it is handed.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-FUN-STATE throw then ;

: LEN-OK ( n -- n )
   dup 0 < if E-IR-FUN-STATE throw then ;

: STEP-CK ( n n -- )
   <> if E-IR-FUN-WINDOW throw then ;

: ROW-ORD ( n n IR-ID:ir-fun-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-fun-id :}
   id IR-ID:FUN-OWNER MID-SERIAL hs SERIAL-CK
   id IR-ID:FUN-LOCAL
   dup 0 < over cnt >= or if E-IR-FUN-BOUND throw then ;

: BLK-ORD ( n n IR-ID:ir-block-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-block-id :}
   id IR-ID:BLOCK-OWNER MID-SERIAL hs SERIAL-CK
   id IR-ID:BLOCK-LOCAL
   dup 0 < over cnt >= or if E-IR-FUN-BOUND throw then ;

: FNROW-AT ( IR-ARENA:reader IR-ID:ir-fun-id -- n )
   {: fnr:IR-ARENA:reader id:IR-ID:ir-fun-id :}
   fnr HC-SERIAL IR-ARENA:RD@ fnr FNCNT id ROW-ORD ;

: BROW-AT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: blr:IR-ARENA:reader id:IR-ID:ir-block-id :}
   blr HC-SERIAL IR-ARENA:RD@ blr BCNT id BLK-ORD ;

\ ---- row and pool addressing -------------------------------------------------
: FNROW-CELL ( n n -- n )
   swap FNROW-CELLS * HDR-CELLS + + ;

: BROW-CELL ( n n -- n )
   swap BROW-CELLS * HDR-CELLS + + ;

: FNC@ ( IR-ARENA:reader n n -- n )
   FNROW-CELL IR-ARENA:RD@ ;

: BC@ ( IR-ARENA:reader n n -- n )
   BROW-CELL IR-ARENA:RD@ ;

: PC@ ( IR-ARENA:reader n -- n )
   HDR-CELLS + IR-ARENA:RD@ ;

\ ---- the window tilings ------------------------------------------------------
\ A function row's attribute window continues exactly where the row before it
\ ended and ends inside the live pool, so overlap and gaps are both impossible
\ and non-overlap costs one comparison against one neighbouring row.
: FNAT-END ( IR-ARENA:reader n -- n )
   {: fnr:IR-ARENA:reader l:n :}
   fnr l OFF-ATST FNC@ LEN-OK  fnr l OFF-ATN FNC@ LEN-OK + ;

: ATILE-CK ( IR-ARENA:reader IR-ARENA:reader n -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader l:n :}
   l 0= if 0 else fnr l 1- FNAT-END then {: at:n :}
   at fnr l OFF-ATST FNC@ STEP-CK
   fnr l FNAT-END fnp PCELLS > if E-IR-FUN-STATE throw then ;

\ A function row's block window continues exactly where the row before it ended.
\ Its upper bound belongs to the block table, so the readers that hold one check
\ it there.
: FNBK-END ( IR-ARENA:reader n -- n )
   {: fnr:IR-ARENA:reader l:n :}
   fnr l OFF-BST FNC@ LEN-OK  fnr l OFF-BN FNC@ LEN-OK + ;

: BTILE-CK ( IR-ARENA:reader n -- )
   {: fnr:IR-ARENA:reader l:n :}
   l 0= if 0 else fnr l 1- FNBK-END then
   fnr l OFF-BST FNC@ STEP-CK ;

\ A block row's operation window continues exactly where the row before it
\ ended. Its upper bound belongs to IR-OP's operation table.
: BOP-END ( IR-ARENA:reader n -- n )
   {: blr:IR-ARENA:reader l:n :}
   blr l OFF-OPST BC@ LEN-OK  blr l OFF-OPN BC@ LEN-OK + ;

: OTILE-CK ( IR-ARENA:reader n -- )
   {: blr:IR-ARENA:reader l:n :}
   l 0= if 0 else blr l 1- BOP-END then
   blr l OFF-OPST BC@ STEP-CK ;

\ One element of a stored window, read only after the row's tiling holds.
: AWIN@ ( IR-ARENA:reader IR-ARENA:reader n n -- n )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader l:n i:n :}
   fnp fnr l ATILE-CK
   fnr l OFF-ATN FNC@ {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   fnp  fnr l OFF-ATST FNC@ i +  PC@ ORD-OK ;

\ ---- the staged function and block --------------------------------------------
\ Two package-owned stages under the single-task compilation discipline, in the
\ shape IR-TYPE, IR-ATTR, IR-SCHEMA, and IR-OP established. The block stage
\ nests inside the function stage: a block may only be opened while a function
\ is open, which is what makes an orphan block unrepresentable. Any end consumes
\ its stage, so neither a rejected append nor an end without a begin can leave a
\ half-staged record behind.
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

here CELL 1- and CELL swap - CELL 1- and allot
variable FSTG-MODE
MODE-NONE FSTG-MODE !
variable FSTG-SYM
variable FSTG-SYMO
variable FSTG-SIG
variable FSTG-SIGO
variable FSTG-BST
variable FSTG-SRC
variable FSTG-SRCO
variable FSTG-SBEG
variable FSTG-SLEN
variable FSTG-LNK
variable FSTG-VIS
variable FSTG-CC
variable FSTG-ATN
create FSTG-AT ATTR-MAX cells allot
create FSTG-ATO ATTR-MAX cells allot

variable BSTG-MODE
MODE-NONE BSTG-MODE !
variable BSTG-OPST
variable BSTG-AGST
variable BSTG-AGN
variable BSTG-SRC
variable BSTG-SRCO
variable BSTG-SBEG
variable BSTG-SLEN

: AT@ ( n -- n )
   cells FSTG-AT + @ ;

: AT! ( n n -- )
   cells FSTG-AT + ! ;

: ATO@ ( n -- n )
   cells FSTG-ATO + @ ;

: ATO! ( n n -- )
   cells FSTG-ATO + ! ;

: FSTG-RESET ( -- )
   UNSET FSTG-SIG !  0 FSTG-SIGO !
   UNSET FSTG-SRC !  0 FSTG-SRCO !  0 FSTG-SBEG !  0 FSTG-SLEN !
   UNSET FSTG-LNK !  UNSET FSTG-VIS !  UNSET FSTG-CC !
   0 FSTG-ATN ! ;

: BSTG-RESET ( -- )
   0 BSTG-AGN !  UNSET BSTG-AGST !
   UNSET BSTG-SRC !  0 BSTG-SRCO !  0 BSTG-SBEG !  0 BSTG-SLEN ! ;

: FSTG-OPEN-CK ( -- )
   FSTG-MODE @ MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: BSTG-OPEN-CK ( -- )
   BSTG-MODE @ MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: FSTG-TAKE ( -- )
   FSTG-MODE @ {: have:n :}
   MODE-NONE FSTG-MODE !
   have MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: BSTG-TAKE ( -- )
   BSTG-MODE @ {: have:n :}
   MODE-NONE BSTG-MODE !
   have MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: ONCE-CK ( n -- )
   UNSET <> if E-IR-FUN-STAGE throw then ;

: ATTR+ ( IR-ID:ir-attr-id -- )
   {: a:IR-ID:ir-attr-id :}
   FSTG-ATN @ ATTR-MAX >= if E-IR-FUN-CAP throw then
   FSTG-ATN @ {: n:n :}
   a IR-ID:ATTR-LOCAL n AT!
   a ATTR-OWNER n ATO!
   n 1+ FSTG-ATN ! ;

\ ---- staged validation -------------------------------------------------------
: OWNED-CK ( n n -- )
   {: hs:n owner:n :}
   owner hs SERIAL-CK ;

: FNFIELDS-CK ( -- )
   FSTG-SIG @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-SRC @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-LNK @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-VIS @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-CC @ UNSET = if E-IR-FUN-FIELD throw then ;

: BFIELDS-CK ( -- )
   BSTG-SRC @ UNSET = if E-IR-FUN-FIELD throw then ;

\ Section 6.5's symbol uniqueness rule at the point the name is claimed: one
\ module's function table defines a name at most once.
: DUP-CK ( IR-ARENA:reader -- )
   {: fnr:IR-ARENA:reader :}
   fnr FNCNT 0 ?do
      fnr i OFF-SYM FNC@ FSTG-SYM @ = if E-IR-FUN-DUP throw then
   loop ;

\ Design line 385: a function's signature is the type table's name for a
\ callable with an effect, which design line 456 calls a code reference.
: SIG-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: tr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL FSTG-SIGO @ OWNED-CK
   tr  key FSTG-SIG @ IR-ID:PACK-TYPE  IR-TYPE:KIND@
   IR--TYPE-KIND:CODE-REF IR--TYPE-KIND:EQ
   0= if E-IR-FUN-SIG throw then ;

: ATTRS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: ar:IR-ARENA:arena key:IR-ID:ir-module-key :}
   FSTG-ATN @ 0 ?do
      key KEY-SERIAL i ATO@ OWNED-CK
      ar  key i AT@ IR-ID:PACK-ATTR  IR-ATTR:KIND@ drop
   loop ;

: FNSPAN-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL FSTG-SRCO @ OWNED-CK
   sa  key FSTG-SRC @ IR-ID:PACK-SOURCE FSTG-SBEG @ FSTG-SLEN @ IR--SOURCE-SPAN:MAKE
   IR-SOURCE:SPAN-CK ;

: BSPAN-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL BSTG-SRCO @ OWNED-CK
   sa  key BSTG-SRC @ IR-ID:PACK-SOURCE BSTG-SBEG @ BSTG-SLEN @ IR--SOURCE-SPAN:MAKE
   IR-SOURCE:SPAN-CK ;

\ The linkage rules of the header: an imported function declares a body it does
\ not carry and must be nameable from outside; a replaceable definition must be
\ nameable from outside to be replaced; a function that carries a body needs at
\ least an entry block.
: BODY-CK ( n -- )
   {: bn:n :}
   FSTG-LNK @ LK-IMPORTED = if
      bn 0 <> if E-IR-FUN-LINKAGE throw then
      FSTG-VIS @ VS-EXPORTED <> if E-IR-FUN-LINKAGE throw then
      exit
   then
   bn 1 < if E-IR-FUN-LINKAGE throw then
   FSTG-LNK @ LK-REPLACEABLE = FSTG-VIS @ VS-EXPORTED <> and
   if E-IR-FUN-LINKAGE throw then ;

\ Design line 391 with section 5.4: each convention belongs to one KIND of
\ architecture, and the context's bound contract has to be of that kind. A
\ kernel entry point is a PTX fact and nothing else can provide it. The section
\ 7.6 Habu word ABI and the target contract's C ABI are facts of a NATIVE
\ architecture and not of one native architecture: every backend that compiles
\ Habu words hands arguments over in the same data-stack slots (docs/x86-64.md,
\ "Calls between compiled words"), and the C ABI a contract names is its own -
\ AAPCS64 on AArch64, SysV on x86-64. Naming AArch64 here refused every
\ function of a second native backend before it had an operation in it.
: CC-KERNEL? ( n -- bool )
   CV-KERNEL = ;

: PTX? ( CTARGET:arch -- bool )
   CTARGET-ARCH:PTX CTARGET-ARCH:EQ ;

: TARGET-CK ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ PTX? {: ptx:bool :}
   FSTG-CC @ CC-KERNEL? if
      ptx 0= if E-IR-FUN-TARGET throw then
      exit
   then
   ptx if E-IR-FUN-TARGET throw then ;

\ Every block this function is about to claim must already name this function as
\ its parent. The block window and the parent field are both section 6.3 fields
\ and this is where they are made to agree, so a later cross-function block use
\ is a disagreement between two records rather than an unchecked assumption.
: PARENTS-CK ( IR-ARENA:reader n n n -- )
   {: blr:IR-ARENA:reader l:n bst:n bn:n :}
   bn 0 ?do
      blr bst i + OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then
   loop ;

\ ---- block-side staged validation --------------------------------------------
\ Every operation the block is about to claim, asked of the opcode's schema:
\ exactly one terminator, and it is the last (design line 403).
: TERM-AT-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n n -- )
   {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n i:n last:n :}
   qr  r key  key opst i + IR-ID:PACK-OP  IR-OP:OPCODE@  IR-SCHEMA:TERMINATOR?
   {: t:bool :}
   t if
      i last <> if E-IR-FUN-TERM throw then
      exit
   then
   i last = if E-IR-FUN-TERM throw then ;

: TERM-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n -- )
   {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n opn:n :}
   opn 1 < if E-IR-FUN-TERM throw then
   opn 0 ?do
      r qr key opst i opn 1- TERM-AT-CK
   loop ;

\ Every value the block is about to claim as an argument, asked of the value row
\ itself: a block argument, of this block, at this position. Two blocks cannot
\ share one argument, and an argument minted after the block's first operation
\ breaks the run and lands here.
: ARGS-CK ( IR-ARENA:arena IR-ID:ir-module-key n n n -- )
   {: v:IR-ARENA:arena key:IR-ID:ir-module-key l:n agst:n agn:n :}
   agn 0 ?do
      key agst i + IR-ID:PACK-VALUE {: id:IR-ID:ir-value-id :}
      v id IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
      0= if E-IR-FUN-ARG throw then
      v key id IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
      v id IR-OP:VALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   loop ;

\ ---- room and append ---------------------------------------------------------
: FNROOM-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader :}
   fnr FNCNT fnr HC-CAP IR-ARENA:RD@ >= if E-IR-FUN-CAP throw then
   fnp PCELLS FSTG-ATN @ + fnp HC-CAP IR-ARENA:RD@ > if E-IR-FUN-CAP throw then ;

\ Ending a function writes the attribute pool and then the function row, so
\ both are reserved here, before either is touched: an attribute window that
\ landed without its row leaves the pool and the row table disagreeing about
\ what the function owns. Reserving after the room check keeps this store's own
\ named capacity error ahead of the arena's. The readers opened at the public
\ word outlive both reservations: a RESERVE changes neither the registry
\ generation nor the state, and a reader re-reads the row's pointer and count
\ on every call, so it follows the reservation's new span.
: FNROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena f:IR-ARENA:arena :}
   c p FSTG-ATN @ IR-ARENA:RESERVE
   c f FNROW-CELLS IR-ARENA:RESERVE ;

: BROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-ARENA:arena :}
   c b BROW-CELLS IR-ARENA:RESERVE ;

: BROOM-CK ( IR-ARENA:reader -- )
   {: blr:IR-ARENA:reader :}
   blr BCNT blr HC-CAP IR-ARENA:RD@ >= if E-IR-FUN-CAP throw then ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: ATTRS-ADD ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena :}
   FSTG-ATN @ 0 ?do
      c p i AT@ CELL+
   loop ;

: FNROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx f:IR-ARENA:arena atst:n bst:n bn:n :}
   c f FSTG-SYM @ CELL+
   c f FSTG-SIG @ CELL+
   c f bst CELL+   c f bn CELL+
   c f atst CELL+  c f FSTG-ATN @ CELL+
   c f FSTG-SRC @ CELL+
   c f FSTG-SBEG @ CELL+
   c f FSTG-SLEN @ CELL+
   c f FSTG-LNK @ CELL+
   c f FSTG-VIS @ CELL+
   c f FSTG-CC @ CELL+ ;

: BROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n n -- )
   {: c:IR-CTX:ctx b:IR-ARENA:arena par:n agst:n opst:n opn:n :}
   c b par CELL+
   c b agst CELL+  c b BSTG-AGN @ CELL+
   c b opst CELL+  c b opn CELL+
   c b opst opn + 1- CELL+
   c b BSTG-SRC @ CELL+
   c b BSTG-SBEG @ CELL+
   c b BSTG-SLEN @ CELL+ ;

\ ---- creation checks ---------------------------------------------------------
: FNROW-CAP-OK ( n -- )
   dup 1 < over FNROW-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

: BLK-CAP-OK ( n -- )
   dup 0 < over BLK-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 0 < over POOL-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Create one module's function and block store: an attribute pool committed to
\ exactly pcap cells, a function table committed to exactly fcap functions, and
\ a block table committed to exactly bcap blocks, all three headers bound to
\ key's module serial. The three handles plus the key are the store; they stay
\ with the module owner and die with the owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key fcap:n bcap:n pcap:n :}
   fcap FNROW-CAP-OK
   bcap BLK-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p HDR-CELLS IR-ARENA:RESERVE
   c p FNP-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c fcap FNROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: f:IR-ARENA:arena :}
   c f HDR-CELLS IR-ARENA:RESERVE
   c f FNR-MAGIC CELL+
   c f key KEY-SERIAL CELL+
   c f fcap CELL+
   c bcap BROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: b:IR-ARENA:arena :}
   c b HDR-CELLS IR-ARENA:RESERVE
   c b BLR-MAGIC CELL+
   c b key KEY-SERIAL CELL+
   c b bcap CELL+
   p f b ;

\ ---- the function builder (design lines 498-499) ------------------------------
\ Open one function, named by the module's interned symbol. The block table's
\ live count is captured here and revalidated at the end as the start of this
\ function's block window, so blocks appended for a function that was then
\ abandoned cannot be absorbed by the next one.
: BEGIN-FUN ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: b:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   FSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   b IR-ARENA:OPEN-LIVE BCNT {: bst:n :}
   MODE-OPEN FSTG-MODE !
   FSTG-RESET
   sym SYM-ORD FSTG-SYM !
   sym SYM-OWNER FSTG-SYMO !
   bst FSTG-BST ! ;

\ Abandon an open function without appending it.
: ABANDON-FUN ( -- )
   FSTG-TAKE ;

\ Design line 385: the callable type this function implements.
: SET-SIGNATURE ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   FSTG-OPEN-CK
   FSTG-SIG @ ONCE-CK
   t IR-ID:TYPE-LOCAL FSTG-SIG !
   t TYP-OWNER FSTG-SIGO ! ;

\ Design line 390: how this function's definition binds.
: SET-LINKAGE ( IR-FUN:linkage -- )
   FSTG-OPEN-CK
   FSTG-LNK @ ONCE-CK
   LNK-CODE FSTG-LNK ! ;

\ Who outside this module may name it.
: SET-VISIBILITY ( IR-FUN:visibility -- )
   FSTG-OPEN-CK
   FSTG-VIS @ ONCE-CK
   VIS-CODE FSTG-VIS ! ;

\ Design line 391: how a call to this function passes its arguments.
: SET-CONVENTION ( IR-FUN:convention -- )
   FSTG-OPEN-CK
   FSTG-CC @ ONCE-CK
   CC-CODE FSTG-CC ! ;

\ Design line 388: one attribute this function carries.
: ADD-FUN-ATTR ( IR-ID:ir-attr-id -- )
   FSTG-OPEN-CK
   ATTR+ ;

\ Design line 389: where this function came from.
: SET-FUN-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   FSTG-OPEN-CK
   FSTG-SRC @ ONCE-CK
   src IR-ID:SOURCE-LOCAL FSTG-SRC !
   src SRC-OWNER FSTG-SRCO !
   st FSTG-SBEG !
   ln FSTG-SLEN ! ;

\ Close the staged function: validate it whole against these stores, the
\ module's symbol, type, attribute, and source tables, the block table it is
\ claiming from, the linkage rules, the bound target contract, and both
\ committed ceilings, then append its attribute cells and its row. The stage is
\ consumed whatever the outcome, and every check runs before the first cell is
\ written, so an append either lands whole or changes nothing.
: END-FUN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx p:IR-ARENA:arena f:IR-ARENA:arena b:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   FSTG-TAKE
   BSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   p IR-ARENA:OPEN-LIVE {: fnp:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   fnp fnr blr TRIO-CK
   fnr key FNKEY-CK
   FNFIELDS-CK
   key KEY-SERIAL FSTG-SYMO @ OWNED-CK
   sr  key FSTG-SYM @ IR-ID:PACK-SYMBOL  IR-SYM:LEN@ drop
   fnr DUP-CK
   tr key SIG-CK
   ar key ATTRS-CK
   sa key FNSPAN-CK
   fnr FNCNT {: l:n :}
   l 0= if 0 else fnr l 1- FNBK-END then FSTG-BST @ STEP-CK
   blr BCNT FSTG-BST @ - {: bn:n :}
   bn 0 < if E-IR-FUN-WINDOW throw then
   bn BODY-CK
   c TARGET-CK
   blr l FSTG-BST @ bn PARENTS-CK
   fnp fnr FNROOM-CK
   c p f FNROOM-TAKE
   fnp PCELLS {: atst:n :}
   c p ATTRS-ADD
   c f atst FSTG-BST @ bn FNROW-ADD
   key l IR-ID:PACK-FUN ;

\ ---- the block builder (design lines 501-503) ---------------------------------
\ Open one block inside the open function. IR-OP's operation table is presented
\ so the block can capture the operation count it starts at; an operation
\ appended while no block is open therefore leaves a gap the window tiling
\ rejects instead of being absorbed by the next block.
: BEGIN-BLOCK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   FSTG-OPEN-CK
   BSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   r IR-OP:OPS {: opst:n :}
   MODE-OPEN BSTG-MODE !
   BSTG-RESET
   opst BSTG-OPST ! ;

\ Abandon an open block without appending it.
: ABANDON-BLOCK ( -- )
   BSTG-TAKE ;

\ Design line 502: one argument of this block. The value is minted through
\ IR-OP's value store, named by this block's identity and this argument's
\ position, and returned so the block's operations can take it as an operand.
: ADD-BLOCK-ARG ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx v:IR-ARENA:arena tr:IR-ARENA:arena b:IR-ARENA:arena key:IR-ID:ir-module-key t:IR-ID:ir-type-id :}
   BSTG-OPEN-CK
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   c v tr key  key blr BCNT IR-ID:PACK-BLOCK  t BSTG-AGN @ IR-OP:MINT-ARG
   {: val:IR-ID:ir-value-id :}
   BSTG-AGN @ 0= if val IR-ID:VALUE-LOCAL BSTG-AGST ! then
   BSTG-AGN @ 1+ BSTG-AGN !
   val ;

\ Design line 406: where this block came from.
: SET-BLOCK-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   BSTG-OPEN-CK
   BSTG-SRC @ ONCE-CK
   src IR-ID:SOURCE-LOCAL BSTG-SRC !
   src SRC-OWNER BSTG-SRCO !
   st BSTG-SBEG !
   ln BSTG-SLEN ! ;

\ Close the staged block: validate its parent, its operation window against
\ IR-OP's operation table, its terminator against the dialect's schema table,
\ its argument run against the value rows themselves, its source span, and the
\ committed ceiling, then append its row.
: END-BLOCK ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx b:IR-ARENA:arena f:IR-ARENA:arena key:IR-ID:ir-module-key v:IR-ARENA:arena r:IR-ARENA:arena qr:IR-ARENA:arena sa:IR-ARENA:arena :}
   BSTG-TAKE
   FSTG-OPEN-CK
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   BFIELDS-CK
   sa key BSPAN-CK
   blr BCNT {: l:n :}
   l 0= if 0 else blr l 1- BOP-END then BSTG-OPST @ STEP-CK
   r IR-OP:OPS BSTG-OPST @ - {: opn:n :}
   opn 0 < if E-IR-FUN-WINDOW throw then
   r qr key BSTG-OPST @ opn TERM-CK
   BSTG-AGN @ 0= if 0 else BSTG-AGST @ ORD-OK then {: agst:n :}
   agst BSTG-AGN @ + v IR-OP:VALUES > if E-IR-FUN-BOUND throw then
   v key l agst BSTG-AGN @ ARGS-CK
   blr BROOM-CK
   c b BROOM-TAKE
   c b fnr FNCNT agst BSTG-OPST @ opn BROW-ADD
   key l IR-ID:PACK-BLOCK ;

\ ---- table readers -----------------------------------------------------------
: FUNS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE FNCNT ;

: BLOCKS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE BCNT ;

: ATTR-CELLS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE PCELLS ;

private

: FNFLD ( IR-ARENA:reader IR-ID:ir-fun-id n -- n )
   {: fnr:IR-ARENA:reader id:IR-ID:ir-fun-id off:n :}
   fnr  fnr id FNROW-AT  off FNC@ ;

: BFLD ( IR-ARENA:reader IR-ID:ir-block-id n -- n )
   {: blr:IR-ARENA:reader id:IR-ID:ir-block-id off:n :}
   blr  blr id BROW-AT  off BC@ ;

public

\ ---- function readers --------------------------------------------------------
: SYMBOL@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SYM FNFLD ORD-OK IR-ID:PACK-SYMBOL ;

: SIGNATURE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SIG FNFLD ORD-OK IR-ID:PACK-TYPE ;

: LINKAGE@ ( IR-ARENA:arena IR-ID:ir-fun-id -- IR-FUN:linkage )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-LNK FNFLD N>LNK ;

: VISIBILITY@ ( IR-ARENA:arena IR-ID:ir-fun-id -- IR-FUN:visibility )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-VIS FNFLD N>VIS ;

: CONVENTION@ ( IR-ARENA:arena IR-ID:ir-fun-id -- IR-FUN:convention )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-CC FNFLD N>CC ;

\ The span this function came from. A span is a value, so the consumer
\ revalidates it against the registry it names with IR-SOURCE:SPAN-CK.
: SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   fnr id FNROW-AT {: l:n :}
   key fnr l OFF-SRC FNC@ ORD-OK IR-ID:PACK-SOURCE
   fnr l OFF-SBEG FNC@ LEN-OK
   fnr l OFF-SLEN FNC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: BLOCK-COUNT ( IR-ARENA:arena IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-BN FNFLD LEN-OK ;

: ATTR-COUNT ( IR-ARENA:arena IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-ATN FNFLD LEN-OK ;

: ATTR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-attr-id )
   {: p:IR-ARENA:arena f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   p IR-ARENA:OPEN-LIVE {: fnp:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnp fnr key PFN-CK
   key fnp fnr  fnr id FNROW-AT  i AWIN@ IR-ID:PACK-ATTR ;

\ One block of this function's block window. The window is revalidated as a
\ tiling of the block table, the ordinal is bound-checked against that table,
\ and the block is then asked whether it agrees that this function is its
\ parent - so a window that reaches into another function's blocks is a named
\ reject rather than a silent cross-function read.
: BLOCK@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ARENA:arena b:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   fnr id FNROW-AT {: l:n :}
   fnr l BTILE-CK
   fnr l OFF-BN FNC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   fnr l OFF-BST FNC@ LEN-OK i + {: ord:n :}
   ord blr BCNT >= if E-IR-FUN-BOUND throw then
   blr ord OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then
   key ord IR-ID:PACK-BLOCK ;

\ ---- block readers -----------------------------------------------------------
\ The function this block belongs to, bound-checked against the function table
\ that owns the reference.
: PARENT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-fun-id )
   {: b:IR-ARENA:arena f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   blr id OFF-PAR BFLD ORD-OK {: par:n :}
   par fnr FNCNT >= if E-IR-FUN-BOUND throw then
   key par IR-ID:PACK-FUN ;

: ARG-COUNT ( IR-ARENA:arena IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:arena id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE id OFF-AGN BFLD LEN-OK ;

\ One argument of this block, revalidated against the value row it names: a
\ block argument, of this block, at this position.
: ARG@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:arena v:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OFF-AGN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-AGST BC@ LEN-OK i + {: ord:n :}
   ord v IR-OP:VALUES >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-VALUE {: val:IR-ID:ir-value-id :}
   v val IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   0= if E-IR-FUN-ARG throw then
   v key val IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
   v val IR-OP:VALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   val ;

: OP-COUNT ( IR-ARENA:arena IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:arena id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE id OFF-OPN BFLD LEN-OK ;

\ One operation of this block's operation window. The window is revalidated as a
\ tiling of IR-OP's operation table and the ordinal is bound-checked there, so
\ an operation can be read through exactly one block.
: OP@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l OFF-OPN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-OPST BC@ LEN-OK i + {: ord:n :}
   ord r IR-OP:OPS >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-OP ;

\ Design line 403: the terminator this block ends in, read in constant time and
\ rechecked against the window it must end.
: TERMINATOR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l BOP-END 1- {: want:n :}
   blr l OFF-TERM BC@ ORD-OK want <> if E-IR-FUN-TERM throw then
   want r IR-OP:OPS >= if E-IR-FUN-BOUND throw then
   key want IR-ID:PACK-OP ;

: BLOCK-SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-SOURCE:span )
   {: b:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   key blr l OFF-BSRC BC@ ORD-OK IR-ID:PACK-SOURCE
   blr l OFF-BBEG BC@ LEN-OK
   blr l OFF-BLEN BC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its functions and blocks through the three arena views;
\ the retired builder handles reject every touch with E-IR-ARENA-FROZEN.
: RFUNS ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER FNCNT ;

: FFUNS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN RFUNS ;

: RBLOCKS ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER BCNT ;

: FBLOCKS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN RBLOCKS ;

: FATTR-CELLS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN PCELLS ;

: FSYMBOL@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SYM FNFLD ORD-OK IR-ID:PACK-SYMBOL ;

: RSIGNATURE@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:FROZEN-READER {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SIG FNFLD ORD-OK IR-ID:PACK-TYPE ;

: FSIGNATURE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN key id RSIGNATURE@ ;

: FLINKAGE@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:linkage )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-LNK FNFLD N>LNK ;

: FVISIBILITY@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:visibility )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-VIS FNFLD N>VIS ;

: FCONVENTION@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:convention )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-CC FNFLD N>CC ;

: FSPAN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   fnr id FNROW-AT {: l:n :}
   key fnr l OFF-SRC FNC@ ORD-OK IR-ID:PACK-SOURCE
   fnr l OFF-SBEG FNC@ LEN-OK
   fnr l OFF-SLEN FNC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: RBLOCK-COUNT ( IR-ARENA:reader IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:reader id:IR-ID:ir-fun-id :}
   f IR-ARENA:FROZEN-READER id OFF-BN FNFLD LEN-OK ;

: FBLOCK-COUNT ( IR-ARENA:view IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id RBLOCK-COUNT ;

: FATTR-COUNT ( IR-ARENA:view IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-ATN FNFLD LEN-OK ;

: FATTR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-attr-id )
   {: p:IR-ARENA:view f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   p IR-ARENA:OPEN {: fnp:IR-ARENA:reader :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   fnp fnr key PFN-CK
   key fnp fnr  fnr id FNROW-AT  i AWIN@ IR-ID:PACK-ATTR ;

: RBLOCK@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ARENA:reader b:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   b IR-ARENA:FROZEN-READER {: blr:IR-ARENA:reader :}
   f IR-ARENA:FROZEN-READER {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   fnr id FNROW-AT {: l:n :}
   fnr l BTILE-CK
   fnr l OFF-BN FNC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   fnr l OFF-BST FNC@ LEN-OK i + {: ord:n :}
   ord blr BCNT >= if E-IR-FUN-BOUND throw then
   blr ord OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then
   key ord IR-ID:PACK-BLOCK ;

: FBLOCK@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ARENA:view b:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   b IR-ARENA:OPEN {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN blr key id i RBLOCK@ ;

: FPARENT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-fun-id )
   {: b:IR-ARENA:view f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   blr id OFF-PAR BFLD ORD-OK {: par:n :}
   par fnr FNCNT >= if E-IR-FUN-BOUND throw then
   key par IR-ID:PACK-FUN ;

: RARG-COUNT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:reader id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER id OFF-AGN BFLD LEN-OK ;

: FARG-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:view id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN id RARG-COUNT ;

private

\ Admit the block before opening its companion view in the public wrappers.
\ Reader callers retain the same row and window checks.
: ARG-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- n n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OFF-AGN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   l blr l OFF-AGST BC@ LEN-OK i + ;

: ARG-VALUE ( IR-ARENA:reader IR-ID:ir-module-key n n n -- IR-ID:ir-value-id )
   {: v:IR-ARENA:reader key:IR-ID:ir-module-key l:n ord:n i:n :}
   ord v IR-OP:RVALUES >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-VALUE {: val:IR-ID:ir-value-id :}
   v val IR-OP:RVALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   0= if E-IR-FUN-ARG throw then
   v key val IR-OP:RVALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
   v val IR-OP:RVALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   val ;

public

: RARG@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:reader v:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:FROZEN-READER key id i ARG-ORD {: l:n ord:n :}
   v key l ord i ARG-VALUE ;

: FARG@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:view v:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN key id i ARG-ORD {: l:n ord:n :}
   v IR-ARENA:OPEN key l ord i ARG-VALUE ;

: ROP-COUNT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:reader id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER id OFF-OPN BFLD LEN-OK ;

: FOP-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:view id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN id ROP-COUNT ;

private

: OP-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l OFF-OPN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-OPST BC@ LEN-OK i + ;

: TERM-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id -- n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l BOP-END 1- {: want:n :}
   blr l OFF-TERM BC@ ORD-OK want <> if E-IR-FUN-TERM throw then
   want ;

: OP-ID ( IR-ARENA:reader IR-ID:ir-module-key n -- IR-ID:ir-op-id )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key ord:n :}
   ord r IR-OP:ROPS >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-OP ;

public

: ROP@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:FROZEN-READER key id i OP-ORD {: ord:n :}
   r key ord OP-ID ;

: FOP@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN key id i OP-ORD {: ord:n :}
   r IR-ARENA:OPEN key ord OP-ID ;

: RTERMINATOR@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER key id TERM-ORD {: ord:n :}
   r key ord OP-ID ;

: FTERMINATOR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN key id TERM-ORD {: ord:n :}
   r IR-ARENA:OPEN key ord OP-ID ;

: FBLOCK-SPAN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id -- IR-SOURCE:span )
   {: b:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   key blr l OFF-BSRC BC@ ORD-OK IR-ID:PACK-SOURCE
   blr l OFF-BBEG BC@ LEN-OK
   blr l OFF-BLEN BC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
