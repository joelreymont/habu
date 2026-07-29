\ verify.f - the independent structural verifier: the one authority that reads a
\ whole module and decides whether it may be published.
\
\ docs/compiler-ir-design.md section 5.8 (nothing is visible until every stage
\ validator passes), section 6.5 (the list of structural checks IR:FREEZE must
\ perform), and section 16.1 (every IR has a hostile fixture for each of them).
\ This file checks a module; it builds no record and owns no producer's table.
\
\ WHY A SEPARATE PACKAGE. Every producer in this substrate validates the record
\ it is appending: IR-OP:END-OP against the opcode's schema and the pool tiling,
\ IR-FUN:END-BLOCK against the terminator rule and the argument run,
\ IR-FUN:END-FUN against the linkage rules and the target contract. None of them
\ can see the whole module, and several of the section 6.5 rules are only
\ decidable once construction has stopped: a branch to a block that is still
\ being built is ordinary SSA construction, so a successor's existence cannot be
\ checked when the branch is written; a block's predecessors are unknown until
\ the last branch to it exists; and an operation appended after the last block
\ belongs to no block, which only a total count can catch. Those checks are this
\ file's, and it re-derives them from the tables rather than trusting any
\ producer's summary - which is what makes it an independent check and not a
\ second copy of the producers' own arithmetic.
\
\ WHEN IT RUNS. IR-BUILD:FREEZE calls VERIFY as its last refusal arm, before the
\ first arena is frozen. That ordering is the whole point: IR-ARENA:FREEZE
\ cannot be undone, so a verifier that ran after it would leave a builder whose
\ tables are frozen and whose slot is still live - unpublished but also no longer
\ correctable. Running first keeps FREEZE's promise that a refusal changes
\ nothing, so the caller can repair the module and freeze again. Everything here
\ therefore reads the live builder arenas, not the frozen views.
\
\ WHAT IT PUBLISHES (design lines 404-405 and 410). A block record deliberately
\ stores neither a predecessor count nor a successor count, because design line
\ 410 says those tables are derived at freeze rather than maintained through
\ every builder mutation - a stored count would be wrong for as long as any
\ branch to the block is unwritten. This file derives them and publishes them as
\ one more module table, in the pool-and-rows shape every other table uses: a
\ pool of predecessor block ordinals, and one row per block holding that block's
\ predecessor window and its successor count. The frozen module then serves them
\ through IR-BUILD's published views, so a later pass reads predecessors the same
\ way it reads any other table, from the module rather than from a pass-local
\ cache that could disagree with it.
\
\ WHY THE SUCCESSOR LIST IS NOT COPIED. A block's successors are already stored,
\ exactly once, in its terminator's successor window, and IR-OP is that window's
\ single authority. Copying the list here would create a second authority for a
\ fact the module already holds, and section 6.3's rule against duplicated
\ fields is the same rule that kept parent-block out of the operation row. What
\ is genuinely derived is the inverse relation - which blocks branch TO a block -
\ and the two counts design lines 404-405 name. Those are what this table holds:
\ the successor COUNT, because the block record does not carry it, and the
\ predecessor window, because nothing else in the module records it at all.
\
\ THE TABLES ARE HELD FOR THE LENGTH OF ONE CALL. A verifier helper would
\ otherwise have to take twelve arenas to ask one question, so VERIFY records the
\ module's tables in package-owned storage before its first check and every check
\ below reads them by name. This is the same package-owned staging discipline
\ IR-OP, IR-FUN and IR-SCHEMA already keep for their staged records, under the
\ same single-task compilation rule, and it is what lets each check read as a
\ small named word. There is no open/closed flag, because there is nothing for
\ one to protect: VERIFY overwrites all fourteen handles and the key before it
\ reads any of them, so a verification abandoned by a refusal leaves nothing a
\ later one could read, and one refused freeze cannot lock out the next. The
\ compilation context is deliberately NOT recorded: IR-CTX's rule is that no
\ context handle outlives the call it was given to, so the words that need it -
\ the target check and the one that appends the derived table - take it from
\ their caller.
\
\ DOMINANCE (design line 541). Operands cannot reach forward: IR-OP refuses an
\ operand whose ordinal is at or past the live value count, so within one block a
\ definition always precedes its use and no cycle can be built. What that rule
\ cannot see is a use in a block the definition does not dominate - block two
\ using a value block one defined, when block two is reachable without passing
\ through block one. The dominator sets are computed here by the ordinary
\ iterative dataflow the design leaves open: the entry block dominates only
\ itself, every other block starts dominated by all of its function's blocks, and
\ each round replaces a block's set with the intersection of its predecessors'
\ sets plus itself until nothing changes. The sets only ever shrink, so the
\ iteration terminates, and the result is the greatest fixpoint the definition
\ asks for. It is stated over the predecessor table this file just derived, which
\ is the only place that relation exists. A block with no predecessors is
\ unreachable and keeps the full set, which is the standard initialisation: an
\ unreachable use constrains nothing, and unreachable code is not an SSA fault.
\
\ THE COMMITTED WORKING SET. The dominator sets and the predecessor lists are
\ package-owned arrays with named ceilings, in the shape IR-OP's staged lists and
\ IR-FUN's staged attributes already use. BLOCK-MAX matches the block ceiling the
\ production plan commits to, so a module built to that plan always fits; a module
\ planned larger than the verifier's working set is refused by name with
\ E-IR-VERIFY-CAP rather than checked partially. Growing that working set with the
\ module instead of committing to a ceiling is its own capacity decision and its
\ own dot, exactly as the arena registry's own limits were.
\
\ NAMED DIAGNOSTICS. Every arm throws its own code from the -8080..-8099 block, so
\ a refusal says which invariant failed rather than which reader noticed. Where a
\ producer's reader would throw its own code first, this file makes the comparison
\ itself and throws its own: a verifier that reported E-IR-FUN-BOUND would be
\ telling the caller about the table it consulted instead of about the module.

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
require src/compiler/ir/fun.f

package IR-VERIFY
private

\ The one raw crossing this package needs: a one-way projection of the sealed
\ module key onto its serial, for the derived table's header binding.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;

\ ---- the derived block-edge table --------------------------------------------
$45445031 constant EDP-MAGIC         \ "EDP1": the predecessor pool header tag
$45445231 constant EDR-MAGIC         \ "EDR1": the edge row header tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-PST                   \ predecessor window start
1 constant OFF-PN                    \ design line 404: predecessor count
2 constant OFF-SN                    \ design line 405: successor count
3 constant EROW-CELLS

$FFFFFFFF HDR-CELLS - EROW-CELLS / constant EROW-CAP-MAX
$FFFFFFFF HDR-CELLS - constant EPOOL-CAP-MAX

\ ---- the committed working set -----------------------------------------------
256 constant BLOCK-MAX               \ matches IR-BUILD's committed block ceiling
2048 constant EDGE-MAX               \ control-flow edges one module may hold
64 constant SET-BITS                 \ dominator-set bits per cell
BLOCK-MAX SET-BITS / constant SET-CELLS
-1 constant UNSET

\ ---- the module under verification -------------------------------------------
\ One open verification per process, in the shape the producer packages already
\ keep their staged records. VERIFY records the tables here, every check below
\ reads them by name, and the slot is given back before VERIFY returns.
0 constant T-OPP                     \ operation cell pool
1 constant T-VAL                     \ value rows
2 constant T-OPR                     \ operation rows
3 constant T-FNP                     \ function attribute pool
4 constant T-FNR                     \ function rows
5 constant T-BLR                     \ block rows
6 constant T-SCP                     \ dialect schema pool
7 constant T-SCR                     \ dialect schema rows
8 constant T-SYR                     \ symbol interner rows
9 constant T-TYR                     \ type table rows
10 constant T-ATR                    \ attribute table rows
11 constant T-SRC                    \ source registry
12 constant T-EDP                    \ derived predecessor pool
13 constant T-EDR                    \ derived edge rows
14 constant TAB#

here CELL 1- and CELL swap - CELL 1- and allot
TAB# TYPED-BUFFER VT IR-ARENA:arena
1 TYPED-BUFFER VK IR-ID:ir-module-key

create PN BLOCK-MAX cells allot      \ predecessors counted per block
create PS BLOCK-MAX cells allot      \ predecessor window start per block
create PF BLOCK-MAX cells allot      \ fill cursor while the lists are written
create SN BLOCK-MAX cells allot      \ successors counted per block
create PL EDGE-MAX cells allot       \ the predecessor lists themselves
create DOM BLOCK-MAX SET-CELLS * cells allot
create TMP SET-CELLS cells allot

\ Locals are single-assignment, so the three walks that carry a running value -
\ the window tilings, the dominator rounds, and the block bisection - keep it in
\ a named cell each. Each one is written and read inside a single word, so they
\ never overlap: the bisection does not run a tiling walk and the round does not
\ run the bisection.
variable AT                          \ running ordinal while a window tiling is walked
variable LO                          \ block bisection, low bound
variable HI                          \ block bisection, high bound
variable SAME                        \ did a dominator set stay the same
variable MOVED                       \ did any set change this round

: T@ ( n -- IR-ARENA:arena )
   VT @ ;

: T! ( IR-ARENA:arena n -- )
   VT ! ;

: K@ ( -- IR-ID:ir-module-key )
   0 VK @ ;

\ ---- small typed accessors ----------------------------------------------------
: BLOCKS ( -- n )
   T-BLR T@ IR-FUN:BLOCKS ;

: FUNS ( -- n )
   T-FNR T@ IR-FUN:FUNS ;

: OPS ( -- n )
   T-OPR T@ IR-OP:OPS ;

: VALS ( -- n )
   T-VAL T@ IR-OP:VALUES ;

: BLK ( n -- IR-ID:ir-block-id )
   K@ swap IR-ID:PACK-BLOCK ;

: FUN ( n -- IR-ID:ir-fun-id )
   K@ swap IR-ID:PACK-FUN ;

: OPI ( n -- IR-ID:ir-op-id )
   K@ swap IR-ID:PACK-OP ;

: VAL ( n -- IR-ID:ir-value-id )
   K@ swap IR-ID:PACK-VALUE ;

\ A block ordinal this module really defines. Every place that reads an ordinal
\ out of a stored row passes through here, so a forged or dangling reference is
\ this file's named refusal rather than another table's bound error.
: BLK-OK ( n -- n )
   dup 0 < over BLOCKS >= or if E-IR-VERIFY-BOUND throw then ;

: OP-COUNT ( n -- n )
   BLK T-BLR T@ swap IR-FUN:OP-COUNT ;

: ARG-COUNT ( n -- n )
   BLK T-BLR T@ swap IR-FUN:ARG-COUNT ;

: BLOCK-OP ( n n -- IR-ID:ir-op-id )
   {: b:n i:n :}
   T-BLR T@ T-OPR T@ K@ b BLK i IR-FUN:OP@ ;

: TERM-OF ( n -- IR-ID:ir-op-id )
   BLK {: b:IR-ID:ir-block-id :}
   T-BLR T@ T-OPR T@ K@ b IR-FUN:TERMINATOR@ ;

: OPCODE-OF ( IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   T-OPR T@ K@ rot IR-OP:OPCODE@ ;

: SUCC-COUNT ( IR-ID:ir-op-id -- n )
   T-OPR T@ swap IR-OP:SUCCESSORS ;

: SUCC-OF ( IR-ID:ir-op-id n -- n )
   {: o:IR-ID:ir-op-id i:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:SUCCESSOR@ IR-ID:BLOCK-LOCAL ;

\ ---- capacity ----------------------------------------------------------------
: FITS-CK ( -- )
   BLOCKS BLOCK-MAX > if E-IR-VERIFY-CAP throw then ;

: EDGE-ROOM ( n -- )
   EDGE-MAX > if E-IR-VERIFY-CAP throw then ;

\ ---- coverage (the two window tilings, totalled) ------------------------------
\ IR-FUN's tiling proves each window continues where the one before it ended, so
\ the windows can only fail to cover the table at the top: an operation appended
\ after the last block belongs to no block, and a block left behind by an
\ abandoned function belongs to no function. Comparing the total against the
\ table's own count is what catches both, and it is the only check that can.
: BLOCK-COVER-CK ( -- )
   0
   FUNS 0 ?do
      T-FNR T@ i FUN IR-FUN:BLOCK-COUNT +
   loop
   BLOCKS <> if E-IR-VERIFY-COVER throw then ;

: OP-COVER-CK ( -- )
   0
   BLOCKS 0 ?do
      i OP-COUNT +
   loop
   OPS <> if E-IR-VERIFY-COVER throw then ;

\ ---- parents ------------------------------------------------------------------
\ Walking the functions in order hands out the block ordinals in exactly the
\ order the windows tile, so each block's own parent field can be compared
\ against the function whose window is claiming it without searching.
: PARENT-AT-CK ( n n -- )
   {: b:n f:n :}
   T-BLR T@ T-FNR T@ K@ b BLK IR-FUN:PARENT@ IR-ID:FUN-LOCAL
   f <> if E-IR-VERIFY-PARENT throw then ;

: PARENTS-CK ( -- )
   0 AT !
   FUNS 0 ?do
      T-FNR T@ i FUN IR-FUN:BLOCK-COUNT {: bn:n :}
      bn 0 ?do
         AT @ BLK-OK j PARENT-AT-CK
         AT @ 1+ AT !
      loop
   loop ;

\ ---- terminators --------------------------------------------------------------
: TERMINATOR? ( IR-ID:ir-op-id -- bool )
   OPCODE-OF {: s:IR-ID:ir-symbol-id :}
   T-SCR T@ s IR-SCHEMA:TERMINATOR? ;

\ Design line 403: exactly one terminator, and it is the block's last operation.
: TERM-CK ( n -- )
   {: b:n :}
   b OP-COUNT {: n:n :}
   n 1 < if E-IR-VERIFY-TERM throw then
   n 0 ?do
      b i BLOCK-OP TERMINATOR? {: t:bool :}
      i n 1- = if
         t 0= if E-IR-VERIFY-TERM throw then
      else
         t if E-IR-VERIFY-TERM throw then
      then
   loop
   b TERM-OF drop ;

: TERMS-CK ( -- )
   BLOCKS 0 ?do
      i TERM-CK
   loop ;

\ ---- value definitions (design line 537) --------------------------------------
\ Every value has exactly one definition, and the definition has to claim it
\ back. A result value names an operation and a position, so that operation's
\ result window must hold this very value there; a block argument names a block
\ and a position, so that block must exist and its argument window must hold it.
: RESULT-DEF-CK ( n -- )
   {: v:n :}
   T-VAL T@ T-OPR T@ K@ v VAL IR-OP:VALUE-OP@ {: o:IR-ID:ir-op-id :}
   T-VAL T@ v VAL IR-OP:VALUE-POS@ {: pos:n :}
   pos T-OPR T@ o IR-OP:RESULTS >= if E-IR-VERIFY-DEF throw then
   T-OPP T@ T-OPR T@ K@ o pos IR-OP:RESULT@ IR-ID:VALUE-LOCAL
   v <> if E-IR-VERIFY-DEF throw then ;

: ARG-DEF-CK ( n -- )
   {: v:n :}
   T-VAL T@ K@ v VAL IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL {: b:n :}
   b 0 < b BLOCKS >= or if E-IR-VERIFY-ARGDEF throw then
   T-VAL T@ v VAL IR-OP:VALUE-ARG@ {: pos:n :}
   pos b ARG-COUNT >= if E-IR-VERIFY-ARGDEF throw then
   T-BLR T@ T-VAL T@ K@ b BLK pos IR-FUN:ARG@ IR-ID:VALUE-LOCAL
   v <> if E-IR-VERIFY-ARGDEF throw then ;

: VALUE-CK ( n -- )
   {: v:n :}
   T-VAL T@ v VAL IR-OP:VALUE-KIND@
   IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   if v ARG-DEF-CK else v RESULT-DEF-CK then ;

: VALUES-CK ( -- )
   VALS 0 ?do
      i VALUE-CK
   loop ;

\ ---- the derived predecessor and successor tables ----------------------------
: PN@ ( n -- n )   cells PN + @ ;
: PN! ( n n -- )   cells PN + ! ;
: PS@ ( n -- n )   cells PS + @ ;
: PS! ( n n -- )   cells PS + ! ;
: PF@ ( n -- n )   cells PF + @ ;
: PF! ( n n -- )   cells PF + ! ;
: SN@ ( n -- n )   cells SN + @ ;
: SN! ( n n -- )   cells SN + ! ;
: PL@ ( n -- n )   cells PL + @ ;
: PL! ( n n -- )   cells PL + ! ;

: EDGES-RESET ( -- )
   BLOCKS 0 ?do
      0 i PN!  0 i SN!
   loop ;

\ A successor must name a block this module defined. This is the check IR-OP
\ deliberately left open, because when the branch was written the destination
\ could still have been under construction.
: SUCC-EXISTS-CK ( n -- n )
   dup 0 < over BLOCKS >= or if E-IR-VERIFY-SUCC throw then ;

: EDGES-COUNT ( -- )
   BLOCKS 0 ?do
      i TERM-OF {: o:IR-ID:ir-op-id :}
      o SUCC-COUNT {: n:n :}
      n i SN!
      n 0 ?do
         o i SUCC-OF SUCC-EXISTS-CK {: s:n :}
         s PN@ 1+ s PN!
      loop
   loop ;

: EDGES-LAYOUT ( -- )
   0 AT !
   BLOCKS 0 ?do
      AT @ i PS!
      AT @ i PF!
      AT @ i PN@ + AT !
   loop
   AT @ EDGE-ROOM ;

: EDGES-FILL ( -- )
   BLOCKS 0 ?do
      i TERM-OF {: o:IR-ID:ir-op-id :}
      o SUCC-COUNT 0 ?do
         o i SUCC-OF {: s:n :}
         j s PF@ PL!
         s PF@ 1+ s PF!
      loop
   loop ;

\ The predecessor of a successor edge is the block whose terminator carries it,
\ so the outer index is the predecessor and the inner one is the edge position.
\ EDGES-FILL reads both, which is why it re-walks rather than reusing a cursor.
: EDGES-BUILD ( -- )
   EDGES-RESET
   EDGES-COUNT
   EDGES-LAYOUT
   EDGES-FILL ;

\ ---- successor arguments (design line 536) ------------------------------------
\ A terminator's operands are the arguments it hands its successor, so with one
\ successor the count and the types must be the destination block's. With more
\ than one successor the operation model has no way to say which operand belongs
\ to which destination, so the count is all that can be stated: see MODEL GAPS in
\ the dot. The single-successor case is the one every dialect built so far uses.
: SUCCARG-TYPE-CK ( IR-ID:ir-op-id n n -- )
   {: o:IR-ID:ir-op-id s:n i:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:OPERAND@ {: v:IR-ID:ir-value-id :}
   T-BLR T@ T-VAL T@ K@ s BLK i IR-FUN:ARG@ {: a:IR-ID:ir-value-id :}
   T-VAL T@ K@ v IR-OP:VALUE-TYPE@ IR-ID:TYPE-LOCAL
   T-VAL T@ K@ a IR-OP:VALUE-TYPE@ IR-ID:TYPE-LOCAL
   <> if E-IR-VERIFY-SUCCARG throw then ;

: SUCCARGS-CK ( n -- )
   {: b:n :}
   b TERM-OF {: o:IR-ID:ir-op-id :}
   o SUCC-COUNT 1 <> if exit then
   o 0 SUCC-OF {: s:n :}
   T-OPR T@ o IR-OP:OPERANDS {: n:n :}
   n s ARG-COUNT <> if E-IR-VERIFY-SUCCARG throw then
   n 0 ?do
      o s i SUCCARG-TYPE-CK
   loop ;

: ALL-SUCCARGS-CK ( -- )
   BLOCKS 0 ?do
      i SUCCARGS-CK
   loop ;

\ ---- dominator sets -----------------------------------------------------------
: DOM-CELL ( n n -- n )
   {: b:n w:n :}
   b SET-CELLS * w + ;

: DOM@ ( n n -- n )
   DOM-CELL cells DOM + @ ;

: DOM! ( n n n -- )
   DOM-CELL cells DOM + ! ;

: TMP@ ( n -- n )   cells TMP + @ ;
: TMP! ( n n -- )   cells TMP + ! ;

: BIT-CELL ( n -- n )
   SET-BITS / ;

: BIT-MASK ( n -- n )
   SET-BITS mod 1 swap lshift ;

: DOM-HAS? ( n n -- bool )
   {: b:n d:n :}
   b d BIT-CELL DOM@ d BIT-MASK and 0<> ;

: TMP-SET ( n -- )
   {: d:n :}
   d BIT-CELL TMP@ d BIT-MASK or  d BIT-CELL TMP! ;

: TMP-CLEAR ( -- )
   SET-CELLS 0 ?do
      0 i TMP!
   loop ;

: TMP-FULL ( n n -- )
   {: bst:n bn:n :}
   TMP-CLEAR
   bn 0 ?do
      bst i + TMP-SET
   loop ;

: TMP>DOM ( n -- bool )
   {: b:n :}
   0 SAME !
   SET-CELLS 0 ?do
      i TMP@ b i DOM@ <> if 1 SAME ! then
   loop
   SAME @ 0<> if
      SET-CELLS 0 ?do
         i TMP@ b i DOM!
      loop
   then
   SAME @ 0<> ;

: TMP-AND-DOM ( n -- )
   {: p:n :}
   SET-CELLS 0 ?do
      i TMP@ p i DOM@ and i TMP!
   loop ;

: DOM-SEED ( n n -- )
   {: bst:n bn:n :}
   bst bn TMP-FULL
   bn 0 ?do
      SET-CELLS 0 ?do
         i TMP@ bst j + i DOM!
      loop
   loop
   TMP-CLEAR
   bst TMP-SET
   SET-CELLS 0 ?do
      i TMP@ bst i DOM!
   loop ;

\ One round over one function: every block but the entry takes the intersection
\ of its predecessors' sets plus itself. Sets only shrink, so the rounds below
\ terminate; the bound is the block count, since each round that changes nothing
\ is the fixpoint and each round that changes something removes at least one bit.
: DOM-ROUND ( n n -- bool )
   {: bst:n bn:n :}
   0 MOVED !
   bn 1 ?do
      bst i + {: b:n :}
      bst bn TMP-FULL
      b PN@ 0 ?do
         b PS@ i + PL@ TMP-AND-DOM
      loop
      b TMP-SET
      b TMP>DOM if 1 MOVED ! then
   loop
   MOVED @ 0<> ;

: DOM-FIX ( n n -- )
   {: bst:n bn:n :}
   bst bn DOM-SEED
   bn 1+ 0 ?do
      bst bn DOM-ROUND 0= if leave then
   loop ;

\ ---- dominance of every operand -----------------------------------------------
\ Which block defines a value: its own block for an argument, and the block whose
\ operation window holds the defining operation for a result. The windows tile
\ the operation table in block order, so the owning block is found by bisection
\ rather than by a scan, and no working-set array is needed for the map.
: OP-BLOCK ( n -- n )
   {: o:n :}
   0 LO !
   BLOCKS HI !
   begin
      HI @ LO @ 1+ >
   while
      LO @ HI @ + 2 / {: mid:n :}
      T-BLR T@ T-OPR T@ K@ mid BLK 0 IR-FUN:OP@ IR-ID:OP-LOCAL o >
      if mid HI ! else mid LO ! then
   repeat
   LO @ ;

: DEF-BLOCK ( n -- n )
   {: v:n :}
   T-VAL T@ v VAL IR-OP:VALUE-KIND@
   IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   if
      T-VAL T@ K@ v VAL IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL BLK-OK exit
   then
   T-VAL T@ T-OPR T@ K@ v VAL IR-OP:VALUE-OP@ IR-ID:OP-LOCAL OP-BLOCK ;

: SAME-FUN? ( n n -- bool )
   {: a:n b:n :}
   T-BLR T@ T-FNR T@ K@ a BLK IR-FUN:PARENT@ IR-ID:FUN-LOCAL
   T-BLR T@ T-FNR T@ K@ b BLK IR-FUN:PARENT@ IR-ID:FUN-LOCAL = ;

: OPERAND-DOM-CK ( n IR-ID:ir-op-id n -- )
   {: b:n o:IR-ID:ir-op-id i:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:OPERAND@ IR-ID:VALUE-LOCAL {: v:n :}
   v DEF-BLOCK {: d:n :}
   b d SAME-FUN? 0= if E-IR-VERIFY-SCOPE throw then
   b d DOM-HAS? 0= if E-IR-VERIFY-DOM throw then ;

: BLOCK-DOM-CK ( n -- )
   {: b:n :}
   b OP-COUNT 0 ?do
      b i BLOCK-OP {: o:IR-ID:ir-op-id :}
      T-OPR T@ o IR-OP:OPERANDS 0 ?do
         b o i OPERAND-DOM-CK
      loop
   loop ;

: DOM-CK ( -- )
   0 AT !
   FUNS 0 ?do
      T-FNR T@ i FUN IR-FUN:BLOCK-COUNT {: bn:n :}
      bn 0 > if
         AT @ bn DOM-FIX
         bn 0 ?do
            AT @ i + BLOCK-DOM-CK
         loop
      then
      AT @ bn + AT !
   loop ;

\ ---- schema type rules (design line 542) --------------------------------------
\ The schema's declared list is shorter than the operation's when the last entry
\ is a variadic tail: every further entry is described by that tail type.
: LIST-AT ( n n bool -- n )
   {: want:n i:n tail:bool :}
   tail i want 1- >= and if want 1- exit then
   i ;

: OPERAND-TYPE-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id n -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id i:n :}
   T-SCR T@ s IR-SCHEMA:OPERANDS T-SCR T@ s IR-SCHEMA:OPERAND-TAIL? {: want:n tail:bool :}
   want 0= if exit then
   T-SCP T@ T-SCR T@ K@ s  want i tail LIST-AT  IR-SCHEMA:OPERAND@
   IR-ID:TYPE-LOCAL {: declared:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:OPERAND@ {: v:IR-ID:ir-value-id :}
   T-VAL T@ K@ v IR-OP:VALUE-TYPE@ IR-ID:TYPE-LOCAL
   declared <> if E-IR-VERIFY-OPTYPE throw then ;

: RESULT-TYPE-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id n -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id i:n :}
   T-SCR T@ s IR-SCHEMA:RESULTS T-SCR T@ s IR-SCHEMA:RESULT-TAIL? {: want:n tail:bool :}
   want 0= if exit then
   T-SCP T@ T-SCR T@ K@ s  want i tail LIST-AT  IR-SCHEMA:RESULT@
   IR-ID:TYPE-LOCAL {: declared:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:RESULT@ {: v:IR-ID:ir-value-id :}
   T-VAL T@ K@ v IR-OP:VALUE-TYPE@ IR-ID:TYPE-LOCAL
   declared <> if E-IR-VERIFY-RESTYPE throw then ;

: TYPES-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id :}
   T-OPR T@ o IR-OP:OPERANDS 0 ?do
      s o i OPERAND-TYPE-CK
   loop
   T-OPR T@ o IR-OP:RESULTS 0 ?do
      s o i RESULT-TYPE-CK
   loop ;

\ ---- attribute keys (design lines 479 and 484) --------------------------------
: OP-KEY ( IR-ID:ir-op-id n -- n )
   {: o:IR-ID:ir-op-id i:n :}
   T-OPP T@ T-OPR T@ K@ o i IR-OP:ATTR-KEY@ IR-ID:SYMBOL-LOCAL ;

: SCHEMA-KEY ( IR-ID:ir-symbol-id n -- n )
   {: s:IR-ID:ir-symbol-id i:n :}
   T-SCP T@ T-SCR T@ K@ s i IR-SCHEMA:ATTR@ IR-ID:SYMBOL-LOCAL ;

: KEY-COUNT ( IR-ID:ir-op-id n -- n )
   {: o:IR-ID:ir-op-id k:n :}
   0
   T-OPR T@ o IR-OP:ATTRS 0 ?do
      o i OP-KEY k = if 1+ then
   loop ;

: DECLARED? ( IR-ID:ir-symbol-id n -- bool )
   {: s:IR-ID:ir-symbol-id k:n :}
   0 0 <>
   T-SCR T@ s IR-SCHEMA:ATTRS 0 ?do
      s i SCHEMA-KEY k = if drop 0 0= then
   loop ;

\ Every key the opcode requires appears exactly once, and no key it does not
\ declare appears at all unless the opcode opened its extension set.
: REQUIRED-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id :}
   T-SCR T@ s IR-SCHEMA:ATTRS 0 ?do
      o  s i SCHEMA-KEY  KEY-COUNT 1 <> if E-IR-VERIFY-ATTRKEY throw then
   loop ;

: EXTRA-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id :}
   T-SCR T@ s IR-SCHEMA:ATTR-EXT? if exit then
   T-OPR T@ o IR-OP:ATTRS 0 ?do
      s  o i OP-KEY  DECLARED? 0= if E-IR-VERIFY-ATTRKEY throw then
   loop ;

\ A key names a symbol, and IR-OP has no interner to ask whether it exists.
: KEY-SYMBOL-CK ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   T-OPR T@ o IR-OP:ATTRS 0 ?do
      o i OP-KEY {: k:n :}
      k 0 < k T-SYR T@ IR-SYM:SYMBOLS >= or if E-IR-VERIFY-SYMBOL throw then
   loop ;

: ATTRS-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id :}
   o KEY-SYMBOL-CK
   s o REQUIRED-CK
   s o EXTRA-CK ;

\ ---- effect, span, and target -------------------------------------------------
\ Design line 543: an operation that carries an effect token has to have one.
\ The effect class is the schema's field, and this is the cross-check IR-SCHEMA
\ cannot make on its own: an opcode that reads or writes a memory domain must
\ declare a memory-token operand or result to carry that domain.
: TOKEN-TYPE? ( IR-ID:ir-type-id -- bool )
   T-TYR T@ swap IR-TYPE:KIND@
   IR--TYPE-KIND:MEMORY-TOKEN IR--TYPE-KIND:EQ ;

: SCHEMA-TOKEN? ( IR-ID:ir-symbol-id -- bool )
   {: s:IR-ID:ir-symbol-id :}
   0 0 <>
   T-SCR T@ s IR-SCHEMA:OPERANDS 0 ?do
      T-SCP T@ T-SCR T@ K@ s i IR-SCHEMA:OPERAND@ TOKEN-TYPE? if drop 0 0= then
   loop
   T-SCR T@ s IR-SCHEMA:RESULTS 0 ?do
      T-SCP T@ T-SCR T@ K@ s i IR-SCHEMA:RESULT@ TOKEN-TYPE? if drop 0 0= then
   loop ;

: EFFECT-CK ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   T-SCR T@ s IR-SCHEMA:EFFECT@
   IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ if exit then
   s SCHEMA-TOKEN? 0= if E-IR-VERIFY-EFFECT throw then ;

\ Design line 545: a span is a valid slice of a source this module registered.
\ A span is a three-field value, so it is unmade to reach the fields the way
\ every other holder of one does.
: SPAN-CK ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src IR-ID:SOURCE-LOCAL {: ord:n :}
   ord 0 < ord T-SRC T@ IR-SOURCE:SOURCES >= or if E-IR-VERIFY-SPAN throw then
   st 0 < ln 0 < or if E-IR-VERIFY-SPAN throw then
   st ln + T-SRC T@ src IR-SOURCE:LEN@ > if E-IR-VERIFY-SPAN throw then ;

: TARGET-CK ( IR-CTX:ctx IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx s:IR-ID:ir-symbol-id :}
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@
   T-SCR T@ s IR-SCHEMA:ARCH@ CTARGET-ARCH:EQ
   0= if E-IR-VERIFY-TARGET throw then ;

\ ---- one operation ------------------------------------------------------------
: OPCODE-CK ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   s IR-ID:SYMBOL-LOCAL {: ord:n :}
   ord 0 < ord T-SYR T@ IR-SYM:SYMBOLS >= or if E-IR-VERIFY-SYMBOL throw then
   T-SCR T@ s IR-SCHEMA:DEFINED? 0= if E-IR-VERIFY-SYMBOL throw then ;

: OP-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l OPI {: o:IR-ID:ir-op-id :}
   o OPCODE-OF {: s:IR-ID:ir-symbol-id :}
   s OPCODE-CK
   s o TYPES-CK
   s o ATTRS-CK
   s EFFECT-CK
   c s TARGET-CK
   T-OPR T@ K@ o IR-OP:SPAN@ SPAN-CK ;

: OPS-CK ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   OPS 0 ?do
      c i OP-CK
   loop ;

\ ---- functions ----------------------------------------------------------------
\ Section 6.5's symbol rules, restated over the finished table: a function names
\ a symbol this module interned, and it names it alone.
: FUN-SYMBOL-CK ( n -- n )
   {: f:n :}
   T-FNR T@ K@ f FUN IR-FUN:SYMBOL@ IR-ID:SYMBOL-LOCAL {: ord:n :}
   ord 0 < ord T-SYR T@ IR-SYM:SYMBOLS >= or if E-IR-VERIFY-SYMBOL throw then
   ord ;

: FUN-UNIQUE-CK ( n n -- )
   {: f:n ord:n :}
   f 0 ?do
      i FUN-SYMBOL-CK ord = if E-IR-VERIFY-SYMBOL throw then
   loop ;

: FUNS-CK ( -- )
   FUNS 0 ?do
      i FUN-SYMBOL-CK {: ord:n :}
      i ord FUN-UNIQUE-CK
      T-FNR T@ K@ i FUN IR-FUN:SPAN@ SPAN-CK
   loop ;

: BLOCK-SPANS-CK ( -- )
   BLOCKS 0 ?do
      T-BLR T@ K@ i BLK IR-FUN:BLOCK-SPAN@ SPAN-CK
   loop ;

\ ---- publishing the derived table ---------------------------------------------
: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: ROOM-CK ( -- )
   T-EDR T@ HC-CAP LCELL@ BLOCKS < if E-IR-VERIFY-CAP throw then
   T-EDP T@ HC-CAP LCELL@  0 BLOCKS 0 ?do i PN@ + loop  < if E-IR-VERIFY-CAP throw then ;

: EDGES-PUBLISH ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   ROOM-CK
   BLOCKS 0 ?do
      i PN@ 0 ?do
         c T-EDP T@  j PS@ i + PL@  CELL+
      loop
   loop
   BLOCKS 0 ?do
      c T-EDR T@ i PS@ CELL+
      c T-EDR T@ i PN@ CELL+
      c T-EDR T@ i SN@ CELL+
   loop ;

public

\ ---- creation -----------------------------------------------------------------
\ The derived table's two arenas, created with the module's other tables so the
\ verifier allocates nothing at freeze time. The ceilings are the module's own
\ block ceiling and an upper bound on its control-flow edges.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n :}
   rcap 1 < rcap EROW-CAP-MAX > or if E-IR-VERIFY-CAP throw then
   pcap 0 < pcap EPOOL-CAP-MAX > or if E-IR-VERIFY-CAP throw then
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p EDP-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c rcap EROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r EDR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r rcap CELL+
   p r ;

private

: TABLES-TAKE ( IR-ID:ir-module-key -- )
   0 VK ! ;

public

\ Verify one module whole and derive its predecessor and successor tables. Every
\ check runs against the live builder arenas, before IR-BUILD freezes any of
\ them, so a refusal leaves the module exactly as it was. On success the derived
\ table holds one row per block and IR-BUILD publishes it with the rest.
: VERIFY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key opp:IR-ARENA:arena val:IR-ARENA:arena opr:IR-ARENA:arena fnp:IR-ARENA:arena fnr:IR-ARENA:arena blr:IR-ARENA:arena scp:IR-ARENA:arena scr:IR-ARENA:arena syr:IR-ARENA:arena tyr:IR-ARENA:arena atr:IR-ARENA:arena src:IR-ARENA:arena edp:IR-ARENA:arena edr:IR-ARENA:arena :}
   key TABLES-TAKE
   opp T-OPP T!   val T-VAL T!   opr T-OPR T!
   fnp T-FNP T!   fnr T-FNR T!   blr T-BLR T!
   scp T-SCP T!   scr T-SCR T!   syr T-SYR T!
   tyr T-TYR T!   atr T-ATR T!   src T-SRC T!
   edp T-EDP T!   edr T-EDR T!
   FITS-CK
   BLOCK-COVER-CK
   OP-COVER-CK
   PARENTS-CK
   TERMS-CK
   VALUES-CK
   FUNS-CK
   BLOCK-SPANS-CK
   c OPS-CK
   EDGES-BUILD
   ALL-SUCCARGS-CK
   DOM-CK
   c EDGES-PUBLISH ;

private

\ ---- frozen readers -----------------------------------------------------------
: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

: FPSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-VERIFY-STATE throw then ;

: FRSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-VERIFY-STATE throw then
   HDR-CELLS - EROW-CELLS mod 0 <> if E-IR-VERIFY-STATE throw then ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: p:IR-ARENA:view :}
   p IR-ARENA:SIZE FPSHAPE-CK
   EDP-MAGIC p HC-MAGIC FCELL@ <> if E-IR-VERIFY-STATE throw then ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: r:IR-ARENA:view :}
   r IR-ARENA:SIZE FRSHAPE-CK
   EDR-MAGIC r HC-MAGIC FCELL@ <> if E-IR-VERIFY-STATE throw then ;

: FRKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: r:IR-ARENA:view key:IR-ID:ir-module-key :}
   r FRHDR-CK
   r HC-SERIAL FCELL@ key KEY-SERIAL <> if E-IR-VERIFY-OWNER throw then ;

: FCNT ( IR-ARENA:view -- n )
   dup FRHDR-CK IR-ARENA:SIZE HDR-CELLS - EROW-CELLS / ;

: FROW-AT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   {: r:IR-ARENA:view id:IR-ID:ir-block-id :}
   id IR-ID:BLOCK-LOCAL {: l:n :}
   l 0 < l r FCNT >= or if E-IR-VERIFY-BOUND throw then
   l ;

: FRC@ ( IR-ARENA:view n n -- n )
   {: r:IR-ARENA:view l:n off:n :}
   r  l EROW-CELLS * HDR-CELLS + off +  FCELL@ ;

: FFLD ( IR-ARENA:view IR-ID:ir-block-id n -- n )
   {: r:IR-ARENA:view id:IR-ID:ir-block-id off:n :}
   r  r id FROW-AT  off FRC@ ;

public

\ ---- the published derived table ----------------------------------------------
: FEDGE-BLOCKS ( IR-ARENA:view -- n )
   FCNT ;

\ Design line 404: how many blocks branch to this one.
: FPRED-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   OFF-PN FFLD ;

\ Design line 405: how many blocks this one branches to.
: FSUCC-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   OFF-SN FFLD ;

: FPRED@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-block-id )
   {: p:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   p FPHDR-CK
   r key FRKEY-CK
   r id FROW-AT {: l:n :}
   r l OFF-PN FRC@ {: n:n :}
   i 0 < i n >= or if E-IR-VERIFY-BOUND throw then
   p  r l OFF-PST FRC@ i + HDR-CELLS +  FCELL@ {: ord:n :}
   ord 0 < if E-IR-VERIFY-STATE throw then
   key ord IR-ID:PACK-BLOCK ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
