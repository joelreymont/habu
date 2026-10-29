\ verify.f - the independent whole-module verifier, run by IR-BUILD:FREEZE as its
\ last refusal arm before any arena is frozen (IR-ARENA:FREEZE cannot be undone),
\ so it reads the live builder arenas. It re-derives every rule from the tables,
\ trusting no producer's summary, and covers what producers cannot see: a
\ successor's existence, predecessors, operations after the last block,
\ dominance across blocks.

require src/lib/prelude.f
require src/lib/errors.f
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

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )

\ ---- the derived block-edge table --------------------------------------------
\ The one module table this file derives and publishes: a pool of predecessor
\ block ordinals and one row per block (predecessor window, successor count). The
\ successor list itself stays in the terminator's window, IR-OP's.
$45445031 constant EDP-MAGIC         \ "EDP1": the predecessor pool header tag
$45445231 constant EDR-MAGIC         \ "EDR1": the edge row header tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-PST                   \ predecessor window start
1 constant OFF-PN                    \ predecessor count
2 constant OFF-SN                    \ successor count
3 constant EROW-CELLS

$FFFFFFFF HDR-CELLS - EROW-CELLS / constant EROW-CAP-MAX
$FFFFFFFF HDR-CELLS - constant EPOOL-CAP-MAX

\ ---- the reusable working set ------------------------------------------------
64 constant SET-BITS                 \ dominator-set bits per cell

\ ---- the module under verification -------------------------------------------
\ One open verification per process; the slot is given back before VERIFY
\ returns. VERIFY records the fourteen tables in package storage for one call,
\ overwriting them all first, so no open flag is needed; the context is never
\ stored.
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

align
TAB# CWORK-BUFFER VT IR-ARENA:arena
1 CWORK-BUFFER VK IR-ID:ir-module-key

DYNAMIC-BUFFER WORK n
0 CWORK-VARIABLE BLOCK-N n
0 CWORK-VARIABLE EDGE-N n
0 CWORK-VARIABLE SET-N n
0 CWORK-VARIABLE PL-OFF n
0 CWORK-VARIABLE DOM-OFF n
0 CWORK-VARIABLE TMP-OFF n

\ Locals are single-assignment, so the three walks with a running value keep it
\ in a cell; each is used inside one word and none overlaps another.
0 CWORK-VARIABLE AT n                          \ running ordinal while a window tiling is walked
0 CWORK-VARIABLE LO n                          \ block bisection, low bound
0 CWORK-VARIABLE HI n                          \ block bisection, high bound
0 CWORK-VARIABLE SAME n                        \ did a dominator set stay the same
0 CWORK-VARIABLE MOVED n                       \ did any set change this round

: T@ ( n -- IR-ARENA:arena )
   VT @ ;

: T! ( IR-ARENA:arena n -- )
   VT ! ;

: K@ ( -- IR-ID:ir-module-key )
   0 VK @ ;

\ ---- small typed accessors ---------------------------------------------------
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

\ Every stored block ordinal passes here, so a forged reference is this file's
\ refusal rather than another table's bound error.
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

\ ---- scratch sizing ----------------------------------------------------------
\ Sizes and offsets count cells; an extent is refused before cells overflow.
: WORK+ ( n n -- n )
   {: a:n b:n :}
   a MEM-MAX-CELLS b - > if E-IR-VERIFY-CAP throw then
   a b + ;

: WORK* ( n n -- n )
   {: a:n b:n :}
   b 0= if 0 exit then
   a MEM-MAX-CELLS b / > if E-IR-VERIFY-CAP throw then
   a b * ;

: WORK-CLEAR ( -- )
   0 BLOCK-N ! 0 EDGE-N ! 0 SET-N !
   0 PL-OFF ! 0 DOM-OFF ! 0 TMP-OFF ! ;

: WORK-BEGIN ( -- )
   BLOCKS BLOCK-N !
   BLOCK-N @ SET-BITS / BLOCK-N @ SET-BITS mod 0<> if 1+ then SET-N !
   BLOCK-N @ 4 WORK* dup PL-OFF ! WORK-RESERVE ;

\ RESERVE preserves the counts already initialized; no pointer is retained
\ across growth.
: WORK-GROW ( bool -- )
   {: dominance:bool :}
   PL-OFF @ EDGE-N @ WORK+ dup DOM-OFF !
   dominance if
      BLOCK-N @ SET-N @ WORK* WORK+ dup TMP-OFF !
      SET-N @ WORK+
   then
   WORK-RESERVE ;

\ ---- coverage (the two window tilings, totalled) -----------------------------
\ IR-FUN's tiling leaves only the top uncovered: an operation after the last
\ block, or a block of an abandoned function. Only the total catches both.
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

\ ---- parents -----------------------------------------------------------------
\ Walking functions in order hands out block ordinals in window order, so no
\ search is needed.
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

\ ---- terminators -------------------------------------------------------------
: TERMINATOR? ( IR-ID:ir-op-id -- bool )
   OPCODE-OF {: s:IR-ID:ir-symbol-id :}
   T-SCR T@ s IR-SCHEMA:TERMINATOR? ;

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

\ ---- value definitions -------------------------------------------------------
\ The definition has to claim each value back: a result's operation holds it in
\ its result window at its position; a block argument's block holds it likewise.
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
: PN@ ( n -- n )   WORK @ ;
: PN! ( n n -- )   WORK ! ;
: PS@ ( n -- n )   BLOCK-N @ + WORK @ ;
: PS! ( n n -- )   BLOCK-N @ + WORK ! ;
: PF@ ( n -- n )   BLOCK-N @ 2 * + WORK @ ;
: PF! ( n n -- )   BLOCK-N @ 2 * + WORK ! ;
: SN@ ( n -- n )   BLOCK-N @ 3 * + WORK @ ;
: SN! ( n n -- )   BLOCK-N @ 3 * + WORK ! ;
: PL@ ( n -- n )   PL-OFF @ + WORK @ ;
: PL! ( n n -- )   PL-OFF @ + WORK ! ;

: EDGES-RESET ( -- )
   BLOCKS 0 ?do
      0 i PN!  0 i SN!
   loop ;

\ The check IR-OP left open: the destination could still have been under
\ construction when the branch was written.
: SUCC-EXISTS-CK ( n -- n )
   dup 0 < over BLOCKS >= or if E-IR-VERIFY-SUCC throw then ;

: SAME-FUN? ( n n -- bool )
   {: a:n b:n :}
   T-BLR T@ T-FNR T@ K@ a BLK IR-FUN:PARENT@ IR-ID:FUN-LOCAL
   T-BLR T@ T-FNR T@ K@ b BLK IR-FUN:PARENT@ IR-ID:FUN-LOCAL = ;

: EDGES-COUNT ( -- )
   BLOCKS 0 ?do
      i TERM-OF {: o:IR-ID:ir-op-id :}
      o SUCC-COUNT {: n:n :}
      n i SN!
      n 0 ?do
         o i SUCC-OF SUCC-EXISTS-CK {: s:n :}
         j s SAME-FUN? 0= if E-IR-VERIFY-SCOPE throw then
         s PN@ 1+ s PN!
      loop
   loop ;

: EDGES-LAYOUT ( -- )
   0 AT !
   BLOCKS 0 ?do
      AT @ i PS!
      AT @ i PF!
      AT @ i PN@ WORK+ AT !
   loop
   AT @ EDGE-N ! ;

: EDGES-FILL ( -- )
   BLOCKS 0 ?do
      i TERM-OF {: o:IR-ID:ir-op-id :}
      o SUCC-COUNT 0 ?do
         o i SUCC-OF {: s:n :}
         j s PF@ PL!
         s PF@ 1+ s PF!
      loop
   loop ;

\ EDGES-FILL reads both indices, so it re-walks rather than reusing a cursor.
: EDGES-BUILD ( bool -- )
   EDGES-RESET
   EDGES-COUNT
   EDGES-LAYOUT
   WORK-GROW
   EDGES-FILL ;

\ ---- successor arguments -----------------------------------------------------
\ With one successor, the terminator's operands must match the destination's
\ arguments in count and type. With more there are no per-edge windows, so such
\ destinations must take no arguments.
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
   o SUCC-COUNT {: successors:n :}
   successors 1 <> if
      successors 0 ?do
         o i SUCC-OF ARG-COUNT 0<> if E-IR-VERIFY-SUCCARG throw then
      loop
      exit
   then
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

\ ---- dominator sets ----------------------------------------------------------
: DOM-CELL ( n n -- n )
   {: b:n w:n :}
   b SET-N @ * w + DOM-OFF @ + ;

: DOM@ ( n n -- n )
   DOM-CELL WORK @ ;

: DOM! ( n n n -- )
   DOM-CELL WORK ! ;

: TMP@ ( n -- n )   TMP-OFF @ + WORK @ ;
: TMP! ( n n -- )   TMP-OFF @ + WORK ! ;

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
   SET-N @ 0 ?do
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
   SET-N @ 0 ?do
      i TMP@ b i DOM@ <> if 1 SAME ! then
   loop
   SAME @ 0<> if
      SET-N @ 0 ?do
         i TMP@ b i DOM!
      loop
   then
   SAME @ 0<> ;

: TMP-AND-DOM ( n -- )
   {: p:n :}
   SET-N @ 0 ?do
      i TMP@ p i DOM@ and i TMP!
   loop ;

: DOM-SEED ( n n -- )
   {: bst:n bn:n :}
   bst bn TMP-FULL
   bn 0 ?do
      SET-N @ 0 ?do
         i TMP@ bst j + i DOM!
      loop
   loop
   TMP-CLEAR
   bst TMP-SET
   SET-N @ 0 ?do
      i TMP@ bst i DOM!
   loop ;

\ Iterative dataflow: the entry dominates itself, other blocks start with all
\ blocks of the function. Sets only shrink, so the rounds terminate at the
\ greatest fixpoint; an unreachable block keeps the full set and constrains
\ nothing.
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

\ ---- dominance of every operand ----------------------------------------------
\ The windows tile the operation table in block order, so a result's block is
\ found by bisection with no working-set map.
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

\ ---- schema type rules -------------------------------------------------------
: LIST-AT ( n n bool -- n )
   {: want:n i:n tail:bool :}
   tail i want 1- >= and if want 1- exit then
   i ;

\ The operands of a one-successor terminator are the destination's arguments,
\ which SUCCARGS-CK checks position by position; a schema cannot know them, so
\ their declared schema type is not consulted. A return or a two-way branch is
\ checked against its schema.
: SUCCARG-OPERANDS? ( IR-ID:ir-op-id -- bool )
   SUCC-COUNT 1 = ;

: OPERAND-TYPE-CK ( IR-ID:ir-symbol-id IR-ID:ir-op-id n -- )
   {: s:IR-ID:ir-symbol-id o:IR-ID:ir-op-id i:n :}
   o SUCCARG-OPERANDS? if exit then
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

\ ---- attribute keys ----------------------------------------------------------
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

\ Unless the opcode opened its extension set, no undeclared key may appear.
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

\ IR-OP has no interner to check a key's symbol with.
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

\ ---- effect, span, and target ------------------------------------------------
\ The cross-check IR-SCHEMA cannot make: an opcode reading or writing a memory
\ domain must declare a memory-token operand or result for it.
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

\ ---- one operation -----------------------------------------------------------
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

\ ---- functions ---------------------------------------------------------------
\ A function names a symbol this module interned, and no other function names it.
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

\ ---- publishing the derived table --------------------------------------------
: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

\ The ceiling check and the reservation use the same live total.
: EDGE-CELLS ( -- n )
   EDGE-N @ ;

: ROW-ROOM-CK ( IR-ARENA:reader -- )
   HC-CAP IR-ARENA:RD@ BLOCKS < if E-IR-VERIFY-CAP throw then ;

: POOL-ROOM-CK ( IR-ARENA:reader -- )
   HC-CAP IR-ARENA:RD@ EDGE-CELLS < if E-IR-VERIFY-CAP throw then ;

\ Both arenas are reserved before the first append, after the room check so the
\ verifier's capacity error comes first: a half-published table makes the
\ frozen module unreadable.
: ROOM-TAKE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c T-EDP T@ EDGE-CELLS IR-ARENA:RESERVE
   c T-EDR T@ BLOCKS EROW-CELLS * IR-ARENA:RESERVE ;

: EDGES-PUBLISH ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   T-EDR T@ IR-ARENA:OPEN-LIVE ROW-ROOM-CK
   T-EDP T@ IR-ARENA:OPEN-LIVE POOL-ROOM-CK
   c ROOM-TAKE
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

\ ---- creation ----------------------------------------------------------------
\ Sized from the module's block ceiling and an edge bound; the private scratch
\ slab grows independently.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n :}
   rcap 1 < rcap EROW-CAP-MAX > or if E-IR-VERIFY-CAP throw then
   pcap 0 < pcap EPOOL-CAP-MAX > or if E-IR-VERIFY-CAP throw then
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p HDR-CELLS IR-ARENA:RESERVE
   c p EDP-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c rcap EROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r EDR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r rcap CELL+
   p r ;

private

: TABLES-TAKE ( IR-ID:ir-module-key -- )
   0 VK ! ;

\ Both entry points record the same fourteen tables in the same order.
: TABLES! ( IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: key:IR-ID:ir-module-key opp:IR-ARENA:arena val:IR-ARENA:arena opr:IR-ARENA:arena fnp:IR-ARENA:arena fnr:IR-ARENA:arena blr:IR-ARENA:arena scp:IR-ARENA:arena scr:IR-ARENA:arena syr:IR-ARENA:arena tyr:IR-ARENA:arena atr:IR-ARENA:arena src:IR-ARENA:arena edp:IR-ARENA:arena edr:IR-ARENA:arena :}
   key TABLES-TAKE
   opp T-OPP T!   val T-VAL T!   opr T-OPR T!
   fnp T-FNP T!   fnr T-FNR T!   blr T-BLR T!
   scp T-SCP T!   scr T-SCR T!   syr T-SYR T!
   tyr T-TYR T!   atr T-ATR T!   src T-SRC T!
   edp T-EDP T!   edr T-EDR T! ;

: VERIFY-MODULE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   WORK-BEGIN
   BLOCK-COVER-CK
   OP-COVER-CK
   PARENTS-CK
   TERMS-CK
   VALUES-CK
   FUNS-CK
   BLOCK-SPANS-CK
   c OPS-CK
   true EDGES-BUILD
   ALL-SUCCARGS-CK
   DOM-CK
   c EDGES-PUBLISH ;

: DERIVE-MODULE ( IR-CTX:ctx -- )
   WORK-BEGIN
   false EDGES-BUILD
   EDGES-PUBLISH ;

public

\ A refusal leaves the module exactly as it was. Every arm throws its own code
\ from -8080..-8099, comparing itself where a producer's reader would throw that
\ producer's code first.
: VERIFY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key opp:IR-ARENA:arena val:IR-ARENA:arena opr:IR-ARENA:arena fnp:IR-ARENA:arena fnr:IR-ARENA:arena blr:IR-ARENA:arena scp:IR-ARENA:arena scr:IR-ARENA:arena syr:IR-ARENA:arena tyr:IR-ARENA:arena atr:IR-ARENA:arena src:IR-ARENA:arena edp:IR-ARENA:arena edr:IR-ARENA:arena :}
   key opp val opr fnp fnr blr scp scr syr tyr atr src edp edr TABLES!
   c [: VERIFY-MODULE ;] [: WORK-CLEAR ;] finally ;

\ For IR-BUILD:FREEZE-INTERIM. The derivation is not optional: A64SEL reads
\ predecessors off the module it lowers. A successor naming an undefined or
\ foreign-function block (EDGES-COUNT), pool overflow (EDGES-PUBLISH) and a
\ misplaced terminator (IR-FUN:TERMINATOR@) still refuse.
: DERIVE-EDGES ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key opp:IR-ARENA:arena val:IR-ARENA:arena opr:IR-ARENA:arena fnp:IR-ARENA:arena fnr:IR-ARENA:arena blr:IR-ARENA:arena scp:IR-ARENA:arena scr:IR-ARENA:arena syr:IR-ARENA:arena tyr:IR-ARENA:arena atr:IR-ARENA:arena src:IR-ARENA:arena edp:IR-ARENA:arena edr:IR-ARENA:arena :}
   key opp val opr fnp fnr blr scp scr syr tyr atr src edp edr TABLES!
   c [: DERIVE-MODULE ;] [: WORK-CLEAR ;] finally ;

private

\ ---- frozen readers ----------------------------------------------------------
: FPSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-VERIFY-STATE throw then ;

: FRSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-VERIFY-STATE throw then
   HDR-CELLS - EROW-CELLS mod 0 <> if E-IR-VERIFY-STATE throw then ;

: FPHDR-CK ( IR-ARENA:reader -- )
   {: p:IR-ARENA:reader :}
   p IR-ARENA:RD-SIZE FPSHAPE-CK
   EDP-MAGIC p HC-MAGIC IR-ARENA:RD@ <> if E-IR-VERIFY-STATE throw then ;

: FRHDR-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r IR-ARENA:RD-SIZE FRSHAPE-CK
   EDR-MAGIC r HC-MAGIC IR-ARENA:RD@ <> if E-IR-VERIFY-STATE throw then ;

: FRKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key :}
   r FRHDR-CK
   r HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL <> if E-IR-VERIFY-OWNER throw then ;

: FCNT ( IR-ARENA:reader -- n )
   dup FRHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - EROW-CELLS / ;

: FROW-AT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: r:IR-ARENA:reader id:IR-ID:ir-block-id :}
   id IR-ID:BLOCK-LOCAL {: l:n :}
   l 0 < l r FCNT >= or if E-IR-VERIFY-BOUND throw then
   l ;

: FRC@ ( IR-ARENA:reader n n -- n )
   {: r:IR-ARENA:reader l:n off:n :}
   r  l EROW-CELLS * HDR-CELLS + off +  IR-ARENA:RD@ ;

: FFLD ( IR-ARENA:reader IR-ID:ir-block-id n -- n )
   {: r:IR-ARENA:reader id:IR-ID:ir-block-id off:n :}
   r  r id FROW-AT  off FRC@ ;

public

\ ---- the published derived table ---------------------------------------------
: RPRED-COUNT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: rview:IR-ARENA:reader id:IR-ID:ir-block-id :}
   rview IR-ARENA:FROZEN-READER id OFF-PN FFLD ;

private

: PRED-AT ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-block-id )
   {: p:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   r key FRKEY-CK
   r id FROW-AT {: l:n :}
   r l OFF-PN FRC@ {: n:n :}
   i 0 < i n >= or if E-IR-VERIFY-BOUND throw then
   p  r l OFF-PST FRC@ i + HDR-CELLS +  IR-ARENA:RD@ {: ord:n :}
   ord 0 < if E-IR-VERIFY-STATE throw then
   key ord IR-ID:PACK-BLOCK ;

public

: RPRED@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-block-id )
   {: p:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   p IR-ARENA:FROZEN-READER FPHDR-CK
   p r IR-ARENA:FROZEN-READER key id i PRED-AT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
