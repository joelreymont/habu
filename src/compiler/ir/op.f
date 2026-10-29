\ op.f - the append-only operation and value store: one row per operation, one
\ row per SSA value, and a cell pool of each operation's operand, result,
\ successor and attribute windows. Operations and values share a package because
\ each validates against the other's live count. An operation names its opcode
\ by the module's interned symbol, resolved in the dialect's schema table;
\ parent block (IR-FUN) and effect class (the schema) are not stored here.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f

package IR-OP
public

\ The wire codes below persist it.
ENUM def-kind DERIVE eq
   op-result
   blk-arg
;ENUM

private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$4F504C31 constant OPL-MAGIC         \ "OPL1": the cell-pool header format tag
$4F505631 constant OPV-MAGIC         \ "OPV1": the value-table header format tag
$4F505231 constant OPR-MAGIC         \ "OPR1": the operation-table header format tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-OPC                   \ the opcode name symbol
1 constant OFF-OPST                  \ the operand window
2 constant OFF-OPN
3 constant OFF-RSST                  \ the result window
4 constant OFF-RSN
5 constant OFF-SCST                  \ the successor window
6 constant OFF-SCN
7 constant OFF-ATST                  \ the attribute window
8 constant OFF-ATN                   \ in pool cells, so the window tiling is
                                     \ arithmetic on cells like the other three
9 constant OFF-SRC                   \ the source span
10 constant OFF-SBEG
11 constant OFF-SLEN
12 constant ROW-CELLS

\ The window length is in cells, not entries, so the tiling ignores the stride;
\ an odd stored length is a forged row.
0 constant AT-KEY                    \ the key symbol ordinal
1 constant AT-VAL                    \ the attribute ordinal
2 constant AT-CELLS

0 constant OFF-VTYP                  \ the value's type
1 constant OFF-VKIND                 \ block argument or result
2 constant OFF-VDEF                  \ what defined it
3 constant OFF-VPOS                  \ where in that definition
4 constant VROW-CELLS

public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant ROW-CAP-MAX
$FFFFFFFF HDR-CELLS - VROW-CELLS / constant VAL-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX
private
64 constant ARITY-MAX                \ committed staged entries per list

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump.
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
\ Each store is resolved once at the public word into a reader, which refuses
\ the wrong live/frozen state itself.

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

: PHDR-CK ( IR-ARENA:reader -- )
   {: p:IR-ARENA:reader :}
   p IR-ARENA:RD-SIZE PSHAPE-CK
   OPL-MAGIC p HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: VHDR-CK ( IR-ARENA:reader -- )
   {: v:IR-ARENA:reader :}
   v IR-ARENA:RD-SIZE VSHAPE-CK
   OPV-MAGIC v HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: RHDR-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r IR-ARENA:RD-SIZE RSHAPE-CK
   OPR-MAGIC r HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: CNT ( IR-ARENA:reader -- n )
   dup RHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - ROW-CELLS / ;

: VCNT ( IR-ARENA:reader -- n )
   dup VHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - VROW-CELLS / ;

: PCELLS ( IR-ARENA:reader -- n )
   dup PHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-OP-OWNER throw then ;

\ A cross-module trio rejects before a row window is trusted against the pool.
: TRIO-CK ( IR-ARENA:reader IR-ARENA:reader IR-ARENA:reader -- )
   {: p:IR-ARENA:reader v:IR-ARENA:reader r:IR-ARENA:reader :}
   p PHDR-CK
   v VHDR-CK
   r RHDR-CK
   p HC-SERIAL IR-ARENA:RD@ r HC-SERIAL IR-ARENA:RD@ SERIAL-CK
   v HC-SERIAL IR-ARENA:RD@ r HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: RKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: VKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: v:IR-ARENA:reader key:IR-ID:ir-module-key :}
   v VHDR-CK
   v HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

\ A window reader holds only the pool and row table, so it checks their tags,
\ that both share an owner serial, and that it is the presented key's.
: PR-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: p:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key :}
   p PHDR-CK
   r key RKEY-CK
   p HC-SERIAL IR-ARENA:RD@ r HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

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

\ Non-negative only: the owning table revalidates the id it is handed.
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

: ROW-AT ( IR-ARENA:reader IR-ID:ir-op-id -- n )
   {: r:IR-ARENA:reader id:IR-ID:ir-op-id :}
   r HC-SERIAL IR-ARENA:RD@ r CNT id ROW-ORD ;

: VAL-AT ( IR-ARENA:reader IR-ID:ir-value-id -- n )
   {: v:IR-ARENA:reader id:IR-ID:ir-value-id :}
   v HC-SERIAL IR-ARENA:RD@ v VCNT id VAL-ORD ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: VROW-CELL ( n n -- n )
   swap VROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

: VC@ ( IR-ARENA:reader n n -- n )
   VROW-CELL IR-ARENA:RD@ ;

: PC@ ( IR-ARENA:reader n -- n )
   HDR-CELLS + IR-ARENA:RD@ ;

\ ---- the window tiling -------------------------------------------------------
\ The four windows tile the pool in order (operands, results, successors,
\ attributes) with lengths in pool cells, so non-overlap is a constant check on
\ one row against the row before it (TILE-CK).
: LEN-OK ( n -- n )
   dup 0 < if E-IR-OP-STATE throw then ;

: STEP-CK ( n n -- )
   <> if E-IR-OP-WINDOW throw then ;

: ROW-END ( IR-ARENA:reader n -- n )
   {: r:IR-ARENA:reader l:n :}
   r l OFF-ATST RC@ LEN-OK  r l OFF-ATN RC@ LEN-OK + ;

: TILE-CK ( IR-ARENA:reader IR-ARENA:reader n -- )
   {: p:IR-ARENA:reader r:IR-ARENA:reader l:n :}
   l 0= if 0 else r l 1- ROW-END then {: at:n :}
   at r l OFF-OPST RC@ STEP-CK
   at r l OFF-OPN RC@ LEN-OK + {: a1:n :}
   a1 r l OFF-RSST RC@ STEP-CK
   a1 r l OFF-RSN RC@ LEN-OK + {: a2:n :}
   a2 r l OFF-SCST RC@ STEP-CK
   a2 r l OFF-SCN RC@ LEN-OK + {: a3:n :}
   a3 r l OFF-ATST RC@ STEP-CK
   a3 r l OFF-ATN RC@ LEN-OK + p PCELLS > if E-IR-OP-STATE throw then ;

\ Frozen contents cannot change through the arena API, so one validated row's
\ windows are reused, keyed by both reader tokens and the operation owner. No
\ mapping pointer is retained; every hit revalidates both readers, so retirement,
\ teardown and slot reuse still reject. Does not detect memory corruption.
2 TYPED-BUFFER FROW-READERS IR-ARENA:reader
8 TYPED-BUFFER FROW-FIELDS n
1 TYPED-BUFFER FROW-READY bool
false 0 FROW-READY !
variable FROW-KEY
variable FROW-LOCAL

: FROW-FIELD ( n -- n )
   OFF-OPST - FROW-FIELDS @ ;

: FTILE-CK ( IR-ARENA:reader IR-ARENA:reader n -- )
   {: p:IR-ARENA:reader r:IR-ARENA:reader l:n :}
   8 0 ?do r l i OFF-OPST + RC@ i FROW-FIELDS ! loop
   l 0= if 0 else r l 1- ROW-END then {: at:n :}
   at OFF-OPST FROW-FIELD STEP-CK
   at OFF-OPN FROW-FIELD LEN-OK + {: a1:n :}
   a1 OFF-RSST FROW-FIELD STEP-CK
   a1 OFF-RSN FROW-FIELD LEN-OK + {: a2:n :}
   a2 OFF-SCST FROW-FIELD STEP-CK
   a2 OFF-SCN FROW-FIELD LEN-OK + {: a3:n :}
   a3 OFF-ATST FROW-FIELD STEP-CK
   a3 OFF-ATN FROW-FIELD LEN-OK + p PCELLS > if E-IR-OP-STATE throw then ;

: WIN@ ( IR-ARENA:reader IR-ARENA:reader n n n n -- n )
   {: p:IR-ARENA:reader r:IR-ARENA:reader l:n stoff:n lenoff:n i:n :}
   p r l TILE-CK
   r l lenoff RC@ {: ln:n :}
   i 0 < i ln >= or if E-IR-OP-BOUND throw then
   p  r l stoff RC@ i +  PC@ ORD-OK ;

: FROW-HIT? ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id -- bool )
   {: p:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   0 FROW-READY @ 0= if false exit then
   p 0 FROW-READERS @ IR-ARENA:READER-SAME? 0= if false exit then
   r 1 FROW-READERS @ IR-ARENA:READER-SAME? 0= if false exit then
   key KEY-SERIAL FROW-KEY @ <> if false exit then
   id IR-ID:OP-OWNER MID-SERIAL FROW-KEY @ <> if false exit then
   id IR-ID:OP-LOCAL FROW-LOCAL @ = ;

\ Frozen readers only: a matching token for a mutable arena does not prove the
\ row still tiles.
: FROW-USE ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id -- )
   {: p:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   p IR-ARENA:FROZEN-READER drop r IR-ARENA:FROZEN-READER drop
   p r key id FROW-HIT? if exit then
   p r key PR-CK
   r id ROW-AT {: l:n :}
   false 0 FROW-READY !
   p r l FTILE-CK
   p 0 FROW-READERS ! r 1 FROW-READERS !
   key KEY-SERIAL FROW-KEY !
   id IR-ID:OP-LOCAL FROW-LOCAL !
   true 0 FROW-READY ! ;

: FWIN@ ( IR-ARENA:reader n n n -- n )
   {: p:IR-ARENA:reader stoff:n lenoff:n i:n :}
   lenoff FROW-FIELD {: ln:n :}
   i 0 < i ln >= or if E-IR-OP-BOUND throw then
   p stoff FROW-FIELD i + PC@ ORD-OK ;

\ A length that is not a whole number of entries is refused, not rounded down.
: AT-N ( n -- n )
   dup AT-CELLS mod 0 <> if E-IR-OP-STATE throw then
   AT-CELLS / ;

\ k is AT-KEY or AT-VAL. Bounded by the entry count, so a doubled index cannot
\ reach past the window.
: ATWIN@ ( IR-ARENA:reader IR-ARENA:reader n n n -- n )
   {: p:IR-ARENA:reader r:IR-ARENA:reader l:n i:n k:n :}
   p r l TILE-CK
   r l OFF-ATN RC@ LEN-OK AT-N {: n:n :}
   i 0 < i n >= or if E-IR-OP-BOUND throw then
   p  r l OFF-ATST RC@ i AT-CELLS * + k +  PC@ ORD-OK ;

: FATWIN@ ( IR-ARENA:reader n n -- n )
   {: p:IR-ARENA:reader i:n k:n :}
   OFF-ATN FROW-FIELD AT-N {: n:n :}
   i 0 < i n >= or if E-IR-OP-BOUND throw then
   p OFF-ATST FROW-FIELD i AT-CELLS * + k + PC@ ORD-OK ;

\ ---- the staged operation ----------------------------------------------------
\ Any end consumes the stage, so no half-staged operation survives.
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

\ The staged lists share one pair of arrays addressed by list index and segment
\ base, because a Habu word cannot take a storage array as an argument.
5 constant LIST#
0 constant L-OP                      \ operand value ordinals
1 constant L-RS                      \ result type ordinals
2 constant L-SC                      \ successor block ordinals
3 constant L-AT                      \ attribute ordinals
4 constant L-AK                      \ attribute key symbol ordinals

align
0 CWORK-VARIABLE STG-MODE n
0 CWORK-VARIABLE STG-OPC n
0 CWORK-VARIABLE STG-OPCO n
0 CWORK-VARIABLE STG-SRC n
0 CWORK-VARIABLE STG-SRCO n
0 CWORK-VARIABLE STG-SBEG n
0 CWORK-VARIABLE STG-SLEN n
LIST# ARITY-MAX * CWORK-SPAN STG-V n
LIST# ARITY-MAX * CWORK-SPAN STG-O n
LIST# CWORK-SPAN STG-N n

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
\ Every check runs before the first IR-ARENA:PUSH. An operand's ordinal is below
\ the live value count, so construction order is dominance order (E-IR-OP-SSA).
\ Successors and block-argument blocks point forward legitimately: only their
\ module is checked here; existence is IR-FUN's and IR-VERIFY's. So are attribute
\ key existence and key legality: an attribute is a (key, value) pair because the
\ schema declares required keys.
: OWNED-CK ( n n -- )
   {: hs:n owner:n :}
   owner hs SERIAL-CK ;

: OPERANDS-CK ( n n -- )
   {: hs:n vcnt:n :}
   L-OP SN@ 0 ?do
      hs L-OP SEG i + SO@ OWNED-CK
      L-OP SEG i + SV@ dup 0 < swap vcnt >= or if E-IR-OP-SSA throw then
   loop ;

: RESULTS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: tr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   L-RS SN@ 0 ?do
      key KEY-SERIAL L-RS SEG i + SO@ OWNED-CK
      tr key L-RS SEG i + SV@ IR-ID:PACK-TYPE IR-TYPE:KIND@ drop
   loop ;

\ Key declaration and key existence are the freeze verifier's: this file holds
\ no symbol table.
: ATTRS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: ar:IR-ARENA:arena key:IR-ID:ir-module-key :}
   L-AT SN@ 0 ?do
      key KEY-SERIAL L-AT SEG i + SO@ OWNED-CK
      ar key L-AT SEG i + SV@ IR-ID:PACK-ATTR IR-ATTR:KIND@ drop
      key KEY-SERIAL L-AK SEG i + SO@ OWNED-CK
      L-AK SEG i + SV@ 0 < if E-IR-OP-BOUND throw then
   loop ;

: SUCCS-CK ( n -- )
   {: hs:n :}
   L-SC SN@ 0 ?do
      hs L-SC SEG i + SO@ OWNED-CK
      L-SC SEG i + SV@ 0 < if E-IR-OP-BOUND throw then
   loop ;

\ Exact count without a variadic tail; with one, the fixed part is one shorter
\ and the tail covers any number of further entries, including none.
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
   L-OP SN@ L-RS SN@ + L-SC SN@ + L-AT SN@ AT-CELLS * + ;

: ROOM-CK ( IR-ARENA:reader IR-ARENA:reader IR-ARENA:reader -- )
   {: p:IR-ARENA:reader v:IR-ARENA:reader r:IR-ARENA:reader :}
   r CNT r HC-CAP IR-ARENA:RD@ >= if E-IR-OP-CAP throw then
   v VCNT L-RS SN@ + v HC-CAP IR-ARENA:RD@ > if E-IR-OP-CAP throw then
   p PCELLS STAGED-CELLS + p HC-CAP IR-ARENA:RD@ > if E-IR-OP-CAP throw then ;

\ Pool, operation row and value rows are all reserved before any is written, so
\ the windows always tile; after the room check so this store's capacity error
\ comes first.
: ROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c p STAGED-CELLS IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE
   c v L-RS SN@ VROW-CELLS * IR-ARENA:RESERVE ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: LIST-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena l:n :}
   l SN@ 0 ?do
      c p l SEG i + SV@ CELL+
   loop ;

: RESULTS-ADD ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena base:n :}
   L-RS SN@ 0 ?do
      c p base i + CELL+
   loop ;

: ATTRS-ADD ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena :}
   L-AT SN@ 0 ?do
      c p L-AK SEG i + SV@ CELL+
      c p L-AT SEG i + SV@ CELL+
   loop ;

\ The tiling TILE-CK later revalidates.
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
   c r sat CELL+   c r L-AT SN@ AT-CELLS * CELL+
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
\ All three headers are bound to key's module serial; the stores die with the
\ owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key ocap:n vcap:n pcap:n :}
   ocap ROW-CAP-OK
   vcap VAL-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p HDR-CELLS IR-ARENA:RESERVE
   c p OPL-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c vcap VROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: v:IR-ARENA:arena :}
   c v HDR-CELLS IR-ARENA:RESERVE
   c v OPV-MAGIC CELL+
   c v key KEY-SERIAL CELL+
   c v vcap CELL+
   c ocap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r OPR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r ocap CELL+
   p v r ;

\ ---- the operation builder ---------------------------------------------------
: BEGIN-OP ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   STG-MODE @ MODE-NONE <> if E-IR-OP-STAGE throw then
   MODE-OPEN STG-MODE !
   STG-RESET
   op SYM-ORD STG-OPC !
   op SYM-OWNER STG-OPCO ! ;

: ABANDON ( -- )
   STG-TAKE ;

: ADD-OPERAND ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   STG-OPEN-CK
   L-OP val IR-ID:VALUE-LOCAL val VAL-OWNER ORD+ ;

\ The value identity is minted by END-OP.
: ADD-RESULT ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   STG-OPEN-CK
   L-RS t IR-ID:TYPE-LOCAL t TYP-OWNER ORD+ ;

: ADD-SUCCESSOR ( IR-ID:ir-block-id -- )
   {: b:IR-ID:ir-block-id :}
   STG-OPEN-CK
   L-SC b IR-ID:BLOCK-LOCAL b BLK-OWNER ORD+ ;

\ The key push runs first and is refused at the same count as the value push, so
\ the two lists stay the same length.
: ADD-ATTR ( IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: k:IR-ID:ir-symbol-id a:IR-ID:ir-attr-id :}
   STG-OPEN-CK
   L-AK k SYM-ORD k SYM-OWNER ORD+
   L-AT a IR-ID:ATTR-LOCAL a ATTR-OWNER ORD+ ;

: SET-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   STG-OPEN-CK
   STG-SRC @ UNSET <> if E-IR-OP-STAGE throw then
   src IR-ID:SOURCE-LOCAL STG-SRC !
   src SRC-OWNER STG-SRCO !
   st STG-SBEG !
   ln STG-SLEN ! ;

\ The stage is consumed whatever the outcome; all checks precede the first write.
: END-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx pa:IR-ARENA:arena va:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   STG-TAKE
   pa IR-ARENA:OPEN-LIVE {: p:IR-ARENA:reader :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
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
   c pa va ra ROOM-TAKE
   p PCELLS {: st:n :}
   r CNT {: l:n :}
   v VCNT {: base:n :}
   c pa L-OP LIST-ADD
   c pa base RESULTS-ADD
   c pa L-SC LIST-ADD
   c pa ATTRS-ADD
   c ra st ROW-ADD
   c va l VROWS-ADD
   key l IR-ID:PACK-OP ;

\ ---- block arguments ---------------------------------------------------------
\ The block is owner-checked only: arguments are minted before the block's row
\ exists. All checks precede the first write.
: MINT-ARG ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id IR-ID:ir-type-id n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx va:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key blk:IR-ID:ir-block-id t:IR-ID:ir-type-id pos:n :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   v key VKEY-CK
   key KEY-SERIAL blk BLK-OWNER OWNED-CK
   key KEY-SERIAL t TYP-OWNER OWNED-CK
   tr t IR-TYPE:KIND@ drop
   pos 0 < if E-IR-OP-BOUND throw then
   v VCNT v HC-CAP IR-ARENA:RD@ >= if E-IR-OP-CAP throw then
   v VCNT {: l:n :}
   c va t IR-ID:TYPE-LOCAL CELL+
   c va IR--OP-DEF--KIND:BLK-ARG KIND-CODE CELL+
   c va blk IR-ID:BLOCK-LOCAL CELL+
   c va pos CELL+
   key l IR-ID:PACK-VALUE ;

\ ---- table readers -----------------------------------------------------------
: OPS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE CNT ;

: VALUES ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE VCNT ;

: POOL-CELLS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE PCELLS ;

private

: FLD ( IR-ARENA:reader IR-ID:ir-op-id n -- n )
   {: r:IR-ARENA:reader id:IR-ID:ir-op-id off:n :}
   r  r id ROW-AT  off RC@ ;

: VFLD ( IR-ARENA:reader IR-ID:ir-value-id n -- n )
   {: v:IR-ARENA:reader id:IR-ID:ir-value-id off:n :}
   v  v id VAL-AT  off VC@ ;

public

\ ---- operation readers -------------------------------------------------------
: OPCODE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key RKEY-CK
   key r id OFF-OPC FLD ORD-OK IR-ID:PACK-SYMBOL ;

: OPERANDS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:arena id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE id OFF-OPN FLD LEN-OK ;

: RESULTS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:arena id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE id OFF-RSN FLD LEN-OK ;

: SUCCESSORS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:arena id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE id OFF-SCN FLD LEN-OK ;

: ATTRS ( IR-ARENA:arena IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:arena id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE id OFF-ATN FLD LEN-OK AT-N ;

\ The consumer revalidates the span with IR-SOURCE:SPAN-CK.
: SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id -- IR-SOURCE:span )
   {: ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key RKEY-CK
   r id ROW-AT {: l:n :}
   key r l OFF-SRC RC@ ORD-OK IR-ID:PACK-SOURCE
   r l OFF-SBEG RC@ LEN-OK
   r l OFF-SLEN RC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pa:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pa IR-ARENA:OPEN-LIVE {: p:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-OPST OFF-OPN i WIN@ IR-ID:PACK-VALUE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pa:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pa IR-ARENA:OPEN-LIVE {: p:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-RSST OFF-RSN i WIN@ IR-ID:PACK-VALUE ;

: SUCCESSOR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: pa:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pa IR-ARENA:OPEN-LIVE {: p:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   p r key PR-CK
   key p r  r id ROW-AT  OFF-SCST OFF-SCN i WIN@ IR-ID:PACK-BLOCK ;

\ Whether the opcode declares this key is the freeze verifier's question.
: ATTR-KEY@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-symbol-id )
   {: pa:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pa IR-ARENA:OPEN-LIVE {: p:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   p r key PR-CK
   key p r  r id ROW-AT  i AT-KEY ATWIN@ IR-ID:PACK-SYMBOL ;

\ ---- value readers -----------------------------------------------------------
: VALUE-KIND@ ( IR-ARENA:arena IR-ID:ir-value-id -- IR-OP:def-kind )
   {: va:IR-ARENA:arena id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE id OFF-VKIND VFLD N>KIND ;

: VALUE-TYPE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: va:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   v key VKEY-CK
   key v id OFF-VTYP VFLD ORD-OK IR-ID:PACK-TYPE ;

private

: RESULT-KIND-CK ( n -- )
   N>KIND IR--OP-DEF--KIND:OP-RESULT IR--OP-DEF--KIND:EQ
   0= if E-IR-OP-KIND throw then ;

: ARG-KIND-CK ( n -- )
   N>KIND IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   0= if E-IR-OP-KIND throw then ;

public

: VALUE-OP@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: va:IR-ARENA:arena ra:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   ra IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   v key VKEY-CK
   r key RKEY-CK
   v id OFF-VKIND VFLD RESULT-KIND-CK
   v id OFF-VDEF VFLD ORD-OK
   dup r CNT >= if E-IR-OP-BOUND throw then
   key swap IR-ID:PACK-OP ;

: VALUE-POS@ ( IR-ARENA:arena IR-ID:ir-value-id -- n )
   {: va:IR-ARENA:arena id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   v id OFF-VKIND VFLD RESULT-KIND-CK
   v id OFF-VPOS VFLD LEN-OK ;

\ Owner-checked here; the block table bound-checks it.
: VALUE-BLOCK@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-block-id )
   {: va:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   v key VKEY-CK
   v id OFF-VKIND VFLD ARG-KIND-CK
   key v id OFF-VDEF VFLD ORD-OK IR-ID:PACK-BLOCK ;

: VALUE-ARG@ ( IR-ARENA:arena IR-ID:ir-value-id -- n )
   {: va:IR-ARENA:arena id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN-LIVE {: v:IR-ARENA:reader :}
   v id OFF-VKIND VFLD ARG-KIND-CK
   v id OFF-VPOS VFLD LEN-OK ;

\ ---- frozen readers ----------------------------------------------------------
: ROPS ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER CNT ;

: RVALUES ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER VCNT ;

: FVALUES ( IR-ARENA:view -- n )
   IR-ARENA:OPEN RVALUES ;

: ROPCODE@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: ra:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER {: r:IR-ARENA:reader :}
   r key RKEY-CK
   key r id OFF-OPC FLD ORD-OK IR-ID:PACK-SYMBOL ;

: FOPCODE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id -- IR-ID:ir-symbol-id )
   {: ra:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN key id ROPCODE@ ;

: ROPERANDS ( IR-ARENA:reader IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:reader id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER id OFF-OPN FLD LEN-OK ;

: FOPERANDS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:view id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN id ROPERANDS ;

: RRESULTS ( IR-ARENA:reader IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:reader id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER id OFF-RSN FLD LEN-OK ;

: FRESULTS ( IR-ARENA:view IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:view id:IR-ID:ir-op-id :}
   ra IR-ARENA:OPEN id RRESULTS ;

: RSUCCESSORS ( IR-ARENA:reader IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:reader id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER id OFF-SCN FLD LEN-OK ;

: RATTRS ( IR-ARENA:reader IR-ID:ir-op-id -- n )
   {: ra:IR-ARENA:reader id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER id OFF-ATN FLD LEN-OK AT-N ;

: RSPAN@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id -- IR-SOURCE:span )
   {: ra:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id :}
   ra IR-ARENA:FROZEN-READER {: r:IR-ARENA:reader :}
   r key RKEY-CK
   r id ROW-AT {: l:n :}
   key r l OFF-SRC RC@ ORD-OK IR-ID:PACK-SOURCE
   r l OFF-SBEG RC@ LEN-OK
   r l OFF-SLEN RC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: ROPERAND@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pv:IR-ARENA:reader rv:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv rv key id FROW-USE
   key pv OFF-OPST OFF-OPN i FWIN@ IR-ID:PACK-VALUE ;

: FOPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN key id i ROPERAND@ ;

: RRESULT@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pv:IR-ARENA:reader rv:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv rv key id FROW-USE
   key pv OFF-RSST OFF-RSN i FWIN@ IR-ID:PACK-VALUE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN key id i RRESULT@ ;

: RSUCCESSOR@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: pv:IR-ARENA:reader rv:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv rv key id FROW-USE
   key pv OFF-SCST OFF-SCN i FWIN@ IR-ID:PACK-BLOCK ;

: FSUCCESSOR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-block-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN key id i RSUCCESSOR@ ;

: RATTR@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-attr-id )
   {: pv:IR-ARENA:reader rv:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv rv key id FROW-USE
   key pv i AT-VAL FATWIN@ IR-ID:PACK-ATTR ;

: FATTR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-attr-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN key id i RATTR@ ;

: RATTR-KEY@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-symbol-id )
   {: pv:IR-ARENA:reader rv:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv rv key id FROW-USE
   key pv i AT-KEY FATWIN@ IR-ID:PACK-SYMBOL ;

: FATTR-KEY@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ID:ir-symbol-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN key id i RATTR-KEY@ ;

: RVALUE-KIND@ ( IR-ARENA:reader IR-ID:ir-value-id -- IR-OP:def-kind )
   {: va:IR-ARENA:reader id:IR-ID:ir-value-id :}
   va IR-ARENA:FROZEN-READER id OFF-VKIND VFLD N>KIND ;

: RVALUE-TYPE@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: va:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:FROZEN-READER {: v:IR-ARENA:reader :}
   v key VKEY-CK
   key v id OFF-VTYP VFLD ORD-OK IR-ID:PACK-TYPE ;

: FVALUE-OP@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: va:IR-ARENA:view ra:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:OPEN {: v:IR-ARENA:reader :}
   ra IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   v key VKEY-CK
   r key RKEY-CK
   v id OFF-VKIND VFLD RESULT-KIND-CK
   v id OFF-VDEF VFLD ORD-OK
   dup r CNT >= if E-IR-OP-BOUND throw then
   key swap IR-ID:PACK-OP ;

: RVALUE-BLOCK@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-value-id -- IR-ID:ir-block-id )
   {: va:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-value-id :}
   va IR-ARENA:FROZEN-READER {: v:IR-ARENA:reader :}
   v key VKEY-CK
   v id OFF-VKIND VFLD ARG-KIND-CK
   key v id OFF-VDEF VFLD ORD-OK IR-ID:PACK-BLOCK ;

: RVALUE-ARG@ ( IR-ARENA:reader IR-ID:ir-value-id -- n )
   {: va:IR-ARENA:reader id:IR-ID:ir-value-id :}
   va IR-ARENA:FROZEN-READER {: v:IR-ARENA:reader :}
   v id OFF-VKIND VFLD ARG-KIND-CK
   v id OFF-VPOS VFLD LEN-OK ;

\ The row memo is reset at capture so an image's bytes do not depend on which row
\ the build read last.
: CAPTURE-PREPARE ( -- )
   false 0 FROW-READY !
   0 0 FROW-READERS BYTE-VIEW CELL-VIEW !  0 1 FROW-READERS BYTE-VIEW CELL-VIEW !
   8 0 ?do 0 i FROW-FIELDS ! loop
   0 FROW-KEY !  0 FROW-LOCAL ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
