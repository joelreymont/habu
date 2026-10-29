\ build.f - the builder and freeze lifecycle of one IR module.
\ A live builder is the only route to its module's sixteen table arenas: they
\ never leave this package. FREEZE publishes them as read-only views, ABORT and
\ RETIRE retire them. Every append delegates to the table's owner; whole-module
\ checks are IR-VERIFY's (src/compiler/ir/verify.f), per-record checks are
\ IR-OP:END-OP, IR-FUN:END-FUN and IR-FUN:END-BLOCK.

require src/lib/prelude.f
require src/lib/errors.f
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
require src/compiler/ir/verify.f

package IR-BUILD
public

NEWTYPE builder
NEWTYPE module

private

CAST: MINT-B ( n -- IR-BUILD:builder )
CAST: B>N ( IR-BUILD:builder -- n )
CAST: MINT-M ( n -- IR-BUILD:module )
CAST: M>N ( IR-BUILD:module -- n )

\ ---- capacities and slot states ----------------------------------------------
16 constant SLOT-MAX                 \ simultaneous live and frozen modules
$7FFFFFFF constant BGEN-MAX          \ builder generation ceiling
1 constant ST-LIVE
2 constant ST-FROZEN
3 constant ST-ABORTED
4 constant ST-RETIRED
0 constant STG-FREE                  \ no builder holds this stage

\ ---- the sixteen tables a module is made of ----------------------------------
0 constant T-SP                      \ symbol interner byte pool
1 constant T-SR                      \ symbol interner rows
2 constant T-TP                      \ type table pool
3 constant T-TR                      \ type table rows
4 constant T-AR                      \ attribute table rows
5 constant T-SA                      \ source registry rows
6 constant T-QP                      \ dialect schema pool
7 constant T-QR                      \ dialect schema rows
8 constant T-OP                      \ operation cell pool
9 constant T-OV                      \ value rows
10 constant T-OR                     \ operation rows
11 constant T-FP                     \ function attribute pool
12 constant T-FR                     \ function rows
13 constant T-BR                     \ block rows
14 constant T-EP                     \ derived predecessor pool
15 constant T-ER                     \ derived block-edge rows
16 constant TABLES#

\ ---- the committed ceiling plan ----------------------------------------------
\ Staged by name before NEW-BUILDER, which consumes it.
0 constant P-SYMS                    \ symbol rows
1 constant P-SBYTES                  \ symbol pool bytes
2 constant P-TYPES
3 constant P-TPOOL
4 constant P-ATTRS
5 constant P-SRCS
6 constant P-SCHEMAS
7 constant P-QPOOL
8 constant P-OPS
9 constant P-VALS
10 constant P-OPOOL
11 constant P-FUNS
12 constant P-BLKS
13 constant P-FPOOL
14 constant PLAN#
-1 constant UNSET

\ Defaults are the table formats' representable ranges; arenas grow from small
\ seeds.
IR-SYM:CAP-MAX constant D-SYMS
IR-SYM:BYTE-MAX constant D-SBYTES
IR-TYPE:CAP-MAX constant D-TYPES
IR-TYPE:LIST-MAX constant D-TPOOL
IR-ATTR:CAP-MAX constant D-ATTRS
IR-SOURCE:CAP-MAX constant D-SRCS
IR-SCHEMA:CAP-MAX constant D-SCHEMAS
IR-SCHEMA:POOL-MAX constant D-QPOOL
IR-OP:ROW-CAP-MAX constant D-OPS
IR-OP:VAL-CAP-MAX constant D-VALS
IR-OP:POOL-CAP-MAX constant D-OPOOL
IR-FUN:FNROW-CAP-MAX constant D-FUNS
IR-FUN:BLK-CAP-MAX constant D-BLKS
IR-FUN:POOL-CAP-MAX constant D-FPOOL

\ ---- registry storage --------------------------------------------------------
align
variable BGEN-CELL
0 BGEN-CELL !
SLOT-MAX CWORK-SPAN BGENS n
SLOT-MAX CWORK-SPAN BOWNERS n
SLOT-MAX CWORK-SPAN BSTATES n
SLOT-MAX PLAN# * CWORK-SPAN BCEILS n
SLOT-MAX CWORK-SPAN BPROTOS n  \ the prototype rows this module's interner
                                     \ was copied from, zero if it interned its own
SLOT-MAX TABLES# * CWORK-BUFFER BTAB IR-ARENA:arena
SLOT-MAX TABLES# * CWORK-BUFFER BVIEW IR-ARENA:view
SLOT-MAX CWORK-BUFFER BKEY IR-ID:ir-module-key
SLOT-MAX CWORK-BUFFER BMID IR-ID:ir-module-id

: BGEN@ ( n -- n )
   cells BGENS + @ ;

: BGEN! ( n n -- )
   cells BGENS + ! ;

: BOWNER@ ( n -- n )
   cells BOWNERS + @ ;

: BOWNER! ( n n -- )
   cells BOWNERS + ! ;

: BSTATE@ ( n -- n )
   cells BSTATES + @ ;

: BSTATE! ( n n -- )
   cells BSTATES + ! ;

CAST: ARENA>N ( IR-ARENA:arena -- n )

: BPROTO@ ( n -- n )
   cells BPROTOS + @ ;

: BPROTO! ( n n -- )
   cells BPROTOS + ! ;

: CEIL-IDX ( n n -- n )
   {: slot:n f:n :}
   slot PLAN# * f + ;

: BCEIL@ ( n n -- n )
   CEIL-IDX cells BCEILS + @ ;

: BCEIL! ( n n n -- )
   CEIL-IDX cells BCEILS + ! ;

\ ---- the table transaction ---------------------------------------------------
\ A failed creation gives no arena back: each is a record in the context's
\ region and dies with its mark. TAB! is the only writer of the table array and
\ counts what it writes; tables are created in index order T-SP..T-ER, so a
\ write whose index is not the count is refused. The inputs are parked here
\ because a checked catch takes a stack-neutral quotation.
-1 constant TX-NONE                  \ no builder is being built

TX-NONE CWORK-VARIABLE TX-SLOT n
0 CWORK-VARIABLE TX-N n                        \ tables of TX-SLOT that exist
0 CWORK-VARIABLE TX-U n
0 CWORK-VARIABLE TX-MAJOR n
0 CWORK-VARIABLE TX-MINOR n
0 CWORK-VARIABLE TX-DIA ptr u8
1 CWORK-BUFFER TX-CTX IR-CTX:ctx

: TX-SLOT@ ( -- n )
   TX-SLOT @ dup TX-NONE = if E-IR-BUILD-STATE throw then ;

: TX-CTX@ ( -- IR-CTX:ctx )
   0 TX-CTX @ ;

: TX-OPEN ( IR-CTX:ctx n ptr u8 n n n -- )
   {: c:IR-CTX:ctx slot:n p u:n major:n minor:n :}
   TX-SLOT @ TX-NONE <> if E-IR-BUILD-STATE throw then
   c 0 TX-CTX !
   p TX-DIA !
   u TX-U !
   major TX-MAJOR !
   minor TX-MINOR !
   slot TX-SLOT !
   0 TX-N ! ;

: TX-CLOSE ( -- )
   TX-NONE TX-SLOT !
   0 TX-N !
   NULL-PTR TX-DIA !  0 TX-U !
   0 TX-MAJOR !  0 TX-MINOR ! ;

public

: CAPTURE-PREPARE ( -- )
   TX-SLOT @ TX-NONE <> if E-IR-BUILD-STATE throw then
   IR-ARENA:CAPTURE-PREPARE
   IR-CTX:CAPTURE-PREPARE ;

private

: TAB@ ( n n -- IR-ARENA:arena )
   {: slot:n k:n :}
   slot TABLES# * k + BTAB @ ;

\ Refuses a write outside the open transaction or out of index order.
: TAB! ( IR-ARENA:arena n n -- )
   {: slot:n k:n :}
   slot TX-SLOT@ <> if E-IR-BUILD-STATE throw then
   k TX-N @ <> if E-IR-BUILD-STATE throw then
   slot TABLES# * k + BTAB !
   k 1+ TX-N ! ;

\ Publication needs all sixteen tables: a short count would publish the
\ previous holder's arena in the missing entry.
: TX-ABANDON ( -- )
   TX-CLOSE ;

: TX-COMMIT ( -- )
   TX-N @ TABLES# <> if TX-ABANDON E-IR-BUILD-STATE throw then
   TX-CLOSE ;

: VIEW@ ( n n -- IR-ARENA:view )
   {: slot:n k:n :}
   slot TABLES# * k + BVIEW @ ;

: VIEW! ( IR-ARENA:view n n -- )
   {: slot:n k:n :}
   slot TABLES# * k + BVIEW ! ;

: KEY@ ( n -- IR-ID:ir-module-key )
   BKEY @ ;

: KEY! ( IR-ID:ir-module-key n -- )
   BKEY ! ;

\ ---- generation serials ------------------------------------------------------
\ Builder and module handles are never-reused generation serials, and a slot
\ keeps its generation after use so a bad handle is named E-IR-BUILD-FROZEN,
\ -ABORTED, -RETIRED, -STALE or -MODULE.
: BGEN-NEXT-N ( n -- n )
   dup 0 < over BGEN-MAX >= or if E-IR-BUILD-SERIALS throw then
   1+ ;

: TRY-BGEN ( -- n bool )
   BGEN-CELL atomic@ {: current:n :}
   current BGEN-NEXT-N {: next:n :}
   current next BGEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-BGEN ( -- n )
   begin
      TRY-BGEN dup 0=
   while
      2drop
   repeat
   drop ;

\ FILE objects may contain retired builder handles. Replaying their allocation
\ history prevents a later builder from receiving one of those identities.
public
: HISTORY ( -- n ) BGEN-CELL atomic@ ;
: CHECK-HISTORY ( n -- )
   dup 0 < swap BGEN-MAX > or if E-IR-BUILD-SERIALS throw then ;
: REPLAY-HISTORY ( n -- )
   {: want:n :}
   want CHECK-HISTORY
   begin
      BGEN-CELL atomic@ {: old:n :}
      old want >= if exit then
      old want BGEN-CELL atomic-cas old =
   until ;
private

\ ---- handle resolution -------------------------------------------------------
\ No context handle is stored (IR-CTX forbids it outliving WITH-CONTEXT): each
\ word takes the context and checks it against the slot's owner serial.
: FIND-B ( n -- n )
   {: g:n :}
   -1
   SLOT-MAX 0 ?do
      g i BGEN@ = if drop i leave then
   loop ;

\ A generation never minted and a slot whose owning context has torn down are
\ both stale; a dead-owner slot is retired on touch.
: RESOLVE ( n -- n )
   FIND-B
   dup 0 < if E-IR-BUILD-STALE throw then
   dup BOWNER@ IR-CTX:SERIAL-LIVE? 0= if
      0 over BGEN! E-IR-BUILD-STALE throw
   then ;

: STATE-OK ( n -- n )
   dup BSTATE@ {: st:n :}
   st ST-LIVE = st ST-FROZEN = or st ST-ABORTED = or st ST-RETIRED = or 0= if
      E-IR-BUILD-STATE throw
   then ;

: LIVE-SLOT ( IR-BUILD:builder -- n )
   B>N RESOLVE STATE-OK
   dup BSTATE@ ST-ABORTED = if E-IR-BUILD-ABORTED throw then
   dup BSTATE@ ST-LIVE <> if E-IR-BUILD-FROZEN throw then ;

\ A frozen slot changes state once more at most (RETIRE), so any other state
\ here is corrupted registry state.
: FROZEN-SLOT ( IR-BUILD:module -- n )
   M>N RESOLVE STATE-OK
   dup BSTATE@ ST-RETIRED = if E-IR-BUILD-RETIRED throw then
   dup BSTATE@ ST-FROZEN <> if E-IR-BUILD-STATE throw then ;

: OWN-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c IR-CTX:SERIAL slot BOWNER@ <> if E-IR-BUILD-OWNER throw then ;

\ The gate every append and reader passes: builder live, ctx its creator.
: USE ( IR-CTX:ctx IR-BUILD:builder -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b LIVE-SLOT {: slot:n :}
   c slot OWN-CK
   slot ;

\ ---- stage ownership ---------------------------------------------------------
\ IR-OP and IR-FUN keep one open record each per process; these cells name the
\ builder generation holding each. A claim is recorded only after the owner's
\ begin returned and given back before or with the end that consumes it.
0 CWORK-VARIABLE FN-STG n
0 CWORK-VARIABLE BK-STG n
0 CWORK-VARIABLE OP-STG n

: HOLDER-LIVE? ( n -- bool )
   {: g:n :}
   g FIND-B {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot BOWNER@ IR-CTX:SERIAL-LIVE?
   slot BSTATE@ ST-LIVE = and ;

\ A claim whose holder is gone (abandoned by a throw, or its context torn down)
\ is reclaimed with the owner's orphaned record; otherwise one failed
\ construction would lock the stage for the rest of the process.
: OP-REAP ( -- )
   OP-STG @ STG-FREE = if exit then
   OP-STG @ HOLDER-LIVE? if exit then
   IR-OP:ABANDON
   STG-FREE OP-STG ! ;

: BK-REAP ( -- )
   BK-STG @ STG-FREE = if exit then
   BK-STG @ HOLDER-LIVE? if exit then
   IR-FUN:ABANDON-BLOCK
   STG-FREE BK-STG ! ;

: FN-REAP ( -- )
   FN-STG @ STG-FREE = if exit then
   FN-STG @ HOLDER-LIVE? if exit then
   IR-FUN:ABANDON-FUN
   STG-FREE FN-STG ! ;

: FN-FREE-CK ( -- )
   FN-REAP
   FN-STG @ STG-FREE <> if E-IR-BUILD-STAGE throw then ;

: FN-MINE ( n -- )
   FN-STG @ <> if E-IR-BUILD-STAGE throw then ;

: FN-HOLD ( n -- )
   FN-STG ! ;

: FN-DONE ( n -- )
   FN-MINE STG-FREE FN-STG ! ;

: BK-FREE-CK ( -- )
   BK-REAP
   BK-STG @ STG-FREE <> if E-IR-BUILD-STAGE throw then ;

: BK-MINE ( n -- )
   BK-STG @ <> if E-IR-BUILD-STAGE throw then ;

: BK-HOLD ( n -- )
   BK-STG ! ;

: BK-DONE ( n -- )
   BK-MINE STG-FREE BK-STG ! ;

: OP-FREE-CK ( -- )
   OP-REAP
   OP-STG @ STG-FREE <> if E-IR-BUILD-STAGE throw then ;

: OP-MINE ( n -- )
   OP-STG @ <> if E-IR-BUILD-STAGE throw then ;

: OP-HOLD ( n -- )
   OP-STG ! ;

: OP-DONE ( n -- )
   OP-MINE STG-FREE OP-STG ! ;

\ An open record is a builder-only placeholder; it cannot be published.
: STG-CLEAR-CK ( n -- )
   {: g:n :}
   FN-STG @ g = BK-STG @ g = or OP-STG @ g = or
   if E-IR-BUILD-OPEN throw then ;

\ Innermost first, so no staged field survives into the next builder.
: STG-RELEASE ( n -- )
   {: g:n :}
   OP-STG @ g = if IR-OP:ABANDON STG-FREE OP-STG ! then
   BK-STG @ g = if IR-FUN:ABANDON-BLOCK STG-FREE BK-STG ! then
   FN-STG @ g = if IR-FUN:ABANDON-FUN STG-FREE FN-STG ! then ;

\ ---- the ceiling plan --------------------------------------------------------
PLAN# CWORK-SPAN PSTG n
0 CWORK-VARIABLE PSTG-OPEN n

: PSTG@ ( n -- n )
   cells PSTG + @ ;

: PSTG! ( n n -- )
   cells PSTG + ! ;

: PLAN-OPEN-CK ( -- )
   PSTG-OPEN @ 0= if E-IR-BUILD-PLAN throw then ;

: PLAN-SET ( n n -- )
   {: v:n f:n :}
   PLAN-OPEN-CK
   v 0 < if E-IR-BUILD-PLAN throw then
   f PSTG@ UNSET <> if E-IR-BUILD-PLAN throw then
   v f PSTG! ;

\ A field nobody declared is a plan error, not a silent default.
: PLAN-TAKE ( n -- )
   {: slot:n :}
   PLAN-OPEN-CK
   PLAN# 0 ?do
      i PSTG@ UNSET = if E-IR-BUILD-PLAN throw then
   loop
   PLAN# 0 ?do
      i PSTG@ slot i BCEIL!
   loop
   0 PSTG-OPEN ! ;

public

\ ---- declaring the ceilings a module commits to ------------------------------
\ Every field must be declared exactly once before NEW-BUILDER consumes the plan.
: PLAN-BEGIN ( -- )
   PLAN# 0 ?do
      UNSET i PSTG!
   loop
   1 PSTG-OPEN ! ;

: PLAN-SYMBOLS ( n n -- )
   {: rows:n bytes:n :}
   rows P-SYMS PLAN-SET
   bytes P-SBYTES PLAN-SET ;

: PLAN-TYPES ( n n -- )
   {: rows:n pool:n :}
   rows P-TYPES PLAN-SET
   pool P-TPOOL PLAN-SET ;

: PLAN-ATTRS ( n -- )
   P-ATTRS PLAN-SET ;

: PLAN-SOURCES ( n -- )
   P-SRCS PLAN-SET ;

: PLAN-SCHEMAS ( n n -- )
   {: rows:n pool:n :}
   rows P-SCHEMAS PLAN-SET
   pool P-QPOOL PLAN-SET ;

: PLAN-OPS ( n n n -- )
   {: ops:n vals:n pool:n :}
   ops P-OPS PLAN-SET
   vals P-VALS PLAN-SET
   pool P-OPOOL PLAN-SET ;

: PLAN-FUNS ( n n n -- )
   {: funs:n blks:n pool:n :}
   funs P-FUNS PLAN-SET
   blks P-BLKS PLAN-SET
   pool P-FPOOL PLAN-SET ;

: PLAN-DEFAULT ( -- )
   PLAN-BEGIN
   D-SYMS D-SBYTES PLAN-SYMBOLS
   D-TYPES D-TPOOL PLAN-TYPES
   D-ATTRS PLAN-ATTRS
   D-SRCS PLAN-SOURCES
   D-SCHEMAS D-QPOOL PLAN-SCHEMAS
   D-OPS D-VALS D-OPOOL PLAN-OPS
   D-FUNS D-BLKS D-FPOOL PLAN-FUNS ;

private

\ ---- creation ----------------------------------------------------------------
\ Retire every slot whose owning context has torn down.
: SWEEP ( -- )
   SLOT-MAX 0 ?do
      i BGEN@ 0 <> if
         i BOWNER@ IR-CTX:SERIAL-LIVE? 0= if
            0 i BGEN!
         then
      then
   loop ;

: FREE-SLOT ( -- n )
   -1
   SLOT-MAX 0 ?do
      i BGEN@ 0= if drop i leave then
   loop
   dup 0 >= if exit then
   drop -1
   SLOT-MAX 0 ?do
      i BSTATE@ dup ST-ABORTED = swap ST-RETIRED = or if
         drop i leave
      then
   loop
   dup 0 < if E-IR-BUILD-SLOTS throw then ;

\ ---- starting a module's interner from a prototype ---------------------------
\ A builder created FROM a prototype starts with the prototype's interned
\ dialect vocabulary at the same ordinals, so the dialect binding mints its
\ identities instead of looking them up. The prototype is staged for one
\ creation and cleared on success or throw.
2 CWORK-BUFFER PROTO-SYM IR-ARENA:arena
0 CWORK-VARIABLE PROTO-ON n

: PROTO-CLEAR ( -- )
   0 PROTO-ON ! ;

\ The slot records the prototype's row table, or zero when the module interned
\ its own names: only a copied interner lets a caller read a prototype ordinal
\ as the same spelling here. A slot is reused, so the plain path clears it.
: SYM-NEW ( IR-CTX:ctx n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx slot:n :}
   PROTO-ON @ 0<> if
      c slot KEY@ 0 PROTO-SYM @ 1 PROTO-SYM @
      slot P-SYMS BCEIL@ slot P-SBYTES BCEIL@ IR-SYM:NEW-FROM
      1 PROTO-SYM @ ARENA>N slot BPROTO!
      exit
   then
   0 slot BPROTO!
   c slot KEY@ slot P-SYMS BCEIL@ slot P-SBYTES BCEIL@ IR-SYM:NEW ;

: SYM-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot SYM-NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a slot T-SP TAB!
   r slot T-SR TAB! ;

: TYPE-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-TYPES BCEIL@ slot P-TPOOL BCEIL@ IR-TYPE:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-TP TAB!
   r slot T-TR TAB! ;

: ATTR-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-ATTRS BCEIL@ IR-ATTR:NEW slot T-AR TAB! ;

: SRC-TABLE ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-SRCS BCEIL@ IR-SOURCE:NEW slot T-SA TAB! ;

: SCHEMA-TABLES ( IR-CTX:ctx n IR-ID:ir-symbol-id n n -- )
   {: c:IR-CTX:ctx slot:n dia:IR-ID:ir-symbol-id major:n minor:n :}
   c  slot T-SR TAB@  slot KEY@ dia major minor
   slot P-SCHEMAS BCEIL@ slot P-QPOOL BCEIL@ IR-SCHEMA:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-QP TAB!
   r slot T-QR TAB! ;

: OP-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-OPS BCEIL@ slot P-VALS BCEIL@ slot P-OPOOL BCEIL@ IR-OP:NEW
   {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-OP TAB!
   v slot T-OV TAB!
   r slot T-OR TAB! ;

: FUN-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-FUNS BCEIL@ slot P-BLKS BCEIL@ slot P-FPOOL BCEIL@ IR-FUN:NEW
   {: p:IR-ARENA:arena f:IR-ARENA:arena b:IR-ARENA:arena :}
   p slot T-FP TAB!
   f slot T-FR TAB!
   b slot T-BR TAB! ;

\ Ceilings come from the plan: one row per block, one predecessor cell per
\ operand pool cell (every successor a terminator names occupies one).
: EDGE-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-BLKS BCEIL@ slot P-OPOOL BCEIL@ IR-VERIFY:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-EP TAB!
   r slot T-ER TAB! ;

\ The symbol interner comes first: the schema table names its dialect by a
\ symbol of this module. Stack-neutral so it can run under a checked catch.
: TABLES-BUILD ( -- )
   TX-CTX@ {: c:IR-CTX:ctx :}
   TX-SLOT@ {: slot:n :}
   c slot SYM-TABLES
   c slot TYPE-TABLES
   c slot ATTR-TABLES
   c slot SRC-TABLE
   c  slot T-SP TAB@  slot T-SR TAB@  slot KEY@
   TX-DIA @ TX-U @ IR-SYM:INTERN {: dia:IR-ID:ir-symbol-id :}
   c slot dia TX-MAJOR @ TX-MINOR @ SCHEMA-TABLES
   c slot OP-TABLES
   c slot FUN-TABLES
   c slot EDGE-TABLES ;

\ On a refusal the caller sees the original refusal; the tables already made are
\ records in the context's region and go back with its mark.
: TABLES-TRY ( -- )
   [: TABLES-BUILD ;] catch {: rc:n :}
   rc 0= if TX-COMMIT exit then
   TX-ABANDON
   rc throw ;

public

\ The plan becomes the module's committed ceilings. The module serial counts
\ against the context's module ceiling and is not reused after a failed
\ creation; the generation is installed last, so a failure leaves no slot.
: NEW-BUILDER ( IR-CTX:ctx ptr u8 n n n -- IR-BUILD:builder )
   {: c:IR-CTX:ctx p u:n major:n minor:n :}
   PLAN-OPEN-CK
   SWEEP
   FREE-SLOT {: slot:n :}
   TAKE-BGEN {: g:n :}
   slot PLAN-TAKE
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   key slot KEY!
   mid slot BMID !
   c slot p u major minor TX-OPEN
   TABLES-TRY
   c IR-CTX:SERIAL slot BOWNER!
   ST-LIVE slot BSTATE!
   g slot BGEN!
   g MINT-B ;

: NEW-BUILDER-FROM ( IR-CTX:ctx ptr u8 n n n IR-ARENA:arena IR-ARENA:arena -- IR-BUILD:builder )
   {: c:IR-CTX:ctx p u:n major:n minor:n pa:IR-ARENA:arena pr:IR-ARENA:arena :}
   pa 0 PROTO-SYM !
   pr 1 PROTO-SYM !
   1 PROTO-ON !
   c p u major minor [: NEW-BUILDER ;] [: PROTO-CLEAR ;] finally ;

\ ---- identity ----------------------------------------------------------------
: SERIAL ( IR-BUILD:builder -- n )
   B>N ;

: FROZEN? ( IR-BUILD:module -- bool )
   M>N FIND-B {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot BOWNER@ IR-CTX:SERIAL-LIVE?
   slot BSTATE@ ST-FROZEN = and ;

: MODULE@ ( IR-BUILD:builder -- IR-ID:ir-module-id )
   LIVE-SLOT BMID @ ;

\ Confers no mutation power.
: MODULE-KEY ( IR-BUILD:builder -- IR-ID:ir-module-key )
   LIVE-SLOT KEY@ ;

\ Answered from what NEW-BUILDER-FROM recorded, never from the spellings held:
\ a module that interned the same names itself answers no.
: CLONED-FROM? ( IR-BUILD:builder IR-ARENA:arena -- bool )
   {: b:IR-BUILD:builder pr:IR-ARENA:arena :}
   b LIVE-SLOT BPROTO@ {: have:n :}
   have 0= if false exit then
   have pr ARENA>N = ;

\ ---- interning: symbols, sources, and spans ----------------------------------
: INTERN-SYMBOL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :}
   c b USE {: slot:n :}
   c  slot T-SP TAB@  slot T-SR TAB@  slot KEY@  p u IR-SYM:INTERN ;

\ Lets a rewriting pass name what it builds by the old module's symbol.
: CARRY-SYMBOL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder pv:IR-ARENA:view rv:IR-ARENA:view
      id:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   c  slot T-SP TAB@  slot T-SR TAB@  slot KEY@  pv rv id IR-SYM:CARRY ;

: ADD-SOURCE ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :}
   c b USE {: slot:n :}
   c slot T-SA TAB@ slot KEY@ p u IR-SOURCE:REGISTER ;

\ The rewriting pass inherits the source identity instead of re-digesting text.
: CARRY-SOURCE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-source-id -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view id:IR-ID:ir-source-id :}
   c b USE {: slot:n :}
   c slot T-SA TAB@ slot KEY@ v id IR-SOURCE:CARRY ;

: ADD-SOURCE-FROM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-source-id ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder parent:IR-ID:ir-source-id p u:n :}
   c b USE {: slot:n :}
   c slot T-SA TAB@ slot KEY@ parent p u IR-SOURCE:REGISTER-FROM ;

: ADD-SPAN ( IR-BUILD:builder IR-ID:ir-source-id n n -- IR-SOURCE:span )
   {: b:IR-BUILD:builder src:IR-ID:ir-source-id st:n ln:n :}
   b LIVE-SLOT T-SA TAB@ src st ln IR-SOURCE:SPAN ;

\ ---- interning: types --------------------------------------------------------
private

: TYPE-USE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-TP TAB@  slot T-TR TAB@  slot KEY@ ;

public

: INTERN-INT ( IR-CTX:ctx IR-BUILD:builder IR-TYPE:width IR-TYPE:sign -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder w:IR-TYPE:width s:IR-TYPE:sign :}
   c b TYPE-USE w s IR-TYPE:INT ;

: INTERN-FLT ( IR-CTX:ctx IR-BUILD:builder IR-TYPE:fmt -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-TYPE:fmt :}
   c b TYPE-USE f IR-TYPE:FLT ;

: INTERN-POINTER ( IR-CTX:ctx IR-BUILD:builder IR-TYPE:space IR-ID:ir-type-id -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder sp:IR-TYPE:space t:IR-ID:ir-type-id :}
   c b TYPE-USE sp t IR-TYPE:POINTER ;

: INTERN-TOKEN ( IR-CTX:ctx IR-BUILD:builder IR-TYPE:domain -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder d:IR-TYPE:domain :}
   c b TYPE-USE d IR-TYPE:TOKEN ;

: INTERN-MASK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   TYPE-USE IR-TYPE:MASK ;

: INTERN-OPAQUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   TYPE-USE IR-TYPE:OPAQUE ;

: INTERN-QUOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   TYPE-USE IR-TYPE:QUOT ;

: INTERN-CODE-REF ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   TYPE-USE IR-TYPE:CODE-REF ;

\ ---- interning: attributes ---------------------------------------------------
private

: ATTR-USE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-AR TAB@  slot KEY@ ;

public

: INTERN-INT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n :}
   c b ATTR-USE v IR-ATTR:INT ;

: INTERN-SYMBOL-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   c  slot T-AR TAB@  slot KEY@  slot T-SR TAB@ s IR-ATTR:SYMBOL ;

\ ---- the dialect schema table ------------------------------------------------
\ The IR-SCHEMA staging words take identities only and need no table.
: SCHEMA-DEFINED? ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b USE T-QR TAB@ op IR-SCHEMA:DEFINED? ;

: DEFINE-OP ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-QP TAB@  slot T-QR TAB@  slot KEY@  slot T-SR TAB@  slot T-TR TAB@
   IR-SCHEMA:DEFINE ;

\ ---- the operation builder ---------------------------------------------------
: BEGIN-OP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b USE drop
   OP-FREE-CK
   op IR-OP:BEGIN-OP
   b SERIAL OP-HOLD ;

: ADD-OPERAND ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id :}
   c b USE drop
   b SERIAL OP-MINE
   v IR-OP:ADD-OPERAND ;

: ADD-RESULT ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b USE drop
   b SERIAL OP-MINE
   t IR-OP:ADD-RESULT ;

: ADD-SUCCESSOR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder blk:IR-ID:ir-block-id :}
   c b USE drop
   b SERIAL OP-MINE
   blk IR-OP:ADD-SUCCESSOR ;

: ADD-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-symbol-id a:IR-ID:ir-attr-id :}
   c b USE drop
   b SERIAL OP-MINE
   k a IR-OP:ADD-ATTR ;

: SET-OP-SPAN ( IR-CTX:ctx IR-BUILD:builder IR-SOURCE:span -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder span:IR-SOURCE:span :}
   c b USE drop
   b SERIAL OP-MINE
   span IR-OP:SET-SPAN ;

\ The stage is given back before IR-OP runs, so a rejected append leaves none.
: END-OP ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL OP-DONE
   c  slot T-OP TAB@  slot T-OV TAB@  slot T-OR TAB@  slot KEY@
   slot T-QR TAB@  slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   IR-OP:END-OP ;

\ ---- the function builder ----------------------------------------------------
: BEGIN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder sym:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   FN-FREE-CK
   slot T-BR TAB@ sym IR-FUN:BEGIN-FUN
   b SERIAL FN-HOLD ;

: SET-SIGNATURE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b USE drop
   b SERIAL FN-MINE
   t IR-FUN:SET-SIGNATURE ;

: SET-LINKAGE ( IR-CTX:ctx IR-BUILD:builder IR-FUN:linkage -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-FUN:linkage :}
   c b USE drop
   b SERIAL FN-MINE
   v IR-FUN:SET-LINKAGE ;

: SET-VISIBILITY ( IR-CTX:ctx IR-BUILD:builder IR-FUN:visibility -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-FUN:visibility :}
   c b USE drop
   b SERIAL FN-MINE
   v IR-FUN:SET-VISIBILITY ;

: SET-CONVENTION ( IR-CTX:ctx IR-BUILD:builder IR-FUN:convention -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-FUN:convention :}
   c b USE drop
   b SERIAL FN-MINE
   v IR-FUN:SET-CONVENTION ;

: ADD-FUN-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-attr-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ID:ir-attr-id :}
   c b USE drop
   b SERIAL FN-MINE
   a IR-FUN:ADD-FUN-ATTR ;

: SET-FUN-SPAN ( IR-CTX:ctx IR-BUILD:builder IR-SOURCE:span -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder span:IR-SOURCE:span :}
   c b USE drop
   b SERIAL FN-MINE
   span IR-FUN:SET-FUN-SPAN ;

: END-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL FN-DONE
   c  slot T-FP TAB@  slot T-FR TAB@  slot T-BR TAB@  slot KEY@
   slot T-SR TAB@  slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   IR-FUN:END-FUN ;

\ ---- the block builder -------------------------------------------------------
: BEGIN-BLOCK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL FN-MINE
   BK-FREE-CK
   slot T-OR TAB@ IR-FUN:BEGIN-BLOCK
   b SERIAL BK-HOLD ;

: ADD-BLOCK-ARG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b USE {: slot:n :}
   b SERIAL BK-MINE
   c  slot T-OV TAB@  slot T-TR TAB@  slot T-BR TAB@  slot KEY@ t IR-FUN:ADD-BLOCK-ARG ;

: SET-BLOCK-SPAN ( IR-CTX:ctx IR-BUILD:builder IR-SOURCE:span -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder span:IR-SOURCE:span :}
   c b USE drop
   b SERIAL BK-MINE
   span IR-FUN:SET-BLOCK-SPAN ;

: END-BLOCK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL BK-DONE
   c  slot T-BR TAB@  slot T-FR TAB@  slot KEY@
   slot T-OV TAB@  slot T-OR TAB@  slot T-QR TAB@  slot T-SA TAB@
   IR-FUN:END-BLOCK ;

\ ---- what the builder holds so far -------------------------------------------
\ Live counts, without handing out a table a caller could write through.
: SYMBOLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-SR TAB@ IR-SYM:SYMBOLS ;

: SCHEMAS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-QR TAB@ IR-SCHEMA:SCHEMAS ;

: OP-CELLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-OP TAB@ IR-OP:POOL-CELLS ;

: FUNS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-FR TAB@ IR-FUN:FUNS ;

: BLOCKS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-BR TAB@ IR-FUN:BLOCKS ;

: FUN-CELLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-FP TAB@ IR-FUN:ATTR-CELLS ;

\ ---- what the builder can be asked about the module so far -------------------
\ Readers of a module still being built, for callers (a source tape, a dialect)
\ that cannot wait for FREEZE. Each passes USE and answers with a scalar, an
\ identity or the owning table's own refusal; no arena handle leaves.
\ The copy into a fixed buffer refuses a span smaller than the symbol; a caller
\ reading a program-chosen name asks this first to answer "too long" as a no.
: SYMBOL-LEN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   slot T-SR TAB@ id IR-SYM:LEN@ ;

: SYMBOL-CK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   SYMBOL-LEN drop ;

\ A dialect memo names one exact module. A mismatch grants no use of its
\ ordinals and touches neither caller span. On a hit, the symbol owner checks
\ the current live header and every ordinal before publishing the batch.
: BIND-SYMBOLS? ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-module-id ptr n n ptr IR-ID:ir-symbol-id n -- IR-ID:ir-module-id bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder prior:IR-ID:ir-module-id
      src:ptr n:n dst:ptr cap:n :}
   c b USE {: slot:n :}
   slot BMID @ {: mid:IR-ID:ir-module-id :}
   mid prior IR-ID:MODULE-SAME? 0= if mid false exit then
   slot T-SR TAB@ slot KEY@ src n dst cap IR-SYM:IDS!
   mid true ;

\ Checks an identity without interning: interning absent bytes would append.
: SYMBOL-IS? ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id p u:n :}
   c b USE {: slot:n :}
   slot T-SP TAB@  slot T-SR TAB@  id p u IR-SYM:EQ? ;

: SYMBOL-COPY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id ptr u8 n -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id p cap:n :}
   c b USE {: slot:n :}
   slot T-SP TAB@  slot T-SR TAB@  id p cap IR-SYM:COPY ;

\ The span names a source of this module and lies inside its registered bytes.
: SPAN-CK ( IR-CTX:ctx IR-BUILD:builder IR-SOURCE:span -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder span:IR-SOURCE:span :}
   c b USE {: slot:n :}
   slot T-SA TAB@ span IR-SOURCE:SPAN-CK ;

\ NEW-BUILDER fixes dialect and version; a dialect reading them back turns
\ "create through the dialect" into a check.
: DIALECT@ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   slot T-QR TAB@ slot KEY@ IR-SCHEMA:DIALECT@ ;

: SCHEMA-MAJOR@ ( IR-CTX:ctx IR-BUILD:builder -- n )
   USE T-QR TAB@ IR-SCHEMA:MAJOR@ ;

: SCHEMA-MINOR@ ( IR-CTX:ctx IR-BUILD:builder -- n )
   USE T-QR TAB@ IR-SCHEMA:MINOR@ ;

\ IR-OP:END-OP validates the staged operation against the same rows.
: SCHEMA-OPERANDS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   slot T-QR TAB@ op IR-SCHEMA:OPERANDS ;

: SCHEMA-RESULTS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   slot T-QR TAB@ op IR-SCHEMA:RESULTS ;

: SCHEMA-OPERAND@ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id i:n :}
   c b USE {: slot:n :}
   slot T-QP TAB@  slot T-QR TAB@  slot KEY@  op i IR-SCHEMA:OPERAND@ ;

: SCHEMA-RESULT@ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id i:n :}
   c b USE {: slot:n :}
   slot T-QP TAB@  slot T-QR TAB@  slot KEY@  op i IR-SCHEMA:RESULT@ ;

\ IR-OP mints the results; END-OP returns only the operation.
: OP-RESULT@ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-op-id i:n :}
   c b USE {: slot:n :}
   slot T-OP TAB@  slot T-OR TAB@  slot KEY@ id i IR-OP:RESULT@ ;

: VALUE-TYPE@ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id :}
   c b USE {: slot:n :}
   slot T-OV TAB@  slot KEY@ v IR-OP:VALUE-TYPE@ ;

private

\ ---- freeze validation -------------------------------------------------------
: CEIL-CK ( n n n -- )
   {: slot:n live:n f:n :}
   live 0 < if E-IR-BUILD-STATE throw then
   live slot f BCEIL@ > if E-IR-BUILD-CEILING throw then ;

\ Each owner refuses appends past its ceiling already; this rereads the counts
\ as an independent check that the published prefix is the committed one.
: CEILINGS-CK ( n -- )
   {: slot:n :}
   slot  slot T-SR TAB@ IR-SYM:SYMBOLS      P-SYMS CEIL-CK
   slot  slot T-TR TAB@ IR-TYPE:TYPES       P-TYPES CEIL-CK
   slot  slot T-AR TAB@ IR-ATTR:ATTRS       P-ATTRS CEIL-CK
   slot  slot T-SA TAB@ IR-SOURCE:SOURCES   P-SRCS CEIL-CK
   slot  slot T-QR TAB@ IR-SCHEMA:SCHEMAS   P-SCHEMAS CEIL-CK
   slot  slot T-OR TAB@ IR-OP:OPS           P-OPS CEIL-CK
   slot  slot T-OV TAB@ IR-OP:VALUES        P-VALS CEIL-CK
   slot  slot T-OP TAB@ IR-OP:POOL-CELLS    P-OPOOL CEIL-CK
   slot  slot T-FR TAB@ IR-FUN:FUNS         P-FUNS CEIL-CK
   slot  slot T-BR TAB@ IR-FUN:BLOCKS       P-BLKS CEIL-CK
   slot  slot T-FP TAB@ IR-FUN:ATTR-CELLS   P-FPOOL CEIL-CK ;

\ Checked first so the freezing pass cannot fail part way through.
: TABLES-LIVE-CK ( n -- )
   {: slot:n :}
   TABLES# 0 ?do
      slot i TAB@ IR-ARENA:LIVE? 0= if E-IR-BUILD-STATE throw then
   loop ;

: TABLES-FREEZE ( n -- )
   {: slot:n :}
   TABLES# 0 ?do
      slot i TAB@ IR-ARENA:FREEZE slot i VIEW!
   loop ;

: TABLES-ABORT ( n -- )
   {: slot:n :}
   TABLES# 0 ?do
      slot i TAB@ IR-ARENA:ABORT
   loop ;

: TABLES-RETIRE ( n -- )
   {: slot:n :}
   TABLES# 0 ?do
      slot i VIEW@ IR-ARENA:RETIRE
   loop ;

: PUBLISH-CK ( IR-CTX:ctx IR-BUILD:builder -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   slot BGEN@ STG-CLEAR-CK
   slot CEILINGS-CK
   slot TABLES-LIVE-CK
   slot ;

: PUBLISH-M ( n -- IR-BUILD:module )
   {: slot:n :}
   slot TABLES-FREEZE
   ST-FROZEN slot BSTATE!
   slot BGEN@ MINT-M ;

\ Both freezes hand the verifier the same fourteen tables.
: VVIEW ( n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: slot:n :}
   slot KEY@
   slot T-OP TAB@  slot T-OV TAB@  slot T-OR TAB@
   slot T-FP TAB@  slot T-FR TAB@  slot T-BR TAB@
   slot T-QP TAB@  slot T-QR TAB@  slot T-SR TAB@
   slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   slot T-EP TAB@  slot T-ER TAB@ ;

: VERIFY-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot VVIEW
   IR-VERIFY:VERIFY ;

: DERIVE-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot VVIEW
   IR-VERIFY:DERIVE-EDGES ;

public

\ ---- freeze and abort --------------------------------------------------------
\ A refused freeze changes nothing; the builder stays live and can be fixed and
\ frozen again. FREEZE takes the context to pass USE (a handle escaped into
\ another compilation must not publish) and because validation needs the target
\ contract and an allocator; ABORT and RETIRE allocate nothing and take none.
: FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b PUBLISH-CK {: slot:n :}
   c slot VERIFY-CK
   slot PUBLISH-M ;

\ FREEZE without whole-module verification, for a module a later pass reads and
\ no routine emits. Verifying every interim module re-walked each table up to
\ three times per word. Defects are still refused when VERIFY checks the emitted
\ module; only the localisation to the earlier module is lost.
: FREEZE-INTERIM ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b PUBLISH-CK {: slot:n :}
   c slot DERIVE-CK
   slot PUBLISH-M ;

\ Retiring the tables makes every index and mark they minted stale; the slot
\ records the abort so a later use is named.
: ABORT ( IR-BUILD:builder -- )
   LIVE-SLOT {: slot:n :}
   slot BGEN@ STG-RELEASE
   slot TABLES-ABORT
   ST-ABORTED slot BSTATE! ;

\ For a pass that has copied everything out of the old module. Liveness is the
\ caller's claim; being wrong is loud: the handle gives E-IR-BUILD-RETIRED and
\ any view, reader or index it handed out gives E-IR-ARENA-STALE. Reclaims no
\ storage: that returns with the context's mark.
: RETIRE ( IR-BUILD:module -- )
   FROZEN-SLOT {: slot:n :}
   slot TABLES-RETIRE
   ST-RETIRED slot BSTATE! ;

\ ---- the frozen module -------------------------------------------------------
\ Each view is the table's own frozen reader surface; this package adds none.
: FKEY ( IR-BUILD:module -- IR-ID:ir-module-key )
   FROZEN-SLOT KEY@ ;

: FMODULE ( IR-BUILD:module -- IR-ID:ir-module-id )
   FROZEN-SLOT BMID @ ;

: FSYM-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-SP VIEW@ ;

: FSYM-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-SR VIEW@ ;

: FTYPE-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-TP VIEW@ ;

: FTYPE-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-TR VIEW@ ;

: FATTR-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-AR VIEW@ ;

: FSOURCES ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-SA VIEW@ ;

: FSCHEMA-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-QP VIEW@ ;

: FSCHEMA-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-QR VIEW@ ;

: FOP-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-OP VIEW@ ;

: FVALUE-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-OV VIEW@ ;

: FOP-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-OR VIEW@ ;

: FFUN-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-FR VIEW@ ;

: FBLOCK-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-BR VIEW@ ;

\ Published as a module table so a later pass reads them from the module, not
\ from a cache that could disagree. IR-VERIFY's frozen readers serve them.
: FEDGE-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-EP VIEW@ ;

: FEDGE-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-ER VIEW@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
