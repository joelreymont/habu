\ build.f - the builder and freeze lifecycle: one uniquely owned handle for a
\ module under construction, and the one act that publishes it.
\
\ docs/compiler-ir-design.md section 6.2 (NEW-BUILDER / FREEZE / ABORT are the
\ ownership shape the substrate must already have), section 6.4 (the small
\ common builder API every dialect wraps), and section 6.5 (freeze is where a
\ module is validated before anything is published). This file owns the
\ lifecycle of that construction: who may mutate, when mutation stops, what
\ abandoning costs, and what publication produces. It defines no table format
\ and repeats no table logic; every append here is a delegation to the module
\ that owns the table being appended to.
\
\ WHY THE BUILDER HOLDS THE TABLES. A module under construction is fifteen
\ arenas: the symbol interner's byte pool and rows, the type table's pool and
\ rows, the attribute table's pool and rows, the source registry, the dialect
\ schema table's pool and rows, the operation store's cell pool, value rows and
\ operation rows, and the function store's attribute pool, function rows and
\ block rows. Before this file, a caller assembled those fifteen handles itself
\ and passed the right subset to every append; nothing stopped it from keeping
\ one and writing through it after the module was finished. Here the fifteen
\ handles are created by NEW-BUILDER and never leave this package while the
\ builder is live, so the builder handle is the only way to reach them and
\ therefore the single mutation authority. FREEZE turns each of them into an
\ IR-ARENA read-only view and publishes the views; ABORT retires all fifteen
\ and publishes nothing.
\
\ HANDLES AND HOW THEY STAY HONEST. A builder handle and the frozen module
\ handle it becomes are the same nonzero, monotonic, never-reused generation
\ serial from this package's atomic counter, exactly like IR-CTX and IR-ARENA.
\ A bounded registry keeps one slot per generation with the owning context's
\ serial, the slot's state, the committed ceilings, and the fifteen handles.
\ The handles are stored in typed storage (TYPED-BUFFER over IR-ARENA:arena,
\ IR-ARENA:view and the IR-ID identity families), so this package never re-mints
\ a raw cell into a sealed nominal and adds no forging power to the substrate.
\ The context handle itself is deliberately NOT stored: IR-CTX's rule is that no
\ context handle survives its WITH-CONTEXT body, so every mutating word here
\ takes the context from its caller and checks it against the owner serial the
\ slot recorded. That check is also what rejects a builder used from a foreign
\ or later compilation.
\
\ THE THREE WAYS A HANDLE CAN BE WRONG, AND THEIR THREE NAMES. A slot keeps its
\ generation after the builder stops being usable, so the registry can still say
\ which of the three things happened: E-IR-BUILD-FROZEN for anything asked of a
\ builder FREEZE already consumed (including a second freeze), E-IR-BUILD-ABORTED
\ for anything asked of a builder ABORT retired, and E-IR-BUILD-STALE for a
\ generation this registry has no slot for at all or whose owning context has
\ since torn down. E-IR-BUILD-MODULE separates the two handle families: a frozen
\ module presented where a live builder belongs, or the reverse.
\
\ STAGES ARE PROCESS-WIDE, SO THE BUILDER OWNS THEM. IR-OP and IR-FUN keep one
\ open operation, one open block and one open function per process, not per
\ store, so two live builders could otherwise interleave into each other's
\ staged record. This file records which generation holds each of the three
\ stages; a begin against a stage another builder holds, or an end without a
\ begin, is E-IR-BUILD-STAGE. It is also what makes the freeze refusal below
\ decidable: FREEZE knows whether this builder left a record half-declared.
\
\ FREEZE IS ALL OR NOTHING. Every check runs before the first arena is frozen:
\ the builder must be live and presented with its own context, must own no open
\ stage (design line 544's "no builder-only placeholder"), every table must hold
\ no more records than the ceiling this builder committed to, and all fifteen
\ arenas must still be live. Only then are the arenas frozen, the slot marked
\ frozen and the module handle minted. A refusal therefore leaves the context,
\ the tables and the builder exactly as they were, and the caller may fix the
\ module and freeze again.
\
\ WHAT FREEZE DOES NOT CHECK YET. Design section 6.5 also lists the structural
\ verification of a whole module - dominance, use-before-def across blocks,
\ derived predecessor and successor tables, attribute canonicalisation, span
\ validity against registered source. That verifier is its own owner with its
\ own error block (-8080..-8079's neighbour, package IR-VERIFY, dot
\ habu-verify-frozen-compiler-224d78ad); it is not silently skipped here and it
\ is not duplicated here. When it lands it becomes one more refusal arm of
\ FREEZE, in front of the arena freezing, and nothing else about this file
\ changes. The per-record structural rules are already enforced where the record
\ is appended: IR-OP:END-OP, IR-FUN:END-FUN and IR-FUN:END-BLOCK each validate
\ their record whole against the schema, the windows and the ceilings.
\
\ CAPACITY. Fifteen arenas per module and IR-ARENA's sixty-four registry slots
\ mean at most four modules - builders, frozen modules, or a mix - can be live
\ at one time. ABORT frees its fifteen slots at once and IR-ARENA reclaims the
\ slots of any module whose context tore down, so the limit is on modules alive
\ together, not on modules built in sequence. Raising it is IR-ARENA's own
\ capacity decision, not a side effect of this file.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
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

NEWTYPE builder 0
NEWTYPE module 0

private

CAST: MINT-B ( n -- IR-BUILD:builder ) ;
CAST: B>N ( IR-BUILD:builder -- n ) ;
CAST: MINT-M ( n -- IR-BUILD:module ) ;
CAST: M>N ( IR-BUILD:module -- n ) ;

\ ---- capacities and slot states ----------------------------------------------
16 constant SLOT-MAX                 \ live + frozen + aborted registry slots
$7FFFFFFF constant BGEN-MAX          \ builder generation ceiling
1 constant ST-LIVE
2 constant ST-FROZEN
3 constant ST-ABORTED
0 constant STG-FREE                  \ no builder holds this stage

\ ---- the fifteen tables a module is made of ----------------------------------
0 constant T-SP                      \ symbol interner byte pool
1 constant T-SR                      \ symbol interner rows
2 constant T-TP                      \ type table pool
3 constant T-TR                      \ type table rows
4 constant T-AP                      \ attribute table pool
5 constant T-AR                      \ attribute table rows
6 constant T-SA                      \ source registry rows
7 constant T-QP                      \ dialect schema pool
8 constant T-QR                      \ dialect schema rows
9 constant T-OP                      \ operation cell pool
10 constant T-OV                     \ value rows
11 constant T-OR                     \ operation rows
12 constant T-FP                     \ function attribute pool
13 constant T-FR                     \ function rows
14 constant T-BR                     \ block rows
15 constant T-EP                     \ derived predecessor pool
16 constant T-ER                     \ derived block-edge rows
17 constant TABLES#

\ ---- the committed ceiling plan ----------------------------------------------
\ One field per number the fifteen tables are created with. A plan is staged by
\ name before NEW-BUILDER so no caller has to remember fifteen positional
\ numbers, and NEW-BUILDER consumes it.
0 constant P-SYMS                    \ symbol rows
1 constant P-SBYTES                  \ symbol pool bytes
2 constant P-TYPES
3 constant P-TPOOL
4 constant P-ATTRS
5 constant P-APOOL
6 constant P-SRCS
7 constant P-SCHEMAS
8 constant P-QPOOL
9 constant P-OPS
10 constant P-VALS
11 constant P-OPOOL
12 constant P-FUNS
13 constant P-BLKS
14 constant P-FPOOL
15 constant PLAN#
-1 constant UNSET

\ The production plan. Ceilings are commitments, not allocations: IR-ARENA
\ seeds every table small and grows it geometrically only as far as it is used,
\ so a generous ceiling costs nothing until the module needs it. The real limit
\ is the owning context's mapping, which every table shares.
256 constant D-SYMS
4096 constant D-SBYTES
128 constant D-TYPES
1024 constant D-TPOOL
128 constant D-ATTRS
1024 constant D-APOOL
64 constant D-SRCS
128 constant D-SCHEMAS
1024 constant D-QPOOL
512 constant D-OPS
512 constant D-VALS
4096 constant D-OPOOL
64 constant D-FUNS
256 constant D-BLKS
1024 constant D-FPOOL

\ ---- registry storage --------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable BGEN-CELL
0 BGEN-CELL !
create BGENS SLOT-MAX cells allot
create BOWNERS SLOT-MAX cells allot
create BSTATES SLOT-MAX cells allot
create BCEILS SLOT-MAX PLAN# * cells allot
SLOT-MAX TABLES# * TYPED-BUFFER BTAB IR-ARENA:arena
SLOT-MAX TABLES# * TYPED-BUFFER BVIEW IR-ARENA:view
SLOT-MAX TYPED-BUFFER BKEY IR-ID:ir-module-key
SLOT-MAX TYPED-BUFFER BMID IR-ID:ir-module-id

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

: CEIL-IDX ( n n -- n )
   {: slot:n f:n :}
   slot PLAN# * f + ;

: BCEIL@ ( n n -- n )
   CEIL-IDX cells BCEILS + @ ;

: BCEIL! ( n n n -- )
   CEIL-IDX cells BCEILS + ! ;

: TAB@ ( n n -- IR-ARENA:arena )
   {: slot:n k:n :}
   slot TABLES# * k + BTAB @ ;

: TAB! ( IR-ARENA:arena n n -- )
   {: slot:n k:n :}
   slot TABLES# * k + BTAB ! ;

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

: SLOTS-CLEAR ( -- )
   SLOT-MAX 0 ?do
      0 i BGEN!
   loop ;
SLOTS-CLEAR

\ ---- generation serials ------------------------------------------------------
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

\ ---- handle resolution -------------------------------------------------------
: FIND-B ( n -- n )
   {: g:n :}
   -1
   SLOT-MAX 0 ?do
      g i BGEN@ = if drop i leave then
   loop ;

\ Resolve a generation to its slot. A generation this registry never minted, and
\ a slot whose owning context has torn down, are both stale; the dead-owner slot
\ is retired on touch so its handles are never read afterwards.
: RESOLVE ( n -- n )
   FIND-B
   dup 0 < if E-IR-BUILD-STALE throw then
   dup BOWNER@ IR-CTX:SERIAL-LIVE? 0= if
      0 over BGEN! E-IR-BUILD-STALE throw
   then ;

: STATE-OK ( n -- n )
   dup BSTATE@ {: st:n :}
   st ST-LIVE = st ST-FROZEN = or st ST-ABORTED = or 0= if
      E-IR-BUILD-STATE throw
   then ;

\ A live builder: the state tells the caller exactly how the handle went bad.
: LIVE-SLOT ( IR-BUILD:builder -- n )
   B>N RESOLVE STATE-OK
   dup BSTATE@ ST-ABORTED = if E-IR-BUILD-ABORTED throw then
   dup BSTATE@ ST-LIVE <> if E-IR-BUILD-FROZEN throw then ;

\ A module handle is minted only by FREEZE, and a frozen slot never changes
\ state again, so anything but ST-FROZEN here is corrupted registry state
\ rather than a caller mistake.
: FROZEN-SLOT ( IR-BUILD:module -- n )
   M>N RESOLVE STATE-OK
   dup BSTATE@ ST-FROZEN <> if E-IR-BUILD-STATE throw then ;

: OWN-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c IR-CTX:SERIAL slot BOWNER@ <> if E-IR-BUILD-OWNER throw then ;

\ The mutation gate every append passes through: this builder is live and this
\ context is the one that created it.
: USE ( IR-CTX:ctx IR-BUILD:builder -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b LIVE-SLOT {: slot:n :}
   c slot OWN-CK
   slot ;

\ ---- stage ownership ---------------------------------------------------------
\ IR-OP and IR-FUN keep one open record each per process. These three cells say
\ which builder generation holds each of them, so two builders cannot interleave
\ and FREEZE can see a half-declared record. A cell that names a generation is
\ the promise that the authority behind it really has that record open: the
\ claim is recorded only after the authority's own begin returned, and it is
\ given back before or with the end that consumes it.
variable FN-STG
STG-FREE FN-STG !
variable BK-STG
STG-FREE BK-STG !
variable OP-STG
STG-FREE OP-STG !

\ A builder that can still finish the record it claimed.
: HOLDER-LIVE? ( n -- bool )
   {: g:n :}
   g FIND-B {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot BOWNER@ IR-CTX:SERIAL-LIVE?
   slot BSTATE@ ST-LIVE = and ;

\ A claim whose holder is gone - a builder abandoned by a throw, or one whose
\ context tore down - is reclaimed before the next builder is refused for it,
\ and the authority's orphaned record is abandoned with it. Without this a
\ single failed construction would keep every later builder out of that stage
\ for the rest of the process.
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

\ The three checks a claim needs: the stage is free to take, the stage is this
\ builder's, and the claim is recorded or given back.
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

\ Design line 544: a module that still has a record open carries a builder-only
\ placeholder, so it cannot be published.
: STG-CLEAR-CK ( n -- )
   {: g:n :}
   FN-STG @ g = BK-STG @ g = or OP-STG @ g = or
   if E-IR-BUILD-OPEN throw then ;

\ Abandoning gives every stage this builder holds back to the authority that
\ owns it, innermost first, so no staged field survives into the next builder.
: STG-RELEASE ( n -- )
   {: g:n :}
   OP-STG @ g = if IR-OP:ABANDON STG-FREE OP-STG ! then
   BK-STG @ g = if IR-FUN:ABANDON-BLOCK STG-FREE BK-STG ! then
   FN-STG @ g = if IR-FUN:ABANDON-FUN STG-FREE FN-STG ! then ;

\ ---- the ceiling plan --------------------------------------------------------
create PSTG PLAN# cells allot
variable PSTG-OPEN
0 PSTG-OPEN !

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

\ Consume the staged plan into one slot's committed ceilings. A field nobody
\ declared is a plan error, not a silent default.
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
\ PLAN-BEGIN opens a fresh plan; each setter names the table it bounds; every
\ field must be declared exactly once before NEW-BUILDER consumes the plan.
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

: PLAN-ATTRS ( n n -- )
   {: rows:n pool:n :}
   rows P-ATTRS PLAN-SET
   pool P-APOOL PLAN-SET ;

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

\ The production plan, for callers that want the committed defaults rather than
\ a hand-sized module.
: PLAN-DEFAULT ( -- )
   PLAN-BEGIN
   D-SYMS D-SBYTES PLAN-SYMBOLS
   D-TYPES D-TPOOL PLAN-TYPES
   D-ATTRS D-APOOL PLAN-ATTRS
   D-SRCS PLAN-SOURCES
   D-SCHEMAS D-QPOOL PLAN-SCHEMAS
   D-OPS D-VALS D-OPOOL PLAN-OPS
   D-FUNS D-BLKS D-FPOOL PLAN-FUNS ;

private

\ ---- creation ----------------------------------------------------------------
\ Retire every slot whose owning context has torn down; the arenas it named are
\ already unmapped and its generation can never resolve again.
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
   dup 0 < if E-IR-BUILD-SLOTS throw then ;

: SYM-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-SYMS BCEIL@ slot P-SBYTES BCEIL@ IR-SYM:NEW
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
   c slot KEY@ slot P-ATTRS BCEIL@ slot P-APOOL BCEIL@ IR-ATTR:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-AP TAB!
   r slot T-AR TAB! ;

: SRC-TABLE ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-SRCS BCEIL@ IR-SOURCE:NEW slot T-SA TAB! ;

\ The dialect's name is interned into this module's own symbol table, so the
\ schema table names its dialect the way every other record names a symbol.
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

\ The derived block-edge table the freeze verifier fills. It is created here with
\ the module's other tables so verification allocates nothing of its own at
\ freeze time, and its ceilings are read off the same committed plan: one row per
\ block, and one predecessor cell per operand pool cell, which bounds the edges
\ because every successor a terminator names occupies one of those cells.
: EDGE-TABLES ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c slot KEY@ slot P-BLKS BCEIL@ slot P-OPOOL BCEIL@ IR-VERIFY:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p slot T-EP TAB!
   r slot T-ER TAB! ;

\ Build all fifteen tables into one slot. The symbol interner comes first
\ because the dialect name is a symbol of this module, and the schema table
\ cannot be created without it.
: TABLES-BUILD ( IR-CTX:ctx n ptr u8 n n n -- )
   {: c:IR-CTX:ctx slot:n p u:n major:n minor:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c slot SYM-TABLES
   c slot TYPE-TABLES
   c slot ATTR-TABLES
   c slot SRC-TABLE
   c  slot T-SP TAB@  slot T-SR TAB@  slot KEY@  p u IR-SYM:INTERN {: dia:IR-ID:ir-symbol-id :}
   c slot dia major minor SCHEMA-TABLES
   c slot OP-TABLES
   c slot FUN-TABLES
   c slot EDGE-TABLES ;

public

\ Create a uniquely owned builder for one new module of ctx, whose dialect is
\ named by the presented bytes at the presented schema version. The staged plan
\ is consumed here and becomes this module's committed ceilings. The module
\ identity is minted from the context, so it counts against the context's own
\ module ceiling; the generation is installed last, so a failure anywhere in
\ table creation leaves no half-installed slot behind.
: NEW-BUILDER ( IR-CTX:ctx ptr u8 n n n -- IR-BUILD:builder )
   {: c:IR-CTX:ctx p u:n major:n minor:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   PLAN-OPEN-CK
   SWEEP
   FREE-SLOT {: slot:n :}
   TAKE-BGEN {: g:n :}
   slot PLAN-TAKE
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   key slot KEY!
   mid slot BMID !
   c slot p u major minor TABLES-BUILD
   c IR-CTX:SERIAL slot BOWNER!
   ST-LIVE slot BSTATE!
   g slot BGEN!
   g MINT-B ;

\ ---- identity ----------------------------------------------------------------
: SERIAL ( IR-BUILD:builder -- n )
   B>N ;

: LIVE? ( IR-BUILD:builder -- bool )
   B>N FIND-B {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot BOWNER@ IR-CTX:SERIAL-LIVE?
   slot BSTATE@ ST-LIVE = and ;

: FROZEN? ( IR-BUILD:module -- bool )
   M>N FIND-B {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot BOWNER@ IR-CTX:SERIAL-LIVE?
   slot BSTATE@ ST-FROZEN = and ;

: MODULE@ ( IR-BUILD:builder -- IR-ID:ir-module-id )
   LIVE-SLOT BMID @ ;

\ The module key this builder mints identities under. Dialect packages need it
\ to name the identities they read back; it confers no mutation power on its own.
: MODULE-KEY ( IR-BUILD:builder -- IR-ID:ir-module-key )
   LIVE-SLOT KEY@ ;

\ ---- interning: symbols, sources, and spans (design lines 517-520) ------------
: INTERN-SYMBOL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c b USE {: slot:n :}
   c  slot T-SP TAB@  slot T-SR TAB@  slot KEY@  p u IR-SYM:INTERN ;

: ADD-SOURCE ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c b USE {: slot:n :}
   c slot T-SA TAB@ slot KEY@ p u IR-SOURCE:REGISTER ;

: ADD-SOURCE-FROM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-source-id ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder parent:IR-ID:ir-source-id p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c b USE {: slot:n :}
   c slot T-SA TAB@ slot KEY@ parent p u IR-SOURCE:REGISTER-FROM ;

: ADD-SPAN ( IR-BUILD:builder IR-ID:ir-source-id n n -- IR-SOURCE:span )
   {: b:IR-BUILD:builder src:IR-ID:ir-source-id st:n ln:n :}
   b LIVE-SLOT T-SA TAB@ src st ln IR-SOURCE:SPAN ;

\ ---- interning: types (design line 517) --------------------------------------
private

: TYPE-USE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-TP TAB@  slot T-TR TAB@  slot KEY@ ;

public

: INTERN-INT ( IR-CTX:ctx IR-BUILD:builder IR-TYPE:width IR-TYPE:sign -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder w:IR-TYPE:width s:IR-TYPE:sign :}
   c b TYPE-USE w s IR-TYPE:INT ;

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

\ ---- interning: attributes (design line 519) ---------------------------------
private

: ATTR-USE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-AP TAB@  slot T-AR TAB@  slot KEY@ ;

public

: INTERN-INT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n :}
   c b ATTR-USE v IR-ATTR:INT ;

: INTERN-BOOL-ATTR ( IR-CTX:ctx IR-BUILD:builder bool -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:bool :}
   c b ATTR-USE v IR-ATTR:BOOLEAN ;

: INTERN-TEXT-ATTR ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c b ATTR-USE p u IR-ATTR:TEXT ;

: INTERN-SYMBOL-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-symbol-id :}
   c b USE {: slot:n :}
   c  slot T-AP TAB@  slot T-AR TAB@  slot KEY@  slot T-SR TAB@ s IR-ATTR:SYMBOL ;

: INTERN-TYPE-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b USE {: slot:n :}
   c  slot T-AP TAB@  slot T-AR TAB@  slot KEY@  slot T-TR TAB@ t IR-ATTR:TYPE-REF ;

\ The digest is four cells, so it is unmade to reach the builder underneath it
\ and remade unchanged for the interner.
: INTERN-DIGEST-ATTR ( IR-CTX:ctx IR-BUILD:builder CDIGEST:digest -- IR-ID:ir-attr-id )
   CDIGEST-DIGEST:UNMAKE
   {: c:IR-CTX:ctx b:IR-BUILD:builder w0:n w1:n w2:n w3:n :}
   c b ATTR-USE  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE  IR-ATTR:DIGEST ;

: INTERN-OVERFLOW-ATTR ( IR-CTX:ctx IR-BUILD:builder CNUM:overflow -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CNUM:overflow :}
   c b ATTR-USE v IR-ATTR:OVERFLOW ;

: INTERN-FLOAT-MODEL-ATTR ( IR-CTX:ctx IR-BUILD:builder CNUM:float-model -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CNUM:float-model :}
   c b ATTR-USE v IR-ATTR:FLOAT-MODEL ;

: INTERN-FAST-MATH-ATTR ( IR-CTX:ctx IR-BUILD:builder CNUM:fast-math -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CNUM:fast-math :}
   c b ATTR-USE v IR-ATTR:FAST-MATH ;

: INTERN-CONTRACTION-ATTR ( IR-CTX:ctx IR-BUILD:builder CNUM:contraction -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CNUM:contraction :}
   c b ATTR-USE v IR-ATTR:CONTRACTION ;

: INTERN-COMPARE-ATTR ( IR-CTX:ctx IR-BUILD:builder CNUM:compare -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CNUM:compare :}
   c b ATTR-USE v IR-ATTR:COMPARE ;

: INTERN-ARCH-ATTR ( IR-CTX:ctx IR-BUILD:builder CTARGET:arch -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CTARGET:arch :}
   c b ATTR-USE v IR-ATTR:ARCH ;

: INTERN-ABI-ATTR ( IR-CTX:ctx IR-BUILD:builder CTARGET:abi -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CTARGET:abi :}
   c b ATTR-USE v IR-ATTR:ABI ;

: INTERN-ENDIAN-ATTR ( IR-CTX:ctx IR-BUILD:builder CTARGET:endian -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CTARGET:endian :}
   c b ATTR-USE v IR-ATTR:ENDIAN ;

: INTERN-PTR-WIDTH-ATTR ( IR-CTX:ctx IR-BUILD:builder CTARGET:ptr-width -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:CTARGET:ptr-width :}
   c b ATTR-USE v IR-ATTR:PTR-WIDTH ;

: INTERN-INT-LIST-ATTR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   ATTR-USE IR-ATTR:INT-LIST ;

: INTERN-RECORD-ATTR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-AP TAB@  slot T-AR TAB@  slot KEY@  slot T-SR TAB@ IR-ATTR:RECORD ;

\ ---- the dialect schema table ------------------------------------------------
\ The staged opcode schema is closed against this builder's own tables. The
\ IR-SCHEMA staging words (BEGIN-OP, ADD-OPERAND, SET-PURE and the rest) take
\ identities only and need no table, so they stay where they are defined.
: DEFINE-OP ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   c  slot T-QP TAB@  slot T-QR TAB@  slot KEY@  slot T-SR TAB@  slot T-TR TAB@
   IR-SCHEMA:DEFINE ;

\ ---- the operation builder (design lines 507-514) ----------------------------
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

\ One attribute this operation carries, under the key it answers: the schema
\ declares keys, so an operation has to name one (design line 479).
: ADD-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-symbol-id a:IR-ID:ir-attr-id :}
   c b USE drop
   b SERIAL OP-MINE
   k a IR-OP:ADD-ATTR ;

: SET-OP-SPAN ( IR-CTX:ctx IR-BUILD:builder IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: c:IR-CTX:ctx b:IR-BUILD:builder src:IR-ID:ir-source-id st:n ln:n :}
   c b USE drop
   b SERIAL OP-MINE
   src st ln IR--SOURCE-SPAN:MAKE IR-OP:SET-SPAN ;

\ Close the staged operation into this module's operation store. The stage is
\ given back before IR-OP runs, so a rejected append leaves no stage behind for
\ the next one either way.
: END-OP ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL OP-DONE
   c  slot T-OP TAB@  slot T-OV TAB@  slot T-OR TAB@  slot KEY@
   slot T-QR TAB@  slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   IR-OP:END-OP ;

: ABANDON-OP ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE drop
   b SERIAL OP-DONE
   IR-OP:ABANDON ;

\ ---- the function builder (design lines 498-499) -----------------------------
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
   IR--SOURCE-SPAN:UNMAKE
   {: c:IR-CTX:ctx b:IR-BUILD:builder src:IR-ID:ir-source-id st:n ln:n :}
   c b USE drop
   b SERIAL FN-MINE
   src st ln IR--SOURCE-SPAN:MAKE IR-FUN:SET-FUN-SPAN ;

: END-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL FN-DONE
   c  slot T-FP TAB@  slot T-FR TAB@  slot T-BR TAB@  slot KEY@
   slot T-SR TAB@  slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   IR-FUN:END-FUN ;

: ABANDON-FUN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE drop
   b SERIAL FN-DONE
   IR-FUN:ABANDON-FUN ;

\ ---- the block builder (design lines 501-503) --------------------------------
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
   IR--SOURCE-SPAN:UNMAKE
   {: c:IR-CTX:ctx b:IR-BUILD:builder src:IR-ID:ir-source-id st:n ln:n :}
   c b USE drop
   b SERIAL BK-MINE
   src st ln IR--SOURCE-SPAN:MAKE IR-FUN:SET-BLOCK-SPAN ;

: END-BLOCK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   b SERIAL BK-DONE
   c  slot T-BR TAB@  slot T-FR TAB@  slot KEY@
   slot T-OV TAB@  slot T-OR TAB@  slot T-QR TAB@  slot T-SA TAB@
   IR-FUN:END-BLOCK ;

: ABANDON-BLOCK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE drop
   b SERIAL BK-DONE
   IR-FUN:ABANDON-BLOCK ;

\ ---- what the builder holds so far -------------------------------------------
\ Live counts, so a caller can see the committed prefix without being handed a
\ table it could write through.
: SYMBOLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-SR TAB@ IR-SYM:SYMBOLS ;

: TYPES ( IR-BUILD:builder -- n )
   LIVE-SLOT T-TR TAB@ IR-TYPE:TYPES ;

: ATTRS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-AR TAB@ IR-ATTR:ATTRS ;

: SOURCES ( IR-BUILD:builder -- n )
   LIVE-SLOT T-SA TAB@ IR-SOURCE:SOURCES ;

: SCHEMAS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-QR TAB@ IR-SCHEMA:SCHEMAS ;

: OPS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-OR TAB@ IR-OP:OPS ;

: VALUES ( IR-BUILD:builder -- n )
   LIVE-SLOT T-OV TAB@ IR-OP:VALUES ;

: OP-CELLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-OP TAB@ IR-OP:POOL-CELLS ;

: FUNS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-FR TAB@ IR-FUN:FUNS ;

: BLOCKS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-BR TAB@ IR-FUN:BLOCKS ;

: FUN-CELLS ( IR-BUILD:builder -- n )
   LIVE-SLOT T-FP TAB@ IR-FUN:ATTR-CELLS ;

private

\ ---- freeze validation -------------------------------------------------------
: CEIL-CK ( n n n -- )
   {: slot:n live:n f:n :}
   live 0 < if E-IR-BUILD-STATE throw then
   live slot f BCEIL@ > if E-IR-BUILD-CEILING throw then ;

\ Design line 380's committed ceilings, reread from the tables themselves. Each
\ authority already refuses an append past its ceiling; reading the counts back
\ here is the independent check that what will be published is exactly the
\ prefix this builder committed to.
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

\ Every table must still be a live builder arena before the first one is frozen,
\ so the freezing pass below cannot fail part way through.
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

\ Design section 6.5's whole-module verification, run as the last refusal arm
\ before any table is frozen. The verifier is its own authority: this file hands
\ it the module's tables and its own derived table and repeats none of its logic.
: VERIFY-CK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx slot:n :}
   c  slot KEY@
   slot T-OP TAB@  slot T-OV TAB@  slot T-OR TAB@
   slot T-FP TAB@  slot T-FR TAB@  slot T-BR TAB@
   slot T-QP TAB@  slot T-QR TAB@  slot T-SR TAB@
   slot T-TR TAB@  slot T-AR TAB@  slot T-SA TAB@
   slot T-EP TAB@  slot T-ER TAB@
   IR-VERIFY:VERIFY ;

public

\ ---- freeze and abort (design lines 505-507) ---------------------------------
\ Publish the module. Every refusal arm runs before the first table is frozen,
\ so a refused freeze changes nothing at all: the context keeps its scratch and
\ its module count, the tables keep their contents, and the builder stays live
\ and can be corrected and frozen again. On success the fifteen tables become
\ read-only views owned by the context, the slot stops being a builder, and
\ every mutation word rejects the old handle with E-IR-BUILD-FROZEN.
\
\ FREEZE TAKES THE CONTEXT, ABORT DOES NOT. Publication is the one state change
\ this package makes that has to prove the caller owns the compilation, so it
\ passes the same USE gate every append does rather than merely checking that
\ the builder is live: a builder handle that escaped into another compilation
\ could otherwise publish a module its context never agreed to. The context is
\ also what section 6.5's validation needs - the bound target contract, and an
\ allocator for the tables that validation derives - so the argument is the
\ interface that check requires, not a courtesy. ABORT publishes nothing and
\ allocates nothing: retiring an arena needs no allocator, so it keeps the
\ narrower signature.
: FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b USE {: slot:n :}
   slot BGEN@ STG-CLEAR-CK
   slot CEILINGS-CK
   slot TABLES-LIVE-CK
   c slot VERIFY-CK
   slot TABLES-FREEZE
   ST-FROZEN slot BSTATE!
   slot BGEN@ MINT-M ;

\ Give the module up. Every stage this builder holds goes back to its authority,
\ all fifteen tables are retired at once - which releases their registry slots
\ and makes every index and mark they minted stale - and the slot records that
\ it was aborted, so a later use of the handle is named rather than merely
\ unknown. The abandoned spans die with the owning context's mapping, which is
\ the same discipline IR-ARENA's own ABORT records.
: ABORT ( IR-BUILD:builder -- )
   LIVE-SLOT {: slot:n :}
   slot BGEN@ STG-RELEASE
   slot TABLES-ABORT
   ST-ABORTED slot BSTATE! ;

\ ---- the frozen module -------------------------------------------------------
\ The views the later read-only passes read through. Each is the table's own
\ frozen reader surface; this package publishes the view and no reader of its
\ own, so there is exactly one authority per table on both sides of the freeze.
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

: FATTR-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-AP VIEW@ ;

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

: FFUN-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-FP VIEW@ ;

: FFUN-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-FR VIEW@ ;

: FBLOCK-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-BR VIEW@ ;

\ The predecessor and successor facts the freeze verifier derived. They are
\ published as a module table rather than returned to whoever called FREEZE,
\ because a later pass needs them from the module it was handed and not from a
\ cache that could disagree with it. IR-VERIFY's frozen readers serve them.
: FEDGE-POOL ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-EP VIEW@ ;

: FEDGE-ROWS ( IR-BUILD:module -- IR-ARENA:view )
   FROZEN-SLOT T-ER VIEW@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
