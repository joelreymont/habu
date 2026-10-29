\ schema.f - the closed-world operation schema table: one row per opcode, the
\ single authority on an operation's shape (arity rules, control flow, effect,
\ trap, target needs, ties, fixed registers). IR-OP checks each append against
\ it and IR-VERIFY the frozen module; dialect packages fill their own tables.
\ One table is one dialect: the row header holds the dialect symbol and schema
\ version, and a row is named by the module's interned opcode symbol.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f

package IR-SCHEMA
public

\ Pure carries no effect token; every other class uses one in exactly one domain.
ENUM effect DERIVE eq
   pure
   read
   write
   read-write
;ENUM

\ `unaliased` reaches only locations no other operation can reach, `grouped`
\ only its alias group's, `unrestricted` any location in its memory space.
ENUM alias DERIVE eq
   unaliased
   grouped
   unrestricted
;ENUM

ENUM side DERIVE eq
   operand
   result
;ENUM

private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

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
3 constant HC-DIALECT                \ this table's dialect
4 constant HC-MAJOR                  \ schema major version
5 constant HC-MINOR                  \ schema minor version
6 constant RHDR-CELLS

0 constant OFF-NAME                  \ the opcode name
1 constant OFF-OPST                  \ the operand type list
2 constant OFF-OPN
3 constant OFF-OPTAIL
4 constant OFF-RSST                  \ the result type list
5 constant OFF-RSN
6 constant OFF-RSTAIL
7 constant OFF-SUCC                  \ the successor count
8 constant OFF-REGN                  \ the region count
9 constant OFF-ATST                  \ the required attribute keys
10 constant OFF-ATN
11 constant OFF-ATEXT
12 constant OFF-EFF                  \ the effect class
13 constant OFF-DOM                  \ the effect domain
14 constant OFF-SPC                  \ the memory space
15 constant OFF-ALI                  \ the alias behavior
16 constant OFF-TRAP                 \ the may-trap flag
17 constant OFF-ARCH                 \ the required architecture
18 constant OFF-FEAT                 \ the required features
19 constant OFF-TERM                 \ the terminator flag
20 constant OFF-RULE                 \ the semantic rule identifier
21 constant OFF-REND                 \ the renderer identifier
22 constant OFF-TIST                 \ the tied-operand list
23 constant OFF-TIN
24 constant OFF-FXST                 \ the fixed-register list
25 constant OFF-FXN
26 constant ROW-CELLS

2 constant TIE-PAIR
0 constant TIE-RS                    \ the result half of a stored pair
1 constant TIE-OP                    \ the operand half

: TIE-CELLS ( n -- n )
   TIE-PAIR * ;

3 constant FIX-TRIPLE
0 constant FIX-SIDE                  \ the side code of a stored triple
1 constant FIX-ORD                   \ the ordinal on that side
2 constant FIX-REG                   \ the register the field is fixed to

: FIX-CELLS ( n -- n )
   FIX-TRIPLE * ;

public
256 constant CAP-MAX                 \ committed opcodes per dialect table
$FFFFFFFF PHDR-CELLS - constant POOL-MAX
private
32 constant ARITY-MAX                \ committed per-list stage ceiling
32 constant SUCC-MAX                 \ committed successors per terminator
8 constant REGION-MAX                \ committed regions per operation
$FFFF constant VERSION-MAX           \ committed schema major/minor ceiling
-1 constant UNUSED                   \ a field this schema's effect shape does not carry

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump.
0 constant EF-PURE
1 constant EF-READ
2 constant EF-WRITE
3 constant EF-RW

0 constant AL-UNALIASED
1 constant AL-GROUPED
2 constant AL-UNRESTRICTED

0 constant FX-OPERAND
1 constant FX-RESULT

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
2 constant AR-A32
3 constant AR-THUMB2
4 constant AR-C66X
5 constant AR-X86-64
6 constant AR-WASM

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

: SIDE-CODE ( IR-SCHEMA:side -- n )
   MATCH side
      operand OF FX-OPERAND ENDOF
      result  OF FX-RESULT ENDOF
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
      a32     OF AR-A32 ENDOF
      thumb2  OF AR-THUMB2 ENDOF
      c66x    OF AR-C66X ENDOF
      x86-64  OF AR-X86-64 ENDOF
      wasm    OF AR-WASM ENDOF
   ;MATCH ;

\ ---- wire-code decoders ------------------------------------------------------
\ A stored code outside a family's vocabulary is a forged row and is refused.
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

: N>SIDE ( n -- IR-SCHEMA:side )
   case
      FX-OPERAND of IR--SCHEMA-SIDE:OPERAND endof
      FX-RESULT  of IR--SCHEMA-SIDE:RESULT endof
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
      AR-A32     of CTARGET-ARCH:A32 endof
      AR-THUMB2  of CTARGET-ARCH:THUMB2 endof
      AR-C66X    of CTARGET-ARCH:C66X endof
      AR-X86-64  of CTARGET-ARCH:X86-64 endof
      AR-WASM    of CTARGET-ARCH:WASM endof
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
\ Each store is resolved once at the public word into a reader, which refuses
\ the wrong live/frozen state itself.

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

: PHDR-CK ( IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader :}
   pr IR-ARENA:RD-SIZE PSHAPE-CK
   pr PC-MAGIC IR-ARENA:RD@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:reader -- )
   {: rr:IR-ARENA:reader :}
   rr IR-ARENA:RD-SIZE RSHAPE-CK
   rr HC-MAGIC IR-ARENA:RD@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   RHDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE USED>CNT ;

: PCELLS ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE PHDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-SCHEMA-OWNER throw then ;

\ A cross-module pairing rejects before a row window is trusted against the pool.
: PAIR-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr PHDR-CK
   rr RHDR-CK
   pr PC-SERIAL IR-ARENA:RD@ rr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   pr rr PAIR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: RKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

\ ---- symbol and type projections ---------------------------------------------
: SYM-ORD ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-LOCAL ;

: SYM-OWNER ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

: TYP-ORD ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-LOCAL ;

: TYP-OWNER ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

\ Non-negative only: the owning table revalidates the id it is handed.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-SCHEMA-STATE throw then ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * RHDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

: PC@ ( IR-ARENA:reader n -- n )
   PHDR-CELLS + IR-ARENA:RD@ ;

\ Revalidated on every access, so a forged row cannot read past the pool.
: WIN-CK-N ( n n n -- )
   {: pc:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SCHEMA-STATE throw then
   st ln + pc > if E-IR-SCHEMA-STATE throw then ;

\ ---- opcode resolution -------------------------------------------------------
\ Every reader reaches its row through this walk and every wire-code decoder is
\ exact, so no reader answers for an opcode the table never defined, forged rows
\ included. IR-ARENA:RD-FIND validates the reader once for the whole name column
\ instead of once per row.
: SCAN-NAME ( IR-ARENA:reader n -- n )
   {: rr:IR-ARENA:reader ord:n :}
   rr  RHDR-CELLS OFF-NAME +  ROW-CELLS  rr CNT  ord  IR-ARENA:RD-FIND ;

: ROW-OF ( IR-ARENA:reader IR-ID:ir-symbol-id -- n )
   {: rr:IR-ARENA:reader op:IR-ID:ir-symbol-id :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ op SYM-OWNER SERIAL-CK
   rr op SYM-ORD SCAN-NAME
   dup 0 < if E-IR-SCHEMA-OPCODE throw then ;

\ ---- effect-shape projections ------------------------------------------------
: SHAPE-CK ( bool -- )
   0= if E-IR-SCHEMA-KIND throw then ;

: HAS-DOM? ( n -- bool )
   EF-PURE <> ;

: HAS-MEM? ( n n -- bool )
   {: eff:n dom:n :}
   eff HAS-DOM? if dom DM-DATA = else false then ;

\ ---- the staged schema -------------------------------------------------------
\ Each single-valued field starts UNSET and refuses a second declaration, so
\ DEFINE can tell "never declared" from "declared as zero".
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

\ The five staged lists share arrays addressed by list index and segment base,
\ because a Habu word cannot take a storage array as an argument. The second
\ array holds the owning module serial for the three identity lists, the tied
\ operand ordinal for ties, and the ordinal for fixed entries; the third array
\ is the fixed list's register.
5 constant LIST#
0 constant L-OP
1 constant L-RS
2 constant L-AT
3 constant L-TI
4 constant L-FX

0 CWORK-VARIABLE STG-MODE n
0 CWORK-VARIABLE STG-NAME n
0 CWORK-VARIABLE STG-NAMEO n
0 CWORK-VARIABLE STG-ATEXT n
0 CWORK-VARIABLE STG-EFF n
0 CWORK-VARIABLE STG-DOM n
0 CWORK-VARIABLE STG-SPC n
0 CWORK-VARIABLE STG-ALI n
0 CWORK-VARIABLE STG-TRAP n
0 CWORK-VARIABLE STG-CTRL n
0 CWORK-VARIABLE STG-TERM n
0 CWORK-VARIABLE STG-SUCC n
0 CWORK-VARIABLE STG-REGN n
0 CWORK-VARIABLE STG-ARCH n
0 CWORK-VARIABLE STG-FEAT n
0 CWORK-VARIABLE STG-RULE n
0 CWORK-VARIABLE STG-RULEO n
0 CWORK-VARIABLE STG-REND n
0 CWORK-VARIABLE STG-RENDO n
LIST# ARITY-MAX * CWORK-SPAN STG-V n
LIST# ARITY-MAX * CWORK-SPAN STG-O n
LIST# ARITY-MAX * CWORK-SPAN STG-W n
LIST# CWORK-SPAN STG-N n
LIST# CWORK-SPAN STG-T n

: SV@ ( n -- n )
   cells STG-V + @ ;

: SV! ( n n -- )
   cells STG-V + ! ;

: SO@ ( n -- n )
   cells STG-O + @ ;

: SO! ( n n -- )
   cells STG-O + ! ;

: SW@ ( n -- n )
   cells STG-W + @ ;

: SW! ( n n -- )
   cells STG-W + ! ;

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

\ Any end consumes the stage, whatever its outcome.
: STG-TAKE ( -- )
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have MODE-OPEN <> if E-IR-SCHEMA-STAGE throw then ;

: ONCE-CK ( ptr n -- )
   @ UNSET <> if E-IR-SCHEMA-STAGE throw then ;

: LIST-ROOM ( n -- )
   SN@ ARITY-MAX >= if E-IR-SCHEMA-ARITY throw then ;

\ The tail is necessarily the last entry: appending after one, or a second
\ one, violates the arity rule.
: TAIL-CK ( n -- )
   ST@ 0<> if E-IR-SCHEMA-ARITY throw then ;

: ORD+ ( n n n -- )
   {: l:n ord:n owner:n :}
   l LIST-ROOM
   l SN@ {: n:n :}
   ord l SEG n + SV!
   owner l SEG n + SO!
   n 1+ l SN! ;

: FIX+ ( n n n -- )
   {: sd:n ord:n reg:n :}
   L-FX LIST-ROOM
   L-FX SN@ {: n:n :}
   sd L-FX SEG n + SV!
   ord L-FX SEG n + SO!
   reg L-FX SEG n + SW!
   n 1+ L-FX SN! ;

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

\ A zero count means "no operands"; a missing effect means unfinished.
: FIELDS-CK ( -- )
   STG-EFF @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-CTRL @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-TRAP @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-ARCH @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-RULE @ UNSET = if E-IR-SCHEMA-FIELD throw then
   STG-REND @ UNSET = if E-IR-SCHEMA-FIELD throw then ;

: ARITY-CK ( -- )
   STG-SUCC @ dup 0 < swap SUCC-MAX > or if E-IR-SCHEMA-ARITY throw then
   STG-REGN @ dup 0 < swap REGION-MAX > or if E-IR-SCHEMA-ARITY throw then
   L-OP ST@ 0<> L-OP SN@ 0= and if E-IR-SCHEMA-ARITY throw then
   L-RS ST@ 0<> L-RS SN@ 0= and if E-IR-SCHEMA-ARITY throw then ;

\ A tie or fixed entry names a fixed list entry, never the variadic tail: a tail
\ stands for many fields and names no single register.
: ORD-FIXED? ( n n -- bool )
   {: l:n ord:n :}
   ord 0 < ord l SN@ >= or if false exit then
   l ST@ 0<> ord l SN@ 1- = and if false exit then
   true ;

: FIXED-CK ( n n -- )
   ORD-FIXED? 0= if E-IR-SCHEMA-TIE throw then ;

: TIE-TYPE-CK ( n n -- )
   {: rs:n op:n :}
   L-RS SEG rs + SV@  L-OP SEG op + SV@  <> if E-IR-SCHEMA-TIE throw then
   L-RS SEG rs + SO@  L-OP SEG op + SO@  <> if E-IR-SCHEMA-TIE throw then ;

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

: FX-LIST ( n -- n )
   FX-RESULT = if L-RS else L-OP then ;

: FX-ORD-CK ( n n -- )
   ORD-FIXED? 0= if E-IR-SCHEMA-FIXED throw then ;

: FIX-ONCE-CK ( n -- )
   {: k:n :}
   k 0 ?do
      L-FX SEG k + SV@  L-FX SEG i + SV@  =
      L-FX SEG k + SO@  L-FX SEG i + SO@  =  and
      if E-IR-SCHEMA-FIXED throw then
   loop ;

\ A fixed register is only checked non-negative here: the register allocator
\ refuses one outside the routine's pool (src/compiler/native/regalloc.f,
\ E-A64RA-FIXED), so this file has no native dependency.
: FIX-REG-CK ( n -- )
   0 < if E-IR-SCHEMA-FIXED throw then ;

: FIXES-CK ( -- )
   L-FX SN@ 0 ?do
      L-FX SEG i + SV@ {: sd:n :}
      L-FX SEG i + SO@ {: ord:n :}
      sd FX-LIST ord FX-ORD-CK
      L-FX SEG i + SW@ FIX-REG-CK
      i FIX-ONCE-CK
   loop ;

\ A non-terminator declares no successors and a terminator no results.
: TERM-CK ( -- )
   STG-TERM @ 0= STG-SUCC @ 0<> and if E-IR-SCHEMA-TERM throw then
   STG-TERM @ 0<> L-RS SN@ 0<> and if E-IR-SCHEMA-TERM throw then ;

: WANT-ARCH ( -- CTARGET:arch )
   STG-ARCH @ N>ARCH ;

: WANT-FEAT ( -- CTARGET:features )
   STG-FEAT @ CTARGET:FEATURE-SET ;

: BOUND ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:TARGET@ ;

\ Target needs are checked against the context's binding (IR-CTX:BINDING@),
\ never a rederived table. A feature the required architecture cannot have is
\ incoherent whatever the bound contract says.
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

: DUP-CK ( IR-ARENA:reader -- )
   {: rr:IR-ARENA:reader :}
   rr STG-NAME @ SCAN-NAME 0 < 0= if E-IR-SCHEMA-DUP throw then ;

\ ---- room and append ---------------------------------------------------------
: ROOM-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-SCHEMA-CAP throw then
   pr PCELLS L-OP SN@ + L-RS SN@ + L-AT SN@ + L-TI SN@ TIE-CELLS +
      L-FX SN@ FIX-CELLS +
   pr PC-CAP IR-ARENA:RD@ > if E-IR-SCHEMA-CAP throw then ;

\ The room check and the reservation both count these, so the checked number is
\ the allocated number.
: STAGED-CELLS ( -- n )
   L-OP SN@ L-RS SN@ + L-AT SN@ + L-TI SN@ TIE-CELLS + L-FX SN@ FIX-CELLS + ;

\ Both arenas are reserved before either is written, after the room check so
\ this table's capacity error comes first.
: ROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a STAGED-CELLS IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

\ The reader survives RESERVE and PUSH: it re-reads its row's pointer and count
\ on every call.
: LIST-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena pr:IR-ARENA:reader l:n :}
   pr PCELLS {: st:n :}
   l SN@ 0 ?do
      c a l SEG i + SV@ CELL+
   loop
   st ;

: TIE-LIST-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena pr:IR-ARENA:reader :}
   pr PCELLS {: st:n :}
   L-TI SN@ 0 ?do
      c a L-TI SEG i + SV@ CELL+
      c a L-TI SEG i + SO@ CELL+
   loop
   st ;

: FIX-LIST-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena pr:IR-ARENA:reader :}
   pr PCELLS {: st:n :}
   L-FX SN@ 0 ?do
      c a L-FX SEG i + SV@ CELL+
      c a L-FX SEG i + SO@ CELL+
      c a L-FX SEG i + SW@ CELL+
   loop
   st ;

: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena opst:n rsst:n atst:n tist:n fxst:n :}
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
   c r tist CELL+   c r L-TI SN@ CELL+
   c r fxst CELL+   c r L-FX SN@ CELL+ ;

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
\ Both headers are bound to key's module serial; the table dies with the owning
\ context.
: NEW ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n n n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx syr:IR-ARENA:arena key:IR-ID:ir-module-key dia:IR-ID:ir-symbol-id major:n minor:n rcap:n pcap:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   major VERSION-OK
   minor VERSION-OK
   dia SYM-OWNER key KEY-SERIAL SERIAL-CK
   syr dia IR-SYM:LEN@ drop
   c pcap PHDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a PHDR-CELLS IR-ARENA:RESERVE
   c a SCL-MAGIC CELL+
   c a key KEY-SERIAL CELL+
   c a pcap CELL+
   c rcap ROW-CELLS * RHDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r RHDR-CELLS IR-ARENA:RESERVE
   c r SCR-MAGIC CELL+
   c r key KEY-SERIAL CELL+
   c r rcap CELL+
   c r dia SYM-ORD CELL+
   c r major CELL+
   c r minor CELL+
   a r ;

\ ---- the schema builder ------------------------------------------------------
: BEGIN-OP ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   STG-MODE @ MODE-NONE <> if E-IR-SCHEMA-STAGE throw then
   MODE-OPEN STG-MODE !
   STG-RESET
   op SYM-ORD STG-NAME !
   op SYM-OWNER STG-NAMEO ! ;

: ABANDON ( -- )
   STG-TAKE ;

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

\ Result ordinal first, then operand. Ordinals are checked against the finished
\ lists at DEFINE, so the lists may be declared in any order around this.
: ADD-TIE ( n n -- )
   {: rs:n op:n :}
   STG-OPEN-CK
   L-TI rs op ORD+ ;

\ E.g. the count `shl r64, cl` reads, the rdx:rax `idiv r64` divides. Ordinals
\ are checked against the finished lists at DEFINE.
: ADD-FIXED ( IR-SCHEMA:side n n -- )
   {: sd:IR-SCHEMA:side ord:n reg:n :}
   STG-OPEN-CK
   sd SIDE-CODE ord reg FIX+ ;

: ADD-ATTR ( IR-ID:ir-symbol-id -- )
   {: k:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   L-AT k SYM-ORD k SYM-OWNER ORD+ ;

\ An attribute outside the required keys may come from the dialect's extension
\ set.
: ADD-ATTR-EXT ( -- )
   STG-OPEN-CK
   STG-ATEXT @ 0<> if E-IR-SCHEMA-STAGE throw then
   1 STG-ATEXT ! ;

: SET-CONTROL ( bool n n -- )
   {: term:bool succ:n regn:n :}
   STG-OPEN-CK
   STG-CTRL ONCE-CK
   1 STG-CTRL !
   term BOOL>N STG-TERM !
   succ STG-SUCC !
   regn STG-REGN ! ;

\ Only the data-memory domain has a space and alias class, so an effect is one
\ of SET-PURE, SET-MEMORY or SET-TOKEN (E-IR-SCHEMA-EFFECT).
: SET-PURE ( -- )
   STG-OPEN-CK
   STG-EFF ONCE-CK
   EF-PURE STG-EFF ! ;

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

: SET-TRAP ( bool -- )
   {: t:bool :}
   STG-OPEN-CK
   STG-TRAP ONCE-CK
   t BOOL>N STG-TRAP ! ;

: SET-TARGET ( CTARGET:arch CTARGET:features -- )
   STG-OPEN-CK
   STG-ARCH ONCE-CK
   CTARGET:FEATURES-N STG-FEAT !
   ARCH-CODE STG-ARCH ! ;

: SET-RULE ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   STG-RULE ONCE-CK
   s SYM-ORD STG-RULE !
   s SYM-OWNER STG-RULEO ! ;

: SET-RENDERER ( IR-ID:ir-symbol-id -- )
   {: s:IR-ID:ir-symbol-id :}
   STG-OPEN-CK
   STG-REND ONCE-CK
   s SYM-ORD STG-REND !
   s SYM-OWNER STG-RENDO ! ;

\ The stage is consumed whatever the outcome; all checks precede the first write.
: DEFINE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key syr:IR-ARENA:arena tyr:IR-ARENA:arena :}
   STG-TAKE
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   FIELDS-CK
   ARITY-CK
   TERM-CK
   TIES-CK
   FIXES-CK
   syr key STG-NAME @ STG-NAMEO @ STG-SYM-CK
   syr key STG-RULE @ STG-RULEO @ STG-SYM-CK
   syr key STG-REND @ STG-RENDO @ STG-SYM-CK
   syr key STG-ATTRS-CK
   tyr key L-OP STG-TYPES-CK
   tyr key L-RS STG-TYPES-CK
   c TARGET-CK
   rr DUP-CK
   pr rr ROOM-CK
   c a r ROOM-TAKE
   c a pr L-OP LIST-ADD {: opst:n :}
   c a pr L-RS LIST-ADD {: rsst:n :}
   c a pr L-AT LIST-ADD {: atst:n :}
   c a pr TIE-LIST-ADD {: tist:n :}
   c a pr FIX-LIST-ADD {: fxst:n :}
   c r opst rsst atst tist fxst ROW-ADD ;

\ ---- table readers -----------------------------------------------------------
: SCHEMAS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK CNT ;

: DIALECT@ ( IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr HC-DIALECT IR-ARENA:RD@ ORD-OK IR-ID:PACK-SYMBOL ;

: MAJOR@ ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK HC-MAJOR IR-ARENA:RD@ ;

: MINOR@ ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK HC-MINOR IR-ARENA:RD@ ;

: DEFINED? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ op SYM-OWNER SERIAL-CK
   rr op SYM-ORD SCAN-NAME 0 < 0= ;

\ ---- schema readers ----------------------------------------------------------
private

\ The name scan and the field read share one reader resolution.
: FLD ( IR-ARENA:reader IR-ID:ir-symbol-id n -- n )
   {: rr:IR-ARENA:reader op:IR-ID:ir-symbol-id off:n :}
   rr  rr op ROW-OF  off RC@ ;

: LFLD ( IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id off:n :}
   r IR-ARENA:OPEN-LIVE op off FLD ;

: FFLD ( IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id off:n :}
   rv IR-ARENA:OPEN op off FLD ;

public

: OPERANDS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-OPN LFLD ;

: OPERAND-TAIL? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-OPTAIL LFLD N>BOOL ;

: RESULTS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-RSN LFLD ;

: RESULT-TAIL? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-RSTAIL LFLD N>BOOL ;

: SUCCESSORS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-SUCC LFLD ;

: REGIONS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-REGN LFLD ;

: ATTRS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-ATN LFLD ;

: TIES ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-TIN LFLD ;

: FIXED-COUNT@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   OFF-FXN LFLD ;

: ATTR-EXT? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-ATEXT LFLD N>BOOL ;

: TRAPS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-TRAP LFLD N>BOOL ;

: TERMINATOR? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OFF-TERM LFLD N>BOOL ;

: EFFECT@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-SCHEMA:effect )
   OFF-EFF LFLD N>EFF ;

: DOMAIN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-TYPE:domain )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ HAS-DOM? SHAPE-CK
   rr l OFF-DOM RC@ N>DOM ;

: SPACE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-TYPE:space )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ rr l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   rr l OFF-SPC RC@ N>SPC ;

: ALIAS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-SCHEMA:alias )
   {: r:IR-ARENA:arena op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ rr l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   rr l OFF-ALI RC@ N>ALI ;

: ARCH@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- CTARGET:arch )
   OFF-ARCH LFLD N>ARCH ;

: FEATURES@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- CTARGET:features )
   OFF-FEAT LFLD CTARGET:FEATURE-SET ;

: RULE@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr op OFF-RULE FLD ORD-OK IR-ID:PACK-SYMBOL ;

: RENDERER@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr op OFF-REND FLD ORD-OK IR-ID:PACK-SYMBOL ;

private

: WIN@ ( IR-ARENA:reader IR-ARENA:reader n n n n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n stoff:n lenoff:n i:n :}
   rr l stoff RC@ {: st:n :}
   rr l lenoff RC@ {: ln:n :}
   pr PCELLS st ln WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   pr st i + PC@ ORD-OK ;

\ The index counts ties, not cells.
: TWIN@ ( IR-ARENA:reader IR-ARENA:reader n n n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n i:n col:n :}
   rr l OFF-TIST RC@ {: st:n :}
   rr l OFF-TIN RC@ {: ln:n :}
   pr PCELLS st ln TIE-CELLS WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   pr st i TIE-CELLS + col + PC@ ORD-OK ;

\ The index counts entries, not cells.
: FWIN@ ( IR-ARENA:reader IR-ARENA:reader n n n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n i:n col:n :}
   rr l OFF-FXST RC@ {: st:n :}
   rr l OFF-FXN RC@ {: ln:n :}
   pr PCELLS st ln FIX-CELLS WIN-CK-N
   i 0 < i ln >= or if E-IR-SCHEMA-BOUND throw then
   pr st i FIX-CELLS + col + PC@ ORD-OK ;

public

: OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   key pr rr  rr op ROW-OF  OFF-OPST OFF-OPN i WIN@ IR-ID:PACK-TYPE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   key pr rr  rr op ROW-OF  OFF-RSST OFF-RSN i WIN@ IR-ID:PACK-TYPE ;

: ATTR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   key pr rr  rr op ROW-OF  OFF-ATST OFF-ATN i WIN@ IR-ID:PACK-SYMBOL ;

\ Ordinals into this schema's own lists, so neither takes the key.
: TIE-RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i TIE-RS TWIN@ ;

: TIE-OPERAND@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i TIE-OP TWIN@ ;

: FIXED-SIDE@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- IR-SCHEMA:side )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-SIDE FWIN@ N>SIDE ;

: FIXED-ORDINAL@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-ORD FWIN@ ;

: FIXED-REG@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena op:IR-ID:ir-symbol-id i:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-REG FWIN@ ;

\ ---- frozen readers ----------------------------------------------------------
: FSCHEMAS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup RHDR-CK CNT ;

: FDIALECT@ ( IR-ARENA:view IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr HC-DIALECT IR-ARENA:RD@ ORD-OK IR-ID:PACK-SYMBOL ;

: FMAJOR@ ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup RHDR-CK HC-MAJOR IR-ARENA:RD@ ;

: FMINOR@ ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup RHDR-CK HC-MINOR IR-ARENA:RD@ ;

: FDEFINED? ( IR-ARENA:view IR-ID:ir-symbol-id -- bool )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ op SYM-OWNER SERIAL-CK
   rr op SYM-ORD SCAN-NAME 0 < 0= ;

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

: FFIXED-COUNT@ ( IR-ARENA:view IR-ID:ir-symbol-id -- n )
   OFF-FXN FFLD ;

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
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ HAS-DOM? SHAPE-CK
   rr l OFF-DOM RC@ N>DOM ;

: FSPACE@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-TYPE:space )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ rr l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   rr l OFF-SPC RC@ N>SPC ;

: FALIAS@ ( IR-ARENA:view IR-ID:ir-symbol-id -- IR-SCHEMA:alias )
   {: rv:IR-ARENA:view op:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr op ROW-OF {: l:n :}
   rr l OFF-EFF RC@ rr l OFF-DOM RC@ HAS-MEM? SHAPE-CK
   rr l OFF-ALI RC@ N>ALI ;

: FARCH@ ( IR-ARENA:view IR-ID:ir-symbol-id -- CTARGET:arch )
   OFF-ARCH FFLD N>ARCH ;

: FFEATURES@ ( IR-ARENA:view IR-ID:ir-symbol-id -- CTARGET:features )
   OFF-FEAT FFLD CTARGET:FEATURE-SET ;

: FRULE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr op OFF-RULE FLD ORD-OK IR-ID:PACK-SYMBOL ;

: FRENDERER@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   key rr op OFF-REND FLD ORD-OK IR-ID:PACK-SYMBOL ;

: FOPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr key RKEY-CK
   key pr rr  rr op ROW-OF  OFF-OPST OFF-OPN i WIN@ IR-ID:PACK-TYPE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr key RKEY-CK
   key pr rr  rr op ROW-OF  OFF-RSST OFF-RSN i WIN@ IR-ID:PACK-TYPE ;

: FATTR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-symbol-id n -- IR-ID:ir-symbol-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr key RKEY-CK
   key pr rr  rr op ROW-OF  OFF-ATST OFF-ATN i WIN@ IR-ID:PACK-SYMBOL ;

: FTIE-RESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i TIE-RS TWIN@ ;

: FTIE-OPERAND@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i TIE-OP TWIN@ ;

: FFIXED-SIDE@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- IR-SCHEMA:side )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-SIDE FWIN@ N>SIDE ;

: FFIXED-ORDINAL@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-ORD FWIN@ ;

: FFIXED-REG@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-symbol-id n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view op:IR-ID:ir-symbol-id i:n :}
   pv IR-ARENA:OPEN {: pr:IR-ARENA:reader :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   pr rr  rr op ROW-OF  i FIX-REG FWIN@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
