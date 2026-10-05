\ select.f - WSEL, the Wasm backend's selector: the folded interim HIR a
\ definition froze (src/compiler/native/backend.f FREEZE), read through NFROZEN
\ and written out as a WSTRUCT module frozen through WSTRUCT:FREEZE, so the full
\ verifier checks what the interim freeze skipped (docs/wasm-backend.md 17.2).
\
\ THE CALL. Every function is (ctx:i32, in x i64) -> (status:i32, out x i64),
\ its arity the HIR function's, and function zero's held to the DECLARE row.
\ Past WPROF's 16 input or 16 output lanes a function takes the aligned frame
\ instead: (ctx:i32) -> (status:i32), its inputs and outputs in the eight-byte
\ cells of the context stack, the native data stack's layout (section 7.3). At
\ every call, wordcall and terminal the live row the operation carries through
\ is stored to the context stack, deepest cell first, and reloaded after, as
\ the native boundary stores it (src/compiler/native/select-x64.f CALL-SAVE),
\ so a callee that reads the stack - depth, .s, catch - sees what it sees
\ natively; the arguments and results of a lane call travel as lanes. A call
\ whose operands and results disagree on that row is refused, as native
\ CALL-LIVE refuses it. Every store to the context stack, a saved row or a
\ framed output, first checks that its cells end inside WPROF's stack region:
\ past it they would overwrite static data, where the native stack meets its
\ guard page, so the store faults first, as STACK-BOUNDS.
\
\ THE STATUS. Status 0 is a return, status 1 a catchable throw whose full code
\ the callee stored in ctx.throw-code (section 8.1). Every caller tests it
\ before it reads an output and propagates a 1 through its own return with zero
\ outputs, leaving the context stack where the throw left it: the catch that
\ takes the throw restores the depth, as natively. A tail call is a call then
\ a return, self calls included: not bounded space (section 17.4).
\
\ THE TERMINAL AND THE TRAP. HIR ends control with `terminal`, a call to the
\ engine's throw or die that carries the whole live row and states no arity
\ (src/compiler/native/elaborate.f STAGE-TERMINAL). It is selected as the
\ dynamic adapter's call (section 7.2): the row stored, no lanes, so the callee
\ takes its operands from the stack. Its status 1 propagates; a callee that
\ came back with 0 is a fault. A fault - that, a `trap`, a store past the
\ stack region or a pointer that fails its check - is fatal and is never a
\ status (section 8.2): it records ctx.fault-kind, the exit code the native
\ process would end with, and ctx.fault-addr, the trap's message, the top the
\ store would have left, the pointer, or zero, then executes unreachable.
\
\ THE INTEGERS. Wrapping i64 arithmetic; `/` tests its divisor before any
\ i64.div_s, storing E-DIV-ZERO and returning status 1 for zero and answering
\ 0 - n for -1, so MIN-N -1 / is MIN-N; `mod` is elaborate.f's expansion over
\ that division; a comparison's i32 predicate becomes the observable mask
\ 0 - extend_u(p); the shifts are Wasm's, whose count is taken modulo 64 as the
\ native shift-by-register takes it (src/compiler/native/hir.f); and a brz tests
\ the whole cell through i64.eqz. A unit whose overflow policy traps makes add,
\ sub and mul may-trap, which wrapping i64 arithmetic cannot reproduce, so it is
\ refused.
\
\ THE FLOATS. A real is an f64 value, which bitsreal and realbits, the
\ elaborator's crossings to and from cells, reinterpret. Habu's NaN rule binds
\ Wasm (section 17.3): after add, sub, mul, div and sqrt the selector answers,
\ without a branch, the left operand when it is a NaN, else the right when it
\ is, else $7FF8000000000000 when the result is, else the result. Each test is
\ an f64.ne of a value with itself and each answer a select, which moves bits
\ and makes none, so a made NaN is $7FF8000000000000 and a quiet NaN operand
\ passes through unchanged, the left of two; a signalling one passes unquieted,
\ outside the rule. neg and abs are sign-bit operations and add nothing. A
\ comparison answers the integer mask, f0< and f0= against +0.0. s>f is
\ f64.convert_i64_s, nearest with ties to even, and f>s i64.trunc_sat_f64_s,
\ which truncates, saturates and answers 0 for a NaN as src/compiler/native/
\ hir-word.f DEF-FLOAT defines realint. Each operation is selected alone, so no
\ multiply and add contract.
\
\ THE BLOCKS. HIR blocks keep their order and arguments; a function opens with a
\ prologue block that takes the signature's arguments and branches into HIR's
\ entry block, and ends with one block that propagates a status. A call, a
\ terminal, a division, a stack room check and a pointer check split their
\ block, so before a function is built its plan counts the blocks each HIR
\ block becomes and every branch is aimed by that plan. No conditional edge
\ meets an argument, so none needs a landing block: the elaborator aims every
\ brz at a block that takes nothing and passes any arguments on by br, the
\ token's included, and no block this selector puts behind a brz takes one.
\ The interim HIR freeze does not check this, but the full verify in
\ WSTRUCT:FREEZE refuses a brz into a block with arguments (src/compiler/ir/
\ verify.f SUCCARGS-CK), so a stray one is refused there, never selected.
\
\ THE MEMORY TOKEN. HIR's token values map to WSTRUCT's; the effects this
\ selector adds itself take the token current where they stand: the block's
\ token argument, the prologue's, or for a block without one its first
\ predecessor's last.
\
\ THE CHECKED ACCESS. A pointer is a zero-extended offset in an i64 cell and
\ memory32 takes an i32 address, so a load or store first proves on the whole
\ cell that the pointer's high 32 bits are zero and that it lies past the null
\ reservation [0, WPROF:CTX-BASE), which no Wasm engine faults on since address
\ zero is in bounds, and only then narrows it by i32.wrap_i64 (sections 6.2,
\ 6.3 and 17.5). A pointer that fails is fatal, as a native access fault is,
\ and records the exit code of native's crash handler with the pointer as its
\ address. The access takes the narrowed address at offset 0, so the engine's
\ bounds check covers p + n whole: an access running past the memory's end
\ traps there and never wraps into its start.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require src/core/engine-error.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f
require src/compiler/native/hir.f
require src/compiler/native/backend.f
require src/arch/wasm/profile.f
require src/arch/wasm/wstruct.f

\ WSEL's codes, -9820..-9824, in the Wasm backend's block -9800..-9829.
-9820 constant E-WSEL-FIRST
-9824 constant E-WSEL-LAST
-9820 constant E-WSEL-REFUSED \ an HIR operation this selector does not lower: quot, whose descriptor a sibling selects
-9821 constant E-WSEL-DECLARE \ a selection no DECLARE preceded, a definition whose arity is not the one declared, or a return whose operands are not its function's output cells
-9822 constant E-WSEL-SOURCE  \ a module that is not HIR of the version this selector reads
-9823 constant E-WSEL-TRAP    \ a may-trap operation whose rule cannot raise the trap: add, sub or mul of a unit whose overflow traps
-9824 constant E-WSEL-CALL    \ a call whose operands and results disagree on the cells live across it: fewer operands than its token and arguments, or results other than its token, that kept row and its outputs

package WSEL
public

\ ---- the signature descriptor -----------------------------------------------
\ The pinned 16/16 rule (section 7.3): a function past either count takes the
\ frame.
: FRAMED? ( n n -- bool )
   {: in:n out:n :}
   in WPROF:PARAMS-MAX >  out WPROF:RESULTS-MAX >  or ;

private

DYNAMIC-BUFFER FUN-IN n             \ each function's input cells, last selection
DYNAMIC-BUFFER FUN-OUT n            \ and its output cells

public

\ What function n of the last module selected takes and leaves, in cells: with
\ FRAMED? the descriptor its encoding records.
: ARITY ( n -- n n )
   {: k:n :}
   k FUN-IN @  k FUN-OUT @ ;

private

\ ---- the declaration ---------------------------------------------------------
variable D-IN
variable D-OUT
variable D-SET                       \ whether a DECLARE awaits its SELECT
0 D-SET !

\ Each selection takes its own declaration, so a stale one is never read.
: DECLARE-TAKE ( -- )
   D-SET @ 0= if E-WSEL-DECLARE throw then
   0 D-SET ! ;

\ ---- what one selection works on ---------------------------------------------
1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-SPAN IR-SOURCE:span
1 TYPED-BUFFER S-CTXV IR-ID:ir-value-id    \ the function's context address
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id     \ the token the next effect takes
1 TYPED-BUFFER S-PTOK IR-ID:ir-value-id    \ the token the prologue leaves
1 TYPED-BUFFER S-TOP IR-ID:ir-value-id     \ the stack top a call site stood at
1 TYPED-BUFFER S-SELF IR-ID:ir-symbol-id   \ function zero, a self-call's callee
variable S-IN                        \ the function's input cells
variable S-OUT                       \ and output cells
variable S-FRAMED                    \ whether it takes the frame
variable R-BASE                      \ its first HIR block's ordinal
variable MADE                        \ WSTRUCT blocks built so far
variable PROP-ORD                    \ the function's propagating block
variable PROP-NEED                   \ whether anything branches to it

DYNAMIC-BUFFER VMAP IR-ID:ir-value-id      \ HIR value -> WSTRUCT value
DYNAMIC-BUFFER WB n                        \ HIR block -> its first WSTRUCT block
DYNAMIC-BUFFER EXIT-TOK IR-ID:ir-value-id  \ HIR block -> its last token
DYNAMIC-BUFFER INS IR-ID:ir-value-id       \ the values the prologue hands on

: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: CTXV ( -- IR-ID:ir-value-id )      0 S-CTXV @ ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;
: FRAMED ( -- bool )                 S-FRAMED @ 0<> ;

\ ---- the source dialect, bound off the frozen module --------------------------
HIR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OUT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-REAL IR-ID:ir-type-id

: BIND-HIR ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m HIR:FROM? 0= if E-WSEL-SOURCE throw then
   HIR:OPCODES 0 ?do  m i HIR:FBIND i BND-OP !  loop
   m HIR:FKEY-VALUE 0 BND-VAL !
   m HIR:FKEY-ADDR 0 BND-ADDR !
   m HIR:FKEY-ENTRY 0 BND-ENTRY !
   m HIR:FKEY-IN 0 BND-IN !
   m HIR:FKEY-OUT 0 BND-OUT !
   m HIR:FMEM-TYPE 0 BND-MEM !
   m HIR:FREAL-TYPE 0 BND-REAL ! ;

\ A symbol naming no opcode answers -1, which HIR:NTH refuses.
: OP-SLOT ( IR-ID:ir-op-id -- n )
   NFROZEN:OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   -1
   HIR:OPCODES 0 ?do
      sym i BND-OP @ NFROZEN:SAME-SYM? if drop i leave then
   loop ;

: IS? ( IR-ID:ir-op-id HIR:opcode -- bool )
   HIR:ORD {: k:n :}
   NFROZEN:OPCODE-AT  k BND-OP @  NFROZEN:SAME-SYM? ;

\ A key the operation does not carry answers -1, which the reader refuses.
: ATTR ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id NFROZEN:ATTRS-OF 0 ?do
      id i NFROZEN:ATTR-KEY-AT want NFROZEN:SAME-SYM? if drop i leave then
   loop
   id swap NFROZEN:ATTR-INT-AT ;

\ ---- the value map -----------------------------------------------------------
\ Each entry starts as the HIR value itself, which no WSTRUCT builder accepts,
\ so a value read before its definition was selected is refused where it is used.
: VMAP-INIT ( -- )
   NFROZEN:VALUE-COUNT {: n:n :}
   n VMAP-RESERVE
   n 0 ?do  NFROZEN:MKEY i IR-ID:PACK-VALUE  i VMAP !  loop ;

: BIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: h:IR-ID:ir-value-id w:IR-ID:ir-value-id :}
   w  h IR-ID:VALUE-LOCAL VMAP ! ;

: V ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   IR-ID:VALUE-LOCAL VMAP @ ;

: OPND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   NFROZEN:OPERAND-AT V ;

: TOKEN? ( IR-ID:ir-value-id -- bool )
   NFROZEN:VALUE-TYPE-AT 0 BND-MEM @ NFROZEN:SAME-TYPE? ;

: REAL? ( IR-ID:ir-value-id -- bool )
   NFROZEN:VALUE-TYPE-AT 0 BND-REAL @ NFROZEN:SAME-TYPE? ;

\ ---- spans -------------------------------------------------------------------
: SPAN ( IR-SOURCE:span -- IR-SOURCE:span )
   IR--SOURCE-SPAN:UNMAKE {: src:IR-ID:ir-source-id st:n ln:n :}
   BLD  0 S-SID @  st ln IR-BUILD:ADD-SPAN ;

: SPAN! ( IR-SOURCE:span -- )
   SPAN 0 S-SPAN ! ;

: FUN-SPAN! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   NFROZEN:V-FUNR NFROZEN:VW NFROZEN:MKEY f IR-FUN:FSPAN@ SPAN! ;

: BLOCK-SPAN! ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   NFROZEN:V-BLKR NFROZEN:VW NFROZEN:MKEY bk IR-FUN:FBLOCK-SPAN@ SPAN! ;

\ ---- staging one WSTRUCT operation --------------------------------------------
: I32 ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:I32-TYPE ;
: I64 ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:I64-TYPE ;
: F64 ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:F64-TYPE ;
: MEM ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:MEM-TYPE ;

: OPEN ( WSTRUCT:opcode -- )
   {: o:WSTRUCT:opcode :}
   CTX BLD  CTX BLD o WSTRUCT:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  0 S-SPAN @  IR-BUILD:SET-OP-SPAN ;

: USE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   CTX BLD v IR-BUILD:ADD-OPERAND ;

: RES ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   CTX BLD t IR-BUILD:ADD-RESULT ;

: CLOSE ( -- IR-ID:ir-op-id )
   CTX BLD IR-BUILD:END-OP ;

: RESULT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: o:IR-ID:ir-op-id i:n :}
   CTX BLD o i IR-BUILD:OP-RESULT@ ;

: CLOSE1 ( IR-ID:ir-type-id -- IR-ID:ir-value-id )
   RES CLOSE 0 RESULT ;

: INT-ATTR ( IR-ID:ir-symbol-id n -- )
   {: k:IR-ID:ir-symbol-id v:n :}
   CTX BLD k  CTX BLD v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

\ A host word's callee symbol: `host`, a space and its entry in decimal. No
\ word's name holds a space, so no function of the module is spelled like one,
\ and the encoder reads the entry back for the call's site row (src/compiler/
\ native/emission.f CALL-SITE+), as the native emitters file a host call.
: HOST-CALLEE ( n -- IR-ID:ir-symbol-id )
   {: entry:n :}
   SB-RESET
   s" host " SB-APPEND
   entry FMT:SB-U
   CTX BLD SB$ IR-BUILD:INTERN-SYMBOL ;

: SUCC ( n -- )
   {: ord:n :}
   CTX BLD  BLD IR-BUILD:MODULE-KEY ord IR-ID:PACK-BLOCK  IR-BUILD:ADD-SUCCESSOR ;

\ ---- values ------------------------------------------------------------------
\ A cell-wide constant of HIR's address kind.
: K64 ( n n -- IR-ID:ir-value-id )
   {: v:n kind:n :}
   WSTRUCT-OPCODE:I64-CONST OPEN
   CTX BLD WSTRUCT:KEY-VALUE v INT-ATTR
   CTX BLD  CTX BLD WSTRUCT:KEY-ADDR  CTX BLD kind WSTRUCT:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   I64 CLOSE1 ;

: N64 ( n -- IR-ID:ir-value-id )
   WSTRUCT:ADDR-NONE K64 ;

: K32 ( n -- IR-ID:ir-value-id )
   {: v:n :}
   WSTRUCT-OPCODE:I32-CONST OPEN
   CTX BLD WSTRUCT:KEY-VALUE v INT-ATTR
   I32 CLOSE1 ;

\ The double whose IEEE 754 bits are n.
: KF64 ( n -- IR-ID:ir-value-id )
   {: v:n :}
   WSTRUCT-OPCODE:F64-CONST OPEN
   CTX BLD WSTRUCT:KEY-VALUE v INT-ATTR
   F64 CLOSE1 ;

: OP1 ( IR-ID:ir-value-id WSTRUCT:opcode IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id o:WSTRUCT:opcode t:IR-ID:ir-type-id :}
   o OPEN  a USE  t CLOSE1 ;

: OP2 ( IR-ID:ir-value-id IR-ID:ir-value-id WSTRUCT:opcode IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id b:IR-ID:ir-value-id o:WSTRUCT:opcode
      t:IR-ID:ir-type-id :}
   o OPEN  a USE  b USE  t CLOSE1 ;

\ ---- linear memory -----------------------------------------------------------
\ An access at an i32 address plus a byte offset, ordered by the current token.
: MEMARG ( n n -- )
   {: al:n off:n :}
   CTX BLD WSTRUCT:KEY-ALIGN al INT-ATTR
   CTX BLD WSTRUCT:KEY-OFFSET off INT-ATTR ;

: LOAD ( IR-ID:ir-value-id n WSTRUCT:opcode IR-ID:ir-type-id n -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id off:n o:WSTRUCT:opcode t:IR-ID:ir-type-id al:n :}
   o OPEN  a USE  TOK USE
   al off MEMARG
   t RES  MEM RES
   CLOSE {: id:IR-ID:ir-op-id :}
   id 1 RESULT TOK!
   id 0 RESULT ;

: STORE ( IR-ID:ir-value-id IR-ID:ir-value-id n WSTRUCT:opcode n -- )
   {: a:IR-ID:ir-value-id v:IR-ID:ir-value-id off:n o:WSTRUCT:opcode al:n :}
   o OPEN  a USE  v USE  TOK USE
   al off MEMARG
   MEM CLOSE1 TOK! ;

: LD32 ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   WSTRUCT-OPCODE:I32-LOAD I32 2 LOAD ;

: LD64 ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   WSTRUCT-OPCODE:I64-LOAD I64 3 LOAD ;

: ST32 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   WSTRUCT-OPCODE:I32-STORE 2 STORE ;

: ST64 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   WSTRUCT-OPCODE:I64-STORE 3 STORE ;

\ The context stack's top, an i32 address in its eight-byte field.
: TOP@ ( -- IR-ID:ir-value-id )
   CTXV WPROF:CTX-STACK-TOP LD32 ;

: TOP! ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   CTXV a WPROF:CTX-STACK-TOP ST32 ;

\ top + 8n, the top after n cells more.
: CELLS+ ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id n:n :}
   a  n 8 * K32  WSTRUCT-OPCODE:I32-ADD I32 OP2 ;

\ The fault a trap records: the native exit code and the message, or zero.
: FAULT ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: kind:IR-ID:ir-value-id addr:IR-ID:ir-value-id :}
   CTXV kind WPROF:CTX-FAULT-KIND ST32
   CTXV addr WPROF:CTX-FAULT-ADDR ST64
   WSTRUCT-OPCODE:UNREACHABLE OPEN CLOSE drop ;

\ ---- blocks ------------------------------------------------------------------
: BLOCK ( -- )
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD 0 S-SPAN @ IR-BUILD:SET-BLOCK-SPAN ;

: BLOCK-END ( -- )
   CTX BLD IR-BUILD:END-BLOCK drop
   MADE @ 1+ MADE ! ;

: ARG ( IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-type-id :}
   CTX BLD t IR-BUILD:ADD-BLOCK-ARG ;

: BR0 ( n -- )
   {: ord:n :}
   WSTRUCT-OPCODE:BR OPEN  ord SUCC  CLOSE drop  BLOCK-END ;

\ The first destination when the i32 condition is zero, the second otherwise.
: BRZ ( IR-ID:ir-value-id n n -- )
   {: c:IR-ID:ir-value-id z:n nz:n :}
   WSTRUCT-OPCODE:BRZ OPEN  c USE  z SUCC  nz SUCC  CLOSE drop  BLOCK-END ;

\ A return opened on its status; the output lanes follow it.
: RET-OPEN ( IR-ID:ir-value-id -- )
   WSTRUCT-OPCODE:RETURN OPEN USE ;

\ A status of zero continues in the block built next; any other propagates.
: STATUS ( IR-ID:ir-value-id -- )
   MADE @ 1+  PROP-ORD @  BRZ
   1 PROP-NEED ! ;

\ Room for n cells stored from the top t: they must end inside WPROF's stack
\ region, or the block built next records the fault native's guard page raises,
\ STACK-BOUNDS, with the top the store would have left as its address. Answers
\ that top, in the block built after the fault, where the stores follow.
: ROOM ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-value-id n:n :}
   t n CELLS+ {: e:IR-ID:ir-value-id :}
   e WSTRUCT-OPCODE:I64-EXTEND-I32-U I64 OP1 {: w:IR-ID:ir-value-id :}
   MADE @ {: at:n :}
   TOK {: k:IR-ID:ir-value-id :}
   w  WPROF:DATA-BASE N64  WSTRUCT-OPCODE:I64-GT-S I32 OP2  at 2 +  at 1+  BRZ
   BLOCK
   ENGINE-ERROR:STACK-BOUNDS K32  w  FAULT
   BLOCK-END
   k TOK!                            \ the fault's stores order nothing after it
   BLOCK
   e ;

\ ---- checked memory ------------------------------------------------------------
\ The exit code native's crash handler ends the process with after an access
\ fault outside every stack's guard pages (src/habu/crash.f EMIT-CRASH-HANDLER,
\ src/habu/boot-x64.f CRASH-RC).
134 constant CRASH-RC

\ How many bits a memory32 address has: a pointer at or past 2^32 names no byte.
32 constant ADDR-BITS

\ The i32 address of the pointer p, once its whole cell proved that its high
\ bits are zero and that it is at least WPROF:CTX-BASE; otherwise the block
\ built next records CRASH-RC with the pointer as its address. Answers the
\ address in the block built after the fault, where the access follows.
: >ADDR ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: p:IR-ID:ir-value-id :}
   p  ADDR-BITS N64  WSTRUCT-OPCODE:I64-SHR-U I64 OP2 {: hi:IR-ID:ir-value-id :}
   p  WPROF:CTX-BASE N64  WSTRUCT-OPCODE:I64-LT-S I32 OP2
   WSTRUCT-OPCODE:I64-EXTEND-I32-U I64 OP1 {: nul:IR-ID:ir-value-id :}
   hi nul WSTRUCT-OPCODE:I64-OR I64 OP2
   WSTRUCT-OPCODE:I64-EQZ I32 OP1 {: ok:IR-ID:ir-value-id :}
   MADE @ {: at:n :}
   TOK {: k:IR-ID:ir-value-id :}
   ok  at 1+  at 2 +  BRZ
   BLOCK
   CRASH-RC K32  p  FAULT
   BLOCK-END
   k TOK!                            \ the fault's stores order nothing after it
   BLOCK
   p WSTRUCT-OPCODE:I32-WRAP-I64 I32 OP1 ;

: ACCESS? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id HIR-OPCODE:LOAD IS?  id HIR-OPCODE:STORE IS? or
   id HIR-OPCODE:BLOAD IS? or  id HIR-OPCODE:BSTORE IS? or ;

\ hir.load and hir.bload: the access at the checked address, ordered by the
\ token HIR states, at alignment exponent al.
: SEL-LOAD ( IR-ID:ir-op-id WSTRUCT:opcode n -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode al:n :}
   id 1 OPND TOK!
   id 0 OPND >ADDR  0 o I64 al LOAD {: v:IR-ID:ir-value-id :}
   id 0 NFROZEN:RESULT-AT v BIND
   id 1 NFROZEN:RESULT-AT TOK BIND ;

\ hir.store and hir.bstore: Forth's value then address, Wasm's address then
\ value. Source `!` and `c!` reach HIR as calls to the engine's guarded stores
\ (src/compiler/native/elaborate.f DO-STORE), so these come from a builder
\ that stages HIR directly.
: SEL-STORE ( IR-ID:ir-op-id WSTRUCT:opcode n -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode al:n :}
   id 2 OPND TOK!
   id 1 OPND >ADDR  id 0 OPND  0 o al STORE
   id 0 NFROZEN:RESULT-AT TOK BIND ;

\ ---- the shape of a call --------------------------------------------------------
\ What a call takes and leaves: RECURSE names the definition, so a self call's
\ shape is the declaration's, and a wordcall states its callee's.
: SHAPE ( IR-ID:ir-op-id -- n n )
   {: id:IR-ID:ir-op-id :}
   id HIR-OPCODE:CALL IS? if  D-IN @ D-OUT @  exit  then
   id 0 BND-IN @ ATTR  id 0 BND-OUT @ ATTR ;

\ The cells live across a call. Its operands are its token, the kept row and the
\ arguments, its results the token, the same kept row and the outputs
\ (elaborate.f CALL-OPERANDS+, CALL-CLOSE), and both lists must say so, as
\ native CALL-LIVE holds them (src/compiler/native/select-x64.f).
: KEPT ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n :}
   id NFROZEN:OPERANDS-OF 1- a - {: kk:n :}
   kk 0 < if E-WSEL-CALL throw then
   id NFROZEN:RESULTS-OF 1- r - kk <> if E-WSEL-CALL throw then
   kk ;

\ The cells an operation stores to the context stack, its operands from the
\ first after any token: a call's kept row, and in the frame its arguments
\ after it; a terminal's whole row; a framed return's outputs.
: STORED ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id HIR-OPCODE:CALL IS?  id HIR-OPCODE:WORDCALL IS?  or if
      id SHAPE {: a:n r:n :}
      id a r KEPT  a r FRAMED? if a + then  exit
   then
   id HIR-OPCODE:TERMINAL IS? if  id NFROZEN:OPERANDS-OF 1-  exit  then
   id HIR-OPCODE:RETURN IS? FRAMED and if  id NFROZEN:OPERANDS-OF  exit  then
   0 ;

\ ---- the plan ----------------------------------------------------------------
\ How many blocks beyond its own one HIR operation's selection builds: a
\ division's five; a pointer check's or a stack room check's fault and the
\ block after it; and the block after a call's or a terminal's status test.
: EXTRA ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id HIR-OPCODE:DIV IS? if 5 exit then
   id ACCESS? if 2 exit then
   id STORED 0<> if 2 else 0 then
   id HIR-OPCODE:CALL IS?  id HIR-OPCODE:WORDCALL IS? or
   id HIR-OPCODE:TERMINAL IS? or  if 1+ then ;

: BLOCK-SIZE ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   1
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT EXTRA +  loop ;

\ The prologue is the function's first block, HIR's blocks follow it, and the
\ propagating block ends it.
: PLAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f NFROZEN:BLOCK-COUNT {: n:n :}
   n WB-RESERVE
   n EXIT-TOK-RESERVE
   f 0 NFROZEN:BLOCK-AT IR-ID:BLOCK-LOCAL R-BASE !
   MADE @ 1+
   n 0 ?do
      dup i WB !
      f i NFROZEN:BLOCK-AT BLOCK-SIZE +
      NFROZEN:MKEY 0 IR-ID:PACK-VALUE i EXIT-TOK !
   loop
   PROP-ORD !
   0 PROP-NEED ! ;

: ORD ( IR-ID:ir-block-id -- n )
   IR-ID:BLOCK-LOCAL R-BASE @ - WB @ ;

: HIR-ORD ( IR-ID:ir-op-id n -- n )
   NFROZEN:SUCC-AT ORD ;

\ ---- the integer rules --------------------------------------------------------
: SEL-CONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-VAL @ ATTR  id 0 BND-ADDR @ ATTR  K64
   id 0 NFROZEN:RESULT-AT swap BIND ;

: SEL-BINARY ( IR-ID:ir-op-id WSTRUCT:opcode -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode :}
   id 0 OPND  id 1 OPND  o I64 OP2
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ The i32 predicate p as the observable flag, all ones: 0 - extend_u(p).
: FLAG ( IR-ID:ir-op-id IR-ID:ir-value-id -- )
   {: id:IR-ID:ir-op-id p:IR-ID:ir-value-id :}
   p WSTRUCT-OPCODE:I64-EXTEND-I32-U I64 OP1 {: e:IR-ID:ir-value-id :}
   0 N64  e  WSTRUCT-OPCODE:I64-SUB I64 OP2
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ Two cells' or two doubles' predicate, the left operand first.
: SEL-COMPARE ( IR-ID:ir-op-id WSTRUCT:opcode -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode :}
   id  id 0 OPND  id 1 OPND  o I32 OP2  FLAG ;

: SEL-INVERT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPND  -1 N64  WSTRUCT-OPCODE:I64-XOR I64 OP2
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ Zero stores E-DIV-ZERO and propagates before anything divides; -1 answers
\ 0 - n, which wraps MIN-N to itself where i64.div_s would trap. The blocks are
\ the five EXTRA counts: the zero path, the -1 test, the two quotients and the
\ join that takes the quotient as its argument.
: SEL-DIV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPND {: a:IR-ID:ir-value-id :}
   id 1 OPND {: b:IR-ID:ir-value-id :}
   MADE @ {: at:n :}
   TOK {: k:IR-ID:ir-value-id :}
   b WSTRUCT-OPCODE:I64-EQZ I32 OP1  at 2 +  at 1+  BRZ
   BLOCK
   CTXV  E-DIV-ZERO N64  WPROF:CTX-THROW-CODE ST64
   k TOK!                            \ the zero path's store orders nothing after it
   PROP-ORD @ BR0
   1 PROP-NEED !
   BLOCK
   b  -1 N64  WSTRUCT-OPCODE:I64-EQ I32 OP2  at 3 +  at 4 +  BRZ
   BLOCK
   a b WSTRUCT-OPCODE:I64-DIV-S I64 OP2 {: q:IR-ID:ir-value-id :}
   WSTRUCT-OPCODE:BR OPEN  q USE  at 5 + SUCC  CLOSE drop  BLOCK-END
   BLOCK
   0 N64  a  WSTRUCT-OPCODE:I64-SUB I64 OP2 {: r:IR-ID:ir-value-id :}
   WSTRUCT-OPCODE:BR OPEN  r USE  at 5 + SUCC  CLOSE drop  BLOCK-END
   BLOCK
   id 0 NFROZEN:RESULT-AT  I64 ARG  BIND ;

\ HIR's first token: the one the prologue leaves.
: SEL-MEM ( IR-ID:ir-op-id -- )
   0 NFROZEN:RESULT-AT TOK BIND ;

\ ---- the float rules ---------------------------------------------------------
\ The NaN every operation that makes one answers.
$7FF8000000000000 constant NAN-MADE

\ HIR's value is the double's own bits, which f64.const carries as they are.
: SEL-FCONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-VAL @ ATTR KF64
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ One operand to one result of type t: a sign operation, a conversion or a
\ reinterpretation, none of which adds anything.
: SEL-UNARY ( IR-ID:ir-op-id WSTRUCT:opcode IR-ID:ir-type-id -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode t:IR-ID:ir-type-id :}
   id 0 OPND o t OP1
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ The double against +0.0, whose bits are zero: -0.0 equals it and is not below
\ it, as natively.
: SEL-COMPARE0 ( IR-ID:ir-op-id WSTRUCT:opcode -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode :}
   id  id 0 OPND  0 KF64  o I32 OP2  FLAG ;

\ x when the double c is a NaN, else y: c's f64.ne with itself is the select's
\ condition, and a select moves bits and makes none.
: IF-NAN ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-ID:ir-value-id x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   c c WSTRUCT-OPCODE:F64-NE I32 OP2 {: p:IR-ID:ir-value-id :}
   WSTRUCT-OPCODE:F64-SELECT OPEN  x USE  y USE  p USE  F64 CLOSE1 ;

\ The rule's two halves: an answer r that is a NaN becomes NAN-MADE, whatever
\ sign and payload Wasm gave it, and an operand a that is a NaN takes the
\ place of the answer v so far.
: CANON ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: r:IR-ID:ir-value-id :}
   r  NAN-MADE KF64  r  IF-NAN ;

: PASS ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id v:IR-ID:ir-value-id :}
   a  a  v  IF-NAN ;

\ The right operand passes first and the left over it, so of two NaNs the
\ left is the answer.
: SEL-FARITH ( IR-ID:ir-op-id WSTRUCT:opcode -- )
   {: id:IR-ID:ir-op-id o:WSTRUCT:opcode :}
   id 0 OPND {: a:IR-ID:ir-value-id :}
   id 1 OPND {: b:IR-ID:ir-value-id :}
   a  b  a b o F64 OP2 CANON  PASS  PASS
   id 0 NFROZEN:RESULT-AT swap BIND ;

: SEL-FSQRT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPND {: a:IR-ID:ir-value-id :}
   a  a WSTRUCT-OPCODE:F64-SQRT F64 OP1 CANON  PASS
   id 0 NFROZEN:RESULT-AT swap BIND ;

\ ---- control -----------------------------------------------------------------
: SEL-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   WSTRUCT-OPCODE:BR OPEN
   id NFROZEN:OPERANDS-OF 0 ?do  id i OPND USE  loop
   id 0 HIR-ORD SUCC
   CLOSE drop  BLOCK-END ;

\ A whole cell is tested, so i64.eqz answers 1 for zero and HIR's zero
\ successor is WSTRUCT's second.
: SEL-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPND WSTRUCT-OPCODE:I64-EQZ I32 OP1
   id 1 HIR-ORD  id 0 HIR-ORD  BRZ ;

\ A return leaves exactly its function's output cells, as native EMIT-EXIT holds
\ it (src/compiler/native/select-x64.f). The interim freeze checks no
\ operation's shape, and a framed signature states no output lane for the full
\ verify to hold the return to, so a short framed return would answer status 0
\ over cells it never wrote.
: SEL-RETURN ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id NFROZEN:OPERANDS-OF S-OUT @ <> if E-WSEL-DECLARE throw then
   id STORED {: n:n :}
   n 0<> if
      TOP@ {: t:IR-ID:ir-value-id :}
      t n ROOM {: e:IR-ID:ir-value-id :}
      n 0 ?do  t  id i OPND  i 8 *  ST64  loop
      e TOP!
   then
   0 K32 RET-OPEN
   FRAMED 0= if  id NFROZEN:OPERANDS-OF 0 ?do  id i OPND USE  loop  then
   CLOSE drop  BLOCK-END ;

\ A fatal end: the exit code and the message address it carries.
: SEL-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 2 OPND WSTRUCT-OPCODE:I32-WRAP-I64 I32 OP1  id 0 OPND  FAULT
   BLOCK-END ;

\ ---- calls -------------------------------------------------------------------
\ The n cells STORED answers go to the stack; a lane call passes its arguments
\ as lanes, and a framed one reads its outputs off the stack where it stood.
: CALL-SAVE ( IR-ID:ir-op-id n bool -- )
   {: id:IR-ID:ir-op-id n:n fr:bool :}
   n 0= fr 0= and if exit then
   TOP@ {: t:IR-ID:ir-value-id :}
   t 0 S-TOP !
   n 0= if exit then
   t n ROOM {: e:IR-ID:ir-value-id :}
   n 0 ?do  t  id i 1+ OPND  i 8 *  ST64  loop
   e TOP! ;

: CALL-OP ( IR-ID:ir-op-id n n n bool IR-ID:ir-symbol-id -- IR-ID:ir-op-id )
   {: id:IR-ID:ir-op-id kk:n a:n r:n fr:bool callee:IR-ID:ir-symbol-id :}
   WSTRUCT-OPCODE:CALL OPEN
   CTXV USE  TOK USE
   fr 0= if  a 0 ?do  id kk i + 1+ OPND USE  loop  then
   CTX BLD  CTX BLD WSTRUCT:KEY-CALLEE  CTX BLD callee IR-BUILD:INTERN-SYMBOL-ATTR
   IR-BUILD:ADD-ATTR
   I32 RES  MEM RES
   fr 0= if  r 0 ?do  I64 RES  loop  then
   CLOSE {: c:IR-ID:ir-op-id :}
   c 1 RESULT TOK!
   c ;

\ After the status test: the outputs from their lanes or the frame, the kept
\ cells back from the stack, and the top where it stood.
: CALL-BACK ( IR-ID:ir-op-id IR-ID:ir-op-id n n bool -- )
   {: id:IR-ID:ir-op-id c:IR-ID:ir-op-id kk:n r:n fr:bool :}
   r 0 ?do
      id  kk i + 1+ NFROZEN:RESULT-AT
      fr if  0 S-TOP @  kk i + 8 *  LD64  else  c i 2 + RESULT  then
      BIND
   loop
   kk 0 ?do  id i 1+ NFROZEN:RESULT-AT  0 S-TOP @ i 8 * LD64  BIND  loop
   kk 0<> fr or if  0 S-TOP @ TOP!  then
   id 0 NFROZEN:RESULT-AT TOK BIND ;

: SEL-CALL ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- )
   {: id:IR-ID:ir-op-id callee:IR-ID:ir-symbol-id :}
   id SHAPE {: a:n r:n :}
   id a r KEPT {: kk:n :}
   a r FRAMED? {: fr:bool :}
   id 0 OPND TOK!
   id  id STORED  fr CALL-SAVE
   id kk a r fr callee CALL-OP {: c:IR-ID:ir-op-id :}
   c 0 RESULT STATUS
   BLOCK
   id c kk r fr CALL-BACK ;

: SEL-SELF-CALL ( IR-ID:ir-op-id -- )
   0 S-SELF @ SEL-CALL ;

: SEL-WORDCALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 BND-ENTRY @ ATTR HOST-CALLEE  SEL-CALL ;

\ The whole row goes to the stack and the call passes no lane: the adapter's
\ convention, since the operation states no arity.
: SEL-TERMINAL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPND TOK!
   id  id STORED  false CALL-SAVE
   id 0 0 0 true  id 0 BND-ENTRY @ ATTR HOST-CALLEE
   CALL-OP {: c:IR-ID:ir-op-id :}
   c 0 RESULT STATUS
   BLOCK
   ENGINE-ERROR:CODE-CERT K32  0 N64  FAULT
   BLOCK-END ;

\ ---- one HIR operation --------------------------------------------------------
: REFUSE ( -- )
   E-WSEL-REFUSED throw ;

\ The rules that raise what may trap as Habu raises it: a zero divisor and a
\ callee's throw return status 1, a trap ends in unreachable.
: RAISES? ( HIR:opcode -- bool )
   {: o:HIR:opcode :}
   o HIR-OPCODE:DIV HIR-OPCODE:EQ
   o HIR-OPCODE:CALL HIR-OPCODE:EQ or
   o HIR-OPCODE:WORDCALL HIR-OPCODE:EQ or
   o HIR-OPCODE:TERMINAL HIR-OPCODE:EQ or
   o HIR-OPCODE:TRAP HIR-OPCODE:EQ or ;

\ An operation whose schema says it may trap, selected by a rule that cannot
\ raise it, is refused: what a unit whose overflow traps makes of add, sub and
\ mul (src/compiler/native/select-x64.f TRAP-CK).
: TRAP-CK ( IR-ID:ir-op-id HIR:opcode -- HIR:opcode )
   {: id:IR-ID:ir-op-id o:HIR:opcode :}
   NFROZEN:V-SCHR NFROZEN:VW  id NFROZEN:OPCODE-AT  IR-SCHEMA:FTRAPS?
   o RAISES? 0= and if E-WSEL-TRAP throw then
   o ;

: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id NFROZEN:SPAN-AT SPAN!
   id  id OP-SLOT HIR:NTH  TRAP-CK
   MATCH HIR:opcode
      const    OF id SEL-CONST ENDOF
      add      OF id WSTRUCT-OPCODE:I64-ADD SEL-BINARY ENDOF
      sub      OF id WSTRUCT-OPCODE:I64-SUB SEL-BINARY ENDOF
      mul      OF id WSTRUCT-OPCODE:I64-MUL SEL-BINARY ENDOF
      div      OF id SEL-DIV ENDOF
      lt       OF id WSTRUCT-OPCODE:I64-LT-S SEL-COMPARE ENDOF
      le       OF id WSTRUCT-OPCODE:I64-LE-S SEL-COMPARE ENDOF
      gt       OF id WSTRUCT-OPCODE:I64-GT-S SEL-COMPARE ENDOF
      ge       OF id WSTRUCT-OPCODE:I64-GE-S SEL-COMPARE ENDOF
      equal    OF id WSTRUCT-OPCODE:I64-EQ SEL-COMPARE ENDOF
      ne       OF id WSTRUCT-OPCODE:I64-NE SEL-COMPARE ENDOF
      and      OF id WSTRUCT-OPCODE:I64-AND SEL-BINARY ENDOF
      or       OF id WSTRUCT-OPCODE:I64-OR SEL-BINARY ENDOF
      xor      OF id WSTRUCT-OPCODE:I64-XOR SEL-BINARY ENDOF
      lshift   OF id WSTRUCT-OPCODE:I64-SHL SEL-BINARY ENDOF
      rshift   OF id WSTRUCT-OPCODE:I64-SHR-U SEL-BINARY ENDOF
      invert   OF id SEL-INVERT ENDOF
      mem      OF id SEL-MEM ENDOF
      load     OF id WSTRUCT-OPCODE:I64-LOAD 3 SEL-LOAD ENDOF
      store    OF id WSTRUCT-OPCODE:I64-STORE 3 SEL-STORE ENDOF
      bload    OF id WSTRUCT-OPCODE:I64-LOAD8-U 0 SEL-LOAD ENDOF
      bstore   OF id WSTRUCT-OPCODE:I64-STORE8 0 SEL-STORE ENDOF
      br       OF id SEL-BR ENDOF
      brz      OF id SEL-BRZ ENDOF
      call     OF id SEL-SELF-CALL ENDOF
      wordcall OF id SEL-WORDCALL ENDOF
      quot     OF REFUSE ENDOF
      return   OF id SEL-RETURN ENDOF
      trap     OF id SEL-TRAP ENDOF
      fconst   OF id SEL-FCONST ENDOF
      fadd     OF id WSTRUCT-OPCODE:F64-ADD SEL-FARITH ENDOF
      fsub     OF id WSTRUCT-OPCODE:F64-SUB SEL-FARITH ENDOF
      fmul     OF id WSTRUCT-OPCODE:F64-MUL SEL-FARITH ENDOF
      fdiv     OF id WSTRUCT-OPCODE:F64-DIV SEL-FARITH ENDOF
      fneg     OF id WSTRUCT-OPCODE:F64-NEG F64 SEL-UNARY ENDOF
      fabs     OF id WSTRUCT-OPCODE:F64-ABS F64 SEL-UNARY ENDOF
      fsqrt    OF id SEL-FSQRT ENDOF
      flt      OF id WSTRUCT-OPCODE:F64-LT SEL-COMPARE ENDOF
      fgt      OF id WSTRUCT-OPCODE:F64-GT SEL-COMPARE ENDOF
      feq      OF id WSTRUCT-OPCODE:F64-EQ SEL-COMPARE ENDOF
      fltz     OF id WSTRUCT-OPCODE:F64-LT SEL-COMPARE0 ENDOF
      feqz     OF id WSTRUCT-OPCODE:F64-EQ SEL-COMPARE0 ENDOF
      intreal  OF id WSTRUCT-OPCODE:F64-CONVERT-I64-S F64 SEL-UNARY ENDOF
      realint  OF id WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S I64 SEL-UNARY ENDOF
      bitsreal OF id WSTRUCT-OPCODE:F64-REINTERPRET-I64 F64 SEL-UNARY ENDOF
      realbits OF id WSTRUCT-OPCODE:I64-REINTERPRET-F64 I64 SEL-UNARY ENDOF
      terminal OF id SEL-TERMINAL ENDOF
   ;MATCH ;

\ ---- one HIR block -------------------------------------------------------------
: ARG-TYPE ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: v:IR-ID:ir-value-id :}
   v TOKEN? if MEM exit then
   v REAL? if F64 exit then
   I64 ;

\ The token current at the block's start: its own argument, the prologue's for
\ the entry block, else its first predecessor's last. A block without a token
\ argument is entered with one HIR token on every edge, and every block ends on
\ the token its last HIR token maps to, so any predecessor answers; the first
\ was walked already, since IR-VERIFY lists predecessors in block order
\ (src/compiler/ir/verify.f EDGES-FILL) and the elaborator opens a block after
\ the one that first enters it: a loop's header after the block before the
\ loop, so a backedge is never a header's first predecessor.
: ENTER-TOK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id j:n :}
   j 0= if 0 S-PTOK @ TOK! exit then
   bk 0 NFROZEN:PRED-AT IR-ID:BLOCK-LOCAL R-BASE @ - EXIT-TOK @ TOK! ;

: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id j:n :}
   f j NFROZEN:BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk BLOCK-SPAN!
   BLOCK
   bk j ENTER-TOK
   bk NFROZEN:ARG-COUNT 0 ?do
      bk i NFROZEN:ARG-AT {: h:IR-ID:ir-value-id :}
      h ARG-TYPE ARG {: w:IR-ID:ir-value-id :}
      h w BIND
      h TOKEN? if w TOK! then
   loop
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT RULE  loop
   TOK j EXIT-TOK ! ;

\ ---- one function -------------------------------------------------------------
: SIGNATURE ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   I32 IR-TYPE:FN-PARAM
   FRAMED 0= if  S-IN @ 0 ?do  I64 IR-TYPE:FN-PARAM  loop  then
   I32 IR-TYPE:FN-RESULT
   FRAMED 0= if  S-OUT @ 0 ?do  I64 IR-TYPE:FN-RESULT  loop  then
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id k:n :}
   CTX BLD  NFROZEN:V-SYMP NFROZEN:VW NFROZEN:V-SYMR NFROZEN:VW
   NFROZEN:V-FUNR NFROZEN:VW NFROZEN:MKEY f IR-FUN:FSYMBOL@
   IR-BUILD:CARRY-SYMBOL {: s:IR-ID:ir-symbol-id :}
   k 0= if s 0 S-SELF ! then
   CTX BLD s IR-BUILD:BEGIN-FUN
   CTX BLD SIGNATURE IR-BUILD:SET-SIGNATURE
   CTX BLD  NFROZEN:V-FUNR NFROZEN:VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  NFROZEN:V-FUNR NFROZEN:VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  NFROZEN:V-FUNR NFROZEN:VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD  0 S-SPAN @  IR-BUILD:SET-FUN-SPAN ;

\ The signature's arguments, or the inputs taken off the frame, handed to HIR's
\ entry block, which a loop may enter again.
: PROLOGUE ( -- )
   S-IN @ {: in:n :}
   in INS-RESERVE
   BLOCK
   I32 ARG 0 S-CTXV !
   FRAMED 0= if  in 0 ?do  I64 ARG i INS !  loop  then
   MEM ARG TOK!
   FRAMED in 0<> and if
      TOP@  in 8 * K32  WSTRUCT-OPCODE:I32-SUB I32 OP2 {: base:IR-ID:ir-value-id :}
      in 0 ?do  base i 8 * LD64  i INS !  loop
      base TOP!
   then
   TOK 0 S-PTOK !
   WSTRUCT-OPCODE:BR OPEN
   in 0 ?do  i INS @ USE  loop
   0 WB @ SUCC
   CLOSE drop  BLOCK-END ;

\ Status 1 with every output lane zero, the context stack left as it stands.
: PROPAGATE ( -- )
   PROP-NEED @ 0= if exit then
   BLOCK
   1 K32 {: s:IR-ID:ir-value-id :}
   FRAMED if 0 else S-OUT @ then {: n:n :}
   n 0= if
      s RET-OPEN
   else
      0 N64 {: z:IR-ID:ir-value-id :}
      s RET-OPEN
      n 0 ?do  z USE  loop
   then
   CLOSE drop  BLOCK-END ;

: WALK-FUN ( n -- )
   {: k:n :}
   NFROZEN:MKEY k IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f NFROZEN:FUN-ARITY {: in:n out:n :}
   k 0= if
      in D-IN @ <> out D-OUT @ <> or if E-WSEL-DECLARE throw then
   then
   in k FUN-IN !
   out k FUN-OUT !
   in S-IN !
   out S-OUT !
   in out FRAMED? if 1 else 0 then S-FRAMED !
   f PLAN
   f FUN-SPAN!
   f k OPEN-FUN
   PROLOGUE
   f NFROZEN:BLOCK-COUNT 0 ?do  f i WALK-BLOCK  loop
   f FUN-SPAN!
   PROPAGATE
   CTX BLD IR-BUILD:END-FUN drop ;

\ The source the HIR module registered, carried so every span names it.
: SOURCE! ( -- )
   CTX BLD  NFROZEN:V-SRC NFROZEN:VW  NFROZEN:MKEY 0 IR-ID:PACK-SOURCE
   IR-BUILD:CARRY-SOURCE 0 S-SID ! ;

public

\ ---- the stage rows ----------------------------------------------------------
\ What the definition takes and leaves. The linkage changes nothing here: a
\ tail call is a call then a return, and a definition that never returns simply
\ has no return to select.
: DECLARE ( n n NBACK:linkage -- )
   {: in:n out:n l:NBACK:linkage :}
   in D-IN !
   out D-OUT !
   1 D-SET ! ;

\ The module NBACK:FREEZE froze, selected to a WSTRUCT module frozen whole.
: SELECT ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   DECLARE-TAKE
   m BIND-HIR
   m NFROZEN:VIEWS!
   c 0 S-CTX !
   IR-BUILD:PLAN-DEFAULT
   c WSTRUCT:NEW-BUILDER 0 S-BLD !
   SOURCE!
   VMAP-INIT
   NFROZEN:FUN-COUNT {: n:n :}
   n FUN-IN-RESERVE
   n FUN-OUT-RESERVE
   0 MADE !
   n 0 ?do  i WALK-FUN  loop
   c BLD WSTRUCT:FREEZE ;

;package
