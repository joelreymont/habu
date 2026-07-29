\ ir-fun.f - checked compiler function and block store tests.
\
\ Proves the contract of src/compiler/ir/fun.f: an appended function reads back
\ exactly the symbol, signature, linkage, visibility, convention, attributes,
\ block window, and span it was given, and an appended block reads back its
\ parent, its arguments, its operations, its terminator, and its span; a block
\ argument is a value of that block at that position and no other block can
\ claim it; a function's block window and a block's parent field must agree, so
\ cross-function block use is a named reject; the windows tile the block table,
\ the operation table, and the attribute pool exactly, so a forged row can
\ neither overlap another record's window, leave a gap, nor reach past the cells
\ that exist; a block must end in exactly one terminator; the linkage,
\ visibility, and body combinations that no function can have reject; a calling
\ convention the bound target contract cannot provide rejects; one module's
\ function table defines a symbol at most once; out-of-order construction -
\ an operation appended outside a block, an argument minted between a block's
\ operations, a function ended while a block is open, a block ended with no
\ function open - rejects; module keys and identities from another module or
\ another context reject; both committed ceilings reject an append and leave no
\ partial record behind; a frozen module serves every reader through the arena
\ views while the retired builders reject; context teardown releases everything;
\ and checker fixtures prove no mutation can be spelled against a frozen store.

require lib/test.f
require test/checker-assert.f
require src/compiler/ir/fun.f

package IR-FUN-TEST
private

\ The row shapes src/compiler/ir/fun.f commits to, mirrored here so a fixture
\ can append a raw row past that package's constructors and prove the readers
\ still hold. A change to the layout must change this mirror too.
12 constant FROW-CELLS
0 constant F-SYM
1 constant F-SIG
2 constant F-BST
3 constant F-BN
4 constant F-ATST
5 constant F-ATN
6 constant F-SRC
7 constant F-SBEG
8 constant F-SLEN
9 constant F-LNK
10 constant F-VIS
11 constant F-CC

9 constant BROW-CELLS
0 constant B-PAR
1 constant B-AGST
2 constant B-AGN
3 constant B-OPST
4 constant B-OPN
5 constant B-TERM
6 constant B-SRC
7 constant B-BEG
8 constant B-LEN

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract with the baseline instruction set and plain
\ floating point.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the dialect this module declares ----------------------------------------
\ Two opcodes: one ordinary value-producing operation and one terminator.
0 constant K-CONST                   \ no operands, one result, not a terminator
1 constant K-RET                     \ a terminator with no successor

: OPC-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-CONST = if c sp sr key s" hir.const" IR-SYM:INTERN exit then
   c sp sr key s" hir.ret" IR-SYM:INTERN ;

\ ---- the function names this module defines ----------------------------------
0 constant N-MAIN
1 constant N-HELP
2 constant N-EXT

: FN-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k N-MAIN = if c sp sr key s" main" IR-SYM:INTERN exit then
   k N-HELP = if c sp sr key s" help" IR-SYM:INTERN exit then
   c sp sr key s" ext" IR-SYM:INTERN ;

\ ---- module rigging ----------------------------------------------------------
: SYM-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 256 IR-SYM:NEW ;

: TYP-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 64 IR-TYPE:NEW ;

: ATT-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 64 IR-ATTR:NEW ;

: SRC-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 64 IR-SOURCE:NEW ;

: I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

\ Design line 385's signature type: a code reference over one input and one
\ output, which is what design line 456 calls a callable with an effect.
: SIGT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c tp tr key I64 {: ty:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ty IR-TYPE:FN-PARAM
   ty IR-TYPE:FN-RESULT
   c tp tr key IR-TYPE:CODE-REF ;

: A-SPAN ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-SOURCE:span )
   {: c:IR-CTX:ctx sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   sa  c sa key s" fun-source" IR-SOURCE:REGISTER  0 4 IR-SOURCE:SPAN ;

\ ---- the schema table --------------------------------------------------------
: SCH-NEW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 8 64 IR-SCHEMA:NEW ;

: SCH-SHAPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-CONST = if
      c tp tr key I64 IR-SCHEMA:ADD-RESULT
      false 0 0 IR-SCHEMA:SET-CONTROL
      exit
   then
   true 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-SCHEMA:BEGIN-OP
   c tp tr key k SCH-SHAPE
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key s" rule.hir" IR-SYM:INTERN IR-SCHEMA:SET-RULE
   c sp sr key s" render.hir" IR-SYM:INTERN IR-SCHEMA:SET-RENDERER
   c qp qr key sr tr IR-SCHEMA:DEFINE ;

: SCH-ALL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c sp sr tp tr key qp qr K-CONST SCH-DEF
   c sp sr tp tr key qp qr K-RET SCH-DEF ;

\ One rigged module: an interner, a type table, an attribute table, a source
\ registry, a schema table holding both opcodes, an operation store, and a
\ function and block store with the requested ceilings. Every fixture starts
\ here, so a fixture body differs only in what it then asks the store to do.
: RIG ( IR-CTX:ctx n n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx fcap:n bcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c key ATT-NEW {: ap:IR-ARENA:arena ar:IR-ARENA:arena :}
   c key SRC-NEW {: sa:IR-ARENA:arena :}
   c sp sr key SCH-NEW {: qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c key 32 32 256 IR-OP:NEW {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key fcap bcap pcap IR-FUN:NEW {: fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c sp sr tp tr key qp qr SCH-ALL
   key sp sr tp tr ap ar sa qr p v r fp fr br ;

\ ---- appending one operation -------------------------------------------------
: OP+ ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   k K-CONST = if c tp tr key I64 IR-OP:ADD-RESULT then
   c p v r key qr tr ar sa IR-OP:END-OP ;

\ ---- the function and block vocabulary ---------------------------------------
0 constant L-DEF
1 constant L-REPL
2 constant L-IMP
0 constant V-HIDDEN
1 constant V-EXPORT
0 constant C-HABU
1 constant C-C
2 constant C-KERNEL

: SET-LNK ( n -- )
   dup L-DEF = if drop IR--FUN-LINKAGE:DEFINED IR-FUN:SET-LINKAGE exit then
   L-REPL = if IR--FUN-LINKAGE:REPLACEABLE IR-FUN:SET-LINKAGE exit then
   IR--FUN-LINKAGE:IMPORTED IR-FUN:SET-LINKAGE ;

: SET-VIS ( n -- )
   V-HIDDEN = if IR--FUN-VISIBILITY:HIDDEN IR-FUN:SET-VISIBILITY exit then
   IR--FUN-VISIBILITY:EXPORTED IR-FUN:SET-VISIBILITY ;

: SET-CC ( n -- )
   dup C-HABU = if drop IR--FUN-CONVENTION:HABU IR-FUN:SET-CONVENTION exit then
   C-C = if IR--FUN-CONVENTION:C-ABI IR-FUN:SET-CONVENTION exit then
   IR--FUN-CONVENTION:KERNEL IR-FUN:SET-CONVENTION ;

: FUN-OPEN ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena br:IR-ARENA:arena k:n :}
   br  c sp sr key k FN-SYM  IR-FUN:BEGIN-FUN ;

\ The three closed declarations every function needs, plus its signature.
: FUN-DECL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key lk:n vk:n ck:n :}
   c tp tr key SIGT IR-FUN:SET-SIGNATURE
   lk SET-LNK
   vk SET-VIS
   ck SET-CC ;

: FUN-CLOSE ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sa:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena :}
   c sa key A-SPAN IR-FUN:SET-FUN-SPAN
   c fp fr br key sr tr ar sa IR-FUN:END-FUN ;

: ARG+ ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key tp:IR-ARENA:arena tr:IR-ARENA:arena v:IR-ARENA:arena br:IR-ARENA:arena :}
   c v tr br key  c tp tr key I64  IR-FUN:ADD-BLOCK-ARG ;

: BLK-CLOSE ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sa:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena qr:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c sa key A-SPAN IR-FUN:SET-BLOCK-SPAN
   c br fr key v r qr sa IR-FUN:END-BLOCK ;

\ ---- reading one appended function and block back ----------------------------
: READ-BODY ( IR-CTX:ctx -- n n n n n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-EXPORT C-HABU FUN-DECL
   c ap ar key 42 IR-ATTR:INT IR-FUN:ADD-FUN-ATTR
   r IR-FUN:BEGIN-BLOCK
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE drop
   c key sa fp fr br sr tr ar FUN-CLOSE {: f0:IR-ID:ir-fun-id :}
   fr IR-FUN:FUNS
   br IR-FUN:BLOCKS
   fp IR-FUN:ATTR-CELLS
   fr f0 IR-FUN:BLOCK-COUNT
   fr f0 IR-FUN:ATTR-COUNT
   fr key f0 IR-FUN:SYMBOL@ IR-ID:SYMBOL-LOCAL
      c sp sr key N-MAIN FN-SYM IR-ID:SYMBOL-LOCAL =
   fr key f0 IR-FUN:SIGNATURE@ IR-ID:TYPE-LOCAL
      c tp tr key SIGT IR-ID:TYPE-LOCAL =
   fr f0 IR-FUN:LINKAGE@ IR--FUN-LINKAGE:DEFINED IR--FUN-LINKAGE:EQ
   fr f0 IR-FUN:VISIBILITY@ IR--FUN-VISIBILITY:EXPORTED IR--FUN-VISIBILITY:EQ
   fr f0 IR-FUN:CONVENTION@ IR--FUN-CONVENTION:HABU IR--FUN-CONVENTION:EQ ;

: READ-CASE ( -- )
   s" an appended function reads back the record it was given" T-LABEL
   BND [: READ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 1 T= 1 T= 1 T= 1 T= 1 T= ;

\ The block side: two arguments, two operations, and the terminator the second
\ of them is.
: BLOCK-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key tp tr v br ARG+ {: a0:IR-ID:ir-value-id :}
   c key tp tr v br ARG+ {: a1:IR-ID:ir-value-id :}
   c key sp sr tp tr ar sa qr p v r K-CONST OP+ drop
   c key sp sr tp tr ar sa qr p v r K-RET OP+ {: o1:IR-ID:ir-op-id :}
   c key sa v r qr fr br BLK-CLOSE {: b0:IR-ID:ir-block-id :}
   c key sa fp fr br sr tr ar FUN-CLOSE {: f0:IR-ID:ir-fun-id :}
   br b0 IR-FUN:ARG-COUNT
   br b0 IR-FUN:OP-COUNT
   br v key b0 1 IR-FUN:ARG@ IR-ID:VALUE-LOCAL a1 IR-ID:VALUE-LOCAL =
   br r key b0 0 IR-FUN:OP@ IR-ID:OP-LOCAL 0 =
   br r key b0 IR-FUN:TERMINATOR@ IR-ID:OP-LOCAL o1 IR-ID:OP-LOCAL =
   br fr key b0 IR-FUN:PARENT@ IR-ID:FUN-LOCAL f0 IR-ID:FUN-LOCAL =
   fr br key f0 0 IR-FUN:BLOCK@ IR-ID:BLOCK-LOCAL b0 IR-ID:BLOCK-LOCAL = ;

: BLOCK-CASE ( -- )
   s" an appended block reads back its arguments, operations, and parent" T-LABEL
   BND [: BLOCK-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 2 T= 2 T= ;

\ A block argument is a value that knows the block that defined it and its own
\ argument index, which is the other half of design line 434.
: ARGVAL-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key tp tr v br ARG+ drop
   c key tp tr v br ARG+ {: a1:IR-ID:ir-value-id :}
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE {: b0:IR-ID:ir-block-id :}
   c key sa fp fr br sr tr ar FUN-CLOSE drop
   v a1 IR-OP:VALUE-ARG@
   v key a1 IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL b0 IR-ID:BLOCK-LOCAL =
   v a1 IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ ;

: ARGVAL-CASE ( -- )
   s" a block argument reads back its block and its argument index" T-LABEL
   BND [: ARGVAL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 1 T= ;

\ An imported function declares a body it does not carry, so its block window is
\ empty and it needs no block at all.
: IMPORT-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-EXT FUN-OPEN
   c tp tr key L-IMP V-EXPORT C-C FUN-DECL
   c key sa fp fr br sr tr ar FUN-CLOSE {: f0:IR-ID:ir-fun-id :}
   fr f0 IR-FUN:BLOCK-COUNT
   br IR-FUN:BLOCKS
   fr f0 IR-FUN:CONVENTION@ IR--FUN-CONVENTION:C-ABI IR--FUN-CONVENTION:EQ ;

: IMPORT-CASE ( -- )
   s" an imported function carries no block and reads back whole" T-LABEL
   BND [: IMPORT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= ;

\ Two functions, so the block windows and the parent fields have to agree over
\ more than one row.
: TWO-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE drop
   c key sa fp fr br sr tr ar FUN-CLOSE drop
   c key sp sr br N-HELP FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE {: b1:IR-ID:ir-block-id :}
   c key sa fp fr br sr tr ar FUN-CLOSE {: f1:IR-ID:ir-fun-id :}
   fr IR-FUN:FUNS
   br IR-FUN:BLOCKS
   fr br key f1 0 IR-FUN:BLOCK@ IR-ID:BLOCK-LOCAL b1 IR-ID:BLOCK-LOCAL =
   br fr key b1 IR-FUN:PARENT@ IR-ID:FUN-LOCAL f1 IR-ID:FUN-LOCAL = ;

: TWO-CASE ( -- )
   s" a second function claims exactly its own blocks" T-LABEL
   BND [: TWO-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 2 T= 2 T= ;

\ ---- one legal function, then one deviation ----------------------------------
\ Every negative append fixture builds the same legal function and block up to
\ the point it deviates, so a fixture differs from the legal shape in exactly
\ one decision.
1 constant S-DUP                     \ a second function with the same symbol
2 constant S-NOSIG                   \ no signature declared
3 constant S-BADSIG                  \ a signature that is not a code reference
4 constant S-NOLNK                   \ no linkage declared
5 constant S-NOVIS                   \ no visibility declared
6 constant S-NOCC                    \ no convention declared
7 constant S-NOSPAN                  \ no source span declared
8 constant S-IMP-BODY                \ an imported function carrying a block
9 constant S-IMP-HIDDEN              \ an imported function nothing can name
10 constant S-REPL-HIDDEN            \ a replaceable definition nothing can replace
11 constant S-EMPTY                  \ a definition with no block at all
12 constant S-KERNEL                 \ a kernel convention on an AArch64 contract
13 constant S-NOTERM                 \ a block whose last operation is not a terminator
14 constant S-MIDTERM                \ a block with a terminator before its end
15 constant S-NOOPS                  \ a block with no operation at all
16 constant S-STRAY-OP               \ an operation appended while no block is open
17 constant S-LATE-ARG               \ an argument minted between the block's operations
18 constant S-FUN-OPEN-BLK           \ a function ended while its block is open
19 constant S-BLK-NO-FUN             \ a block ended with no function open
20 constant S-SIG-TWICE              \ the signature declared twice
21 constant S-FUN-TWICE              \ a function opened while one is open
22 constant S-BLK-TWICE              \ a block opened while one is open
23 constant S-FUN-NONE               \ a function ended that was never opened
24 constant S-BLK-NONE               \ a block ended that was never opened

: SC-LNK ( n -- n )
   {: s:n :}
   s S-IMP-BODY = s S-IMP-HIDDEN = or if L-IMP exit then
   s S-REPL-HIDDEN = if L-REPL exit then
   L-DEF ;

: SC-VIS ( n -- n )
   {: s:n :}
   s S-IMP-HIDDEN = s S-REPL-HIDDEN = or if V-HIDDEN exit then
   s S-IMP-BODY = if V-EXPORT exit then
   V-EXPORT ;

: SC-CC ( n -- n )
   S-KERNEL = if C-KERNEL else C-HABU then ;

\ Does this scenario build a block at all?
: SC-BLOCK? ( n -- bool )
   {: s:n :}
   s S-EMPTY = if false exit then
   s S-IMP-HIDDEN = s S-REPL-HIDDEN = or if false exit then
   true ;

\ ---- the negative append body ------------------------------------------------
: NEG-DECL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key s:n :}
   s S-NOSIG <> if
      s S-BADSIG = if
         c tp tr key I64 IR-FUN:SET-SIGNATURE
      else
         c tp tr key SIGT IR-FUN:SET-SIGNATURE
      then
   then
   s S-SIG-TWICE = if c tp tr key SIGT IR-FUN:SET-SIGNATURE then
   s S-NOLNK <> if s SC-LNK SET-LNK then
   s S-NOVIS <> if s SC-VIS SET-VIS then
   s S-NOCC <> if s SC-CC SET-CC then ;

: NEG-OPS ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena s:n :}
   s S-NOOPS = if exit then
   s S-NOTERM = if
      c key sp sr tp tr ar sa qr p v r K-CONST OP+ drop
      exit
   then
   s S-MIDTERM = if
      c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
      c key sp sr tp tr ar sa qr p v r K-CONST OP+ drop
      c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
      exit
   then
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop ;

: NEG-BLOCK ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena s:n :}
   s S-STRAY-OP = if c key sp sr tp tr ar sa qr p v r K-CONST OP+ drop then
   r IR-FUN:BEGIN-BLOCK
   s S-BLK-TWICE = if r IR-FUN:BEGIN-BLOCK then
   s S-LATE-ARG = if
      c key tp tr v br ARG+ drop
      c key sp sr tp tr ar sa qr p v r K-CONST OP+ drop
      c key tp tr v br ARG+ drop
   then
   c key sp sr tp tr ar sa qr p v r s NEG-OPS
   s S-FUN-OPEN-BLK = if exit then
   c key sa v r qr fr br BLK-CLOSE drop ;

: NEG-BODY ( n IR-CTX:ctx -- )
   {: s:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   s S-BLK-NO-FUN = if
      r IR-FUN:BEGIN-BLOCK
      c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
      IR-FUN:ABANDON-BLOCK
      r IR-FUN:BEGIN-BLOCK
      c key sa v r qr fr br BLK-CLOSE drop
      exit
   then
   s S-FUN-NONE = if c key sa fp fr br sr tr ar FUN-CLOSE drop exit then
   s S-BLK-NONE = if IR-FUN:ABANDON-BLOCK exit then
   c key sp sr br N-MAIN FUN-OPEN
   s S-FUN-TWICE = if c key sp sr br N-HELP FUN-OPEN then
   c tp tr key s NEG-DECL
   s SC-BLOCK? if
      c key sp sr tp tr ar sa qr p v r fr br s NEG-BLOCK
   then
   s S-NOSPAN = if c fp fr br key sr tr ar sa IR-FUN:END-FUN drop exit then
   c key sa fp fr br sr tr ar FUN-CLOSE drop
   s S-DUP = if
      c key sp sr br N-MAIN FUN-OPEN
      c tp tr key SIGT IR-FUN:SET-SIGNATURE
      L-IMP SET-LNK
      V-EXPORT SET-VIS
      C-HABU SET-CC
      c key sa fp fr br sr tr ar FUN-CLOSE drop
   then ;

: NEG-RUN ( n -- )
   BND [: NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A throw inside a staged declaration leaves the function or block open, because
\ only an end consumes the stage. The next begin therefore rejects until the
\ caller abandons, which is what this does between fixtures.
: CLEAR-STAGE ( -- )
   [: IR-FUN:ABANDON-BLOCK ;] catch drop
   [: IR-FUN:ABANDON-FUN ;] catch drop ;

: FIELD-CASES ( -- )
   s" a function with no signature rejects" T-LABEL
   [: S-NOSIG NEG-RUN ;] E-IR-FUN-FIELD TTHROWSQ
   CLEAR-STAGE
   s" a function with no linkage rejects" T-LABEL
   [: S-NOLNK NEG-RUN ;] E-IR-FUN-FIELD TTHROWSQ
   CLEAR-STAGE
   s" a function with no visibility rejects" T-LABEL
   [: S-NOVIS NEG-RUN ;] E-IR-FUN-FIELD TTHROWSQ
   CLEAR-STAGE
   s" a function with no calling convention rejects" T-LABEL
   [: S-NOCC NEG-RUN ;] E-IR-FUN-FIELD TTHROWSQ
   CLEAR-STAGE ;

: FIELD-CASES-B ( -- )
   s" a function with no source span rejects" T-LABEL
   [: S-NOSPAN NEG-RUN ;] E-IR-FUN-FIELD TTHROWSQ
   CLEAR-STAGE
   s" a signature that is not a code reference rejects" T-LABEL
   [: S-BADSIG NEG-RUN ;] E-IR-FUN-SIG TTHROWSQ
   CLEAR-STAGE
   s" a symbol this function table already defines rejects" T-LABEL
   [: S-DUP NEG-RUN ;] E-IR-FUN-DUP TTHROWSQ
   CLEAR-STAGE ;

: LINKAGE-CASES ( -- )
   s" an imported function that carries a block rejects" T-LABEL
   [: S-IMP-BODY NEG-RUN ;] E-IR-FUN-LINKAGE TTHROWSQ
   CLEAR-STAGE
   s" an imported function nothing outside can name rejects" T-LABEL
   [: S-IMP-HIDDEN NEG-RUN ;] E-IR-FUN-LINKAGE TTHROWSQ
   CLEAR-STAGE
   s" a replaceable definition nothing can replace rejects" T-LABEL
   [: S-REPL-HIDDEN NEG-RUN ;] E-IR-FUN-LINKAGE TTHROWSQ
   CLEAR-STAGE
   s" a definition with no block at all rejects" T-LABEL
   [: S-EMPTY NEG-RUN ;] E-IR-FUN-LINKAGE TTHROWSQ
   CLEAR-STAGE ;

: TARGET-CASES ( -- )
   s" a kernel convention on an AArch64 contract rejects" T-LABEL
   [: S-KERNEL NEG-RUN ;] E-IR-FUN-TARGET TTHROWSQ
   CLEAR-STAGE ;

: TERM-CASES ( -- )
   s" a block whose last operation is not a terminator rejects" T-LABEL
   [: S-NOTERM NEG-RUN ;] E-IR-FUN-TERM TTHROWSQ
   CLEAR-STAGE
   s" a block with a terminator before its end rejects" T-LABEL
   [: S-MIDTERM NEG-RUN ;] E-IR-FUN-TERM TTHROWSQ
   CLEAR-STAGE
   s" a block with no operation at all rejects" T-LABEL
   [: S-NOOPS NEG-RUN ;] E-IR-FUN-TERM TTHROWSQ
   CLEAR-STAGE ;

: ORDER-CASES ( -- )
   s" an operation appended while no block is open rejects" T-LABEL
   [: S-STRAY-OP NEG-RUN ;] E-IR-FUN-WINDOW TTHROWSQ
   CLEAR-STAGE
   s" an argument minted between a block's operations rejects" T-LABEL
   [: S-LATE-ARG NEG-RUN ;] E-IR-FUN-ARG TTHROWSQ
   CLEAR-STAGE
   s" a function ended while its block is open rejects" T-LABEL
   [: S-FUN-OPEN-BLK NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE ;

: STAGE-CASES ( -- )
   s" a block ended with no function open rejects" T-LABEL
   [: S-BLK-NO-FUN NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE
   s" declaring the signature twice rejects" T-LABEL
   [: S-SIG-TWICE NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE
   s" opening a function while one is open rejects" T-LABEL
   [: S-FUN-TWICE NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE ;

: STAGE-CASES-B ( -- )
   s" opening a block while one is open rejects" T-LABEL
   [: S-BLK-TWICE NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE
   s" ending a function that was never opened rejects" T-LABEL
   [: S-FUN-NONE NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE
   s" ending a block that was never opened rejects" T-LABEL
   [: S-BLK-NONE NEG-RUN ;] E-IR-FUN-STAGE TTHROWSQ
   CLEAR-STAGE ;

\ ---- one legal module, then a bad read ---------------------------------------
\ The read fixtures share one rigged module holding a single function with a
\ single block, so a fixture differs only in the read it then attempts.
: ONE-FUN ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena k:n :}
   c key sp sr br k FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key tp tr v br ARG+ drop
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE drop
   c key sa fp fr br sr tr ar FUN-CLOSE drop ;

: IDX-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   key 0 IR-ID:PACK-FUN {: f0:IR-ID:ir-fun-id :}
   key 0 IR-ID:PACK-BLOCK {: b0:IR-ID:ir-block-id :}
   k 1 = if fr  key 3 IR-ID:PACK-FUN  IR-FUN:BLOCK-COUNT drop then
   k 2 = if br  key 3 IR-ID:PACK-BLOCK  IR-FUN:OP-COUNT drop then
   k 3 = if fr br key f0 1 IR-FUN:BLOCK@ drop then
   k 4 = if br r key b0 1 IR-FUN:OP@ drop then
   k 5 = if br v key b0 1 IR-FUN:ARG@ drop then
   k 6 = if br v key b0 -1 IR-FUN:ARG@ drop then
   k 7 = if fp fr key f0 0 IR-FUN:ATTR@ drop then ;

: IDX-RUN ( n -- )
   BND [: IDX-BODY ;] IR-CTX:WITH-CONTEXT ;

: IDX-CASES ( -- )
   s" a function id past the appended count rejects" T-LABEL
   [: 1 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" a block id past the appended count rejects" T-LABEL
   [: 2 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" a block index past the function's window rejects" T-LABEL
   [: 3 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ ;

: IDX-CASES-B ( -- )
   s" an operation index past the block's window rejects" T-LABEL
   [: 4 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" an argument index past the block's window rejects" T-LABEL
   [: 5 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" a negative argument index rejects" T-LABEL
   [: 6 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" an attribute index on a function that carries none rejects" T-LABEL
   [: 7 IDX-RUN ;] E-IR-FUN-BOUND TTHROWSQ ;

\ ---- non-table, misaligned, and forged rows ----------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-FUN:FUNS drop ;

\ The three stores are not interchangeable: the block table presented as the
\ function table is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   br IR-FUN:FUNS drop ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c fr 7 IR-ARENA:PUSH drop
   fr IR-FUN:FUNS drop ;

: RAW-RUN ( -- )     BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;
: SWAP-RUN ( -- )    BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHAPE-RUN ( -- )   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a function table" T-LABEL
   [: RAW-RUN ;] E-IR-FUN-STATE TTHROWSQ
   s" the block table presented as the function table rejects" T-LABEL
   [: SWAP-RUN ;] E-IR-FUN-STATE TTHROWSQ
   s" a misaligned function row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-FUN-STATE TTHROWSQ ;

\ The value one raw forged function cell takes: the fields of a row that
\ continues exactly where the last real row ended, with cell `off` replaced.
: FFROW-CELL ( n n n n n -- n )
   {: i:n bbase:n abase:n off:n val:n :}
   i off = if val exit then
   i F-BST = if bbase exit then
   i F-ATST = if abase exit then
   0 ;

: FORGE-FROW ( IR-CTX:ctx IR-ARENA:arena n n n n -- )
   {: c:IR-CTX:ctx f:IR-ARENA:arena bbase:n abase:n off:n val:n :}
   FROW-CELLS 0 ?do
      c f  i bbase abase off val FFROW-CELL  IR-ARENA:PUSH drop
   loop ;

: FBROW-CELL ( n n n n -- n )
   {: i:n obase:n off:n val:n :}
   i off = if val exit then
   i B-OPST = if obase exit then
   i B-TERM = if obase 1- exit then
   0 ;

: FORGE-BROW ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx b:IR-ARENA:arena obase:n off:n val:n :}
   BROW-CELLS 0 ?do
      c b  i obase off val FBROW-CELL  IR-ARENA:PUSH drop
   loop ;

: FORGE-BODY ( n n n IR-CTX:ctx -- )
   {: off:n val:n k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   fr IR-FUN:FUNS {: l:n :}
   c fr br IR-FUN:BLOCKS fp IR-FUN:ATTR-CELLS off val FORGE-FROW
   key l IR-ID:PACK-FUN {: bad:IR-ID:ir-fun-id :}
   k 1 = if fr br key bad 0 IR-FUN:BLOCK@ drop then
   k 2 = if fp fr key bad 0 IR-FUN:ATTR@ drop then
   k 3 = if fr bad IR-FUN:LINKAGE@ drop then
   k 4 = if fr bad IR-FUN:VISIBILITY@ drop then
   k 5 = if fr bad IR-FUN:CONVENTION@ drop then
   k 6 = if fr key bad IR-FUN:SPAN@ drop then ;

: FORGE-RUN ( n n n -- )
   BND [: FORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: FORGE-CASES ( -- )
   s" a forged block window starting before the previous row ended rejects" T-LABEL
   [: F-BST 0 1 FORGE-RUN ;] E-IR-FUN-WINDOW TTHROWSQ
   s" a forged attribute window leaving a gap rejects" T-LABEL
   [: F-ATST 3 2 FORGE-RUN ;] E-IR-FUN-WINDOW TTHROWSQ
   s" a forged attribute window reaching past the pool rejects" T-LABEL
   [: F-ATN 9 2 FORGE-RUN ;] E-IR-FUN-STATE TTHROWSQ ;

: FORGE-CASES-B ( -- )
   s" a forged linkage code outside the vocabulary rejects" T-LABEL
   [: F-LNK 7 3 FORGE-RUN ;] E-IR-FUN-STATE TTHROWSQ
   s" a forged visibility code outside the vocabulary rejects" T-LABEL
   [: F-VIS 7 4 FORGE-RUN ;] E-IR-FUN-STATE TTHROWSQ
   s" a forged convention code outside the vocabulary rejects" T-LABEL
   [: F-CC 7 5 FORGE-RUN ;] E-IR-FUN-STATE TTHROWSQ
   s" a forged negative source ordinal rejects at the span reader" T-LABEL
   [: F-SRC -3 6 FORGE-RUN ;] E-IR-FUN-STATE TTHROWSQ ;

\ ---- cross-function block use ------------------------------------------------
\ A forged block row that names a parent no function table row can be, and a
\ forged function row that claims it anyway. The two records disagree, and every
\ path that reads one through the other says so.
: CROSS-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   br IR-FUN:BLOCKS {: bl:n :}
   fr IR-FUN:FUNS {: fl:n :}
   c br r IR-OP:OPS B-PAR 5 FORGE-BROW
   c fr bl fp IR-FUN:ATTR-CELLS F-BN 1 FORGE-FROW
   k 1 = if fr br key  key fl IR-ID:PACK-FUN  0 IR-FUN:BLOCK@ drop then
   k 2 = if br fr key  key bl IR-ID:PACK-BLOCK  IR-FUN:PARENT@ drop then ;

: CROSS-RUN ( n -- )
   BND [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The same disagreement caught while the function is still being built.
: CROSS-BUILD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   c br r IR-OP:OPS B-PAR 9 FORGE-BROW
   c key sa fp fr br sr tr ar FUN-CLOSE drop ;

: CROSS-BUILD-RUN ( -- )
   BND [: CROSS-BUILD-BODY ;] IR-CTX:WITH-CONTEXT ;

: CROSS-CASES ( -- )
   s" a function window reaching into another function's block rejects" T-LABEL
   [: 1 CROSS-RUN ;] E-IR-FUN-PARENT TTHROWSQ
   s" a block naming a parent past the function table rejects" T-LABEL
   [: 2 CROSS-RUN ;] E-IR-FUN-BOUND TTHROWSQ
   s" a function claiming a block that names another parent rejects" T-LABEL
   [: CROSS-BUILD-RUN ;] E-IR-FUN-PARENT TTHROWSQ ;

\ A forged block row whose terminator is not the last operation of its window,
\ and one whose argument window claims an operation result.
: BFORGE-BODY ( n n n IR-CTX:ctx -- )
   {: off:n val:n k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   br IR-FUN:BLOCKS {: bl:n :}
   c br r IR-OP:OPS off val FORGE-BROW
   key bl IR-ID:PACK-BLOCK {: bad:IR-ID:ir-block-id :}
   k 1 = if br r key bad IR-FUN:TERMINATOR@ drop then
   k 2 = if br r key bad 0 IR-FUN:OP@ drop then
   k 3 = if br v key bad 0 IR-FUN:ARG@ drop then ;

: BFORGE-RUN ( n n n -- )
   BND [: BFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: BFORGE-CASES ( -- )
   s" a forged terminator that is not the window's last operation rejects" T-LABEL
   [: B-TERM 5 1 BFORGE-RUN ;] E-IR-FUN-TERM TTHROWSQ
   s" a forged operation window starting before the previous row ended rejects" T-LABEL
   [: B-OPST 0 2 BFORGE-RUN ;] E-IR-FUN-WINDOW TTHROWSQ
   s" a forged argument window claiming an operation result rejects" T-LABEL
   [: B-AGN 1 3 BFORGE-RUN ;] E-IR-FUN-ARG TTHROWSQ ;

\ ---- cross-owner references --------------------------------------------------
: FKEY-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   k 1 = if fr other  key 0 IR-ID:PACK-FUN  IR-FUN:SYMBOL@ drop then
   k 2 = if br other  key 0 IR-ID:PACK-BLOCK  IR-FUN:BLOCK-SPAN@ drop then
   k 3 = if
      c other 4 4 8 IR-FUN:NEW {: fp2:IR-ARENA:arena fr2:IR-ARENA:arena br2:IR-ARENA:arena :}
      fr br2 key  key 0 IR-ID:PACK-FUN  0 IR-FUN:BLOCK@ drop
   then
   k 4 = if
      c other 4 4 8 IR-FUN:NEW {: fp3:IR-ARENA:arena fr3:IR-ARENA:arena br3:IR-ARENA:arena :}
      fp3 fr key  key 0 IR-ID:PACK-FUN  0 IR-FUN:ATTR@ drop
   then ;

: FKEY-RUN ( n -- )
   BND [: FKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The staged cross-owner rejects need a second module's own tables.
: FTAB-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   k 1 = if
      c other SYM-NEW {: sp2:IR-ARENA:arena sr2:IR-ARENA:arena :}
      br  c sp2 sr2 other N-MAIN FN-SYM  IR-FUN:BEGIN-FUN
      c tp tr key L-IMP V-EXPORT C-HABU FUN-DECL
   then
   k 2 = if
      c key sp sr br N-MAIN FUN-OPEN
      c other TYP-NEW {: tp2:IR-ARENA:arena tr2:IR-ARENA:arena :}
      c tp2 tr2 other SIGT IR-FUN:SET-SIGNATURE
      L-IMP SET-LNK
      V-EXPORT SET-VIS
      C-HABU SET-CC
   then
   k 3 = if
      c key sp sr br N-MAIN FUN-OPEN
      c tp tr key L-IMP V-EXPORT C-HABU FUN-DECL
      c other ATT-NEW {: ap2:IR-ARENA:arena ar2:IR-ARENA:arena :}
      c ap2 ar2 other 42 IR-ATTR:INT IR-FUN:ADD-FUN-ATTR
   then
   k 4 = if
      c key sp sr br N-MAIN FUN-OPEN
      c tp tr key L-IMP V-EXPORT C-HABU FUN-DECL
      other 0 IR-ID:PACK-SOURCE 0 4 IR--SOURCE-SPAN:MAKE IR-FUN:SET-FUN-SPAN
      c fp fr br key sr tr ar sa IR-FUN:END-FUN drop
      exit
   then
   c key sa fp fr br sr tr ar FUN-CLOSE drop ;

: FTAB-RUN ( n -- )
   BND [: FTAB-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A store built under one context, read with a key minted by another.
: FCTX-KEY ( IR-CTX:ctx -- IR-ID:ir-module-key )
   IR-CTX:NEW-MODULE drop ;

: FCTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   BND [: FCTX-KEY ;] IR-CTX:WITH-CONTEXT {: other:IR-ID:ir-module-key :}
   fr other  key 0 IR-ID:PACK-FUN  IR-FUN:SYMBOL@ drop ;

: FCTX-RUN ( -- )
   BND [: FCTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES-A ( -- )
   s" a foreign module key rejects at the function table" T-LABEL
   [: 1 FKEY-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" a foreign module key rejects at the block table" T-LABEL
   [: 2 FKEY-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" another module's block table cannot answer this function's window" T-LABEL
   [: 3 FKEY-RUN ;] E-IR-FUN-OWNER TTHROWSQ ;

: OWNER-CASES-B ( -- )
   s" another module's attribute pool cannot be read through these windows" T-LABEL
   [: 4 FKEY-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" another module's function symbol rejects" T-LABEL
   [: 1 FTAB-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" another module's signature type rejects" T-LABEL
   [: 2 FTAB-RUN ;] E-IR-FUN-OWNER TTHROWSQ ;

: OWNER-CASES-C ( -- )
   s" another module's function attribute rejects" T-LABEL
   [: 3 FTAB-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" another module's source span rejects" T-LABEL
   [: 4 FTAB-RUN ;] E-IR-FUN-OWNER TTHROWSQ
   s" a key minted by another context does not open this store" T-LABEL
   [: FCTX-RUN ;] E-IR-FUN-OWNER TTHROWSQ ;

\ ---- capacity and overflow ---------------------------------------------------
: CAP-BODY ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx fcap:n bcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key fcap bcap pcap IR-FUN:NEW
   IR-ARENA:ABORT IR-ARENA:ABORT IR-ARENA:ABORT ;

: CAP-ZERO-RUN ( -- )   BND [: 0 8 32 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-BIG-RUN ( -- )    BND [: $100000000 8 32 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-BNEG-RUN ( -- )   BND [: 8 -1 32 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-PNEG-RUN ( -- )   BND [: 8 8 -1 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-CASES ( -- )
   s" a zero function capacity rejects at creation" T-LABEL
   [: CAP-ZERO-RUN ;] E-IR-FUN-CAP TTHROWSQ
   s" a function capacity past the ordinal range rejects" T-LABEL
   [: CAP-BIG-RUN ;] E-IR-FUN-CAP TTHROWSQ
   s" a negative block capacity rejects" T-LABEL
   [: CAP-BNEG-RUN ;] E-IR-FUN-CAP TTHROWSQ
   s" a negative attribute pool capacity rejects" T-LABEL
   [: CAP-PNEG-RUN ;] E-IR-FUN-CAP TTHROWSQ ;

\ The caught quotation re-pushes its inputs before the throwing call, so the
\ stores stay readable after the named reject and can show that nothing was
\ written.
: OVF-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   c fp fr br key sr tr ar sa
   c fp fr br key sr tr ar sa IR-FUN:END-FUN drop ;

: OVF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 1 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   c key sp sr br N-HELP FUN-OPEN
   c tp tr key L-IMP V-EXPORT C-HABU FUN-DECL
   c sa key A-SPAN IR-FUN:SET-FUN-SPAN
   c fp fr br key sr tr ar sa [: OVF-TRY ;] catch
   {: c2:IR-CTX:ctx fp2:IR-ARENA:arena fr2:IR-ARENA:arena br2:IR-ARENA:arena key2:IR-ID:ir-module-key sr2:IR-ARENA:arena tr2:IR-ARENA:arena ar2:IR-ARENA:arena sa2:IR-ARENA:arena rc:n :}
   rc
   fr2 IR-FUN:FUNS
   br2 IR-FUN:BLOCKS ;

: OVF-BLK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 1 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-MAIN FUN-OPEN
   c tp tr key L-DEF V-HIDDEN C-HABU FUN-DECL
   r IR-FUN:BEGIN-BLOCK
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE drop
   r IR-FUN:BEGIN-BLOCK
   c key sp sr tp tr ar sa qr p v r K-RET OP+ drop
   c key sa v r qr fr br BLK-CLOSE drop ;

: OVF-ATT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 1 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-EXT FUN-OPEN
   c tp tr key L-IMP V-EXPORT C-HABU FUN-DECL
   c ap ar key 42 IR-ATTR:INT IR-FUN:ADD-FUN-ATTR
   c ap ar key 43 IR-ATTR:INT IR-FUN:ADD-FUN-ATTR
   c key sa fp fr br sr tr ar FUN-CLOSE drop ;

: OVF-BLK-RUN ( -- )   BND [: OVF-BLK-BODY ;] IR-CTX:WITH-CONTEXT ;
: OVF-ATT-RUN ( -- )   BND [: OVF-ATT-BODY ;] IR-CTX:WITH-CONTEXT ;

: OVF-CASES ( -- )
   s" an append past the function ceiling rejects and writes nothing" T-LABEL
   BND [: OVF-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= E-IR-FUN-CAP T=
   s" an append past the block ceiling rejects" T-LABEL
   [: OVF-BLK-RUN ;] E-IR-FUN-CAP TTHROWSQ
   CLEAR-STAGE
   s" an append past the attribute pool ceiling rejects" T-LABEL
   [: OVF-ATT-RUN ;] E-IR-FUN-CAP TTHROWSQ ;

\ ---- frozen modules ----------------------------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   key 0 IR-ID:PACK-FUN {: f0:IR-ID:ir-fun-id :}
   key 0 IR-ID:PACK-BLOCK {: b0:IR-ID:ir-block-id :}
   v IR-ARENA:FREEZE {: vv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   fp IR-ARENA:FREEZE {: fpv:IR-ARENA:view :}
   fr IR-ARENA:FREEZE {: frv:IR-ARENA:view :}
   br IR-ARENA:FREEZE {: brv:IR-ARENA:view :}
   frv IR-FUN:FFUNS
   brv IR-FUN:FBLOCKS
   fpv IR-FUN:FATTR-CELLS
   brv b0 IR-FUN:FARG-COUNT
   frv brv key f0 0 IR-FUN:FBLOCK@ IR-ID:BLOCK-LOCAL 0 =
   brv frv key b0 IR-FUN:FPARENT@ IR-ID:FUN-LOCAL 0 =
   brv rv key b0 IR-FUN:FTERMINATOR@ IR-ID:OP-LOCAL 0 = ;

: FZ-CASE ( -- )
   s" a frozen module serves the function readers through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 1 T= 0 T= 1 T= 1 T= ;

: FZ2-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr br N-EXT FUN-OPEN
   c tp tr key L-IMP V-EXPORT C-C FUN-DECL
   c ap ar key 42 IR-ATTR:INT IR-FUN:ADD-FUN-ATTR
   c key sa fp fr br sr tr ar FUN-CLOSE {: f0:IR-ID:ir-fun-id :}
   c ap ar key 42 IR-ATTR:INT IR-ID:ATTR-LOCAL {: at:n :}
   c sp sr key N-EXT FN-SYM IR-ID:SYMBOL-LOCAL {: sy:n :}
   c tp tr key SIGT IR-ID:TYPE-LOCAL {: ty:n :}
   fp IR-ARENA:FREEZE {: fpv:IR-ARENA:view :}
   fr IR-ARENA:FREEZE {: frv:IR-ARENA:view :}
   frv f0 IR-FUN:FATTR-COUNT
   frv f0 IR-FUN:FBLOCK-COUNT
   fpv frv key f0 0 IR-FUN:FATTR@ IR-ID:ATTR-LOCAL at =
   frv key f0 IR-FUN:FSYMBOL@ IR-ID:SYMBOL-LOCAL sy =
   frv key f0 IR-FUN:FSIGNATURE@ IR-ID:TYPE-LOCAL ty =
   frv f0 IR-FUN:FLINKAGE@ IR--FUN-LINKAGE:IMPORTED IR--FUN-LINKAGE:EQ
   frv f0 IR-FUN:FVISIBILITY@ IR--FUN-VISIBILITY:EXPORTED IR--FUN-VISIBILITY:EQ ;

: FZ2-CASE ( -- )
   s" every remaining frozen function reader answers through the views" T-LABEL
   BND [: FZ2-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 0 T= 1 T= ;

: FZ3-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   key 0 IR-ID:PACK-FUN {: f0:IR-ID:ir-fun-id :}
   key 0 IR-ID:PACK-BLOCK {: b0:IR-ID:ir-block-id :}
   v IR-ARENA:FREEZE {: vv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   fr IR-ARENA:FREEZE {: frv:IR-ARENA:view :}
   br IR-ARENA:FREEZE {: brv:IR-ARENA:view :}
   brv b0 IR-FUN:FOP-COUNT
   brv vv key b0 0 IR-FUN:FARG@ IR-ID:VALUE-LOCAL 0 =
   brv rv key b0 0 IR-FUN:FOP@ IR-ID:OP-LOCAL 0 =
   frv f0 IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:HABU IR--FUN-CONVENTION:EQ ;

: FZ3-CASE ( -- )
   s" the frozen block readers answer through the views" T-LABEL
   BND [: FZ3-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 1 T= ;

\ The retired builder handles reject every touch once the module is frozen, so
\ there is no public mutation left for a freeze to retract.
: FZ-RETIRED-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c key sp sr tp tr ar sa qr p v r fp fr br N-MAIN ONE-FUN
   fp IR-ARENA:FREEZE drop
   fr IR-ARENA:FREEZE drop
   br IR-ARENA:FREEZE drop
   k 1 = if fr IR-FUN:FUNS drop then
   k 2 = if c key sp sr tp tr ar sa qr p v r fp fr br N-HELP ONE-FUN then ;

: FZ-RETIRED-RUN ( n -- )
   BND [: FZ-RETIRED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- teardown ----------------------------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c 8 8 32 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   fr ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-FUN:FUNS drop ;

: FROZEN-CASES ( -- )
   s" live readers reject the retired builder handle" T-LABEL
   [: 1 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" appending through retired frozen handles rejects" T-LABEL
   [: 2 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ
   CLEAR-STAGE
   s" a function store is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and stores succeed after teardown" T-LABEL
   3 0 ?do
      BND [: IMPORT-BODY ;] IR-CTX:WITH-CONTEXT
      TTRUE 0 T= 0 T=
   loop ;

\ ---- the checker keeps the API sealed ----------------------------------------
\ The view fixtures are the standing proof that no public mutation can be
\ spelled against a frozen store: every writing word demands a builder handle,
\ and the checker refuses an IR-ARENA:view in that position.
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRF-POS ( IR-ARENA:arena -- n ) IR-FUN:FUNS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRF-RAW-ID ( IR-ARENA:arena n -- n ) IR-FUN:BLOCK-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-VIEW-AS-ARENA ( IR-ARENA:view IR-ID:ir-fun-id -- n ) IR-FUN:BLOCK-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-FUN-AS-BLOCK ( IR-ARENA:arena IR-ID:ir-fun-id -- n ) IR-FUN:OP-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-VIEW-APPEND ( IR-CTX:ctx IR-ARENA:view IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-fun-id ) IR-FUN:END-FUN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-VIEW-CREATE ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:view IR-ARENA:arena IR-ARENA:arena ) IR-FUN:NEW"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-block-id ) IR-FUN:END-BLOCK"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-RAW-LINKAGE ( n -- ) IR-FUN:SET-LINKAGE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-VIS-AS-LINKAGE ( IR-FUN:visibility -- ) IR-FUN:SET-LINKAGE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-RAW-SPAN ( n -- ) IR-FUN:SET-FUN-SPAN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRF-BLOCK-AS-FUN ( IR-ARENA:arena IR-ID:ir-block-id -- IR-FUN:linkage ) IR-FUN:LINKAGE@"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside an outermost harness context, so a context
\ abandoned by a throw is reclaimed by that harness exit instead of holding its
\ arena registry slots for the rest of the process. The groups are small because
\ each rigged module holds fifteen arenas, and no one harness may accumulate
\ more leaked slots than the registry holds.
: HARNESS-FIELD ( IR-CTX:ctx -- )     drop  FIELD-CASES ;
: HARNESS-FIELD-B ( IR-CTX:ctx -- )   drop  FIELD-CASES-B ;
: HARNESS-LINKAGE ( IR-CTX:ctx -- )   drop  LINKAGE-CASES ;
: HARNESS-TARGET ( IR-CTX:ctx -- )    drop  TARGET-CASES ;
: HARNESS-TERM ( IR-CTX:ctx -- )      drop  TERM-CASES ;
: HARNESS-ORDER ( IR-CTX:ctx -- )     drop  ORDER-CASES ;
: HARNESS-STAGE ( IR-CTX:ctx -- )     drop  STAGE-CASES ;
: HARNESS-STAGE-B ( IR-CTX:ctx -- )   drop  STAGE-CASES-B ;
: HARNESS-IDX ( IR-CTX:ctx -- )       drop  IDX-CASES ;
: HARNESS-IDX-B ( IR-CTX:ctx -- )     drop  IDX-CASES-B ;
: HARNESS-STATE ( IR-CTX:ctx -- )     drop  STATE-CASES ;
: HARNESS-FORGE ( IR-CTX:ctx -- )     drop  FORGE-CASES ;
: HARNESS-FORGE-B ( IR-CTX:ctx -- )   drop  FORGE-CASES-B ;
: HARNESS-CROSS ( IR-CTX:ctx -- )     drop  CROSS-CASES ;
: HARNESS-BFORGE ( IR-CTX:ctx -- )    drop  BFORGE-CASES ;
: HARNESS-OWNER-A ( IR-CTX:ctx -- )   drop  OWNER-CASES-A ;
: HARNESS-OWNER-B ( IR-CTX:ctx -- )   drop  OWNER-CASES-B ;
: HARNESS-OWNER-C ( IR-CTX:ctx -- )   drop  OWNER-CASES-C ;
: HARNESS-CAP ( IR-CTX:ctx -- )       drop  CAP-CASES ;
: HARNESS-OVF ( IR-CTX:ctx -- )       drop  OVF-CASES ;
: HARNESS-FROZEN ( IR-CTX:ctx -- )    drop  FROZEN-CASES ;

public

: RUN ( -- )
   T-RESET
   READ-CASE
   BLOCK-CASE
   ARGVAL-CASE
   IMPORT-CASE
   TWO-CASE
   FZ-CASE
   FZ2-CASE
   FZ3-CASE
   BND [: HARNESS-FIELD ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FIELD-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LINKAGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-TARGET ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-TERM ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-ORDER ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-IDX ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-IDX-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STATE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FORGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FORGE-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-CROSS ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-BFORGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-A ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-C ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-CAP ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OVF ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FROZEN ;] IR-CTX:WITH-CONTEXT
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-FUN-TEST:RUN
