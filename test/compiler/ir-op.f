\ ir-op.f - checked compiler operation and value store tests.
\
\ Proves the contract of src/compiler/ir/op.f: an appended operation reads back
\ exactly the opcode, windows, span, and result values it was given, and each
\ result value knows its type, its defining operation, and its position; an
\ operand may only name a value the module has already defined; operand, result,
\ and successor counts must agree with the opcode's schema, including the
\ variadic-tail rule; a window read revalidates the row's exact tiling of the
\ pool, so a row appended past this package's constructors can neither overlap
\ another operation's window, leave a gap, nor reach past the cells the pool
\ holds; indices past a count or a window length reject; module keys and
\ identities from another module or another context reject; each of the three
\ committed ceilings rejects an append and leaves no partial record behind; a
\ frozen module serves every reader through the arena views while the retired
\ builders reject; context teardown releases everything; and checker fixtures
\ prove no mutation can be spelled against a frozen store.

require lib/test.f
require test/checker-assert.f
require src/compiler/ir/op.f

package IR-OP-TEST
private

\ The row shapes src/compiler/ir/op.f commits to, mirrored here so a fixture can
\ append a raw row past that package's constructors and prove the readers still
\ hold. A change to the layout must change this mirror too.
12 constant ROW-CELLS
0 constant F-OPC
1 constant F-OPST
2 constant F-OPN
3 constant F-RSST
4 constant F-RSN
5 constant F-SCST
6 constant F-SCN
7 constant F-ATST
8 constant F-ATN
9 constant F-SRC
10 constant F-SBEG
11 constant F-SLEN

4 constant VROW-CELLS
0 constant F-VTYP
1 constant F-VKIND
2 constant F-VDEF
3 constant F-VPOS

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
\ Five opcodes, chosen to cover every arity rule the store enforces.
0 constant K-CONST                   \ no operands, one result
1 constant K-ADD                     \ two operands, one result
2 constant K-CALL                    \ a variadic operand tail, one result
3 constant K-BR                      \ a terminator with one successor
4 constant K-VOID                    \ neither operands nor results

: OPC-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-CONST = if c sp sr key s" hir.const" IR-SYM:INTERN exit then
   k K-ADD = if c sp sr key s" hir.add" IR-SYM:INTERN exit then
   k K-CALL = if c sp sr key s" hir.call" IR-SYM:INTERN exit then
   k K-BR = if c sp sr key s" hir.br" IR-SYM:INTERN exit then
   c sp sr key s" hir.void" IR-SYM:INTERN ;

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
   c key 8 IR-SOURCE:NEW ;

: I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: A-SPAN ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-SOURCE:span )
   {: c:IR-CTX:ctx sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   sa  c sa key s" op-source" IR-SOURCE:REGISTER  0 4 IR-SOURCE:SPAN ;

\ ---- the schema table --------------------------------------------------------
: SCH-NEW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 8 64 IR-SCHEMA:NEW ;

: SCH-SHAPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-CONST = if c tp tr key I64 IR-SCHEMA:ADD-RESULT exit then
   k K-CALL = if
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL
      c tp tr key I64 IR-SCHEMA:ADD-RESULT exit then
   k K-ADD = if
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND
      c tp tr key I64 IR-SCHEMA:ADD-RESULT
   then ;

: SCH-CTRL ( n -- )
   K-BR = if true 1 0 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-SCHEMA:BEGIN-OP
   c tp tr key k SCH-SHAPE
   k SCH-CTRL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key s" rule.hir" IR-SYM:INTERN IR-SCHEMA:SET-RULE
   c sp sr key s" render.hir" IR-SYM:INTERN IR-SCHEMA:SET-RENDERER
   c qp qr key sr tr IR-SCHEMA:DEFINE ;

: SCH-ALL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c sp sr tp tr key qp qr K-CONST SCH-DEF
   c sp sr tp tr key qp qr K-ADD SCH-DEF
   c sp sr tp tr key qp qr K-CALL SCH-DEF
   c sp sr tp tr key qp qr K-BR SCH-DEF
   c sp sr tp tr key qp qr K-VOID SCH-DEF ;

\ One rigged module: an interner, a type table, an attribute table, a source
\ registry, a schema table holding all five opcodes, and an operation store with
\ the requested ceilings. Every fixture starts here, so a fixture body differs
\ only in what it then asks the store to do.
: RIG ( IR-CTX:ctx n n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx ocap:n vcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c key ATT-NEW {: ap:IR-ARENA:arena ar:IR-ARENA:arena :}
   c key SRC-NEW {: sa:IR-ARENA:arena :}
   c sp sr key SCH-NEW {: qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c key ocap vcap pcap IR-OP:NEW {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key qp qr SCH-ALL
   key sp sr tp tr ap ar sa qr p v r ;

\ ---- the scenario vocabulary -------------------------------------------------
\ Every append fixture stages one scenario. A scenario names an opcode and the
\ operand, result, successor, and attribute counts it presents, so a fixture
\ differs from the legal shape in exactly one number.
0 constant S-SEED                    \ hir.const: the legal shape that mints a value
1 constant S-ADD                     \ hir.add over the two seeded values
2 constant S-FEW-OPN                 \ hir.add with one operand
3 constant S-MANY-OPN                \ hir.add with three operands
4 constant S-FEW-RSN                 \ hir.add with no result
5 constant S-MANY-RSN                \ hir.add with two results
6 constant S-CONST-OPN               \ hir.const with an operand it does not take
7 constant S-BR                      \ hir.br with its one successor
8 constant S-BR-0                    \ hir.br with no successor
9 constant S-BR-2                    \ hir.br with two successors
10 constant S-TAIL-0                 \ hir.call with no operand at all
11 constant S-TAIL-3                 \ hir.call with three operands
12 constant S-SSA                    \ an operand naming a value that is not defined yet
13 constant S-HUGE                   \ an operand list past the staged ceiling
14 constant S-NOSPAN                 \ no source span declared
15 constant S-ATTR                   \ hir.add carrying one attribute
16 constant S-VOID                   \ hir.void: neither operands nor results
17 constant S-NO-OPN                 \ hir.add with an empty operand window
18 constant S-BR-ATTR                \ hir.br with its successor and one attribute

: SC-OPC ( n -- n )
   {: s:n :}
   s S-SEED = s S-CONST-OPN = or if K-CONST exit then
   s S-BR = s S-BR-0 = or s S-BR-2 = or s S-BR-ATTR = or if K-BR exit then
   s S-TAIL-0 = s S-TAIL-3 = or if K-CALL exit then
   s S-VOID = if K-VOID exit then
   K-ADD ;

: SC-OPN ( n -- n )
   {: s:n :}
   s S-SEED = s S-BR = or s S-BR-0 = or s S-BR-2 = or if 0 exit then
   s S-TAIL-0 = s S-VOID = or s S-NO-OPN = or s S-BR-ATTR = or if 0 exit then
   s S-FEW-OPN = s S-CONST-OPN = or if 1 exit then
   s S-MANY-OPN = s S-TAIL-3 = or if 3 exit then
   s S-HUGE = if 65 exit then
   2 ;

: SC-RSN ( n -- n )
   {: s:n :}
   s S-BR = s S-BR-0 = or s S-BR-2 = or s S-BR-ATTR = or if 0 exit then
   s S-VOID = s S-FEW-RSN = or if 0 exit then
   s S-MANY-RSN = if 2 exit then
   1 ;

: SC-SCN ( n -- n )
   {: s:n :}
   s S-BR-0 = if 0 exit then
   s S-BR-2 = if 2 exit then
   s S-BR = s S-BR-ATTR = or if 1 exit then
   0 ;

: SC-ATN ( n -- n )
   {: s:n :}
   s S-ATTR = s S-BR-ATTR = or if 1 exit then
   0 ;

: SC-SPAN? ( n -- bool )
   S-NOSPAN <> ;

\ The seeded module defines values 0 and 1, so a scenario that starts one
\ ordinal later reaches an ordinal no operation has defined.
: SC-OPBASE ( n -- n )
   S-SSA = if 1 else 0 then ;

\ ---- staging one scenario ----------------------------------------------------
: STG-OPEN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key sa:IR-ARENA:arena s:n :}
   c sp sr key s SC-OPC OPC-SYM IR-OP:BEGIN-OP
   s SC-SPAN? if c sa key A-SPAN IR-OP:SET-SPAN then ;

: STG-VALS ( IR-ID:ir-module-key n -- )
   {: key:IR-ID:ir-module-key s:n :}
   s SC-OPN 0 ?do
      key s SC-OPBASE i + IR-ID:PACK-VALUE IR-OP:ADD-OPERAND
   loop
   s SC-SCN 0 ?do
      key i IR-ID:PACK-BLOCK IR-OP:ADD-SUCCESSOR
   loop ;

: STG-RES ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key s:n :}
   s SC-RSN 0 ?do
      c tp tr key I64 IR-OP:ADD-RESULT
   loop ;

: STG-ATT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx ap:IR-ARENA:arena ar:IR-ARENA:arena key:IR-ID:ir-module-key s:n :}
   s SC-ATN 0 ?do
      c ap ar key 42 IR-ATTR:INT IR-OP:ADD-ATTR
   loop ;

\ Stage and append one scenario against the rigged module.
: APPEND ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena s:n :}
   c sp sr key sa s STG-OPEN
   key s STG-VALS
   c tp tr key s STG-RES
   c ap ar key s STG-ATT
   c p v r key qr tr ar sa IR-OP:END-OP ;

\ ---- reading one appended operation back -------------------------------------
: READ-BODY ( IR-CTX:ctx -- n n n n n n bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-ATTR APPEND {: o2:IR-ID:ir-op-id :}
   r IR-OP:OPS
   v IR-OP:VALUES
   p IR-OP:POOL-CELLS
   r o2 IR-OP:OPERANDS
   r o2 IR-OP:RESULTS
   r o2 IR-OP:ATTRS
   p r key o2 0 IR-OP:OPERAND@ IR-ID:VALUE-LOCAL 0 =
   p r key o2 1 IR-OP:OPERAND@ IR-ID:VALUE-LOCAL 1 =
   p r key o2 0 IR-OP:ATTR@ IR-ID:ATTR-LOCAL
      c ap ar key 42 IR-ATTR:INT IR-ID:ATTR-LOCAL =
   r key o2 IR-OP:OPCODE@ IR-ID:SYMBOL-LOCAL
      c sp sr key K-ADD OPC-SYM IR-ID:SYMBOL-LOCAL = ;

: READ-CASE ( -- )
   s" an appended operation reads back the shape it was given" T-LABEL
   BND [: READ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE 1 T= 1 T= 2 T= 6 T= 3 T= 3 T= ;

: VALUE-BODY ( IR-CTX:ctx -- bool bool n bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-ADD APPEND {: o2:IR-ID:ir-op-id :}
   p r key o2 0 IR-OP:RESULT@ {: res:IR-ID:ir-value-id :}
   res IR-ID:VALUE-LOCAL 2 =
   v res IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:OP-RESULT IR--OP-DEF--KIND:EQ
   v res IR-OP:VALUE-POS@
   v key res IR-OP:VALUE-TYPE@ IR-ID:TYPE-LOCAL
      c tp tr key I64 IR-ID:TYPE-LOCAL =
   v r key res IR-OP:VALUE-OP@ IR-ID:OP-LOCAL o2 IR-ID:OP-LOCAL =
   r key o2 IR-OP:SPAN@ IR-SOURCE:SPAN-LEN 4 = ;

: VALUE-CASE ( -- )
   s" a result value knows its type, its operation, and its position" T-LABEL
   BND [: VALUE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 0 T= TTRUE TTRUE ;

\ A span is a value, so the reader hands one back and the consumer revalidates
\ it against the registry it names - which is exactly what IR-SOURCE demands.
: SPAN-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND {: o0:IR-ID:ir-op-id :}
   sa  r key o0 IR-OP:SPAN@  IR-SOURCE:SPAN-CK
   r key o0 IR-OP:SPAN@ IR-SOURCE:SPAN-START
   r key o0 IR-OP:SPAN@ IR-SOURCE:SPAN-LEN
   r key o0 IR-OP:SPAN@ IR-SOURCE:SPAN-SRC IR-ID:SOURCE-LOCAL 0 = ;

: SPAN-CASE ( -- )
   s" a stored source span revalidates against the registry it names" T-LABEL
   BND [: SPAN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 4 T= 0 T= ;

\ hir.br is a terminator: no results, one successor, and the successor block may
\ still be under construction, which is why it is the one forward reference the
\ store accepts.
: SUCC-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-BR APPEND {: o0:IR-ID:ir-op-id :}
   r o0 IR-OP:SUCCESSORS
   r o0 IR-OP:RESULTS
   p r key o0 0 IR-OP:SUCCESSOR@ IR-ID:BLOCK-LOCAL 0 = ;

: SUCC-CASE ( -- )
   s" a terminator records its successor block and no result" T-LABEL
   BND [: SUCC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= ;

\ The variadic tail describes every operand past the fixed ones, and may
\ describe none, so both a shorter and a longer call are legal.
: TAIL-BODY ( n IR-CTX:ctx -- n )
   {: s:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r s APPEND {: o:IR-ID:ir-op-id :}
   r o IR-OP:OPERANDS ;

: TAIL-CASE ( -- )
   s" a variadic tail accepts no operand at all" T-LABEL
   S-TAIL-0 BND [: TAIL-BODY ;] IR-CTX:WITH-CONTEXT 0 T=
   s" a variadic tail accepts more operands than the schema lists" T-LABEL
   S-TAIL-3 BND [: TAIL-BODY ;] IR-CTX:WITH-CONTEXT 3 T= ;

: VOID-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-VOID APPEND drop
   r IR-OP:OPS
   v IR-OP:VALUES
   p IR-OP:POOL-CELLS ;

: VOID-CASE ( -- )
   s" an operation with neither operands nor results mints no value" T-LABEL
   BND [: VOID-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= ;

\ ---- the negative append body ------------------------------------------------
: NEG-BODY ( n IR-CTX:ctx -- )
   {: s:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r s APPEND drop ;

: NEG-RUN ( n -- )
   BND [: NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A throw inside a staged declaration leaves the operation open, because only an
\ end - END-OP or ABANDON - consumes the stage. The next BEGIN-OP therefore
\ rejects until the caller abandons, which is what this does between fixtures.
: CLEAR-STAGE ( -- )
   [: IR-OP:ABANDON ;] catch drop ;

: ARITY-CASES-A ( -- )
   s" an operand short of the schema's count rejects" T-LABEL
   [: S-FEW-OPN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" an operand past the schema's count rejects" T-LABEL
   [: S-MANY-OPN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" a result short of the schema's count rejects" T-LABEL
   [: S-FEW-RSN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" a result past the schema's count rejects" T-LABEL
   [: S-MANY-RSN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" an empty operand window where the schema demands operands rejects" T-LABEL
   [: S-NO-OPN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ ;

: ARITY-CASES-B ( -- )
   s" an operand on an opcode that takes none rejects" T-LABEL
   [: S-CONST-OPN NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" a terminator with no successor rejects" T-LABEL
   [: S-BR-0 NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" a terminator with a successor too many rejects" T-LABEL
   [: S-BR-2 NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   s" an operand list past the staged ceiling rejects" T-LABEL
   [: S-HUGE NEG-RUN ;] E-IR-OP-ARITY TTHROWSQ
   CLEAR-STAGE ;

: SSA-CASES ( -- )
   s" an operand naming a value no operation has defined rejects" T-LABEL
   [: S-SSA NEG-RUN ;] E-IR-OP-SSA TTHROWSQ
   s" an operation with no source span rejects" T-LABEL
   [: S-NOSPAN NEG-RUN ;] E-IR-OP-FIELD TTHROWSQ ;

\ ---- the stage protocol ------------------------------------------------------
: STG-TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key K-ADD OPC-SYM IR-OP:BEGIN-OP
   c sp sr key K-ADD OPC-SYM IR-OP:BEGIN-OP ;

: STG-LOOSE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key 0 IR-ID:PACK-VALUE IR-OP:ADD-OPERAND ;

: STG-SPAN2-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key SRC-NEW {: sa:IR-ARENA:arena :}
   c sp sr key K-CONST OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   c sa key A-SPAN IR-OP:SET-SPAN ;

: STG-ABANDON-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key K-CONST OPC-SYM IR-OP:BEGIN-OP
   IR-OP:ABANDON
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   r IR-OP:OPS ;

: STG-TWICE-RUN ( -- )   BND [: STG-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;
: STG-LOOSE-RUN ( -- )   BND [: STG-LOOSE-BODY ;] IR-CTX:WITH-CONTEXT ;
: STG-SPAN2-RUN ( -- )   BND [: STG-SPAN2-BODY ;] IR-CTX:WITH-CONTEXT ;
: STG-NONE-RUN ( -- )    IR-OP:ABANDON ;

: STAGE-CASES ( -- )
   s" declaring an operand with no operation open rejects" T-LABEL
   [: STG-LOOSE-RUN ;] E-IR-OP-STAGE TTHROWSQ
   s" opening an operation while one is open rejects" T-LABEL
   [: STG-TWICE-RUN ;] E-IR-OP-STAGE TTHROWSQ
   CLEAR-STAGE
   s" declaring the source span twice rejects" T-LABEL
   [: STG-SPAN2-RUN ;] E-IR-OP-STAGE TTHROWSQ
   CLEAR-STAGE
   s" ending an operation that was never opened rejects" T-LABEL
   [: STG-NONE-RUN ;] E-IR-OP-STAGE TTHROWSQ
   s" abandoning an operation leaves the next one free to open" T-LABEL
   BND [: STG-ABANDON-BODY ;] IR-CTX:WITH-CONTEXT 1 T= ;

\ ---- bounds ------------------------------------------------------------------
: IDX-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-ADD APPEND {: o:IR-ID:ir-op-id :}
   k 1 = if p r key o 2 IR-OP:OPERAND@ drop then
   k 2 = if p r key o -1 IR-OP:OPERAND@ drop then
   k 3 = if r  key 3 IR-ID:PACK-OP  IR-OP:OPERANDS drop then
   k 4 = if v key  key 3 IR-ID:PACK-VALUE  IR-OP:VALUE-TYPE@ drop then ;

: IDX-RUN ( n -- )
   BND [: IDX-BODY ;] IR-CTX:WITH-CONTEXT ;

: IDX-CASES ( -- )
   s" an operand index past the window rejects" T-LABEL
   [: 1 IDX-RUN ;] E-IR-OP-BOUND TTHROWSQ
   s" a negative operand index rejects" T-LABEL
   [: 2 IDX-RUN ;] E-IR-OP-BOUND TTHROWSQ
   s" an operation id past the appended count rejects" T-LABEL
   [: 3 IDX-RUN ;] E-IR-OP-BOUND TTHROWSQ
   s" a value id past the minted count rejects" T-LABEL
   [: 4 IDX-RUN ;] E-IR-OP-BOUND TTHROWSQ ;

\ ---- non-table, misaligned, and forged rows ----------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-OP:OPS drop ;

\ The three stores are not interchangeable: the pool presented as the operation
\ table is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   p IR-OP:OPS drop ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c r 7 IR-ARENA:PUSH drop
   r IR-OP:OPS drop ;

\ The value one raw forged operation cell takes: the fields of an empty row that
\ continues exactly where the last real row ended, with cell `off` replaced.
: FROW-CELL ( n n n n -- n )
   {: i:n base:n off:n val:n :}
   i off = if val exit then
   i F-OPST = if base exit then
   i F-RSST = if base exit then
   i F-SCST = if base exit then
   i F-ATST = if base exit then
   0 ;

: FORGE-ROW ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena base:n off:n val:n :}
   ROW-CELLS 0 ?do
      c r  i base off val FROW-CELL  IR-ARENA:PUSH drop
   loop ;

: FORGE-BODY ( n n n IR-CTX:ctx -- )
   {: off:n val:n k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   r IR-OP:OPS {: l:n :}
   c r p IR-OP:POOL-CELLS off val FORGE-ROW
   key l IR-ID:PACK-OP {: bad:IR-ID:ir-op-id :}
   k 1 = if p r key bad 0 IR-OP:OPERAND@ drop then
   k 2 = if r bad IR-OP:OPERANDS drop then
   k 3 = if r key bad IR-OP:SPAN@ drop then ;

: FORGE-RUN ( n n n -- )
   BND [: FORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: RAW-RUN ( -- )     BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;
: SWAP-RUN ( -- )    BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHAPE-RUN ( -- )   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not an operation table" T-LABEL
   [: RAW-RUN ;] E-IR-OP-STATE TTHROWSQ
   s" the cell pool presented as the operation table rejects" T-LABEL
   [: SWAP-RUN ;] E-IR-OP-STATE TTHROWSQ
   s" a misaligned operation row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-OP-STATE TTHROWSQ ;

: WINDOW-CASES ( -- )
   s" a forged window starting before the previous row ended rejects" T-LABEL
   [: F-OPST 1 1 FORGE-RUN ;] E-IR-OP-WINDOW TTHROWSQ
   s" a forged window leaving a gap after the previous row rejects" T-LABEL
   [: F-OPST 3 1 FORGE-RUN ;] E-IR-OP-WINDOW TTHROWSQ
   s" a forged window reaching past the pool rejects" T-LABEL
   [: F-ATN 99 1 FORGE-RUN ;] E-IR-OP-STATE TTHROWSQ
   s" a forged negative window length rejects" T-LABEL
   [: F-OPN -1 2 FORGE-RUN ;] E-IR-OP-STATE TTHROWSQ ;

: FVROW-CELL ( n n n -- n )
   {: i:n off:n val:n :}
   i off = if val exit then
   0 ;

: FORGE-VROW ( IR-CTX:ctx IR-ARENA:arena n n -- )
   {: c:IR-CTX:ctx v:IR-ARENA:arena off:n val:n :}
   VROW-CELLS 0 ?do
      c v  i off val FVROW-CELL  IR-ARENA:PUSH drop
   loop ;

: FVROW-BODY ( n n n IR-CTX:ctx -- )
   {: off:n val:n k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   v IR-OP:VALUES {: n0:n :}
   c v off val FORGE-VROW
   key n0 IR-ID:PACK-VALUE {: bad:IR-ID:ir-value-id :}
   k 1 = if v r key bad IR-OP:VALUE-OP@ drop then
   k 2 = if v bad IR-OP:VALUE-KIND@ drop then
   k 3 = if r key  key 9 IR-ID:PACK-OP  IR-OP:SPAN@ drop then ;

: FVROW-RUN ( n n n -- )
   BND [: FVROW-BODY ;] IR-CTX:WITH-CONTEXT ;

: KIND-CASES ( -- )
   s" a forged block-argument value refuses the operation-result reader" T-LABEL
   [: F-VKIND 1 1 FVROW-RUN ;] E-IR-OP-KIND TTHROWSQ
   s" a forged definition kind outside the vocabulary rejects" T-LABEL
   [: F-VKIND 7 2 FVROW-RUN ;] E-IR-OP-STATE TTHROWSQ
   s" a forged negative source ordinal rejects at the span reader" T-LABEL
   [: F-SRC -3 3 FORGE-RUN ;] E-IR-OP-STATE TTHROWSQ ;

\ ---- cross-owner references --------------------------------------------------
\ Stage a legal hir.add whose first operand belongs to another module.
: FOREIGN-OPERAND ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key sa:IR-ARENA:arena other:IR-ID:ir-module-key :}
   c sp sr key K-ADD OPC-SYM IR-OP:BEGIN-OP
   other 0 IR-ID:PACK-VALUE IR-OP:ADD-OPERAND
   key 0 IR-ID:PACK-VALUE IR-OP:ADD-OPERAND
   c tp tr key I64 IR-OP:ADD-RESULT
   c sa key A-SPAN IR-OP:SET-SPAN ;

\ Stage a legal hir.br whose successor block belongs to another module.
: FOREIGN-SUCC ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key sa:IR-ARENA:arena other:IR-ID:ir-module-key :}
   c sp sr key K-BR OPC-SYM IR-OP:BEGIN-OP
   other 0 IR-ID:PACK-BLOCK IR-OP:ADD-SUCCESSOR
   c sa key A-SPAN IR-OP:SET-SPAN ;

\ Stage a legal hir.const whose source span names another module's registry.
: FOREIGN-SPAN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key other:IR-ID:ir-module-key :}
   c sp sr key K-CONST OPC-SYM IR-OP:BEGIN-OP
   c tp tr key I64 IR-OP:ADD-RESULT
   other 0 IR-ID:PACK-SOURCE 0 4 IR--SOURCE-SPAN:MAKE IR-OP:SET-SPAN ;

: FKEY-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   k 1 = if r other  key 0 IR-ID:PACK-OP  IR-OP:OPCODE@ drop then
   k 2 = if c sp sr tp tr key sa other FOREIGN-OPERAND then
   k 3 = if c sp sr key sa other FOREIGN-SUCC then
   k 4 = if c sp sr tp tr key other FOREIGN-SPAN then
   k 5 = if
      c other 4 4 8 IR-OP:NEW {: p2:IR-ARENA:arena v2:IR-ARENA:arena r2:IR-ARENA:arena :}
      p2 r key  key 0 IR-ID:PACK-OP  0 IR-OP:OPERAND@ drop
   then
   k 1 = k 5 = or 0= if c p v r key qr tr ar sa IR-OP:END-OP drop then ;

: FKEY-RUN ( n -- )
   BND [: FKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES-A ( -- )
   s" a foreign module key rejects at the operation table" T-LABEL
   [: 1 FKEY-RUN ;] E-IR-OP-OWNER TTHROWSQ
   s" another module's operand value rejects" T-LABEL
   [: 2 FKEY-RUN ;] E-IR-OP-OWNER TTHROWSQ
   s" another module's successor block rejects" T-LABEL
   [: 3 FKEY-RUN ;] E-IR-OP-OWNER TTHROWSQ
   s" another module's source span rejects" T-LABEL
   [: 4 FKEY-RUN ;] E-IR-OP-OWNER TTHROWSQ ;

: OWNER-CASES-D ( -- )
   s" another module's cell pool cannot be read through these windows" T-LABEL
   [: 5 FKEY-RUN ;] E-IR-OP-OWNER TTHROWSQ ;

\ The remaining cross-owner rejects need a second module's own tables, so they
\ carry a second interner, type table, and attribute table.
: FTAB-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   k 1 = if
      c other SYM-NEW {: sp2:IR-ARENA:arena sr2:IR-ARENA:arena :}
      c sp2 sr2 other K-CONST OPC-SYM IR-OP:BEGIN-OP
      c tp tr key I64 IR-OP:ADD-RESULT
   then
   k 2 = if
      c sp sr key K-CONST OPC-SYM IR-OP:BEGIN-OP
      c other TYP-NEW {: tp2:IR-ARENA:arena tr2:IR-ARENA:arena :}
      c tp2 tr2 other I64 IR-OP:ADD-RESULT
   then
   k 3 = if
      c sp sr key K-CONST OPC-SYM IR-OP:BEGIN-OP
      c tp tr key I64 IR-OP:ADD-RESULT
      c other ATT-NEW {: ap2:IR-ARENA:arena ar2:IR-ARENA:arena :}
      c ap2 ar2 other 42 IR-ATTR:INT IR-OP:ADD-ATTR
   then
   c sa key A-SPAN IR-OP:SET-SPAN
   c p v r key qr tr ar sa IR-OP:END-OP drop ;

: FTAB-RUN ( n -- )
   BND [: FTAB-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A store built under one context, read with a key minted by another.
: FCTX-KEY ( IR-CTX:ctx -- IR-ID:ir-module-key )
   IR-CTX:NEW-MODULE drop ;

: FCTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   BND [: FCTX-KEY ;] IR-CTX:WITH-CONTEXT {: other:IR-ID:ir-module-key :}
   r other  key 0 IR-ID:PACK-OP  IR-OP:OPCODE@ drop ;

: FCTX-RUN ( -- )
   BND [: FCTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES-B ( -- )
   s" another module's opcode symbol rejects" T-LABEL
   [: 1 FTAB-RUN ;] E-IR-OP-OWNER TTHROWSQ
   s" another module's result type rejects" T-LABEL
   [: 2 FTAB-RUN ;] E-IR-OP-OWNER TTHROWSQ ;

: OWNER-CASES-C ( -- )
   s" another module's attribute rejects" T-LABEL
   [: 3 FTAB-RUN ;] E-IR-OP-OWNER TTHROWSQ
   s" a key minted by another context does not open this store" T-LABEL
   [: FCTX-RUN ;] E-IR-OP-OWNER TTHROWSQ ;

\ ---- capacity and overflow ---------------------------------------------------
: CAP-BODY ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx ocap:n vcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key ocap vcap pcap IR-OP:NEW
   IR-ARENA:ABORT IR-ARENA:ABORT IR-ARENA:ABORT ;

: OVF-CAPS ( n -- n n n )
   {: k:n :}
   k 1 = if 2 16 128 exit then
   k 2 = if 16 2 128 exit then
   16 16 2 ;

\ The caught quotation re-pushes its inputs before the throwing call, so the
\ three stores stay readable after the named reject and can show that nothing
\ was written.
: OVF-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key qr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   c p v r key qr tr ar sa
   c p v r key qr tr ar sa IR-OP:END-OP drop ;

: OVF-BODY ( n IR-CTX:ctx -- n n n n )
   {: k:n c:IR-CTX:ctx :}
   c k OVF-CAPS RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c sp sr key sa S-SEED STG-OPEN
   key S-SEED STG-VALS
   c tp tr key S-SEED STG-RES
   c ap ar key S-SEED STG-ATT
   c p v r key qr tr ar sa [: OVF-TRY ;] catch
   {: c2:IR-CTX:ctx p2:IR-ARENA:arena v2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key qr2:IR-ARENA:arena tr2:IR-ARENA:arena ar2:IR-ARENA:arena sa2:IR-ARENA:arena rc:n :}
   rc
   r2 IR-OP:OPS
   v2 IR-OP:VALUES
   p2 IR-OP:POOL-CELLS ;

: OVF-CASE ( n -- )
   BND [: OVF-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 2 T= 2 T= E-IR-OP-CAP T= ;

: CAP-ZERO-RUN ( -- )   BND [: 0 16 128 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-BIG-RUN ( -- )    BND [: $100000000 16 128 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-VNEG-RUN ( -- )   BND [: 8 -1 128 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-PNEG-RUN ( -- )   BND [: 8 16 -1 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-CASES ( -- )
   s" a zero operation capacity rejects at creation" T-LABEL
   [: CAP-ZERO-RUN ;] E-IR-OP-CAP TTHROWSQ
   s" an operation capacity past the ordinal range rejects" T-LABEL
   [: CAP-BIG-RUN ;] E-IR-OP-CAP TTHROWSQ
   s" a negative value capacity rejects" T-LABEL
   [: CAP-VNEG-RUN ;] E-IR-OP-CAP TTHROWSQ
   s" a negative pool capacity rejects" T-LABEL
   [: CAP-PNEG-RUN ;] E-IR-OP-CAP TTHROWSQ ;

: OVF-CASES ( -- )
   s" an append past the operation ceiling rejects and writes nothing" T-LABEL
   1 OVF-CASE
   s" an append past the value ceiling rejects and writes nothing" T-LABEL
   2 OVF-CASE
   s" an append past the pool ceiling rejects and writes nothing" T-LABEL
   3 OVF-CASE ;

\ ---- frozen modules ----------------------------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n n n n bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   c key sp sr tp tr ap ar sa qr p v r S-ADD APPEND {: o2:IR-ID:ir-op-id :}
   p IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   v IR-ARENA:FREEZE {: vv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv IR-OP:FOPS
   vv IR-OP:FVALUES
   pv IR-OP:FPOOL-CELLS
   rv o2 IR-OP:FOPERANDS
   pv rv key o2 1 IR-OP:FOPERAND@ IR-ID:VALUE-LOCAL 1 =
   vv rv key  pv rv key o2 0 IR-OP:FRESULT@  IR-OP:FVALUE-OP@
      IR-ID:OP-LOCAL o2 IR-ID:OP-LOCAL = ;

: FZ-CASE ( -- )
   s" a frozen module serves the operation readers through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 2 T= 5 T= 3 T= 3 T= ;

\ The remaining frozen readers, over an operation that carries an attribute and
\ a terminator that carries a successor.
: FZ2-BODY ( IR-CTX:ctx -- n n n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND {: o0:IR-ID:ir-op-id :}
   c key sp sr tp tr ap ar sa qr p v r S-BR-ATTR APPEND {: o1:IR-ID:ir-op-id :}
   c ap ar key 42 IR-ATTR:INT IR-ID:ATTR-LOCAL {: at:n :}
   c tp tr key I64 IR-ID:TYPE-LOCAL {: ty:n :}
   c sp sr key K-BR OPC-SYM IR-ID:SYMBOL-LOCAL {: br:n :}
   p IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   v IR-ARENA:FREEZE {: vv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv o1 IR-OP:FRESULTS
   rv o1 IR-OP:FATTRS
   rv o1 IR-OP:FSUCCESSORS
   pv rv key o1 0 IR-OP:FATTR@ IR-ID:ATTR-LOCAL at =
   pv rv key o1 0 IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL 0 =
   rv key o1 IR-OP:FOPCODE@ IR-ID:SYMBOL-LOCAL br =
   rv key o0 IR-OP:FSPAN@ IR-SOURCE:SPAN-LEN 4 =
   vv key  pv rv key o0 0 IR-OP:FRESULT@  IR-OP:FVALUE-TYPE@ IR-ID:TYPE-LOCAL ty = ;

: FZ2-CASE ( -- )
   s" every remaining frozen reader answers through the views" T-LABEL
   BND [: FZ2-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 1 T= 1 T= 0 T= ;

: FZ3-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND {: o0:IR-ID:ir-op-id :}
   p IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   v IR-ARENA:FREEZE {: vv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   vv  pv rv key o0 0 IR-OP:FRESULT@  IR-OP:FVALUE-POS@
   vv  pv rv key o0 0 IR-OP:FRESULT@  IR-OP:FVALUE-KIND@
      IR--OP-DEF--KIND:OP-RESULT IR--OP-DEF--KIND:EQ ;

: FZ3-CASE ( -- )
   s" a frozen result value keeps its definition kind and position" T-LABEL
   BND [: FZ3-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= ;

\ The retired builder handles reject every touch once the module is frozen, so
\ there is no public mutation left for a freeze to retract.
: FZ-RETIRED-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop
   p IR-ARENA:FREEZE drop
   v IR-ARENA:FREEZE drop
   r IR-ARENA:FREEZE drop
   k 1 = if r IR-OP:OPS drop then
   k 2 = if c key sp sr tp tr ap ar sa qr p v r S-SEED APPEND drop then ;

: FZ-RETIRED-RUN ( n -- )
   BND [: FZ-RETIRED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- teardown ----------------------------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c 16 16 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ap:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   r ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-OP:OPS drop ;

: FROZEN-CASES ( -- )
   s" live readers reject the retired builder handle" T-LABEL
   [: 1 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" appending through retired frozen handles rejects" T-LABEL
   [: 2 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" an operation store is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and stores succeed after teardown" T-LABEL
   3 0 ?do
      BND [: VOID-BODY ;] IR-CTX:WITH-CONTEXT
      0 T= 0 T= 1 T=
   loop ;

\ ---- the checker keeps the API sealed ----------------------------------------
\ The view fixtures are the standing proof that no public mutation can be
\ spelled against a frozen store: every writing word demands a builder handle,
\ and the checker refuses an IR-ARENA:view in that position.
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRO-POS ( IR-ARENA:arena -- n ) IR-OP:OPS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRO-RAW-ID ( IR-ARENA:arena n -- n ) IR-OP:OPERANDS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-VIEW-AS-ARENA ( IR-ARENA:view IR-ID:ir-op-id -- n ) IR-OP:OPERANDS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-VIEW-APPEND ( IR-CTX:ctx IR-ARENA:view IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-op-id ) IR-OP:END-OP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-VIEW-CREATE ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:view IR-ARENA:arena IR-ARENA:arena ) IR-OP:NEW"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-op-id ) IR-OP:END-OP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-RAW-OPERAND ( n -- ) IR-OP:ADD-OPERAND"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-RAW-SPAN ( n -- ) IR-OP:SET-SPAN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-OP-AS-VALUE ( IR-ARENA:arena IR-ID:ir-op-id -- IR-OP:def-kind ) IR-OP:VALUE-KIND@"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRO-VALUE-AS-OP ( IR-ARENA:arena IR-ID:ir-value-id -- n ) IR-OP:OPERANDS"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside an outermost harness context, so a context
\ abandoned by a throw is reclaimed by that harness exit instead of holding its
\ arena registry slots for the rest of the process. The groups are small because
\ each rigged module holds twelve arenas, and no one harness may accumulate more
\ leaked slots than the registry holds.
: HARNESS-ARITY-A ( IR-CTX:ctx -- )   drop  ARITY-CASES-A ;
: HARNESS-ARITY-B ( IR-CTX:ctx -- )   drop  ARITY-CASES-B ;
: HARNESS-SSA ( IR-CTX:ctx -- )       drop  SSA-CASES ;
: HARNESS-STAGE ( IR-CTX:ctx -- )     drop  STAGE-CASES ;
: HARNESS-IDX ( IR-CTX:ctx -- )       drop  IDX-CASES ;
: HARNESS-STATE ( IR-CTX:ctx -- )     drop  STATE-CASES ;
: HARNESS-WINDOW ( IR-CTX:ctx -- )    drop  WINDOW-CASES ;
: HARNESS-KIND ( IR-CTX:ctx -- )      drop  KIND-CASES ;
: HARNESS-OWNER-A ( IR-CTX:ctx -- )   drop  OWNER-CASES-A ;
: HARNESS-OWNER-B ( IR-CTX:ctx -- )   drop  OWNER-CASES-B ;
: HARNESS-OWNER-D ( IR-CTX:ctx -- )   drop  OWNER-CASES-D ;
: HARNESS-OWNER-C ( IR-CTX:ctx -- )   drop  OWNER-CASES-C ;
: HARNESS-CAP ( IR-CTX:ctx -- )       drop  CAP-CASES ;
: HARNESS-FROZEN ( IR-CTX:ctx -- )    drop  FROZEN-CASES ;

public

: RUN ( -- )
   T-RESET
   READ-CASE
   VALUE-CASE
   SPAN-CASE
   SUCC-CASE
   TAIL-CASE
   VOID-CASE
   FZ-CASE
   FZ2-CASE
   FZ3-CASE
   OVF-CASES
   BND [: HARNESS-ARITY-A ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-ARITY-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-SSA ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-IDX ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STATE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-WINDOW ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-KIND ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-A ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-C ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER-D ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-CAP ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FROZEN ;] IR-CTX:WITH-CONTEXT
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-OP-TEST:RUN
