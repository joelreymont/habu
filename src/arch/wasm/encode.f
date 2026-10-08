\ encode.f - WENC, the Wasm backend's encoder: a frozen WSTRUCT module written
\ as one emission, a header and then each function's body in Wasm binary form
\ (docs/wasm-backend.md 10.2, 17.1).
\
\ THE IDENTITIES ARE THE MODULE'S OWN. A module's symbols are its own ordinals,
\ so ENCODE looks each WSTRUCT opcode and attribute key up in the frozen module
\ by its spelling (src/compiler/ir/symbol.f FFIND), as HIR:FBIND does, and then
\ compares symbols. An opcode the module never interned names none of its
\ operations.
\
\ ONE LOCAL PER VALUE. The structure WCTL derives (structure.f) is written step
\ by step: local.get for each operand, the instruction, local.set for each
\ result, the last result first. A memory token is not a Wasm value and has no
\ local and no bytes. The entry block's arguments are the function's
\ parameters; every other value of a block the entry reaches has a local, and
\ so has each of WCTL's temporaries, declared as the i32s, then the i64s, then
\ the f64s, each in the order the steps first meet it. Wasm's validator does
\ not know that every path leaves before a function's end, so a function whose
\ last step closes a loop or an if ends in unreachable.
\
\ A CALL IS A ROW. `call` is followed by a padded five-byte function index the
\ linker rewrites in place, here zero. Each is a call site: the offset of that
\ field, NEMIT:CALL (P6 lowers a tail call as call then return), the target,
\ its published host implementation when external, and its source location.
\ An internal target has no host implementation until publication resolves its
\ own function offset. The target is an absolute address measured, for an
\ unplaced emission, from its first byte
\ (src/compiler/native/emission.f). A callee spelled `host` and a decimal entry
\ is that host word; a callee that names a function of the module is that
\ function's body offset, which the capture reads as a self call, a call row
\ naming the definition's own record (src/arch/wasm/capture.f WC-CALL).
\ Refused by name with E-WENC-FORM: call_indirect, whose type index is the
\ linker's type table's and has no emission row to carry it, and a function
\ with no body, which an emission has no bytes for.
\
\ AN ADDRESS IS A ROW TOO. An i64.const states its address kind, one of
\ WSTRUCT's four: HIR's three (src/compiler/native/hir.f ADDR-NONE..ADDR-CODE)
\ and FUN. A number, kind NONE, is its shortest SLEB. A data or code address is
\ a padded ten-byte SLEB holding the host value, which the capture resolves
\ and the linker rewrites in place; each is an address site: the offset of
\ that field and its kind. A function's address, kind FUN, is a CODE site
\ whose field holds that function's body offset, laid out once every body is
\ written as a self call's target is, which the capture keeps as a FUN site
\ (src/arch/wasm/capture.f) as it keeps an x86-64 `codeaddr`. Any other kind
\ is refused with E-WSTRUCT-ADDR, as WSTRUCT refuses it where the attribute is
\ built.
\
\ THE EMISSION, LITTLE-ENDIAN: MAGIC; the function count, u32; then per
\ function its body offset u32, body size u32, inputs u8, outputs u8, frame
\ variant u8 and a zero pad byte; then the bodies. An offset counts from the
\ emission's first byte, and a body is a code section entry less its size: the
\ local declarations, the code and its end. The inputs and outputs are the
\ lanes the caller states for each function. Past WPROF's lane arity a function
\ takes the aligned frame (variant 1) and its signature passes no lane;
\ otherwise its signature is (ctx, inputs) -> (status, outputs) (variant 0).
\ Readers answer the last emission encoded whole; a refused one leaves nothing.
\
\ STORAGE CLASS. MODULE-OWNED: the emission and its rows live in this
\ package's buffers until the next ENCODE or RETIRE.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/span.f
require lib/adt/option.f
require src/compiler/ir/id.f
require src/compiler/ir/type.f
require src/compiler/ir/symbol.f
require src/compiler/ir/attr.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/source.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f
require src/compiler/native/emission.f
require src/compiler/native/host.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/structure.f

\ WENC's codes, -9825..-9829, in the Wasm backend's block -9800..-9829.
-9825 constant E-WENC-FIRST
-9829 constant E-WENC-LAST
-9825 constant E-WENC-STATE    \ a read of no emission or past its rows
-9826 constant E-WENC-FORM     \ an operation, value or function with no Wasm form here: call_indirect, a function with no body, a one-byte opcode asked of brz or a two-byte one
-9827 constant E-WENC-CALLEE   \ a call naming neither a function of its module nor a host entry
-9828 constant E-WENC-ARITY    \ a stated arity that is not the function's signature or overflows its header byte

package WENC
public

\ The emission's first four bytes, "HBW1" little-endian.
$31574248 constant MAGIC

private

\ ---- the Wasm bytes beside the opcode table ---------------------------------
$02 constant OP-BLOCK
$03 constant OP-LOOP
$04 constant OP-IF
$05 constant OP-ELSE
$0B constant OP-END
$20 constant OP-LOCAL-GET
$21 constant OP-LOCAL-SET
$40 constant EMPTY-TYPE              \ a block, loop or if taking and leaving nothing
$FC constant MISC-PREFIX             \ the saturating truncations' and bulk memory's prefix
6 constant SAT-I64-F64-S             \ i64.trunc_sat_f64_s under it
11 constant BULK-FILL                \ memory.fill under it
10 constant LEB-MOST                 \ the widest LEB, a 64-bit one

\ A local's class is its Wasm value type; a memory token has none.
0 constant C-I32
1 constant C-I64
2 constant C-F64
3 constant C-NONE
3 constant CLASSES

\ ---- the header ----------------------------------------------------------------
8 constant HEAD-BYTES                \ the magic and the function count
12 constant ROW-BYTES                \ one function's row
0 constant FRAME-LANES               \ the lanes are the signature's
1 constant FRAME-ALIGNED             \ the lanes are in the aligned frame
$FF constant U8-MAX

\ ---- the module's opcodes ---------------------------------------------------------
\ Each WSTRUCT opcode's symbol in the module being encoded, if it interned one.
WSTRUCT:OPCODES TYPED-BUFFER OP-SYM IR-ID:ir-symbol-id
WSTRUCT:OPCODES TYPED-BUFFER OP-HAS bool

\ ---- the emission ----------------------------------------------------------------
DYNAMIC-BUFFER EM u8                 \ its bytes
variable AT                          \ where the next byte goes
variable BODY0                       \ where the body being written starts
variable NF                          \ its functions
variable NCALLS                      \ its call sites
variable SEALED                      \ whether it was encoded whole
0 SEALED !
DYNAMIC-BUFFER FUN-OFF n             \ each function's body offset
DYNAMIC-BUFFER CS-OFF n              \ each call site's field
DYNAMIC-BUFFER CS-TGT n              \ its host entry, or its callee's ordinal until laid out
DYNAMIC-BUFFER CS-OWN bool           \ whether the callee is a function of the module
DYNAMIC-BUFFER CS-IMPL n             \ the published host implementation, if external
DYNAMIC-BUFFER CS-LOC n              \ retained definition-body offset
variable NADDRS                      \ its address sites
DYNAMIC-BUFFER AS-OFF n              \ each address site's field
DYNAMIC-BUFFER AS-KIND n             \ its address kind
DYNAMIC-BUFFER AS-FUN n              \ the function a FUN address names, else -1

\ ---- the function being written ---------------------------------------------------
1 TYPED-BUFFER CUR IR-ID:ir-fun-id
DYNAMIC-BUFFER LOC n                 \ each value's local, by its ordinal in the module
DYNAMIC-BUFFER TEMP-LOC n            \ each WCTL temporary's local
CLASSES TYPED-BUFFER CLS-N n         \ the locals of each class
CLASSES TYPED-BUFFER CLS-AT n        \ the next one of each class to hand out
variable NPARAMS
variable CLOSED                      \ whether the last step written closed a label

\ ---- writing bytes -------------------------------------------------------------
: GROW ( n -- )
   AT @ + EM-RESERVE ;

: PUT-BYTE ( n -- )
   {: v:n :}
   1 GROW
   v AT @ EM c!
   1 AT +! ;

\ Room for the widest LEB past the cursor; the writer answers what it took.
: ROOM ( -- SPAN:span<u8> )
   LEB-MOST GROW
   AT @ EM LEB-MOST SPAN:MAKE ;

: PUT-U32 ( n -- )    ROOM WLEB:U32! AT +! ;
: PUT-S32 ( n -- )    ROOM WLEB:S32! AT +! ;
: PUT-S64 ( n -- )    ROOM WLEB:S64! AT +! ;
: PUT-PAD32 ( n -- )  ROOM WLEB:U32-PAD! AT +! ;
: PUT-PAD64 ( n -- )  ROOM WLEB:S64-PAD! AT +! ;

\ An f64 constant's IEEE 754 bits, low byte first.
: PUT-F64 ( n -- )
   {: v:n :}
   8 0 do  v i 8 * rshift $FF and PUT-BYTE  loop ;

\ A header field of len bytes at offset at, low byte first, in reserved room.
: FIELD! ( n n n -- )
   {: v:n at:n len:n :}
   len 0 do  v i 8 * rshift $FF and  at i + EM c!  loop ;

\ ---- the module's identities, looked up by spelling --------------------------------
\ The symbol the module being encoded gave these bytes, if it interned them.
: FSYMBOL ( ptr u8 n -- IR-ID:ir-symbol-id bool )
   {: p u:n :}
   NFROZEN:V-SYMP NFROZEN:VW  NFROZEN:V-SYMR NFROZEN:VW  NFROZEN:MKEY  p u IR-SYM:FFIND ;

: OPCODES! ( -- )
   WSTRUCT:OPCODES 0 ?do  i WSTRUCT:NTH WSTRUCT:OP-NAME FSYMBOL  i OP-HAS !  i OP-SYM !  loop ;

\ The operation's ordinal in WSTRUCT's table.
: SLOT ( IR-ID:ir-op-id -- n )
   NFROZEN:OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   WSTRUCT:OPCODES 0 ?do
      i OP-HAS @ if  sym i OP-SYM @ NFROZEN:SAME-SYM? if i unloop exit then  then
   loop
   E-WENC-FORM throw ;

\ Where the operation's attribute under the key spelled p u sits. The freeze
\ requires the attribute, so the module interned its key.
: ATTR-SLOT ( IR-ID:ir-op-id ptr u8 n -- n )
   {: o:IR-ID:ir-op-id p u:n :}
   p u FSYMBOL 0= if E-WENC-FORM throw then {: k:IR-ID:ir-symbol-id :}
   o NFROZEN:ATTRS-OF 0 ?do
      o i NFROZEN:ATTR-KEY-AT k NFROZEN:SAME-SYM? if i unloop exit then
   loop
   E-WENC-FORM throw ;

: INT-ATTR ( IR-ID:ir-op-id ptr u8 n -- n )
   {: o:IR-ID:ir-op-id p u:n :}
   o  o p u ATTR-SLOT  NFROZEN:ATTR-INT-AT ;

: SYM-ATTR ( IR-ID:ir-op-id ptr u8 n -- IR-ID:ir-symbol-id )
   {: o:IR-ID:ir-op-id p u:n :}
   NFROZEN:V-OPP NFROZEN:VW  NFROZEN:V-OPR NFROZEN:VW  NFROZEN:MKEY
   o  o p u ATTR-SLOT  IR-OP:FATTR@ {: a:IR-ID:ir-attr-id :}
   NFROZEN:V-ATTR NFROZEN:VW  NFROZEN:MKEY  a IR-ATTR:FSYM@ ;

\ ---- values and their locals ------------------------------------------------------
\ A verified module types its values from WSTRUCT's schemas and its entry
\ signature, so another type never reaches a step.
: CLASS ( IR-ID:ir-type-id -- n )
   {: t:IR-ID:ir-type-id :}
   NFROZEN:V-TYPR NFROZEN:VW t IR-TYPE:FKIND@ {: k:IR-TYPE:kind :}
   k IR--TYPE-KIND:MEMORY-TOKEN IR--TYPE-KIND:EQ if C-NONE exit then
   k IR--TYPE-KIND:FLOAT IR--TYPE-KIND:EQ if C-F64 exit then
   k IR--TYPE-KIND:INT IR--TYPE-KIND:EQ 0= if E-WENC-FORM throw then
   NFROZEN:V-TYPR NFROZEN:VW t IR-TYPE:FINT@ drop {: w:IR-TYPE:width :}
   w IR--TYPE-WIDTH:W32 IR--TYPE-WIDTH:EQ if C-I32 exit then
   w IR--TYPE-WIDTH:W64 IR--TYPE-WIDTH:EQ if C-I64 exit then
   E-WENC-FORM throw ;

: VALUE-CLASS ( IR-ID:ir-value-id -- n )
   NFROZEN:VALUE-TYPE-AT CLASS ;

\ The value type a local declaration names for a class.
: VALTYPE ( n -- n )
   {: c:n :}
   c C-I32 = if $7F exit then
   c C-I64 = if $7E exit then
   $7C ;

: LOCAL-OF ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL LOC @ ;

: ENTRY ( -- IR-ID:ir-block-id )
   0 CUR @ 0 NFROZEN:BLOCK-AT ;

\ The entry block's arguments are the parameters, in order.
: PARAMS ( -- )
   0 NPARAMS !
   ENTRY {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:ARG-COUNT 0 ?do
      bk i NFROZEN:ARG-AT {: v:IR-ID:ir-value-id :}
      v VALUE-CLASS C-NONE <> if
         NPARAMS @  v IR-ID:VALUE-LOCAL LOC !
         1 NPARAMS +!
      then
   loop ;

\ The values a block's steps name: its arguments, unless it is the entry, and
\ the results of its operations before the terminator.
: VISIT-BLOCK ( IR-ID:ir-block-id [ IR-ID:ir-value-id -- ] -- )
   {: bk:IR-ID:ir-block-id fn :}
   bk IR-ID:BLOCK-LOCAL ENTRY IR-ID:BLOCK-LOCAL <> if
      bk NFROZEN:ARG-COUNT 0 ?do  bk i NFROZEN:ARG-AT fn execute  loop
   then
   bk NFROZEN:OP-COUNT 1- 0 ?do
      bk i NFROZEN:OP-AT {: o:IR-ID:ir-op-id :}
      o NFROZEN:RESULTS-OF 0 ?do  o i NFROZEN:RESULT-AT fn execute  loop
   loop ;

\ Every value of every block the steps write, in step order.
: VISIT ( [ IR-ID:ir-value-id -- ] -- )
   {: fn :}
   WCTL:STEPS 0 ?do
      i WCTL:STEP@ MATCH WCTL:step
         open-block OF drop ENDOF
         open-loop OF drop ENDOF
         open-if OF drop ENDOF
         else-arm OF ENDOF
         close OF ENDOF
         body OF fn VISIT-BLOCK ENDOF
         final OF drop ENDOF
         copy OF drop drop ENDOF
         save OF drop drop ENDOF
         restore OF drop drop ENDOF
         branch OF drop drop ENDOF
      ;MATCH
   loop ;

: COUNT-ONE ( IR-ID:ir-value-id -- )
   VALUE-CLASS {: c:n :}
   c C-NONE = if exit then
   1 c CLS-N +! ;

: ASSIGN-ONE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   v VALUE-CLASS {: c:n :}
   c C-NONE = if exit then
   c CLS-AT @  v IR-ID:VALUE-LOCAL LOC !
   1 c CLS-AT +! ;

\ WCTL makes a temporary only for a value it copies, so never for a token.
: COUNT-TEMPS ( -- )
   WCTL:TEMPS 0 ?do  1  i WCTL:TEMP-TYPE CLASS CLS-N +!  loop ;

: ASSIGN-TEMPS ( -- )
   WCTL:TEMPS TEMP-LOC-RESERVE
   WCTL:TEMPS 0 ?do
      i WCTL:TEMP-TYPE CLASS {: c:n :}
      c CLS-AT @  i TEMP-LOC !
      1 c CLS-AT +!
   loop ;

\ Each class's first local follows the parameters and the classes before it.
: LOCALS ( -- )
   CLASSES 0 do  0 i CLS-N !  loop
   PARAMS
   [: COUNT-ONE ;] VISIT
   COUNT-TEMPS
   NPARAMS @  CLASSES 0 do  dup i CLS-AT !  i CLS-N @ +  loop  drop
   [: ASSIGN-ONE ;] VISIT
   ASSIGN-TEMPS ;

\ The body's local declarations: one run per class that has a local.
: DECLARE ( -- )
   0  CLASSES 0 do  i CLS-N @ 0<> if 1+ then  loop  PUT-U32
   CLASSES 0 do
      i CLS-N @ 0<> if  i CLS-N @ PUT-U32  i VALTYPE PUT-BYTE  then
   loop ;

public

\ ---- the opcode table ----------------------------------------------------------
\ Each WSTRUCT opcode's one-byte Wasm opcode. brz is no instruction (WCTL writes
\ it as an if) and i64.trunc_sat_f64_s is two bytes, so neither has one.
: OPCODE-BYTE ( WSTRUCT:opcode -- n )
   MATCH WSTRUCT:opcode
      i32-const           OF $41 ENDOF
      i64-const           OF $42 ENDOF
      f64-const           OF $44 ENDOF
      i32-add             OF $6A ENDOF
      i32-sub             OF $6B ENDOF
      i64-add             OF $7C ENDOF
      i64-sub             OF $7D ENDOF
      i64-mul             OF $7E ENDOF
      i64-div-s           OF $7F ENDOF
      i64-and             OF $83 ENDOF
      i64-or              OF $84 ENDOF
      i64-xor             OF $85 ENDOF
      i64-shl             OF $86 ENDOF
      i64-shr-u           OF $88 ENDOF
      i64-eqz             OF $50 ENDOF
      i64-eq              OF $51 ENDOF
      i64-ne              OF $52 ENDOF
      i64-lt-s            OF $53 ENDOF
      i64-gt-s            OF $55 ENDOF
      i64-le-s            OF $57 ENDOF
      i64-ge-s            OF $59 ENDOF
      f64-add             OF $A0 ENDOF
      f64-sub             OF $A1 ENDOF
      f64-mul             OF $A2 ENDOF
      f64-div             OF $A3 ENDOF
      f64-sqrt            OF $9F ENDOF
      f64-neg             OF $9A ENDOF
      f64-abs             OF $99 ENDOF
      f64-eq              OF $61 ENDOF
      f64-ne              OF $62 ENDOF
      f64-lt              OF $63 ENDOF
      f64-gt              OF $64 ENDOF
      i32-wrap-i64        OF $A7 ENDOF
      i64-extend-i32-u    OF $AD ENDOF
      f64-convert-i64-s   OF $B9 ENDOF
      i64-trunc-sat-f64-s OF E-WENC-FORM throw ENDOF
      i64-reinterpret-f64 OF $BD ENDOF
      f64-reinterpret-i64 OF $BF ENDOF
      f64-select          OF $1B ENDOF
      i32-load            OF $28 ENDOF
      i64-load            OF $29 ENDOF
      i64-load8-u         OF $31 ENDOF
      i32-store           OF $36 ENDOF
      i64-store           OF $37 ENDOF
      i64-store8          OF $3C ENDOF
      br                  OF $0C ENDOF
      brz                 OF E-WENC-FORM throw ENDOF
      return              OF $0F ENDOF
      unreachable         OF $00 ENDOF
      call                OF $10 ENDOF
      call-indirect       OF $11 ENDOF
      memory-grow         OF $40 ENDOF
      memory-fill         OF E-WENC-FORM throw ENDOF
   ;MATCH ;

private

\ ---- calls --------------------------------------------------------------------
\ The function of this module the callee names, if it names one.
: MODULE-FUN ( IR-ID:ir-symbol-id -- n bool )
   {: s:IR-ID:ir-symbol-id :}
   NFROZEN:FUN-COUNT 0 ?do
      NFROZEN:V-FUNR NFROZEN:VW  NFROZEN:MKEY  NFROZEN:MKEY i IR-ID:PACK-FUN
      IR-FUN:FSYMBOL@ s NFROZEN:SAME-SYM? if i true unloop exit then
   loop
   0 false ;

\ A host word's callee: "host ", then its entry in at most nineteen digits.
: HOST$ ( -- ptr u8 n )
   s" host " ;

24 constant SPELL-MAX
SPELL-MAX BUFFER: SPELL

: HOST-ENTRY ( IR-ID:ir-symbol-id -- n )
   {: s:IR-ID:ir-symbol-id :}
   NFROZEN:V-SYMR NFROZEN:VW s IR-SYM:FLEN@ {: u:n :}
   u SPELL-MAX > if E-WENC-CALLEE throw then
   NFROZEN:V-SYMP NFROZEN:VW  NFROZEN:V-SYMR NFROZEN:VW  s SPELL SPELL-MAX IR-SYM:FCOPY drop
   SPELL u HOST$ STARTS-WITH? 0= if E-WENC-CALLEE throw then
   HOST$ nip {: h:n :}
   SPELL h +  u h -  {: a d:n :}
   a d STR-DIGITS? 0= if E-WENC-CALLEE throw then
   a d STR>NUMBER? MATCH option
      none OF E-WENC-CALLEE throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ A callee of the module answers its ordinal and true, to be laid out as its
\ body offset once every body is written; any other is a host entry.
: TARGET ( IR-ID:ir-symbol-id -- n bool )
   {: s:IR-ID:ir-symbol-id :}
   s MODULE-FUN if true exit then
   drop s HOST-ENTRY false ;

: CALL-FIELD ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o WSTRUCT:KEY-CALLEE$ SYM-ATTR TARGET {: t:n own:bool :}
   NCALLS @ {: k:n :}
   k 1+ CS-OFF-RESERVE  k 1+ CS-TGT-RESERVE  k 1+ CS-OWN-RESERVE
   k 1+ CS-IMPL-RESERVE  k 1+ CS-LOC-RESERVE
   AT @ k CS-OFF !
   t k CS-TGT !
   own k CS-OWN !
   own if 0 else t NHOST:ID-OF then k CS-IMPL !
   o NFROZEN:SPAN-AT IR-SOURCE:SPAN-START k CS-LOC !
   1 NCALLS +!
   0 PUT-PAD32 ;

: LAY-OUT-CALLS ( -- )
   NCALLS @ 0 ?do
      i CS-OWN @ if  i CS-TGT @ FUN-OFF @  i CS-TGT !  then
   loop ;

\ ---- one operation ----------------------------------------------------------------
: SAME-OP? ( WSTRUCT:opcode WSTRUCT:opcode -- bool )
   WSTRUCT-OPCODE:EQ ;

\ Whether the operation is a memory access, which carries a memarg.
: ACCESS? ( WSTRUCT:opcode -- bool )
   {: o:WSTRUCT:opcode :}
   o WSTRUCT-OPCODE:I64-LOAD8-U SAME-OP?  o WSTRUCT-OPCODE:I64-STORE8 SAME-OP? or
   o WSTRUCT-OPCODE:I32-LOAD SAME-OP? or  o WSTRUCT-OPCODE:I32-STORE SAME-OP? or
   o WSTRUCT-OPCODE:I64-LOAD SAME-OP? or  o WSTRUCT-OPCODE:I64-STORE SAME-OP? or ;

: GET ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   v VALUE-CLASS C-NONE = if exit then
   OP-LOCAL-GET PUT-BYTE  v LOCAL-OF PUT-U32 ;

: SET ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   v VALUE-CLASS C-NONE = if exit then
   OP-LOCAL-SET PUT-BYTE  v LOCAL-OF PUT-U32 ;

\ Wasm takes WSTRUCT's operands in order and leaves its results with the last
\ on top.
: GETS ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NFROZEN:OPERANDS-OF 0 ?do  o i NFROZEN:OPERAND-AT GET  loop ;

: SETS ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NFROZEN:RESULTS-OF {: n:n :}
   n 0 ?do  o  n 1- i -  NFROZEN:RESULT-AT SET  loop ;

\ An access's memarg: the alignment's exponent, then the offset.
: MEMARG ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o WSTRUCT:KEY-ALIGN$ INT-ATTR PUT-U32
   o WSTRUCT:KEY-OFFSET$ INT-ATTR PUT-U32 ;

: ADDR-SITE+ ( n n -- )
   {: kind:n fun:n :}
   NADDRS @ {: k:n :}
   k 1+ AS-OFF-RESERVE  k 1+ AS-KIND-RESERVE  k 1+ AS-FUN-RESERVE
   AT @ k AS-OFF !
   kind k AS-KIND !
   fun k AS-FUN !
   1 NADDRS +! ;

\ An i64.const's value: a number as itself, an address as a padded field and a
\ site of its kind, a function's a CODE site its body offset fills later.
: CELL-CONST ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o WSTRUCT:KEY-VALUE$ INT-ATTR {: v:n :}
   o WSTRUCT:KEY-ADDR$ INT-ATTR {: kind:n :}
   kind WSTRUCT:ADDR-NONE = if  v PUT-S64  exit  then
   kind WSTRUCT:ADDR-FUN = if  WSTRUCT:ADDR-CODE v ADDR-SITE+  0 PUT-PAD64  exit  then
   kind WSTRUCT:ADDR-DATA <>  kind WSTRUCT:ADDR-CODE <> and if E-WSTRUCT-ADDR throw then
   kind -1 ADDR-SITE+
   v PUT-PAD64 ;

\ Each function's address field gets its body offset.
: LAY-OUT-FUNS ( -- )
   NADDRS @ 0 ?do
      i AS-FUN @ {: f:n :}
      f 0 >= if  f FUN-OFF @  0 EM AT @ SPAN:MAKE  i AS-OFF @  WLEB:S64-PATCH  then
   loop ;

\ The opcode and what follows it.
: INSTR ( WSTRUCT:opcode IR-ID:ir-op-id -- )
   {: code:WSTRUCT:opcode o:IR-ID:ir-op-id :}
   code WSTRUCT-OPCODE:CALL-INDIRECT SAME-OP? if E-WENC-FORM throw then
   code WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S SAME-OP? if
      MISC-PREFIX PUT-BYTE  SAT-I64-F64-S PUT-U32  exit
   then
   code WSTRUCT-OPCODE:MEMORY-FILL SAME-OP? if
      MISC-PREFIX PUT-BYTE  BULK-FILL PUT-U32  0 PUT-U32  exit
   then
   code OPCODE-BYTE PUT-BYTE
   code WSTRUCT-OPCODE:I32-CONST SAME-OP? if  o WSTRUCT:KEY-VALUE$ INT-ATTR PUT-S32  exit  then
   code WSTRUCT-OPCODE:I64-CONST SAME-OP? if  o CELL-CONST  exit  then
   code WSTRUCT-OPCODE:F64-CONST SAME-OP? if  o WSTRUCT:KEY-VALUE$ INT-ATTR PUT-F64  exit  then
   code WSTRUCT-OPCODE:CALL SAME-OP? if  o CALL-FIELD  exit  then
   code WSTRUCT-OPCODE:MEMORY-GROW SAME-OP? if  0 PUT-U32 exit  then
   code ACCESS? if  o MEMARG  then ;

: OP ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o SLOT WSTRUCT:NTH {: code:WSTRUCT:opcode :}
   o GETS
   code o INSTR
   o SETS ;

\ ---- the steps ---------------------------------------------------------------------
: BODY ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:OP-COUNT 1- 0 ?do  bk i NFROZEN:OP-AT OP  loop ;

: OPEN ( n -- )
   PUT-BYTE  EMPTY-TYPE PUT-BYTE ;

: TEMP ( n -- )
   TEMP-LOC @ PUT-U32 ;

: STEP ( WCTL:step -- )
   0 CLOSED !
   MATCH WCTL:step
      open-block OF drop OP-BLOCK OPEN ENDOF
      open-loop OF drop OP-LOOP OPEN ENDOF
      open-if OF GET OP-IF OPEN ENDOF
      else-arm OF OP-ELSE PUT-BYTE ENDOF
      close OF OP-END PUT-BYTE  1 CLOSED ! ENDOF
      body OF BODY ENDOF
      final OF OP ENDOF
      copy OF {: d:IR-ID:ir-value-id s:IR-ID:ir-value-id :}
         s GET d SET ENDOF
      save OF {: t:n s:IR-ID:ir-value-id :}
         s GET  OP-LOCAL-SET PUT-BYTE t TEMP ENDOF
      restore OF {: d:IR-ID:ir-value-id t:n :}
         OP-LOCAL-GET PUT-BYTE t TEMP  d SET ENDOF
      branch OF nip  WSTRUCT-OPCODE:BR OPCODE-BYTE PUT-BYTE  PUT-U32 ENDOF
   ;MATCH ;

\ ---- one function -------------------------------------------------------------------
\ The frame variant of a function stated to take in lanes and leave out.
: FRAME ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id in:n out:n :}
   in 0 <  out 0 < or  in U8-MAX > or  out U8-MAX > or if E-WENC-ARITY throw then
   in WPROF:PARAMS-MAX >  out WPROF:RESULTS-MAX > or {: framed:bool :}
   f NFROZEN:FUN-ARITY {: np:n nr:n :}
   framed if 0 0 else in out then {: li:n lo:n :}
   np li 1+ <>  nr lo 1+ <> or if E-WENC-ARITY throw then
   framed if FRAME-ALIGNED else FRAME-LANES then ;

: FUNCTION ( IR-BUILD:module n [ n -- n n ] -- )
   {: m:IR-BUILD:module k:n ar :}
   m IR-BUILD:FKEY k IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f NFROZEN:BLOCK-COUNT 0= if E-WENC-FORM throw then
   m f WCTL:STRUCTURE
   f 0 CUR !
   k ar execute {: in:n out:n :}
   f in out FRAME {: fv:n :}
   AT @ BODY0 !
   AT @ k FUN-OFF !
   LOCALS
   DECLARE
   WCTL:STEPS 0 ?do  i WCTL:STEP@ STEP  loop
   CLOSED @ if  WSTRUCT-OPCODE:UNREACHABLE OPCODE-BYTE PUT-BYTE  then
   OP-END PUT-BYTE
   HEAD-BYTES k ROW-BYTES * + {: row:n :}
   BODY0 @  row 4 FIELD!
   AT @ BODY0 @ -  row 4 + 4 FIELD!
   in  row 8 + 1 FIELD!
   out  row 9 + 1 FIELD!
   fv  row 10 + 1 FIELD!
   0  row 11 + 1 FIELD! ;

\ ---- the sealed emission --------------------------------------------------------------
: SEALED-CK ( -- )
   SEALED @ 0= if E-WENC-STATE throw then ;

: ROW-CK ( n n -- n )
   {: k:n cnt:n :}
   SEALED-CK
   k 0 <  k cnt >= or if E-WENC-STATE throw then
   k ;

public

\ Nothing is read from an emission after this, or after a refused ENCODE.
: RETIRE ( -- )
   0 SEALED !  0 NF !  0 NCALLS !  0 NADDRS !  0 AT !  0 BODY0 ! ;

\ Encode every function of a module WSTRUCT:FREEZE froze; ar answers function
\ n's input and output lanes.
: ENCODE ( IR-BUILD:module [ n -- n n ] -- )
   {: m:IR-BUILD:module ar :}
   RETIRE
   m NFROZEN:VIEWS!
   OPCODES!
   NFROZEN:FUN-COUNT {: nf:n :}
   HEAD-BYTES nf ROW-BYTES * + {: hb:n :}
   NFROZEN:VALUE-COUNT LOC-RESERVE
   nf FUN-OFF-RESERVE
   hb GROW  hb AT !  hb BODY0 !
   MAGIC 0 4 FIELD!
   nf 4 4 FIELD!
   nf 0 ?do  m i ar FUNCTION  loop
   LAY-OUT-CALLS
   LAY-OUT-FUNS
   nf NF !
   1 SEALED ! ;

\ ---- readers shaped like X64EMIT's (src/arch/x86-64/passes.f) ------------------------
: BYTES ( -- ptr u8 )
   SEALED-CK 0 EM ;

: SIZE ( -- n )
   SEALED-CK AT @ ;

: FUNS ( -- n )
   SEALED-CK NF @ ;

: FUNCTION-OFFSET@ ( n -- n )
   NF @ ROW-CK FUN-OFF @ ;

: CALL-SITES ( -- n )
   SEALED-CK NCALLS @ ;

\ The offset of the padded field after the `call` opcode.
: CALL-SITE@ ( n -- n )
   NCALLS @ ROW-CK CS-OFF @ ;

: CALL-KIND@ ( n -- n )
   NCALLS @ ROW-CK drop NEMIT:CALL ;

: CALL-TARGET@ ( n -- n )
   NCALLS @ ROW-CK CS-TGT @ ;

: CALL-IMPL@ ( n -- n )
   NCALLS @ ROW-CK CS-IMPL @ ;

: CALL-LOC@ ( n -- n )
   NCALLS @ ROW-CK CS-LOC @ ;

: ADDR-SITES ( -- n )
   SEALED-CK NADDRS @ ;

\ The offset of the padded field after the `i64.const` opcode.
: ADDR-SITE@ ( n -- n )
   NADDRS @ ROW-CK AS-OFF @ ;

\ Its address kind, WSTRUCT's and so HIR's.
: ADDR-SITE-KIND@ ( n -- n )
   NADDRS @ ROW-CK AS-KIND @ ;

;package
