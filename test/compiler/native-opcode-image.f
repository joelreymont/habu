\ The complete dialect vocabulary and schemas, serialized through their public
\ readers. Fresh builders force name reads instead of a session prototype hit.
require src/compiler/native/abi.f
require src/compiler/native/hir.f
require src/compiler/native/a64ir.f
require src/compiler/ir/symbol.f

package NATIVE-OPCODE-IMAGE
private
32768 constant CAP
CAP BUFFER: ART
variable ART-U
128 BUFFER: TEXT
82 TYPED-BUFFER IDS IR-ID:ir-symbol-id
32 BUFFER: DIGEST
64 BUFFER: HEX
SHA256-CTX-BYTES BUFFER: SHA-CTX

: ROOM ( n -- ) ART-U @ + CAP > if E-STR-CAPACITY throw then ;

: CELL+ ( n -- )
   8 ROOM ART ART-U @ + 0 CDIGEST:SLOT!
   8 ART-U +! ;

: TEXT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CELL+ u ROOM
   a ART ART-U @ + u BYTE-COPY u ART-U +! ;

: DIGEST+ ( CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   w0 CELL+ w1 CELL+ w2 CELL+ w3 CELL+ ;

: SYMBOL+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module sym:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS sym TEXT 128 IR-SYM:FCOPY
   TEXT swap TEXT+ ;

: ROW+ ( IR-BUILD:module n -- )
   {: m:IR-BUILD:module idx:n :}
   idx CELL+ idx IDS @ {: op:IR-ID:ir-symbol-id :}
   m op SYMBOL+
   m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY op IR-SCHEMA:FRULE@ m swap SYMBOL+
   m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY op IR-SCHEMA:FRENDERER@ m swap SYMBOL+
   m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FDIGEST DIGEST+ ;

: TABLE+ ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FTABLE-DIGEST DIGEST+ ;

: HIR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c HIR:NAME HIR:MAJOR HIR:MINOR IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   HIR:OPCODES CELL+
   HIR:OPCODES 0 ?do c b i HIR:NTH HIR:OPCODE i IDS ! loop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   HIR:OPCODES 0 ?do m i ROW+ loop m TABLE+ ;

: A64-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c A64IR:NAME A64IR:MAJOR A64IR:MINOR IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64IR:REGISTER
   A64IR:OPCODES CELL+
   A64IR:OPCODES 0 ?do c b i A64IR:NTH A64IR:OPCODE i IDS ! loop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   A64IR:OPCODES 0 ?do m i ROW+ loop m TABLE+ ;

public

: ARTIFACT$ ( -- ptr u8 n )
   0 ART-U !
   NABI:BINDING [: HIR-BODY ;] IR-CTX:WITH-CONTEXT
   NABI:BINDING [: A64-BODY ;] IR-CTX:WITH-CONTEXT
   ART ART-U @ ;

: PRINT ( -- )
   SHA-CTX ARTIFACT$ DIGEST SHA256-IN
   DIGEST HEX SHA256>HEX
   HEX 64 type cr ;
;package
