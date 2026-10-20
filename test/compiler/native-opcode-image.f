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
HIR:OPCODES A64IR:OPCODES max TYPED-BUFFER IDS IR-ID:ir-symbol-id
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

: SYMBOL+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module sym:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS sym TEXT 128 IR-SYM:FCOPY
   TEXT swap TEXT+ ;

: TYPE+ ( IR-BUILD:module IR-ID:ir-type-id -- )
   {: m:IR-BUILD:module ty:IR-ID:ir-type-id :}
   m IR-BUILD:FTYPE-POOL m IR-BUILD:FTYPE-ROWS ty TEXT 128 IR-TYPE:FRENDER
   TEXT swap TEXT+ ;

: FLAG+ ( bool -- )
   if 1 else 0 then CELL+ ;

: EFFECT+ ( IR-SCHEMA:effect -- )
   MATCH effect
      pure       OF 0 ENDOF
      read       OF 1 ENDOF
      write      OF 2 ENDOF
      read-write OF 3 ENDOF
   ;MATCH CELL+ ;

: DOMAIN+ ( IR-TYPE:domain -- )
   MATCH IR-TYPE:domain
      data-mem OF 0 ENDOF
      dict     OF 1 ENDOF
      code-pub OF 2 ENDOF
      io       OF 3 ENDOF
      process  OF 4 ENDOF
      ffi      OF 5 ENDOF
   ;MATCH CELL+ ;

: SPACE+ ( IR-TYPE:space -- )
   MATCH IR-TYPE:space
      generic OF 0 ENDOF
      global  OF 1 ENDOF
      shared  OF 2 ENDOF
      local   OF 3 ENDOF
      param   OF 4 ENDOF
      const   OF 5 ENDOF
   ;MATCH CELL+ ;

: ALIAS+ ( IR-SCHEMA:alias -- )
   MATCH alias
      unaliased    OF 0 ENDOF
      grouped      OF 1 ENDOF
      unrestricted OF 2 ENDOF
   ;MATCH CELL+ ;

: ARCH+ ( CTARGET:arch -- )
   MATCH CTARGET:arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
      a32     OF 2 ENDOF
      thumb2  OF 3 ENDOF
      c66x    OF 4 ENDOF
      x86-64  OF 5 ENDOF
      wasm    OF 6 ENDOF
   ;MATCH CELL+ ;

: SIDE+ ( IR-SCHEMA:side -- )
   MATCH side
      operand OF 0 ENDOF
      result  OF 1 ENDOF
   ;MATCH CELL+ ;

: TYPES+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FOPERANDS {: n:n :}
   n CELL+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FOPERAND-TAIL? FLAG+
   n 0 ?do
      m  m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      m IR-BUILD:FKEY op i IR-SCHEMA:FOPERAND@ TYPE+
   loop
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FRESULTS {: rn:n :}
   rn CELL+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FRESULT-TAIL? FLAG+
   rn 0 ?do
      m  m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      m IR-BUILD:FKEY op i IR-SCHEMA:FRESULT@ TYPE+
   loop ;

: ATTRS+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FATTRS {: n:n :}
   n CELL+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FATTR-EXT? FLAG+
   n 0 ?do
      m  m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      m IR-BUILD:FKEY op i IR-SCHEMA:FATTR@ SYMBOL+
   loop ;

: EFFECT-FIELDS+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FEFFECT@
   {: eff:IR-SCHEMA:effect :}
   eff EFFECT+
   eff IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ if exit then
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FDOMAIN@
   {: dom:IR-TYPE:domain :}
   dom DOMAIN+
   dom IR--TYPE-DOMAIN:DATA-MEM IR--TYPE-DOMAIN:EQ if
      m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FSPACE@ SPACE+
      m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FALIAS@ ALIAS+
   then ;

: TIES+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FTIES {: n:n :}
   n CELL+
   n 0 ?do
      m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      op i IR-SCHEMA:FTIE-RESULT@ CELL+
      m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      op i IR-SCHEMA:FTIE-OPERAND@ CELL+
   loop ;

: FIXED+ ( IR-BUILD:module IR-ID:ir-symbol-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FFIXED-COUNT@ {: n:n :}
   n CELL+
   n 0 ?do
      m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      op i IR-SCHEMA:FFIXED-SIDE@ SIDE+
      m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      op i IR-SCHEMA:FFIXED-ORDINAL@ CELL+
      m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS
      op i IR-SCHEMA:FFIXED-REG@ CELL+
   loop ;

: ROW+ ( IR-BUILD:module n -- )
   {: m:IR-BUILD:module idx:n :}
   idx CELL+ idx IDS @ {: op:IR-ID:ir-symbol-id :}
   m op SYMBOL+
   m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY op IR-SCHEMA:FRULE@ m swap SYMBOL+
   m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY op IR-SCHEMA:FRENDERER@ m swap SYMBOL+
   m op TYPES+
   m op ATTRS+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FREGIONS CELL+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FSUCCESSORS CELL+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FTERMINATOR? FLAG+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FTRAPS? FLAG+
   m op EFFECT-FIELDS+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FARCH@ ARCH+
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FFEATURES@ CTARGET:FEATURES-N CELL+
   m op TIES+
   m op FIXED+ ;

: TABLE+ ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY IR-SCHEMA:FDIALECT@ SYMBOL+
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMAJOR@ CELL+
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMINOR@ CELL+
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FSCHEMAS CELL+ ;

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
