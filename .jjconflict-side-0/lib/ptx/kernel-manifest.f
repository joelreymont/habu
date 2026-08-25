\ kernel-manifest.f - habu-kernel-manifest JSON renderer.
\
\ Renders the manifest for one kernel from the SAME sources the
\ emitter uses: the active KABI record (name, block, grid derivation, ordered
\ logical params, derived flat .param layout with dedup) plus the
\ module-target accessors (PTX-SM-TARGET$ / PTX-VERSION$ / PTX-ADDRESS-SIZE$)
\ and the PTX text, which is HASHED, never parsed. Schema identity is
\ "habu-kernel-manifest"; the field contract lives in
\ docs/ptx-sketch.md ("Kernel ABI contract"). All JSON goes through
\ lib/json-write.f (escaped, never hand-concatenated) in a FIXED field order,
\ so identical inputs render byte-identical manifests.
\ manifest_content_hash = SHA256 hex of the manifest bytes STRICTLY BEFORE
\ the ,"manifest_content_hash":"..." suffix, so consumers can verify it with
\ plain string slicing. SHA256 words are native (src/core/sha256.f is baked
\ into bin/hb). Load after lib/errors.f, lib/string.f, lib/memory.f,
\ lib/json-write.f, src/arch/ptx/emit.f, and lib/ptx/kernel-abi.f.

package KMAN

$42 constant P-CAP                     \ "p_" + a KABI name
32 constant DG-LEN
64 constant HEX-LEN

create P-BUF P-CAP allot
create PTX-DG DG-LEN 8 + allot
create PTX-HEX HEX-LEN 8 + allot
create MAN-DG DG-LEN 8 + allot
create MAN-HEX HEX-LEN 8 + allot

: P$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ field name -> "p_<name>"
   u 2 + P-CAP > if E-KABI-TOKEN throw then
   s" p_" {: pa:ptr pu:n :}
   pa P-BUF pu BYTE-COPY
   a P-BUF pu + u BYTE-COPY
   P-BUF u pu + ;

: KIND$ ( n -- ptr u8 n ) {: k:n :}
   k KABI:KIND-SPAN = if s" span" exit then
   k KABI:KIND-MATRIX = if s" matrix" exit then
   s" uniform" ;

: ROLE$ ( n -- ptr u8 n ) {: r:n :}
   r KABI:ROLE-BASE = if s" base" exit then
   r KABI:ROLE-LEN = if s" len" exit then
   r KABI:ROLE-COLS = if s" cols" exit then
   r KABI:ROLE-ROWS = if s" rows" exit then
   r KABI:ROLE-STRIDE = if s" stride" exit then
   s" scalar" ;

: SLOT-FIELDS ( ptr u8 n -- ) {: fa:ptr fu:n :}  \ param/offset/size/ptx_type of one named field
   s" param" fa fu P$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" offset" fa fu KABI:OFFSET-OF JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" size" fa fu KABI:SIZE-OF JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" ptx_type" fa fu KABI:FIELD-INDEX KABI:FIELD-PTX$ JSON-WRITE:FIELD-S ;

: SLOT-REF ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n fa:ptr fu:n :}
   ka ku JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   fa fu SLOT-FIELDS
   JSON-WRITE:OBJECT-END ;

: EXT-REF ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n ea:ptr eu:n :}   \ dedup'd extent-value slot
   ka ku JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   ea eu SLOT-FIELDS JSON-WRITE:COMMA
   s" dedup_key" ea eu JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;

: ROWS-REF ( ptr u8 n -- ) {: ea:ptr eu:n :}     \ launch-derived matrix rows: no .param slot
   s" rows" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" source" s" launch-derived" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" from" s" gridDim.x" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" dedup_key" ea eu JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;

: STRIDE-REF ( ptr u8 n -- ) {: ea:ptr eu:n :}   \ dense row-major stride: equals cols
   s" stride" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" source" s" dense-derived" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" equals" ea eu JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;

: LOWER-SPAN ( n -- ) {: i:n :}
   s" base" i KABI:PARAM-NAME$ SLOT-REF JSON-WRITE:COMMA
   s" len" i KABI:PARAM-EXT$ EXT-REF ;

: LOWER-MATRIX ( n -- ) {: i:n :}
   s" base" i KABI:PARAM-NAME$ SLOT-REF JSON-WRITE:COMMA
   s" cols" i KABI:PARAM-EXT2$ EXT-REF JSON-WRITE:COMMA
   i KABI:PARAM-EXT$ ROWS-REF JSON-WRITE:COMMA
   i KABI:PARAM-EXT2$ STRIDE-REF ;

: LOWER-UNIFORM ( n -- ) {: i:n :}
   s" scalar" i KABI:PARAM-NAME$ SLOT-REF ;

: LOWERING ( n -- ) {: i:n :}
   s" lowering" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   i KABI:PARAM-KIND case
      KABI:KIND-SPAN of i LOWER-SPAN endof
      KABI:KIND-MATRIX of i LOWER-MATRIX endof
      i LOWER-UNIFORM
   endcase
   JSON-WRITE:OBJECT-END ;

: PARAM-OBJ ( n -- ) {: i:n :}
   JSON-WRITE:OBJECT-START
   s" name" i KABI:PARAM-NAME$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" kind" i KABI:PARAM-KIND KIND$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" elem" i KABI:PARAM-ELEM$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" align" i KABI:PARAM-ALIGN JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   i LOWERING
   JSON-WRITE:OBJECT-END ;

: PARAMS-ARR ( -- )
   s" params" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START
   KABI:N-PARAMS 0 ?do
      i 0 > if JSON-WRITE:COMMA then
      i PARAM-OBJ
   loop
   JSON-WRITE:ARRAY-END ;

: SLOT-OBJ ( n -- ) {: k:n :}
   JSON-WRITE:OBJECT-START
   s" param" k KABI:FIELD-NAME$ P$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" offset" k KABI:FIELD-OFF JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" size" k KABI:FIELD-SIZE JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" ptx_type" k KABI:FIELD-PTX$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" role" k KABI:FIELD-ROLE ROLE$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;

: SLOTS-ARR ( -- )                     \ the flat cuLaunchKernel/kernelParams layout, offset order
   s" param_slots" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START
   0 KABI:N-FIELDS 0 ?do
      i KABI:FIELD-PARAM? if
         dup 0 > if JSON-WRITE:COMMA then
         i SLOT-OBJ
         1+
      then
   loop drop
   JSON-WRITE:ARRAY-END ;

: BLOCK-OBJ ( -- )                     \ records block.x; y/z are fixed 1
   s" block" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" x" KABI:BLOCK@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" y" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" z" 1 JSON-WRITE:FIELD-U
   JSON-WRITE:OBJECT-END ;

: PTX-SHA! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PTX-DG SHA256
   PTX-DG PTX-HEX SHA256>HEX ;

: MAN-SHA! ( -- )                      \ hash the manifest bytes rendered so far
   JSON-WRITE:$ MAN-DG SHA256
   MAN-DG MAN-HEX SHA256>HEX ;

public

\ manifest for the ACTIVE KABI record + this PTX text (JSON in the JW buffer)
: MANIFEST$ ( ptr u8 n -- ptr u8 n ) {: xa:ptr xu:n :}
   KABI:NAME$ nip 0= if E-KABI-TOKEN throw then
   xa xu PTX-SHA!
   JSON-WRITE:RESET
   JSON-WRITE:OBJECT-START
   s" schema" s" habu-kernel-manifest" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" name" KABI:NAME$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" target" PTX-SM-TARGET$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" ptx_version" PTX-VERSION$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" address_size" PTX-ADDRESS-SIZE$ JSON-WRITE:FIELD-RAW JSON-WRITE:COMMA
   BLOCK-OBJ JSON-WRITE:COMMA
   s" grid_derivation" KABI:GRID$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" param_bytes" KABI:TOTAL JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   PARAMS-ARR JSON-WRITE:COMMA
   SLOTS-ARR JSON-WRITE:COMMA
   s" ptx_sha256" PTX-HEX HEX-LEN JSON-WRITE:FIELD-S
   MAN-SHA!
   JSON-WRITE:COMMA
   s" manifest_content_hash" MAN-HEX HEX-LEN JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

;package
