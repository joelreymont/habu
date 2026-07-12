\ maki/onnx/graph.f - ONNX message walkers -> importer-side graph tables.
\
\ Walks a serialized ModelProto (maki/onnx/proto.f wire subset) into flat
\ importer-side tables the graph builder (maki/onnx/import.f) consumes: interned
\ tensor names, node records (op_type text, input-name window, output name,
\ recognized attributes), initializer records (2D shape + f32 payload span,
\ model-absolute offsets), and graph input/output records (name + static 2D
\ shape). Field numbers from onnx.proto3: ModelProto.graph=7; GraphProto
\ node=1 name=2 initializer=5 input=11 output=12; NodeProto input=1 output=2
\ op_type=4 attribute=5; TensorProto dims=1 data_type=2 float_data=4 name=8
\ raw_data=9; ValueInfoProto name=1 type=2; TypeProto.tensor_type=1;
\ TypeProto.Tensor elem_type=1 shape=2; TensorShapeProto.dim=1; Dimension
\ dim_value=1 dim_param=2; AttributeProto name=1 f=2 i=3 ints=8. Unknown fields are
\ skipped by wire type (PB-SKIP).
\
\ Initializers split by data_type: FLOAT (1) tensors are 2D f32 weights recorded in
\ the OGI table (payload span, materialized later); INT64 (7) tensors are rank-1 int
\ vectors (a Reshape target shape, a Slice range) decoded to cell ints HERE into the
\ OGIC constant table - a movement op reads its dims/range by input name, and a name
\ absent from OGIC is a runtime-computed operand the importer rejects (E-ONNX-DYNSHAPE).
\ The Transpose perm attribute (ints=8, packed or unpacked) is collected onto the node.
\
\ Fail closed (docs/maki/onnx.md dynamic-shape policy): a dim_param (symbolic
\ dim), a missing/empty Dimension, a non-positive dim_value, or a ValueInfo
\ without tensor type+shape throws E-ONNX-DYNSHAPE; rank 0 or above 2 throws
\ E-ONNX-RANK (the IR is 2D; rank 1 maps to 1xC); a non-FLOAT elem_type /
\ data_type throws E-ONNX-DTYPE; an unrecognized attribute NAME (semantics the
\ importer cannot honor) throws E-ONNX-ATTR; a missing/duplicated/size-mismatched
\ initializer payload throws E-ONNX-DATA; a second graph output on a node, and a
\ missing/repeated ModelProto graph, throw E-ONNX-OUTPUT / E-ONNX-NOGRAPH; every
\ table/name capacity throws E-ONNX-CAP. ONNX owns -5220..-5239; graph.f defines
\ the shared -5224..-5235 family plus -5238 (import.f uses ARITY/IDX/SHAPE/TOPO/
\ NAME/OUTPUT from it).

require lib/prelude.f
require lib/string.f
require maki/onnx/proto.f

-5224 constant E-ONNX-CAP       \ importer table / name / arena capacity exceeded
-5225 constant E-ONNX-NAME      \ missing or duplicate tensor name (SSA), or unnamed record
-5226 constant E-ONNX-DTYPE     \ elem_type / data_type is not FLOAT (f32)
-5227 constant E-ONNX-RANK      \ tensor rank 0 or above 2 (the model IR is 2D)
-5228 constant E-ONNX-DYNSHAPE  \ symbolic / missing / non-positive dim (fail-closed dynamic shape)
-5229 constant E-ONNX-TOPO      \ node input not yet bound (non-topological or unknown name)
-5230 constant E-ONNX-ATTR      \ unsupported attribute or attribute value on a supported op
-5231 constant E-ONNX-SHAPE     \ operand / initializer / output shape mismatch
-5232 constant E-ONNX-OUTPUT    \ graph or node output arity outside the single-output form
-5233 constant E-ONNX-NOGRAPH   \ ModelProto graph field missing or repeated
-5234 constant E-ONNX-DATA      \ initializer payload missing, duplicated, or size-mismatched
-5235 constant E-ONNX-ARITY     \ node input count outside the op's arity
-5238 constant E-ONNX-IDX       \ importer accessor index out of range

package ONNX

\ ---- capacities (v1 import caps; every overflow is a named throw) -----------
128 constant OGN-CAP            \ interned tensor names
64  constant OGN-W              \ bytes per name slot
64  constant OND-CAP            \ nodes
24  constant OND-OPW            \ bytes per op_type slot
128 constant OND-INCAP          \ flat node-input name pool
32  constant OGI-CAP            \ initializers
32  constant OGIN-CAP           \ graph inputs
8   constant OGO-CAP            \ graph outputs

\ ---- recognized attribute kinds + presence bits -----------------------------
0 constant OGA-ALPHA   1 constant OGA-BETA
2 constant OGA-TA      3 constant OGA-TB
4 constant OGA-AXIS    5 constant OGA-PERM
1 constant ATTR-ALPHA  2 constant ATTR-BETA
4 constant ATTR-TA     8 constant ATTR-TB
16 constant ATTR-AXIS  32 constant ATTR-PERM

$3F800000 constant F32-ONE      \ IEEE-754 f32 bit pattern of 1.0

\ ---- interned name pool ------------------------------------------------------
create OGN-TEXT OGN-CAP OGN-W * allot
create OGN-LEN  OGN-CAP cells allot
variable OGN-N

: OGN-CK ( n -- n )
   dup 0 < over OGN-N @ >= or if E-ONNX-IDX throw then ;

: OGN$ ( n -- ptr u8 n ) {: i:n :}             \ name slot -> text
   i OGN-CK OGN-W * OGN-TEXT +  OGN-LEN i cells + @ ;

: OGN-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}   \ slot valid only when true
   OGN-N @ 0 ?do
      a u i OGN$ STR= if i true unloop exit then
   loop 0 false ;

: OGN-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :} \ find-or-add a name; slot index
   a u OGN-FIND if exit then drop
   u OGN-W > if E-ONNX-CAP throw then
   OGN-N @ OGN-CAP >= if E-ONNX-CAP throw then
   OGN-N @ {: i:n :}
   a  i OGN-W * OGN-TEXT +  u  BYTE-COPY
   u OGN-LEN i cells + !
   OGN-N @ 1+ OGN-N !
   i ;

\ ---- node table (slot OND-N is the in-progress staging slot) ----------------
create OND-OP    OND-CAP OND-OPW * allot       \ op_type text
create OND-OPLEN OND-CAP cells allot
create OND-INOFF OND-CAP cells allot           \ input window start in OND-INS
create OND-INCNT OND-CAP cells allot
create OND-OUT   OND-CAP cells allot           \ output name slot (-1 staged)
create OND-ATTRS OND-CAP cells allot           \ ATTR-* presence mask
create OND-ALPHA OND-CAP cells allot           \ f32 bit patterns (default 1.0)
create OND-BETA  OND-CAP cells allot
create OND-TA    OND-CAP cells allot
create OND-TB    OND-CAP cells allot
create OND-AXIS  OND-CAP cells allot           \ default -1 (last axis)
create OND-PERMN OND-CAP cells allot           \ Transpose perm length (0 = absent -> default reverse)
create OND-PERM0 OND-CAP cells allot           \ perm[0], perm[1] (2D transpose is [1,0])
create OND-PERM1 OND-CAP cells allot
variable OND-N
create OND-INS OND-INCAP cells allot           \ flat input name-slot pool
variable OND-INS-U

: OND-CK ( n -- n )
   dup 0 < over OND-N @ >= or if E-ONNX-IDX throw then ;

: OND#      ( -- n )   OND-N @ ;
: OND-OP$   ( n -- ptr u8 n ) {: j:n :}
   j OND-CK OND-OPW * OND-OP +  OND-OPLEN j cells + @ ;
: OND-IN#   ( n -- n )  OND-CK cells OND-INCNT + @ ;
: OND-OUT@  ( n -- n )  OND-CK cells OND-OUT   + @ ;
: OND-ATTRS@ ( n -- n ) OND-CK cells OND-ATTRS + @ ;
: OND-ALPHA@ ( n -- n ) OND-CK cells OND-ALPHA + @ ;
: OND-BETA@  ( n -- n ) OND-CK cells OND-BETA  + @ ;
: OND-TA@    ( n -- n ) OND-CK cells OND-TA    + @ ;
: OND-TB@    ( n -- n ) OND-CK cells OND-TB    + @ ;
: OND-AXIS@  ( n -- n ) OND-CK cells OND-AXIS  + @ ;
: OND-PERMN@ ( n -- n ) OND-CK cells OND-PERMN + @ ;
: OND-PERM0@ ( n -- n ) OND-CK cells OND-PERM0 + @ ;
: OND-PERM1@ ( n -- n ) OND-CK cells OND-PERM1 + @ ;

: OND-IN@ ( n n -- n ) {: j:n k:n :}           \ k-th input name slot of node j
   j OND-CK drop
   k 0 < k j cells OND-INCNT + @ >= or if E-ONNX-IDX throw then
   OND-INS  j cells OND-INOFF + @ k +  cells + @ ;

\ ---- initializer table --------------------------------------------------------
create OGI-NAME OGI-CAP cells allot
create OGI-ROWS OGI-CAP cells allot
create OGI-COLS OGI-CAP cells allot
create OGI-OFF  OGI-CAP cells allot            \ model-absolute f32 payload offset
create OGI-LEN  OGI-CAP cells allot            \ payload byte length
variable OGI-N

: OGI-CK ( n -- n )
   dup 0 < over OGI-N @ >= or if E-ONNX-IDX throw then ;

: OGI#       ( -- n )  OGI-N @ ;
: OGI-NAME@  ( n -- n )  OGI-CK cells OGI-NAME + @ ;
: OGI-ROWS@  ( n -- n )  OGI-CK cells OGI-ROWS + @ ;
: OGI-COLS@  ( n -- n )  OGI-CK cells OGI-COLS + @ ;
: OGI-OFF@   ( n -- n )  OGI-CK cells OGI-OFF  + @ ;
: OGI-LEN@   ( n -- n )  OGI-CK cells OGI-LEN  + @ ;

\ ---- int64 constant table (rank-1 int vectors: Reshape target shape) ----------
\ INT64 initializers are static graph constants (a Reshape shape input, a Slice range),
\ not f32 tensors: they are decoded to cell ints HERE at parse time and never enter the
\ f32 arena. A movement op reads its dims/range from this table by the input name; a name
\ absent here is a runtime-computed operand (fail-closed E-ONNX-DYNSHAPE at import).
16 constant OGIC-CAP            \ int-constant records
8  constant OGIC-VW             \ max ints per record
create OGIC-NAME OGIC-CAP cells allot
create OGIC-NVAL OGIC-CAP cells allot
create OGIC-VALS OGIC-CAP OGIC-VW * cells allot
variable OGIC-N

: OGIC-CK ( n -- n )
   dup 0 < over OGIC-N @ >= or if E-ONNX-IDX throw then ;
: OGIC#      ( -- n )  OGIC-N @ ;
: OGIC-NAME@ ( n -- n )  OGIC-CK cells OGIC-NAME + @ ;
: OGIC-NVAL@ ( n -- n )  OGIC-CK cells OGIC-NVAL + @ ;
: OGIC-VAL@ ( n n -- n ) {: c:n k:n :}         \ k-th int of constant c
   c OGIC-CK drop
   k 0 < k c cells OGIC-NVAL + @ >= or if E-ONNX-IDX throw then
   OGIC-VALS c OGIC-VW * cells +  k cells +  @ ;
: OGIC-FIND ( n -- n bool ) {: ni:n :}         \ name slot -> int-constant index? (slot valid iff true)
   OGIC-N @ 0 ?do  i OGIC-NAME@ ni = if i true unloop exit then  loop  0 false ;

\ read one little-endian int64 at a model-absolute byte offset (span already bounds-checked)
: OGIC-LD64 ( ptr u8 n -- n ) {: a:ptr off:n :}
   a off + c@
   a off 1+ + c@ 8  lshift or
   a off 2 + + c@ 16 lshift or
   a off 3 + + c@ 24 lshift or
   a off 4 + + c@ 32 lshift or
   a off 5 + + c@ 40 lshift or
   a off 6 + + c@ 48 lshift or
   a off 7 + + c@ 56 lshift or ;

\ ---- graph input / output tables ---------------------------------------------
create OGIN-NAME OGIN-CAP cells allot
create OGIN-ROWS OGIN-CAP cells allot
create OGIN-COLS OGIN-CAP cells allot
variable OGIN-N
create OGO-NAME OGO-CAP cells allot
create OGO-ROWS OGO-CAP cells allot
create OGO-COLS OGO-CAP cells allot
variable OGO-N

: OGIN-CK ( n -- n )
   dup 0 < over OGIN-N @ >= or if E-ONNX-IDX throw then ;
: OGO-CK ( n -- n )
   dup 0 < over OGO-N @ >= or if E-ONNX-IDX throw then ;

: OGIN#      ( -- n )  OGIN-N @ ;
: OGIN-NAME@ ( n -- n )  OGIN-CK cells OGIN-NAME + @ ;
: OGIN-ROWS@ ( n -- n )  OGIN-CK cells OGIN-ROWS + @ ;
: OGIN-COLS@ ( n -- n )  OGIN-CK cells OGIN-COLS + @ ;
: OGO#       ( -- n )  OGO-N @ ;
: OGO-NAME@  ( n -- n )  OGO-CK cells OGO-NAME + @ ;
: OGO-ROWS@  ( n -- n )  OGO-CK cells OGO-ROWS + @ ;
: OGO-COLS@  ( n -- n )  OGO-CK cells OGO-COLS + @ ;

\ ---- graph name (denormalized into the IR by import.f) ------------------------
create OGG-NAME OGN-W allot
variable OGG-U
: OGG$ ( -- ptr u8 n )  OGG-NAME OGG-U @ ;

\ ---- per-record parse scratch --------------------------------------------------
variable OGW-GRAPH?             \ ModelProto.graph seen exactly once
variable OGS-DIM#  variable OGS-D0  variable OGS-D1
variable OGS-ELEM               \ elem_type / data_type (-1 = not seen)
variable OGS-SHAPE?             \ a TensorShapeProto was present
variable OGS-NAME-I             \ interned name of the current record (-1 = not seen)
variable OGS-OFF  variable OGS-LEN   \ initializer payload span (-1 = not seen)
variable OGS-DVAL  variable OGS-DSEEN
variable OGA-KIND               \ current attribute kind (-1 = not recognized yet)
variable OGA-F  variable OGA-F?      \ fixed32 attr value bits + seen flag
variable OGA-I  variable OGA-I?      \ varint attr value + seen flag
8 constant OGA-PERM-CAP              \ collected perm ints (Transpose; 2D uses 2)
create OGA-PERM-BUF OGA-PERM-CAP cells allot
variable OGA-PERM-N

: OGA-PERM+ ( n -- ) {: v:n :}       \ append one collected perm int
   OGA-PERM-N @ OGA-PERM-CAP >= if E-ONNX-ATTR throw then
   v OGA-PERM-BUF OGA-PERM-N @ cells + !
   OGA-PERM-N @ 1+ OGA-PERM-N ! ;

: OGS-DIM+ ( n -- ) {: d:n :}                  \ record one collected dim (2D cap)
   d 0 <= if E-ONNX-DYNSHAPE throw then
   OGS-DIM# @ 2 >= if E-ONNX-RANK throw then
   OGS-DIM# @ 0= if d OGS-D0 ! else d OGS-D1 ! then
   OGS-DIM# @ 1+ OGS-DIM# ! ;

: OGS-RANK>RC ( -- n n )                       \ collected dims -> rows cols (1 -> 1xC)
   OGS-DIM# @ 1 = if 1 OGS-D0 @ exit then
   OGS-DIM# @ 2 = if OGS-D0 @ OGS-D1 @ exit then
   E-ONNX-RANK throw ;

\ ---- Dimension / shape / tensor-type / type walkers (window = base lo hi) -----
: OGW-DIM-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-VARINT = and if
      a hi p PB-VARINT@ {: v:n p2:n :}  v OGS-DVAL !  1 OGS-DSEEN !  p2 exit then
   f 2 = w WT-LEN = and if E-ONNX-DYNSHAPE throw then   \ dim_param: symbolic -> fail closed
   a hi p w PB-SKIP ;

: OGW-DIM ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   0 OGS-DSEEN !
   lo begin dup hi < while  a hi rot OGW-DIM-1  repeat drop
   OGS-DSEEN @ 0= if E-ONNX-DYNSHAPE throw then         \ empty Dimension: unknown extent
   OGS-DVAL @ OGS-DIM+ ;

: OGW-SHAPE-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}  a off p2 OGW-DIM  p2 exit then
   a hi p w PB-SKIP ;

: OGW-SHAPE ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   lo begin dup hi < while  a hi rot OGW-SHAPE-1  repeat drop ;

: OGW-TTYPE-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-VARINT = and if
      a hi p PB-VARINT@ {: v:n p2:n :}  v OGS-ELEM !  p2 exit then
   f 2 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      1 OGS-SHAPE? !  a off p2 OGW-SHAPE  p2 exit then
   a hi p w PB-SKIP ;

: OGW-TTYPE ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   lo begin dup hi < while  a hi rot OGW-TTYPE-1  repeat drop ;

: OGW-TYPE-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}  a off p2 OGW-TTYPE  p2 exit then
   a hi p w PB-SKIP ;                                  \ non-tensor types: absent facts fail below

: OGW-TYPE ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   lo begin dup hi < while  a hi rot OGW-TYPE-1  repeat drop ;

\ ---- ValueInfoProto walker (graph inputs and outputs share it) ----------------
: OGW-VI-RESET ( -- )
   -1 OGS-NAME-I !  -1 OGS-ELEM !  0 OGS-SHAPE? !  0 OGS-DIM# ! ;

: OGW-VI-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      a off + len OGN-INTERN OGS-NAME-I !  p2 exit then
   f 2 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}  a off p2 OGW-TYPE  p2 exit then
   a hi p w PB-SKIP ;

\ validate the collected facts, then commit rows cols to the caller
: OGW-VI-FACTS ( -- n n )
   OGS-NAME-I @ 0 < if E-ONNX-NAME throw then
   OGS-ELEM @ -1 = if E-ONNX-DYNSHAPE throw then       \ no tensor type at all
   OGS-ELEM @ 1 <> if E-ONNX-DTYPE throw then          \ TensorProto.FLOAT = 1
   OGS-SHAPE? @ 0= if E-ONNX-DYNSHAPE throw then
   OGS-RANK>RC ;

: OGW-INPUT+ ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   OGW-VI-RESET
   lo begin dup hi < while  a hi rot OGW-VI-1  repeat drop
   OGW-VI-FACTS {: rows:n cols:n :}
   OGIN-N @ OGIN-CAP >= if E-ONNX-CAP throw then
   OGS-NAME-I @ OGIN-N @ cells OGIN-NAME + !
   rows OGIN-N @ cells OGIN-ROWS + !
   cols OGIN-N @ cells OGIN-COLS + !
   OGIN-N @ 1+ OGIN-N ! ;

: OGW-OUTPUT+ ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   OGW-VI-RESET
   lo begin dup hi < while  a hi rot OGW-VI-1  repeat drop
   OGW-VI-FACTS {: rows:n cols:n :}
   OGO-N @ OGO-CAP >= if E-ONNX-CAP throw then
   OGS-NAME-I @ OGO-N @ cells OGO-NAME + !
   rows OGO-N @ cells OGO-ROWS + !
   cols OGO-N @ cells OGO-COLS + !
   OGO-N @ 1+ OGO-N ! ;

\ ---- AttributeProto walker (writes into the staged node slot OND-N) -----------
: OGA-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}    \ attr name -> kind; unknown fails closed
   a u s" alpha"  STR= if OGA-ALPHA OGA-KIND ! exit then
   a u s" beta"   STR= if OGA-BETA  OGA-KIND ! exit then
   a u s" transA" STR= if OGA-TA    OGA-KIND ! exit then
   a u s" transB" STR= if OGA-TB    OGA-KIND ! exit then
   a u s" axis"   STR= if OGA-AXIS  OGA-KIND ! exit then
   a u s" perm"   STR= if OGA-PERM  OGA-KIND ! exit then
   E-ONNX-ATTR throw ;

: OGW-PERM-PACK ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}   \ packed repeated int64 (perm ints)
   lo begin dup hi < while
      a hi rot PB-VARINT@ swap OGA-PERM+
   repeat drop ;

: OGW-ATTR-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      a off + len OGA-NAME!  p2 exit then
   f 2 = w WT-I32 = and if
      a hi p PB-I32@ {: v:n p2:n :}  v OGA-F !  1 OGA-F? !  p2 exit then
   f 3 = w WT-VARINT = and if
      a hi p PB-VARINT@ {: v:n p2:n :}  v OGA-I !  1 OGA-I? !  p2 exit then
   f 8 = w WT-VARINT = and if                          \ AttributeProto.ints (perm), unpacked
      a hi p PB-VARINT@ {: v:n p2:n :}  v OGA-PERM+  p2 exit then
   f 8 = w WT-LEN = and if                             \ AttributeProto.ints, packed
      a hi p PB-LEN@ {: off:n len:n p2:n :}  a off p2 OGW-PERM-PACK  p2 exit then
   a hi p w PB-SKIP ;

: OGA-BIT+ ( n -- )                            \ record presence on the staged node
   OND-ATTRS OND-N @ cells + dup @ rot or swap ! ;

: OGA-F-COMMIT ( ptr a -- ) {: fld:ptr :}      \ float attr: require f, store its bits
   OGA-F? @ 0= if E-ONNX-ATTR throw then
   OGA-F @ fld OND-N @ cells + ! ;

: OGA-I-COMMIT ( ptr a -- ) {: fld:ptr :}      \ int attr: require i, store the value
   OGA-I? @ 0= if E-ONNX-ATTR throw then
   OGA-I @ fld OND-N @ cells + ! ;

: OGA-PERM-COMMIT ( -- )                       \ perm ints -> length + first two elems on the node
   OGA-PERM-N @ 0= if E-ONNX-ATTR throw then    \ a perm attr with no ints
   OGA-PERM-N @ OND-PERMN OND-N @ cells + !
   OGA-PERM-BUF @  OND-PERM0 OND-N @ cells + !
   OGA-PERM-N @ 1 > if OGA-PERM-BUF cell+ @ else -1 then  OND-PERM1 OND-N @ cells + ! ;

: OGA-COMMIT ( -- )                            \ dispatch the finished attribute
   OGA-KIND @ 0 < if E-ONNX-ATTR throw then
   OGA-KIND @ OGA-ALPHA = if OND-ALPHA OGA-F-COMMIT ATTR-ALPHA OGA-BIT+ exit then
   OGA-KIND @ OGA-BETA  = if OND-BETA  OGA-F-COMMIT ATTR-BETA  OGA-BIT+ exit then
   OGA-KIND @ OGA-TA    = if OND-TA    OGA-I-COMMIT ATTR-TA    OGA-BIT+ exit then
   OGA-KIND @ OGA-TB    = if OND-TB    OGA-I-COMMIT ATTR-TB    OGA-BIT+ exit then
   OGA-KIND @ OGA-PERM  = if OGA-PERM-COMMIT ATTR-PERM OGA-BIT+ exit then
   OND-AXIS OGA-I-COMMIT  ATTR-AXIS OGA-BIT+ ;

: OGW-ATTR ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   -1 OGA-KIND !  0 OGA-F? !  0 OGA-I? !  0 OGA-PERM-N !
   lo begin dup hi < while  a hi rot OGW-ATTR-1  repeat drop
   OGA-COMMIT ;

\ ---- NodeProto walker -----------------------------------------------------------
: OND-STAGE ( -- )                             \ stage defaults into slot OND-N
   OND-N @ OND-CAP >= if E-ONNX-CAP throw then
   OND-N @ {: j:n :}
   OND-INS-U @ j cells OND-INOFF + !
   0  j cells OND-INCNT + !
   -1 j cells OND-OUT   + !
   0  j cells OND-ATTRS + !
   F32-ONE j cells OND-ALPHA + !
   F32-ONE j cells OND-BETA  + !
   0  j cells OND-TA + !  0 j cells OND-TB + !
   -1 j cells OND-AXIS + !
   0  j cells OND-PERMN + !
   0  j cells OND-OPLEN + ! ;

: OND-IN+ ( n -- ) {: ni:n :}                  \ append one input name slot to the pool
   OND-INS-U @ OND-INCAP >= if E-ONNX-CAP throw then
   ni OND-INS OND-INS-U @ cells + !
   OND-INS-U @ 1+ OND-INS-U !
   OND-INCNT OND-N @ cells + dup @ 1+ swap ! ;

: OND-OUT! ( n -- ) {: ni:n :}                 \ the single node output (second fails)
   OND-OUT OND-N @ cells + @ 0 < 0= if E-ONNX-OUTPUT throw then
   ni OND-OUT OND-N @ cells + ! ;

: OND-OP! ( ptr u8 n -- ) {: a:ptr u:n :}      \ record the op_type text
   u OND-OPW > if E-ONNX-CAP throw then
   a  OND-N @ OND-OPW * OND-OP +  u  BYTE-COPY
   u OND-OPLEN OND-N @ cells + ! ;

: OGW-NODE-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   w WT-LEN = if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      f 1 = if a off + len OGN-INTERN OND-IN+  p2 exit then
      f 2 = if a off + len OGN-INTERN OND-OUT! p2 exit then
      f 4 = if a off + len OND-OP!             p2 exit then
      f 5 = if a off p2 OGW-ATTR               p2 exit then
      p2 exit then
   a hi p w PB-SKIP ;

: OGW-NODE ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   OND-STAGE
   lo begin dup hi < while  a hi rot OGW-NODE-1  repeat drop
   OND-OUT OND-N @ cells + @ 0 < if E-ONNX-OUTPUT throw then
   OND-N @ 1+ OND-N ! ;

\ ---- TensorProto (initializer) walker --------------------------------------------
: OGS-SPAN! ( n n -- ) {: off:n len:n :}       \ record the single payload span
   OGS-OFF @ 0 < 0= if E-ONNX-DATA throw then  \ raw_data + float_data / repeats: reject
   off OGS-OFF !  len OGS-LEN ! ;

: OGW-PDIMS ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}   \ packed repeated int64 dims
   lo begin dup hi < while
      a hi rot PB-VARINT@ swap OGS-DIM+
   repeat drop ;

: OGW-TENSOR-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 1 = w WT-VARINT = and if
      a hi p PB-VARINT@ swap OGS-DIM+ exit then
   f 2 = w WT-VARINT = and if
      a hi p PB-VARINT@ {: v:n p2:n :}  v OGS-ELEM !  p2 exit then
   w WT-LEN = if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      f 1 = if a off p2 OGW-PDIMS  p2 exit then
      f 4 = if off len OGS-SPAN!   p2 exit then
      f 8 = if a off + len OGN-INTERN OGS-NAME-I !  p2 exit then
      f 9 = if off len OGS-SPAN!   p2 exit then
      p2 exit then
   a hi p w PB-SKIP ;

: OGW-TENSOR-F32-FACTS ( -- n n )              \ FLOAT initializer: rows cols; f32 payload span
   OGS-RANK>RC {: rows:n cols:n :}
   OGS-OFF @ 0 < if E-ONNX-DATA throw then
   OGS-LEN @ rows cols * 4 * <> if E-ONNX-DATA throw then
   rows cols ;

: OGW-TENSOR-F32 ( -- )                        \ commit a FLOAT (f32) initializer into OGI
   OGI-N @ OGI-CAP >= if E-ONNX-CAP throw then
   OGW-TENSOR-F32-FACTS {: rows:n cols:n :}
   OGS-NAME-I @ OGI-N @ cells OGI-NAME + !
   rows OGI-N @ cells OGI-ROWS + !
   cols OGI-N @ cells OGI-COLS + !
   OGS-OFF @ OGI-N @ cells OGI-OFF + !
   OGS-LEN @ OGI-N @ cells OGI-LEN + !
   OGI-N @ 1+ OGI-N ! ;

: OGW-TENSOR-INT ( ptr u8 -- ) {: a:ptr :}     \ commit an INT64 rank-1 constant into OGIC
   OGIC-N @ OGIC-CAP >= if E-ONNX-CAP throw then
   OGS-DIM# @ 1 <> if E-ONNX-DATA throw then    \ int constants are rank-1 vectors
   OGS-D0 @ {: nv:n :}
   nv OGIC-VW > if E-ONNX-CAP throw then
   OGS-OFF @ 0 < if E-ONNX-DATA throw then
   OGS-LEN @ nv 8 * <> if E-ONNX-DATA throw then
   OGS-NAME-I @ OGIC-N @ cells OGIC-NAME + !
   nv OGIC-N @ cells OGIC-NVAL + !
   nv 0 ?do
      a  OGS-OFF @ i 8 * +  OGIC-LD64
      OGIC-VALS OGIC-N @ OGIC-VW * cells +  i cells +  !
   loop
   OGIC-N @ 1+ OGIC-N ! ;

\ dispatch a TensorProto by data_type: FLOAT (1) -> f32 arena; INT64 (7) -> int constant.
: OGW-TENSOR ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   -1 OGS-NAME-I !  -1 OGS-ELEM !  0 OGS-DIM# !  -1 OGS-OFF !  -1 OGS-LEN !
   lo begin dup hi < while  a hi rot OGW-TENSOR-1  repeat drop
   OGS-NAME-I @ 0 < if E-ONNX-NAME throw then
   OGS-ELEM @ 1 = if OGW-TENSOR-F32 exit then
   OGS-ELEM @ 7 = if a OGW-TENSOR-INT exit then
   E-ONNX-DTYPE throw ;

\ ---- GraphProto / ModelProto walkers ----------------------------------------------
: OGG-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u OGN-W > if E-ONNX-CAP throw then
   a OGG-NAME u BYTE-COPY  u OGG-U ! ;

: OGW-GRAPH-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   w WT-LEN = if
      a hi p PB-LEN@ {: off:n len:n p2:n :}
      f 1  = if a off p2 OGW-NODE     p2 exit then
      f 2  = if a off + len OGG-NAME! p2 exit then
      f 5  = if a off p2 OGW-TENSOR   p2 exit then
      f 11 = if a off p2 OGW-INPUT+   p2 exit then
      f 12 = if a off p2 OGW-OUTPUT+  p2 exit then
      p2 exit then
   a hi p w PB-SKIP ;

: OGW-GRAPH ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   OGW-GRAPH? @ if E-ONNX-NOGRAPH throw then   \ a second graph field: reject
   1 OGW-GRAPH? !
   lo begin dup hi < while  a hi rot OGW-GRAPH-1  repeat drop ;

: OGW-MODEL-1 ( ptr u8 n n -- n ) {: a:ptr hi:n pos:n :}
   a hi pos PB-TAG@ {: f:n w:n p:n :}
   f 7 = w WT-LEN = and if
      a hi p PB-LEN@ {: off:n len:n p2:n :}  a off p2 OGW-GRAPH  p2 exit then
   a hi p w PB-SKIP ;

: OGW-MODEL ( ptr u8 n n -- ) {: a:ptr lo:n hi:n :}
   lo begin dup hi < while  a hi rot OGW-MODEL-1  repeat drop ;

: OG-RESET ( -- )
   0 OGN-N !  0 OND-N !  0 OND-INS-U !
   0 OGI-N !  0 OGIC-N !  0 OGIN-N !  0 OGO-N !
   0 OGG-U !  0 OGW-GRAPH? ! ;

: OG-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}     \ walk a serialized ModelProto into the tables
   OG-RESET
   a 0 u OGW-MODEL
   OGW-GRAPH? @ 0= if E-ONNX-NOGRAPH throw then ;

;package
