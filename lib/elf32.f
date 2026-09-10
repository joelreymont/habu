\ Bounded ELF32 little-endian header and program-segment reader.
require lib/type/deftype.f
require lib/cad-num-types.f
require lib/errors.f

package ELF32
public

DEFTYPE FILE-KIND
DEFTYPE MACHINE-ID
DEFTYPE ADDRESS
DEFTYPE PROGRAM-COUNT
DEFTYPE PROGRAM-INDEX
DEFTYPE SEGMENT-KIND

E-ELF32-FORMAT constant E-FORMAT
E-ELF32-BOUNDS constant E-BOUNDS
E-ELF32-UNSUPPORTED constant E-UNSUPPORTED

STRUCTURE header 0
   FIELD kind file-kind
   FIELD machine machine-id
   FIELD entry address
   FIELD flags n
   FIELD os-abi n
   FIELD abi-version n
   FIELD programs program-count
;STRUCTURE

STRUCTURE segment 0
   FIELD data ptr u8
   FIELD size CAD-NUM:byte-len
   FIELD kind segment-kind
   FIELD virtual address
   FIELD physical address
   FIELD memory-size CAD-NUM:byte-len
   FIELD flags n
   FIELD alignment n
;STRUCTURE

private

$34 constant HEADER-BYTES
$20 constant PROGRAM-BYTES

CAST: BLEN>N ( CAD-NUM:byte-len -- n )

: AS-BLEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-BOUNDS throw ENDOF
      zero OF E-BOUNDS throw ENDOF
      overflow OF E-BOUNDS throw ENDOF
      underflow OF E-BOUNDS throw ENDOF
      bad-alignment OF E-BOUNDS throw ENDOF
      misaligned OF E-BOUNDS throw ENDOF
   ;MATCH ;

: CHECK-RANGE ( n n n -- ) {: total:n offset:n size:n :}
   offset 0 < size 0 < or if E-BOUNDS throw then
   offset total > if E-BOUNDS throw then
   size total offset - > if E-BOUNDS throw then ;

: LE16@ ( ptr u8 -- n ) {: data :}
   data c@ data $01 + c@ 8 lshift or ;

: LE32@ ( ptr u8 -- n ) {: data :}
   data LE16@ data $02 + LE16@ 16 lshift or ;

: PH-OFFSET ( ptr u8 -- n )
   $1C + LE32@ ;

: PH-STRIDE ( ptr u8 -- n )
   $2A + LE16@ ;

: PH-COUNT ( ptr u8 -- n )
   $2C + LE16@ ;

: CHECK-IDENT ( ptr u8 -- ) {: data :}
   data LE32@ $464C457F <> if E-FORMAT throw then
   data $04 + c@ 1 <> data $05 + c@ 1 <> or if E-UNSUPPORTED throw then
   data $06 + c@ 1 <> data $14 + LE32@ 1 <> or if E-UNSUPPORTED throw then ;

: CHECK-HEADER ( ptr u8 n -- ) {: data size:n :}
   size 0 HEADER-BYTES CHECK-RANGE data CHECK-IDENT
   data $28 + LE16@ dup HEADER-BYTES < if E-FORMAT throw then
   size 0 rot CHECK-RANGE ;

: CHECK-TABLE ( ptr u8 n -- ) {: data size:n :}
   data PH-COUNT dup $FFFF = if E-UNSUPPORTED throw then
   dup 0= if drop exit then
   data PH-STRIDE dup PROGRAM-BYTES < if E-FORMAT throw then
   * size data PH-OFFSET rot CHECK-RANGE
   data PH-OFFSET 0= if E-FORMAT throw then ;

: HEADER-CHECKED ( ptr u8 n -- )
   2dup CHECK-HEADER CHECK-TABLE ;

: PROGRAM-ROW ( ptr u8 n -- ptr u8 ) {: data index:n :}
   data data PH-OFFSET + index data PH-STRIDE * + ;

: CHECK-INDEX ( ptr u8 n -- ) {: data index:n :}
   index 0 < index data PH-COUNT >= or if E-BOUNDS throw then ;

: CHECK-LOAD-EXTENT ( ptr u8 -- ) {: row :}
   row $10 + LE32@ row $14 + LE32@ > if E-FORMAT throw then
   row $08 + LE32@ row $14 + LE32@ + $100000000 > if E-BOUNDS throw then ;

: CHECK-LOAD-ALIGNMENT ( ptr u8 -- ) {: row :}
   row $1C + LE32@ dup 1 <= if drop exit then {: alignment:n :}
   alignment alignment 1 - and 0 <> if E-FORMAT throw then
   row $08 + LE32@ alignment mod row $04 + LE32@ alignment mod
   <> if E-FORMAT throw then ;

: CHECK-ROW ( ptr u8 n -- ) {: row total:n :}
   row LE32@ 0= if exit then
   row $10 + LE32@ dup 0 <> if total row $04 + LE32@ rot CHECK-RANGE else drop then
   row LE32@ 1 = if row CHECK-LOAD-EXTENT row CHECK-LOAD-ALIGNMENT then ;

: SEGMENT-SPAN ( ptr u8 ptr u8 -- ptr u8 CAD-NUM:byte-len ) {: data row :}
   row LE32@ 0= row $10 + LE32@ 0= or if data 0 AS-BLEN exit then
   data row $04 + LE32@ + row $10 + LE32@ AS-BLEN ;

: MAKE-HEADER ( ptr u8 -- header ) {: data :}
   data $10 + LE16@ >FILE-KIND data $12 + LE16@ >MACHINE-ID
   data $18 + LE32@ >ADDRESS data $24 + LE32@
   data $07 + c@ data $08 + c@ data PH-COUNT >PROGRAM-COUNT
   ELF32-HEADER:MAKE ;

: MAKE-SEGMENT ( ptr u8 ptr u8 -- segment ) {: data row :}
   data row SEGMENT-SPAN row LE32@ >SEGMENT-KIND
   row $08 + LE32@ >ADDRESS row $0C + LE32@ >ADDRESS
   row $14 + LE32@ AS-BLEN row $18 + LE32@ row $1C + LE32@
   ELF32-SEGMENT:MAKE ;

public

: FILE-BYTES ( n -- CAD-NUM:byte-len )
   AS-BLEN ;

\ INFO validates the header and table bounds, not every segment or the sections.
: INFO ( ptr u8 CAD-NUM:byte-len -- header ) {: data size:CAD-NUM:byte-len :}
   data size BLEN>N HEADER-CHECKED data MAKE-HEADER ;

\ The result borrows bytes from the unchanged input until its caller is finished.
: PROGRAM ( ptr u8 CAD-NUM:byte-len program-index -- segment )
   {: data size:CAD-NUM:byte-len index:program-index :}
   data size BLEN>N HEADER-CHECKED data index PROGRAM-INDEX>N CHECK-INDEX
   data index PROGRAM-INDEX>N PROGRAM-ROW {: row :}
   row size BLEN>N CHECK-ROW data row MAKE-SEGMENT ;

: VALIDATE ( ptr u8 CAD-NUM:byte-len -- ) {: data size:CAD-NUM:byte-len :}
   data size BLEN>N HEADER-CHECKED
   data PH-COUNT 0 ?do data i PROGRAM-ROW size BLEN>N CHECK-ROW loop ;

;package
