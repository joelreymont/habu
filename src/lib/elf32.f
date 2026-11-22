split-str
\ elf32.f - bounded ELF32 little-endian header and program-segment reader, in
\ `package ELF32`; it never allocates, loads code, reads sections or relocations
\ or picks an ABI. The caller supplies the input's real readable extent;
\ addresses are file values, never host pointers. A segment's data span borrows
\ the input: do not release, mutate or resize it while the span is in use.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state; no error changes input.
require src/lib/type/deftype.f
require src/lib/num-types.f
require src/lib/error.f

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

STRUCTURE header
   FIELD kind file-kind
   FIELD machine machine-id
   FIELD entry address
   FIELD flags n
   FIELD os-abi n
   FIELD abi-version n
   FIELD programs program-count
;STRUCTURE

STRUCTURE segment
   FIELD data ^u8
   FIELD size NUM:byte-len
   FIELD kind segment-kind
   FIELD virtual address
   FIELD physical address
   FIELD memory-size NUM:byte-len
   FIELD flags n
   FIELD alignment n
;STRUCTURE

private

$34 constant HEADER-BYTES
$20 constant PROGRAM-BYTES

CAST: BLEN>N ( NUM:byte-len -- n )
\ Each caller has shown the cell nonnegative.
CAST: N>BLEN ( n -- NUM:byte-len )

: AS-BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF E-BOUNDS throw ENDOF
      some OF ENDOF
   ;MATCH ;

: CHECK-RANGE ( n n n -- ) {: total:n offset:n size:n :}
   offset 0 < size 0 < or if E-BOUNDS throw then
   offset total > if E-BOUNDS throw then
   size total offset - > if E-BOUNDS throw then ;

: LE16@ ( ^u8 -- n ) {: data :}
   data c@ data $01 + c@ 8 lshift or ;

: LE32@ ( ^u8 -- n ) {: data :}
   data LE16@ data $02 + LE16@ 16 lshift or ;

: PH-OFFSET ( ^u8 -- n )
   $1C + LE32@ ;

: PH-STRIDE ( ^u8 -- n )
   $2A + LE16@ ;

: PH-COUNT ( ^u8 -- n )
   $2C + LE16@ ;

: CHECK-IDENT ( ^u8 -- ) {: data :}
   data LE32@ $464C457F <> if E-FORMAT throw then
   data $04 + c@ 1 <> data $05 + c@ 1 <> or if E-UNSUPPORTED throw then
   data $06 + c@ 1 <> data $14 + LE32@ 1 <> or if E-UNSUPPORTED throw then ;

: CHECK-HEADER ( str -- ) {: data size:n :}
   size 0 HEADER-BYTES CHECK-RANGE data CHECK-IDENT
   data $28 + LE16@ dup HEADER-BYTES < if E-FORMAT throw then
   size 0 rot CHECK-RANGE ;

: CHECK-TABLE ( str -- ) {: data size:n :}
   data PH-COUNT dup $FFFF = if E-UNSUPPORTED throw then
   dup 0= if drop exit then
   data PH-STRIDE dup PROGRAM-BYTES < if E-FORMAT throw then
   * size data PH-OFFSET rot CHECK-RANGE
   data PH-OFFSET 0= if E-FORMAT throw then ;

: HEADER-CHECKED ( str -- )
   2dup CHECK-HEADER CHECK-TABLE ;

: PROGRAM-ROW ( ^u8 n -- ^u8 ) {: data index:n :}
   data data PH-OFFSET + index data PH-STRIDE * + ;

: CHECK-INDEX ( ^u8 n -- ) {: data index:n :}
   index 0 < index data PH-COUNT >= or if E-BOUNDS throw then ;

: CHECK-LOAD-EXTENT ( ^u8 -- ) {: row :}
   row $10 + LE32@ row $14 + LE32@ > if E-FORMAT throw then
   row $08 + LE32@ row $14 + LE32@ + $100000000 > if E-BOUNDS throw then ;

: CHECK-LOAD-ALIGNMENT ( ^u8 -- ) {: row :}
   row $1C + LE32@ dup 1 <= if drop exit then {: alignment:n :}
   alignment alignment 1 - and 0 <> if E-FORMAT throw then
   row $08 + LE32@ alignment mod row $04 + LE32@ alignment mod
   <> if E-FORMAT throw then ;

: CHECK-ROW ( ^u8 n -- ) {: row total:n :}
   row LE32@ 0= if exit then
   row $10 + LE32@ dup 0 <> if total row $04 + LE32@ rot CHECK-RANGE else drop then
   row LE32@ 1 = if row CHECK-LOAD-EXTENT row CHECK-LOAD-ALIGNMENT then ;

: SEGMENT-SPAN ( ^u8 ^u8 -- ^u8 NUM:byte-len ) {: data row :}
   row LE32@ 0= row $10 + LE32@ 0= or if data 0 N>BLEN exit then
   data row $04 + LE32@ + row $10 + LE32@ N>BLEN ;

: MAKE-HEADER ( ^u8 -- header ) {: data :}
   data $10 + LE16@ >FILE-KIND data $12 + LE16@ >MACHINE-ID
   data $18 + LE32@ >ADDRESS data $24 + LE32@
   data $07 + c@ data $08 + c@ data PH-COUNT >PROGRAM-COUNT
   ELF32-HEADER:MAKE ;

: MAKE-SEGMENT ( ^u8 ^u8 -- segment ) {: data row :}
   data row SEGMENT-SPAN row LE32@ >SEGMENT-KIND
   row $08 + LE32@ >ADDRESS row $0C + LE32@ >ADDRESS
   row $14 + LE32@ N>BLEN row $18 + LE32@ row $1C + LE32@
   ELF32-SEGMENT:MAKE ;

public

: FILE-BYTES ( n -- NUM:byte-len )
   AS-BLEN ;

\ INFO validates the header and table bounds, not every segment or the sections.
: INFO ( ^u8 NUM:byte-len -- header ) {: data size:NUM:byte-len :}
   data size BLEN>N HEADER-CHECKED data MAKE-HEADER ;

\ The result borrows bytes from the unchanged input until its caller is finished.
: PROGRAM ( ^u8 NUM:byte-len program-index -- segment )
   {: data size:NUM:byte-len index:program-index :}
   data size BLEN>N HEADER-CHECKED data index PROGRAM-INDEX>N CHECK-INDEX
   data index PROGRAM-INDEX>N PROGRAM-ROW {: row :}
   row size BLEN>N CHECK-ROW data row MAKE-SEGMENT ;

\ Accepted: version 1, ELFCLASS32, ELFDATA2LSB, larger declared header and entry
\ strides with valid bounds, no program entries; PN_XNUM is E-UNSUPPORTED. Every
\ nonempty non-null segment fits the input; PT_LOAD also needs file size <=
\ memory size, a virtual extent inside 32 bits and valid alignment. A zero-length
\ segment and PT_NULL answer an empty span at the input start. Physical
\ addresses and unknown machine and segment ids pass through; ordering, dynamic
\ linking, permissions, entry and machine flags are not checked.
: VALIDATE ( ^u8 NUM:byte-len -- ) {: data size:NUM:byte-len :}
   data size BLEN>N HEADER-CHECKED
   data PH-COUNT 0 ?do data i PROGRAM-ROW size BLEN>N CHECK-ROW loop ;

;package
