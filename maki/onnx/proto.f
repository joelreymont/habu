\ maki/onnx/proto.f - protobuf wire-format subset decoder (ONNX import substrate).
\
\ The byte-level layer of the ONNX graph importer (dot habu-maki-onnx-graph): a
\ protobuf message is a sequence of (tag, value) fields where
\ tag = (field << 3) | wire-type. ONNX uses wire type 0 (varint: int32/int64/
\ enum), 2 (length-delimited: nested messages, strings, bytes, packed repeated),
\ 5 (fixed32: float attrs), and this decoder also skips wire type 1 (fixed64).
\ A varint is little-endian base-128 with an MSB continuation bit, at most 10
\ bytes for 64 bits.
\
\ Every reader takes the model byte span plus an ABSOLUTE position and returns
\ the value(s) and the position after the field, so nested messages are walked
\ as (base, lo, hi) windows and recorded offsets stay model-absolute (no pointer
\ slicing, no pointer cells). Fail closed: a truncated varint or a length/fixed
\ field overrunning the window throws E-PB-TRUNC; an over-long or overflowing
\ varint throws E-PB-VARINT; the deprecated group wires (3/4) and reserved wire
\ types cannot be skipped and throw E-PB-WIRE; field number 0 throws E-PB-FIELD.
\ The ONNX subsystem owns -5220..-5239; proto.f uses -5220..-5223.

require lib/prelude.f

-5220 constant E-PB-TRUNC    \ truncated varint / length or fixed field overruns the buffer
-5221 constant E-PB-VARINT   \ varint longer than 10 bytes or 64-bit overflow
-5222 constant E-PB-WIRE     \ wire type this decoder cannot skip (groups / reserved)
-5223 constant E-PB-FIELD    \ field number 0 (illegal in protobuf)

package ONNX
public

\ ---- protobuf wire types ----------------------------------------------------
0 constant WT-VARINT
1 constant WT-I64
2 constant WT-LEN
5 constant WT-I32

private

10 constant PB-VMAX          \ max varint bytes (64 bits of payload)

variable PB-V                \ varint accumulator
variable PB-SH               \ varint shift (bits consumed so far)
variable PB-P                \ varint cursor
variable PB-MORE             \ varint continuation flag

: PB-BYTE@ ( ptr u8 n n -- n ) {: a:ptr u:n pos:n :}   \ bounds-checked byte read
   pos u >= if E-PB-TRUNC throw then
   a pos + c@ ;

\ one varint byte: accumulate 7 bits, advance, clear PB-MORE on the last byte
: PB-VSTEP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PB-P @ PB-BYTE@ {: b:n :}
   PB-SH @ 63 =  b $7E and 0<>  and if E-PB-VARINT throw then   \ 10th byte: only bit 0 fits
   b $7F and PB-SH @ lshift  PB-V @ or  PB-V !
   PB-P @ 1+ PB-P !
   PB-SH @ 7 + PB-SH !
   b $80 and 0= if 0 PB-MORE ! then
   PB-SH @ PB-VMAX 7 * >=  PB-MORE @ 0<>  and if E-PB-VARINT throw then ;

public

\ read one varint at pos; value is the raw 64-bit pattern (an int64 field that
\ encodes a negative number comes back as the negative cell)
: PB-VARINT@ ( ptr u8 n n -- n n ) {: a:ptr u:n pos:n :}
   0 PB-V !  0 PB-SH !  pos PB-P !  1 PB-MORE !
   begin PB-MORE @ 0<> while  a u PB-VSTEP  repeat
   PB-V @ PB-P @ ;

\ decode one tag: field number + wire type + position after the tag
: PB-TAG@ ( ptr u8 n n -- n n n ) {: a:ptr u:n pos:n :}
   a u pos PB-VARINT@ {: v:n p:n :}
   v 3 rshift {: f:n :}
   f 0= if E-PB-FIELD throw then
   f  v 7 and  p ;

\ length-delimited payload: start offset + byte length + position after it
: PB-LEN@ ( ptr u8 n n -- n n n ) {: a:ptr u:n pos:n :}
   a u pos PB-VARINT@ {: len:n off:n :}
   len 0 <  off len + u >  or if E-PB-TRUNC throw then
   off  len  off len + ;

\ little-endian fixed32: the raw 32-bit pattern + position after it
: PB-I32@ ( ptr u8 n n -- n n ) {: a:ptr u:n pos:n :}
   pos 4 + u > if E-PB-TRUNC throw then
   a pos + c@
   a pos 1+ + c@ 8  lshift or
   a pos 2 + + c@ 16 lshift or
   a pos 3 + + c@ 24 lshift or
   pos 4 + ;

\ skip one field's payload by wire type; fail closed on unskippable wires
: PB-SKIP ( ptr u8 n n n -- n ) {: a:ptr u:n pos:n w:n :}
   w WT-VARINT = if a u pos PB-VARINT@ nip exit then
   w WT-I64    = if pos 8 +  dup u > if E-PB-TRUNC throw then  exit then
   w WT-LEN    = if a u pos PB-LEN@ nip nip exit then
   w WT-I32    = if pos 4 +  dup u > if E-PB-TRUNC throw then  exit then
   E-PB-WIRE throw ;

;package
