\ maki/onnx/encode.f - protobuf wire-format subset encoder (fixture builder).
\
\ The write side of maki/onnx/proto.f: checked words that append tag / varint /
\ length-delimited bytes to a builder, used by the ONNX importer tests to
\ hand-encode ModelProto fixtures in source (no binary blob files, no external
\ tools). Nesting uses a small builder stack: `f ENC-SUB ... ;ENC-SUB` builds the
\ child message in its own level buffer, then the closer frames it into the
\ parent as one length-delimited field f. ENC$ returns the finished top-level
\ bytes.
\
\ The encoder deliberately does NOT validate field numbers or wire types - the
\ negative decoder fixtures must be able to emit illegal wire data (field 0,
\ group wires, truncations via ENC-B). Fail closed on the encoder's own state
\ only: an unbalanced ;ENC-SUB / an ENC$ inside an open sub throws E-ONNX-ENC;
\ a level overflowing its buffer or the depth cap throws E-ONNX-ENC-CAP. Floats
\ are appended as little-endian IEEE-754 f32 via F64>F32 (lib/ptx/cg.f, the
\ audited narrowing). ONNX owns -5220..-5239; encode.f uses -5236..-5237.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require maki/onnx/proto.f

-5236 constant E-ONNX-ENC      \ encoder misuse: unbalanced ;ENC-SUB or ENC$ inside a sub
-5237 constant E-ONNX-ENC-CAP  \ encoder level buffer or builder depth exceeded

package ONNX

8     constant ENC-DEPTH-CAP    \ builder stack depth (model>graph>vi>type>tensor>shape>dim)
$1000 constant ENC-LVL-CAP      \ bytes per level (level 0 holds the whole model)

create ENC-BUFS ENC-DEPTH-CAP ENC-LVL-CAP * allot
create ENC-LENS ENC-DEPTH-CAP cells allot
create ENC-FLDS ENC-DEPTH-CAP cells allot      \ pending field number per open sub
variable ENC-D                                  \ current depth (0 = top level)
variable ENC-V                                  \ varint encode scratch

: ENC-LVL  ( -- ptr u8 )  ENC-D @ ENC-LVL-CAP * ENC-BUFS + ;
: ENC-LEN@ ( -- n )       ENC-LENS ENC-D @ cells + @ ;
: ENC-LEN! ( n -- )       ENC-LENS ENC-D @ cells + ! ;

public

: ENC-RESET ( -- )  0 ENC-D !  0 ENC-LEN! ;

: ENC-B ( n -- ) {: b:n :}                     \ append one raw byte at the current level
   ENC-LEN@ ENC-LVL-CAP >= if E-ONNX-ENC-CAP throw then
   b ENC-LVL ENC-LEN@ + c!
   ENC-LEN@ 1+ ENC-LEN! ;

private

\ emit one 7-bit varint group from ENC-V (with continuation bit)
: ENC-V7 ( -- )
   ENC-V @ $7F and  ENC-V @ 7 rshift  {: g:n rest:n :}
   rest 0<> if g $80 or else g then ENC-B
   rest ENC-V ! ;

public

: ENC-VARINT ( n -- )                          \ append a varint (negatives: 10-byte pattern)
   ENC-V !
   begin ENC-V7 ENC-V @ 0<> while repeat ;

: ENC-TAG ( n n -- ) {: f:n w:n :}             \ append tag = (field << 3) | wire
   f 3 lshift w or ENC-VARINT ;

: ENC-INT ( n n -- ) {: v:n f:n :}             \ append varint field f with value v
   f WT-VARINT ENC-TAG  v ENC-VARINT ;

: ENC-STR ( ptr u8 n n -- ) {: a:ptr u:n f:n :} \ append string/bytes field f
   f WT-LEN ENC-TAG  u ENC-VARINT
   u 0 ?do  a i + c@ ENC-B  loop ;

: ENC-F32 ( r -- )                             \ append 4 raw LE f32 bytes (payload element)
   F64>F32 {: b:n :}
   b $FF and ENC-B          b 8 rshift $FF and ENC-B
   b 16 rshift $FF and ENC-B  b 24 rshift $FF and ENC-B ;

: ENC-F32A ( r n -- ) {: x:r f:n :}            \ append fixed32 float field f (attr float)
   f WT-I32 ENC-TAG  x ENC-F32 ;

: ENC-SUB ( n -- ) {: f:n :}                   \ open a nested message for field f
   ENC-D @ 1+ ENC-DEPTH-CAP >= if E-ONNX-ENC-CAP throw then
   f  ENC-FLDS ENC-D @ 1+ cells +  !
   ENC-D @ 1+ ENC-D !
   0 ENC-LEN! ;

: ;ENC-SUB ( -- )                              \ close it: frame into the parent as field f
   ENC-D @ 0= if E-ONNX-ENC throw then
   ENC-LVL ENC-LEN@ {: a:ptr u:n :}
   ENC-FLDS ENC-D @ cells + @ {: f:n :}
   ENC-D @ 1- ENC-D !
   f WT-LEN ENC-TAG  u ENC-VARINT
   u 0 ?do  a i + c@ ENC-B  loop ;

: ENC$ ( -- ptr u8 n )                         \ the finished top-level bytes
   ENC-D @ 0<> if E-ONNX-ENC throw then
   ENC-LVL ENC-LEN@ ;

;package
