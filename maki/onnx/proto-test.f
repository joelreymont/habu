\ maki/onnx/proto-test.f - checked tests for the protobuf wire-format decoder.
\
\ Fixtures are hand-encoded IN SOURCE by the checked encoder DSL
\ (maki/onnx/encode.f) - no binary blob files, no external tools - and the
\ encoder is cross-validated byte-exactly against the spec examples (300 =
\ AC 02, tag(7,LEN) = 3A, max u64 = FF*9 01). Covers: varint edges (single
\ byte, multibyte, 10-byte max, negative int64), tag decode, length-delimited
\ slices, fixed32, unknown-field skip over every skippable wire type, and the
\ fail-closed paths: truncated varint, 64-bit varint overflow, 11-byte varint,
\ length overrun, short fixed fields, field number 0, and the unskippable
\ group wire.

require lib/test.f
require lib/string.f
require maki/onnx/proto.f
require maki/onnx/encode.f

package ONNX-PROTO-TEST

\ ---- fail-closed probes ------------------------------------------------------
: TRY-TRUNCV ( -- )                            \ lone continuation byte: truncated varint
   ONNX:ENC-RESET  $80 ONNX:ENC-B
   ONNX:ENC$ 0 ONNX:PB-VARINT@ 2drop ;

: TRY-OVERFLOW ( -- )                          \ 10th byte carries bits above bit 63
   ONNX:ENC-RESET  9 0 ?do $FF ONNX:ENC-B loop  $7F ONNX:ENC-B
   ONNX:ENC$ 0 ONNX:PB-VARINT@ 2drop ;

: TRY-LONGV ( -- )                             \ an 11-byte varint (10 continuations)
   ONNX:ENC-RESET  10 0 ?do $80 ONNX:ENC-B loop  0 ONNX:ENC-B
   ONNX:ENC$ 0 ONNX:PB-VARINT@ 2drop ;

: TRY-F0 ( -- )                                \ field number 0 is illegal
   ONNX:ENC-RESET  0 ONNX:WT-VARINT ONNX:ENC-TAG
   ONNX:ENC$ 0 ONNX:PB-TAG@ drop 2drop ;

: TRY-LENOVER ( -- )                           \ declared length overruns the buffer
   ONNX:ENC-RESET  9 ONNX:ENC-VARINT
   ONNX:ENC$ 0 ONNX:PB-LEN@ drop 2drop ;

: TRY-GROUP ( -- )                             \ deprecated group wire cannot be skipped
   ONNX:ENC-RESET  1 3 ONNX:ENC-TAG
   ONNX:ENC$ {: a:ptr u:n :}
   a u 0 ONNX:PB-TAG@ {: f:n w:n p:n :}
   a u p w ONNX:PB-SKIP drop ;

: TRY-SKIP8 ( -- )                             \ fixed64 skip with no payload bytes
   ONNX:ENC-RESET  1 ONNX:WT-I64 ONNX:ENC-TAG
   ONNX:ENC$ {: a:ptr u:n :}
   a u 0 ONNX:PB-TAG@ {: f:n w:n p:n :}
   a u p w ONNX:PB-SKIP drop ;

: TRY-I32T ( -- )                              \ fixed32 read with only 2 bytes left
   ONNX:ENC-RESET  0 ONNX:ENC-B 0 ONNX:ENC-B
   ONNX:ENC$ 0 ONNX:PB-I32@ 2drop ;

\ ---- multi-field skip walk: every skippable wire type in one message ---------
: BUILD-MIX ( -- )                             \ varint + fixed32 + bytes + fixed64 fields
   ONNX:ENC-RESET
   300 1 ONNX:ENC-INT
   1.5 2 ONNX:ENC-F32A
   s" ab" 3 ONNX:ENC-STR
   4 ONNX:WT-I64 ONNX:ENC-TAG  8 0 ?do 0 ONNX:ENC-B loop ;

: SKIP-WALK ( -- n )                           \ tag+skip every field; the final position
   ONNX:ENC$ {: a:ptr u:n :}
   0 begin dup u < while
      a u rot ONNX:PB-TAG@ {: f:n w:n p:n :}
      a u p w ONNX:PB-SKIP
   repeat ;

T-RESET

\ ---- varint edges (encoder proven byte-exact, decoder round-trips) -----------
ONNX:ENC-RESET 0 ONNX:ENC-VARINT
ONNX:ENC$ nip 1 T=
ONNX:ENC$ 0 ONNX:PB-VARINT@ 1 T= 0 T=

ONNX:ENC-RESET 127 ONNX:ENC-VARINT
ONNX:ENC$ nip 1 T=
ONNX:ENC$ 0 ONNX:PB-VARINT@ 1 T= 127 T=

ONNX:ENC-RESET 300 ONNX:ENC-VARINT             \ spec example: 300 = AC 02
ONNX:ENC$ nip 2 T=
ONNX:ENC$ drop c@ $AC T=
ONNX:ENC$ drop 1+ c@ $02 T=
ONNX:ENC$ 0 ONNX:PB-VARINT@ 2 T= 300 T=

ONNX:ENC-RESET -1 ONNX:ENC-VARINT              \ max u64 / int64 -1: ten bytes
ONNX:ENC$ nip 10 T=
ONNX:ENC$ drop c@ $FF T=
ONNX:ENC$ drop 9 + c@ $01 T=
ONNX:ENC$ 0 ONNX:PB-VARINT@ 10 T= -1 T=

\ ---- tag decode ---------------------------------------------------------------
ONNX:ENC-RESET 7 ONNX:WT-LEN ONNX:ENC-TAG      \ (7 << 3) | 2 = 3A
ONNX:ENC$ drop c@ $3A T=
ONNX:ENC$ 0 ONNX:PB-TAG@ 1 T= ONNX:WT-LEN T= 7 T=

ONNX:ENC-RESET 1000 ONNX:WT-VARINT ONNX:ENC-TAG   \ multibyte tag varint
ONNX:ENC$ 0 ONNX:PB-TAG@ 2 T= ONNX:WT-VARINT T= 1000 T=

\ ---- length-delimited slice -----------------------------------------------------
ONNX:ENC-RESET s" hello" 3 ONNX:ENC-STR
ONNX:ENC$ 0 ONNX:PB-TAG@ 1 T= ONNX:WT-LEN T= 3 T=
ONNX:ENC$ 1 ONNX:PB-LEN@ 7 T= 5 T= 2 T=        \ off=2 len=5 pos'=7
ONNX:ENC$ drop 2 + 5 s" hello" STR= TTRUE

\ ---- fixed32 --------------------------------------------------------------------
ONNX:ENC-RESET 1.0 ONNX:ENC-F32
ONNX:ENC$ nip 4 T=
ONNX:ENC$ 0 ONNX:PB-I32@ 4 T= $3F800000 T=     \ 1.0f bit pattern, LE assembled

\ ---- unknown-field skip: walk a mixed message entirely by PB-SKIP ----------------
BUILD-MIX
SKIP-WALK ONNX:ENC$ nip T=

\ ---- fail closed ------------------------------------------------------------------
' TRY-TRUNCV   E-PB-TRUNC  TTHROWS
' TRY-OVERFLOW E-PB-VARINT TTHROWS
' TRY-LONGV    E-PB-VARINT TTHROWS
' TRY-F0       E-PB-FIELD  TTHROWS
' TRY-LENOVER  E-PB-TRUNC  TTHROWS
' TRY-GROUP    E-PB-WIRE   TTHROWS
' TRY-SKIP8    E-PB-TRUNC  TTHROWS
' TRY-I32T     E-PB-TRUNC  TTHROWS

T-REPORT

end-package
