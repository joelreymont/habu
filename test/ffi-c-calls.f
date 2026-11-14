\ ffi-c-calls.f - FUNCTION: calls against C functions the run builds.
\ Loaded by src/lib/ffi-test.f on macOS and Linux arm64, inside package
\ FFI-TEST. No system function takes nine longs and a double, or returns a
\ narrow integer with the register's high bits set, so the run builds its
\ callees with the system compiler. A spilling callee weighs its inputs by
\ position, so an input read from the wrong register or stack cell fails its
\ case. A declaration has at most 16 inputs, so no call spills both kinds:
\ a mixed case spills longs between floats in registers. On Linux a narrow
\ callee returns its long input whole; Apple's ABI has the callee extend it.

require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require src/lib/test/c-library.f

$800 CODEGEN:BUFFER FFI-T-CC-SRC
create FFI-T-CC-SRC-PATH PATH-CAP allot
create FFI-T-CC-LIB-PATH PATH-CAP allot
variable FFI-T-CC-SRC-U
variable FFI-T-CC-LIB-U

: FFI-T-CC-L+ ( ptr u8 n -- )
   FFI-T-CC-SRC CODEGEN:APPEND-STRING
   10 FFI-T-CC-SRC CODEGEN:APPEND-BYTE ;

: FFI-T-CC-C ( -- )
   s" typedef long L; typedef double D;" FFI-T-CC-L+
   s" D hb_cc_l9d(L a, L b, L c, L d, L e, L f, L g, L h, L i, D j) {"
   FFI-T-CC-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j; }"
   FFI-T-CC-L+
   s" D hb_cc_d10(D a, D b, D c, D d, D e, D f, D g, D h, D i, D j) {"
   FFI-T-CC-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j; }"
   FFI-T-CC-L+
   s" L hb_cc_l9dl(L a, L b, L c, L d, L e, L f, L g, L h, L i, D j) {"
   FFI-T-CC-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + (L)(10*j); }"
   FFI-T-CC-L+
   s" D hb_cc_mix(L a, L b, L c, L d, L e, L f, L g, L h, D i, L j, L k," FFI-T-CC-L+
   s"    D l, D m, D n, D o, D p) {" FFI-T-CC-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j" FFI-T-CC-L+
   s"       + 11*k + 12*l + 13*m + 14*n + 15*o + 16*p; }" FFI-T-CC-L+
   s" unsigned char hb_cc_u8(L x) { return x; }" FFI-T-CC-L+
   s" unsigned short hb_cc_u16(L x) { return x; }" FFI-T-CC-L+
   s" char hb_cc_char(L x) { return x; }" FFI-T-CC-L+ ;

: FFI-T-CC-LIB$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libhbccalls.dylib" exit then
   s" libhbccalls.so" ;

: FFI-T-CC-SRC$ ( -- ptr u8 n ) FFI-T-CC-SRC-PATH FFI-T-CC-SRC-U @ ;
: FFI-T-CC-LIB-PATH$ ( -- ptr u8 n ) FFI-T-CC-LIB-PATH FFI-T-CC-LIB-U @ ;

: FFI-T-CC-BUILD ( -- )
   s" habu-ffi-c-calls" FS:MKTEMP-DIR {: dir:ptr diru :}
   dir diru FS:CLEANUP-TREE+
   dir diru s" hb-c-calls.c" FFI-T-CC-SRC-PATH FS:JOIN-PATH FFI-T-CC-SRC-U !
   dir diru FFI-T-CC-LIB$ FFI-T-CC-LIB-PATH FS:JOIN-PATH FFI-T-CC-LIB-U !
   FFI-T-CC-SRC CODEGEN:RESET
   FFI-T-CC-C
   FFI-T-CC-SRC$ FFI-T-CC-SRC CODEGEN:CONTENTS FS:WRITE-ALL
   FFI-T-CC-SRC$ FFI-T-CC-LIB-PATH$ C-LIBRARY:BUILD
   FFI-T-CC-LIB-PATH$ FFI:SELECT-LIBRARY ;
FFI-T-CC-BUILD

FUNCTION: FFI-T-CC-L9D hb_cc_l9d ( n n n n n n n n n r -- r ) ;FUNCTION
FUNCTION: FFI-T-CC-D10 hb_cc_d10 ( r r r r r r r r r r -- r ) ;FUNCTION
FUNCTION: FFI-T-CC-L9DL hb_cc_l9dl ( n n n n n n n n n r -- n ) ;FUNCTION
FUNCTION: FFI-T-CC-MIX hb_cc_mix
   ( n n n n n n n n r n n r r r r r -- r )
;FUNCTION
FUNCTION: FFI-T-CC-U8 hb_cc_u8 ( n -- u8 ) ;FUNCTION
FUNCTION: FFI-T-CC-U16 hb_cc_u16 ( n -- u16 ) ;FUNCTION
FUNCTION: FFI-T-CC-CHAR hb_cc_char ( n -- char ) ;FUNCTION

\ The input at position p, from 1: a long or a double, distinct and nonzero
\ at every position, so that a sum weighed by position is exact.
: FFI-T-CC-N ( n -- n ) {: p:n :} p p * p * p 9 * - 1+ ;
: FFI-T-CC-R ( n -- r ) s>f 0.75 f* 0.5 f+ ;

\ The inputs at positions lo through hi, weighed by position.
: FFI-T-CC-NS ( n n -- n ) {: lo:n hi:n :}
   0 hi 1+ lo ?do i dup FFI-T-CC-N * + loop ;
: FFI-T-CC-RS ( n n -- r ) {: lo:n hi:n :}
   0.0 hi 1+ lo ?do i s>f i FFI-T-CC-R f* f+ loop ;

: FFI-T-CC-LONGS ( -- )
   s" nine longs and a double: the ninth long on the stack" T-LABEL
   1 FFI-T-CC-N 2 FFI-T-CC-N 3 FFI-T-CC-N 4 FFI-T-CC-N 5 FFI-T-CC-N
   6 FFI-T-CC-N 7 FFI-T-CC-N 8 FFI-T-CC-N 9 FFI-T-CC-N
   10 FFI-T-CC-R FFI-T-CC-L9D
   1 9 FFI-T-CC-NS s>f 10 10 FFI-T-CC-RS f+ f= T-ASSERT

   s" the same inputs with a long result" T-LABEL
   1 FFI-T-CC-N 2 FFI-T-CC-N 3 FFI-T-CC-N 4 FFI-T-CC-N 5 FFI-T-CC-N
   6 FFI-T-CC-N 7 FFI-T-CC-N 8 FFI-T-CC-N 9 FFI-T-CC-N
   10 FFI-T-CC-R FFI-T-CC-L9DL
   1 9 FFI-T-CC-NS 10 10 FFI-T-CC-RS f>s + T= ;

: FFI-T-CC-DOUBLES ( -- )
   s" ten doubles: the ninth and tenth on the stack" T-LABEL
   1 FFI-T-CC-R 2 FFI-T-CC-R 3 FFI-T-CC-R 4 FFI-T-CC-R 5 FFI-T-CC-R
   6 FFI-T-CC-R 7 FFI-T-CC-R 8 FFI-T-CC-R 9 FFI-T-CC-R 10 FFI-T-CC-R
   FFI-T-CC-D10
   1 10 FFI-T-CC-RS f= T-ASSERT ;

: FFI-T-CC-MIXED ( -- )
   s" two longs on the stack between doubles in registers" T-LABEL
   1 FFI-T-CC-N 2 FFI-T-CC-N 3 FFI-T-CC-N 4 FFI-T-CC-N 5 FFI-T-CC-N
   6 FFI-T-CC-N 7 FFI-T-CC-N 8 FFI-T-CC-N 9 FFI-T-CC-R
   10 FFI-T-CC-N 11 FFI-T-CC-N 12 FFI-T-CC-R 13 FFI-T-CC-R
   14 FFI-T-CC-R 15 FFI-T-CC-R 16 FFI-T-CC-R FFI-T-CC-MIX
   1 8 FFI-T-CC-NS 10 11 FFI-T-CC-NS + s>f
   9 9 FFI-T-CC-RS f+ 12 16 FFI-T-CC-RS f+ f= T-ASSERT ;

: FFI-T-CC-NARROW ( -- )
   s" a u8 result reads its 8 bits" T-LABEL
   $1FF FFI-T-CC-U8 $FF T=
   s" a u16 result reads its 16 bits" T-LABEL
   $1FFFF FFI-T-CC-U16 $FFFF T=
   s" a char result reads its 8 bits" T-LABEL
   $1FF FFI-T-CC-CHAR $FF T= ;

FFI-T-CC-LONGS
FFI-T-CC-DOUBLES
FFI-T-CC-MIXED
FFI-T-CC-NARROW
