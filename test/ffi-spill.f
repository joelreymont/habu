\ ffi-spill.f - FUNCTION: inputs past the AAPCS64 registers, on the stack.
\ Loaded by src/lib/ffi-test.f on macOS and Linux arm64, inside package
\ FFI-TEST. No system function takes nine longs and a double, so the run
\ builds its callees with the system compiler. Each weighs its inputs by
\ position, so an input read from the wrong register or stack cell fails its
\ case. A declaration has at most 16 inputs, so no call spills both kinds:
\ the last case spills longs between floats in registers.

require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require src/lib/test/c-library.f

$800 CODEGEN:BUFFER FFI-T-SP-SRC
create FFI-T-SP-SRC-PATH PATH-CAP allot
create FFI-T-SP-LIB-PATH PATH-CAP allot
variable FFI-T-SP-SRC-U
variable FFI-T-SP-LIB-U

: FFI-T-SP-L+ ( ptr u8 n -- )
   FFI-T-SP-SRC CODEGEN:APPEND-STRING
   10 FFI-T-SP-SRC CODEGEN:APPEND-BYTE ;

: FFI-T-SP-C ( -- )
   s" typedef long L; typedef double D;" FFI-T-SP-L+
   s" D hb_sp_l9d(L a, L b, L c, L d, L e, L f, L g, L h, L i, D j) {"
   FFI-T-SP-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j; }"
   FFI-T-SP-L+
   s" D hb_sp_d10(D a, D b, D c, D d, D e, D f, D g, D h, D i, D j) {"
   FFI-T-SP-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j; }"
   FFI-T-SP-L+
   s" L hb_sp_l9dl(L a, L b, L c, L d, L e, L f, L g, L h, L i, D j) {"
   FFI-T-SP-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + (L)(10*j); }"
   FFI-T-SP-L+
   s" D hb_sp_mix(L a, L b, L c, L d, L e, L f, L g, L h, D i, L j, L k," FFI-T-SP-L+
   s"    D l, D m, D n, D o, D p) {" FFI-T-SP-L+
   s"    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j" FFI-T-SP-L+
   s"       + 11*k + 12*l + 13*m + 14*n + 15*o + 16*p; }" FFI-T-SP-L+ ;

: FFI-T-SP-LIB$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libhbspill.dylib" exit then
   s" libhbspill.so" ;

: FFI-T-SP-SRC$ ( -- ptr u8 n ) FFI-T-SP-SRC-PATH FFI-T-SP-SRC-U @ ;
: FFI-T-SP-LIB-PATH$ ( -- ptr u8 n ) FFI-T-SP-LIB-PATH FFI-T-SP-LIB-U @ ;

: FFI-T-SP-BUILD ( -- )
   s" habu-ffi-spill" FS:MKTEMP-DIR {: dir:ptr diru :}
   dir diru FS:CLEANUP-TREE+
   dir diru s" hb-spill.c" FFI-T-SP-SRC-PATH FS:JOIN-PATH FFI-T-SP-SRC-U !
   dir diru FFI-T-SP-LIB$ FFI-T-SP-LIB-PATH FS:JOIN-PATH FFI-T-SP-LIB-U !
   FFI-T-SP-SRC CODEGEN:RESET
   FFI-T-SP-C
   FFI-T-SP-SRC$ FFI-T-SP-SRC CODEGEN:CONTENTS FS:WRITE-ALL
   FFI-T-SP-SRC$ FFI-T-SP-LIB-PATH$ C-LIBRARY:BUILD
   FFI-T-SP-LIB-PATH$ FFI:SELECT-LIBRARY ;
FFI-T-SP-BUILD

FUNCTION: FFI-T-SP-L9D hb_sp_l9d ( n n n n n n n n n r -- r ) ;FUNCTION
FUNCTION: FFI-T-SP-D10 hb_sp_d10 ( r r r r r r r r r r -- r ) ;FUNCTION
FUNCTION: FFI-T-SP-L9DL hb_sp_l9dl ( n n n n n n n n n r -- n ) ;FUNCTION
FUNCTION: FFI-T-SP-MIX hb_sp_mix
   ( n n n n n n n n r n n r r r r r -- r )
;FUNCTION

\ The input at position p, from 1: a long or a double, distinct and nonzero
\ at every position, so that a sum weighed by position is exact.
: FFI-T-SP-N ( n -- n ) {: p:n :} p p * p * p 9 * - 1+ ;
: FFI-T-SP-R ( n -- r ) s>f 0.75 f* 0.5 f+ ;

\ The inputs at positions lo through hi, weighed by position.
: FFI-T-SP-NS ( n n -- n ) {: lo:n hi:n :}
   0 hi 1+ lo ?do i dup FFI-T-SP-N * + loop ;
: FFI-T-SP-RS ( n n -- r ) {: lo:n hi:n :}
   0.0 hi 1+ lo ?do i s>f i FFI-T-SP-R f* f+ loop ;

: FFI-T-SP-LONGS ( -- )
   s" nine longs and a double: the ninth long on the stack" T-LABEL
   1 FFI-T-SP-N 2 FFI-T-SP-N 3 FFI-T-SP-N 4 FFI-T-SP-N 5 FFI-T-SP-N
   6 FFI-T-SP-N 7 FFI-T-SP-N 8 FFI-T-SP-N 9 FFI-T-SP-N
   10 FFI-T-SP-R FFI-T-SP-L9D
   1 9 FFI-T-SP-NS s>f 10 10 FFI-T-SP-RS f+ f= T-ASSERT

   s" the same inputs with a long result" T-LABEL
   1 FFI-T-SP-N 2 FFI-T-SP-N 3 FFI-T-SP-N 4 FFI-T-SP-N 5 FFI-T-SP-N
   6 FFI-T-SP-N 7 FFI-T-SP-N 8 FFI-T-SP-N 9 FFI-T-SP-N
   10 FFI-T-SP-R FFI-T-SP-L9DL
   1 9 FFI-T-SP-NS 10 10 FFI-T-SP-RS f>s + T= ;

: FFI-T-SP-DOUBLES ( -- )
   s" ten doubles: the ninth and tenth on the stack" T-LABEL
   1 FFI-T-SP-R 2 FFI-T-SP-R 3 FFI-T-SP-R 4 FFI-T-SP-R 5 FFI-T-SP-R
   6 FFI-T-SP-R 7 FFI-T-SP-R 8 FFI-T-SP-R 9 FFI-T-SP-R 10 FFI-T-SP-R
   FFI-T-SP-D10
   1 10 FFI-T-SP-RS f= T-ASSERT ;

: FFI-T-SP-MIXED ( -- )
   s" two longs on the stack between doubles in registers" T-LABEL
   1 FFI-T-SP-N 2 FFI-T-SP-N 3 FFI-T-SP-N 4 FFI-T-SP-N 5 FFI-T-SP-N
   6 FFI-T-SP-N 7 FFI-T-SP-N 8 FFI-T-SP-N 9 FFI-T-SP-R
   10 FFI-T-SP-N 11 FFI-T-SP-N 12 FFI-T-SP-R 13 FFI-T-SP-R
   14 FFI-T-SP-R 15 FFI-T-SP-R 16 FFI-T-SP-R FFI-T-SP-MIX
   1 8 FFI-T-SP-NS 10 11 FFI-T-SP-NS + s>f
   9 9 FFI-T-SP-RS f+ 12 16 FFI-T-SP-RS f+ f= T-ASSERT ;

FFI-T-SP-LONGS
FFI-T-SP-DOUBLES
FFI-T-SP-MIXED
