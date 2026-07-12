\ ffi-test.f - checked C-ABI FFI tests against libc.
\ Run: bin/hb --load lib/ffi-test.f

require lib/test.f
require lib/ffi.f
require test/checker-assert.f

create FFI-T-LIBC   108 c, 105 c, 98 c, 99 c, 46 c, 115 c, 111 c, 46 c, 54 c, 0 c, \ "libc.so.6"
create FFI-T-LIBM   108 c, 105 c, 98 c, 109 c, 46 c, 115 c, 111 c, 46 c, 54 c, 0 c, \ "libm.so.6"
create FFI-T-LIBSYSTEM
   47 c, 117 c, 115 c, 114 c, 47 c, 108 c, 105 c, 98 c, 47 c,
   108 c, 105 c, 98 c, 83 c, 121 c, 115 c, 116 c, 101 c, 109 c,
   46 c, 66 c, 46 c, 100 c, 121 c, 108 c, 105 c, 98 c, 0 c,            \ "/usr/lib/libSystem.B.dylib"
create FFI-T-STRLEN 115 c, 116 c, 114 c, 108 c, 101 c, 110 c, 0 c,                 \ "strlen"
create FFI-T-STRNCMP 115 c, 116 c, 114 c, 110 c, 99 c, 109 c, 112 c, 0 c,          \ "strncmp"
create FFI-T-GETPID 103 c, 101 c, 116 c, 112 c, 105 c, 100 c, 0 c,                 \ "getpid"
create FFI-T-SQRT   115 c, 113 c, 114 c, 116 c, 0 c,                               \ "sqrt"
create FFI-T-HELLO  104 c, 101 c, 108 c, 108 c, 111 c, 0 c,                        \ "hello"
create FFI-T-HELP   104 c, 101 c, 108 c, 112 c, 0 c,                               \ "help"
create FFI-T-CSTR-SRC 119 c, 111 c, 114 c, 108 c, 100 c,                           \ "world" (no NUL)
create FFI-T-CSTR-DST 8 allot
create FFI-T-X8-OUT 1 cells allot
create FFI-T-SYM-BUF 64 allot

variable FFI-T-LIB
variable FFI-T-MATH

: FFI-T-LIB-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBC ;
: FFI-T-MATH-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBM ;
: FFI-T-OPEN ( -- n )  FFI-T-LIB-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-SYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-LIB @ name FFI:DLSYM ;
: FFI-T-SYM$ ( ptr u8 n -- n ) {: name:ptr nameu:n :}
   name nameu FFI-T-SYM-BUF FFI:CSTR
   FFI-T-LIB @ FFI-T-SYM-BUF FFI:DLSYM ;
: FFI-T-OPEN-MATH ( -- n )  FFI-T-MATH-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-MSYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-MATH @ name FFI:DLSYM ;

deftype ffi-dev
deftype ffi-ctx

TRUSTED: FFI-T-STRLEN$ ( ptr u8 -- n ) {: str:ptr :}
   s" strlen" FFI-T-SYM$ {: fn:n :}
   FFI:RESET
   str 0 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: FFI-T-STRNCMP$ ( ptr u8 ptr u8 n -- n )
   {: a:ptr b:ptr len:n :}
   s" strncmp" FFI-T-SYM$ {: fn:n :}
   FFI:RESET
   a 0 FFI:READABLE!
   b 1 FFI:READABLE!
   len 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 fn ffi-call-bounded ;

TRUSTED: FFI-T-GETPID$ ( -- n )
   s" getpid" FFI-T-SYM$ {: fn:n :}
   FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 fn ffi-call-bounded ;

TRUSTED: FFI-T-CTX-SET ( ffi-ctx -- rc ) {: ctx:ffi-ctx :}
   s" getpid" FFI-T-SYM$ {: fn:n :}
   FFI:RESET
   ctx 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: FFI-T-VOID$ ( -- )
   s" getpid" FFI-T-SYM$ {: fn:n :}
   FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 fn ffi-call-bounded drop ;

: FFI-T-CHECK-PASSES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: FFI-T-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Leaf stub at cp@: x0 = x0+..+x7 + [sp+0] + [sp+8]; ret. Exercises ffi-call-n's
\ register args + the 16-byte-aligned stack spill. Built inside a word so cp@ is
\ the stable free code slot (a top-level cp@ patch would clobber the line buffer).
TRUSTED: FFI-T-SUM10 ( -- n ) cp@ {: fn:n :}
   $8B010000 fn       patch32  $8B020000 fn $4 +  patch32  $8B030000 fn $8 +  patch32
   $8B040000 fn $C +  patch32  $8B050000 fn $10 + patch32  $8B060000 fn $14 + patch32
   $8B070000 fn $18 + patch32
   $F94003E9 fn $1C + patch32  $8B090000 fn $20 + patch32
   $F94007E9 fn $24 + patch32  $8B090000 fn $28 + patch32
   $D65F03C0 fn $2C + patch32  fn ;

TRUSTED: FFI-T-FSUM3 ( -- n ) cp@ {: fn:n :}
   $1E612800 fn      patch32  $1E622800 fn $4 + patch32
   $D65F03C0 fn $8 + patch32  fn ;

TRUSTED: FFI-T-FADD-X0 ( -- n ) cp@ {: fn:n :}
   $9E620008 fn       patch32  $1E682800 fn $4 + patch32
   $D65F03C0 fn $8 +  patch32  fn ;

TRUSTED: FFI-T-FADD-FSTACK ( -- n ) cp@ {: fn:n :}
   $F94003E9 fn       patch32  $9E670128 fn $4 + patch32
   $1E682800 fn $8 +  patch32  $D65F03C0 fn $C + patch32  fn ;

TRUSTED: FFI-T-X8-STORE ( -- n ) cp@ {: fn:n :}
   $F9000100 fn patch32  $D65F03C0 fn $4 + patch32  fn ;

TRUSTED: FFI-T-STRLEN-LATE ( ptr u8 -- n ) {: str:ptr :}
   FFI:RESET
   str 0 FFI:READABLE!
   FFI-T-STRLEN FFI-T-SYM {: fn:n :}
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: FFI-T-SUM10-CALL ( -- n )
   FFI:RESET
   10 0 ?do i 1+ i FFI:VALUE! loop
   FFI:ARGS FFI:REG-LENS 10 FFI-T-SUM10 ffi-call-bounded ;

TRUSTED: FFI-T-FSUM3-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT!
   2.5 1 FFI:FLOAT!
   3.0 2 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-FSUM3 ffi-call-abi-r-bounded ;

TRUSTED: FFI-T-FADD-X0-CALL ( -- r )
   FFI:RESET
   4 0 FFI:VALUE!
   1.5 0 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-FADD-X0 ffi-call-abi-r-bounded ;

TRUSTED: FFI-T-FADD-FSTACK-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT!
   2.75 0 FFI:STACK-FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   1 FFI-T-FADD-FSTACK ffi-call-abi-r-bounded ;

TRUSTED: FFI-T-X8-ABI-CALL ( ptr a -- n ) {: out:ptr :}
   FFI:RESET
   42 0 FFI:VALUE!
   out 8 FFI:X8-WRITABLE!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-X8-STORE ffi-call-abi-bounded ;

TRUSTED: FFI-T-SQRT-CALL ( r -- r ) {: value:r :}
   FFI-T-SQRT FFI-T-MSYM {: fn:n :}
   FFI:RESET
   value 0 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 fn ffi-call-abi-r-bounded ;

: FFI-RUN ( -- )
   T-RESET
   FFI-T-OPEN dup FFI-T-LIB !
   0 T<>                                          \ dlopen handle is non-null
   FFI-T-OPEN-MATH dup FFI-T-MATH !
   0 T<>

   FFI-T-STRLEN FFI-T-SYM 0 T<>                   \ dlsym resolved strlen
   FFI-T-SQRT FFI-T-MSYM 0 T<>
   FFI-T-HELLO FFI-T-STRLEN$ 5 T=                 \ explicit typed wrapper

   FFI-T-HELLO FFI-T-HELP 3 FFI-T-STRNCMP$ 0 T=
   FFI-T-HELLO FFI-T-HELP 4 FFI-T-STRNCMP$ 0 T<>

   FFI-T-GETPID$ 0 T<>
   FFI-T-VOID$  FFI-T-GETPID$ 0 T<>               \ void-return wrapper is stack-neutral

   FFI-T-CSTR-SRC 5 FFI-T-CSTR-DST FFI:CSTR       \ build "world\0" then strlen==5
   FFI-T-CSTR-DST FFI-T-STRLEN$ 5 T=

   s" FFI-T-ROLE-GOOD ( ffi-ctx -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-PASSES
   s" FFI-T-ROLE-BAD ( ffi-dev -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-REJECTS

   \ FFI-CALLN with INTERLEAVED resolve: fill the arg, THEN resolve via DLSYM.
   \ DLSYM uses its own FFI-DLBUF, so slot 0 survives -> strlen("hello")==5.
   FFI-T-HELLO FFI-T-STRLEN-LATE 5 T=
   \ 10-arg call: x0..x7 + 2 stack-spilled args, sum 1..10 == 55
   FFI-T-SUM10-CALL 55 T=

   FFI-T-FSUM3-CALL 6.75 f= T-ASSERT
   FFI-T-FADD-X0-CALL 5.5 f= T-ASSERT
   FFI-T-FADD-FSTACK-CALL 4.0 f= T-ASSERT

   FFI-T-X8-OUT FFI-T-X8-ABI-CALL drop
   FFI-T-X8-OUT @ 42 T=

   9.0 FFI-T-SQRT-CALL 3.0 f= T-ASSERT

   s" FFI-T-RAW ( ptr a ptr a n n -- n ) ffi-call-bounded" FFI-T-CHECK-REJECTS
   s" FFI-T-RAW-ABI ( ptr a ptr a ptr a ptr a ptr a n n -- n ) ffi-call-abi-bounded" FFI-T-CHECK-REJECTS
   s" FFI-T-LIE ( n -- ) 8 0 FFI:WRITABLE!" FFI-T-CHECK-REJECTS
   s" FFI-T-MULTI ( ptr u8 n -- n n ) FFI:DLOPEN" FFI-T-CHECK-REJECTS
   s" FFI:" 0 search-wl 0= TTRUE
   s" CALL0" 0 search-wl 0= TTRUE ;

FFI-RUN

T-REPORT
