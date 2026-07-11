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
: FFI-T-OPEN ( -- n )  FFI-T-LIB-PATH RTLD-NOW DLOPEN ;
: FFI-T-SYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-LIB @ name DLSYM ;
: FFI-T-SYM$ ( ptr u8 n -- n ) {: name:ptr nameu:n :}
   name nameu FFI-T-SYM-BUF >CSTR
   FFI-T-LIB @ FFI-T-SYM-BUF DLSYM ;
: FFI-T-OPEN-MATH ( -- n )  FFI-T-MATH-PATH RTLD-NOW DLOPEN ;
: FFI-T-MSYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-MATH @ name DLSYM ;

deftype ffi-dev
deftype ffi-ctx

FFI: FFI-T-STRLEN$ ( ptr u8 -- n ) FFI-T-SYM$ strlen FFI;
FFI: FFI-T-STRNCMP$ ( ptr u8 ptr u8 n -- n ) FFI-T-SYM$ strncmp FFI;
FFI: FFI-T-GETPID$ ( -- n ) FFI-T-SYM$ getpid FFI;
FFI: FFI-T-CTX-SET ( ffi-ctx -- rc ) FFI-T-SYM$ getpid FFI;
FFI: FFI-T-VOID$ ( -- ) FFI-T-SYM$ getpid FFI;   \ void return kind: result dropped

: FFI-T-CHECK-PASSES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: FFI-T-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Leaf stub at cp@: x0 = x0+..+x7 + [sp+0] + [sp+8]; ret. Exercises ffi-call-n's
\ register args + the 16-byte-aligned stack spill. Built inside a word so cp@ is
\ the stable free code slot (a top-level cp@ patch would clobber the line buffer).
: FFI-T-SUM10 ( -- n ) cp@ {: fn:n :}
   $8B010000 fn       FFI-PATCH  $8B020000 fn $4 +  FFI-PATCH  $8B030000 fn $8 +  FFI-PATCH
   $8B040000 fn $C +  FFI-PATCH  $8B050000 fn $10 + FFI-PATCH  $8B060000 fn $14 + FFI-PATCH
   $8B070000 fn $18 + FFI-PATCH
   $F94003E9 fn $1C + FFI-PATCH  $8B090000 fn $20 + FFI-PATCH
   $F94007E9 fn $24 + FFI-PATCH  $8B090000 fn $28 + FFI-PATCH
   $D65F03C0 fn $2C + FFI-PATCH  fn ;

: FFI-T-FSUM3 ( -- n ) cp@ {: fn:n :}
   $1E612800 fn      FFI-PATCH  $1E622800 fn $4 + FFI-PATCH
   $D65F03C0 fn $8 + FFI-PATCH  fn ;

: FFI-T-FADD-X0 ( -- n ) cp@ {: fn:n :}
   $9E620008 fn       FFI-PATCH  $1E682800 fn $4 + FFI-PATCH
   $D65F03C0 fn $8 +  FFI-PATCH  fn ;

: FFI-T-FADD-FSTACK ( -- n ) cp@ {: fn:n :}
   $F94003E9 fn       FFI-PATCH  $9E670128 fn $4 + FFI-PATCH
   $1E682800 fn $8 +  FFI-PATCH  $D65F03C0 fn $C + FFI-PATCH  fn ;

: FFI-T-X8-STORE ( -- n ) cp@ {: fn:n :}
   $F9000100 fn FFI-PATCH  $D65F03C0 fn $4 + FFI-PATCH  fn ;

: FFI-RUN ( -- )
   T-RESET
   FFI-T-OPEN dup FFI-T-LIB !
   0 T<>                                          \ dlopen handle is non-null
   FFI-T-OPEN-MATH dup FFI-T-MATH !
   0 T<>

   FFI-T-STRLEN FFI-T-SYM 0 T<>                   \ dlsym resolved strlen
   FFI-T-SQRT FFI-T-MSYM 0 T<>
   FFI-T-HELLO P>N  FFI-T-STRLEN FFI-T-SYM CALL1   5 T=   \ strlen("hello") == 5
   FFI-T-HELLO FFI-T-STRLEN$ 5 T=                 \ generated typed wrapper

   FFI-T-HELLO P>N FFI-T-HELP P>N 3  FFI-T-STRNCMP FFI-T-SYM CALL3  0 T=  \ strncmp("hello","help",3)==0
   FFI-T-HELLO P>N FFI-T-HELP P>N 4  FFI-T-STRNCMP FFI-T-SYM CALL3  0 T<> \ first 4 differ ('l' vs 'p')
   FFI-T-HELLO FFI-T-HELP 3 FFI-T-STRNCMP$ 0 T=
   FFI-T-HELLO FFI-T-HELP 4 FFI-T-STRNCMP$ 0 T<>

   FFI-T-GETPID FFI-T-SYM CALL0  0 T<>            \ getpid() > 0 (non-zero)
   FFI-T-GETPID$ 0 T<>
   FFI-T-VOID$  FFI-T-GETPID$ 0 T<>               \ void-return wrapper is stack-neutral

   FFI-T-CSTR-SRC 5 FFI-T-CSTR-DST >CSTR          \ build "world\0" then strlen==5
   FFI-T-CSTR-DST P>N  FFI-T-STRLEN FFI-T-SYM CALL1  5 T=
   FFI-T-CSTR-DST FFI-T-STRLEN$ 5 T=

   s" FFI-T-ROLE-GOOD ( ffi-ctx -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-PASSES
   s" FFI-T-ROLE-BAD ( ffi-dev -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-REJECTS

   \ FFI-CALLN with INTERLEAVED resolve: fill the arg, THEN resolve via DLSYM.
   \ DLSYM uses its own FFI-DLBUF, so slot 0 survives -> strlen("hello")==5.
   FFI-T-HELLO P>N 0 FFI-ARG!
   1 FFI-T-STRLEN FFI-T-SYM FFI-CALLN  5 T=
   \ 10-arg call: x0..x7 + 2 stack-spilled args, sum 1..10 == 55
   1 0 FFI-ARG!  2 1 FFI-ARG!  3 2 FFI-ARG!  4 3 FFI-ARG!  5 4 FFI-ARG!
   6 5 FFI-ARG!  7 6 FFI-ARG!  8 7 FFI-ARG!  9 8 FFI-ARG!  10 9 FFI-ARG!
   10 FFI-T-SUM10 FFI-CALLN  55 T=

   1.25 0 FFI-FARG!  2.5 1 FFI-FARG!  3.0 2 FFI-FARG!
   0 FFI-T-FSUM3 FFI-CALLABI-R  6.75 f= T-ASSERT

   4 0 FFI-ARG!  1.5 0 FFI-FARG!
   0 FFI-T-FADD-X0 FFI-CALLABI-R  5.5 f= T-ASSERT

   1.25 0 FFI-FARG!  2.75 0 FFI-FSTACK!
   1 FFI-T-FADD-FSTACK FFI-CALLABI-R  4.0 f= T-ASSERT

   42 0 FFI-ARG!  FFI-T-X8-OUT P>N FFI-X8!
   0 FFI-T-X8-STORE FFI-CALLABI drop
   FFI-T-X8-OUT @ 42 T=
   0 FFI-X8!

   9.0 0 FFI-FARG!
   0 FFI-T-SQRT FFI-T-MSYM FFI-CALLABI-R  3.0 f= T-ASSERT ;

FFI-RUN

\ Kind-surface checked regressions (wave C: FDEF kinds are a PRIVATE ENUM).
\ Inside the reopened package the family resolves: the positive baseline
\ proves the rejects are type errors, and a raw n can no longer pose as a
\ kind, a kind cannot launder back to n, and kinds do not compare with `=`.
package FFI
s" FFI-T-KPOS ( kind n -- ) FDEF-TAG!" FFI-T-CHECK-PASSES
s" FFI-T-KNEG1 ( n n -- ) FDEF-TAG!" FFI-T-CHECK-REJECTS
s" FFI-T-KNEG2 ( n -- n ) FDEF-TAG@" FFI-T-CHECK-REJECTS
s" FFI-T-KNEG3 ( kind kind -- bool ) =" FFI-T-CHECK-REJECTS
end-package
\ Outside the package the private family never resolves, bare or qualified.
s" FFI-T-KNEG4 ( kind -- ) drop" FFI-T-CHECK-REJECTS
s" FFI-T-KNEG5 ( ffi:kind -- ) drop" FFI-T-CHECK-REJECTS

T-REPORT
