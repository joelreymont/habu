\ ffi-test.f - checked C-ABI FFI tests against libc.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/ffi.f lib/ffi-test.f | bin/hb

create FFI-T-LIBC   108 c, 105 c, 98 c, 99 c, 46 c, 115 c, 111 c, 46 c, 54 c, 0 c, \ "libc.so.6"
create FFI-T-STRLEN 115 c, 116 c, 114 c, 108 c, 101 c, 110 c, 0 c,                 \ "strlen"
create FFI-T-STRNCMP 115 c, 116 c, 114 c, 110 c, 99 c, 109 c, 112 c, 0 c,          \ "strncmp"
create FFI-T-GETPID 103 c, 101 c, 116 c, 112 c, 105 c, 100 c, 0 c,                 \ "getpid"
create FFI-T-HELLO  104 c, 101 c, 108 c, 108 c, 111 c, 0 c,                        \ "hello"
create FFI-T-HELP   104 c, 101 c, 108 c, 112 c, 0 c,                               \ "help"
create FFI-T-CSTR-SRC 119 c, 111 c, 114 c, 108 c, 100 c,                           \ "world" (no NUL)
create FFI-T-CSTR-DST 8 allot

variable FFI-T-LIB

: FFI-T-OPEN ( -- n )  FFI-T-LIBC RTLD-NOW DLOPEN ;
: FFI-T-SYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-LIB @ name DLSYM ;

\ Leaf stub at cp@: x0 = x0+..+x7 + [sp+0] + [sp+8]; ret. Exercises ffi-call-n's
\ register args + the 16-byte-aligned stack spill. Built inside a word so cp@ is
\ the stable free code slot (a top-level cp@ patch would clobber the line buffer).
: FFI-T-SUM10 ( -- n ) cp@ {: fn:n :}
   $8B010000 fn       patch32  $8B020000 fn $4 +  patch32  $8B030000 fn $8 +  patch32
   $8B040000 fn $C +  patch32  $8B050000 fn $10 + patch32  $8B060000 fn $14 + patch32
   $8B070000 fn $18 + patch32
   $F94003E9 fn $1C + patch32  $8B090000 fn $20 + patch32
   $F94007E9 fn $24 + patch32  $8B090000 fn $28 + patch32
   $D65F03C0 fn $2C + patch32  fn ;

: FFI-RUN ( -- )
   T-RESET
   FFI-T-OPEN dup FFI-T-LIB !
   0 T<>                                          \ dlopen handle is non-null

   FFI-T-STRLEN FFI-T-SYM 0 T<>                   \ dlsym resolved strlen
   FFI-T-HELLO P>N  FFI-T-STRLEN FFI-T-SYM CALL1   5 T=   \ strlen("hello") == 5

   FFI-T-HELLO P>N FFI-T-HELP P>N 3  FFI-T-STRNCMP FFI-T-SYM CALL3  0 T=  \ strncmp("hello","help",3)==0
   FFI-T-HELLO P>N FFI-T-HELP P>N 4  FFI-T-STRNCMP FFI-T-SYM CALL3  0 T<> \ first 4 differ ('l' vs 'p')

   FFI-T-GETPID FFI-T-SYM CALL0  0 T<>            \ getpid() > 0 (non-zero)

   FFI-T-CSTR-SRC 5 FFI-T-CSTR-DST >CSTR          \ build "world\0" then strlen==5
   FFI-T-CSTR-DST P>N  FFI-T-STRLEN FFI-T-SYM CALL1  5 T=

   \ FFI-CALLN with INTERLEAVED resolve: fill the arg, THEN resolve via DLSYM.
   \ DLSYM uses its own FFI-DLBUF, so slot 0 survives -> strlen("hello")==5.
   FFI-T-HELLO P>N 0 FFI-ARG!
   1 FFI-T-STRLEN FFI-T-SYM FFI-CALLN  5 T=
   \ 10-arg call: x0..x7 + 2 stack-spilled args, sum 1..10 == 55
   1 0 FFI-ARG!  2 1 FFI-ARG!  3 2 FFI-ARG!  4 3 FFI-ARG!  5 4 FFI-ARG!
   6 5 FFI-ARG!  7 6 FFI-ARG!  8 7 FFI-ARG!  9 8 FFI-ARG!  10 9 FFI-ARG!
   10 FFI-T-SUM10 FFI-CALLN  55 T= ;

FFI-RUN
T-REPORT
