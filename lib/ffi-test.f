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
   FFI-T-CSTR-DST P>N  FFI-T-STRLEN FFI-T-SYM CALL1  5 T= ;

FFI-RUN
T-REPORT
