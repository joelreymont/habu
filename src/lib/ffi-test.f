split-str
\ ffi-test.f - checked C-ABI FFI tests against libc.
\ Run: bin/hb --load src/lib/ffi-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f         \ DEFTYPE - the ffi-dev / ffi-ctx test nominals
require src/lib/string.f               \ SB / CONTAINS? - the child's stderr is matched here
require src/lib/fmt.f                  \ the path cap a huge base's rejection prints
require src/lib/process.f
require src/lib/process-argv.f         \ the growth case needs a child engine
require src/lib/le.f                   \ the spilled %n out-parameter is a C int
require src/lib/memory.f               \ the read-only page the %n format is copied into
require src/lib/engine-candidate.f
require src/lib/test/ending.f
require src/lib/test/guard-page.f      \ a path whose bytes end where memory does
require src/lib/ffi-callback.f         \ a struct argument held across a callback
require src/lib/task.f
require test/ffi-struct-fixture.f      \ the struct cases' callee
require test/fixtures/ffi-str-unmarked.f \ a str input outside split-str
require test/e2e-tree.f                \ the struct clauses' load children
require tools/check-verify-core.f       \ LSP-CHECK:VERIFY-BYTES - the struct clauses' check
require test/decl-diag-capture.f

package FFI-TEST

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
8 SPAN-BUFFER: FFI-T-CSTR-DST
64 SPAN-BUFFER: FFI-T-SYM-BUF

variable FFI-T-LIB
variable FFI-T-MATH

: FFI-T-LIB-PATH ( -- ^u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBC ;
: FFI-T-MATH-PATH ( -- ^u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBM ;
: FFI-T-OPEN ( -- n )  FFI-T-LIB-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-SYM ( ^u8 -- n ) {: name:^ :}  FFI-T-LIB @ name FFI:DLSYM ;
: FFI-T-SYM$ ( str -- n )
   {: name:^ nameu:n :}
   name nameu FFI-T-SYM-BUF FFI:CSTR {: sym:^ :}
   FFI-T-LIB @ sym FFI:DLSYM ;
: FFI-T-OPEN-MATH ( -- n )  FFI-T-MATH-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-MSYM ( ^u8 -- n ) {: name:^ :}  FFI-T-MATH @ name FFI:DLSYM ;

DEFTYPE FFI-DEV
DEFTYPE FFI-CTX

PROCESS-SYMBOLS
FUNCTION: FFI-T-STRLEN$ strlen ( ^u8 -- n ) ;FUNCTION
FUNCTION: FFI-T-STRNCMP$ strncmp ( ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: FFI-T-GETPID$ getpid ( -- i32 ) ;FUNCTION
FUNCTION: FFI-T-CTX-CALL getpid ( n -- i32 ) ;FUNCTION

\ crc32 through the process: the engine does not link libz, so process-wide
\ resolution finds it exactly when a libz open made its symbols global.
FUNCTION: FFI-T-GLOBAL-PROBE crc32 ( n ^u8 n -- n ) ;FUNCTION

\ A declaration with no result drops the machine return cell, so the word is
\ stack-neutral for its caller.
FUNCTION: FFI-T-VOID$ getpid ( -- ) ;FUNCTION

\ gcvt(double, int, char *) returns its output buffer. This real libc call
\ exercises a pointer result beside floating, integer and pointer arguments.
64 BUFFER: FFI-T-GCVT-BUF
FUNCTION: FFI-T-GCVT gcvt ( r n ^u8 -- ^u8 ) ;FUNCTION

\ Resolution happens inside the call, after the arguments are staged: DLSYM
\ marshals through its own buffer, so a symbol resolved for the first time here
\ cannot overwrite the pending argument. Call this alias before FFI-T-STRLEN$ so
\ the shared descriptor has not yet resolved either.
FUNCTION: FFI-T-STRLEN-LATE strlen ( ^u8 -- n ) ;FUNCTION

FUNCTION: FFI-T-MEMCPY memcpy ( ^u8 ^u8 n -- n ) ;FUNCTION

\ A C int result fills only the low half of the return register, and the half
\ above it is whatever the callee left there. close(-1) fails with -1 in every
\ libc: `i32` must read it as -1, never $FFFFFFFF, and the same result read
\ through `u32` must be $FFFFFFFF, never -1.
FUNCTION: FFI-T-CLOSE close ( n -- i32 ) ;FUNCTION
FUNCTION: FFI-T-CLOSE-U32 close ( n -- u32 ) ;FUNCTION
FUNCTION: FFI-T-LABS-I64 labs ( i64 -- i64 ) ;FUNCTION
FUNCTION: FFI-T-LABS-CELL labs ( cell -- cell ) ;FUNCTION
FUNCTION: FFI-T-LABS-ADDR labs ( addr -- addr ) ;FUNCTION
FUNCTION: FFI-T-ATOI atoi ( ^u8 -- i32 ) ;FUNCTION

\ Nominal-input fixture proves ABI-cell identity does not erase a role: the
\ conversion is visible at this call site and the checker keeps ffi-ctx apart
\ from ffi-dev.
: FFI-T-CTX-SET ( ffi-ctx -- rc )
   FFI-CTX>N FFI-T-CTX-CALL >RC ;

\ The declarer's contract to the loader's closed evaluate: a declaration
\ defines words and leaves the stack exactly as it found it.
variable FFI-T-DEPTH-BEFORE
variable FFI-T-DEPTH-AFTER

: FFI-T-DEPTH! ( ^n -- ) depth swap ! ;

FFI-T-DEPTH-BEFORE FFI-T-DEPTH!
FUNCTION: FFI-T-DEPTH-PROBE getpid ( -- i32 ) ;FUNCTION
FFI-T-DEPTH-AFTER FFI-T-DEPTH!

\ Declarations the declarer rejects, each by one rule; evaluate-closed raises
\ the throw a file-level declaration would.
: FFI-T-BAD-PTR ( -- )
   s" FUNCTION: FFI-T-X1 getpid ( ^ -- n ) ;FUNCTION" evaluate-closed ;
: FFI-T-BAD-RESULTS ( -- )
   s" FUNCTION: FFI-T-X2 getpid ( -- n n ) ;FUNCTION" evaluate-closed ;
: FFI-T-BAD-IN-TYPE ( -- )
   s" FUNCTION: FFI-T-XI getpid ( fd -- n ) ;FUNCTION" evaluate-closed ;
: FFI-T-PTR-IN ( -- )
   s" FUNCTION: FFI-T-XP getpid ( ptr u8 -- n ) ;FUNCTION" evaluate-closed ;
: FFI-T-BAD-OUT-TYPE ( -- )
   s" FUNCTION: FFI-T-XO getpid ( -- bool ) ;FUNCTION" evaluate-closed ;

\ A version list with an empty piece or a piece that is not a number.
: FFI-T-LIST-END ( -- )
   s" VERSIONED-LIBRARY z 1," evaluate-closed ;
: FFI-T-LIST-START ( -- )
   s" VERSIONED-LIBRARY z ,1" evaluate-closed ;
: FFI-T-LIST-GAP ( -- )
   s" VERSIONED-LIBRARY z 1,,2" evaluate-closed ;
: FFI-T-LIST-WORD ( -- )
   s" VERSIONED-LIBRARY z 1,x" evaluate-closed ;
: FFI-T-BAD-F32-VARARG ( -- )
   s" FUNCTION: FFI-T-XP getpid ( n f32 -- f32 ) 1 VARIADIC ;FUNCTION" evaluate-closed ;
: FFI-T-BAD-VARARG ( -- )
   s" FUNCTION: FFI-T-XV getpid ( n -- n ) 2 VARIADIC ;FUNCTION" evaluate-closed ;
: FFI-T-NO-FIXED ( -- )
   s" FUNCTION: FFI-T-XZ getpid ( n -- n ) 0 VARIADIC ;FUNCTION" evaluate-closed ;

\ Promotion reaches only the variadic tail: a fixed f32 before it is admitted.
: FFI-T-F32-FIXED ( -- )
   s" FUNCTION: FFI-T-XQ getpid ( f32 n r -- f32 ) 2 VARIADIC ;FUNCTION" evaluate-closed ;

\ VARIADIC counts a str as one input, so the f32 after it stays fixed.
: FFI-T-STR-FIXED ( -- )
   s" FUNCTION: FFI-T-XS getpid ( str f32 r -- n ) 2 VARIADIC ;FUNCTION" evaluate-closed ;

\ Declaration-only shapes: Cairo's exact signature must load without requiring
\ Cairo, and Darwin's variadic stack routing must admit a pointer result. These
\ words never call their symbols; gcvt below witnesses the mixed pointer call
\ and FFI-T-VARARGS the variadic stack staging.
: FFI-T-PTR-MIXED ( -- )
   s" FUNCTION: FFI-T-XM cairo_pdf_surface_create ( ^u8 r r -- ^u8 ) ;FUNCTION"
   evaluate-closed ;
: FFI-T-PTR-VARARG ( -- )
   s" FUNCTION: FFI-T-XW habu-pointer-variadic ( ^u8 r n ^u8 r -- ^u8 ) 1 VARIADIC ;FUNCTION"
   evaluate-closed ;

\ Both cases declare in the public section, which the private PROCESS-SYMBOLS
\ above never selected for; inside FFI-TEST a `package` would open a part.
variable FFI-T-SCOPE-BEFORE
variable FFI-T-SCOPE-AFTER

: FFI-T-NO-LIBRARY ( -- )
   s" public FUNCTION: FFI-T-XC getpid ( -- n ) ;FUNCTION"
   evaluate-closed ;
: FFI-T-PRIVATE-AGAIN ( -- )
   s" private" evaluate-closed ;
\ The scoped selection above is the public section's, so this file's own scope
\ states its selection again before the fixtures that follow declare in it.
: FFI-T-RESELECT ( -- )
   s" private PROCESS-SYMBOLS" evaluate-closed ;
: FFI-T-SCOPED-LIBRARY ( -- )
   s" public PROCESS-SYMBOLS FUNCTION: FFI-T-XD getpid ( -- n ) ;FUNCTION private"
   evaluate-closed ;

\ A refused declaration is ABANDONED, not left half open. The refusal below
\ counts more inputs than the effect has; the good declaration after it must
\ still compile, and the pair must leave the stack where it found it. Without
\ that the next declaration in a file refuses too and the first diagnostic names
\ the wrong one.
variable FFI-T-PAIR-BEFORE
variable FFI-T-PAIR-AFTER

: FFI-T-REFUSED-CLAUSE ( -- )
   s" FUNCTION: FFI-T-XA getpid ( ^u8 -- n ) 9 VARIADIC ;FUNCTION"
   evaluate-closed ;
: FFI-T-GOOD-AFTER ( -- )
   s" FUNCTION: FFI-T-XB getpid ( -- n ) ;FUNCTION" evaluate-closed ;

\ A declaration that never closes defines nothing, and is rejected when the
\ next FUNCTION: opens; the rejection abandons both, so the declaration after
\ it compiles and a closer with nothing open is rejected.
: FFI-T-BAD-UNCLOSED ( -- )
   s" FUNCTION: FFI-T-X7 getpid ( -- n ) FUNCTION: FFI-T-X8 getpid ( -- n )"
   evaluate-closed ;
: FFI-T-CLOSE-DANGLING ( -- )
   s" ;FUNCTION" evaluate-closed ;
: FFI-T-REDECLARE ( -- )
   s" FUNCTION: FFI-T-X9 getpid ( -- n ) ;FUNCTION" evaluate-closed ;

\ A child declares 270 distinct symbols and 270 library paths, past any fixed
\ table; an alias and a declaration after the paths then call.
$4000 constant FFI-T-CAP

CODEGEN:BUFFER FFI-T-SRC

create FFI-T-OUT FFI-T-CAP allot
create FFI-T-ERR FFI-T-CAP allot
create FFI-T-LONG-PATH PATH-CAP allot

: FFI-T-SRC+ ( str -- )
   FFI-T-SRC CODEGEN:APPEND-STRING ;

: FFI-T-FILL-SRC+ ( -- )
   s\" require src/lib/ffi-abi.f\nrequire src/lib/codegen.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-BASE getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" CODEGEN:BUFFER ZZ-DECL\n: ZZ-FILL ( -- ) 270 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-DECL CODEGEN:RESET\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-OVER habu-ffi-fill-\" >str ZZ-DECL CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-DECL CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    s\"  ( -- n ) ;FUNCTION\" >str ZZ-DECL CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    ZZ-DECL CODEGEN:CONTENTS str> evaluate-closed\n" FFI-T-SRC+
   s\"    s\" undefine ZZ-OVER\" evaluate-closed\n" FFI-T-SRC+
   s\" loop ;\nZZ-FILL\n" FFI-T-SRC+ ;

: FFI-T-LIBS-SRC+ ( -- )
   s\" CODEGEN:BUFFER ZZ-PATH\n" FFI-T-SRC+
   s\" : ZZ-PATH$ ( n -- str ) {: id:n :} ZZ-PATH CODEGEN:RESET\n" FFI-T-SRC+
   s\"    s\" /tmp/habu-ffi-path-missing-\" >str ZZ-PATH CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    id ZZ-PATH CODEGEN:APPEND-DECIMAL ZZ-PATH CODEGEN:CONTENTS ;\n" FFI-T-SRC+
   s\" : ZZ-LIBS ( -- ) 270 0 ?do i ZZ-PATH$ FFI:SELECT-LIBRARY loop ;\n" FFI-T-SRC+
   s\" ZZ-LIBS\n" FFI-T-SRC+
   s\" : ZZ-SYSTEM$ ( -- str ) HB-TARGET-MACOS? if s\" /usr/lib/libSystem.B.dylib\" >str exit then\n" FFI-T-SRC+
   s\"    s\" libc.so.6\" >str ;\n" FFI-T-SRC+
   s\" ZZ-SYSTEM$ FFI:SELECT-LIBRARY\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-LATE getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" : ZZ-LIB-CHECK ( -- )\n" FFI-T-SRC+
   s\"    ZZ-LATE 0 <= if s\" late library call\" 1 die then ;\n" FFI-T-SRC+ ;

: FFI-T-GROW-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   FFI-T-FILL-SRC+
   s\" FUNCTION: ZZ-ALIAS getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   FFI-T-LIBS-SRC+
   s\" : ZZ-CHECK ( -- )\n" FFI-T-SRC+
   s\"    ZZ-BASE 0 <= ZZ-ALIAS 0 <= or if s\" getpid refused\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-LIB-CHECK\n" FFI-T-SRC+
   s\"    s\" growth-alias:ok\" type cr ;\nZZ-CHECK\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;
: FFI-T-ERR$ ( len -- str ) {: u:len :}
   FFI-T-ERR u LEN>N ;

\ Runs the source on a fresh engine's stdin and asserts its exit code.
: FFI-T-RUN-EXITS ( str n -- len len ) {: src:^ srcu:n want:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN src srcu >LEN
   FFI-T-OUT FFI-T-CAP >LEN FFI-T-ERR FFI-T-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   src srcu FFI-T-OUT outu LEN>N FFI-T-ERR erru LEN>N oc want T-ENDING-EXITED=
   outu erru ;

: FFI-T-GROW-ALIAS ( -- )
   s" descriptors and library paths grow past 256 and later declarations call" T-LABEL
   FFI-T-GROW-SRC$ 0 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   FFI-T-OUT outu LEN>N s\" growth-alias:ok\n" T$=
   erru LEN>N 0 T= ;
: FFI-T-LIBRARY-SHARED-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nrequire src/lib/codegen.f\n" FFI-T-SRC+
   s\" CODEGEN:BUFFER ZZ-NAME\npackage ZZ-LIB-A\npublic\n" FFI-T-SRC+
   s\" : SELECT-DISTINCT ( -- ) 10 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:RESET s\" /tmp/habu-ffi-missing-\" >str ZZ-NAME CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-NAME CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:CONTENTS FFI:SELECT-LIBRARY\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-MISSING habu-missing ( -- n ) ;FUNCTION\" evaluate-closed\n" FFI-T-SRC+
   s\"    s\" undefine ZZ-MISSING\" evaluate-closed\n" FFI-T-SRC+
   s\" loop ;\nSELECT-DISTINCT\n" FFI-T-SRC+
   s" LIBRARY " FFI-T-SRC+
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib" FFI-T-SRC+
   else
      s" libc.so.6" FFI-T-SRC+
   then
   s\" \nFUNCTION: PID-A getpid ( -- i32 ) ;FUNCTION\n;package\n" FFI-T-SRC+
   s\" package ZZ-LIB-B\n: SELECT-REPEATED ( -- ) 20 0 ?do s\" LIBRARY " FFI-T-SRC+
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib" FFI-T-SRC+
   else
      s" libc.so.6" FFI-T-SRC+
   then
   s\" \" evaluate-closed loop ;\nSELECT-REPEATED\n" FFI-T-SRC+
   s\" FUNCTION: PID-B getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" : RUN ( -- ) ZZ-LIB-A:PID-A 0 > PID-B 0 > and if s\" shared-library:ok\" type cr else s\" getpid refused\" 1 die then ;\nRUN\n;package\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LIBRARY-SHARED ( -- )
   s" repeated library selections share a callable binding" T-LABEL
   FFI-T-LIBRARY-SHARED-SRC$ 0 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   FFI-T-OUT outu LEN>N s\" shared-library:ok\n" T$=
   erru LEN>N 0 T= ;

\ Runs a rejected case with its record captured, so stderr carries none,
\ and asserts the record's header line.
: FFI-T-RECORD ( [ -- ] str -- )
   {: h:^ hu:n :}
   DECL-DIAG:PROSE
   70 TTHROWSQ
   h hu DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF ;

: FFI-T-SCOPE-RECORD ( [ -- ] -- )
   s" error[E-FFI-LIBRARY]: the library selected last belongs to another package section or to top level"
   FFI-T-RECORD ;

\ The overflowing path carries no extension: it stands for a literal LIBRARY row
\ on either target, which the spelling rule below leaves alone.
: FFI-T-LONG-PATH! ( -- )
   PATH-CAP 0 ?do [char] a FFI-T-LONG-PATH i + c! loop ;

: FFI-T-PATH-LENGTH ( -- )
   s" path length is exact from one through PATH-CAP" T-LABEL
   1 REJECTION:QUIET +!
   [: FFI-T-LONG-PATH 0 FFI:SELECT-LIBRARY ;] 70 TTHROWSQ
   -1 REJECTION:QUIET +!
   FFI-T-LONG-PATH!
   FFI-T-LONG-PATH PATH-CAP FFI:SELECT-LIBRARY
   1 REJECTION:QUIET +!
   [: FFI-T-LONG-PATH PATH-CAP 1+ FFI:SELECT-LIBRARY ;] 70 TTHROWSQ
   -1 REJECTION:QUIET +!
   FFI:SELECT-PROCESS ;

: FFI-T-LONG-SRC! ( -- )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\ncreate ZZ-P PATH-CAP allot\n" FFI-T-SRC+
   s\" : ZZ-LONG ( -- ^u8 n ) PATH-CAP 0 ?do [char] a ZZ-P i + c! loop ZZ-P PATH-CAP ;\n" FFI-T-SRC+ ;

\ libz, which the VERSIONED-LIBRARY row below opens with FFI:NOW alone.
\ dlopen(3) names the scope its symbols join: on Linux an open without
\ RTLD_GLOBAL is RTLD_LOCAL, and RTLD_DEFAULT - what PROCESS-SYMBOLS resolves
\ through - misses it; macOS defaults to RTLD_GLOBAL when an open names neither.
\ The library's own handle finds crc32 either way, so a miss through the
\ process is the scope and not the library. The second open promotes the
\ already loaded library, as dlopen(3) does for a reopen with RTLD_GLOBAL.
: FFI-T-GLOBAL-LIB$ ( -- ^u8 )
   HB-TARGET-MACOS? if s\" libz.1.dylib\z" drop exit then s\" libz.so.1\z" drop ;

\ The miss prints its record, so it runs in a child whose stderr is asserted.
: FFI-T-LOCAL-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-PROBE crc32 ( n ^u8 n -- n ) ;FUNCTION\n: ZZ-MISS ( -- )\n" FFI-T-SRC+
   s\"    s\\\" libz.so.1\\z\" drop FFI:NOW FFI:DLOPEN 0= if s\" open\" 1 die then\n" FFI-T-SRC+
   s\"    [: 0 s\" a\" ZZ-PROBE drop ;] catch E-FFI-DLSYM <> if s\" miss\" 1 die then ;\n" FFI-T-SRC+
   s\" ZZ-MISS\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LOCAL ( -- )
   s" Linux: an FFI:NOW open is local, so the process does not resolve it" T-LABEL
   FFI-T-LOCAL-SRC$ 0 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$
   s" error[E-FFI-DLSYM]: no C symbol 'crc32' among the process's symbols"
   CONTAINS? TTRUE
   erru FFI-T-ERR$
   s" = help: name the library that defines it with LIBRARY, or open it with FFI:PROCESS-WIDE"
   CONTAINS? TTRUE ;

: FFI-T-GLOBAL ( -- )
   s" an FFI:NOW open finds the function through its own handle" T-LABEL
   FFI-T-GLOBAL-LIB$ FFI:NOW FFI:DLOPEN {: h:n :}
   h 0 T<>
   h s\" crc32\z" drop FFI:DLSYM 0 T<>
   HB-TARGET-MACOS? if
      s" macOS: an FFI:NOW open is global, so the process resolves it" T-LABEL
      0 s" a" FFI-T-GLOBAL-PROBE $E8B7BE43 T=
   else
      FFI-T-LOCAL
   then
   s" an FFI:NOW FFI:PROCESS-WIDE or open puts the function in process-wide resolution" T-LABEL
   FFI-T-GLOBAL-LIB$ FFI:NOW FFI:PROCESS-WIDE or FFI:DLOPEN 0 T<>
   0 s" a" FFI-T-GLOBAL-PROBE $E8B7BE43 T=
   s" a str input works in a file without split-str" T-LABEL
   FFI-STR-UNMARKED:FFI-U-CRC-A $E8B7BE43 T= ;

: FFI-T-CHECK-PASSES ( str -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: FFI-T-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ The libm rows ride the ABI call: sqrt a float argument and a float result,
\ fma three float registers, scalbln a float register beside all 64 bits of the
\ first integer register (d0-d2 and x0 on AAPCS64, xmm0-xmm2 and rdi on SysV).
\ The library is chosen at load time because the two targets keep it in
\ different files, which is what the runtime-string form of LIBRARY is for.
: FFI-T-SELECT-MATH ( -- )
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib"
   else
      s" libm.so.6"
   then FFI:SELECT-LIBRARY ;

FFI-T-SELECT-MATH
FUNCTION: FFI-T-SQRT-CALL sqrt ( r -- r ) ;FUNCTION
FUNCTION: FFI-T-FMA-CALL fma ( r r r -- r ) ;FUNCTION
FUNCTION: FFI-T-FMAF-CALL fmaf ( f32 f32 f32 -- f32 ) ;FUNCTION
FUNCTION: FFI-T-SCALBLN-CALL scalbln ( r n -- r ) ;FUNCTION
PROCESS-SYMBOLS

\ The same declaration exercises Apple's stack varargs and Linux's registers.
FUNCTION: FFI-T-PRINTF snprintf ( ^u8 n ^u8 n r ^u8 -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-SNPRINT-SHORT snprintf ( ^u8 n ^u8 n -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-SNPRINT-LONG snprintf ( ^u8 n ^u8 n n n n n n n -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-PRINTF-INTS snprintf ( ^u8 n ^u8 n n n n n n n n -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-PRINTF-MIXED snprintf ( ^u8 n ^u8 n n n n r n -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-PRINTF-COUNT snprintf ( ^u8 n ^u8 n n n ^u8 -- i32 )
   3 VARIADIC
;FUNCTION
FUNCTION: FFI-T-MPROTECT mprotect ( ^u8 n n -- i32 ) ;FUNCTION
1 constant FFI-T-PROT-READ
64 BUFFER: FFI-T-PRINT-BUF
4 BUFFER: FFI-T-COUNT

\ macOS's printf aborts on a %n whose format lies in writable memory, and a
\ literal's bytes live in DATA (src/habu/intern.f), so the %n format
\ is copied into a fresh mapping and the page holding it made read-only for
\ the call.
: FFI-T-PRINTF-COUNTED ( -- n )
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: fmt:^ fmtu :}
   s\" %ld %ld %ld%n\z" {: src:^ srcu:n :}
   src fmt srcu BYTE-COPY
   fmt srcu FFI-T-PROT-READ FFI-T-MPROTECT 0 T=
   FFI-T-PRINT-BUF 64 fmt 11 22 33 FFI-T-COUNT FFI-T-PRINTF-COUNT
   fmt fmtu MEM:RELEASE-BYTES ;


: FFI-T-VARARGS ( -- )
   FFI-T-PRINT-BUF 64 s\" %ld %.1f %s\z" drop 42 1.5 s\" ok\z" drop FFI-T-PRINTF
   FFI-T-PRINT-BUF swap s" 42 1.5 ok" T$=
   FFI-T-PRINT-BUF 64 s\" %ld %ld %ld %ld %ld %ld %ld %ld\z" drop
      1 2 3 4 5 6 7 8 FFI-T-PRINTF-INTS
   FFI-T-PRINT-BUF swap s" 1 2 3 4 5 6 7 8" T$=
   FFI-T-PRINT-BUF 64 s\" %ld %ld %ld %ld %.1f %ld\z" drop
      11 22 33 44 5.5 66 FFI-T-PRINTF-MIXED
   FFI-T-PRINT-BUF swap s" 11 22 33 44 5.5 66" T$=
   0 FFI-T-COUNT LE:U32!
   FFI-T-PRINTF-COUNTED
   FFI-T-PRINT-BUF swap s" 11 22 33" T$=
   FFI-T-COUNT LE:U32@ 8 T= ;

\ snprintf's ninth double goes on the stack on every target.
: FFI-T-F9-FORMAT ( -- ^u8 )
   s\" %.0f %.0f %.0f %.0f %.0f %.0f %.0f %.0f %.0f\z" drop ;
: FFI-T-F9-WANT ( -- ^u8 n )
   s" 1 2 3 4 5 6 7 8 9" ;
: FFI-T-FLOAT-SPILL ( -- )
   s" FUNCTION: FFI-T-PRINTF-F9 snprintf ( ^u8 n ^u8 r r r r r r r r r -- i32 ) 3 VARIADIC ;FUNCTION"
      evaluate-closed
   FFI-T-SRC CODEGEN:RESET
   s" : FFI-T-F9-CHECK ( -- ) FFI-T-PRINT-BUF 64 FFI-T-F9-FORMAT"
      FFI-T-SRC+
   s"  1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 FFI-T-PRINTF-F9"
      FFI-T-SRC+
   s"  FFI-T-PRINT-BUF swap FFI-T-F9-WANT T$= ; FFI-T-F9-CHECK"
      FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS evaluate-closed ;

: FFI-T-VARARG-KEY ( -- )
   s" a short variadic binding resolves before a longer one" T-LABEL
   FFI-T-PRINT-BUF 64 s\" %ld\z" drop 9 FFI-T-SNPRINT-SHORT 1 T=
   FFI-T-PRINT-BUF 1 s" 9" T$=
   s" a longer binding preserves its ABI arguments and output" T-LABEL
   FFI-T-PRINT-BUF 64 s\" %ld %ld %ld %ld %ld %ld %ld\z" drop
      1 2 3 4 5 6 7 FFI-T-SNPRINT-LONG 13 T=
   FFI-T-PRINT-BUF 13 s" 1 2 3 4 5 6 7" T$= ;

\ A library selected by base name and soname version: libz.so.1 here and
\ libz.1.dylib on macOS both exist, so the symbol resolves through the rendered
\ selection at the first call on either target.
VERSIONED-LIBRARY z 1
FUNCTION: FFI-T-VERSIONED-CRC crc32 ( n ^u8 n -- n ) ;FUNCTION
PROCESS-SYMBOLS

: FFI-T-VERSIONED-CALL ( -- )
   0 s" a" FFI-T-VERSIONED-CRC $E8B7BE43 T= ;

\ A version list: no libz 10000 exists on either target, so the first call is
\ served by the second version written.
VERSIONED-LIBRARY z 10000,1
FUNCTION: FFI-T-LISTED-CRC crc32 ( n ^u8 n -- n ) ;FUNCTION
PROCESS-SYMBOLS

: FFI-T-LISTED-CALL ( -- )
   0 s" a" FFI-T-LISTED-CRC $E8B7BE43 T= ;

: FFI-T-NO-SUCH2-NAME$ ( -- str )
   HB-TARGET-MACOS? if s" libhabu-no-such.2.dylib" exit then
   s" libhabu-no-such.so.2" ;

: FFI-T-NO-SUCH1-NAME$ ( -- str )
   HB-TARGET-MACOS? if s" libhabu-no-such.1.dylib" exit then
   s" libhabu-no-such.so.1" ;

: FFI-T-Z1-NAME$ ( -- str )
   HB-TARGET-MACOS? if s" libz.1.dylib" exit then s" libz.so.1" ;

: FFI-T-Z9999-NAME$ ( -- str )
   HB-TARGET-MACOS? if s" libz.9999.dylib" exit then s" libz.so.9999" ;

\ A 100-byte name no library has, for a symbol, a token and a base name.
100 constant FFI-T-LONG-U
create FFI-T-LONG FFI-T-LONG-U allot

: FFI-T-LONG$ ( -- str )
   FFI-T-LONG-U 0 ?do [char] x FFI-T-LONG i + c! loop
   s" habu-ffi-long-" {: p:^ pu:n :}
   p FFI-T-LONG pu BYTE-COPY
   FFI-T-LONG FFI-T-LONG-U ;

: FFI-T-LONG+ ( -- ) FFI-T-LONG$ FFI-T-SRC+ ;

\ Appends the library name version 1 of the 100-byte base renders.
: FFI-T-LONG-NAME+ ( -- )
   s" lib" SB-APPEND  FFI-T-LONG$ SB-APPEND
   HB-TARGET-MACOS? if s" .1.dylib" else s" .so.1" then SB-APPEND ;


\ A path one byte longer than the table holds, whose bytes end at an
\ inaccessible page: the length is refused before the spelling check reads it.
: FFI-T-GUARDED-PATH ( -- )
   PATH-CAP 1+ [char] a GUARD-PAGE:TAIL
   PATH-CAP 1+ 1+ FFI:SELECT-LIBRARY ;

\ A literal spelled in the OTHER target's convention names no file here, so the
\ declarer rejects it where it is stated. The rejection ends the load, which is
\ why it is measured in a child like the growth case.
: FFI-T-FOREIGN-NAME$ ( -- str )
   HB-TARGET-MACOS? if s" libx.so.5" exit then s" libx.5.dylib" ;

: FFI-T-TARGET$ ( -- str )
   HB-TARGET-MACOS? if s" macos" exit then s" linux" ;

: FFI-T-FOREIGN-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\n" FFI-T-SRC+
   s\" : ZZ-FOREIGN ( -- ) s\" LIBRARY " FFI-T-SRC+
   FFI-T-FOREIGN-NAME$ FFI-T-SRC+
   s\" \" evaluate-closed ;\nZZ-FOREIGN\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-FOREIGN-LIBRARY ( -- )
   s" a library name spelled for the other target is rejected by name" T-LABEL
   FFI-T-FOREIGN-SRC$ 70 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   s" the record names the rejected name and this target" T-LABEL
   SB-RESET s" error[E-FFI-LIBRARY]: library '" SB-APPEND
   FFI-T-FOREIGN-NAME$ SB-APPEND
   s" ' is spelled for the other target" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE
   SB-RESET s" = help: name it with VERSIONED-LIBRARY, which spells it for " SB-APPEND
   FFI-T-TARGET$ SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE ;

\ A fresh engine has made no selection, which this file's PROCESS-SYMBOLS has,
\ and the rejection ends the load: the case runs in a child.
: FFI-T-UNSELECTED-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-PID getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-UNSELECTED ( -- )
   s" a declaration before any selection is rejected with a record" T-LABEL
   FFI-T-UNSELECTED-SRC$ 70 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ s" error[E-FFI-LIBRARY]: FUNCTION: with no library selected" CONTAINS? TTRUE ;

: FFI-T-LONG-FOREIGN-SRC$ ( -- str )
   FFI-T-LONG-SRC!
   s\" : ZZ-FOREIGN ( -- ) ZZ-LONG {: path:^ u:n :}\n" FFI-T-SRC+
   s\" HB-TARGET-MACOS? if s\" .so\" else s\" .dylib\" then {: ext:^ eu:n :}\n" FFI-T-SRC+
   s\" ext path u eu - + eu BYTE-COPY path u >str FFI:SELECT-LIBRARY ;\nZZ-FOREIGN\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LONG-FOREIGN-LIBRARY ( -- )
   s" a PATH-CAP foreign name keeps the target rejection and its full name" T-LABEL
   FFI-T-LONG-FOREIGN-SRC$ 70 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ s" error[E-FFI-LIBRARY]" CONTAINS? TTRUE
   FFI-T-LONG-PATH!
   HB-TARGET-MACOS? if s" .so" else s" .dylib" then {: ext:^ eu:n :}
   ext FFI-T-LONG-PATH PATH-CAP eu - + eu BYTE-COPY
   erru FFI-T-ERR$ s" error[E-FFI-LIBRARY]: library 'aaaa" CONTAINS? TTRUE
   erru FFI-T-ERR$ FFI-T-LONG-PATH PATH-CAP CONTAINS? TTRUE
   SB-RESET s" = help: name it with VERSIONED-LIBRARY, which spells it for " SB-APPEND
   FFI-T-TARGET$ SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE ;

: FFI-T-NO-LIB$ ( -- str )
   HB-TARGET-MACOS? if s" libhabu-no-such-library.dylib" exit then
   s" libhabu-no-such-library.so" ;

: FFI-T-LIBC$ ( -- str )
   HB-TARGET-MACOS? if s" /usr/lib/libSystem.B.dylib" exit then
   s" libc.so.6" ;

\ Missing symbols and a missing library fail at the first call, never at the
\ declaration; the final uncaught error ends the load, so the case runs in a child.
: FFI-T-NO-LIB-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-ABSENT habu-no-such-symbol ( -- n ) ;FUNCTION\nLIBRARY " FFI-T-SRC+
   FFI-T-NO-LIB$ FFI-T-SRC+
   s\" \nFUNCTION: ZZ-NO-LIB getpid ( -- n ) ;FUNCTION\nLIBRARY " FFI-T-SRC+
   FFI-T-LIBC$ FFI-T-SRC+
   s\" \nFUNCTION: ZZ-ABSENT-IN habu-no-such-symbol ( -- n ) ;FUNCTION\n" FFI-T-SRC+
   s" FUNCTION: " FFI-T-SRC+ FFI-T-LONG+ s"  " FFI-T-SRC+ FFI-T-LONG+
   s\"  ( -- n ) ;FUNCTION\n: ZZ-CALL ( -- )\n" FFI-T-SRC+
   s\"    [: ZZ-ABSENT drop ;] catch E-FFI-DLSYM <> if s\" absent\" 1 die then\n" FFI-T-SRC+
   s" [: " FFI-T-SRC+ FFI-T-LONG+
   s\"  drop ;] catch E-FFI-DLSYM <> if s\" long\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-ABSENT-IN drop ;] catch E-FFI-DLSYM <> if s\" absent in\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-LIB drop ;] catch E-FFI-DLOPEN <> if s\" no lib\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-LIB drop ;] catch E-FFI-DLOPEN <> if s\" no lib again\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-NO-LIB drop ;\nZZ-CALL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-NO-LIB ( -- )
   s" a missing library fails at the first call with its path and reason, once" T-LABEL
   FFI-T-NO-LIB-SRC$ 67 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-DLOPEN TEST-EVAL:THROWN-HEAD$ CONTAINS? TTRUE
   SB-RESET s" error[E-FFI-DLOPEN]: cannot open library '" SB-APPEND
   FFI-T-NO-LIB$ SB-APPEND s\" '\n  = note: tried '" SB-APPEND
   FFI-T-NO-LIB$ SB-APPEND s" ': " SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   erru FFI-T-ERR$
   s" = help: install the library, or name a path the loader finds"
   COUNT-SUB 1 T=
   s" a missing process symbol fails with its name" T-LABEL
   erru FFI-T-ERR$
   S\" error[E-FFI-DLSYM]: no C symbol 'habu-no-such-symbol' among the process's symbols\n"
   CONTAINS? TTRUE
   erru FFI-T-ERR$
   s" = help: name the library that defines it with LIBRARY, or open it with FFI:PROCESS-WIDE"
   CONTAINS? TTRUE
   s" a missing library symbol fails with its name and the library's path" T-LABEL
   SB-RESET
   s" error[E-FFI-DLSYM]: no C symbol 'habu-no-such-symbol' in '" SB-APPEND
   FFI-T-LIBC$ SB-APPEND S\" '\n" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE
   erru FFI-T-ERR$
   s" = help: check the symbol's spelling against the library's exports"
   CONTAINS? TTRUE
   s" a 100-byte name and symbol reach the call whole" T-LABEL
   SB-RESET s" error[E-FFI-DLSYM]: no C symbol '" SB-APPEND FFI-T-LONG$ SB-APPEND
   s" ' in '" SB-APPEND FFI-T-LIBC$ SB-APPEND S\" '\n" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE ;

\ No version of the list exists, so the first call reports every name and a
\ second call nothing; the final uncaught error ends the load, so it runs in a child.
\ A symbol missing from a list whose second version opens names that version.
: FFI-T-NO-VERSION-SRC$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 9999,1\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-LISTED-ABSENT habu-no-such-symbol ( -- n ) ;FUNCTION\n" FFI-T-SRC+
   s\" VERSIONED-LIBRARY habu-no-such 2,1\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-NO-VERSION getpid ( -- n ) ;FUNCTION\n" FFI-T-SRC+
   s" VERSIONED-LIBRARY " FFI-T-SRC+ FFI-T-LONG+
   s\"  1\nFUNCTION: ZZ-LONG-BASE getpid ( -- n ) ;FUNCTION\n: ZZ-CALL ( -- )\n" FFI-T-SRC+
   s\"    [: ZZ-LISTED-ABSENT drop ;] catch E-FFI-DLSYM <> if s\" listed absent\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-LONG-BASE drop ;] catch E-FFI-DLOPEN <> if s\" long base\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-VERSION drop ;] catch E-FFI-DLOPEN <> if s\" no version\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-NO-VERSION drop ;\nZZ-CALL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-HUGE-BASE-SRC$ ( n -- str ) {: u:n :}
   FFI-T-LONG-PATH!
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY " FFI-T-SRC+
   FFI-T-LONG-PATH u FFI-T-SRC+
   s\"  1\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

\ The longest base whose name at version 1 fits a path.
: FFI-T-BASE-MAX ( -- n )
   PATH-CAP  HB-TARGET-MACOS? if s" lib.1.dylib" else s" lib.so.1" then nip - ;

: FFI-T-HUGE-BASE ( -- )
   s" a base whose library name fills a path loads" T-LABEL
   FFI-T-BASE-MAX FFI-T-HUGE-BASE-SRC$ 0 FFI-T-RUN-EXITS
   {: fitu:len fiterr:len :}
   fitu LEN>N 0 T=
   fiterr LEN>N 0 T=
   s" a base whose library name passes the path cap is rejected at the base" T-LABEL
   FFI-T-BASE-MAX 1+ FFI-T-HUGE-BASE-SRC$ 70 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ s" error[E-FFI-SYNTAX]: the library name made from this base" CONTAINS? TTRUE
   SB-RESET s" = limit: " SB-APPEND PATH-CAP FMT:SB-INT s"  bytes" SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   SB-RESET s" = found: " SB-APPEND PATH-CAP 1+ FMT:SB-INT s"  bytes" SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   erru FFI-T-ERR$ s" error[" COUNT-SUB 1 T= ;

\ LIBRARY's path one byte past the path cap: its record names the length found.
: FFI-T-HUGE-PATH-SRC$ ( -- str )
   FFI-T-LONG-PATH!
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nLIBRARY " FFI-T-SRC+
   FFI-T-LONG-PATH PATH-CAP FFI-T-SRC+  s\" a\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-HUGE-PATH ( -- )
   s" a library path past the path cap is rejected with its length" T-LABEL
   FFI-T-HUGE-PATH-SRC$ 70 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ s" error[E-FFI-SYNTAX]: the library path is longer than a path can be" CONTAINS? TTRUE
   SB-RESET s" = found: " SB-APPEND PATH-CAP 1+ FMT:SB-INT s"  bytes" SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T= ;

: FFI-T-NO-VERSION ( -- )
   s" a version list with none present names each version in order, once" T-LABEL
   FFI-T-NO-VERSION-SRC$ 67 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-DLOPEN TEST-EVAL:THROWN-HEAD$ CONTAINS? TTRUE
   FFI-T-NO-SUCH2-NAME$ {: a:^ au:n :}
   FFI-T-NO-SUCH1-NAME$ {: b:^ bu:n :}
   \ The first name carries no reason; the last one tried carries the loader's.
   SB-RESET s" error[E-FFI-DLOPEN]: cannot open library '" SB-APPEND
   a au SB-APPEND s\" '\n  = note: tried '" SB-APPEND
   a au SB-APPEND s\" '\n  = note: tried '" SB-APPEND
   b bu SB-APPEND s" ': " SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   SB-RESET s" tried '" SB-APPEND a au SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   SB-RESET s" tried '" SB-APPEND b bu SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   s" a missing symbol names the version that opened, never one that did not" T-LABEL
   FFI-T-Z1-NAME$ {: z1:^ z1u:n :}
   FFI-T-Z9999-NAME$ {: z9:^ z9u:n :}
   SB-RESET
   s" error[E-FFI-DLSYM]: no C symbol 'habu-no-such-symbol' in '" SB-APPEND
   z1 z1u SB-APPEND S\" '\n" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE
   erru FFI-T-ERR$ z9 z9u CONTAINS? TFALSE
   s" a 100-byte base name reaches the loader whole" T-LABEL
   SB-RESET s" error[E-FFI-DLOPEN]: cannot open library '" SB-APPEND
   FFI-T-LONG-NAME+ s\" '\n  = note: tried '" SB-APPEND
   FFI-T-LONG-NAME+ s" ': " SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T= ;

: FFI-RUN ( -- )
   T-RESET
   FFI-T-OPEN dup FFI-T-LIB !
   0 T<>
   FFI-T-OPEN-MATH dup FFI-T-MATH !
   0 T<>

   FFI-T-STRLEN FFI-T-SYM 0 T<>
   FFI-T-SQRT FFI-T-MSYM 0 T<>
   FFI-T-HELLO FFI-T-STRLEN-LATE 5 T=
   FFI-T-HELLO FFI-T-STRLEN$ 5 T=

   FFI-T-HELLO FFI-T-HELP 3 FFI-T-STRNCMP$ 0 T=
   FFI-T-HELLO FFI-T-HELP 4 FFI-T-STRNCMP$ 0 T<>

   s" C int results preserve zero, positive and negative values" T-LABEL
   s\" 0\z" drop FFI-T-ATOI 0 T=
   s\" 2147483647\z" drop FFI-T-ATOI 2147483647 T=
   s\" -2147483648\z" drop FFI-T-ATOI -2147483648 T=
   -1 FFI-T-CLOSE -1 T=
   FFI:ERRNO 9 T=                                \ EBADF immediately after close(-1)
   s" an unsigned int result is masked to its low half" T-LABEL
   -1 FFI-T-CLOSE-U32 $FFFFFFFF T=
   s" i64, cell and addr pass a whole register" T-LABEL
   $123456789 negate FFI-T-LABS-I64 $123456789 T=
   $123456789 negate FFI-T-LABS-CELL $123456789 T=
   $123456789 negate FFI-T-LABS-ADDR $123456789 T=

   FFI-T-GETPID$ 0 T<>
   FFI-T-VOID$  FFI-T-GETPID$ 0 T<>               \ void-return wrapper is stack-neutral

   s" a mixed floating call returns the output buffer and writes its value" T-LABEL
   72.5 5 FFI-T-GCVT-BUF FFI-T-GCVT {: converted:^ :}
   converted FFI-T-GCVT-BUF - 0 T=
   converted converted FFI-T-STRLEN$ s" 72.5" T$=

   FFI-T-CSTR-SRC 5 FFI-T-CSTR-DST FFI:CSTR       \ build "world\0" then strlen==5
   FFI-T-STRLEN$ 5 T=

   s" FFI-T-ROLE-GOOD ( ffi-ctx -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-PASSES
   s" FFI-T-ROLE-BAD ( ffi-dev -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-REJECTS

   FFI-T-GETPID$ 0 T<>
   1.5 4 FFI-T-SCALBLN-CALL 24.0 f= T-ASSERT

   1.25 2.5 3.0 FFI-T-FMA-CALL 6.125 f= T-ASSERT
   1.25 2.5 3.0 FFI-T-FMAF-CALL 6.125 f= T-ASSERT

   9.0 FFI-T-SQRT-CALL 3.0 f= T-ASSERT

   s" a declaration leaves the stack as it found it" T-LABEL
   FFI-T-DEPTH-AFTER @ FFI-T-DEPTH-BEFORE @ T=
   FFI-T-DEPTH-PROBE 0 T<>

   FFI-T-CSTR-DST SPAN:$ drop FFI-T-HELLO 6 FFI-T-MEMCPY drop
   FFI-T-CSTR-DST SPAN:$ drop FFI-T-STRLEN$ 5 T=
   s" a writable span of 0 bytes is called and left as it was" T-LABEL
   FFI-T-CSTR-DST SPAN:$ drop FFI-T-HELLO 0 FFI-T-MEMCPY drop
   FFI-T-CSTR-DST SPAN:$ drop FFI-T-STRLEN$ 5 T=

   [: FFI-T-BAD-PTR ;]
   s" error[E-FFI-SYNTAX]: expected 'u8' after '^'" FFI-T-RECORD
   [: FFI-T-BAD-RESULTS ;]
   s" error[E-FFI-SYNTAX]: a FUNCTION: declaration has at most one output" FFI-T-RECORD
   [: FFI-T-BAD-IN-TYPE ;]
   s" error[E-FFI-SYNTAX]: 'fd' is not an input type: an input is ^u8, str, f32, r, i32, n, i64, cell, addr, u8, u16, u32 or char"
   FFI-T-RECORD
   \ A pointer input is spelled ^u8: `ptr` is no input type.
   [: FFI-T-PTR-IN ;]
   s" error[E-FFI-SYNTAX]: 'ptr' is not an input type: an input is ^u8, str, f32, r, i32, n, i64, cell, addr, u8, u16, u32 or char"
   FFI-T-RECORD
   [: FFI-T-BAD-OUT-TYPE ;]
   s" error[E-FFI-SYNTAX]: 'bool' is not an output type: an output is ^u8, f32, i32, nullable<u8>, r, n, i64, cell, addr, u8, u16, u32 or char"
   FFI-T-RECORD
   [: FFI-T-BAD-F32-VARARG ;]
   s" error[E-FFI-SYNTAX]: an f32 variadic argument" FFI-T-RECORD
   [: FFI-T-F32-FIXED ;] 0 TTHROWSQ
   [: FFI-T-STR-FIXED ;] 0 TTHROWSQ
   [: FFI-T-PTR-MIXED ;] 0 TTHROWSQ
   [: FFI-T-PTR-VARARG ;] 0 TTHROWSQ
   [: FFI-T-BAD-VARARG ;]
   s" error[E-FFI-SYNTAX]: VARIADIC's fixed count is not from 1 to the inputs declared" FFI-T-RECORD
   [: FFI-T-NO-FIXED ;]
   s" error[E-FFI-SYNTAX]: VARIADIC's fixed count is not from 1 to the inputs declared" FFI-T-RECORD
   [: FFI-T-LIST-END ;]
   s" error[E-FFI-SYNTAX]: a library version is not a number" FFI-T-RECORD
   [: FFI-T-LIST-START ;]
   s" error[E-FFI-SYNTAX]: a library version is not a number" FFI-T-RECORD
   [: FFI-T-LIST-GAP ;]
   s" error[E-FFI-SYNTAX]: a library version is not a number" FFI-T-RECORD
   [: FFI-T-LIST-WORD ;]
   s" error[E-FFI-SYNTAX]: a library version is not a number" FFI-T-RECORD
   s" a library selection is scoped to the scope that states it" T-LABEL
   FFI-T-SCOPE-BEFORE FFI-T-DEPTH!
   [: FFI-T-NO-LIBRARY ;] FFI-T-SCOPE-RECORD
   FFI-T-PRIVATE-AGAIN
   [: FFI-T-SCOPED-LIBRARY ;] 0 TTHROWSQ
   FFI-T-RESELECT
   FFI-T-SCOPE-AFTER FFI-T-DEPTH!
   FFI-T-SCOPE-AFTER @ FFI-T-SCOPE-BEFORE @ T=

   s" a refused declaration is abandoned, not left open" T-LABEL
   FFI-T-PAIR-BEFORE FFI-T-DEPTH!
   [: FFI-T-REFUSED-CLAUSE ;]
   s" error[E-FFI-SYNTAX]: VARIADIC's fixed count is not from 1 to the inputs declared" FFI-T-RECORD
   [: FFI-T-GOOD-AFTER ;] 0 TTHROWSQ
   FFI-T-PAIR-AFTER FFI-T-DEPTH!
   FFI-T-PAIR-AFTER @ FFI-T-PAIR-BEFORE @ T=

   [: FFI-T-BAD-UNCLOSED ;]
   s" error[E-UNCLOSED]: `FUNCTION:` is still open at `FUNCTION:`"
   FFI-T-RECORD
   [: FFI-T-REDECLARE ;] 0 TTHROWSQ
   [: FFI-T-CLOSE-DANGLING ;]
   s" error[E-FFI-SYNTAX]: ;FUNCTION with no declaration open" FFI-T-RECORD

   s" FFI-T-RAW ( ^a n n -- n ) ffi-call-n" FFI-T-CHECK-REJECTS
   s" FFI-T-RAW-K ( n n -- n ) ffi-call-1" FFI-T-CHECK-REJECTS
   s" FFI-T-ROW ( n n -- n ) FFI:CALL-1" FFI-T-CHECK-REJECTS
   s" FFI-T-STAGE ( n n -- ) FFI:VALUE!" FFI-T-CHECK-REJECTS
   s" FFI-T-RAW-ABI ( ^a ^a ^a n n -- n ) ffi-call-abi" FFI-T-CHECK-REJECTS
   s" FFI-T-MULTI ( ^u8 n -- n n ) FFI:DLOPEN" FFI-T-CHECK-REJECTS
   s" FFI:" 0 search-wl 0= TTRUE
   s" CALL0" 0 search-wl 0= TTRUE

   HB-TARGET-MACOS? if
      s" a library path's spelling is judged by its file name, not its directories" T-LABEL
      [: s" /opt/build.so.cache/libx" FFI:SELECT-LIBRARY ;] 0 TTHROWSQ
      FFI:SELECT-PROCESS
   then
   s" a negative or over-long length is refused before a byte is copied or read" T-LABEL
   [: FFI-T-CSTR-SRC -1 FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-LENGTH TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-LENGTH TTHROWSQ
   [: s" abcdefgh" FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-CAPACITY TTHROWSQ
   1 REJECTION:QUIET +!
   [: FFI-T-CSTR-SRC -1 FFI:SELECT-LIBRARY ;] 70 TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:SELECT-LIBRARY ;] 70 TTHROWSQ
   [: FFI-T-GUARDED-PATH ;] 70 TTHROWSQ
   -1 REJECTION:QUIET +!
   s" a string holding a 0 byte is refused before it reaches C" T-LABEL
   0 FFI-T-CSTR-DST SPAN:FILL
   [: s\" a\zb" FFI-T-CSTR-DST FFI:CSTR drop ;] E-FFI-NUL TTHROWSQ
   FFI-T-CSTR-DST SPAN:$ drop c@ 0 T=                  \ a refused copy wrote nothing
   [: s\" lib\zz" FFI:SELECT-LIBRARY ;] E-FFI-NUL TTHROWSQ
   s" a version list opens the first version present, in the order written" T-LABEL
   FFI-T-LISTED-CALL
   s" a declaration through the versioned form resolves its symbol" T-LABEL
   FFI-T-VERSIONED-CALL
   FFI-T-VARARGS
   FFI-T-VARARG-KEY
   FFI-T-FLOAT-SPILL

   FFI-T-GROW-ALIAS
   FFI-T-LIBRARY-SHARED
   FFI-T-PATH-LENGTH
   FFI-T-FOREIGN-LIBRARY
   FFI-T-UNSELECTED
   FFI-T-LONG-FOREIGN-LIBRARY
   FFI-T-NO-LIB
   FFI-T-NO-VERSION
   FFI-T-HUGE-BASE
   FFI-T-HUGE-PATH
   FFI-T-GLOBAL ;

FFI-RUN

: FFI-T-STRUCT-LOAD ( -- )
   HB-TARGET-MACOS? HB-TARGET-LINUX-KERNEL? or if
      s" test/ffi-struct-result.f" required
      s" test/ffi-struct-arg.f" required
      s" test/ffi-struct-verify.f" required
   then ;
FFI-T-STRUCT-LOAD

: FFI-T-C-CALLS-LOAD ( -- )
   HB-TARGET-MACOS? HB-TARGET-LINUX? or if
      s" test/ffi-c-calls.f" required
   then ;
FFI-T-C-CALLS-LOAD

T-REPORT

;package
