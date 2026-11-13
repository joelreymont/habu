\ ffi-test.f - checked C-ABI FFI tests against libc.
\ Run: bin/hb --load src/lib/ffi-test.f

require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f         \ DEFTYPE - the ffi-dev / ffi-ctx test nominals
require src/lib/string.f               \ SB / CONTAINS? - the child's stderr is matched here
require src/lib/fmt.f                  \ the code the child prints is rendered from its name
require src/lib/process.f
require src/lib/process-argv.f         \ the growth case needs a child engine
require src/lib/le.f                   \ the spilled %n out-parameter is a C int
require src/lib/memory.f               \ the read-only page the %n format is copied into
require src/lib/engine-candidate.f
require src/lib/test/ending.f
require src/lib/test/guard-page.f      \ a path whose bytes end where memory does
require src/lib/ffi-callback.f         \ a struct argument held across a callback
require src/lib/task.f
require test/ffi-struct-fixture.f      \ the struct-argument callee
require test/e2e-tree.f                \ the struct clauses' load children
require tools/check-verify-core.f       \ LSP-CHECK:VERIFY-BYTES - the struct clauses' check

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

: FFI-T-LIB-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBC ;
: FFI-T-MATH-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if FFI-T-LIBSYSTEM exit then
   FFI-T-LIBM ;
: FFI-T-OPEN ( -- n )  FFI-T-LIB-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-SYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-LIB @ name FFI:DLSYM ;
: FFI-T-SYM$ ( ptr u8 n -- n )
   {: name:ptr nameu:n :}
   name nameu FFI-T-SYM-BUF FFI:CSTR {: sym:ptr :}
   FFI-T-LIB @ sym FFI:DLSYM ;
: FFI-T-OPEN-MATH ( -- n )  FFI-T-MATH-PATH FFI:NOW FFI:DLOPEN ;
: FFI-T-MSYM ( ptr u8 -- n ) {: name:ptr :}  FFI-T-MATH @ name FFI:DLSYM ;

DEFTYPE FFI-DEV
DEFTYPE FFI-CTX

\ No C function witnesses an x8 result on Linux, macOS fixed integers past x7, a
\ double register beside a stack double, or binary32 beside a double or in s7.

PROCESS-SYMBOLS
FUNCTION: FFI-T-STRLEN$ strlen ( ptr u8 -- n ) ;FUNCTION
FUNCTION: FFI-T-STRNCMP$ strncmp ( ptr u8 ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: FFI-T-GETPID$ getpid ( -- i32 ) ;FUNCTION
FUNCTION: FFI-T-CTX-CALL getpid ( n -- i32 ) ;FUNCTION

\ crc32 through the process: the engine does not link libz, so process-wide
\ resolution finds it exactly when a libz open made its symbols global.
FUNCTION: FFI-T-GLOBAL-PROBE crc32 ( n ptr u8 n -- n ) ;FUNCTION

\ A declaration with no result drops the machine return cell, so the word is
\ stack-neutral for its caller.
FUNCTION: FFI-T-VOID$ getpid ( -- ) ;FUNCTION

\ gcvt(double, int, char *) returns its output buffer. This real libc call
\ exercises a pointer result beside floating, integer and pointer arguments.
64 BUFFER: FFI-T-GCVT-BUF
FUNCTION: FFI-T-GCVT gcvt ( r n ptr u8 -- ptr u8 )
   2 64 WRITES-BYTES
;FUNCTION

\ Resolution happens inside the call, after the arguments are staged: DLSYM
\ marshals through its own buffer, so a symbol resolved for the first time here
\ cannot overwrite the pending argument. Call this alias before FFI-T-STRLEN$ so
\ the shared descriptor has not yet resolved either.
FUNCTION: FFI-T-STRLEN-LATE strlen ( ptr u8 -- n ) ;FUNCTION

\ The writable form: memcpy's destination is written and its extent is the
\ third argument, so the bounded call guards exactly that span.
FUNCTION: FFI-T-MEMCPY memcpy ( ptr u8 ptr u8 n -- n )
   0 2 WRITES-ARG
;FUNCTION

\ A C int result fills only the low half of the return register, and the half
\ above it is whatever the callee left there. close(-1) fails with -1 in every
\ libc: `i32` must read it as -1, never $FFFFFFFF, and the same result read
\ through `u32` must be $FFFFFFFF, never -1.
FUNCTION: FFI-T-CLOSE close ( n -- i32 ) ;FUNCTION
FUNCTION: FFI-T-CLOSE-U32 close ( n -- u32 ) ;FUNCTION
FUNCTION: FFI-T-ATOI atoi ( ptr u8 -- i32 ) ;FUNCTION

\ Nominal-input fixture proves ABI-cell identity does not erase a role: the
\ conversion is visible at this call site and the checker keeps ffi-ctx apart
\ from ffi-dev.
: FFI-T-CTX-SET ( ffi-ctx -- rc )
   FFI-CTX>N FFI-T-CTX-CALL >RC ;

\ The declarer's contract to the loader's closed evaluate: a declaration
\ defines words and leaves the stack exactly as it found it.
variable FFI-T-DEPTH-BEFORE
variable FFI-T-DEPTH-AFTER

: FFI-T-DEPTH! ( ptr n -- ) depth swap ! ;

FFI-T-DEPTH-BEFORE FFI-T-DEPTH!
FUNCTION: FFI-T-DEPTH-PROBE getpid ( -- i32 ) ;FUNCTION
FFI-T-DEPTH-AFTER FFI-T-DEPTH!

\ Declarations the declarer must refuse. Each rides INCLUDE-EVALUATE so the
\ refusal is the throw a file-level declaration would raise, and each names one
\ structural rule: a pointer without its byte pointee, two results, an extent
\ that is not a positive width, an extent argument that is not a value, an
\ extent index past the arity, and more registers than the ABI call packs.
: FFI-T-BAD-PTR ( -- )
   s" FUNCTION: FFI-T-X1 getpid ( ptr -- n ) ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-RESULTS ( -- )
   s" FUNCTION: FFI-T-X2 getpid ( -- n n ) ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-EXTENT ( -- )
   s" FUNCTION: FFI-T-X3 getpid ( ptr u8 -- n ) 0 0 WRITES-BYTES ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-EXTENT-ARG ( -- )
   s" FUNCTION: FFI-T-X4 getpid ( ptr u8 ptr u8 -- n ) 0 1 WRITES-ARG ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-EXTENT-INDEX ( -- )
   s" FUNCTION: FFI-T-X5 getpid ( ptr u8 -- n ) 3 $10 WRITES-BYTES ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-FLOAT-ARITY ( -- )
   s" FUNCTION: FFI-T-X6 getpid ( r r r r r r r r r -- r ) ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-F32-ARITY ( -- )
   s" FUNCTION: FFI-T-XF getpid ( f32 f32 f32 f32 f32 f32 f32 f32 f32 -- f32 ) ;FUNCTION" INCLUDE-EVALUATE ;

\ A version list with an empty piece or a piece that is not a number.
: FFI-T-LIST-END ( -- )
   s" VERSIONED-LIBRARY z 1," INCLUDE-EVALUATE ;
: FFI-T-LIST-START ( -- )
   s" VERSIONED-LIBRARY z ,1" INCLUDE-EVALUATE ;
: FFI-T-LIST-GAP ( -- )
   s" VERSIONED-LIBRARY z 1,,2" INCLUDE-EVALUATE ;
: FFI-T-LIST-WORD ( -- )
   s" VERSIONED-LIBRARY z 1,x" INCLUDE-EVALUATE ;
: FFI-T-BAD-F32-VARARG ( -- )
   s" FUNCTION: FFI-T-XP getpid ( n f32 -- f32 ) 1 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-BAD-VARARG ( -- )
   s" FUNCTION: FFI-T-XV getpid ( n -- n ) 2 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-NO-FIXED ( -- )
   s" FUNCTION: FFI-T-XZ getpid ( n -- n ) 0 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;

\ Promotion reaches only the variadic tail: a fixed f32 before it is admitted.
: FFI-T-F32-FIXED ( -- )
   s" FUNCTION: FFI-T-XQ getpid ( f32 n r -- f32 ) 2 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;

\ Declaration-only shapes: Cairo's exact signature must load without requiring
\ Cairo, and Darwin's variadic stack routing must admit a pointer result. These
\ words never call their symbols; gcvt below witnesses the mixed pointer call
\ and FFI-T-VARARGS the variadic stack staging.
: FFI-T-PTR-MIXED ( -- )
   s" FUNCTION: FFI-T-XM cairo_pdf_surface_create ( ptr u8 r r -- ptr u8 ) ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-PTR-VARARG ( -- )
   s" FUNCTION: FFI-T-XW habu-pointer-variadic ( ptr u8 r n ptr u8 r -- ptr u8 ) 1 VARIADIC ;FUNCTION"
   INCLUDE-EVALUATE ;

\ A library selection belongs to the scope that states it, and there is no
\ default: a declaration in a scope that never stated one is refused by name,
\ so a file cannot inherit the library the previously loaded file selected.
\ Both cases run in this package's PUBLIC section, a wordlist the file's own
\ PROCESS-SYMBOLS - stated in the private section - never selected for. A
\ package of its own would say the same thing, but inside FFI-TEST a `package`
\ opens a part of FFI-TEST, not a package of its own.
variable FFI-T-SCOPE-BEFORE
variable FFI-T-SCOPE-AFTER

: FFI-T-NO-LIBRARY ( -- )
   s" public FUNCTION: FFI-T-XC getpid ( -- n ) ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-PRIVATE-AGAIN ( -- )
   s" private" INCLUDE-EVALUATE ;
\ The scoped selection above is the public section's, so this file's own scope
\ states its selection again before the fixtures that follow declare in it.
: FFI-T-RESELECT ( -- )
   s" private PROCESS-SYMBOLS" INCLUDE-EVALUATE ;
: FFI-T-SCOPED-LIBRARY ( -- )
   s" public PROCESS-SYMBOLS FUNCTION: FFI-T-XD getpid ( -- n ) ;FUNCTION private"
   INCLUDE-EVALUATE ;

\ A refused declaration is ABANDONED, not left half open. The refusal below
\ names an argument the effect does not have; the good declaration after it must
\ still compile, and the pair must leave the stack where it found it. Without
\ that the next declaration in a file refuses too and the first diagnostic names
\ the wrong one.
variable FFI-T-PAIR-BEFORE
variable FFI-T-PAIR-AFTER

: FFI-T-REFUSED-CLAUSE ( -- )
   s" FUNCTION: FFI-T-XA getpid ( ptr u8 -- n ) 9 $10 WRITES-BYTES ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-GOOD-AFTER ( -- )
   s" FUNCTION: FFI-T-XB getpid ( -- n ) ;FUNCTION" INCLUDE-EVALUATE ;

\ A declaration that never closes defines nothing, and the next FUNCTION:
\ refuses because one is still open. Closing it by hand restores the declarer,
\ and a closer with nothing open is refused in turn.
: FFI-T-BAD-UNCLOSED ( -- )
   s" FUNCTION: FFI-T-X7 getpid ( -- n ) FUNCTION: FFI-T-X8 getpid ( -- n )"
   INCLUDE-EVALUATE ;
: FFI-T-CLOSE-DANGLING ( -- )
   s" ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-REDECLARE ( -- )
   s" FUNCTION: FFI-T-X9 getpid ( -- n ) ;FUNCTION" INCLUDE-EVALUATE ;

\ A child declares 270 distinct symbols and 270 library paths, past any fixed
\ table: an exact alias takes no row, paths keep their ids and later rows call.
$800 constant FFI-T-SRC-CAP
$4000 constant FFI-T-CAP
67 constant FFI-T-UNCAUGHT-RC             \ hb's exit status for an uncaught throw

FFI-T-SRC-CAP CODEGEN:BUFFER FFI-T-SRC

create FFI-T-OUT FFI-T-CAP allot
create FFI-T-ERR FFI-T-CAP allot
create FFI-T-LONG-PATH PATH-CAP allot

: FFI-T-SRC+ ( ptr u8 n -- )
   FFI-T-SRC CODEGEN:APPEND-STRING ;

: FFI-T-FILL-SRC+ ( -- )
   s\" require src/lib/ffi-abi.f\nrequire src/lib/codegen.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-BASE getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" : ZZ-PROBE ( ptr u8 n -- n ) FFI:PROCESS 0 FFI:DECLARE ;\n" FFI-T-SRC+
   s\" 128 CODEGEN:BUFFER ZZ-DECL\n: ZZ-FILL ( -- ) 270 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-DECL CODEGEN:RESET\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-OVER habu-ffi-fill-\" ZZ-DECL CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-DECL CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    s\"  ( -- n ) ;FUNCTION\" ZZ-DECL CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    ZZ-DECL CODEGEN:CONTENTS INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\"    s\" undefine ZZ-OVER\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\" loop ;\nZZ-FILL\ns\" habu-ffi-probe-before\" ZZ-PROBE constant ZZ-BEFORE\n" FFI-T-SRC+ ;

: FFI-T-LIBS-SRC+ ( -- )
   s\" 128 CODEGEN:BUFFER ZZ-PATH\nvariable ZZ-FIRST-LIB\nvariable ZZ-LAST-LIB\n" FFI-T-SRC+
   s\" : ZZ-PATH$ ( n -- ptr u8 n ) {: id:n :} ZZ-PATH CODEGEN:RESET\n" FFI-T-SRC+
   s\"    s\" /tmp/habu-ffi-path-missing-\" ZZ-PATH CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    id ZZ-PATH CODEGEN:APPEND-DECIMAL ZZ-PATH CODEGEN:CONTENTS ;\n" FFI-T-SRC+
   s\" : ZZ-LIBS ( -- ) 270 0 ?do i ZZ-PATH$ FFI:LIBRARY-PATH\n" FFI-T-SRC+
   s\"    i 0= if dup ZZ-FIRST-LIB ! then ZZ-LAST-LIB ! loop ;\nZZ-LIBS\n" FFI-T-SRC+
   s\" : ZZ-SYSTEM$ ( -- ptr u8 n ) HB-TARGET-MACOS? if s\" /usr/lib/libSystem.B.dylib\" exit then\n" FFI-T-SRC+
   s\"    s\" libc.so.6\" ;\n" FFI-T-SRC+
   s\" s\" getpid\" ZZ-SYSTEM$ FFI:LIBRARY-PATH 0 FFI:DECLARE constant ZZ-LIB-ROW\n" FFI-T-SRC+
   s\" : ZZ-LIB-CHECK ( -- )\n" FFI-T-SRC+
   s\"    0 ZZ-PATH$ FFI:LIBRARY-PATH ZZ-FIRST-LIB @ <> if s\" early library\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-LAST-LIB @ ZZ-FIRST-LIB @ <= if s\" library ids\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-LIB-ROW FFI:CALL 0 <= if s\" late library call\" 1 die then ;\n" FFI-T-SRC+ ;

: FFI-T-GROW-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   FFI-T-FILL-SRC+
   s\" FUNCTION: ZZ-ALIAS getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" s\" habu-ffi-probe-after\" ZZ-PROBE constant ZZ-AFTER\n" FFI-T-SRC+
   FFI-T-LIBS-SRC+
   s\" : ZZ-CHECK ( -- ) ZZ-BEFORE 1+ ZZ-AFTER <> if s\" alias took row\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-BASE 0 <= ZZ-ALIAS 0 <= or if s\" getpid refused\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-LIB-CHECK\n" FFI-T-SRC+
   s\"    s\" growth-alias:ok\" type cr ;\nZZ-CHECK\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;
: FFI-T-ERR$ ( len -- ptr u8 n ) {: u:len :}
   FFI-T-ERR u LEN>N ;

\ Runs the source on a fresh engine's stdin and asserts its exit code.
: FFI-T-RUN-EXITS ( ptr u8 n n -- len len ) {: src:ptr srcu:n want:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN src srcu >LEN
   FFI-T-OUT FFI-T-CAP >LEN FFI-T-ERR FFI-T-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   src srcu FFI-T-OUT outu LEN>N FFI-T-ERR erru LEN>N oc want T-ENDING-EXITED=
   outu erru ;

\ The line the child prints for an uncaught code, rendered from the name so the
\ needle follows src/lib/error.f instead of repeating its number.
: FFI-T-CODE$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

: FFI-T-GROW-ALIAS ( -- )
   s" descriptors and library paths grow past 256 and an alias keeps its row" T-LABEL
   FFI-T-GROW-SRC$ 0 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   FFI-T-OUT outu LEN>N s\" growth-alias:ok\n" T$=
   erru LEN>N 0 T= ;
: FFI-T-LIBRARY-SHARED-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nrequire src/lib/codegen.f\n" FFI-T-SRC+
   s\" 128 CODEGEN:BUFFER ZZ-NAME\npackage ZZ-LIB-A\npublic\n" FFI-T-SRC+
   s\" : SELECT-DISTINCT ( -- ) 10 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:RESET s\" /tmp/habu-ffi-missing-\" ZZ-NAME CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-NAME CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:CONTENTS FFI:SELECT-LIBRARY\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-MISSING habu-missing ( -- n ) ;FUNCTION\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\"    s\" undefine ZZ-MISSING\" INCLUDE-EVALUATE\n" FFI-T-SRC+
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
   s\" \" INCLUDE-EVALUATE loop ;\nSELECT-REPEATED\n" FFI-T-SRC+
   s\" FUNCTION: PID-B getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" : RUN ( -- ) ZZ-LIB-A:PID-A 0 > PID-B 0 > and if s\" shared-library:ok\" type cr else s\" getpid refused\" 1 die then ;\nRUN\n;package\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LIBRARY-SHARED ( -- )
   s" repeated library selections share a callable binding" T-LABEL
   FFI-T-LIBRARY-SHARED-SRC$ 0 FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   FFI-T-OUT outu LEN>N s\" shared-library:ok\n" T$=
   erru LEN>N 0 T= ;

\ The overflowing path carries no extension: it stands for a literal LIBRARY row
\ on either target, which the spelling rule below leaves alone.
: FFI-T-LONG-PATH! ( -- )
   PATH-CAP 0 ?do [char] a FFI-T-LONG-PATH i + c! loop ;

: FFI-T-PATH-IDENTITY ( -- )
   s" different path bytes use different rows" T-LABEL
   s" /tmp/ffi-path/libx.so.1" FFI:LIBRARY-PATH {: a:n :}
   s" /tmp/ffi-path/libx.so.1" FFI:LIBRARY-PATH a T=
   s" /tmp/ffi-path/libx.so.2" FFI:LIBRARY-PATH a T<>
   s" /tmp/ffi-path/libX.so.1" FFI:LIBRARY-PATH a T<>
   s" /tmp/ffi-other/libx.so.1" FFI:LIBRARY-PATH a T<>
   s" path length is exact from one through PATH-CAP" T-LABEL
   [: FFI-T-LONG-PATH 0 FFI:LIBRARY-PATH drop ;] E-FFI-SYNTAX TTHROWSQ
   FFI-T-LONG-PATH!
   FFI-T-LONG-PATH PATH-CAP FFI:LIBRARY-PATH {: long:n :}
   FFI-T-LONG-PATH PATH-CAP FFI:LIBRARY-PATH long T=
   [: FFI-T-LONG-PATH PATH-CAP 1+ FFI:LIBRARY-PATH drop ;]
   E-FFI-SYNTAX TTHROWSQ ;

: FFI-T-LONG-SRC! ( -- )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\ncreate ZZ-P PATH-CAP allot\n" FFI-T-SRC+
   s\" : ZZ-LONG ( -- ptr u8 n ) PATH-CAP 0 ?do [char] a ZZ-P i + c! loop ZZ-P PATH-CAP ;\n" FFI-T-SRC+ ;

\ libz, which the VERSIONED-LIBRARY row below opens with FFI:NOW alone.
\ dlopen(3) names the scope its symbols join: on Linux an open without
\ RTLD_GLOBAL is RTLD_LOCAL, and RTLD_DEFAULT - what PROCESS-SYMBOLS resolves
\ through - misses it; macOS defaults to RTLD_GLOBAL when an open names neither.
\ The library's own handle finds crc32 either way, so a miss through the
\ process is the scope and not the library. The second open promotes the
\ already loaded library, as dlopen(3) does for a reopen with RTLD_GLOBAL.
: FFI-T-GLOBAL-LIB$ ( -- ptr u8 )
   HB-TARGET-MACOS? if s\" libz.1.dylib\z" drop exit then s\" libz.so.1\z" drop ;

: FFI-T-GLOBAL ( -- )
   s" an FFI:NOW open finds the function through its own handle" T-LABEL
   FFI-T-GLOBAL-LIB$ FFI:NOW FFI:DLOPEN {: h:n :}
   h 0 T<>
   h s\" crc32\z" drop FFI:DLSYM 0 T<>
   HB-TARGET-MACOS? if
      s" macOS: an FFI:NOW open is global, so the process resolves it" T-LABEL
      0 s" a" FFI-T-GLOBAL-PROBE $E8B7BE43 T=
   else
      s" Linux: an FFI:NOW open is local, so the process does not resolve it" T-LABEL
      [: 0 s" a" FFI-T-GLOBAL-PROBE drop ;] E-FFI-DLSYM TTHROWSQ
   then
   s" an FFI:NOW FFI:GLOBAL or open puts the function in process-wide resolution" T-LABEL
   FFI-T-GLOBAL-LIB$ FFI:NOW FFI:GLOBAL or FFI:DLOPEN 0 T<>
   0 s" a" FFI-T-GLOBAL-PROBE $E8B7BE43 T= ;

: FFI-T-CHECK-PASSES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: FFI-T-CHECK-REJECTS ( ptr u8 n -- )
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
FUNCTION: FFI-T-PRINTF snprintf ( ptr u8 n ptr u8 n r ptr u8 -- i32 )
   3 VARIADIC
   0 64 WRITES-BYTES
;FUNCTION
FUNCTION: FFI-T-SNPRINT-SHORT snprintf ( ptr u8 n ptr u8 n -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
;FUNCTION
FUNCTION: FFI-T-SNPRINT-LONG snprintf ( ptr u8 n ptr u8 n n n n n n n -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
;FUNCTION
FUNCTION: FFI-T-PRINTF-INTS snprintf ( ptr u8 n ptr u8 n n n n n n n n -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
;FUNCTION
FUNCTION: FFI-T-PRINTF-MIXED snprintf ( ptr u8 n ptr u8 n n n n r n -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
;FUNCTION
FUNCTION: FFI-T-PRINTF-COUNT snprintf ( ptr u8 n ptr u8 n n n ptr u8 -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
   6 4 WRITES-BYTES
;FUNCTION
FUNCTION: FFI-T-MPROTECT mprotect ( ptr u8 n n -- i32 ) ;FUNCTION
1 constant FFI-T-PROT-READ
64 BUFFER: FFI-T-PRINT-BUF
4 BUFFER: FFI-T-COUNT

\ macOS's printf aborts on a %n whose format lies in writable memory, and a
\ literal's bytes live in DATA (src/habu/intern.f), so the %n format
\ is copied into a fresh mapping and the page holding it made read-only for
\ the call.
: FFI-T-PRINTF-COUNTED ( -- n )
   MEM:ALLOC-64K {: fmt:ptr fmtu :}
   s\" %ld %ld %ld%n\z" {: src:ptr srcu:n :}
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

\ SysV's ninth float spills after eight XMM registers. The declaration is
\ target-conditional because the AAPCS64 planner keeps an eight-float limit.
: FFI-T-F9-FORMAT ( -- ptr u8 )
   s\" %.0f %.0f %.0f %.0f %.0f %.0f %.0f %.0f %.0f\z" drop ;
: FFI-T-F9-WANT ( -- ptr u8 n )
   s" 1 2 3 4 5 6 7 8 9" ;
: FFI-T-X86-FLOAT-SPILL ( -- )
   HB-TARGET-LINUX-X86-64? 0= if exit then
   s" FUNCTION: FFI-T-PRINTF-F9 snprintf ( ptr u8 n ptr u8 r r r r r r r r r -- i32 ) 3 VARIADIC 0 1 WRITES-ARG ;FUNCTION"
      INCLUDE-EVALUATE
   FFI-T-SRC CODEGEN:RESET
   s" : FFI-T-F9-CHECK ( -- ) FFI-T-PRINT-BUF 64 FFI-T-F9-FORMAT"
      FFI-T-SRC+
   s"  1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 FFI-T-PRINTF-F9"
      FFI-T-SRC+
   s"  FFI-T-PRINT-BUF swap FFI-T-F9-WANT T$= ; FFI-T-F9-CHECK"
      FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS INCLUDE-EVALUATE ;

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
FUNCTION: FFI-T-VERSIONED-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION
PROCESS-SYMBOLS

: FFI-T-VERSIONED-CALL ( -- )
   0 s" a" FFI-T-VERSIONED-CRC $E8B7BE43 T= ;

\ A version list: no libz 9999 exists on either target, so the first call is
\ served by the second version written.
VERSIONED-LIBRARY z 9999,1
FUNCTION: FFI-T-LISTED-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION
PROCESS-SYMBOLS

: FFI-T-LISTED-CALL ( -- )
   0 s" a" FFI-T-LISTED-CRC $E8B7BE43 T= ;

\ The rendered name itself, byte for byte on this target.
FFI:LIBRARY-PATH-CAP CODEGEN:BUFFER FFI-T-NAME-A
FFI:LIBRARY-PATH-CAP CODEGEN:BUFFER FFI-T-NAME-B

: FFI-T-PQ-NAME$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libpq.5.dylib" exit then s" libpq.so.5" ;

\ A base name past LIB-BASE-CAP: refused by name, never rendered short.
: FFI-T-LONG-BASE ( -- )
   s" aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 5 FFI-T-NAME-A FFI:LIBRARY-NAME$ 2drop ;


\ A path one byte longer than the table holds, whose bytes end at an
\ inaccessible page: the length is refused before the spelling check reads it.
: FFI-T-GUARDED-PATH ( -- )
   FFI:LIBRARY-PATH-CAP [char] a GUARD-PAGE:TAIL
   FFI:LIBRARY-PATH-CAP 1+ FFI:SELECT-LIBRARY ;

: FFI-T-GUARDED-DIRECT ( -- )
   PATH-CAP [char] a GUARD-PAGE:TAIL
   PATH-CAP 1+ FFI:LIBRARY-PATH drop ;

\ A literal spelled in the OTHER target's convention names no file here, so the
\ declarer refuses it where it is stated. The refusal ends the load, which is
\ why it is measured in a child like the growth case.
: FFI-T-FOREIGN-NAME$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libx.so.5" exit then s" libx.5.dylib" ;

: FFI-T-TARGET$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" macos" exit then s" linux" ;

: FFI-T-FOREIGN-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\n" FFI-T-SRC+
   s\" : ZZ-FOREIGN ( -- ) s\" LIBRARY " FFI-T-SRC+
   FFI-T-FOREIGN-NAME$ FFI-T-SRC+
   s\" \" INCLUDE-EVALUATE ;\nZZ-FOREIGN\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-FOREIGN-LIBRARY ( -- )
   s" a library name spelled for the other target is refused by name" T-LABEL
   FFI-T-FOREIGN-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-LIBRARY FFI-T-CODE$ CONTAINS? TTRUE
   s" the refusal names the rejected name and this target" T-LABEL
   erru FFI-T-ERR$ FFI-T-FOREIGN-NAME$ CONTAINS? TTRUE
   erru FFI-T-ERR$ FFI-T-TARGET$ CONTAINS? TTRUE ;

: FFI-T-LONG-FOREIGN-SRC$ ( -- ptr u8 n )
   FFI-T-LONG-SRC!
   s\" : ZZ-FOREIGN ( -- ) ZZ-LONG {: path:ptr u:n :}\n" FFI-T-SRC+
   s\" HB-TARGET-MACOS? if s\" .so\" else s\" .dylib\" then {: ext:ptr eu:n :}\n" FFI-T-SRC+
   s\" ext path u eu - + eu BYTE-COPY path u FFI:SELECT-LIBRARY ;\nZZ-FOREIGN\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LONG-FOREIGN-LIBRARY ( -- )
   s" a PATH-CAP foreign name keeps the target refusal and its full name" T-LABEL
   FFI-T-LONG-FOREIGN-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-LIBRARY FFI-T-CODE$ CONTAINS? TTRUE
   FFI-T-LONG-PATH!
   HB-TARGET-MACOS? if s" .so" else s" .dylib" then {: ext:ptr eu:n :}
   ext FFI-T-LONG-PATH PATH-CAP eu - + eu BYTE-COPY
   erru FFI-T-ERR$ FFI-T-LONG-PATH PATH-CAP CONTAINS? TTRUE ;

: FFI-T-NO-LIB$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libhabu-no-such-library.dylib" exit then
   s" libhabu-no-such-library.so" ;

: FFI-T-LIBC$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" /usr/lib/libSystem.B.dylib" exit then
   s" libc.so.6" ;

\ Missing symbols and a missing library fail at the first call, never at the
\ declaration; the final uncaught error ends the load, so the case runs in a child.
: FFI-T-NO-LIB-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-ABSENT habu-no-such-symbol ( -- n ) ;FUNCTION\nLIBRARY " FFI-T-SRC+
   FFI-T-NO-LIB$ FFI-T-SRC+
   s\" \nFUNCTION: ZZ-NO-LIB getpid ( -- n ) ;FUNCTION\nLIBRARY " FFI-T-SRC+
   FFI-T-LIBC$ FFI-T-SRC+
   s\" \nFUNCTION: ZZ-ABSENT-IN habu-no-such-symbol ( -- n ) ;FUNCTION\n: ZZ-CALL ( -- )\n" FFI-T-SRC+
   s\"    [: ZZ-ABSENT drop ;] catch E-FFI-DLSYM <> if s\" absent\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-ABSENT-IN drop ;] catch E-FFI-DLSYM <> if s\" absent in\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-LIB drop ;] catch E-FFI-DLOPEN <> if s\" no lib\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-LIB drop ;] catch E-FFI-DLOPEN <> if s\" no lib again\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-NO-LIB drop ;\nZZ-CALL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-NO-LIB ( -- )
   s" a missing library fails at the first call with its path and reason, once" T-LABEL
   FFI-T-NO-LIB-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-DLOPEN FFI-T-CODE$ CONTAINS? TTRUE
   SB-RESET s" E-FFI-DLOPEN: " SB-APPEND FFI-T-NO-LIB$ SB-APPEND s" : " SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   s" a missing process symbol fails with its name" T-LABEL
   erru FFI-T-ERR$ S\" E-FFI-DLSYM: habu-no-such-symbol\n" CONTAINS? TTRUE
   s" a missing library symbol fails with its name and the library's path" T-LABEL
   SB-RESET s" E-FFI-DLSYM: habu-no-such-symbol in " SB-APPEND FFI-T-LIBC$ SB-APPEND
   S\" \n" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE ;

\ No version of the list exists, so the first call reports every name and a
\ second call nothing; the final uncaught error ends the load, so it runs in a child.
\ A symbol missing from a list whose second version opens names that version.
: FFI-T-NO-VERSION-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 9999,1\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-LISTED-ABSENT habu-no-such-symbol ( -- n ) ;FUNCTION\n" FFI-T-SRC+
   s\" VERSIONED-LIBRARY habu-no-such 2,1\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-NO-VERSION getpid ( -- n ) ;FUNCTION\n: ZZ-CALL ( -- )\n" FFI-T-SRC+
   s\"    [: ZZ-LISTED-ABSENT drop ;] catch E-FFI-DLSYM <> if s\" listed absent\" 1 die then\n" FFI-T-SRC+
   s\"    [: ZZ-NO-VERSION drop ;] catch E-FFI-DLOPEN <> if s\" no version\" 1 die then\n" FFI-T-SRC+
   s\"    ZZ-NO-VERSION drop ;\nZZ-CALL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-NO-VERSION ( -- )
   s" a version list with none present names each version in order, once" T-LABEL
   FFI-T-NO-VERSION-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-DLOPEN FFI-T-CODE$ CONTAINS? TTRUE
   s" habu-no-such" 2 FFI-T-NAME-A FFI:LIBRARY-NAME$ {: a:ptr au:n :}
   s" habu-no-such" 1 FFI-T-NAME-B FFI:LIBRARY-NAME$ {: b:ptr bu:n :}
   \ The first name carries no reason; the last one tried carries the loader's.
   SB-RESET s" E-FFI-DLOPEN: " SB-APPEND a au SB-APPEND s\" \nE-FFI-DLOPEN: " SB-APPEND
   b bu SB-APPEND s" : " SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   SB-RESET s" E-FFI-DLOPEN: " SB-APPEND a au SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   SB-RESET s" E-FFI-DLOPEN: " SB-APPEND b bu SB-APPEND
   erru FFI-T-ERR$ SB$ COUNT-SUB 1 T=
   s" a missing symbol names the version that opened, never one that did not" T-LABEL
   s" z" 1 FFI-T-NAME-A FFI:LIBRARY-NAME$ {: z1:ptr z1u:n :}
   s" z" 9999 FFI-T-NAME-B FFI:LIBRARY-NAME$ {: z9:ptr z9u:n :}
   SB-RESET s" E-FFI-DLSYM: habu-no-such-symbol in " SB-APPEND z1 z1u SB-APPEND
   S\" \n" SB-APPEND
   erru FFI-T-ERR$ SB$ CONTAINS? TTRUE
   erru FFI-T-ERR$ z9 z9u CONTAINS? TFALSE ;

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

   FFI-T-GETPID$ 0 T<>
   FFI-T-VOID$  FFI-T-GETPID$ 0 T<>               \ void-return wrapper is stack-neutral

   s" a mixed floating call returns the output buffer and writes its value" T-LABEL
   72.5 5 FFI-T-GCVT-BUF FFI-T-GCVT {: converted:ptr :}
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

   [: FFI-T-BAD-PTR ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-RESULTS ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT-ARG ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT-INDEX ;] E-FFI-SYNTAX TTHROWSQ
   \ A ninth C float has no stack staging on either ABI.
   HB-TARGET-LINUX-X86-64? if
      [: FFI-T-BAD-FLOAT-ARITY ;] 0 TTHROWSQ
   else
      [: FFI-T-BAD-FLOAT-ARITY ;] E-FFI-ARITY TTHROWSQ
   then
   [: FFI-T-BAD-F32-ARITY ;] E-FFI-ARITY TTHROWSQ
   [: FFI-T-BAD-F32-VARARG ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-F32-FIXED ;] 0 TTHROWSQ
   [: FFI-T-PTR-MIXED ;] 0 TTHROWSQ
   [: FFI-T-PTR-VARARG ;] 0 TTHROWSQ
   [: FFI-T-BAD-VARARG ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-NO-FIXED ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-LIST-END ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-LIST-START ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-LIST-GAP ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-LIST-WORD ;] E-FFI-SYNTAX TTHROWSQ
   s" a library selection is scoped to the scope that states it" T-LABEL
   FFI-T-SCOPE-BEFORE FFI-T-DEPTH!
   [: FFI-T-NO-LIBRARY ;] E-FFI-LIBRARY TTHROWSQ
   FFI-T-PRIVATE-AGAIN
   [: FFI-T-SCOPED-LIBRARY ;] 0 TTHROWSQ
   FFI-T-RESELECT
   FFI-T-SCOPE-AFTER FFI-T-DEPTH!
   FFI-T-SCOPE-AFTER @ FFI-T-SCOPE-BEFORE @ T=

   s" a refused declaration is abandoned, not left open" T-LABEL
   FFI-T-PAIR-BEFORE FFI-T-DEPTH!
   [: FFI-T-REFUSED-CLAUSE ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-GOOD-AFTER ;] 0 TTHROWSQ
   FFI-T-PAIR-AFTER FFI-T-DEPTH!
   FFI-T-PAIR-AFTER @ FFI-T-PAIR-BEFORE @ T=

   [: FFI-T-BAD-UNCLOSED ;] E-FFI-SYNTAX TTHROWSQ
   FFI-T-CLOSE-DANGLING
   [: FFI-T-REDECLARE ;] 0 TTHROWSQ
   [: FFI-T-CLOSE-DANGLING ;] E-FFI-SYNTAX TTHROWSQ

   s" FFI-T-RAW ( ptr a n n -- n ) ffi-call-n" FFI-T-CHECK-REJECTS
   s" FFI-T-RAW-ABI ( ptr a ptr a ptr a n n -- n ) ffi-call-abi" FFI-T-CHECK-REJECTS
   s" FFI-T-LIE ( n -- ) 0 FFI:POINTER!" FFI-T-CHECK-REJECTS
   s" FFI-T-MULTI ( ptr u8 n -- n n ) FFI:DLOPEN" FFI-T-CHECK-REJECTS
   s" FFI:" 0 search-wl 0= TTRUE
   s" CALL0" 0 search-wl 0= TTRUE

   s" a library name renders this target's soname spelling" T-LABEL
   s" pq" 5 FFI-T-NAME-A FFI:LIBRARY-NAME$
   s" z" 1 FFI-T-NAME-B FFI:LIBRARY-NAME$ 2drop
   FFI-T-PQ-NAME$ T$=
   HB-TARGET-MACOS? if s" /opt/build.so.cache/libx.1.dylib"
   else s" /opt/build.dylib/libx.so.1" then FFI:SELECT-LIBRARY
   FFI:SELECT-PROCESS
   [: FFI-T-LONG-BASE ;] E-FFI-SYNTAX TTHROWSQ
   s" a negative or over-long length is refused before a byte is copied or read" T-LABEL
   [: FFI-T-CSTR-SRC -1 FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-LENGTH TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-LENGTH TTHROWSQ
   [: s" abcdefgh" FFI-T-CSTR-DST FFI:CSTR drop ;] E-SPAN-CAPACITY TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI:LIBRARY-PATH drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:LIBRARY-PATH drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI:PROCESS 0 FFI:DECLARE drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:PROCESS 0 FFI:DECLARE drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI:SELECT-LIBRARY ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:SELECT-LIBRARY ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-GUARDED-PATH ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-GUARDED-DIRECT ;] E-FFI-SYNTAX TTHROWSQ
   s" a string holding a 0 byte is refused before it reaches C" T-LABEL
   0 FFI-T-CSTR-DST SPAN:FILL
   [: s\" a\zb" FFI-T-CSTR-DST FFI:CSTR drop ;] E-FFI-NUL TTHROWSQ
   FFI-T-CSTR-DST SPAN:$ drop c@ 0 T=                  \ a refused copy wrote nothing
   \ A refused path or name adds no row; a declared symbol followed by a 0 byte
   \ is a different name, not that symbol's row.
   s" /tmp/habu-ffi-nul-before" FFI:LIBRARY-PATH {: lib-before:n :}
   [: s\" libz\z.so" FFI:LIBRARY-PATH drop ;] E-FFI-NUL TTHROWSQ
   s" /tmp/habu-ffi-nul-after" FFI:LIBRARY-PATH lib-before 1+ T=
   s" getpid" FFI:PROCESS 0 FFI:DECLARE drop
   s" habu-ffi-nul-before" FFI:PROCESS 0 FFI:DECLARE {: fn-before:n :}
   [: s\" getpid\z" FFI:PROCESS 0 FFI:DECLARE drop ;] E-FFI-NUL TTHROWSQ
   s" habu-ffi-nul-after" FFI:PROCESS 0 FFI:DECLARE fn-before 1+ T=
   s" a version list opens the first version present, in the order written" T-LABEL
   FFI-T-LISTED-CALL
   s" a declaration through the versioned form resolves its symbol" T-LABEL
   FFI-T-VERSIONED-CALL
   FFI-T-VARARGS
   FFI-T-VARARG-KEY
   FFI-T-X86-FLOAT-SPILL

   FFI-T-GROW-ALIAS
   FFI-T-LIBRARY-SHARED
   FFI-T-PATH-IDENTITY
   FFI-T-FOREIGN-LIBRARY
   FFI-T-LONG-FOREIGN-LIBRARY
   FFI-T-NO-LIB
   FFI-T-NO-VERSION
   FFI-T-GLOBAL ;

FFI-RUN

\ Structs by value are AAPCS64, results against macOS libraries and arguments
\ against a fixture library built for the run.
: FFI-T-STRUCT-LOAD ( -- )
   HB-TARGET-MACOS? if
      s" test/ffi-struct-result.f" required
      s" test/ffi-struct-arg.f" required
      s" test/ffi-struct-verify.f" required
   then ;
FFI-T-STRUCT-LOAD

T-REPORT

;package
