\ ffi-test.f - checked C-ABI FFI tests against libc.
\ Run: bin/hb --load lib/ffi-test.f

require lib/test.f
require lib/ffi-abi.f
require test/checker-assert.f
require lib/type/deftype.f         \ DEFTYPE - the ffi-dev / ffi-ctx test nominals
require lib/string.f               \ SB / CONTAINS? - the child's stderr is matched here
require lib/fmt.f                  \ the code the child prints is rendered from its name
require lib/process.f
require lib/process-argv.f         \ the full-table case needs a child engine
require lib/process-env.f          \ the stub child runs on the unsealed engine
require lib/fs-mutate.f            \ CLEANUP-RUN - that engine's private copy
require test/whitebox-child.f
require test/suite-budget.f        \ CHILD-MS, the stub child's hang guard
require lib/engine-candidate.f
require lib/test/outcome.f
require lib/test/guard-page.f      \ a path whose bytes end where memory does

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
create FFI-T-CSTR-DST 8 allot
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

DEFTYPE FFI-DEV
DEFTYPE FFI-CTX

\ The libc and libm bindings are FUNCTION: declarations: the declared effect is
\ the generated word's effect and decides every argument's staging. The staging
\ shapes no C function reproduces on both ABIs - two stack integers past x0-x7,
\ a float stack slot, an sret x8 output - run against fixed stubs in a window
\ child (FFI-T-STUBS below).

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

\ Resolution happens inside the call, after the arguments are staged: DLSYM
\ marshals through its own buffer, so a symbol resolved for the first time here
\ cannot overwrite the pending argument. This row's first call is that case.
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

\ Nominal-input fixture proves ABI-cell identity does not erase a role: the
\ conversion is visible at this call site and the checker keeps ffi-ctx apart
\ from ffi-dev.
: FFI-T-CTX-SET ( ffi-ctx -- rc )
   FFI-CTX>N FFI-T-CTX-CALL >RC ;

\ The declarer's contract to the loader's closed evaluate: a declaration
\ defines words and leaves the stack exactly as it found it. Measured across a
\ real declaration rather than asserted in a comment.
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
: FFI-T-BAD-VARARG ( -- )
   s" FUNCTION: FFI-T-XV getpid ( n -- n ) 2 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-NO-FIXED ( -- )
   s" FUNCTION: FFI-T-XZ getpid ( n -- n ) 0 VARIADIC ;FUNCTION" INCLUDE-EVALUATE ;
\ A library selection belongs to the scope that states it, and there is no
\ default: a declaration in a scope that never stated one is refused by name,
\ so a file cannot inherit the library the previously loaded file selected.
\ Both cases run in this package's PUBLIC section, a wordlist the file's own
\ PROCESS-SYMBOLS - stated in the private section - never selected for. A
\ package of its own would say the same thing, but `package` inside `package
\ FFI-TEST` is nesting and rejects.
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

\ A declared symbol that no library carries: the failure is named and lands at
\ the first call, never at the declaration.
FUNCTION: FFI-T-ABSENT habu-no-such-symbol ( -- n ) ;FUNCTION

\ ---- the declaration table's ceiling ---------------------------------------
\ The table belongs to the IMAGE, so its ceiling can only be reached by a
\ process that fills it: this case fills a child engine's. The loop declares and
\ undefines ONE name, so whichever turn runs out of rows, the refusal carries
\ that word and that symbol - and the symbol is one no library exports, which a
\ declaration is entitled to because it never resolves.
$400 constant FFI-T-SRC-CAP
$4000 constant FFI-T-CAP
30000 constant FFI-T-TIMEOUT-MS
67 constant FFI-T-UNCAUGHT-RC             \ hb's exit status for an uncaught throw

FFI-T-SRC-CAP CODEGEN:BUFFER FFI-T-SRC

create FFI-T-OUT FFI-T-CAP allot
create FFI-T-ERR FFI-T-CAP allot
create FFI-T-LONG-PATH PATH-CAP allot

: FFI-T-SRC+ ( ptr u8 n -- )
   FFI-T-SRC CODEGEN:APPEND-STRING ;

: FFI-T-FULL-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require lib/ffi-abi.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" : ZZ-FILL ( -- ) FFI:DECLARATION-MAX 1+ 0 ?do\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-OVER habu-ffi-table-probe ( -- n )" FFI-T-SRC+
   s\"  ;FUNCTION\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\"    s\" undefine ZZ-OVER\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\" loop ;\nZZ-FILL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-ERR$ ( len -- ptr u8 n ) {: u:len :}
   FFI-T-ERR u LEN>N ;

\ Runs the source on a fresh engine's stdin and asserts its exit code, leaving
\ the stdout and stderr lengths.
: FFI-T-RUN-EXITS ( ptr u8 n n -- len len ) {: src:ptr srcu:n want:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN src srcu >LEN
   FFI-T-OUT FFI-T-CAP >LEN FFI-T-ERR FFI-T-CAP >LEN
   FFI-T-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   src srcu FFI-T-OUT outu LEN>N FFI-T-ERR erru LEN>N oc want T-OUTCOME-EXITED=
   outu erru ;

\ The line the child prints for an uncaught code, rendered from the name so the
\ needle follows lib/errors.f instead of repeating its number.
: FFI-T-CODE$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

: FFI-T-TABLE-FULL ( -- )
   s" a full declaration table is refused with its own code" T-LABEL
   FFI-T-FULL-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-TABLE-FULL FFI-T-CODE$ CONTAINS? TTRUE
   s" the refusal names the symbol and the word being declared" T-LABEL
   erru FFI-T-ERR$ s" habu-ffi-table-probe" CONTAINS? TTRUE
   erru FFI-T-ERR$ s" ZZ-OVER" CONTAINS? TTRUE ;

: FFI-T-LIBRARY-SHARED-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require lib/ffi-abi.f\nrequire lib/codegen.f\n" FFI-T-SRC+
   s\" 128 CODEGEN:BUFFER ZZ-NAME\npackage ZZ-LIB-A\npublic\n" FFI-T-SRC+
   s\" : SELECT-DISTINCT ( -- ) 10 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:RESET s\" /tmp/habu-ffi-missing-\" ZZ-NAME CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-NAME CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:CONTENTS FFI-DECL:SELECT-LIBRARY\n" FFI-T-SRC+
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
: FFI-T-LIB-FULL-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require lib/ffi-abi.f\nrequire lib/codegen.f\nPROCESS-SYMBOLS\n" FFI-T-SRC+
   s\" FUNCTION: ZZ-EARLY getpid ( -- i32 ) ;FUNCTION\n" FFI-T-SRC+
   s\" 128 CODEGEN:BUFFER ZZ-NAME\n: ZZ-LIB-FILL ( -- ) FFI:LIBRARY-MAX 0 ?do\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:RESET s\" /tmp/habu-ffi-library-\" ZZ-NAME CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"    i ZZ-NAME CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"    ZZ-NAME CODEGEN:CONTENTS FFI-DECL:SELECT-LIBRARY\n" FFI-T-SRC+
   s\" loop\n" FFI-T-SRC+
   s\" s\" LIBRARY /tmp/habu-ffi-library-0\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\" s\" FUNCTION: ZZ-REUSED absent-symbol ( -- n ) ;FUNCTION\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\" ZZ-EARLY 0 <= if s\" prior call failed\" 1 die then\n" FFI-T-SRC+
   s\" s\" LIBRARY /tmp/habu-ffi-library-overflow\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\"    s\" FUNCTION: ZZ-LIB-OVER habu-ffi-library-probe ( -- n ) ;FUNCTION\" INCLUDE-EVALUATE\n" FFI-T-SRC+
   s\" ;\nZZ-LIB-FILL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LIBRARY-TABLE-FULL ( -- )
   s" a full library table names the path and pending declaration" T-LABEL
   FFI-T-LIB-FULL-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-LIBRARY-FULL FFI-T-CODE$ CONTAINS? TTRUE
   erru FFI-T-ERR$ s" /tmp/habu-ffi-library-overflow" CONTAINS? TTRUE
   erru FFI-T-ERR$ s" ZZ-LIB-OVER" CONTAINS? TTRUE
   erru FFI-T-ERR$ s" habu-ffi-library-probe" CONTAINS? TTRUE ;

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
   s\" require lib/ffi-abi.f\ncreate ZZ-P PATH-CAP allot\n" FFI-T-SRC+
   s\" : ZZ-LONG ( -- ptr u8 n ) PATH-CAP 0 ?do [char] a ZZ-P i + c! loop ZZ-P PATH-CAP ;\n" FFI-T-SRC+ ;

: FFI-T-LONG-LIB-FULL-SRC$ ( -- ptr u8 n )
   FFI-T-LONG-SRC!
   s\" require lib/codegen.f\n128 CODEGEN:BUFFER ZZ-NAME\n" FFI-T-SRC+
   s\" PROCESS-SYMBOLS\n: ZZ-LIB-FILL ( -- ) FFI:LIBRARY-MAX 0 ?do\n" FFI-T-SRC+
   s\"   ZZ-NAME CODEGEN:RESET s\" /tmp/habu-ffi-library-\" ZZ-NAME CODEGEN:APPEND-STRING\n" FFI-T-SRC+
   s\"   i ZZ-NAME CODEGEN:APPEND-DECIMAL\n" FFI-T-SRC+
   s\"   ZZ-NAME CODEGEN:CONTENTS FFI-DECL:SELECT-LIBRARY\n" FFI-T-SRC+
   s\" loop ZZ-LONG FFI-DECL:SELECT-LIBRARY\n" FFI-T-SRC+
   s\" s\" FUNCTION: ZZ-LIB-OVER habu-ffi-library-probe ( -- n ) ;FUNCTION\" INCLUDE-EVALUATE ;\nZZ-LIB-FILL\n" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

: FFI-T-LONG-LIBRARY-TABLE-FULL ( -- )
   s" a PATH-CAP path keeps the library-table refusal and its full name" T-LABEL
   FFI-T-LONG-LIB-FULL-SRC$ FFI-T-UNCAUGHT-RC FFI-T-RUN-EXITS
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru FFI-T-ERR$ E-FFI-LIBRARY-FULL FFI-T-CODE$ CONTAINS? TTRUE
   FFI-T-LONG-PATH!
   erru FFI-T-ERR$ FFI-T-LONG-PATH PATH-CAP CONTAINS? TTRUE ;

\ ---- process-wide symbols: FFI:GLOBAL --------------------------------------
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

\ The staging shapes no C function reproduces on both ABIs: ten integers, x0-x7
\ plus two stack cells; a float register beside a float stack slot; an sret
\ output in x8. A variadic callee is the only system witness for a stack spill,
\ and Apple's ABI puts every variadic argument on the stack, so it never reaches
\ the register exhaustion that spills. test/ffi-stub-child.f mints fixed stubs
\ for them and calls them through owner words test/mcode-window-prepare.f
\ opens, so it runs as a window child of test/native-window-owner-child.f.
\ Reopening the window is refused on the sealed product (`hb: internal engine
\ word`, exit 70), so the child runs on the engine test/whitebox-child.f names.
: FFI-T-WIN-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: FFI-T-STUB-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" FFI-T-WIN-ARG
   s" test/native-window-owner-child.f" FFI-T-WIN-ARG
   s" --" FFI-T-WIN-ARG
   s" test/ffi-stub-child.f" FFI-T-WIN-ARG
   s" src/core/declaration-transaction.f" FFI-T-WIN-ARG
   s" src/core/generated-declaration.f" FFI-T-WIN-ARG
   s" src/core/decl-event.f" FFI-T-WIN-ARG
   s" src/core/structure-make.f" FFI-T-WIN-ARG
   s" src/core/structure-decl.f" FFI-T-WIN-ARG
   s" src/core/enum-decl.f" FFI-T-WIN-ARG
   s" src/core/structures.f" FFI-T-WIN-ARG
   s" src/core/bytes.f" FFI-T-WIN-ARG
   s" src/core/dynamic-storage.f" FFI-T-WIN-ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" FFI-T-WIN-ARG
      s" src/os/linux/layout-constants.f" FFI-T-WIN-ARG
      s" src/os/linux/layout.f" FFI-T-WIN-ARG
   else
      s" src/os/macos/target.f" FFI-T-WIN-ARG s" src/os/macos/layout.f" FFI-T-WIN-ARG
   then
   s" src/habu/stack-abi.f" FFI-T-WIN-ARG
   s" src/habu/layout.f" FFI-T-WIN-ARG
   s" src/os/env-base.f" FFI-T-WIN-ARG
   s" src/core/include.f" FFI-T-WIN-ARG
   s" src/core/sha256.f" FFI-T-WIN-ARG
   s" src/habu/code-span.f" FFI-T-WIN-ARG
   s" test/mcode-window-prepare.f" FFI-T-WIN-ARG
   WHITEBOX-CHILD:ENV! ;

: FFI-T-STUB-RESULT ( -- )
   WHITEBOX-CHILD:ENGINE$ >LEN FFI-T-OUT FFI-T-CAP >LEN FFI-T-ERR FFI-T-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   FFI-T-OUT outu LEN>N S\" test: ok\nwindow: 0\n" STR= 0= rc 0 <> or
      if FFI-T-OUT outu LEN>N type erru FFI-T-ERR$ type cr then
   rc 0 T=
   FFI-T-OUT outu LEN>N S\" test: ok\nwindow: 0\n" T$= ;

: FFI-T-STUBS ( -- )
   [: s" ffi-stub" WHITEBOX-CHILD:PROVIDE FFI-T-STUB-ARGS FFI-T-STUB-RESULT ;]
   [: CLEANUP-RUN ;] finally ;

\ The libm rows ride the AAPCS64 call: sqrt a float argument and a float result,
\ fma three float registers d0-d2, scalbln a float in d0 beside all 64 bits of x0.
\ The library is chosen at load time because the two targets keep it in
\ different files, which is what the runtime-string form of LIBRARY is for.
: FFI-T-SELECT-MATH ( -- )
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib"
   else
      s" libm.so.6"
   then FFI-DECL:SELECT-LIBRARY ;

FFI-T-SELECT-MATH
FUNCTION: FFI-T-SQRT-CALL sqrt ( r -- r ) ;FUNCTION
FUNCTION: FFI-T-FMA-CALL fma ( r r r -- r ) ;FUNCTION
FUNCTION: FFI-T-SCALBLN-CALL scalbln ( r n -- r ) ;FUNCTION
PROCESS-SYMBOLS

\ The same declaration exercises Apple's stack varargs and Linux's registers.
FUNCTION: FFI-T-PRINTF snprintf ( ptr u8 n ptr u8 n r ptr u8 -- i32 )
   3 VARIADIC
   0 1 WRITES-ARG
;FUNCTION
64 BUFFER: FFI-T-PRINT-BUF
: FFI-T-VARARGS ( -- )
   FFI-T-PRINT-BUF 64 s\" %ld %.1f %s\z" drop 42 1.5 s\" ok\z" drop FFI-T-PRINTF
   FFI-T-PRINT-BUF swap s" 42 1.5 ok" T$= ;

\ A library selected by base name and soname version: libz.so.1 here and
\ libz.1.dylib on macOS both exist, so the symbol resolves through the rendered
\ selection at the first call on either target.
VERSIONED-LIBRARY z 1
FUNCTION: FFI-T-VERSIONED-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION
PROCESS-SYMBOLS

: FFI-T-VERSIONED-CALL ( -- )
   0 s" a" FFI-T-VERSIONED-CRC $E8B7BE43 T= ;

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
   FFI:LIBRARY-PATH-CAP 1+ FFI-DECL:SELECT-LIBRARY ;

: FFI-T-GUARDED-DIRECT ( -- )
   PATH-CAP [char] a GUARD-PAGE:TAIL
   PATH-CAP 1+ FFI:LIBRARY-PATH drop ;

\ A literal spelled in the OTHER target's convention names no file here, so the
\ declarer refuses it where it is stated. The refusal ends the load, which is
\ why it is measured in a child the way the table ceilings are.
: FFI-T-FOREIGN-NAME$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" libx.so.5" exit then s" libx.5.dylib" ;

: FFI-T-TARGET$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" macos" exit then s" linux" ;

: FFI-T-FOREIGN-SRC$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s\" require lib/ffi-abi.f\n" FFI-T-SRC+
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
   s\" ext path u eu - + eu BYTE-COPY path u FFI-DECL:SELECT-LIBRARY ;\nZZ-FOREIGN\n" FFI-T-SRC+
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

\ The declaration's hook stays armed: calls after a capture must have their
\ process-owned addresses forgotten by every later capture as well.
: FFI-T-RECAPTURE ( -- )
   IMAGE-LIFECYCLE:PREPARE
   IMAGE-LIFECYCLE:COUNT {: hooks:n :}
   2 0 do
      FFI-T-GETPID$ 0 T<>
      FFI-T-GETPID$ 0 T<>
      IMAGE-LIFECYCLE:COUNT hooks T=
      IMAGE-LIFECYCLE:PREPARE
      IMAGE-LIFECYCLE:COUNT hooks T=
   loop ;

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

   s" a C int result is sign-extended from its low half" T-LABEL
   -1 FFI-T-CLOSE -1 T=
   s" an unsigned int result is masked to its low half" T-LABEL
   -1 FFI-T-CLOSE-U32 $FFFFFFFF T=

   FFI-T-GETPID$ 0 T<>
   FFI-T-VOID$  FFI-T-GETPID$ 0 T<>               \ void-return wrapper is stack-neutral

   FFI-T-CSTR-SRC 5 FFI-T-CSTR-DST FFI:CSTR       \ build "world\0" then strlen==5
   FFI-T-CSTR-DST FFI-T-STRLEN$ 5 T=

   s" FFI-T-ROLE-GOOD ( ffi-ctx -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-PASSES
   s" FFI-T-ROLE-BAD ( ffi-dev -- rc ) FFI-T-CTX-SET" FFI-T-CHECK-REJECTS

   \ INTERLEAVED resolve: this row's symbol is still unresolved, so the call
   \ stages slot 0 and only then reaches DLSYM, which marshals through its own
   \ FFI-DLBUF -> the staged argument survives and strlen("hello") is 5.
   FFI-T-HELLO FFI-T-STRLEN-LATE 5 T=
   FFI-T-STUBS

   1.25 2.5 3.0 FFI-T-FMA-CALL 6.125 f= T-ASSERT
   1.5 4 FFI-T-SCALBLN-CALL 24.0 f= T-ASSERT

   9.0 FFI-T-SQRT-CALL 3.0 f= T-ASSERT

   \ The declaration is stack-neutral: it defines a word and nothing else, which
   \ is what the loader's closed evaluate demands.
   s" a declaration leaves the stack as it found it" T-LABEL
   FFI-T-DEPTH-AFTER @ FFI-T-DEPTH-BEFORE @ T=
   FFI-T-DEPTH-PROBE 0 T<>

   \ memcpy writes its destination and the extent is argument 2.
   FFI-T-CSTR-DST FFI-T-HELLO 6 FFI-T-MEMCPY drop
   FFI-T-CSTR-DST FFI-T-STRLEN$ 5 T=

   \ A symbol nothing carries fails at the CALL, by name, not at the declaration.
   [: FFI-T-ABSENT drop ;] E-FFI-DLSYM TTHROWSQ

   \ What the declarer refuses, one structural rule each.
   [: FFI-T-BAD-PTR ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-RESULTS ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT-ARG ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-EXTENT-INDEX ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-BAD-FLOAT-ARITY ;] E-FFI-ARITY TTHROWSQ
   [: FFI-T-BAD-VARARG ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-NO-FIXED ;] E-FFI-SYNTAX TTHROWSQ
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

   s" FFI-T-RAW ( ptr a ptr a n n -- n ) ffi-call-bounded" FFI-T-CHECK-REJECTS
   s" FFI-T-RAW-ABI ( ptr a ptr a ptr a ptr a ptr a n n -- n ) ffi-call-abi-bounded" FFI-T-CHECK-REJECTS
   s" FFI-T-LIE ( n -- ) 8 0 FFI:WRITABLE!" FFI-T-CHECK-REJECTS
   s" FFI-T-MULTI ( ptr u8 n -- n n ) FFI:DLOPEN" FFI-T-CHECK-REJECTS
   s" FFI:" 0 search-wl 0= TTRUE
   s" CALL0" 0 search-wl 0= TTRUE

   s" a library name renders this target's soname spelling" T-LABEL
   s" pq" 5 FFI-T-NAME-A FFI:LIBRARY-NAME$
   s" z" 1 FFI-T-NAME-B FFI:LIBRARY-NAME$ 2drop
   FFI-T-PQ-NAME$ T$=
   HB-TARGET-MACOS? if s" /opt/build.so.cache/libx.1.dylib"
   else s" /opt/build.dylib/libx.so.1" then FFI-DECL:SELECT-LIBRARY
   FFI-DECL:SELECT-PROCESS
   [: FFI-T-LONG-BASE ;] E-FFI-SYNTAX TTHROWSQ
   s" a negative or over-long length is refused before a byte is copied or read" T-LABEL
   [: FFI-T-CSTR-SRC -1 FFI-T-CSTR-DST FFI:CSTR ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI-T-CSTR-DST FFI:CSTR ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI:LIBRARY-PATH drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:LIBRARY-PATH drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI:PROCESS 0 FFI:DECLARE drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI:PROCESS 0 FFI:DECLARE drop ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC -1 FFI-DECL:SELECT-LIBRARY ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-CSTR-SRC STR-MIN-I64 FFI-DECL:SELECT-LIBRARY ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-GUARDED-PATH ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-GUARDED-DIRECT ;] E-FFI-SYNTAX TTHROWSQ
   s" a declaration through the versioned form resolves its symbol" T-LABEL
   FFI-T-VERSIONED-CALL
   FFI-T-VARARGS

   FFI-T-TABLE-FULL
   FFI-T-LIBRARY-SHARED
   FFI-T-PATH-IDENTITY
   FFI-T-LIBRARY-TABLE-FULL
   FFI-T-LONG-LIBRARY-TABLE-FULL
   FFI-T-FOREIGN-LIBRARY
   FFI-T-LONG-FOREIGN-LIBRARY
   FFI-T-GLOBAL
   s" foreign symbol cleanup remains armed across captures" T-LABEL
   FFI-T-RECAPTURE ;

FFI-RUN

T-REPORT

;package
