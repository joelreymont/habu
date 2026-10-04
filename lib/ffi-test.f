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
require lib/le.f                   \ the spilled %n out-parameter is a C int
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

DEFTYPE FFI-DEV
DEFTYPE FFI-CTX

\ The libc bindings are FUNCTION: declarations: the declared effect is the
\ generated word's effect and decides every argument's staging. What stays
\ TRUSTED: below is the raw-stub half of this suite - five cp@/patch32 code
\ minters and the five fixtures that call a stub address - because a
\ declaration models a C function, and a code-injection minter and a
\ deliberately non-AAPCS64 stub are neither.

PROCESS-SYMBOLS
FUNCTION: FFI-T-STRLEN$ strlen ( ptr u8 -- n ) ;FUNCTION
FUNCTION: FFI-T-STRNCMP$ strncmp ( ptr u8 ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: FFI-T-GETPID$ getpid ( -- i32 ) ;FUNCTION
FUNCTION: FFI-T-CTX-CALL getpid ( n -- i32 ) ;FUNCTION

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

: FFI-T-CHECK-PASSES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: FFI-T-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Raw stubs use the host C ABI: SUM10 adds every integer register and stack
\ argument, FSUM3 adds three float registers, FADD-X0 mixes an integer and a
\ float, FADD-FSTACK reads a float spill, and X8-STORE writes the output cell.
\ On AAPCS64 the last uses x8; on SysV it uses the first integer argument.
\ Built inside words so cp@ is the stable free code slot.
TRUSTED: FFI-T-SUM10 ( -- n ) cp@ {: fn:n :}
   HB-TARGET-LINUX-X86-64? if
      $48F88948 fn       patch32  $0148F001 fn $04 + patch32
      $C80148D0 fn $08 + patch32  $4CC0014C fn $0C + patch32
      $0348C801 fn $10 + patch32  $48082444 fn $14 + patch32
      $10244403 fn $18 + patch32  $24440348 fn $1C + patch32
      $44034818 fn $20 + patch32  $90C32024 fn $24 + patch32
      fn exit
   then
   $8B010000 fn       patch32  $8B020000 fn $4 +  patch32  $8B030000 fn $8 +  patch32
   $8B040000 fn $C +  patch32  $8B050000 fn $10 + patch32  $8B060000 fn $14 + patch32
   $8B070000 fn $18 + patch32
   $F94003E9 fn $1C + patch32  $8B090000 fn $20 + patch32
   $F94007E9 fn $24 + patch32  $8B090000 fn $28 + patch32
   $D65F03C0 fn $2C + patch32  fn ;

TRUSTED: FFI-T-FSUM3 ( -- n ) cp@ {: fn:n :}
   HB-TARGET-LINUX-X86-64? if
      $C1580FF2 fn patch32  $C2580FF2 fn $04 + patch32
      $909090C3 fn $08 + patch32  fn exit
   then
   $1E612800 fn      patch32  $1E622800 fn $4 + patch32
   $D65F03C0 fn $8 + patch32  fn ;

TRUSTED: FFI-T-FADD-X0 ( -- n ) cp@ {: fn:n :}
   HB-TARGET-LINUX-X86-64? if
      $2A0F48F2 fn patch32  $580FF2CF fn $04 + patch32
      $9090C3C1 fn $08 + patch32  fn exit
   then
   $9E620008 fn       patch32  $1E682800 fn $4 + patch32
   $D65F03C0 fn $8 +  patch32  fn ;

TRUSTED: FFI-T-FADD-FSTACK ( -- n ) cp@ {: fn:n :}
   HB-TARGET-LINUX-X86-64? if
      $4C100FF2 fn patch32  $0FF20824 fn $04 + patch32
      $90C3C158 fn $08 + patch32  fn exit
   then
   $F94003E9 fn       patch32  $9E670128 fn $4 + patch32
   $1E682800 fn $8 +  patch32  $D65F03C0 fn $C + patch32  fn ;

TRUSTED: FFI-T-X8-STORE ( -- n ) cp@ {: fn:n :}
   HB-TARGET-LINUX-X86-64? if $C3378948 fn patch32 fn exit then
   $F9000100 fn patch32  $D65F03C0 fn $4 + patch32  fn ;

\ Exact ten-integer binding covers x0-x7 and two stack-spilled cells.
TRUSTED: FFI-T-SUM10-CALL ( -- n )
   FFI:RESET
   10 0 ?do i 1+ i FFI:VALUE! loop
   FFI:ARGS FFI:REG-LENS 10 FFI-T-SUM10 ffi-call-bounded ;

\ Exact three-register floating binding.
TRUSTED: FFI-T-FSUM3-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT!
   2.5 1 FFI:FLOAT!
   3.0 2 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-FSUM3 ffi-call-abi-r-bounded ;

\ Exact mixed x0/d0 binding.
TRUSTED: FFI-T-FADD-X0-CALL ( -- r )
   FFI:RESET
   4 0 FFI:VALUE!
   1.5 0 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-FADD-X0 ffi-call-abi-r-bounded ;

\ Exact floating-register plus stack-spill binding with distinct extents.
TRUSTED: FFI-T-FADD-FSTACK-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT!
   2.75 0 FFI:STACK-FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   1 FFI-T-FADD-FSTACK ffi-call-abi-r-bounded ;

\ Exact sret binding fixes x8 to an eight-byte output.
TRUSTED: FFI-T-X8-ABI-CALL ( ptr a -- n ) {: out:ptr :}
   FFI:RESET
   HB-TARGET-LINUX-X86-64? if
      out 8 0 FFI:WRITABLE!
      42 1 FFI:VALUE!
      FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
      0 FFI-T-X8-STORE ffi-call-abi-bounded exit
   then
   42 0 FFI:VALUE!
   out 8 FFI:X8-WRITABLE!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-X8-STORE ffi-call-abi-bounded ;

\ The libm square root: a float argument and a float result, so the declaration
\ rides the ABI call. The library is chosen at load time because the two
\ targets keep it in different files, which is what the runtime-string form of
\ LIBRARY is for.
: FFI-T-SELECT-MATH ( -- )
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib"
   else
      s" libm.so.6"
   then FFI-DECL:SELECT-LIBRARY ;

FFI-T-SELECT-MATH
FUNCTION: FFI-T-SQRT-CALL sqrt ( r -- r ) ;FUNCTION
PROCESS-SYMBOLS

\ The same declaration exercises Apple's stack varargs and Linux's registers.
FUNCTION: FFI-T-PRINTF snprintf ( ptr u8 n ptr u8 n r ptr u8 -- i32 )
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
64 BUFFER: FFI-T-PRINT-BUF
4 BUFFER: FFI-T-COUNT
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
   FFI-T-PRINT-BUF 64 s\" %ld %ld %ld%n\z" drop
      11 22 33 FFI-T-COUNT FFI-T-PRINTF-COUNT
   FFI-T-PRINT-BUF swap s" 11 22 33" T$=
   FFI-T-COUNT LE:U32@ 8 T= ;

\ SysV's ninth float spills after eight XMM registers. The declaration is
\ target-conditional because the AAPCS64 planner deliberately keeps its
\ existing eight-float limit.
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
   \ 10-arg call: x0..x7 + 2 stack-spilled args, sum 1..10 == 55
   FFI-T-SUM10-CALL 55 T=

   FFI-T-FSUM3-CALL 6.75 f= T-ASSERT
   FFI-T-FADD-X0-CALL 5.5 f= T-ASSERT
   FFI-T-FADD-FSTACK-CALL 4.0 f= T-ASSERT

   FFI-T-X8-OUT FFI-T-X8-ABI-CALL drop
   FFI-T-X8-OUT @ 42 T=

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
   HB-TARGET-LINUX-X86-64? if
      [: FFI-T-BAD-FLOAT-ARITY ;] 0 TTHROWSQ
   else
      [: FFI-T-BAD-FLOAT-ARITY ;] E-FFI-ARITY TTHROWSQ
   then
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
   FFI-T-X86-FLOAT-SPILL

   FFI-T-TABLE-FULL
   FFI-T-LIBRARY-SHARED
   FFI-T-PATH-IDENTITY
   FFI-T-LIBRARY-TABLE-FULL
   FFI-T-LONG-LIBRARY-TABLE-FULL
   FFI-T-FOREIGN-LIBRARY
   FFI-T-LONG-FOREIGN-LIBRARY
   s" foreign symbol cleanup remains armed across captures" T-LABEL
   FFI-T-RECAPTURE ;

FFI-RUN

T-REPORT

;package
