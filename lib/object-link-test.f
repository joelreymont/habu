\ object-link-test.f - focused tests for OBJLINK symbol validation.
\ Run: bin/hb --load lib/object-link-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require lib/object.f
require lib/object-link.f

package OBJLINK-TEST
using OBJ
using OBJLINK

$30000 constant OBJ-CAP
33 constant NAME-START
$1000 constant BIG-TEXT-U

create OBJ-A OBJ-CAP allot
create OBJ-B OBJ-CAP allot
create OBJ-C OBJ-CAP allot
create NAME1 1 allot
create TEXT-A 1 c, 2 c, 3 c,
create TEXT-R 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,
create DATA-B 4 c, 5 c,
create BIG-TEXT BIG-TEXT-U allot

variable OBJ-A-U
variable OBJ-B-U
variable OBJ-C-U

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: BASE ( -- )
   OBJ:RESET
   HASH$ SOURCE!
   s" macos-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   s" hb-arm64-v1" COMPILER! ;

: SAVE-A ( -- )
   BYTES$ {: a:ptr u:n :}
   a OBJ-A u BYTE-COPY
   u OBJ-A-U ! ;

: SAVE-B ( -- )
   BYTES$ {: a:ptr u:n :}
   a OBJ-B u BYTE-COPY
   u OBJ-B-U ! ;

: SAVE-C ( -- )
   BYTES$ {: a:ptr u:n :}
   a OBJ-C u BYTE-COPY
   u OBJ-C-U ! ;

: LOAD-A ( -- )
   OBJ-A OBJ-A-U @ LOAD ;

: LOAD-B ( -- )
   OBJ-B OBJ-B-U @ LOAD ;

: LOAD-C ( -- )
   OBJ-C OBJ-C-U @ LOAD ;

: BUILD-EXPORT-A ( -- )
   BASE
   s" CORE" s" public" PACKAGE+
   s" lib/string.f" REQUIRE+
   s" count" s" nominal" TYPE+
   s" DIE" NORET+
   TEXT-A 3 TEXT+
   s" FOO" s" n -- n" OBJ:EXPORT+
   s" FOO" 1 s" n -- n" DEF+
   SAVE-A ;

: BUILD-IMPORT-B ( -- )
   BASE
   DATA-B 2 DATA+
   s" FOO" s" n -- n" OBJ:IMPORT+
   SAVE-B ;

: BUILD-DUP-C ( -- )
   BASE
   s" FOO" s" n -- n" OBJ:EXPORT+
   SAVE-C ;

: BUILD-MISSING-C ( -- )
   BASE
   s" BAR" s" n -- n" OBJ:IMPORT+
   SAVE-C ;

: BUILD-BAD-EFFECT-C ( -- )
   BASE
   s" FOO" s" -- n" OBJ:IMPORT+
   SAVE-C ;

: BUILD-EMPTY-C ( -- )
   BASE
   SAVE-C ;

: BUILD-RELOC-C ( -- )
   BASE
   TEXT-R 10 TEXT+
   s" FOO" s" n -- n" OBJ:IMPORT+
   s" BAR" 2 s" n -- n" DEF+
   s" abs64" 1 s" FOO" RELOC+
   SAVE-C ;

: BUILD-LOCAL-RELOC-C ( -- )
   BASE
   TEXT-R 10 TEXT+
   s" LOCAL" 2 s" n -- n" DEF+
   s" abs64" 0 s" LOCAL" RELOC+
   SAVE-C ;

: BUILD-UNKNOWN-RELOC-C ( -- )
   BASE
   TEXT-R 10 TEXT+
   s" LOCAL" 2 s" n -- n" DEF+
   s" pc32" 0 s" LOCAL" RELOC+
   SAVE-C ;

: BUILD-PAST-RELOC-C ( -- )
   BASE
   TEXT-R 10 TEXT+
   s" LOCAL" 2 s" n -- n" DEF+
   s" abs64" 3 s" LOCAL" RELOC+
   SAVE-C ;

: BUILD-MISSING-RELOC-C ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" abs64" 0 s" GHOST" RELOC+
   SAVE-C ;

: BUILD-DUP-DEF-C ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" FOO" 0 s" n -- n" DEF+
   SAVE-C ;

: BUILD-DEF-ONLY-C ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" ONLY" 0 s" n -- n" DEF+
   s" ONLY" s" n -- n" OBJ:IMPORT+
   SAVE-C ;

: BUILD-BAD-DEF-C ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" BAD" 9 s" n -- n" DEF+
   SAVE-C ;

: BUILD-BAD-RELOC-C ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" abs64" 9 s" FOO" RELOC+
   SAVE-C ;

: BUILD-MANY-RELOCS ( -- )
   BASE
   TEXT-A 3 TEXT+
   s" FOO" 0 s" n -- n" DEF+
   0 begin dup 65 < while
      s" abs64" 0 s" FOO" RELOC+
      1+
   repeat drop
   SAVE-C ;

: BUILD-BIG-TEXT-C ( -- )
   BASE
   BIG-TEXT BIG-TEXT-U TEXT+
   SAVE-C ;

: NAME$ ( n -- ptr u8 n ) {: idx:n :}
   NAME-START idx + NAME1 c!
   NAME1 1 ;

: BUILD-MANY-EXPORTS ( -- )
   BASE
   0 begin dup 33 < while
      dup NAME$ s" n -- n" OBJ:EXPORT+
      1+
   repeat drop
   SAVE-C ;

: BUILD-MANY-PACKAGES ( -- )
   BASE
   0 begin dup 33 < while
      dup NAME$ s" public" PACKAGE+
      1+
   repeat drop
   SAVE-C ;

: BUILD-MANY-REQUIRES ( -- )
   BASE
   0 begin dup 33 < while
      dup NAME$ REQUIRE+
      1+
   repeat drop
   SAVE-C ;

: BUILD-MANY-TYPES ( -- )
   BASE
   0 begin dup 33 < while
      dup NAME$ s" nominal" TYPE+
      1+
   repeat drop
   SAVE-C ;

: BUILD-MANY-NORETS ( -- )
   BASE
   0 begin dup 33 < while
      dup NAME$ NORET+
      1+
   repeat drop
   SAVE-C ;

: PREPARE ( -- )
   BUILD-EXPORT-A
   BUILD-IMPORT-B
   BUILD-DUP-C ;

: TEXT-BYTE ( n -- n ) {: idx:n :}
   TEXT$ {: a:ptr u:n :}
   idx u >= if E-OBJ-FIELD throw then
   a idx + c@ ;

: DATA-BYTE ( n -- n ) {: idx:n :}
   DATA$ {: a:ptr u:n :}
   idx u >= if E-OBJ-FIELD throw then
   a idx + c@ ;

: RESOLVES ( -- )
   OBJLINK:RESET
   LOAD-A ADD
   LOAD-B ADD
   OBJLINK:CHECK
   PACKAGE-COUNT 1 T=
   REQUIRE-COUNT 1 T=
   TYPE-COUNT 1 T=
   NORET-COUNT 1 T=
   EXPORT-COUNT 1 T=
   IMPORT-COUNT 1 T=
   DEF-COUNT 1 T=
   RELOC-COUNT 0 T=
   OBJECT-COUNT 2 T=
   TEXT-SIZE 3 T=
   OBJLINK:DATA-SIZE 2 T=
   TEXT$ nip 3 T=
   DATA$ nip 2 T=
   0 TEXT-BYTE 1 T=
   1 TEXT-BYTE 2 T=
   2 TEXT-BYTE 3 T=
   0 DATA-BYTE 4 T=
   1 DATA-BYTE 5 T=
   0 OBJECT-TEXT-BASE 0 T=
   0 OBJECT-DATA-BASE 0 T=
   0 OBJECT-TEXT-SIZE 3 T=
   0 OBJECT-DATA-SIZE 0 T=
   1 OBJECT-TEXT-BASE 3 T=
   1 OBJECT-DATA-BASE 0 T=
   1 OBJECT-TEXT-SIZE 0 T=
   1 OBJECT-DATA-SIZE 2 T=
   0 PACKAGE$ s" CORE" T$=
   0 PACKAGE-VIS$ s" public" T$=
   0 REQUIRE$ s" lib/string.f" T$=
   0 TYPE$ s" count" T$=
   0 TYPE-KIND$ s" nominal" T$=
   0 NORET$ s" DIE" T$=
   0 EXPORT$ s" FOO" T$=
   0 IMPORT$ s" FOO" T$=
   0 DEF$ s" FOO" T$=
   0 EXPORT-EFFECT$ s" n -- n" T$=
   0 IMPORT-EFFECT$ s" n -- n" T$=
   0 DEF-EFFECT$ s" n -- n" T$=
   0 DEF-ADDR 1 T=
   s" FOO" DEF-FIND? TTRUE ;

: DIRECT-EFFECT-TABLES ( -- )
   OBJLINK:RESET
   s" Q" s" n --" OBJLINK:EXPORT+
   s" Q" s" n --" OBJLINK:IMPORT+
   OBJLINK:CHECK
   0 EXPORT-EFFECT$ s" n --" T$=
   0 IMPORT-EFFECT$ s" n --" T$= ;

: RELOC-OFFSET-PASSES ( -- )
   BUILD-RELOC-C
   OBJLINK:RESET
   LOAD-A ADD
   LOAD-C ADD
   OBJLINK:CHECK
   OBJECT-COUNT 2 T=
   TEXT-SIZE 13 T=
   DEF-COUNT 2 T=
   1 DEF$ s" BAR" T$=
   1 DEF-ADDR 5 T=
   RELOC-COUNT 1 T=
   0 RELOC-KIND$ s" abs64" T$=
   0 RELOC-SYM$ s" FOO" T$=
   0 RELOC-PATCH 4 T=
   0 RELOC-TARGET 1 T= ;

: APPLY-ABS64-PATCHES ( -- )
   BUILD-RELOC-C
   OBJLINK:RESET
   LOAD-A ADD
   LOAD-C ADD
   APPLY
   4 TEXT-BYTE 1 T=
   5 TEXT-BYTE 0 T=
   6 TEXT-BYTE 0 T=
   7 TEXT-BYTE 0 T=
   8 TEXT-BYTE 0 T=
   9 TEXT-BYTE 0 T=
   10 TEXT-BYTE 0 T=
   11 TEXT-BYTE 0 T= ;

: ADD-BIG-C ( -- )
   LOAD-C ADD ;

: LOCAL-RELOC-PASSES ( -- )
   BUILD-LOCAL-RELOC-C
   OBJLINK:RESET
   LOAD-C ADD
   OBJLINK:CHECK
   RELOC-COUNT 1 T=
   0 RELOC-PATCH 0 T=
   0 RELOC-TARGET 2 T= ;

: RELOC-TARGET-BEFORE-CHECK-FAILS ( -- )
   BUILD-LOCAL-RELOC-C
   OBJLINK:RESET
   LOAD-C ADD
   [: 0 RELOC-TARGET drop ;] E-OBJ-SCHEMA TTHROWSQ ;

: MISSING-RELOC-FAILS ( -- )
   BUILD-MISSING-RELOC-C
   OBJLINK:RESET
   LOAD-C ADD
   [: OBJLINK:CHECK ;] E-OBJ-SCHEMA TTHROWSQ ;

: UNKNOWN-RELOC-FAILS ( -- )
   BUILD-UNKNOWN-RELOC-C
   OBJLINK:RESET
   LOAD-C ADD
   [: APPLY ;] E-OBJ-SCHEMA TTHROWSQ ;

: PAST-RELOC-FAILS ( -- )
   BUILD-PAST-RELOC-C
   OBJLINK:RESET
   LOAD-C ADD
   [: APPLY ;] E-OBJ-SCHEMA TTHROWSQ ;

: RELOC-OFFSET-FAILS ( -- )
   BUILD-BAD-RELOC-C
   OBJLINK:RESET
   LOAD-A ADD
   [: LOAD-C ADD ;] E-OBJ-SCHEMA TTHROWSQ ;

: RELOC-OVERFLOW-FAILS ( -- )
   BUILD-MANY-RELOCS
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: SECTIONS-GROW ( -- )
   BUILD-BIG-TEXT-C
   OBJLINK:RESET
   0 begin dup 17 < while
      ADD-BIG-C
      1+
   repeat drop
   APPLY
   TEXT-SIZE BIG-TEXT-U 17 * T=
   OBJECT-COUNT 17 T= ;

: DUP-DEF-FAILS ( -- )
   BUILD-DUP-DEF-C
   OBJLINK:RESET
   LOAD-A ADD
   [: LOAD-C ADD ;] E-OBJ-SCHEMA TTHROWSQ ;

: DEF-ONLY-IMPORT-FAILS ( -- )
   BUILD-DEF-ONLY-C
   OBJLINK:RESET
   LOAD-C ADD
   [: OBJLINK:CHECK ;] E-OBJ-SCHEMA TTHROWSQ ;

: BAD-DEF-FAILS ( -- )
   BUILD-BAD-DEF-C
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-SCHEMA TTHROWSQ ;

: DUP-EXPORT-FAILS ( -- )
   OBJLINK:RESET
   LOAD-A ADD
   [: LOAD-C ADD ;] E-OBJ-SCHEMA TTHROWSQ ;

: MISSING-IMPORT-FAILS ( -- )
   BUILD-MISSING-C
   OBJLINK:RESET
   LOAD-C ADD
   [: OBJLINK:CHECK ;] E-OBJ-SCHEMA TTHROWSQ ;

: IMPORT-EFFECT-MISMATCH-FAILS ( -- )
   BUILD-BAD-EFFECT-C
   OBJLINK:RESET
   LOAD-A ADD
   LOAD-C ADD
   [: OBJLINK:CHECK ;] E-OBJ-SCHEMA TTHROWSQ ;

: TABLE-OVERFLOW-FAILS ( -- )
   BUILD-MANY-EXPORTS
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: PACKAGE-OVERFLOW-FAILS ( -- )
   BUILD-MANY-PACKAGES
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: REQUIRE-OVERFLOW-FAILS ( -- )
   BUILD-MANY-REQUIRES
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: TYPE-OVERFLOW-FAILS ( -- )
   BUILD-MANY-TYPES
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: NORET-OVERFLOW-FAILS ( -- )
   BUILD-MANY-NORETS
   OBJLINK:RESET
   [: LOAD-C ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

: ADD-EMPTY-C ( -- )
   LOAD-C ADD ;

: OBJECT-OVERFLOW-FAILS ( -- )
   BUILD-EMPTY-C
   OBJLINK:RESET
   0 begin dup 32 < while
      ADD-EMPTY-C
      1+
   repeat drop
   [: ADD-EMPTY-C ;] E-OBJ-CAPACITY TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   PREPARE
   RESOLVES
   DIRECT-EFFECT-TABLES
   RELOC-OFFSET-PASSES
   APPLY-ABS64-PATCHES
   LOCAL-RELOC-PASSES
   RELOC-TARGET-BEFORE-CHECK-FAILS
   MISSING-RELOC-FAILS
   UNKNOWN-RELOC-FAILS
   PAST-RELOC-FAILS
   RELOC-OFFSET-FAILS
   RELOC-OVERFLOW-FAILS
   SECTIONS-GROW
   DUP-DEF-FAILS
   DEF-ONLY-IMPORT-FAILS
   BAD-DEF-FAILS
   DUP-EXPORT-FAILS
   MISSING-IMPORT-FAILS
   IMPORT-EFFECT-MISMATCH-FAILS
   TABLE-OVERFLOW-FAILS
   PACKAGE-OVERFLOW-FAILS
   REQUIRE-OVERFLOW-FAILS
   TYPE-OVERFLOW-FAILS
   NORET-OVERFLOW-FAILS
   OBJECT-OVERFLOW-FAILS
   T-REPORT ;

;using
;using
;package

OBJLINK-TEST:MAIN
