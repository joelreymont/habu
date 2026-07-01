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

$1000 constant OBJ-CAP
33 constant NAME-START

create OBJ-A OBJ-CAP allot
create OBJ-B OBJ-CAP allot
create OBJ-C OBJ-CAP allot
create NAME1 1 allot

variable OBJ-A-U
variable OBJ-B-U
variable OBJ-C-U

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: BASE ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" macos-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER! ;

: SAVE-A ( -- )
   OBJ:BYTES$ {: a:ptr u:n :}
   a OBJ-A u BYTE-COPY
   u OBJ-A-U ! ;

: SAVE-B ( -- )
   OBJ:BYTES$ {: a:ptr u:n :}
   a OBJ-B u BYTE-COPY
   u OBJ-B-U ! ;

: SAVE-C ( -- )
   OBJ:BYTES$ {: a:ptr u:n :}
   a OBJ-C u BYTE-COPY
   u OBJ-C-U ! ;

: LOAD-A ( -- )
   OBJ-A OBJ-A-U @ OBJ:LOAD ;

: LOAD-B ( -- )
   OBJ-B OBJ-B-U @ OBJ:LOAD ;

: LOAD-C ( -- )
   OBJ-C OBJ-C-U @ OBJ:LOAD ;

: BUILD-EXPORT-A ( -- )
   BASE
   s" FOO" s" n -- n" OBJ:EXPORT+
   SAVE-A ;

: BUILD-IMPORT-B ( -- )
   BASE
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

: PREPARE ( -- )
   BUILD-EXPORT-A
   BUILD-IMPORT-B
   BUILD-DUP-C ;

: RESOLVES ( -- )
   OBJLINK:RESET
   LOAD-A OBJLINK:ADD
   LOAD-B OBJLINK:ADD
   OBJLINK:CHECK
   OBJLINK:EXPORT-COUNT 1 T=
   OBJLINK:IMPORT-COUNT 1 T=
   0 OBJLINK:EXPORT$ s" FOO" T$=
   0 OBJLINK:IMPORT$ s" FOO" T$= ;

: DUP-EXPORT-FAILS ( -- )
   OBJLINK:RESET
   LOAD-A OBJLINK:ADD
   [: LOAD-C OBJLINK:ADD ;] E-OBJ-SCHEMA TTHROWSQ ;

: MISSING-IMPORT-FAILS ( -- )
   BUILD-MISSING-C
   OBJLINK:RESET
   LOAD-C OBJLINK:ADD
   [: OBJLINK:CHECK ;] E-OBJ-SCHEMA TTHROWSQ ;

: TABLE-OVERFLOW-FAILS ( -- )
   BUILD-MANY-EXPORTS
   OBJLINK:RESET
   [: LOAD-C OBJLINK:ADD ;] E-OBJ-CAPACITY TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   PREPARE
   RESOLVES
   DUP-EXPORT-FAILS
   MISSING-IMPORT-FAILS
   TABLE-OVERFLOW-FAILS
   T-REPORT ;

end-package

OBJLINK-TEST:MAIN
