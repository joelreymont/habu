\ object-image-test.f - focused tests for tools/object-image.f.
\ Run: bin/hb --load tools/object-image-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/content-key.f
require lib/object.f
require lib/object-link.f
require tools/object-image.f

package OBJIMG-TEST

1024 constant CAP
5000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
create ROOT FS-PATH-CAP allot
create EXE FS-PATH-CAP allot

variable ROOT-U
variable EXE-U

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: EXE$ ( -- ptr u8 n )
   EXE EXE-U @ ;

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: COPY-PATH ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-object-image" TMPDIR-MKDIR 2dup CLEANUP-TREE+
   ROOT ROOT-U COPY-PATH
   ROOT$ s" obj-exit" EXE JOIN-PATH EXE-U ! ;

: BUILD-EXIT-OBJ ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" host-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER!
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: WRITE-IMAGE ( -- )
   OBJIMG:RESET
   OBJIMG:ADD
   EXE$ OBJIMG:WRITE
   EXE$ FILE? TTRUE ;

: CAPTURE>N ( len len rc -- n n n ) {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: RUN-EXE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu:n out:ptr outcap:n err:ptr errcap:n timeout:n :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-CAPTURE
   CAPTURE>N ;

: RUN-IMAGE ( -- )
   EXE$ OUT CAP ERR CAP TIMEOUT-MS RUN-EXE
   {: outu:n erru:n rc:n :}
   rc 0 T=
   outu 0 T=
   erru 0 T= ;

: EMPTY-FAILS ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" host-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER!
   OBJIMG:RESET
   OBJIMG:ADD
   [: EXE$ OBJIMG:WRITE ;] E-OBJ-SCHEMA TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   BUILD-EXIT-OBJ
   WRITE-IMAGE
   RUN-IMAGE
   EMPTY-FAILS
   CLEANUP-RUN
   T-REPORT ;

end-package

OBJIMG-TEST:MAIN
