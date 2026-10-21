\ object-image-test.f - focused tests for tools/object-image.f.
\ Run: bin/hb --load tools/object-image-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/content-key.f
require src/lib/object.f
require tools/object-image.f

package OBJIMG-TEST
: LOAD-EXIT ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" tools/object-image-test-x64.f" required exit
   then
   s" tools/object-image-test-arm.f" required ;
' LOAD-EXIT
;package
execute

package OBJIMG-TEST
using OBJ
using OBJIMG

1024 constant CAP
5000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
create ROOT FS-PATH-CAP allot
create EXE FS-PATH-CAP allot

variable ROOT-U
variable EXE-U
$81000 constant PADDING-U
DYNAMIC-BUFFER PADDING n

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: EXE$ ( -- ptr u8 n )
   EXE EXE-U @ ;

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" host-x86-64" exit then
   s" host-aarch64" ;

: COMPILER$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" hb-x86-64-v1" exit then
   s" hb-arm64-v1" ;

: COPY-PATH ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-object-image" HB-TMP-MKDIR 2dup CLEANUP-TREE+
   ROOT ROOT-U COPY-PATH
   ROOT$ s" obj-exit" EXE JOIN-PATH EXE-U ! ;

: START-OBJ ( -- )
   OBJ:RESET
   HASH$ SOURCE!
   TARGET$ TARGET!
   s" checker-effect-v1" CHECKER!
   COMPILER$ COMPILER! ;

: FINISH-OBJ ( -- )
   CODE-TEXT+
   s" MAIN" s" --" EXPORT+
   s" MAIN" 0 s" --" DEF+ ;

: BUILD-EXIT-OBJ ( -- )
   START-OBJ EXIT-CODE FINISH-OBJ ;

: BUILD-EXIT-OFF ( n -- ) {: off:n :}
   START-OBJ EXIT-CODE
   CODE-TEXT+
   s" MAIN" s" --" EXPORT+
   s" MAIN" off s" --" DEF+ ;

: WRITE-IMAGE ( -- )
   OBJIMG:LOAD
   EXE$ OBJIMG:WRITE
   EXE$ FILE? TTRUE ;

: CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

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

: LARGE-IMAGE ( -- )
   START-OBJ EXIT-CODE
   PADDING-U CELL / PADDING-RESERVE
   \ The exit stub runs before this zero-filled, unreachable payload.
   0 PADDING byte-view PADDING-U CODE-PAD
   FINISH-OBJ
   WRITE-IMAGE
   EXE$ FILE-SIZE PADDING-U > TTRUE
   RUN-IMAGE
   PADDING-RELEASE ;

: EMPTY-FAILS ( -- )
   OBJ:RESET
   HASH$ SOURCE!
   TARGET$ TARGET!
   s" checker-effect-v1" CHECKER!
   COMPILER$ COMPILER!
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ ;

: BAD-SHAPES-FAIL ( -- )
   BUILD-EXIT-OBJ
   s" package" s" public" OBJ:PACKAGE+
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   BUILD-EXIT-OBJ
   s" MAIN" s" --" OBJ:IMPORT+
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   BUILD-EXIT-OBJ
   s" MAIN" 1 s" --" OBJ:DEF+
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   1 BUILD-EXIT-OFF
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   BUILD-EXIT-OBJ
   s" OTHER" 0 s" 0000000000000005" OBJ:ENTRY+
   [: OBJIMG:LOAD ;] E-OBJ-SCHEMA TTHROWSQ ;

: NAMED-ENTRY ( -- )
   BUILD-EXIT-OBJ
   s" MAIN" 1 s" 0000000000000005" OBJ:ENTRY+
   OBJIMG:LOAD
   EXE$ OBJIMG:WRITE
   RUN-IMAGE ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   BUILD-EXIT-OBJ
   WRITE-IMAGE
   RUN-IMAGE
   LARGE-IMAGE
   EMPTY-FAILS
   BAD-SHAPES-FAIL
   NAMED-ENTRY
   CLEANUP-RUN
   T-REPORT ;

;using
;using
;package

OBJIMG-TEST:MAIN
