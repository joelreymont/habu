\ CWORK-BUFFER and CWORK-SPAN storage in a built program: a row index outside
\ the buffer throws, and an accessor used at the wrong pointee is refused.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-CWORK-TEST
create APP PATH-CAP allot variable APP-U
create BIN-PATH PATH-CAP allot variable BIN-PATH-U
create CACHE PATH-CAP allot variable CACHE-U

: BUILD ( ptr u8 n -- )
   AT {: a:ptr u:n :} a BIN-PATH u BYTE-COPY u BIN-PATH-U !
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! SUCCESS
   OUT$ type ERR$ type ;
: RUN-BIN-PATH ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   BIN-PATH BIN-PATH-U @ s" " CAPTURE {: rc:n :}
   OUT$ type ERR$ type rc 0 T= ;

: CHECK-TYPED-STORAGE ( -- )
   s" unit.f" S\" package CWORK-FILE public\n2 CWORK-BUFFER ROWS n\n2 CWORK-SPAN WORDS n\n;package\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- )\n17 0 CWORK-FILE:ROWS ! 0 CWORK-FILE:ROWS @ .\n23 CWORK-FILE:WORDS ! CWORK-FILE:WORDS @ .\n[: -1 CWORK-FILE:ROWS drop ;] catch .\n[: 2 CWORK-FILE:ROWS drop ;] catch .\n;\n" PUT
   s" CWORK typed storage bounds its rows" T-LABEL
   s" typed-hb" BUILD
   RUN-BIN-PATH
   OUT$ S\" 17\n23\n7122\n7122\n" CONTAINS? TTRUE
   s" app.f" S\" require unit.f\npackage CWORK-CLIENT public\n: WRONG ( n -- ptr u8 ) CWORK-FILE:ROWS ;\n;package\n: MAIN ( -- ) ;\n" PUT
   s" CWORK accessors reject the wrong pointee" T-LABEL
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD 70 T=
   NATIVE-FILE-APP:ERR$ s" expected: ptr u8 actual: ptr n" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-cwork" TREE
   s" cache" AT {: cache:ptr cacheu:n :}
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U ! CACHE CACHE-U @ FS:MAKE-DIR
   s" app.f" AT {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   CHECK-TYPED-STORAGE
   T-REPORT ;
;package
;using
NATIVE-FILE-CWORK-TEST:MAIN
