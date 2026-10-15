\ A FILE that extends CWORK must run its source on every build: a native
\ range cannot replay the workspace layout or its mapped default values.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-CWORK-TEST
create APP FS-PATH-CAP allot variable APP-U
create BIN-PATH FS-PATH-CAP allot variable BIN-PATH-U
create CACHE FS-PATH-CAP allot variable CACHE-U

: BUILD ( ptr u8 n -- )
   AT {: a:ptr u:n :} a BIN-PATH u BYTE-COPY u BIN-PATH-U !
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! SUCCESS
   OUT$ type ERR$ type ;
: RUN-BIN-PATH ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   BIN-PATH BIN-PATH-U @ s" " CAPTURE {: rc:n :}
   OUT$ type ERR$ type rc 0 T= ;
: APP-COLD ( -- )
   s" app.f" S\" require unit.f\nvariable OBSERVED\nCWORK-FILE:VALUE @ OBSERVED !\n: MAIN ( -- ) OBSERVED @ . CWORK-FILE:VALUE @ . 42 . ;\n" PUT ;
: APP-WARM ( -- )
   s" app.f" S\" require unit.f\nvariable OBSERVED\nCWORK-FILE:VALUE @ OBSERVED !\n: MAIN ( -- ) OBSERVED @ . CWORK-FILE:VALUE @ . 43 . ;\n" PUT ;

: CHECK-TYPED-STORAGE ( -- )
   s" unit.f" S\" package CWORK-FILE public\n2 CWORK-BUFFER ROWS n\n2 CWORK-SPAN WORDS n\n;package\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- )\n17 0 CWORK-FILE:ROWS ! 0 CWORK-FILE:ROWS @ .\n23 CWORK-FILE:WORDS ! CWORK-FILE:WORDS @ .\n[: -1 CWORK-FILE:ROWS drop ;] catch .\n[: 2 CWORK-FILE:ROWS drop ;] catch .\n;\n" PUT
   s" CWORK typed storage survives capture and bounds its rows" T-LABEL
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
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U ! CACHE CACHE-U @ MAKE-DIR
   s" app.f" AT {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   s" unit.f" S\" package CWORK-FILE public\n-1 CWORK-VARIABLE VALUE n\n;package\ns\" cwork unit source ran\" type cr\n" PUT
   APP-COLD
   s" cold FILE compilation observes the late default" T-LABEL
   s" cold-hb" BUILD
   OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" -1\n-1\n42\n" CONTAINS? TTRUE
   APP-WARM
   s" changed parent reruns CWORK child source" T-LABEL
   s" warm-hb" BUILD
   OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" -1\n-1\n43\n" CONTAINS? TTRUE
   CHECK-TYPED-STORAGE
   T-REPORT
   s" CWORK FILE artifacts: " type ROOT$ type cr ;
;package
;using
NATIVE-FILE-CWORK-TEST:MAIN
