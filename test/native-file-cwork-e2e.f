\ A file that declares CWORK storage or writes a CWORK DEFAULT at top level
\ compiles from source on every build: compiler working state is never
\ carried, so its layout and ordered defaults hold across a parent edit.
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

: CHECK-DEFAULT-ORDER ( -- )
   s" unit.f" S\" package CWORK-FILE public\n7 CWORK-VARIABLE VALUE n\n;package\nCWORK-FILE:VALUE @ drop\nCWORK:DECLARED-BYTES CELL - 9 CWORK:DEFAULT\nCWORK:DECLARED-BYTES CELL - 0 CWORK:DEFAULT\ns\" default unit source ran\" type cr\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- ) CWORK-FILE:VALUE @ . ;\n" PUT
   s" a rebuilt CWORK file retains repeated and zero defaults" T-LABEL
   s" defaults-hb" BUILD
   RUN-BIN-PATH
   OUT$ S\" 0\n" CONTAINS? TTRUE
   s" app.f" S\" require unit.f\n: MAIN ( -- ) CWORK-FILE:VALUE @ . 5 . ;\n" PUT
   s" defaults-warm-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" default unit source ran" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" 0\n5\n" CONTAINS? TTRUE ;

: CHECK-MOVED-PREFIX ( -- )
   s" prefix.f" S\" package CWORK-PREFIX public\n1 constant CELL-COUNT\n1 CWORK-BUFFER PREFIX-ROW n\n;package\n" PUT
   s" unit.f" S\" package CWORK-FILE public\n7 CWORK-VARIABLE VALUE n\n2 CWORK-BUFFER ROWS n\n;package\nCWORK:DECLARED-BYTES 3 CELL * - 9 CWORK:DEFAULT\nCWORK:DECLARED-BYTES 3 CELL * - 19 CWORK:DEFAULT\ns\" moved unit source ran\" type cr\n" PUT
   s" app.f" S\" require prefix.f\nrequire unit.f\n: MAIN ( -- ) CWORK-PREFIX:CELL-COUNT 0 ?do 71 i CWORK-PREFIX:PREFIX-ROW ! loop CWORK-FILE:VALUE @ . 31 1 CWORK-FILE:ROWS ! CWORK-FILE:VALUE @ . 1 CWORK-FILE:ROWS @ . ;\n" PUT
   s" a rebuilt CWORK allocation follows an earlier prefix" T-LABEL
   s" moved-cold-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" moved unit source ran" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" 19\n19\n31\n" CONTAINS? TTRUE
   s" prefix.f" S\" package CWORK-PREFIX public\n3 constant CELL-COUNT\n3 CWORK-BUFFER PREFIX-ROW n\n;package\n" PUT
   s" moved-grow-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" moved unit source ran" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" 19\n19\n31\n" CONTAINS? TTRUE
   s" prefix.f" S\" package CWORK-PREFIX public\n2 constant CELL-COUNT\n2 CWORK-BUFFER PREFIX-ROW n\n;package\n" PUT
   s" moved-shrink-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" moved unit source ran" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" 19\n19\n31\n" CONTAINS? TTRUE ;

: CHECK-STALE-DECL ( -- )
   s" unit.f" S\" package CWORK-FILE public\n7 CWORK-VARIABLE VALUE n\n;package\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- ) CWORK-FILE:VALUE @ . ;\n" PUT
   s" cached client follows CWORK declaration identity" T-LABEL
   s" identity-hb" BUILD
   s" unit.f" S\" package CWORK-FILE public\n7 CWORK-VARIABLE OTHER n\n;package\n" PUT
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD 70 T= ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-cwork" TREE
   s" cache" AT {: cache:ptr cacheu:n :}
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U ! CACHE CACHE-U @ FS:MAKE-DIR
   s" app.f" AT {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   s" unit.f" S\" package CWORK-FILE public\n-1 CWORK-VARIABLE VALUE n\n;package\ns\" cwork unit source ran\" type cr\n" PUT
   APP-COLD
   s" cold FILE compilation observes the late default" T-LABEL
   s" cold-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" -1\n-1\n42\n" CONTAINS? TTRUE
   APP-WARM
   s" a changed parent recompiles its CWORK child from source" T-LABEL
   true NATIVE-FILE-APP:DIAG? !
   s" warm-hb" BUILD
   false NATIVE-FILE-APP:DIAG? !
   ERR$ S\" reason=run-writes-cwork word=CWORK-VARIABLE\n" CONTAINS? TTRUE
   ERR$ s" /unit.f reason=ordered-dependency" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT$ S\" -1\n-1\n43\n" CONTAINS? TTRUE
   CHECK-DEFAULT-ORDER
   CHECK-MOVED-PREFIX
   CHECK-STALE-DECL
   CHECK-TYPED-STORAGE
   T-REPORT ;
;package
;using
NATIVE-FILE-CWORK-TEST:MAIN
