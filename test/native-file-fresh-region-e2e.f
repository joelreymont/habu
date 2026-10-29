\ A cached FILE that defines an inferred fresh-region producer replays its
\ record, and a warm client still mints a region per call: one call's region
\ builds, two calls compared as one region refuse.
\ bin/hb --load test/native-file-fresh-region-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-FRESH-REGION-TEST
create APP PATH-CAP allot variable APP-U
create CACHE PATH-CAP allot variable CACHE-U

\ One build process per step: the edit between builds is the boundary under
\ test, and every step shares one cache.
: BUILD ( ptr u8 n -- n )
   AT {: product:ptr productu:n :}
   APP APP-U @ product productu CACHE CACHE-U @ NATIVE-FILE-APP:BUILD ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-fresh-region" TREE
   s" cache" AT {: cache:ptr cacheu:n :}
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U ! CACHE CACHE-U @ FS:MAKE-DIR
   s" app.f" AT {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   s" unit.f" S\" package RQ public\ndefer RG-ONE ( -- fresh-region-a )\n: SAME-ID ( x x -- ) 2drop ;\n: USE-INF RG-ONE ;\n;package\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- ) ;\n" PUT
   s" cold build caches the inferred producer" T-LABEL
   s" cold-hb" BUILD 0 T=
   s" app.f" S\" require unit.f\n: TP ( -- ) RQ:USE-INF drop ;\n: MAIN ( -- ) ;\n" PUT
   s" warm client of one call builds from the cached record" T-LABEL
   s" warm-hb" BUILD 0 T=
   NATIVE-FILE-APP:OUT$ s" files reused " CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ s" files reused 0" CONTAINS? TFALSE
   s" app.f" S\" require unit.f\n: T3 ( -- ) RQ:USE-INF RQ:USE-INF RQ:SAME-ID ;\n: MAIN ( -- ) ;\n" PUT
   s" warm client comparing two calls as one region refuses" T-LABEL
   s" refused-hb" BUILD 70 T=
   NATIVE-FILE-APP:ERR$ s" region mismatch (different allocation)" CONTAINS? TTRUE
   T-REPORT
   s" fresh-region FILE artifacts: " type ROOT$ type cr ;
;package
;using
NATIVE-FILE-FRESH-REGION-TEST:MAIN
