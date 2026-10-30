\ File objects are keyed by tree-relative file identity and check their input
\ rows against the tree that builds them: an identical copy of an application
\ at another path, built into the same out root, reuses every file and compiles
\ none, and a copy whose library value changed under the same contract
\ recompiles the library under the shared key and reuses its caller, which
\ then calls the recompiled value.
\ Run: bin/hb --load test/native-file-tree-share-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TREE-SHARE-TEST

$8000 constant CAP
create DIR-PATH PATH-CAP allot
create SRC-PATH PATH-CAP allot
create PROD-PATH PATH-CAP allot
create CACHE-PATH PATH-CAP allot
create OUT-A CAP allot  variable OUT-A-U
create OUT-B CAP allot  variable OUT-B-U
create OUT-C CAP allot  variable OUT-C-U
variable OBJ-N
variable OBJS-A  variable OBJS-B  variable OBJS-C

: APP ( -- ptr u8 n )
   S\" require lib.f\n: MAIN ( -- ) TREE-SHARE-LIB:VALUE . ;\n" ;

\ lib.f with BASE set to the given source text; VALUE's contract never changes.
: LIB ( ptr u8 n -- ptr u8 n ) {: base:ptr baseu:n :}
   SB-RESET
   s" package TREE-SHARE-LIB" SB-APPEND 10 SB-APPEND-C
   base baseu SB-APPEND
   S\"  constant BASE\npublic\n: VALUE ( -- n ) BASE 1+ ;\n;package\n" SB-APPEND
   SB$ ;

\ The tree dir under the test root, holding app.f and a lib.f with BASE.
: TREE-FILES ( ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n base:ptr baseu:n :}
   dir diru AT FS:MAKE-DIR
   ROOT$ dir diru DIR-PATH FS:JOIN-PATH {: u:n :}
   DIR-PATH u s" app.f" SRC-PATH FS:JOIN-PATH {: appu:n :}
   SRC-PATH appu APP FS:WRITE-ALL
   DIR-PATH u s" lib.f" SRC-PATH FS:JOIN-PATH {: libu:n :}
   base baseu LIB {: body:ptr bodyu:n :}
   SRC-PATH libu body bodyu FS:WRITE-ALL ;

\ dir/app.f into dir/app-hb with the out root every tree shares.
: BUILD ( ptr u8 n -- n )
   {: dir:ptr diru:n :}
   ROOT$ dir diru DIR-PATH FS:JOIN-PATH {: u:n :}
   DIR-PATH u s" app.f" SRC-PATH FS:JOIN-PATH {: srcu:n :}
   DIR-PATH u s" app-hb" PROD-PATH FS:JOIN-PATH {: produ:n :}
   ROOT$ s" cache" CACHE-PATH FS:JOIN-PATH {: cacheu:n :}
   SRC-PATH srcu PROD-PATH produ CACHE-PATH cacheu NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

\ Runs dir/app-hb; its stdout lands in OUT$.
: RUN ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   ROOT$ dir diru DIR-PATH FS:JOIN-PATH {: u:n :}
   DIR-PATH u s" app-hb" PROD-PATH FS:JOIN-PATH {: produ:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   PROD-PATH produ s" " CAPTURE SUCCESS ;

: KEEP ( ptr u8 ptr len -- ) {: dst:ptr lenp:ptr :}
   0 >LEN lenp ! OUT$ dst CAP lenp BUF-APPEND ;

\ A file object, <64 hex>.hfo, counts once.
: NOTE-OBJ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" .hfo" ENDS-WITH? u 68 = and
   if 1 OBJ-N +! then ;

\ The file objects under the shared out root.
: OBJS ( -- n )
   0 OBJ-N !
   s" cache" AT NATIVE-FILE-APP:OBJ-DIR$ [: NOTE-OBJ ;] FS-LIST:EACH
   OBJ-N @ ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-tree-share" TREE
   s" cache" AT FS:MAKE-DIR
   s" a" s" 41" TREE-FILES
   s" b" s" 41" TREE-FILES
   s" c" s" 99" TREE-FILES

   s" a cold tree builds and runs" T-LABEL
   s" a" BUILD SUCCESS
   OUT$ S\" files reused 0\n compiled 2\n" CONTAINS? TTRUE
   s" a" RUN
   OUT$ S\" 42\n" T$=
   OUT-A OUT-A-U KEEP
   OBJS OBJS-A !
   OBJS-A @ 0 > TTRUE

   s" an identical tree at another path reuses every file" T-LABEL
   s" b" BUILD SUCCESS
   OUT$ S\" files reused 2\n compiled 0\n" CONTAINS? TTRUE
   s" b" RUN
   OUT$ OUT-A OUT-A-U @ T$=
   OUT-B OUT-B-U KEEP
   OBJS OBJS-B !
   OBJS-B @ OBJS-A @ T=

   s" an edited library recompiles and its caller calls the new value" T-LABEL
   s" c" BUILD SUCCESS
   OUT$ S\" files reused 1\n compiled 1\n" CONTAINS? TTRUE
   s" c" RUN
   OUT$ S\" 100\n" T$=
   OUT-C OUT-C-U KEEP
   OBJS OBJS-C !
   OBJS-C @ OBJS-A @ T=

   T-REPORT ;
;package
;using

NATIVE-FILE-TREE-SHARE-TEST:MAIN
