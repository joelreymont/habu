\ The definition cache is keyed by tree-relative file identity: an identical
\ copy of an application at another path, built into the same cache, reuses
\ every definition entry, and a copy whose library value changed under the same
\ contract still reuses its caller, which then calls the recompiled value.
\ Run: bin/hb --load test/native-file-tree-share-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TREE-SHARE-TEST

$8000 constant CAP
create DIR-PATH FS-PATH-CAP allot
create SRC-PATH FS-PATH-CAP allot
create PROD-PATH FS-PATH-CAP allot
create CACHE-PATH FS-PATH-CAP allot
create OUT-A CAP allot  variable OUT-A-U
create OUT-B CAP allot  variable OUT-B-U
create OUT-C CAP allot  variable OUT-C-U
variable DEF-N
variable DEFS-A  variable DEFS-B  variable DEFS-C

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
   dir diru AT MAKE-DIR
   ROOT$ dir diru DIR-PATH JOIN-PATH {: u:n :}
   DIR-PATH u s" app.f" SRC-PATH JOIN-PATH {: appu:n :}
   SRC-PATH appu APP WRITE-ALL
   DIR-PATH u s" lib.f" SRC-PATH JOIN-PATH {: libu:n :}
   base baseu LIB {: body:ptr bodyu:n :}
   SRC-PATH libu body bodyu WRITE-ALL ;

\ dir/app.f into dir/app-hb with the definition cache every tree shares.
: BUILD ( ptr u8 n -- n )
   {: dir:ptr diru:n :}
   ROOT$ dir diru DIR-PATH JOIN-PATH {: u:n :}
   DIR-PATH u s" app.f" SRC-PATH JOIN-PATH {: srcu:n :}
   DIR-PATH u s" app-hb" PROD-PATH JOIN-PATH {: produ:n :}
   ROOT$ s" cache" CACHE-PATH JOIN-PATH {: cacheu:n :}
   SRC-PATH srcu PROD-PATH produ CACHE-PATH cacheu NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

\ Runs dir/app-hb; its stdout lands in OUT$.
: RUN ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   ROOT$ dir diru DIR-PATH JOIN-PATH {: u:n :}
   DIR-PATH u s" app-hb" PROD-PATH JOIN-PATH {: produ:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   PROD-PATH produ s" " CAPTURE SUCCESS ;

: KEEP ( ptr u8 ptr len -- ) {: dst:ptr lenp:ptr :}
   0 >LEN lenp ! OUT$ dst CAP lenp BUF-APPEND ;

\ A definition entry, hb-def-<64 hex>.hbd, counts once.
: NOTE-DEF ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" hb-def-" STARTS-WITH? a u s" .hbd" ENDS-WITH? and
   if 1 DEF-N +! then ;

\ The hb-def-*.hbd entries in the shared cache.
: DEFS ( -- n )
   0 DEF-N !
   s" cache" AT [: NOTE-DEF ;] FS-LIST:EACH
   DEF-N @ ;

: WARM? ( -- bool )
   OUT$ s" definition hits " CONTAINS?
   OUT$ S\" definition hits 0\n" CONTAINS? 0= and ;

: LINE ( ptr u8 n ptr u8 n -- ) {: tag:ptr tagu:n a:ptr u:n :}
   tag tagu SB-APPEND s" : " SB-APPEND a u SB-APPEND ;

: COUNT-LINE ( ptr u8 n n -- ) {: tag:ptr tagu:n n:n :}
   tag tagu SB-APPEND s"  defs " SB-APPEND n FMT:SB-INT 10 SB-APPEND-C ;

: ARTIFACT ( -- )
   SB-RESET
   s" a" OUT-A OUT-A-U @ LINE
   s" b" OUT-B OUT-B-U @ LINE
   s" c" OUT-C OUT-C-U @ LINE
   s" a" DEFS-A @ COUNT-LINE
   s" b" DEFS-B @ COUNT-LINE
   s" c" DEFS-C @ COUNT-LINE
   s" tree-share.txt" AT SB$ WRITE-ALL ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-tree-share" TREE
   s" cache" AT MAKE-DIR
   s" a" s" 41" TREE-FILES
   s" b" s" 41" TREE-FILES
   s" c" s" 99" TREE-FILES

   s" a cold tree builds and runs" T-LABEL
   s" a" BUILD SUCCESS
   s" a" RUN
   OUT$ S\" 42\n" T$=
   OUT-A OUT-A-U KEEP
   DEFS DEFS-A !
   DEFS-A @ 0 > TTRUE

   s" an identical tree at another path reuses every definition" T-LABEL
   s" b" BUILD SUCCESS
   WARM? TTRUE
   s" b" RUN
   OUT$ OUT-A OUT-A-U @ T$=
   OUT-B OUT-B-U KEEP
   DEFS DEFS-B !
   DEFS-B @ DEFS-A @ T=

   s" an edited library value reuses its caller against the new value" T-LABEL
   s" c" BUILD SUCCESS
   WARM? TTRUE
   s" c" RUN
   OUT$ S\" 100\n" T$=
   OUT-C OUT-C-U KEEP
   DEFS DEFS-C !

   ARTIFACT
   T-REPORT
   s" native file tree share artifact: " type s" tree-share.txt" AT type cr ;
;package
;using

NATIVE-FILE-TREE-SHARE-TEST:MAIN
