\ Fresh-process side of native-source-view-e2e.f. The child runs in its
\ private source root, which also links the checkout's compiler sources.

1 set-tier
require lib/executable-build.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/image-lifecycle.f
require tools/native-build-core.f
require tools/native-source-view.f

package SOURCE-VIEW-PROOF

create KEY-BUF 64 allot

: MISSING ( -- ) s" missing.f" SOURCE-VIEW:COLLECT ;
: LATE ( -- ) s" after-image.f" script-required ;

: MUTATE ( -- )
   s" dep.f" S\" package SVDEP\npublic\n: VALUE ( -- n ) 99 ;\n;package\n" WRITE-ALL
   s" nest/dep.f" S\" package SVDEP\npublic\n: VALUE ( -- n ) 77 ;\n;package\n" WRITE-ALL
   s" after-image.f"
      S\" package SVAFTER\npublic\n: VALUE ( -- n ) 123 ;\n;package\n"
      WRITE-ALL ;

public

TRUSTED: VALUE ( -- n ) s" SVTEST:VALUE" evaluate ;

: PREPARE ( -- )
   T-RESET
   SOURCE-VIEW:OPEN
   [: MISSING ;] E-FS-OPEN TTHROWSQ
   SOURCE-VIEW:CLOSE
   SOURCE-VIEW:OPEN
   s" tools/native-build.f" SOURCE-VIEW:COLLECT
   s" nest/entry.f" SOURCE-VIEW:COLLECT
   SOURCE-VIEW:KEY {: key:ptr size:n :}
   size 64 T=
   key KEY-BUF size BYTE-COPY
   SOURCE-VIEW:USE
   MUTATE
   SOURCE-VIEW:KEY KEY-BUF 64 T$=
   [: LATE ;] E-BUILD-SOURCE TTHROWSQ
   s" nest/entry.f" script-required
   VALUE 42 T= ;

;package

\ Reopen the actual driver package so the second half uses its production
\ logical reset and source load. These retained calls survive the reset.
package NATIVE-BUILD

\ The target window answers each of these words as a code address integer.
CAST: TARGET-LOAD-XT ( n -- [ ptr u8 n -- ] )
CAST: SOURCE-USE-XT ( n -- [ [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- ] )
CAST: SOURCE-UNIT-USE-XT ( n -- [ [ ptr u8 n ptr u8 n ptr u8 [ -- ] -- ] -- ] )

\ This fixture enters the private compiler directly, so it supplies the same
\ explicit source policy that a build entry installs before logical reset.
: BIND-OWNED-SOURCE ( -- )
   1 TARGET-SOURCE-BOUND !
   SOURCE-VIEW:CALLBACKS
   s" SOURCE-INPUT:USE" OPEN-TARGET-XT SOURCE-USE-XT execute
   SOURCE-VIEW:LOAD-CALLBACK
   s" SOURCE-UNIT:USE" OPEN-TARGET-XT SOURCE-UNIT-USE-XT execute ;

: TARGET-ENTRY ( -- )
   s" nest/entry.f" s" script-required" TARGET-XT TARGET-LOAD-XT execute ;

: TARGET-LATE ( -- )
   s" after-image.f" s" script-required" TARGET-XT TARGET-LOAD-XT execute ;

public

: SOURCE-VIEW-PROOF ( -- )
   SOURCE-VIEW-PROOF:PREPARE
   ['] BIND-OWNED-SOURCE ['] SOURCE-NOOP ['] SOURCE-VIEW:CLOSE SOURCE-POLICY!
   CHECKER-OWNER LOGICAL-RESET
   OPEN-AND-COMPILE
   TARGET-ENTRY
   [: TARGET-LATE ;] E-BUILD-SOURCE TTHROWSQ
   SOURCE-VIEW-PROOF:VALUE 42 T=
   RESET-TARGET-SOURCE
   SOURCE-VIEW:CLOSE
   SOURCE-VIEW:OPEN
   s" nest/entry.f" SOURCE-VIEW:COLLECT
   SOURCE-VIEW:USE
   IMAGE-LIFECYCLE:PREPARE
   SOURCE-VIEW:READY? 0= TTRUE
   s" after-image.f" script-required
   T-REPORT
   s" source-view: ok" type cr ;

;package

' NATIVE-BUILD:SOURCE-VIEW-PROOF
EXECUTABLE-BUILD:WITH
