\ Production executable build: tier 1 precedes every tool dependency.
1 set-tier
\ A refused command line stops here, before BUILD below loads the whole native
\ compiler to reach the driver's own check.
require tools/native-build-args.f
NATIVE-BUILD:BUILD-ARGS!
require lib/executable-build.f

package NATIVE-BUILD-ENTRY
private

\ The dynamically loaded driver receives this already typed code reference.
: ORIGIN ( n n -- n ) code-origin ;

\ The required driver is loaded inside the protected executable-build scope.
\ This small source-load boundary resolves its entry only after require returns.
TRUSTED: BUILD ( -- )
   s" tools/native-build-core.f" required
   ['] ORIGIN 0 0= 0= s" NATIVE-BUILD:RUN" evaluate ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
