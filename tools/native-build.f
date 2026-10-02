\ Production executable build: tier 1 precedes every tool dependency.
1 set-tier
\ A refused command line stops here, before BUILD below loads the whole native
\ compiler to reach the driver's own check.
require tools/native-build-args.f
NATIVE-BUILD:BUILD-ARGS!
require lib/executable-build.f

package NATIVE-BUILD-ENTRY
public

\ The dynamically loaded driver receives this code reference. BUILD's text
\ runs after this package closes, so it names the word qualified.
: ORIGIN ( n n -- n ) code-origin ;

private

\ The required driver is loaded inside the protected executable-build scope.
\ The entry is named in a text because it resolves only after require returns.
: BUILD ( -- )
   s" tools/native-build-core.f" required
   s" ' NATIVE-BUILD-ENTRY:ORIGIN false NATIVE-BUILD:RUN" evaluate-closed ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
