\ Explicit checked-package native build: tier 1 precedes every dependency.
1 set-tier
require lib/executable-build.f

package NATIVE-UNIT-BUILD-ENTRY
public

\ The dynamically loaded driver receives this code reference. BUILD's text
\ runs after this package closes, so it names the word qualified.
: ORIGIN ( n n -- n ) code-origin ;

private

\ The required driver is loaded inside the protected executable-build scope.
\ The entry is named in a text because it resolves only after require returns.
: BUILD ( -- )
   s" tools/native-unit-build-core.f" required
   s" ' NATIVE-UNIT-BUILD-ENTRY:ORIGIN false NATIVE-BUILD:RUN-UNIT" evaluate-closed ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
