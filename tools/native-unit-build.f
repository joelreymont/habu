\ Explicit checked-package native build: tier 1 precedes every dependency.
1 set-tier
require lib/executable-build.f

package NATIVE-UNIT-BUILD-ENTRY
private

\ The dynamically loaded driver receives this already typed code reference.
: ORIGIN ( n n -- n ) code-origin ;

\ The required driver is loaded inside the protected executable-build scope.
\ This small source-load boundary resolves its entry only after require returns.
TRUSTED: BUILD ( -- )
   s" tools/native-unit-build-core.f" required
   ['] ORIGIN 0 0= 0= s" NATIVE-BUILD:RUN-UNIT" evaluate ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
