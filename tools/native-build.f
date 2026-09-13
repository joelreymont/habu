\ Production executable build: tier 1 precedes every tool dependency.
1 set-tier
require lib/executable-build.f

package NATIVE-BUILD-ENTRY
private

\ The required driver is loaded inside the protected executable-build scope.
\ This small source-load boundary resolves its entry only after require returns.
TRUSTED: BUILD ( -- )
   s" tools/native-build-core.f" required
   [: code-origin ;] 0 0= 0= s" NATIVE-BUILD:RUN" evaluate ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
