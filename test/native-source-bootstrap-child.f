\ Fresh target loader must refuse a source load within include.f, before its
\ caller activates the owned source input. The parent supplies a private copy.

1 set-tier
require lib/executable-build.f
require lib/fs.f
require tools/native-build-core.f
require tools/native-source-view.f

package NATIVE-BUILD

public

: SOURCE-BOOTSTRAP-PROOF ( -- )
   SOURCE-VIEW:OPEN
   s" tools/native-build.f" SOURCE-VIEW:COLLECT
   SOURCE-VIEW:USE
   s" bootstrap-late.f" S\" s\" bootstrap-read\" type\n" WRITE-ALL
   CHECKER-OWNER LOGICAL-RESET
   OPEN-AND-COMPILE ;

;package

' NATIVE-BUILD:SOURCE-BOOTSTRAP-PROOF
EXECUTABLE-BUILD:WITH
