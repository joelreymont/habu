\ Explicit saved native builder. Automatic cache selection is separate.
1 set-tier
require src/habu/app-image.f
require tools/native-build-core.f
require tools/native-emit.f

package NATIVE-BUILDER-IMAGE
private

: ORIGIN ( n n -- n ) code-origin ;

: ENTER ( -- )
   ['] ORIGIN false ['] NATIVE-EMIT:WRITE NATIVE-BUILD:RUN-IMAGE ;

' ENTER
;package
APP-IMAGE:START!
