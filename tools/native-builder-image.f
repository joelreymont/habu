\ Explicit saved native builder. Automatic cache selection is separate.
1 set-tier
require src/habu/app-image.f
require tools/native-build-core.f

package NATIVE-BUILDER-IMAGE
: LOAD-ARM-WRITER ( -- )
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" tools/native-emit.f" required
   then ;
' LOAD-ARM-WRITER
;package
execute

package NATIVE-BUILDER-IMAGE
private

: ORIGIN ( n n -- n ) code-origin ;

: ENTER ( -- )
   ['] ORIGIN false NATIVE-BUILD:RUN-IMAGE-DEFAULT ;

' ENTER
;package
APP-IMAGE:START!
