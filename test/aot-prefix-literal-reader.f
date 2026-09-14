\ Only the completed artifact crosses from the exited native producer.
require tools/native-emit.f
require lib/fs-mutate.f
require lib/codesign.f

package PREFIX-LITERAL-READER
using AOT-FILE

create KEY 32 allot

: RUN ( -- )
   s" LITERAL-WINDOW:HOLDER" EFFECT-QUERY if 79 throw then
   2 SCRIPT-ARGV$ KEY SHA256-FILE 0<> if 79 throw then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   OWN dup NATIVE-LAYOUT:CURRENT 1 SCRIPT-ARGV$ NATIVE-EMIT:WRITE
   AOT-OWNED:CLOSE
   1 SCRIPT-ARGV$ CHMOD-X
   1 SCRIPT-ARGV$ CODESIGN:ENSURE
   s" prefix-literal: baked" type cr ;

RUN
;package
