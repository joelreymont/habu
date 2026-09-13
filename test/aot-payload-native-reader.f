\ This process never loads the producer's source or its live definitions.
require tools/native-emit.f
require lib/fs-mutate.f
require lib/codesign.f

package PAYLOAD-NATIVE-READER
create KEY 32 allot

: RUN ( -- )
   s" PAYLOAD-NATIVE:BUMP" EFFECT-QUERY if 79 throw then
   2 SCRIPT-ARGV$ KEY SHA256-FILE 0<> if 79 throw then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   \ File identity verifies the bytes; it does not invent an origin claim.
   AOT-FILE:OWN dup NATIVE-LAYOUT:CURRENT 1 SCRIPT-ARGV$ NATIVE-EMIT:WRITE
   AOT-OWNED:CLOSE
   1 SCRIPT-ARGV$ CHMOD-X
   1 SCRIPT-ARGV$ CODESIGN:ENSURE
   s" native graph artifact baked" type cr ;

RUN
;package
