\ This process never loads the producer's source or its live definitions.
require tools/native-emit.f
require src/lib/fs-mutate.f
require src/lib/codesign.f

package PAYLOAD-NATIVE-READER
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot

: RUN ( -- )
   s" PAYLOAD-NATIVE:BUMP" EFFECT-QUERY if 79 throw then
   FSHA-CTX 2 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if 79 throw then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   \ File identity verifies the bytes; it does not invent an origin claim.
   AOT-FILE:OWN dup NATIVE-LAYOUT:CURRENT 1 SCRIPT-ARGV$ NATIVE-EMIT:WRITE
   AOT-OWNED:CLOSE
   1 SCRIPT-ARGV$ FS:CHMOD-X
   1 SCRIPT-ARGV$ CODESIGN:ENSURE
   s" native graph artifact baked" type cr ;

RUN
;package
