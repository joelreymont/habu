\ Source-built image writer shared by partial-capture fixtures. One output
\ argument emits an empty cold engine; three also name an artifact and its
\ producer. The file reader verifies that producer before the normal writer
\ imports the owned capture. Neither disk identity nor this copy claims a tier
\ for the imported code.
1 set-tier
require tools/native-emit.f
require lib/fs-mutate.f
require lib/codesign.f

package NATIVE-FIXTURE-WRITE

create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

: WRITER-NATIVE ( -- )
   s" NATIVE-EMIT:WRITE" XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if 79 throw then
   rec XREF-START dup rec XREF-LEN + code-origin 1 <> if
      s" native-fixture: writer is not optimizing native code" 79 die then ;

: READ-ARTIFACT ( -- )
   SCRIPT-ARGC 1 = if exit then
   SCRIPT-ARGC 3 <> if
      s" native-fixture: expected output [artifact producer]" 64 die then
   FSHA-CTX 2 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if 79 throw then
   KEY 1 SCRIPT-ARGV$ AOT-FILE:READ ;

: WRITE-OWNED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup NATIVE-LAYOUT:CURRENT 0 SCRIPT-ARGV$ NATIVE-EMIT:WRITE ;

: RUN ( -- )
   WRITER-NATIVE
   READ-ARTIFACT
   AOT-FILE:OWN ['] WRITE-OWNED catch {: rc:n :}
   AOT-OWNED:CLOSE
   rc 0<> if rc throw then
   0 SCRIPT-ARGV$ CHMOD-X
   0 SCRIPT-ARGV$ CODESIGN:ENSURE ;

RUN
;package
