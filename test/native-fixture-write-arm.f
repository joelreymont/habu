\ Source-built image writer shared by partial-capture fixtures. One output
\ argument emits an empty cold engine; three also name an artifact and its
\ producer, which the file reader verifies before importing the capture; no tier
\ is claimed for imported code. test/fixture-writer.f builds it once per tree
\ into a keyed image at tier 1 (tools/app-build.f).
require tools/native-emit.f
require src/lib/fs-mutate.f
require src/lib/codesign.f

package NATIVE-FIXTURE-WRITE

create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot

tier@ constant WRITER-TIER

: WRITER-NATIVE ( -- )
   WRITER-TIER 1 <> if
      s" native-fixture: writer is not optimizing native code" 79 die then ;

: READ-ARTIFACT ( -- )
   SCRIPT-ARGC 1 = if exit then
   SCRIPT-ARGC 3 <> if
      s" native-fixture: expected output [artifact producer]" 64 die then
   FSHA-CTX 2 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if 79 throw then
   KEY 1 SCRIPT-ARGV$ AOT-FILE:READ ;

: WRITE-OWNED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup NATIVE-LAYOUT:CURRENT 0 SCRIPT-ARGV$ NATIVE-EMIT:WRITE ;

public

: RUN ( -- )
   WRITER-NATIVE
   READ-ARTIFACT
   AOT-FILE:OWN ['] WRITE-OWNED catch {: rc:n :}
   AOT-OWNED:CLOSE
   rc 0<> if rc throw then
   0 SCRIPT-ARGV$ FS:CHMOD-X
   0 SCRIPT-ARGV$ CODESIGN:ENSURE ;

;package

\ After MAIN returns the image runs stdin as a program, which is how
\ test/aot-wid-build.f forges emitted code.
: MAIN ( -- ) NATIVE-FIXTURE-WRITE:RUN ;
