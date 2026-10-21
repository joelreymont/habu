\ Source-owned x86-64 fixture emission. The same native build that produces an
\ ordinary full engine supplies its owned runtime capture to this writer; a
\ three-argument invocation merges the verified partial artifact into it.
1 set-tier
require tools/native-build-core.f

package NATIVE-BUILD
private

create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot
TYPED-VARIABLE HOST-P ptr n
variable HOST-N
TYPED-VARIABLE OUT-P ptr u8
variable OUT-U

: ORIGIN ( n n -- n ) code-origin ;

: PRODUCER! ( -- )
   FSHA-CTX 2 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if
      s" native-fixture: cannot hash producer" BUILD-RC die then ;

\ The build has loaded the source writer (tools/native-build-core.f
\ WRITE-OWNED) before it calls WRITER.
: SOURCE-EMIT ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   s" NATIVE-EMIT:WRITE-C2" SOURCE-WRITER-NAMED execute ;

: WRITE-MERGED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup HOST-P @ HOST-N @ OUT-P @ OUT-U @ SOURCE-EMIT ;

: MERGE-WRITER ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   {: host:ptr count:n out:ptr outu:n :}
   host HOST-P ! count HOST-N !
   out OUT-P ! outu OUT-U !
   AOT-FILE:IMPORT
   PRODUCER!
   KEY 1 SCRIPT-ARGV$ AOT-FILE:MERGE
   AOT-FILE:OWN ['] WRITE-MERGED catch {: rc:n :}
   AOT-OWNED:CLOSE
   rc 0<> if rc throw then ;

: WRITER ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   SCRIPT-ARGC 1 = if SOURCE-EMIT exit then
   MERGE-WRITER ;

public
: FIXTURE ( -- )
   SCRIPT-ARGC 1 <> SCRIPT-ARGC 3 <> and if
      s" native-fixture: expected output [artifact producer]" BUILD-RC die then
   0 SCRIPT-ARGV$ ['] ORIGIN false ['] WRITER RUN-PATH-WRITER-RC {: rc:n :}
   s" " rc EXIT-RC die ;

;package

NATIVE-BUILD:FIXTURE
