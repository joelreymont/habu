\ A fresh seeded engine contributes a short, global prefix, then merges the
\ complete compiler-runtime artifact captured by the bundle host. The merged
\ executable is the evidence: its checker is exercised by the bundle driver.
package NATIVE-MAP-SEED-MERGE
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codesign.f
require src/os/script-argv.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/aot-file.f
require tools/native-emit.f

package NATIVE-MAP-SEED-MERGE
using AOT-BUF
private

create SOURCE PATH-CAP allot
variable SOURCE-U
create OUTPUT PATH-CAP allot
variable OUTPUT-U
create KEY 32 allot
create HASH SHA256-FILE-CTX-BYTES allot

: SOURCE$ ( -- ptr u8 n ) SOURCE SOURCE-U @ ;
: OUTPUT$ ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;

: ARG-N ( n -- n )
   SCRIPT-ARGV$ STR>NUMBER? MATCH option
      none OF s" seed merge: invalid count" 76 die ENDOF
      some OF ENDOF
   ;MATCH ;

: DIGIT+ ( n -- ) 48 + SB-APPEND-C ;

: NAME+ ( n -- )
   {: ord:n :}
   100000
   6 0 ?do
      ord over / 10 mod DIGIT+
      10 /
   loop drop ;

: GENERATE ( -- )
   2 SCRIPT-ARGV$ s" seed-prefix.f" SOURCE FS:JOIN-PATH SOURCE-U !
   SOURCE$ s" " FS:WRITE-ALL
   3 ARG-N 0 ?do
      SB-RESET s" 0 constant SG" SB-APPEND
      i NAME+
      S\" \n" SB-APPEND
      SOURCE$ SB$ FS:APPEND-FILE
   loop ;

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-REC-N @ 3 ARG-N <> if s" seed merge: prefix record count" 76 die then
   AOT-DATA-SIZE @ 0 <> if s" seed merge: prefix DATA span" 76 die then
   AOT-WID-SPAN @ 0 <> if s" seed merge: prefix WID span" 76 die then ;

: MERGE ( -- )
   HASH 1 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if
      s" seed merge: cannot hash producer" 76 die then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:MERGE
   AOT-REC-N @ 4 ARG-N <> if s" seed merge: merged record count" 76 die then
   2 SCRIPT-ARGV$ s" hb-seed-merged" OUTPUT FS:JOIN-PATH OUTPUT-U !
   AOT-FILE:OWN
   dup NATIVE-LAYOUT:CURRENT OUTPUT$ NATIVE-EMIT:WRITE-C2
   AOT-OWNED:CLOSE
   OUTPUT$ FS:CHMOD-X
   OUTPUT$ CODESIGN:ENSURE ;

public

: RUN ( -- )
   SCRIPT-ARGC 5 <> if s" seed merge: expected artifact, producer, directory, prefix, total" 64 die then
   GENERATE
   AOT-ARM:WINDOW-OPEN
   SOURCE$ included
   AOT-ARM:WINDOW-CLOSE
   CAPTURE
   AOT-IDENT:RESET SOURCE$ AOT-IDENT:PATH+
   MERGE ;

;using
;package

NATIVE-MAP-SEED-MERGE:RUN
