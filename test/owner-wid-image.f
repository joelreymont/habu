\ owner-wid-image.f - isolated test-image composition for the cold owner proof.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/vector.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require tools/date.f
require tools/build-fixpoint.f

package OWNER-WID-IMAGE

create ROOT-BUF FS-PATH-CAP allot
create HB-BUF FS-PATH-CAP allot
variable ROOT-U
variable HB-U

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: CANDIDATE-ENGINE ( -- )
   s" HABU_UNDER_TEST" GETENV dup 0 > if BF-ENGINE! else 2drop then ;

: BUILD-IMAGE ( -- )
   BF-PIN-RESET BF-PIN-ON!
   BF-RECORD-RESET
   BUILD-EXT:OWNER-WID-STDIN ;

: BUILD-RESET ( -- )
   BF-RECORD-RESET
   BF-PIN-OFF!
   BF-PIN-RESET
   BF-ENGINE-RESET
   BF-TMP-RESET ;

: BUILD-ACT ( -- )
   ROOT-BUF ROOT-U @ BF-TMP!
   CANDIDATE-ENGINE
   BUILD-IMAGE ;

public

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: HB$ ( -- ptr u8 n )
   ROOT s" hb-stdin" HB-BUF JOIN-PATH HB-U !
   HB-BUF HB-U @ ;

: BUILD ( -- )
   s" habu-owner-wid-image" TMPDIR-MKDIR ROOT!
   ROOT CLEANUP-TREE+
   [: BUILD-ACT ;] catch {: code:n :}
   BUILD-RESET
   code 0 <> if code throw then ;

;package
