\ diagnose-hb-core.f - explain why bin/hb fails to start outside the repo.
\
\ At cold boot the small bin/hb reopens its baked source-prefix files
\ (src/core/*.f and target/habu sources) RELATIVE TO THE CURRENT DIRECTORY
\ (see src/habu/habu2.f PFX-LOAD-BASE-FILES and the EMIT-SOURCE-READ routine,
\ whose open-error path does `exit(74)` with no message). Any consumer that
\ invokes hb from another directory (odin, editors, CI) then sees an opaque
\ exit 74. This tool checks that prefix set against a root and names the first
\ unresolved file plus the root it was expected under.
\
\ Entry point tools/diagnose-hb.f calls DIAGNOSE:MAIN; this core file only
\ defines the package so tests can drive ROOT!/SCAN without exiting.
\ ROOT precedence (RESOLVE-ROOT): argv[0], else $HABU_ROOT, else current dir.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f

package DIAGNOSE

74 constant MISS-RC

create ROOT-BUF FS-PATH-CAP allot
create MISS-BUF FS-PATH-CAP allot
create PATH-BUF FS-PATH-CAP allot
variable ROOT-U
variable MISS-U
variable MISS-SET
variable CHECKED

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: MISS$ ( -- ptr u8 n )
   MISS-BUF MISS-U @ ;

: RESET ( -- )
   0 MISS-SET !
   0 MISS-U !
   0 CHECKED ! ;

: RECORD-MISS ( ptr u8 n -- ) {: a:ptr u:n :}
   MISS-SET @ 0 <> if exit then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a MISS-BUF u BYTE-COPY
   u MISS-U !
   -1 MISS-SET ! ;

: PREFIX-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   1 CHECKED +!
   ROOT$ a u PATH-BUF JOIN-PATH {: pu:n :}
   PATH-BUF pu EXISTS? 0= if a u RECORD-MISS then ;

: TARGET-FILES ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" PREFIX-FILE
      s" src/os/linux/layout.f" PREFIX-FILE
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/target.f" PREFIX-FILE
      s" src/os/macos/layout.f" PREFIX-FILE
   then ;

\ Mirrors src/habu/habu2.f PFX-LOAD-BASE-FILES / PFX-LOAD-SCRIPT-ARGV; util.f
\ is first because it is the first source the engine reopens at boot.
: COMMON-FILES ( -- )
   s" src/core/util.f" PREFIX-FILE
   s" src/core/structures.f" PREFIX-FILE
   s" src/core/checker.f" PREFIX-FILE
   s" src/core/render.f" PREFIX-FILE
   s" src/core/check-hook.f" PREFIX-FILE
   s" src/core/structures-effects.f" PREFIX-FILE
   s" src/core/result.f" PREFIX-FILE
   s" src/core/roles.f" PREFIX-FILE
   s" src/core/bytes.f" PREFIX-FILE
   s" src/habu/layout.f" PREFIX-FILE
   s" src/os/env-base.f" PREFIX-FILE
   s" src/core/include.f" PREFIX-FILE
   s" src/core/enums.f" PREFIX-FILE
   s" src/core/exec-vector.f" PREFIX-FILE
   s" src/core/sha256.f" PREFIX-FILE
   s" src/core/combinators.f" PREFIX-FILE
   s" src/habu/xref.f" PREFIX-FILE
   s" src/os/script-argv.f" PREFIX-FILE ;

public

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: SCAN ( -- )
   RESET
   COMMON-FILES
   TARGET-FILES ;

: MISSING? ( -- bool )
   MISS-SET @ 0 <> ;

: MISSING$ ( -- ptr u8 n )
   MISS$ ;

: CHECKED# ( -- n )
   CHECKED @ ;

: RESOLVE-ROOT ( -- )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ ROOT! exit then
   s" HABU_ROOT" GETENV dup 0 > if ROOT! exit then 2drop
   s" ." ROOT! ;

: REPORT-OK ( -- )
   s" diagnose-hb: OK - all baked prefix sources resolve under " type
   ROOT$ type cr ;

: REPORT-MISS ( -- )
   s" diagnose-hb: unresolved baked prefix source: " type MISS$ type cr
   s"   expected root: " type ROOT$ type cr
   s"   bin/hb reopens these sources cwd-relative at cold boot and exits 74" type cr
   s"   with no message when they are absent; run hb from the repo root" type cr
   s"   (the directory containing src/), or set the cwd there." type cr ;

: MAIN ( -- )
   RESOLVE-ROOT
   SCAN
   MISSING? if
      REPORT-MISS
      s" diagnose-hb: unresolved baked prefix source" MISS-RC die
   then
   REPORT-OK ;

end-package
