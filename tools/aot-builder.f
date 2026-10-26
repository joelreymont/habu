\ Fixed AOT builder support. The producer saves the checked opener image, and
\ each invocation loads the linker after the application window.
1 set-tier
require src/habu/aot-window-latch.f
require src/lib/build-input.f

package AOT-BUILDER

TYPED-VARIABLE OPEN-BODY [ -- ]
TYPED-VARIABLE SAVE-BODY [ ptr u8 n -- ]
TYPED-VARIABLE LINK-LOAD [ -- ]
variable USER-MARK
variable SUPPORT-LIVE
TASK:#USER USER-MARK !
true SUPPORT-LIVE !

;package

\ Everything an application can require is temporary. Both continuations
\ bind to its code before the dictionary and checker rows are retired.
CHECKER-SCOPE-START
REQUIRE-REG:COUNT
: AOT-BUILDER-SUPPORT ( -- ) ;
require src/lib/executable-build.f
require src/lib/fs.f
require src/os/script-argv.f
require src/lib/ffi-abi.f

: AOT-BUILDER-LOAD-SUPPORT ( -- )
   [: s" tools/native-definition-cache.f" required ;]
      FFI:WITH-MAKER-DECLARATIONS ;
AOT-BUILDER-LOAD-SUPPORT
require tools/hb-build-core.f
\ app-image.f loads its core through WITH-MAKER-DECLARATIONS as well.
require src/habu/app-image.f

package AOT-LINK
private

create SAVED-ROOT FS-PATH-CAP allot
variable SAVED-ROOT-U

: RESTORE-ROOT ( -- )
   SAVED-ROOT-U @ 0 > if
      SAVED-ROOT SAVED-ROOT-U @ SOURCE-ROOT:ENGINE!
   then
   FS-PATH-CAP 0 ?do 0 SAVED-ROOT i + c! loop
   0 SAVED-ROOT-U ! ;

: JSON-FLAG? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 1 = if a c@ [char] 1 = else false then ;

: OPEN-OPTIONS ( -- )
   SCRIPT-ARGC 2 < SCRIPT-ARGC 4 > or if
      s" aot-build: source and JSON flag are required" 74 die
   then
   SCRIPT-ARGC 4 = if
      2 SCRIPT-ARGV$ s" --donor" STR= 0= if
         s" aot-build: donor option missing" 74 die then
      3 SCRIPT-ARGV$ FILE? 0= if
         s" aot-build: donor image missing" 74 die then
   then
   SCRIPT-ARGC 3 = if
      2 SCRIPT-ARGV$ JSON-FLAG? 0= if
         s" aot-build: count flag must be 1" 74 die
      then
   then
   1 SCRIPT-ARGV$ JSON-FLAG? DIAG-JSON! ;

: PREPARE ( -- )
   RESTORE-ROOT
   OPEN-OPTIONS
   NCOMP:SESSION-LIVE? if IR-CTX:SESSION-CLOSE then
   \ The saved module and every surviving maker definition sit below this
   \ watermark. The application is the first owner of the open window.
   ndict@ OPENER-NDICT !
   AOT-DATA-START ;

: LOAD ( -- )
   0 SCRIPT-ARGV$ s" HABU_AOT_INPUTS" GETENV
   SCRIPT-ARGC 3 = if
      true
   else
      false
   then NDEF-CACHE:APP-LOAD ;

: FINISH ( -- ) AOT-DATA-SPAN ;

: OPEN-INNER ( -- )
   s" aot" RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      ['] PREPARE ['] LOAD ['] FINISH BUILD-CORE:WITH {: rc:n :}
   rc 0<> if rc throw then ;

: OPEN-IN-SCOPE ( -- )
   ['] OPEN-INNER EXECUTABLE-BUILD:WITH ;

: SAVE-IN-SCOPE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   rootu FS-PATH-CAP > if E-FS-CAPACITY throw then
   root SAVED-ROOT rootu BYTE-COPY
   rootu SAVED-ROOT-U !
   path pathu APP-IMAGE:SAVE ;

: LOAD-LINK-SUPPORT ( -- )
   [: s" tools/aot-build.f" required ;]
      FFI:WITH-MAKER-DECLARATIONS ;

\ APP-LOAD wrote the application's rows to the manifest before this session
\ opened and reset them. The link reads its own sources, the runtime among
\ them, so its rows join the manifest too. The path is read before RUN binds
\ the environment observer: the maker's private manifest is not a build input.
: LOAD-LINK-CACHED ( -- )
   s" HABU_AOT_INPUTS" GETENV {: manifest:ptr manifestu:n :}
   ['] LOAD-LINK-SUPPORT NDEF-CACHE:RUN
   manifest NULL-PTR = if exit then
   NFILE-INPUTS:BYTES$ {: bytes:ptr size:n :}
   size 0= if E-NUNIT-PROFILE throw then
   manifest manifestu bytes size APPEND-FILE ;

: LOAD-LINK-IN-SCOPE ( -- )
   s" aot" ['] LOAD-LINK-CACHED NDEF-CACHE:WITH-QUIET ;

' OPEN-IN-SCOPE
' SAVE-IN-SCOPE
' LOAD-LINK-IN-SCOPE
;package

package AOT-BUILDER
private
LINK-LOAD !
SAVE-BODY !
OPEN-BODY !
;package

s" AOT-BUILDER-SUPPORT" HIDE-DEFS-FROM
CHECKER-SCOPE-DONE
REQUIRE-REG:TRUNCATE

package AOT-BUILDER
private

: SAVE-RETIRED ( ptr u8 n -- )
   2drop s" aot-builder: saved image cannot save itself" 74 die ;

: RESTORE-USER ( ptr u8 -- )
   USER-MARK @ swap CELL-VIEW ! ;

\ Direct OPEN and producer SAVE retire the same temporary support once.
: RETIRE-SUPPORT ( -- )
   SUPPORT-LIVE @ 0= if exit then
   false SUPPORT-LIVE !
   ['] SAVE-RETIRED SAVE-BODY !
   [: RESTORE-USER ;] TASK:OWNED-USER-NEXT ;

public

: OPEN ( -- )
   RETIRE-SUPPORT
   OPEN-BODY @ execute ;

: RUN ( -- )
   LINK-LOAD @ execute
   s" AOT-LINK:BUILD-NATIVE" evaluate-closed ;

\ Invoke from outer stdin after the maker include has returned. The producer's
\ saver uses process support through capture; keep its xt only for that call.
\ The saved maker cannot call it or keep its task-user allocation.
: SAVE ( ptr u8 n -- )
   SAVE-BODY @ {: save :}
   RETIRE-SUPPORT
   save execute ;

;package
