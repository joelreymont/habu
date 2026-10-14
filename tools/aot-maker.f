\ Fixed AOT maker support. The saved image carries the checked opener and
\ saver, while each invocation loads the linker after the application window.
1 set-tier
require src/habu/aot-window-latch.f

package AOT-MAKER

TYPED-VARIABLE OPEN-BODY [ -- ]
TYPED-VARIABLE SAVE-BODY [ ptr u8 n -- ]

;package

\ Everything an application can require is temporary. Both continuations
\ bind to its code before the dictionary and checker rows are retired.
CHECKER-SCOPE-START
REQUIRE-REG:COUNT
: AOT-MAKER-SUPPORT ( -- ) ;
require src/lib/executable-build.f
require src/lib/fs.f
require src/os/script-argv.f
require src/lib/ffi-abi.f

: AOT-MAKER-LOAD-SUPPORT ( -- )
   [: s" tools/native-file-cache.f" required ;]
      FFI:WITH-MAKER-DECLARATIONS ;
AOT-MAKER-LOAD-SUPPORT
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
      s" aot-build: source, JSON flag and optional entry/seed are required" 74 die
   then
   SCRIPT-ARGC 3 = if
      2 SCRIPT-ARGV$ JSON-FLAG? 0= if
         s" aot-build: count flag must be 1" 74 die
      then
   then
   1 SCRIPT-ARGV$ JSON-FLAG? DIAG-JSON! ;

: OPEN-INNER ( -- )
   RESTORE-ROOT
   OPEN-OPTIONS
   NCOMP:SESSION-LIVE? if IR-CTX:SESSION-CLOSE then
   \ The saved module and every surviving maker definition sit below this
   \ watermark. The application is the first owner of the open window.
   ndict@ OPENER-NDICT !
   AOT-DATA-START
   0 SCRIPT-ARGV$ s" aot"
   SCRIPT-ARGC 3 = if
      NFILE-CACHE:LOAD
   else
      NULL-PTR 0 NFILE-CACHE:LOAD-WITH-INPUTS-QUIET
   then
   AOT-DATA-SPAN ;

: OPEN-IN-SCOPE ( -- )
   ['] OPEN-INNER EXECUTABLE-BUILD:WITH ;

: SAVE-IN-SCOPE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   rootu FS-PATH-CAP > if E-FS-CAPACITY throw then
   root SAVED-ROOT rootu BYTE-COPY
   rootu SAVED-ROOT-U !
   NFILE-CACHE:CAPTURE-PRODUCER
   path pathu APP-IMAGE:SAVE ;

' OPEN-IN-SCOPE
' SAVE-IN-SCOPE
;package

package AOT-MAKER
private
SAVE-BODY !
OPEN-BODY !
;package

s" AOT-MAKER-SUPPORT" HIDE-DEFS-FROM
CHECKER-SCOPE-DONE
REQUIRE-REG:TRUNCATE

package AOT-MAKER
public

: OPEN ( -- ) OPEN-BODY @ execute ;

: RUN ( -- )
   s" tools/aot-build.f" required
   s" AOT-LINK:BUILD-NATIVE" evaluate-closed ;

\ Invoke from outer stdin after the maker include has returned. The capture
\ can reset source history only after SAVE-IN-SCOPE has recorded its prefix.
: SAVE ( ptr u8 n -- ) SAVE-BODY @ execute ;

;package
