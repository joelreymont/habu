\ Build a REPL application with native compilation held through its source load.
1 set-tier
require src/lib/errors.f
require src/lib/fs.f
require src/habu/app-image.f
require src/os/script-argv.f
require tools/hb-build-core.f

package APP-BUILD
private

create SAVED-ROOT PATH-CAP allot
variable SAVED-ROOT-U
create INPUTS-PATH PATH-CAP allot
variable INPUTS-U
variable SAVED-RUN
variable REPORT

\ MAIN exists only after the application has loaded. This fixed source defines
\ its startup continuation then passes its typed execution token to START!.
: STARTUP ( -- )
   s" package APP-BUILD-STARTUP : ENTER ( -- ) MAIN ; ' ENTER ;package APP-IMAGE:START!" evaluate-closed ;

: RESTORE-ROOT ( -- )
   SAVED-ROOT-U @ 0 > if
      SAVED-ROOT SAVED-ROOT-U @ SOURCE-ROOT:ENGINE!
   then
   PATH-CAP 0 ?do 0 SAVED-ROOT i + c! loop
   0 SAVED-ROOT-U ! ;

: INPUTS$ ( -- ptr u8 n )
   1 SCRIPT-ARGV$ {: a:ptr u:n :}
   s" .inputs" {: suffix:ptr suffixu:n :}
   u suffixu + PATH-CAP > if E-FS-CAPACITY throw then
   a INPUTS-PATH u BYTE-COPY
   suffix INPUTS-PATH u + suffixu BYTE-COPY
   u suffixu + INPUTS-U !
   INPUTS-PATH INPUTS-U @ ;

: CLEAR-INPUTS ( -- )
   PATH-CAP 0 ?do 0 INPUTS-PATH i + c! loop
   0 INPUTS-U ! ;

: PREPARE ( -- )
   SCRIPT-ARGC 2 <> if
      s" app-build: source and output paths are required" 74 die
   then
   NCOMP:SESSION-LIVE? if IR-CTX:SESSION-CLOSE then ;

: LOAD ( -- )
   SAVED-RUN @ 0<> if
      0 SAVED-RUN !
      0 SCRIPT-ARGV$ INPUTS$ REPORT @ 0<> NDEF-CACHE:APP-LOAD
      CLEAR-INPUTS
   else
      0 SCRIPT-ARGV$ NULL-PTR 0 true NDEF-CACHE:APP-LOAD
   then ;

: FINISH ( -- )
   STARTUP
   1 SCRIPT-ARGV$ APP-IMAGE:SAVE ;

: BUILD ( bool -- )
   REPORT !
   s" repl" RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      ['] PREPARE ['] LOAD ['] FINISH BUILD-CORE:WITH {: rc:n :}
   rc 0<> if rc throw then ;

public

\ The keyed builder snapshots only its source-root selection. Clear that
\ support path before saving the application so the result carries none of it.
: SAVE-ROOT ( -- )
   SOURCE-ROOT:ENGINE$ {: a:ptr u:n :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a SAVED-ROOT u BYTE-COPY
   u SAVED-ROOT-U !
   ;

\ Invoke from the outer stdin stream, after this file's include has returned.
: RUN ( -- )
   \ APP-IMAGE:SAVE prepares capture after the application's source load.
   \ Preparing here erases SOURCE-PRODUCER's observed preloaded reads before
   \ APP-LOAD includes that history in its FILE object key.
   true ['] BUILD EXECUTABLE-BUILD:WITH ;

: RUN-SAVED ( -- )
   RESTORE-ROOT
   -1 SAVED-RUN !
   false ['] BUILD EXECUTABLE-BUILD:WITH ;

: RUN-SAVED-REPORT ( -- )
   RESTORE-ROOT
   -1 SAVED-RUN !
   true ['] BUILD EXECUTABLE-BUILD:WITH ;

;package
