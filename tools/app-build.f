\ Build a REPL application with native compilation held through its source load.
1 set-tier
require src/habu/app-image.f
require src/os/script-argv.f

package APP-BUILD
private

\ MAIN exists only after the application has loaded. This fixed source defines
\ its startup continuation then passes its typed execution token to START!.
TRUSTED: STARTUP ( -- )
   s" package APP-BUILD-STARTUP : ENTER ( -- ) MAIN ; ' ENTER ;package APP-IMAGE:START!" evaluate ;

: BUILD ( -- )
   SCRIPT-ARGC 2 <> if
      s" app-build: source and output paths are required" 74 die
   then
   0 SCRIPT-ARGV$ script-required
   STARTUP
   1 SCRIPT-ARGV$ APP-IMAGE:SAVE ;

public

\ Invoke from the outer stdin stream, after this file's include has returned.
: RUN ( -- ) ['] BUILD EXECUTABLE-BUILD:WITH ;

;package
