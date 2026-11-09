\ power.f - keep the machine awake while work runs, package POWER. KEEP-AWAKE
\ asks the OS to hold off idle system sleep under a name the OS lists among its
\ power assertions, and answers a linear POWER:awake; ALLOW-SLEEP consumes it
\ and lifts the hold. The display may still sleep. On macOS the hold is an
\ IOKit power assertion (src/lib/power/macos.f), which the OS also lifts when
\ the process exits; on any other target KEEP-AWAKE throws E-PLATFORM.
\ STORAGE CLASS. CALLER-OWNED: each token holds its assertion id.
\ PROCESS-WIDE: the backend hooks, bound when this file loads and only read
\ afterwards.
require src/lib/error.f

package POWER

private
-9640 constant E-FIRST
-9649 constant E-LAST
public
-9640 constant E-PLATFORM    \ no keep-awake backend on this target
-9641 constant E-ASSERTION   \ the OS refused to create or release the hold, a name that is not UTF-8 included

DEFLINEAR POWER:awake

private

LINEAR: >AWAKE ( n -- POWER:awake )
LINEAR: AWAKE> ( POWER:awake -- n )

defer KEEP-HOOK ( ptr u8 n -- n )
defer ALLOW-HOOK ( n -- )

: NO-KEEP ( ptr u8 n -- n ) 2drop E-PLATFORM throw ;
: NO-ALLOW ( n -- ) drop E-PLATFORM throw ;

: INSTALL-NONE ( -- )
   ['] NO-KEEP is KEEP-HOOK
   ['] NO-ALLOW is ALLOW-HOOK ;
INSTALL-NONE

public

\ The name is UTF-8 text the OS shows beside the hold.
: KEEP-AWAKE ( ptr u8 n -- POWER:awake )
   KEEP-HOOK >AWAKE ;

\ The token is spent before the release's outcome is known: a refused release
\ throws E-ASSERTION with no token left to retry.
: ALLOW-SLEEP ( POWER:awake -- )
   AWAKE> ALLOW-HOOK ;

;package

HB-TARGET-MACOS? [if] require src/lib/power/macos.f [then]
