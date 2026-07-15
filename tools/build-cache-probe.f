\ build-cache-probe.f - child fixture for canonical cache-root resolution.

require lib/build-cache.f
require lib/process.f

package BUILD-CACHE-PROBE

: QUIET? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" --quiet" STR= else 0 0= 0= then ;

: EXPECT-PATH-ERROR? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" --expect-path-error" STR= else 0 0= 0= then ;

: RESOLVE-DROP ( -- )
   BUILD-CACHE:RESOLVE drop 2drop ;

: EXPECT-PATH-ERROR ( -- )
   [: RESOLVE-DROP ;] catch
   dup 0= if drop E-BUILD-PATH throw then
   dup E-BUILD-PATH <> if throw then
   drop
   s" E-BUILD-PATH" type 9 emit
   BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ type 9 emit
   BUILD-CACHE:SELECTED-ROOT$ type 9 emit
   BUILD-CACHE:CAUSE$ type cr ;

: MAIN ( -- )
   EXPECT-PATH-ERROR? if EXPECT-PATH-ERROR exit then
   BUILD-CACHE:RESOLVE
   QUIET? if drop 2drop exit then
   BUILD-CACHE:SOURCE$ type 9 emit type cr ;

MAIN

;package
