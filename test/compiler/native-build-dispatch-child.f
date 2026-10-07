\ A refused pre-hook source returns through the build's catch with the retained
\ compiler dispatch restored. The enclosing test supplies a private source tree.
1 set-tier
require lib/executable-build.f
require tools/native-build-core.f

package NATIVE-BUILD-DISPATCH-CHILD
private

: ORIGIN ( n n -- n ) code-origin ;

: DRIVE ( -- n )
   0 SCRIPT-ARGV$ ['] ORIGIN false NATIVE-BUILD:RUN-PATH-RC ;

public
: RUN ( -- )
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ {: previous:n :}
   ['] DRIVE EXECUTABLE-BUILD:WITH 70 <> if
      s" native-build-dispatch: expected checked-source refusal" 76 die
   then
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ previous <> if
      s" native-build-dispatch: compiler dispatch changed" 76 die
   then
   s" native-build-dispatch: ok" type cr ;

;package

NATIVE-BUILD-DISPATCH-CHILD:RUN
