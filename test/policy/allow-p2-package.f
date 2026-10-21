\ allow-p2-package.f - a package with a public word spelled like a row only
\ pass 2 dispatches cannot be admitted either.
require src/lib/vocabulary.f

package PTUCK
public
: TUCK ( -- n ) 7 ;
;package

: RUN ( -- ) s" PTUCK" VOCABULARY:ALLOW-PACKAGE ;
RUN
