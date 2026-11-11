\ allow-seal-keyword-package.f - the interpreter reads seal-package as the
\ package keyword before any lookup, so a package publishing it is refused.
require src/lib/vocabulary.f

package PSEAL
public
: seal-package ( -- ) ;
;package

: RUN ( -- ) s" PSEAL" VOCABULARY:ALLOW-PACKAGE ;
RUN
