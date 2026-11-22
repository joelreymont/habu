\ allow-no-package.f - admitting a name no package owns is refused.
require src/lib/vocabulary.f

: RUN ( -- ) s" NOPE" >str VOCABULARY:ALLOW-PACKAGE ;
RUN
