\ allow-no-package.f - admitting a name no package owns is rejected.
require src/lib/vocabulary.f

: RUN ( -- ) s" NOPE" VOCABULARY:ALLOW-PACKAGE ;
RUN
