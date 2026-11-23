\ allow-no-package.f - admitting a name no package owns is refused.
require src/lib/vocabulary.f

: RUN ( -- ) s" NOPE" VOCABULARY:ALLOW-PACKAGE ;
RUN
