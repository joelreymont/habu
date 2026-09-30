\ allow-no-package.f - admitting a name no package owns is refused.
require lib/policy.f

: RUN ( -- ) s" NOPE" POLICY:ALLOW ;
RUN
