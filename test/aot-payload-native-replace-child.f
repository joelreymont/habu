\ Replace a captured parser in this process before the real verifier opens its
\ neutral scope. The subject then names the new dictionary record.
undefine GPARSE
: GPARSE ( -- ) ;
require tools/check-verify-child.f
