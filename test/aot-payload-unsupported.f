\ Exceptional quotation payloads refuse at the target's supported boundary.
require test/aot-payload-unsupported-lib.f

package PAYLOAD-EXCEPTION-SUITE
public
: RUN ( -- ) PAYLOAD-UNSUPPORTED-SUITE:RUN-EXCEPTION ;
RUN
;package
