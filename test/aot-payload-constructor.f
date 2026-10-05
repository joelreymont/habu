\ A dynamic constructor's process-local identity cannot enter an artifact.
require test/aot-payload-unsupported-lib.f

package PAYLOAD-CONSTRUCTOR-SUITE
public
: RUN ( -- ) PAYLOAD-UNSUPPORTED-SUITE:RUN-CONSTRUCTOR ;
RUN
;package
