require test/gate-stdlib-lib.f

STDLIB-GATE:MAIN
using TEST
SUITE release test/gate-images-orch-release.f ;SUITE
SUITE consumer test/gate-images-orch-consumer.f ;SUITE
SUITE later test/gate-images-orch-later.f ;SUITE
RUN
;using
