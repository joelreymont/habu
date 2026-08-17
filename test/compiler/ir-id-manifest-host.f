\ ir-id-manifest-host.f - test/compiler/ir-id-manifest.f on the capture host.
\
\ SUBJECT: source-loading the chain. The manifest proves the frozen identity
\ schema against the live registry AS id.f's own load declares it - thirteen
\ families, once each, in declaration order. Seeded, the require is a no-op
\ and the bracket sees nothing (E-CID-FAMILY); the host performs the load the
\ manifest describes.
require test/host-run-lib.f
s" test/compiler/ir-id-manifest.f" HOST-RUN:HOST-RUN
