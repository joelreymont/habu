\ ir-id-host.f - test/compiler/ir-id.f on the capture host.
\
\ SUBJECT: source-loading the chain. The fixture brackets TFAM-N and the
\ dictionary around its require of src/compiler/ir/id.f (BEFORE/AFTER,
\ FAMILY-SURFACE), so the deltas it measures exist only where that require
\ actually reads the file. The product provides it; the host loads it.
require test/host-run-lib.f
s" test/compiler/ir-id.f" HOST-RUN:HOST-RUN
