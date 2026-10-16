\ build-fixpoint-main.f - CLI entrypoint for tools/build-fixpoint.f.
\ The entry requires the tool. tools/bootstrap.sh, tools/seed.f and
\ tools/ddc-verify.f compose one --load list for the whole chain, so the tool is
\ already loaded there and the require is a no-op; tools/build-fixpoint-refresh.f
\ is the self-contained entry that also requires the library preamble. Without
\ the preamble, build-fixpoint.f's BF-NEED-PREAMBLE reports the load list it
\ needs (rc 64). BUILD-FIXPOINT:BF-CLI is the fail-closed boundary: any escaped
\ throw is reported on stderr and exits PROC-TIMEOUT-RC (124, src/lib/process.f)
\ when it is a deadline that expired, BF-BUILD-RC (74) otherwise. A refused
\ command line exits 64.

require tools/build-fixpoint.f

\ Run the CLI with the package CLOSED: a build certifies its generated sources
\ in this process and the checker resolves those names in whatever package
\ scope is open when it runs.
BUILD-FIXPOINT:BF-CLI
