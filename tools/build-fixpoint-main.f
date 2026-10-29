\ build-fixpoint-main.f - CLI entrypoint for tools/build-fixpoint.f.
\ tools/bootstrap.sh, tools/seed.f and tools/ddc-verify.f load the whole chain in
\ one --load list, so the require is a no-op there; tools/build-fixpoint-refresh.f
\ is the self-contained entry. BUILD-FIXPOINT:BF-CLI exits 124 (PROC-TIMEOUT-RC)
\ for an expired deadline, 74 (BF-BUILD-RC) for any other escaped throw, and 64 for
\ a refused command line.

require tools/build-fixpoint.f

\ Run the CLI with the package CLOSED: a build certifies its generated sources
\ in this process and the checker resolves those names in whatever package
\ scope is open when it runs.
BUILD-FIXPOINT:BF-CLI
