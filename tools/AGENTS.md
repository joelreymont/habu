# tools — commands, build drivers, checks

Every tool is checked Habu run as `bin/hb --load tools/<name>.f -- <args>`.
`bootstrap.sh` is the one shell file: no-binary recovery (`docs/bootstrap.md` "No-Binary Recovery").
`<name>.f` is the command; `<name>-main.f` is the entry where `<name>.f` is
also a library; `-core.f` and `-lib.f` hold what tests and other tools reuse;
`-test.f` is its focused test.

- Builds: `hb-build.f` (applications), `native-build.f` (the engine),
  `build-fixpoint.f` (self-rebuild), `native-emit.f`, `wasm-build.f`, `seed.f`.
- Convergence: `two-generation-*.f`, `ddc-verify.f` and `ddc-drive.f`.
- Checks: `check.f`, the `*-lint.f` entries over `lint/`, `lsp.f` and `lsp-*.f`.
- Inspection: `engine-size.f`, `imagedisasm.f`, `jitdump.f`, `why-threw.f`.
- `unicode/` generates and verifies `src/lib/unicode/class-data.f`.
- `perf/boot-census.f -- LOGFILE CMD [ARGS...]` (macOS): follows CMD's process
  tree and classifies each child as fork or exec.
