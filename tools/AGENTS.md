# tools — commands, build drivers, checks

Every tool is checked Habu run as `bin/hb --load tools/<name>.f -- <args>`.
`<name>.f` is the command; `<name>-main.f` is the entry where `<name>.f` is
also a library; `-core.f` and `-lib.f` hold what tests and other tools reuse;
`-test.f` is its focused test.

- Builds: `hb-build.f` (hb under `--engine`, applications, Wasm modules under
  `--target wasm`).
- Recovery: `seed-main.f` (`docs/bootstrap.md` "Recovery").
- Convergence: `chain-run-build.f`.
- Profiling a build: `build-profile.f`.
- Checks: `check.f`, the `*-lint.f` entries over `lint/`, `lsp.f` and `lsp-*.f`.
- Inspection: `engine-size.f`, `imagedisasm.f`, `jitdump.f`.
- `unicode/` generates and verifies `src/lib/unicode/class-data.f`.
