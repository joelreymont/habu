# Bootstrap

`bin/hb` is generated and ignored. It is the only installed native build output.
A checkout without `bin/hb` uses Gforth only to create private bootstrap
artifacts under `HB_TMP`; those artifacts exist only to produce `bin/hb`.

## Requirements

- macOS ARM64 or Linux AArch64.
- Linux gates require a working devpts setup: `/dev/ptmx`, `/dev/pts`, and PTY
  ioctls must be available to the user running the gate.
- Gforth with `{:` locals support. Homebrew `gforth` 0.7.3 is too old.
  A current Gforth snapshot such as `0.7.9_20260610` works.

Verify the Gforth requirement:

```sh
tmp=$(mktemp)
printf ': f {: a :} a . cr ; 1 f bye\n' > "$tmp"
gforth "$tmp"
rm -f "$tmp"
```

That command must print `1` and exit zero. If the usable Gforth is not first on
`PATH`, set `GFORTH=/path/to/gforth`.

On macOS, a local snapshot build can be used as the recovery host without
installing it:

```sh
curl -L https://github.com/forthy42/gforth/archive/refs/tags/0.7.9_20260610.tar.gz -o gforth-0.7.9_20260610.tar.gz
tar -xzf gforth-0.7.9_20260610.tar.gz
cd gforth-0.7.9_20260610
./autogen.sh
UNSUITABLE_CC=none ./configure --prefix="$HOME/.local/gforth"
make -j"$(sysctl -n hw.ncpu)" gforth-itc gforth-light.fi
```

If `gforth-fast` is not installed, point `GFORTH` at a tiny wrapper around the
snapshot interpreter and image:

```sh
#!/bin/sh
exec /path/to/gforth-0.7.9_20260610/gforth-itc \
  -i /path/to/gforth-0.7.9_20260610/gforth-light.fi "$@"
```

`tools/bootstrap.sh` only requires the `GFORTH` command to pass the locals probe.
It does not require that Gforth was installed globally.

## No-Binary Recovery

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth-or-wrapper tools/bootstrap.sh
```

The script defaults `HABU_TARGET` from the host (`macos-aarch64` or
`linux-aarch64`). Set `HABU_TARGET` explicitly only when the host cannot be
detected.

`tools/bootstrap.sh` does the whole recovery and installs exactly one file:
`bin/hb`.

1. validates that Gforth supports `{:` locals;
2. uses `test/nf.fs` and `bootstrap/` to create private bootstrap executables in
   `HB_TMP` from the same native source layers used by `tools/build-fixpoint.f`;
3. uses those private executables to produce `bin/hb`;
4. runs the normal `bin/hb` self-refresh so the installed binary is rebuilt from
   current source and reaches the byte-for-byte fixpoint.

The temporary files are not build products. The final installed `bin/hb` is the
native checked engine rebuilt from current source.

## Refresh `bin/hb`

After `bin/hb` exists, do not use Gforth for normal work:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
```

`bin/hb --load` selects the host core/checker/env source prefix from the
running binary. Callers load only the libraries and tool source they need.

If a device tool (`maki/eval-device.f`, `maki/gpu.f`, `tools/ptx/*`) errors with a
cryptic missing-primitive name such as `ffi-call-abi`, the running `bin/hb` predates a
native FFI primitive — **refresh it with the command above.** The maki gate guards this:
it loads `lib/ffi.f` (which fails closed on a stale engine) and runs `maki/device-smoke.f`,
a live `cuInit`/`cuDeviceGet` canary, so the break surfaces early at the FFI layer.

Run the gate after bootstrap or refresh:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f \
  test/gate-pool.f test/run.f
```

This is the native port gate. It runs as a checked bounded DAG pool with
private `HB_TMP` roots. It proves the host `bin/hb`, source selection,
checker/lints, self-refresh, engine suite, REPL build, and AOT output for the
current platform. It intentionally does not run LLM benchmark fixtures or
require JavaScript, Python, Rust, TypeScript, or model runtimes.

Checker-heavy gate fixtures bake warm images with `tools/warm-image.f` under
the gate temp root. `hb-check-warm` supports checker/diagnostic slices.
`hb-tools-warm` plus `hb-tools-warm.trust.f` supports tool subprocess fixtures
such as `check-all-errors` and `gate-json-assert`. These are temporary cache
artifacts and must not be committed.

## Future Port Checklist

1. Add one target source seam under `src/os/<target>/` for syscalls, executable
   layout, signing policy, terminal constants, and target metadata. Startup
   argv/envp access stays shared in `src/os/env-base.f`; do not add target
   `env.f` fallbacks.
2. Wire the target into `tools/bootstrap.sh`, `tools/build-fixpoint.f`, and the
   native source-list builders (`src/habu/habu2.f`, `bootstrap/cg/forth.fs`,
   `src/habu/stdin.f`, and `tools/hb-build-lib.f`) so bootstrap, refresh,
   `--load`, baked REPL, and AOT all select the same target prefix automatically
   from the running `bin/hb`.
3. Recover `bin/hb` only if needed with `HABU_ALLOW_BOOTSTRAP=1
   tools/bootstrap.sh`; after that, use only native `bin/hb`.
4. Run the refresh command above and require a byte-for-byte fixpoint.
5. Run the native port gate above on the target machine.
