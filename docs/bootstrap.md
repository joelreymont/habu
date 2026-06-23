# Bootstrap

`bin/hb` is generated and ignored. It is the only installed native build output.
A checkout without `bin/hb` uses Gforth only to create private bootstrap
artifacts under `HB_TMP`; those artifacts exist only to produce `bin/hb`.

## Requirements

- macOS ARM64.
- Gforth with `{:` locals support. Homebrew `gforth` 0.7.3 is too old.
  A current Gforth snapshot such as `0.7.9_20260610` works.

Verify the Gforth requirement:

```sh
printf ': f {: a :} a . ; 1 f bye\n' | gforth
```

That command must print `1` and exit zero. If the usable Gforth is not first on
`PATH`, set `GFORTH=/path/to/gforth`.

## No-Binary Recovery

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth-fast tools/bootstrap.sh
```

`tools/bootstrap.sh` does the whole recovery and installs exactly one file:
`bin/hb`.

1. validates that Gforth supports `{:` locals;
2. uses `test/nf.fs` and `bootstrap/` to create private bootstrap executables in
   `HB_TMP`;
3. uses those private executables to produce `bin/hb`;
4. runs the normal `bin/hb` self-refresh so the installed binary is rebuilt from
   current source and reaches the byte-for-byte fixpoint.

The temporary files are not build products. The final installed `bin/hb` is the
native checked engine rebuilt from current source.

## Refresh `bin/hb`

After `bin/hb` exists, do not use Gforth for normal work:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/build.f \
  lib/codesign.f tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
```

Run the gate after bootstrap or refresh:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
```
