# Habu Tool Argv

`lib/argv.f` is the checked parser for native `bin/hb tool.f args...` scripts.
`tools/argv.f` is a compatibility path to the same module. Load either path
before the tool body, then call `ARGV-PARSE`.

For multi-file tools, pass every source after `--load` and before `--`;
`SCRIPT-ARGV$` starts after that separator. `--load` is explicit file-source
mode, so non-tty stdin is not consumed by startup and remains available to the
loaded tool:

```sh
bin/hb --load lib/argv.f my-tool.f -- --json --label NAME -o out file.f
bin/hb --load tools/argv.f my-tool.f -- --json --label NAME -o out file.f
printf DATA | bin/hb --load lib/source.f my-tool.f -- arg
```

`--build` is reserved for verified compiler payloads emitted by
`tools/build-fixpoint.f` and `tools/hb-build-lib.f`. It uses the same
source-list/separator argv convention, but leaves the friend arena open only
for the certified compiler prefix; every generated payload executes
`SEAL-FRIEND` before its driver. Application and tool sources use `--load`.

## Supported Options

- `--json`
- `--json-errors`
- `--label NAME`
- `--strict-signatures`
- `--all-errors`
- `--strict-boundary`
- `-o OUT`
- `--` to stop option scanning

Unknown flags, missing option values, required-output/label failures, and
positional-count failures throw `ARGV-E-USAGE` (`64`). If uncaught, the process
exits 64.

## Common Words

- `ARGV-USAGE! ( a u -- )` sets the usage line printed on failures.
- `ARGV-LABEL-DEFAULT! ( a u -- )` and `ARGV-OUT-DEFAULT! ( a u -- )` set defaults.
- `ARGV-PARSE ( -- )` scans `SCRIPT-ARGV$`.
- `ARGV-EXPECT-POS ( lo hi -- )` validates positional count; `hi < 0` means unbounded.
- `ARGV-EXPECT-POS-EXACT ( n -- )` validates an exact positional count.
- `ARGV-REQUIRE-OUT ( -- )` and `ARGV-REQUIRE-LABEL ( -- )` require those options.
- `ARGV-JSON?`, `ARGV-STRICT-SIGNATURES?`, `ARGV-ALL-ERRORS?`, and
  `ARGV-STRICT-BOUNDARY?` return flags.
- `ARGV-LABEL$`, `ARGV-OUT$`, and `ARGV-POS$ ( idx -- a u )` return parsed strings.
- `ARGV-PATHZ`, `ARGV-POSZ`, and `ARGV-OUTZ` return NUL-terminated scratch paths.
