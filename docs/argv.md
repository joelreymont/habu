# Habu Tool Argv

`tools/argv.f` is a small parser for native `bin/hb tool.f args...` scripts.
Load it before the tool body, then call `ARGV-PARSE`.

The native engine does not have an `include` loader, so focused tests and
single-file tools concatenate the helper ahead of the tool source:

```sh
cat tools/argv.f my-tool.f > "$HB_TMP/my-tool.with-argv.f"
bin/hb "$HB_TMP/my-tool.with-argv.f" --json --label NAME -o out file.f
```

## Supported Options

- `--json`
- `--label NAME`
- `--strict-signatures`
- `--all-errors`
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
- `ARGV-JSON?`, `ARGV-STRICT-SIGNATURES?`, `ARGV-ALL-ERRORS?` return flags.
- `ARGV-LABEL$`, `ARGV-OUT$`, and `ARGV-POS$ ( idx -- a u )` return parsed strings.
- `ARGV-PATHZ`, `ARGV-POSZ`, and `ARGV-OUTZ` return NUL-terminated scratch paths.

