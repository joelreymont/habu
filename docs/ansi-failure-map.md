# ANSI Failure Map

Source baseline: `docs/ansi-parity-baseline.json` (`tag=baseline-20260205T214222Z`)

## Coverage

- Total failing IDs in baseline: `14`
- Mapped IDs below: `14`
- Unmapped: `0`
- Duplicate assignments: `0`

## Latest Regression Delta

Source: `docs/ansi/results/regression-habu-latest.json`

- Unexpected failures: `0`
- Unexpected IDs: none
- Baseline drift (expected failure now passing): `UnboundVariable`
- Action: refresh baseline after a full-corpus Habu run and keep `UnboundVariable` mapped until then.

## Buckets

### 1) Compiler and Macro Expansion (`P1`)

Owner files:
- `src/compiler/compile.zig`
- `src/interp/repl.zig`
- `lib/stdlib.habu`

Test IDs:
- `DEFINE-COMPILER-MACRO.8`
- `DESTRUCTURING-BIND.ERROR.10`
- `MACROLET.36`
- `UnboundVariable`

### 2) CLOS and Method Combination (`P1`)

Owner files:
- `lib/stdlib.habu`
- `src/runtime/objects.zig`
- `src/compiler/compile.zig`

Test IDs:
- `DEFINE-METHOD-COMBINATION-LONG.11.4`
- `MAKE-LOAD-FORM.ORDER.14`

### 3) Core Runtime Equality and Symbol Semantics (`P2`)

Owner files:
- `src/interp/vm.zig`
- `src/runtime/primitives/symbol.zig`
- `src/runtime/primitives/type.zig`

Test IDs:
- `EQUAL.13`
- `EQUAL.14`
- `MAKE-SYMBOL.11`

### 4) LOOP Expansion Semantics (`P1`)

Owner files:
- `lib/stdlib.habu`

Test IDs:
- `LOOP.1.39`
- `LOOP.1.40`
- `LOOP.1.41`
- `LOOP.1.42`
- `LOOP.1.43`
