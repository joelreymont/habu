# Deep Code Review Report

Date: 2026-06-13  
Repository: `/Users/joel/Work/habu`  
Reviewed checkout: jj working copy `9d56e5cdf06b` on parent `25bde544`
(`checker: model leave (loop-exit row, both checkers)`)  
Mode: read-only review, followed by this report file creation

## Scope

This review focused on correctness and soundness risks in the current self-hosted
native Forth toolchain:

- native checker soundness and signature verification
- AOT linker relocation and capacity behavior
- runtime semantic parity for core primitives
- gates and lints that should catch the above classes

The default native gate `test/run.sh` was not run during the review because it
invokes `tools/build.sh`, which can rewrite tracked binaries (`bin/hb`,
`bin/hbi`). Read-only lints and the gforth oracle were run.

## Summary

Total findings: 5

| Severity | Count |
| --- | ---: |
| Critical | 2 |
| High | 3 |
| Medium | 0 |
| Low | 0 |

The most important class of issue is checker false-certification: the native
checker currently accepts definitions whose declared effects contradict the
documented type system. The second major class is AOT unsoundness: the AOT
linker strips/copies only a subset of relocation surfaces and has fixed-size
closure tables without bounds checks.

## Findings

### Critical 1: Native checker erases `bool` into generic integer

Files:

- `src/core/checker.f:92`
- `src/core/checker.f:253`
- `src/core/checker.f:264`
- `src/core/checker.f:587`
- `docs/effects.md:32`
- `docs/effects.md:44`

Type: Soundness / false certification

Description:

The native checker has a concrete `CC-BOOL` code, and documentation says `bool`
is distinct from `i64` and comparisons return `bool`. However `TOK-TYPE` maps
single-letter `f` to generic integer `n`:

```forth
u 1 = c 102 = and IF 1 MK-CON ELSE          \ 'f' -> flag = generic int
```

The primitive table uses `f` for comparison results (`0=`, `<`, `f<`, etc.), so
comparison results become generic integer values. In addition, control-flow
tokens consume `a --`, not `bool --`:

```forth
: CF-IF  s" a --" PARSE-SIG ...
: CF-UNTIL s" a --" PARSE-SIG ...
: CF-WHILE s" a --" PARSE-SIG ...
```

That means:

- a body that actually returns a boolean can verify against a declared `i64`
- any concrete value, including `char`, can be used as an `IF` condition
- the native checker is weaker than the documented type system and the
  bootstrap checker model

Reproduction:

```sh
printf ': BAD ( i64 -- i64 ) 0= ;\n0 BAD .\n' | bin/habu
```

Observed output:

```text
-1
```

Expected behavior:

The definition should be rejected because `0=` returns `bool`, not `i64`.

Additional reproduction:

```sh
printf ': C ( char -- i64 ) if 1 else 2 then ;\n65 C .\n' | bin/habu
```

Observed output:

```text
1
```

Expected behavior:

The definition should be rejected because `IF` should consume `bool`, not
`char`.

Impact:

This is a checker soundness defect. It permits programs to certify under
declared contracts that are not the real typed effect. Since the project
positions the checker as the trust boundary for LLM-written code, this is a
critical gap.

Recommendation:

- Make `f` parse to `CC-BOOL`, or stop using `f` in native sig strings and use
  `bool` consistently.
- Change control-flow consumers to require `bool`, not unconstrained `a`.
- Add verifier regressions:
  - `: BAD ( i64 -- i64 ) 0= ;` rejects
  - `: GOOD ( i64 -- bool ) 0= ;` certifies
  - `: C ( char -- i64 ) IF 1 ELSE 2 THEN ;` rejects
  - `: C ( bool -- i64 ) IF 1 ELSE 2 THEN ;` certifies

### Critical 2: Signature parser accepts malformed contracts

Files:

- `src/core/checker.f:303`
- `src/core/checker.f:313`
- `src/core/checker.f:315`
- `src/core/checker.f:340`
- `src/core/checker.f:345`
- `test/t-sh-verify.fs:57`

Type: Soundness / input validation

Description:

The native signature parser consumes required delimiters using `NEXT-SIG-TOK
2drop` without checking that the consumed token is actually the expected token.
For quotations:

```forth
NEXT-SIG-TOK 2drop                            \ consume '--'
...
NEXT-SIG-TOK 2drop                            \ consume ']'
```

For top-level signatures:

```forth
NEXT-SIG-TOK 2drop                             \ consume '--'
```

As a result, malformed signatures can be accepted and interpreted as some other
effect. The test suite currently pins one malformed case as certified:

```forth
T{ s" : M1 ( [ -- ) drop ;" V s\" -1\n" compare 0= -> true }T
```

Reproduction:

```sh
printf ': N ( i64 ) drop ;\n5 N 7 .\n' | bin/habu
```

Observed output:

```text
7
```

Expected behavior:

The definition should be rejected because the declared signature is malformed
and lacks `--`.

Additional reproduction:

```sh
printf ': M ( [ -- ) drop ;\n5 M 7 .\n' | bin/habu
```

Observed output:

```text
7
```

Expected behavior:

The definition should be rejected because the quotation type is unterminated.

Impact:

Malformed effect annotations are accepted as valid contracts. This can hide
typos in declared effects and allow code to certify under an unintended type.
That undermines the "declared effect vs inferred body" verification path.

Recommendation:

- Introduce checked delimiter consumers, for example `EXPECT-SIG "--"` and
  `EXPECT-SIG "]"`, which fail closed on mismatch or EOF.
- Treat malformed signatures as reject, not certified and not uncheckable.
- Replace the current robustness test that expects `-1` for malformed input with
  deterministic rejection assertions.
- Add cases for missing top-level `--`, missing quotation `]`, missing quotation
  inner `--`, and malformed return-stack clauses.

### High 1: AOT miscompiles `S"` string literals

Files:

- `src/habu/habu2.f:609`
- `src/habu/habu2.f:621`
- `src/habu/habu2.f:622`
- `src/habu/aot.f:30`
- `src/habu/aot.f:96`
- `src/habu/aot.f:101`
- `bootstrap/cg/forth.fs:1352`
- `bootstrap/cg/forth.fs:1366`

Type: AOT relocation correctness

Description:

Compile-mode `S"` emits string bytes into the JIT code stream, then pushes the
absolute byte address via `C-LIT`:

```forth
12 CP 0 ADDI
...
11 12 0 ADDI,  C-LIT
```

The AOT linker copies reachable word blobs into a new Mach-O image, but its
relocation pass only recognizes and rewrites direct call stencils:

```forth
movz/movk/movk x16 + blr x16
```

It does not relocate absolute literal pushes that point inside copied blobs.
After AOT, the pushed string address still refers to the builder/JIT address, not
the copied location in the standalone image.

Reproduction:

```sh
tmp=$(mktemp -d /tmp/habu-aot-str.XXXXXX)
prog=$tmp/p.f
out=$tmp/p
printf '%s\n' ': MAIN s" hi" type CR ;' > "$prog"
HB_TMP=$tmp ./tools/hb-build.sh "$prog" -o "$out"
"$out" > "$tmp/out"
od -An -tx1 -c "$tmp/out"
```

Observed output bytes:

```text
0a
\n
```

Expected output bytes:

```text
68 69 0a
h  i  \n
```

Impact:

AOT binaries build and exit successfully while silently producing wrong output
for string literals. This is worse than a linker failure because the default
AOT gate currently tests numeric recursion only, so this class can ship
undetected.

Recommendation:

- Prefer PC-relative addressing for compile-mode string literals, or otherwise
  encode relocatable metadata for each absolute literal.
- Extend AOT relocation beyond call stencils to cover literal-push sequences
  whose target is inside a copied blob.
- Add AOT tests for `S"..." TYPE`, `[CHAR]`, `[']`, `CREATE` data, and any other
  feature that can embed an address.

### High 2: AOT closure and relocation tables overflow without a guard

Files:

- `src/habu/aot.f:48`
- `src/habu/aot.f:50`
- `src/habu/aot.f:59`
- `src/habu/aot.f:64`

Type: Bounds checking / linker robustness

Description:

AOT uses several fixed 256-entry arrays:

```forth
create CLO 256 cells allot
create OLDA 256 cells allot
create NEWOFF 256 cells allot
create BLEN 256 cells allot
```

`ADD-CLO` appends to `CLO` with no capacity check:

```forth
: ADD-CLO {: r :}  r IN-CLO? IF exit THEN  r NCLO @ cells CLO + !  NCLO @ 1+ NCLO ! ;
```

The later `COPY-BLOBS` and `RELOCATE` phases index the parallel arrays by
`NCLO`, so once the closure exceeds 256 reachable records the linker writes past
the arrays and corrupts state.

Reproduction:

```sh
tmp=$(mktemp -d /tmp/habu-aot-review.XXXXXX)
prog=$tmp/p.f
out=$tmp/p
{
  printf ': W259 ( -- n ) 1 ;\n'
  for i in $(seq 258 -1 0); do
    j=$((i+1))
    printf ': W%s ( -- n ) W%s 1 + ;\n' "$i" "$j"
  done
  printf ': MAIN W0 . CR ;\n'
} > "$prog"
HB_TMP=$tmp ./tools/hb-build.sh "$prog" -o "$out"
echo "build_rc=$?"
```

Observed behavior:

The build emits the engine crash register dump and exits with code `134`.

Expected behavior:

The linker should either produce a valid binary or fail closed with a clear
capacity error before writing past any table.

Impact:

Larger real programs can crash the build, corrupt linker state, or potentially
produce invalid output. The failure mode is not diagnosable by the source author.

Recommendation:

- Add a named `MAX-CLO` constant and guard `ADD-CLO`.
- Ensure all parallel arrays derive from the same constant.
- Prefer sizing by `ndict@` or using a checked allocation in the data region.
- Add an AOT stress test just below and just above the supported closure count.

### High 3: Engine `/` and `MOD` silently accept divisor zero

Files:

- `src/habu/habu1.f:234`
- `src/habu/habu1.f:236`
- `bootstrap/cg/forth.fs:273`
- `bootstrap/cg/forth.fs:275`
- `bootstrap/cg/templ.fs:48`
- `bootstrap/cg/templ.fs:53`
- `bootstrap/cg/templ.fs:55`

Type: Runtime semantic parity / error handling

Description:

The engine primitive implementations use raw ARM64 `SDIV`:

```forth
: BDIV B G-POP A G-POP  A A B SDIV, A G-PUSH ;
: BMOD B G-POP A G-POP  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;
```

On ARM64, `SDIV` with a zero divisor returns zero. The hosted codegen templates
already document this and insert a trap:

```forth
\ Native SDIV by 0 silently yields 0; gforth THROWS.
: G-DIV0? ... T1 lok CBNZ,  BRK, ...
```

The engine primitives do not use an equivalent guard.

Reproduction:

```sh
printf '1 0 / .\n1 0 mod .\n' | bin/hbi
```

Observed output:

```text
0
1
```

Expected behavior:

Both operations should fail loudly, matching gforth's error semantics at the
level the project already chose for hosted native codegen.

Impact:

Divide-by-zero bugs silently become ordinary values in the native engine. That
breaks semantic parity and can mask real user/program errors.

Recommendation:

- Add the same zero-divisor guard used by `bootstrap/cg/templ.fs` to `BDIV`,
  `BMOD`, and any `/MOD` implementation in the engine path.
- Decide whether the engine should `throw`, `die`, or trap for this path and pin
  that behavior in `hb-suite`.
- Add tests for `/`, `MOD`, and `/MOD` divisor zero in both `bin/hbi` behavior
  and generated standalone binaries.

## Verification Performed

Read-only lint and oracle checks:

```sh
./tools/parity-lint.py
./tools/shadow-lint.py
./tools/clobber-lint.py
./tools/repl-lint.py
./tools/trust-lint.py
./tools/stale-status-lint.py
./tools/oracle.sh
```

Results:

- `parity-lint`: clean
- `shadow-lint`: clean
- `clobber-lint`: clean
- `repl-lint`: clean
- `trust-lint`: 17 TRUST sites, 17 manifest rows, 0 findings
- `stale-status-lint`: clean
- `oracle`: passed (`all.fs + selfhost-all.fs + t-shake + t-sh-jdiag + llm-bench`)

Targeted repros were also run for each finding. The default `test/run.sh` was not
run because it may update tracked binaries as part of the self-rebuild path.

## Test Coverage Gaps

The current gates have good coverage for bootstrap parity, self-host fixpoint,
and many native checker regressions. The findings above escaped because several
important negative cases are not pinned:

- native `bool` vs `i64` mismatches
- non-`bool` control-flow conditions
- malformed signatures that should reject
- AOT string/data/address literals
- AOT closure table overflow
- engine divisor-zero behavior

The default AOT gate currently exercises numeric recursion and checks stripped
size. It should also exercise address-bearing features, because those stress
relocation surfaces that numeric arithmetic does not touch.

## Recommended Fix Order

1. Fix native bool handling and branch condition types.
2. Make signature parsing fail closed on malformed delimiters.
3. Fix AOT relocation for string/data/address literals and add direct tests.
4. Add capacity guards to AOT closure tables.
5. Align engine division/modulo zero behavior with the hosted codegen guard.

The first two are checker soundness issues and should be treated as blockers for
claims that typed native definitions are verified against their declared effects.

