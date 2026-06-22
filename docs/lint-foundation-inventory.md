# Lint Foundation Inventory

Current state for retiring `tools/lint/lib.f` as a broad foundation.
The goal is to replace each concern with checked Habu libraries or narrow,
manifested boundaries, then remove the file-wide `0 set-check` dependency.

## Boundary Groups

### File and string helpers

Owned today by checked `tools/lint/text.f`.

Users:

- `tools/parallel-agent-lint.f:44` uses `READ-FILE` plus string helpers.
- `tools/filemap-lint.f:61` uses `READ-FILE` and path/string helpers.
- `tools/stale-status-lint.f:194` uses `READ-FILE` and split-line style scans.
- `tools/host-lint.f:154` uses `READ-FILE` plus case-insensitive search helpers.
- `tools/stdlib-manifest-test.f:336` and `tools/stdlib-manifest-test.f:460` use
  `READ-FILE` for docs and manifest inputs.
- `bench/llm/validate-results.f:1039` uses `READ-FILE` to load the task
  manifest while validating result rows.

Replacement direction: keep callers loading `tools/lint/text.f` explicitly while
shared `lib/fs.f`, `lib/string.f`, `tools/string.f`, and a checked line-stream
API mature enough to absorb the surface. Do not move text helpers back into
`tools/lint/lib.f`.

### Tokenizer arrays

Owned today by checked `tools/lint/token.f`. It stores source pointers through a
typed token table and exposes `TOKENIZE`, `TOK`, `TOK0?`, `TOK=`, and `TEOL?`.

Users:

- `tools/lint/shadow-lint.f:57`, `tools/lint/shadow-lint.f:67`, and
  `tools/lint/shadow-lint.f:68` scan source tokens for primitive shadows.
- `tools/lint/clobber-lint.f:302` and `tools/lint/clobber-lint.f:427` scan
  assembly-like source tokens.
- `tools/repl-lint.f:140` tokenizes `src/habu/stdin.f` for REPL exit checks.

Replacement direction: keep lint callers on `tools/lint/token.f` while broader
source tools migrate to `tools/lint/source-lex.f` where line/column metadata is
needed. Do not add token pointer reloads back to `tools/lint/lib.f`.

### Intern and string set

Owned today by `tools/lint/lib.f:116-157`.

Users:

- `tools/filemap-lint.f:52`, `tools/filemap-lint.f:60`, `tools/filemap-lint.f:72`,
  and `tools/filemap-lint.f:78` collect path sets from `FILEMAP.md`.
- `tools/repl-lint.f:96`, `tools/repl-lint.f:98`, `tools/repl-lint.f:118`, and
  `tools/repl-lint.f:154` store path identities for sorted REPL reports.
- `tools/public-signatures.f:310`, `tools/public-signatures.f:315`,
  `tools/public-signatures.f:347`, and `tools/public-signatures.f:372` store
  exported names and manifest names.
- `tools/lint/set-test.f` is the focused regression surface for this group.

Replacement direction: checked dynamic cell tables, maps, or growable vectors
should own key storage. Callers should not load pointer spans from untyped
globals except through audited accessors while the port is incomplete.

### Source lexer

Owned today by `tools/lint/source-lex.f:4-151`. Token metadata is stored in
checked `lib/vector.f` headers, and the scanner body is checked. The source
pointer cell uses `ptr-field`, so the lexer no longer needs a `TRUSTED:` reload
boundary for `LEX-A@`.

Users:

- `tools/signature-lint.f:189` runs `READ-FILE LEX-SOURCE`.
- `tools/aot-lint.f:128` runs `READ-FILE LEX-SOURCE`.
- `tools/diag-origin.f` and `tools/check-all-errors.f` load this foundation for
  source-origin diagnostics.
- `bench/llm/attempt-solutions-lib.f`, `bench/llm/forth-task-lines-lib.f`, and
  `bench/llm/large-buffer-bundle-test.f` compose it with benchmark source tools.

Replacement direction: keep new lexer users on `lib/vector.f` and `ptr-field`;
do not introduce raw table reloads or per-tool `TRUSTED:` pointer accessors.

### PAT and trust-site scanners

Owned today by `tools/lint/lib.f:246-422`. These scanners keep parse state and
capture spans in globals (`PSA`, `PSU`, `PX`, `P1A`, `P1U`, `P2A`, `P2U`,
`PTA`, `PTU`).

Users:

- `tools/trust-lint.f:264` calls `TRUST-SITE?` when checking source against
  `TRUSTED.md`.
- `tools/lint/text-foundation-test.f:151-159` tests `TRUST-SITE?`.
- `SRC-PATH-REF?` and `BACKTICK-PATH?` remain in `tools/lint/lib.f` and are part
  of the legacy scanner surface.

Replacement direction: trust and path-site recognition should be a checked
source scanner with token matching, comment/string skipping, and explicit span
outputs. It should not rely on shared PAT global captures.

## Follow-Up Dot Map

- `caf-5d0e5d9f745ae1a9` extracted checked text/file helpers into
  `tools/lint/text.f`.
- `caf-ca64bff01d98a49f` adds the checked line-stream/read foundation.
- `caf-7df3176d9a848aa6` adds typed growable vectors needed by token and set
  tables.
- `caf-2be3fefc7725d250` checks source lexer internals.
- Dedicated dots track the trust scanner and intern set work that remain after
  text and tokenizer extraction.

Validation for each port must include the focused tool fixture plus the default
native gate:

`bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f`
