---
title: Size signature lint buffer dynamically
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T18:30:51.109296+02:00"
---

Problem: `tools/signature-lint-core.f` reads source through the fixed 64 KiB
`SL-FILE-CAP`. A larger file fails with `file exceeds buffer` before any
signature is inspected, so mandatory enforcement does not cover
`maki/infer/gpt2-bind.f`. Splitting that module is separate structural work and
does not remove the lint's obligation to read every valid source file whole.

Required result: the `SIGNATURE-LINT` package owns a runtime-sized source
buffer, using the existing `LINT-SOURCE` abstraction from `tools/lint/text.f`
instead of a larger constant. The lint must inspect the complete file or refuse
before inspection with a named allocation or representability diagnostic;
truncation and partial scans are forbidden. Preserve every current signature
finding and command-line behavior. Correct the stale comments for refine-lint
rows 60 and 61: their erasures are used by every transaction exit, not only
`ABORT`; this is comment truth only.

Acceptance: a structural fixture larger than 64 KiB containing a valid
definition near the end is fully inspected and passes; the same fixture with a
late invalid signature fails for that signature, proving the tail was scanned;
ordinary small-file and hostile comment/string cases remain unchanged;
`maki/infer/gpt2-bind.f` lints successfully on the exact tree; the signature
lint suite, its exact production command, typed-local diff lint, and package
diff lint pass.

Files: `tools/signature-lint.f`, `tools/signature-lint-core.f`, its existing
test/fixture owner, `tools/refine-lint-core.f`, and `FILEMAP.md` only if a new
tracked fixture is required. Dependencies: none. Ownership: dynamic source
input and tests inside `SIGNATURE-LINT`; refine-lint comments only.

Claim: agent=codex-signature workspace=.jj-ws/habu-size-signature-lint-0e39e66d
