---
title: Close verify-source primitive-row parity
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T04:15:00+02:00\""
---

Why: `VERIFY:SOURCE-BUF` does not recognize `PPRIM:` rows and ends `PRIM:`
rows at the first raw token that looks like `PRIM;`. Comments and strings can
therefore terminate the verifier's row model even though the interpreter has
not executed the closer. This also makes package ownership lint reject a
comment-only edit to `src/core/sumtype.f` by treating later registry tokens as
top-level declarations.

Owner and files: package `VERIFY` owns the scanner change in
`src/habu/verify-source.f`. Tests belong in `test/verify-prim-test.f` and the
existing standard-library child gate inventories; update `FILEMAP.md`. Do not
change primitive census policy, package-lint policy, or add another lexer.

Exact result: replace the PRIM-only raw loop with one shared registry-row
consumer that receives the live closer (`PRIM;` or `PPRIM;`), consumes the row
name, then advances through the canonical body-token path. It skips complete
line comments, multiline comments, ordinary strings, and escaped strings and
closes only when a live word token is case-insensitively equal to the supplied
closer. Case-insensitive matching is required because engine dictionary lookup
is case-insensitive; lowercase `prim;` and `pprim;` must close. A wrong closer
does not close and the row fails closed at end of input. `RECORD-DEFINER?`
routes both `PRIM:` and `PPRIM:` to this consumer.

Fix `SKIP-STRING-REST` at its owner: the opener token `s"` ends in a quote
character but is not a complete string. Delete the opener early-exit so the
scanner consumes through the real closing quote. Do not hide the defect in the
registry consumer; the same fix must repair `TRUSTED:` body scanning.

Acceptance: production `VERIFY:SOURCE-BUF` fixtures cover live PRIM and PPRIM
rows; each closer inside a line comment, multiline comment, ordinary string,
and escaped string; lowercase live closers; wrong and attached closers; a
following top-level definition that remains visible; and a `TRUSTED:` body
whose spaced semicolon is inside a string. A missing closer exits 74 through a
spawned `bin/hb` child. Mutations restoring raw-token closure, exact-case
matching, the string-opener early exit, or a consumer-local string workaround
must fail. Existing verify-source, trust, census, and fixpoint behavior remains
unchanged except for the proved prior leakage.

Smallest checks: the focused verify primitive test through native `bin/hb`,
the enrolled child-spawn gate slice, trust lint, package-diff lint on the exact
comment-only `src/core/sumtype.f` diff, candidate validation, and the native
fixpoint. Exact typed-local, package, host, and file-map gates must pass.

Claim: agent=verify_parity workspace=.jj-ws/habu-verify-parity
