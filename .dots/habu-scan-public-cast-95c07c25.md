---
title: Scan public CAST declarations
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T05:48:38.515462+02:00"
blocks:
  - habu-pkg-public-signatures-e25db8b1
---

Problem: `tools/public-signatures-core.f` recognizes colon definitions and generated type rows but ignores public `CAST:` declarations, so a checked public retype can compile while its canonical manifest row is invisible. This blocks `CAD-NUM:AS-BYTE-LEN`. Dependency: `habu-pkg-public-signatures-e25db8b1` must land first because it owns the same core and production CLI boundary. Owner and exact behavior: package `PUBSIG` extends its structural declarer dispatcher with the exact token `CAST:` and routes it through the existing name, standalone stack-effect comment, package visibility, export, JSON, and trust-output path used for ordinary checked definitions. It must emit the declared `( in -- out )` effect exactly once for a public CAST and emit nothing for a private CAST. It must not add aliases, public parser state, a second lexer, substring matching, host code, generated constructor semantics, or support for unrelated declarers. Production proof: the package-owned test builds and loads a real child fixture with public and private nominal roles plus public and private CAST declarations, invokes the actual `PUBSIG:MAIN` CLI through `RUN-ARGV-CAPTURE`, and asserts the qualified public word and exact effect appear once while the private word is absent. Hostile fixtures put `CAST:` in a line comment, paren comment, string, and longer token such as `FORECAST:`; none may emit a row. Deleting the `CAST:` arm, accepting a decoy, changing visibility, or emitting the body identity row instead of the declared retype must fail. Exact files: `tools/public-signatures-core.f` and `tools/public-signatures-test.f` only. Acceptance: the focused real-CLI suite, pinned good-file JSON and trust digests, standalone public-signature load, typed-local and package exact-diff lints, and the public-signature gate slice pass. Smallest owning-path check: the real scanner reports a compiled public CAST with its declared nominal retype, unblocking the separate `AS-BYTE-LEN` leaf.
