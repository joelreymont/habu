---
title: Scan public CAST declarations
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T05:48:38.515462+02:00"
---

Claim: alder, .jj-ws/alder-public-casts, base b4efad25. The current scanner
package is PS; route CAST: through its existing declaration path and exercise
the actual CLI on a compiled fixture. No package/API rename in this slice.

Implementation: the exact case-insensitive CAST: token dispatches to
PS-MAYBE-DEF, sharing name, declared effect, visibility, JSON and trust output.
The real CLI child compiles public/private nominal roles and casts first;
the manifest must contain exactly the public nominal retype once. Comment,
string and FORECAST: decoys cannot add a row; trust output is byte-exact.

Proof: the unchanged scanner fails four new assertions (missing word/effect,
empty definition array, missing trust row); the patch passes. The complete
tool-boundary-doc-public and stdlib-process-fixtures rows pass, including
the standalone core load. good.f JSON/trust output is byte-identical to base:
SHA256 cbbae87e9df05319d457be3fbda5f3d30f25ec7e7ff8411c0a9de2a2ec6e0424
and 739b2592ad080852cb87ca91cec7f30a2648102effb24c9b61ede0b9aafdafc7.
Astra review is clear. Private tree/host b4efad25; Hazel supplies final gate
and integration. The old typed-local/package diff tools named above no
longer exist in this tree.

Problem: `tools/public-signatures-core.f` recognizes colon definitions and generated type rows but ignores public `CAST:` declarations, so a checked public retype can compile while its canonical manifest row is invisible. This blocks `CAD-NUM:AS-BYTE-LEN`. Dependency: `habu-pkg-public-signatures-e25db8b1` must land first because it owns the same core and production CLI boundary. Owner and exact behavior: package `PUBSIG` extends its structural declarer dispatcher with the exact token `CAST:` and routes it through the existing name, standalone stack-effect comment, package visibility, export, JSON, and trust-output path used for ordinary checked definitions. It must emit the declared `( in -- out )` effect exactly once for a public CAST and emit nothing for a private CAST. It must not add aliases, public parser state, a second lexer, substring matching, host code, generated constructor semantics, or support for unrelated declarers. Production proof: the package-owned test builds and loads a real child fixture with public and private nominal roles plus public and private CAST declarations, invokes the actual `PUBSIG:MAIN` CLI through `RUN-ARGV-CAPTURE`, and asserts the qualified public word and exact effect appear once while the private word is absent. Hostile fixtures put `CAST:` in a line comment, paren comment, string, and longer token such as `FORECAST:`; none may emit a row. Deleting the `CAST:` arm, accepting a decoy, changing visibility, or emitting the body identity row instead of the declared retype must fail. Exact files: `tools/public-signatures-core.f` and `tools/public-signatures-test.f` only. Acceptance: the focused real-CLI suite, pinned good-file JSON and trust digests, standalone public-signature load, typed-local and package exact-diff lints, and the public-signature gate slice pass. Smallest owning-path check: the real scanner reports a compiled public CAST with its declared nominal retype, unblocking the separate `AS-BYTE-LEN` leaf.
