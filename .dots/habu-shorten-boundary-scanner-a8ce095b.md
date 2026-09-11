---
title: Shorten boundary scanner
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T03:48:32.740423+02:00"
blocks:
  - habu-shorten-boundary-state-b1d42586
---

Why: the private lexical scanner still exposes obsolete UB-prefixed names after package ownership. Exact result: in tools/checked-boundary-lint-core.f from UB-END? through UB-NEXT-TOK, drop only the UB- prefix from each private scanner declaration and all references. Preserve byte offsets, line and column updates, whitespace rules, line and parenthesis comments, ordinary and escaped strings, case-sensitive and case-insensitive comparisons, previous-token rotation, end-of-input behavior, and boolean results exactly. Keep the production token stream as the sole parser; do not add a second lexer or substring logic. Forbidden: grammar changes, new buffers, copied scanners, public words, aliases, or edits to policy and diagnostics beyond reference renames. Pre-change proof: the package-owned scanner declarations retain UB prefixes. Acceptance: hostile comments, strings, duplicate text, reordered tick forms, and qualified spoof fixtures retain their exact outcomes; the real command and CBLT suite are byte-identical; no UB-prefixed declaration in this scanner slice remains; exact ownership/type checks pass. Files: tools/checked-boundary-lint-core.f. Depends: habu-shorten-boundary-state-b1d42586. Ownership: CHECKED-BOUNDARY-LINT private lexical scanner names only. Claim: unassigned.
