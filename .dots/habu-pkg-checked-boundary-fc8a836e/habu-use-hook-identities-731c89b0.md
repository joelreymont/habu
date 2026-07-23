---
title: Use hook identities in boundary lint
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T03:47:50.303233+02:00"
blocks:
  - habu-own-boundary-lint-0cff8730
  - habu-own-checked-hook-d1588988
---

Why: checked-boundary lint still authorizes checker hooks by token name alone, so moving an allowed token to the wrong file passes. Exact result: CHECKED-BOUNDARY-LINT requires HOOK-SITES and authorizes each parsed set-check install only with HOOK-SITES:CHECK-MATCH? using the exact scanned repository-relative path and installed token; set-top-check uses HOOK-SITES:TOP-MATCH?. Delete UB-HOOK-ALLOWED? and UB-TOP-HOOK-ALLOWED? and every private name list. Do not normalize, shorten, suffix-match, or infer paths: dot-relative, absolute, parent-escaped, symlink, and workspace aliases are noncanonical and reject. Preserve token-role parsing, preflight rearm behavior, strict mode, diagnostic bytes, finding order, and all non-hook policy. The immutable registry and trusted inventory remain the sole row and count authorities. Forbidden: copied registry data, TRUSTED.md parsing, caller allowlists, file-level authorization, tail-only comparison, mutable registration, or duplicated scanner logic. Pre-change proof: an allowed installed name in a wrong file passes the production provider. Acceptance: the real CLI and CBLT suite accept every current canonical path/name/kind row and reject wrong path, name, kind, comments, strings, duplicates, reordered tick forms, qualified spoofing, and every path alias; trusted inventory hostile stale, duplicate, and count-drift cases remain green; exact ownership/type, host, and file-map checks pass. Files: tools/checked-boundary-lint-core.f and tools/checked-boundary-lint-test-lib.f. Depends: habu-own-boundary-lint-0cff8730 and habu-own-checked-hook-d1588988. Ownership: CHECKED-BOUNDARY-LINT hook authorization plus private CBLT production fixtures. Claim: unassigned.
