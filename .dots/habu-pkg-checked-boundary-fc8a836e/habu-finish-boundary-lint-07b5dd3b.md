---
title: Finish boundary lint runner
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T03:49:11.968609+02:00"
blocks:
  - habu-shorten-boundary-policy-cce143f2
---

Why: after the provider API, registry policy, and private slice renames land, the per-file reset and scan loop are the last obsolete UB-prefixed private runner names. Exact result: in tools/checked-boundary-lint-core.f rename UB-RESET-FILE-SCAN to RESET-SCAN and UB-SCAN to SCAN, update their private callers, and make no other behavior change. Preserve which state resets per file versus per command, source buffer ownership, starting line and column, token loop order, strict end-state checks, finding accumulation across files, FILE read behavior, and FINISH result exactly. Forbidden: folding the runner into public FILE or FINISH, aliases, globals, new state, semantic cleanup, or changed diagnostics. Pre-change proof: the two owned runner declarations retain UB prefixes. Acceptance: zero UB-prefixed declaration, reference, alias, or compatibility global remains in the core or its three caller files; package CHECKED-BOUNDARY-LINT exposes exactly six operations; the real CLI, CBLT suite, CHECK suite, trusted inventory, hostile structural fixtures, exact ownership/type checks, host lint, and file-map lint pass on one tree. Files: tools/checked-boundary-lint-core.f. Depends: habu-shorten-boundary-policy-cce143f2. Ownership: CHECKED-BOUNDARY-LINT final private runner names and zero-legacy proof. Claim: unassigned.
