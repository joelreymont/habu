---
title: Shorten boundary diagnostics
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T03:48:43.933061+02:00"
blocks:
  - habu-shorten-boundary-scanner-a8ce095b
---

Why: the package-owned JSON and prose diagnostic pipeline still carries obsolete UB-prefixed names. Exact result: in tools/checked-boundary-lint-core.f from UB-JSON-BASE through UB-REPORT-ROGUE-TOP-HOOK, drop only the UB- prefix from every private diagnostic declaration and all references. Preserve diagnostic codes, keys, values, file/name/origin roles, JSON escaping, commas and newlines, prose text, output descriptor routing, finding count increments, and report order byte for byte. Keep one shared report path per finding; do not duplicate formatting or add fallback output. Forbidden: message changes, schema changes, reordered findings, public words, aliases, or policy changes. Pre-change proof: the owned diagnostic declarations retain UB prefixes. Acceptance: byte snapshots for unchecked definition, checker mutation, missing preflight, rogue check hook, and rogue top hook are unchanged in JSON and prose; the real command and CBLT suite pass; no UB-prefixed declaration in this diagnostic slice remains; exact ownership/type checks pass. Files: tools/checked-boundary-lint-core.f. Depends: habu-shorten-boundary-scanner-a8ce095b. Ownership: CHECKED-BOUNDARY-LINT private diagnostic names only. Claim: unassigned.
