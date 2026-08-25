---
title: Factor field schema validator
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T12:12:06.142339+02:00"
---

Style review findings: PF-NODE-KIND? at src/core/type-family.f:945 is a roughly 60-line recursive dispatcher mixing PARAM/CON/PTR/QUOT/APP classification and validation, obscuring fail-closed branches; the shared PF slice also spells core built-ins such as IF, THEN, BEGIN, WHILE, EXIT, and RECURSE in uppercase, contrary to docs/forth.md. Fix: split each schema kind into a named uppercase checked helper with a real typed stack effect, isolate root-range and recursive-child validation, retain one shallow dispatcher, lowercase every core/system word touched by the PF implementation while keeping project-defined words uppercase, preserve exact results and error ownership, and add branch-focused negative fixtures. Acceptance: no giant or multi-concern word remains, no uppercase core/system word remains in the changed PF slice, typed-local-diff-lint and source-style gates are green, and type-family, declaration, fixpoint, and full gates remain green. Files: src/core/type-family.f shared PF implementation, test/type-family-suite.f.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `factor_field` and workspace `.jj-ws/habu-factor-field-schema-f4d27285` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `src/core/type-family.f:1391` still holds `PF-NODE-KIND?` as one long dispatcher with all five kinds inline. The dot stays active and is free to claim.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
