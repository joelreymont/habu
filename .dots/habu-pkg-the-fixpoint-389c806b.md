---
title: Package the fixpoint tool, or row it in the lint
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T23:25:02.656578+02:00"
---

package-diff-lint rejects ANY body edit to a global in tools/build-fixpoint*.f (neither file is packaged or allowlisted), so as configured the gate rejects every possible change to the self-rebuild tool - measured with a one-line probe (keyfix lane 2026-08-11; the ENGINE-BODY-EDIT? precedent at package-diff-lint-core.f:1201 records the same class for the engine files). The keyfix lane resolved it minimally (package STAMP-KEY around only the edited words, names kept - flagged as lint-driven debt). Either package both files properly (the ~300-line CLI tail cascade priced in the lane report) or add an interim exact-path ENGINE-BODY-EDIT? row with a retirement dot. Files: tools/build-fixpoint*.f or tools/package-diff-lint-core.f. Depends: none.
