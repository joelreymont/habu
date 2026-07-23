---
title: Package boundary lint command
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T03:46:58.199562+02:00"
---

Why: tools/checked-boundary-lint.f still owns command state and RUN globally, so any command edit fails package ownership. Exact result: package BOUNDARY-LINT-CLI owns only tools/checked-boundary-lint.f. ARGV-FILE and RUN are private short tails; the file invokes RUN before closing the package and publishes no word. Preserve every require, argument spelling, default path, JSON and strict option, directory walk order, output byte, exit code, and provider call. Continue using the current global checked-boundary provider API; a later API cutover updates this caller once. Forbidden: public MAIN or RUN, aliases, wrappers, new state, copied provider logic, or behavior changes. Pre-change proof: changing RUN outside a package produces E-PACKAGE-OWNERSHIP. Acceptance: the real command passes on the repository, reports the same hostile unchecked fixture and exit code, rejects unknown options identically, exposes no BOUNDARY-LINT-CLI word, and passes exact typed-local and package diff checks plus host and file-map lints. Files: tools/checked-boundary-lint.f. Depends: habu-pkg-checked-boundary-eb121cc5. Ownership: package BOUNDARY-LINT-CLI and that file only. Claim: unassigned.
