---
title: Preserve frozen engine include facts through symlinked roots
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T00:45:10.526415+03:00"
---

Owner Cedar indexed-dictionary lane. G2 check-cli-boundary reaches duplicate E-A-FIRST before missing-engine diagnostic: canonical cwd/lib/errors.f escapes cwd through a directory symlink, so boot alias lookup misses its frozen lib/errors.f fact. Root repro /tmp/cedar-check-missing-engine-9mmp27hy. Preserve invocation-root engine aliases and physical identity/owner precedence for ordinary application sources, including normalized and absolute paths and symlink/.. controls. Scope include.f resolver plus focused real-load tests. No alias scan on every require and no fixture-copy workaround.
