---
title: Package gate dictionary driver
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:11.840716+02:00\""
blocks:
  - habu-pkg-gate-runner-74b02485
---

Files: test/gate-dictionary-lib.f, test/gate-dictionary.f, test/run-worker-dict.f, and the dictionary dispatch call in test/gate-runner-lib.f. Put the library in package GATE-DICTIONARY, make all state and helpers private with short tails, expose only RUN ( -- ), and update the three callers to GATE-DICTIONARY:RUN. Continue calling the still-global checker API. Acceptance: no GD-* implementation name remains global; the complete dictionary gate and worker slice still execute the same cases; no alias. Verify: bin/hb --load test/gate-dictionary.f, dictionary worker slice, gate runner dictionary slice, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=gate_dictionary_pkg workspace=.jj-ws/habu-pkg-gate-dictionary-e0bd3a9c.
