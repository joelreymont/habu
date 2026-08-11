---
title: CONTENT-KEY folds share one accumulator
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T23:27:23.707035+02:00\""
---

CONTENT-KEY:FILE+ and friends fold into a single shared accumulator (src/core/content-key.f); two interleaved folds silently mix their bytes into one key. The keyfix lane worked around it by completing its whole fold before any other key work, but nothing enforces that - a structural fix is a fold handle (accumulator value on the stack or a keyed instance) so interleaving is impossible by construction. First consumer: tools/build-fixpoint.f STAMP-KEY plus any future second CONTENT-KEY user. Files: src/core/content-key.f, tools/build-fixpoint.f. Depends: none.

Claim: agent=fixpkg workspace=.jj-ws/habu-fixpoint-pkg
