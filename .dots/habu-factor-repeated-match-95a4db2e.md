---
title: Factor repeated MATCH compiler stencils
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T19:27:34.988396+02:00"
---

Mac codegen review 2026-07-19: native MATCH costs 1728 B of compiler binary (defensible total, poorly factored) - the em-adt-match-fam/var/of handlers repeat the checker-bridge call, W^X LPROT 5/3 window, failure-diagnostic, and match-frame state-management stencils around habu2.f:6185-6300. Factor the shared stencils into common emit words with NO behavior change: emitted user code must be byte-identical (prove via dis comparison of MATCH fixtures before/after) and the full gate + TFAM suites green; compiler CODELEN shrink recorded via size rows. Pure refactor - own change, nothing else rides along. SERIALIZE: after habu-slim-match-emitted (same habu2.f section).
