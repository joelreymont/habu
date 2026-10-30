---
title: Share one front end between T0 and tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:31:53.174876+02:00"
---

Census 2026-10-31 (/tmp/jerry-big/t0cmp-A.md, t0cmp-B.md): T0's front end (src/habu/t0.f) calls tier 1's front-end code (elaborate.f NELAB, hir-word.f HIR-WORD) for 3 of 20 name/literal/is/MATCH constructs and 0 of 27 control/local/quotation/does> constructs; it re-implements 40 and lost checks (fixed for the release in the census lanes). Tier 1 rebuilds the checker's tape in feed.f; T0 reads it directly. Joel 2026-10-31 expected the front end copied from tier 1. One shared token model and refusal set for both native tiers, separate code generation. After the T0 release.
