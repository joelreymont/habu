---
title: Share one front end between T0 and tier 1
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:31:53.174876+02:00"
closed-at: "2026-11-10T21:56:45.454707+02:00"
close-reason: merged into habu-copy-every-tier-86155a25, which carries its whole text; its open child habu-tier-0-and-4233b7bd moved there (dots audit 2026-11-10)
---

Census 2026-10-31 (/tmp/jerry-big/t0cmp-A.md, t0cmp-B.md): T0's front end (src/habu/t0.f) calls tier 1's front-end code (elaborate.f NELAB, hir-word.f HIR-WORD) for 3 of 20 name/literal/is/MATCH constructs and 0 of 27 control/local/quotation/does> constructs; it re-implements 40 and lost checks (fixed for the release in the census lanes). Tier 1 rebuilds the checker's tape in feed.f; T0 reads it directly. Joel 2026-10-31 expected the front end copied from tier 1. One shared token model and refusal set for both native tiers, separate code generation. After the T0 release.
