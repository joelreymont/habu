---
title: Validate and publish the combined Habu changes
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.409212+03:00"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar; integration workspace cedar-source. Depends additionally on the existing type, quotation, cleanup, minimal-PRIM, shared-library, source/docs, target/debug and performance/tooling children. Integrate coherent completed changes only, preserve unrelated working copies and push checked-forth periodically. Final combined validation remains pending: the earlier suite attempt stopped at its first failed pool, and subsequent focused fixes do not constitute a full pass. The existing cedar-polish-full-validation workspace runs a frozen tree while implementation continues. Acceptance: rebuild and self-build the final combined tree, run bin/hb --load test/run.f, run standalone application and resource-lifecycle acceptance, resolve regressions and report actual untested target boundaries. No extra manifests, sign-off ledgers or duplicated acceptance wrappers.
