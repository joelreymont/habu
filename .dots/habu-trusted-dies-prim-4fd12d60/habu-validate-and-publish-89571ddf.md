---
title: Validate and publish the combined Habu changes
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.409212+03:00"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar; integration workspace cedar-source. Depends additionally on the existing type, quotation, cleanup, minimal-PRIM, shared-library, source/docs, target/debug and performance/tooling children. Integrate coherent completed changes only, preserve unrelated working copies and push checked-forth periodically. Latest completed full run on 4de24791: 127 pass, 61 fail; XML error allocation, XML recovery and loop/frame-order fixes landed afterward. Acceptance: rebuild and self-build the final combined tree, run bin/hb --load test/run.f, run standalone application and resource-lifecycle acceptance, resolve regressions and report actual untested target boundaries. No extra manifests, sign-off ledgers or duplicated acceptance wrappers.
