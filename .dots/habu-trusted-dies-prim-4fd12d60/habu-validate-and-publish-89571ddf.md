---
title: Validate and publish the combined Habu changes
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.409212+03:00"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar. Frozen combined validation workspace cedar-polish-validation currently matches d8b044e7. Cold rebuild passes; focused native prefix/string/trap, PRODUCT+CORE-DATA and lifecycle/concurrent tasks all pass. Earlier combined app-image, memory, generic/nominal pointer, tail-placement and register tests pass on their recorded integrated revisions. Public hb-build --repl and a new-process MAIN+checked stdin compilation now pass.

The old full run at cdb5ed4b ended108 PASS/70 FAIL. It predates the fixes above and is not final acceptance. Remaining categories include obsolete hb-host fixture paths, emitter tests reading retired IR, scoped-local expectations, IEEE array declarations, PTX/tool declarations and proof/manifest timeouts under load. Diagnose actual current failures without weakening claims or blindly extending timeouts. Finish source-root/library/consumer integration, run bin/hb --load test/run.f on the final rebuilt tree, verify public app and warm/no-binary build paths, update docs, and publish coherent commits. Preserve original checkout's unrelated changes and immutable peer handoffs.
