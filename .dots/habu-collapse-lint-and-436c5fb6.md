---
title: Collapse lint and size test scaffolding
status: active
priority: 2
issue-type: task
created-at: "2026-08-02T18:58:46.939529+02:00"
---

Problem: campaign commits copied the 16-token global result declaration grammar into tools/package-diff-lint-core.f and duplicated it across adversarial fixtures, while the real ENUM parser and result consumers own semantics; commit 4139877a created a circular public test API between gate-engine-lib.f and gate-build-size.f plus synthetic self-check ceremony. Result: make the result global exception the same exact path, definer, and name ownership admission used for option; delete RESULT token/shape state, the second scan/count, and schema-impostor fixtures while retaining wrong path/name/owner/neighbor cases. Make BUILD-SIZE the sole one-way owner of candidate-size classification, baseline, rendering, and enforcement; delete ENGINE-GATE SIZE public back-edge, SIZE-CLASS-EXPECT, ACTION-EXPECT, SELF-CHECK, and circular require, while retaining real candidate file-size, CODELEN, and per-region enforcement. No new lint, parser, suite, package, manifest, compatibility layer, or generic framework. Ownership: tools/package-diff-lint-core.f and its existing fixture; test/gate-engine-lib.f and test/gate-build-size.f. Acceptance: package-diff focused hostile ownership cases pass; result product declaration and live consumers pass; engine build uses the real candidate and size gates pass; lint-tools, test/run.f, exact diff gates pass. Checkpoint: baseline focused gates green and deleting the copied validator without the exact generic exception reds lib/adt/result.f ownership. Claim: agent=scaffold_fold workspace=.jj-ws/habu-collapse-scaffold
