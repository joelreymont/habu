---
title: Validate and publish the combined Habu changes
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.409212+03:00"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar. Additional prerequisites: completed type/quotation/cleanup/PRIM migration, shared library handoff, source/docs usability and required target/debug changes. Preserve all unrelated work and land coherent jj changes with periodic pushes; user explicitly forbids accumulating unrelated unfinished features in one commit. Independently reviewed root stored-quotation/lifecycle/capture revisione9fd7e6d passed native-stored-quot, native-catch, native-quot and image-lifecycle on build5; nominal-pointer1eb56c73 also reviewed and focused-tested. Do not call the full suite green: old uniform cedar-validation run ended with86 failures (/tmp/cedar-native-suite-5.out); pointer baseline suite session70735/PID343563 logs /tmp/cedar-pointer-suite.log and is still running. Triage actual failures on one fixed source/binary; retain harness corrections and replace obsolete negative expectations only with executed proof. Rebuild/self-build and run bin/hb --load test/run.f on the final combined tree; report actual failed/untested boundaries and resolve regressions before completed handoff. Remove obsolete bootstrap machinery only after the replacement path works; keep required historical seed bootstrapping distinct from runtime fallback.
