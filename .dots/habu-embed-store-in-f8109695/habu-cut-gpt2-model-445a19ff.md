---
title: Cut gpt2-model over to embedded store
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:59:20.791150+02:00"
blocks:
  - habu-retype-wstore-disposal-944e0f89
  - habu-store-structure-destructure-8c20c92a
---

The model cutover, one compiling commit: gpt2-model's field changes from WSTORE:resident to WSTORE:store (measured on master: the embedded store declares - the capability removed the resident's premise); construction (both commit arms), MODEL-DISPOSE, and the exact tests change together; disposal consumes WSTORE:DISPOSE directly. Weight reads consume the EXISTING WSTORE:WITH-SLOT surface - the WITH-SLOT redesign (linear-scope-dependent) migrates its consumers itself when it lands, and this leaf does not wait for it. Acceptance: model round-trip suites green on the embedded shape with the same leak-counter stations; both diff lints; destruction review before merge.
