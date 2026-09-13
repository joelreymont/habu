---
title: Preserve language protection and owner checks across native selfbuild
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T16:24:10.390520+03:00"
---

Problem: product-hosted source ccc0661a B1 changes checked behavior even before the local-case patch. Original host28e11361 passes internal-word-gate and type-field-owner-suite; unchanged-source B1 SHA45b5b4eb fails both, as does fresh combined0c602d1c. internal-word-gate assertions579-581: checked definition writing NULL-PTR-CELL exits0 instead of rejecting70/E-UNDEFINED. type-field-owner-suite assertion188 unexpectedly false. Preserve these rejection/owner semantics through native-build, reduce both failures, identify whether metadata or declaration publication is lost and fix its responsible layer. Acceptance: original engine and first product agree on checked internal-word rejection and all type-field-owner cases, repeated product-hosted build preserves results, required native suite passes. Files: tools/native-build.f; src/core/pointer-storage.f/checker.f and source owner/payload handoff; src/habu/native-runtime.f/xref.f as proven by reduction; existing two suites. No bypass, weakened rejection or obsolete-state assertion substituted for behavior. Evidence: private baseline B0/B1/B2 under /tmp/cedar-native-baseline.hJ8uHu; full315-suite log in .jj-ws/cedar-correctness-verify/build/full-gate.log. Depends: diagnose independently; coordinate source owner edits with providerf2/tier1dc23a17/payload eec26aea. Ownership: Cedar diagnosis/integration until assigned. Claim: unassigned.
