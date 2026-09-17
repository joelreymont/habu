---
title: Accept the replacement compiler in Tender, Maki and Kestrel
status: closed
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.378322+03:00"
closed-at: "2026-09-16T14:34:48.742980+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Downstream acceptance of the replacement compiler in Tender, Maki and Kestrel is the campaign's release gate, not a dispatchable leaf"
blocks:
  - habu-preserve-debugger-watchpoint-74e8b1d9
  - habu-preserve-verified-input-d1bd23c6
  - habu-compare-complete-inherited-440084ec
  - habu-preserve-language-protection-a772a9a2
  - habu-bind-locals-by-a16875d6
  - habu-build-the-compiler-c348eab0
  - habu-size-the-snapshot-1ca5db10
  - habu-wire-the-checker-eec26aea
  - habu-declare-quotation-typed-e92b0571
  - habu-forge-the-artifact-25770093
  - habu-gate-build-byte-8d249e4d
  - habu-remove-test-requirements-fec97925
  - habu-exercise-publication-of-ddd5c76f
  - habu-bring-the-no-29c5dc0b
  - habu-compose-every-fd-ffa78ad1
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Integration acceptance only: source fixes remain with leaves; own candidate pin/handoff and concise native docs. Rebuild exact source/run test/run.f, resolve every red by name. Original seven: addrmap-inline,cast,p2-map-rewind,aot-wide-format,aot-wid-restore,build-fixpoint-fixtures,pre-trust-defer. Fresh native-product run also found zip/aot-chain-capture caller collisions (fixed under local-casea16875d6), process-env fixture440084ec and internal-word-gate/type-field-owner product regressionsa772a9a2. Keep real/call,begin-until and quotation-spill KEEP cases. Tender local required scanner+runner closure+real documents+REPL; Maki GEOM:SHAPED-PAIR/full native/warm capture/restore/REPL; Kestrel compiler+embedded handoff and integration tests, hardware separately. Verify repeated source-free saves/startup/argv/rejections. Preserve accepted pins until owners accept identified replacement; hello-world/help insufficient. Final speed is8385810f acceptance.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Current gate evidence: see PLAN.md and campaign8385810f for the complete B3 332-suite/31-failure set. Watchpoint-resume SIGSEGV74e8b1d9 is a required release blocker, distinct from historical PTY timing reports.
