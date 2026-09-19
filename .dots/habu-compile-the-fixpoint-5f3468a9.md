---
title: "Compile the fixpoint's stage 2 with the chosen engine"
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T14:18:09.018198+03:00"
---

Claim: alder, .jj-ws/alder-fixpoint-engine-choice from de37c3e6. Keep this
separate from eeaf6c00's portable capture; use only private source trees and
engines, no install --force. The existing build fixture must seed its selected
engine path before its first build now that that path also supplies stage 2.

Repair: the first stage uses BF-ENGINE$; its three existing accessor definitions
move earlier without a semantic change. Later generations still run hb-stage.
The DDC caller now copies its Gforth seed to its selected private target before
refresh, so that target is both its build engine and its install destination.

Proof in /tmp/alder-fixpoint-engine-choice: with the tree's bin/hb deliberately
set to an unusable executable and HABU_FIXPOINT_ENGINE naming the private host,
the original --stage exits 74/E-BUILD-STATUS and the repair converges (0). The
new fixture selects /usr/bin/false and requires E-BUILD-STATUS: it fails against
the original code and passes against the repair. Its definitions and that
single test run through focused.f; no install command was run. Independent
Astra review is clear after correcting the DDC seed caller. Full DDC audit and
build-fixpoint-fixtures remain for the owning integration run because they
invoke install --force.

Owning/reader registry rows passed with private tree/host/HOME/HB_TMP/XDG:
tail-pure-fixtures (all eleven files together), hb-build-fixtures (all four),
hb-open-failure, certify-generated, pre-trust-defer, cold-runtime,
aot-chain-capture and ddc-verify. No full gate run; Hazel owns the serial slot.

Problem: tools/build-fixpoint.f BF-BOOTSTRAP-STAGE compiles stage 2 with the literal bin/hb while every other path goes through BF-ENGINE$; HABU_FIXPOINT_ENGINE only re-targets the install, so a fixpoint run against a promoted private engine fails until that binary is copied over bin/hb (chain-callees lane, 2026-09-17), and a fully sandboxed fixpoint is impossible. Acceptance: stage 2 compiles with BF-ENGINE$; a fixpoint run with HABU_FIXPOINT_ENGINE set and a stale bin/hb succeeds and installs the new engine; docs/bootstrap.md states which engine each stage uses. Files: tools/build-fixpoint.f, docs/bootstrap.md. Verify: a fixpoint run with HABU_FIXPOINT_ENGINE pointing at a private engine and a deliberately stale bin/hb. Depends: none. Ownership: build tools. Claim: unassigned.
