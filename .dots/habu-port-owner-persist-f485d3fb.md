---
title: Port owner-persist adversarial tests to master
status: active
priority: 2
issue-type: task
created-at: "2026-07-19T00:42:33.635732+02:00"
---

Forensic adjudication 2026-07-19: workspace owner-persist (retired tip 6382c5a0, commit kept un-abandoned as the recovery pointer) held three adversarial scenarios master lacks. 1) test/owner-wid-role-swap.f - nominal locator-index confusion negative: a word taking owner-row-idx fed a prot-row-idx must reject at check time; port directly against the landed nominal types. 2) test/owner-wid-snapshot-poison.f - return-stack canaries proving the snapshot path does not read stale frames; re-derive against the landed TRUSTED: SNAP-RETIRE-GO boundary in src/habu/snap.f. 3) test/owner-wid-snapshot-close-fail.f - close-failure injection; the tip used a SNAP-CLOSE-SEAM:INSTALL-TEST seam master lacks, so add an equivalent named tested fault-injection boundary or derive the scenario without one. Also verify test/owner-wid-build-forge.f subsumes the tip's owner-wid-aot-mutate.f forged-WID mutation scenario before calling coverage complete. All other owner-persist content is proven landed (src/habu/snap.f:31 TRUSTED: SNAP-RETIRE-GO is the tip's snap-drive.f verbatim; seal at owner-wid-emit-seal.f; certs at lower-cert-base.f).

Claim: agent=ownerport-opus workspace=.jj-ws/habu-port-owner-persist-f485d3fb
