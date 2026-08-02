---
title: Delete VJP adjoint alias
status: active
priority: 2
issue-type: task
created-at: "2026-08-02T19:27:08.461018+02:00"
---

Why: lib/ptx/ad.f VJP-ADJOINT is an explicit historical alias whose only consumers are its focused tests; VJP-EXPAND is the canonical production surface. Exact result: move the straight-line guard plus VJP-ADJOINT$ lookup into VJP-EXPAND, migrate lib/ptx/ad-test.f to VJP-EXPAND, delete VJP-ADJOINT and stale alias prose. Dependencies: none. Owned result: one VJP lookup API and unchanged fail-closed semantics. Package owner: existing PTX AD module surface; add no package or export. Acceptance: zero VJP-ADJOINT references; all linear, nonlinear, missing-VJP, control-flow, and reverse-pass tests exercise VJP-EXPAND through the real implementation. Smallest owning check: bin/hb --load lib/ptx/ad-test.f; also ptx-stdlib slice and exact diff gates.

Claim: agent=vjp_alias_cut workspace=.jj-ws/habu-delete-vjp-alias
