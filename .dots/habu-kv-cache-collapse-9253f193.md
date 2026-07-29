---
title: "KV cache: collapse test sweeps, pin HIGH-WATER and COW"
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T23:05:51.103556+02:00"
---

Why: 2026-07-29 design interview ruling by Joel — keep maki/infer/kv-cache.f unchanged and improve its suite. This dot blocks nothing: it is deliberately decoupled from all KV product leaves. Scope: maki/infer/kv-cache-test.f only; kv-cache.f stays byte-identical. Owned result: (1) collapse the page-size sweep (KVT-PAGE-TOKENS) and boundary sweep (KVT-EVERY-BOUNDARY) to distinct failure modes: one full page-size case plus boundary token indices (0, 1, page-1, page, page+1, max) per remaining size; keep the 300-round fixed-seed churn oracle. (2) Delete the 7 search-wl privacy assertions (proven once in test/seal-package.f) and the META-CELLS arithmetic tautology. (3) Add two missing pins from the 2026-07-29 audit: a HIGH-WATER test that fails if the kv-cache.f:275 watermark update is deleted or its max becomes min (both mutations pass today — the measured pre-change defect through the real entry point), and a COW reservation check computed from independent totals so it no longer restates KV-COW-DESIRED's own assignment. Acceptance: bin/hb --load maki/infer/kv-cache-test.f green; bin/hb --load maki/test.f green; both named mutations turn the suite red; all 45 distinct behavioral pins retained. Consumer: maki/test.f:165 already runs the suite. Claim: agent=claude-kv-suite workspace=.jj-ws/kv-suite
