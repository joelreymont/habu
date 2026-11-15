---
title: "Pass a build's rejection status on"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:11.026001+01:00"
---

Problem: hb-build turns a rejection's 70 into 74 (test/wasm/hb-build.f pins 74). Acceptance: a build stopped by a rejection exits 70 with its one record. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-buildrc.
