---
title: "Share the fault record's text layer"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.715176+01:00"
---

Problem: master's machine-code rejections write records through crash.f's arm64-only fault-record words; x86 has none and prints old one-line texts. Acceptance: a target-free FAULT-RECORD layer, arm64 kernel object byte-identical. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
