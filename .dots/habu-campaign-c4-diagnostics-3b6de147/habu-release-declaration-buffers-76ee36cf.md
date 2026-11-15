---
title: Release declaration buffers when a source ends inside one
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.839662+01:00"
closed-at: "2026-11-16T04:07:34.362528+01:00"
---

Problem: a CALLBACK: or FUNCTION: declaration open when its source ends keeps its working buffers (ffi-callback.f, ffi-abi.f): nothing closes it. Acceptance: a source ending inside either declaration gets one record at the declaration and both buffers read released. Depends: habu-release-cb-buffers-da8041c7. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
