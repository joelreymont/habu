---
title: Delete the export hook nothing installs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T18:39:10.456872+01:00"
closed-at: "2026-11-13T19:01:19.145113+01:00"
---

Problem: CHECKER-OWNER-EXT:EXPORT-ACCEPT-OFF (src/core/checker-owner-extension.f) is never stored, so PKG-NOTIFY-EXPORT (src/habu/interpreter.f) always exits. Acceptance: the field, its alias and the reader are gone, the owner record has no hole; the build converges and export-keyword-package passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu11
