---
title: Rebuild an image when an embedded text changes
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:54:42.634311+02:00"
---

Problem: src/lib/build-input.f FILE calls COLLECT, which nothing sets with USE, so it does nothing; ~/Work/maki/src/vocabulary.f:15,62 and src/lib/browser/host.f:29, app.f:37 call it so the image rebuilds when a text changes. A warm Maki build after a text edit may keep the old text and differ from a cold one. Acceptance: a warm build after editing an embedded text equals a cold build of the edited tree, shown on a /tmp copy of Maki; BUILD-INPUT holds only what that needs. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-input
