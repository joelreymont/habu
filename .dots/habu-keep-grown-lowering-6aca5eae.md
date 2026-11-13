---
title: Keep grown lowering buffers out of images
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T11:54:44.680338+02:00"
---

Problem: src/core/lower-cert-base.f BUF-P and the src/core/layout-valid.f DATA-P, ENV-P, GUARD-*-P, TASK-*-P and FETCH-*-P cells are PERSISTED-PTR-VARIABLEs their ENSURE words grow off the boot buffers into process-local storage (ARENA-BYTES-GROW), and the reset that put them back before a capture was never called. A build whose definition grows one, then writes a program or image, may carry or refuse that pointer. Acceptance: a reproducer shows what such a build does and names the cause; after the fix, a build that grew each buffer writes a program that runs as a cold one does. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-cert
