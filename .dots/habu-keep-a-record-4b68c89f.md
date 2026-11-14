---
title: "Keep a record's text out of a saved image"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T20:23:53.388671+01:00"
---

Problem: src/core/rejection.f:20-21 holds TEXT, the arena mapping a rejection record is built in, in a PERSISTED-PTR-VARIABLE TEXT-P beside a plain TEXT-CAP. A mapping does not survive an image, and src/habu/link-copy.f:396-401 dies 'a DATA pointer outside the build's space' on a persisted pointer outside DATA. Whether a build ever maps TEXT before it links or saves is unknown. Acceptance: a reproducer shows what a build that printed a record before linking or saving does; if it fails or keeps a stale pointer, TEXT-P and TEXT-CAP become scratch cells (PTR-VARIABLE, SCRATCH-VARIABLE) and the reproducer passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-text.
