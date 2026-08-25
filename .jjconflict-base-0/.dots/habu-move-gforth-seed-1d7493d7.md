---
title: Move gforth seed region into BL range
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T06:27:09.007275+02:00"
---

Deferred from the direct-BL landing (1e9a3926): the gforth seed (bootstrap/cg/forth.fs) still maps its code region at the far VA $300000000, so it correctly keeps absolute movz/movk chains and the goldens are form-aware. Prereq-B-scoped work: move the seed region into BL range of its emitted code like the native engine (80636de2), then convert seed emission to direct BL and collapse the form-aware goldens back to single-form. Recovery-path change; fixpoint + bootstrap parity proof.
