---
title: ZERO-SPAN wants a failing probe
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T21:47:26.051016+02:00"
---

src/habu/habu2.f AOT-WINDOW:ZERO-SPAN (master 669eb949) zeroes the window span before APPLY-RUNS. Every current path hands the seed a fresh anon mmap, so removing the zero is unobservable today - a guard with no red-first case. A fixture needs a hook to dirty the region between map and seed, which does not exist. Either build that hook (a dev-only smear word in the seed path, gated like DEV-SNAPSHOT-RESET) and keep a case that reds when ZERO-SPAN is deleted, or prove a current path (merge compose, snapshot restore) can already produce a lived-in region and write the case against it. The comment at habu2.f ~4719 states the threat model.
