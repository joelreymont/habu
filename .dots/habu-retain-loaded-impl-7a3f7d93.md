---
title: Retain loaded implementation producers
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T19:03:31.280535+03:00"
---

Engine and application FILE dependency tracking currently marks no-record intrinsics and prefix executions ordered because actual loaded kernel/checker/compiler implementations have no persistent producer facts. This makes most ordinary checked units miss. Retain three role digests from actual consumed implementation source buffers and target options, separate from FILE keys and declaration identity. Captured checker/compiler owners retain their own completed content digests; the writer finalizes immutable kernel producer metadata after loading its actual emitter inputs. Never use current disk content to describe running code, whole ENGINE-ID in FILE keys, or recursively hash prior generation role identities. Missing facts cause a local cache miss. Prefix execution may use a deliberately coarse actual loaded-prefix digest attached only to fresh target prefix records. Read-only Astra design defines SOURCE-PRODUCER:ROLE$ and kernel-producer$ seams; bounded implementation coordinated with shared driver, dependency and reference owners. Acceptance: real warm FILE reuse in application and engine builds, changed active implementation invalidates consumers, cached/cold images agree, and required independent review/native/convergence on integrated feature.
