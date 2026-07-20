---
title: Remove stale duplicate PTX emitter
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-19T20:36:24.969295+02:00\""
---

Repository reachability audit: tools/ptx/emit.f is a stale 74-line copy of the SAXPY text emitter now owned by src/arch/ptx/emit.f:81-139. An exact repo search finds no require/include of tools/ptx/emit.f and no consumer outside its own definitions; tools/ptx/saxpy.f explicitly says it must be loaded after src/arch/ptx/emit.f. The stale copy lacks the canonical emitter's capture sink, configurable PTX target/version, and bounds-checked buffer, yet FILEMAP.md falsely calls it the encoder behind emit drivers. Keeping two global PTX-L/PTX-HEADER/PTX-EMIT-SAXPY families invites load-order shadowing and fixes landing in only one copy. Root fix: delete tools/ptx/emit.f, update FILEMAP.md and any documentation/commands to the canonical src/arch/ptx/emit.f owner, and add a source-discovery/reachability assertion that no second definition of the canonical emitter surface exists. Do not merge the old fixed-target code into the canonical module. Proof: rg has zero remaining path consumers or duplicate public definitions, saxpy emit/capture/target tests and ptx-stdlib pass, filemap/host/shadow/package lints pass.

Claim: agent=staleemit workspace=.jj-ws/fable-staleemit machine=spark (owns tools/ptx/emit.f deletion + FILEMAP + reachability assertion)
