---
title: Remove stale duplicate PTX emitter
status: closed
priority: 3
issue-type: task
created-at: "2026-07-19T20:36:24.969295+02:00"
closed-at: "2026-07-20T22:14:43.832717+02:00"
close-reason: "Landed 7391244b: stale tools/ptx/emit.f deleted (74 lines, zero consumers verified pre-deletion - all 13 emitter-surface words were double-defined), FILEMAP corrected to the canonical src/arch/ptx owner, and a NEW self-hosted reachability lint (tools/lint/ptx-emitter-lint.f, shadow-lint family) walks the tree and dies if any surface word is not defined exactly once - red-first proven firing on all 13 with the duplicate present, clean after. Wired into the gate lint slices; saxpy tests green"
---

Repository reachability audit: tools/ptx/emit.f is a stale 74-line copy of the SAXPY text emitter now owned by src/arch/ptx/emit.f:81-139. An exact repo search finds no require/include of tools/ptx/emit.f and no consumer outside its own definitions; tools/ptx/saxpy.f explicitly says it must be loaded after src/arch/ptx/emit.f. The stale copy lacks the canonical emitter's capture sink, configurable PTX target/version, and bounds-checked buffer, yet FILEMAP.md falsely calls it the encoder behind emit drivers. Keeping two global PTX-L/PTX-HEADER/PTX-EMIT-SAXPY families invites load-order shadowing and fixes landing in only one copy. Root fix: delete tools/ptx/emit.f, update FILEMAP.md and any documentation/commands to the canonical src/arch/ptx/emit.f owner, and add a source-discovery/reachability assertion that no second definition of the canonical emitter surface exists. Do not merge the old fixed-target code into the canonical module. Proof: rg has zero remaining path consumers or duplicate public definitions, saxpy emit/capture/target tests and ptx-stdlib pass, filemap/host/shadow/package lints pass.

Claim: agent=staleemit workspace=.jj-ws/fable-staleemit machine=spark (owns tools/ptx/emit.f deletion + FILEMAP + reachability assertion)
