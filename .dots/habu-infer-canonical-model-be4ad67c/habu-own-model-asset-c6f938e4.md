---
title: Own model asset workspace
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:48.095932+02:00"
---

Why: config, tokenizer, index, and checkpoint intake require bounded path and byte storage while public model opens accept only root and length. Interface: package MODEL-ASSET owns one linear workspace with one FS-PATH-CAP join buffer and one byte buffer sized to the selected pin's exact largest config, tokenizer, or index asset. The checkpoint stage reuses that same buffer in completed chunks. OPEN takes the pin-derived capacity, every consumer takes and returns the workspace in every result arm, and RELEASE consumes it before model publication. Owner: transient model-open allocation, geometry, borrows, release, and namespace confinement only. Production red: parser and staging buffers are ambient or unowned. Acceptance: two workspaces interleave; exact and mmap-page-boundary one-short capacities distinguish every path/byte boundary; zero, total overflow, real allocation refusal, and release are observed through the OS; post-release access faults; public reopen, public publication, and direct private-WID publication all fail with exit 84; no package-global asset buffer exists. Read, parser, chunk-stage, and their failure-owner behavior are owned by their real downstream config, tokenizer, index, and checkpoint caller dots and must be green on the atomic combined tree before this foundation lands; no stand-in consumer belongs here. Forbidden: public raw buffer, verified root, manifest, cache, generic file framework, version, compatibility path, or test-only lifetime shim. Smallest owning check: bin/hb --load maki/infer/model-asset-test.f.

Claim: agent=codex workspace=.jj-ws/habu-own-model-asset-c6f938e4

Frozen interface: `OPEN ( CAD-NUM:byte-len -- result<MODEL-ASSET:ws,n> )`; `PATH-SPAN ( MODEL-ASSET:ws -- MODEL-ASSET:ws ptr u8 CAD-NUM:byte-len )`; `BYTE-SPAN` has the same stack shape; `RELEASE ( MODEL-ASSET:ws -- )`. `RELEASE` delegates to `MEM:RELEASE-BYTES`; a kernel unmap failure is the existing uncatchable exit-71 ownership invariant from `habu-make-owned-release-79de2b5c`, because the consumed workspace can no longer be returned honestly. Each span is an owner-threaded borrow valid only while that exact workspace remains live; callers must not retain it past a consuming call. The raw-buffer ban means no ambient or lifetime-free buffer, not no owner-threaded pointer. No callback, copy-out, limits type, special result type, or direct `munmap` exists.
