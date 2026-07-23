---
title: Package AOT linker and maker
status: closed
priority: 1
issue-type: task
created-at: "2026-07-23T21:59:12.181744+02:00"
closed-at: "2026-07-23T23:56:32.938059+02:00"
close-reason: Landed unchanged from jointly accepted 1c0a3146 as 76f49f1d on verified master@origin a5d89220; positive/negative AOT and all required gates passed.
blocks:
  - habu-pkg-aot-negative-cec87f26
  - habu-pkg-aot-positive-91454b58
---

Why: src/habu/aot-lib.f and src/habu/aot.f form one linker/maker concern but expose linker tables, relocation state, source buffers, seed state, and generic helpers as ambient globals. The owner-WID package closure and exact-entry correction cannot change these files until the complete concern has a real owner. Dependencies: the complete positive and negative AOT test packages must land first. Owner and interface: reopen one package AOT-LINK across both production files. Export only LINK ( -- ), the sentinel target, and RUN ( -- ), the maker entry. Rename the old global AOT-LINK body to public LINK. Keep every parser, table, relocation, copy, seed, source, buffer, and driver helper private and call it bare inside the owner. Preserve the sentinel mechanism but emit AOT-LINK:LINK. Define public RUN, close the package, then invoke AOT-LINK:RUN so replacing INP and INE cannot place user definitions in AOT-LINK. Update white-box positive fixtures by reopening AOT-LINK inside their generated maker source; do not expose raw production helpers for tests. Preserve all records, relocations, persistent-data behavior, object/cache behavior, output bytes, diagnostics, phase ordering, and normal MAIN behavior. Preserve zero net maker data-space allotment before READ-PROG; record dictionary-name bytes, JIT, DATA, CODELEN, AOT output size, and link latency before and after. Files: src/habu/aot-lib.f, src/habu/aot.f, test/gate-aot-positive-lib.f, TRUSTED.md and FILEMAP.md only if exact inventory requires them. Checkpoint: clean real positive AOT baseline plus a representative package wrapper through the exact package diff gate and one baked-reader maker smoke; stop on any unplanned public caller or bootstrap mirror dependency. Acceptance: real tools/hb-build.f positive paths preserve persistent data, relocation, absolute-chain, preseed, object-store, object-hit, relink, output-byte, report, and normal MAIN results. Only AOT-LINK:LINK and AOT-LINK:RUN resolve publicly; bare old AOT-LINK, SENTSET, READ-PROG, MAP-IN-BLOB, COPY-COMPACT-BLOB, SEED+, and their qualified private forms reject while the package is closed. Package, typed-local, trust, file-map, positive/negative AOT, fixpoint, and touched native gates pass. Mutation proof: restore the bare sentinel and the real maker fails; leave the package open while RUN executes and ordinary global MAIN becomes unavailable; publish any raw helper and the exact public-surface check fails. Forbidden: partial ownership bands, compatibility globals, forwarding aliases, temporary packages, broad public test APIs, behavior changes, process-isolation work, publication hardening, cache redesign, or entry-resolution changes.

Claim: agent=aot-linker-pkg workspace=.jj-ws/habu-pkg-aot-linker-7b8acef6.
