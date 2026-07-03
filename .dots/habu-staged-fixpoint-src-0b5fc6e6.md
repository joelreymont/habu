---
title: Staged fixpoint source checking
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.821289+02:00"
---

Implement the trust induction: the RUNNING bin/hb (stage N) must CHECK the full stage N+1 source list - checker.f, render.f, habu1.f, habu2.f, jit.f, util/structures, everything the fixpoint compiles - BEFORE building it, refusing to build on any reject. Wire as a mandatory pre-pass in tools/build-fixpoint.f (same authoritative source list the build uses; do not fork a parallel list). This dissolves the EMIT-HOST-LOAD-PREFIX unchecked-prefix hole (src/habu/habu2.f:412-415): source is only ever executed by a binary whose own source was checked by its predecessor; byte-for-byte fixpoint ties the knot. Subsumes habu-self-check-checker-e10ce327 as its first rung. Depends on: checker soundness (habu-fix-sig-clobber) landed; checker perf (habu-hash-idx-checker, habu-high-water-checker) makes ~14k-line pre-pass affordable; builder TRUST conversion (habu-builder-trust-checked) reduces what the pre-pass must accept as boundaries. Gate: refresh fails closed on any unchecked-source reject.

## Blocked — evidence (2026-07-02, opus-tools)

Root reason this cannot land as specified yet: the "authoritative source list
the build uses" is NOT a plain list of checkable .f files. `BF-EMIT-SOURCE` /
`BF-APPEND-COMMON` (tools/build-fixpoint.f) ASSEMBLE the stage source and inject,
at assembly time, lines that exist in NO .f file:
- `BFR-CHECK-OFF` (src/habu/hide.f = `0 set-check`) before the checker-boot
  region, and `0 set-check` / `' HOOK set-check` around the target-image
  emitters (elf.f/macho.f/sign*.f/image-bytes.f) — build-fixpoint.f:561,564,643.
- five synthetic TRUST lines from `BF-APPEND-IMAGE-TRUSTS`
  (build-fixpoint.f:579-584): ASM-CODE `-- asm`, BUILD-IMAGE `asm -- img`,
  BUILD-SNAP-HDR `n -- snap n`, SET-SIGID `ptr u8 n --`, CODESIG2 `img -- img`.
  These type the raw image emitters; without them the emitter files reject.

So a pre-pass that runs the checker (hook ON) over the real source list would
FAIL CLOSED on the builder's own legitimate emitter boundaries and break every
refresh. The dep `habu-builder-trust-checked` is precisely the work that would
convert those injected set-check spans + synthetic TRUSTs into checked source (or
into TRUSTED.md-audited definitions the pre-pass can accept). No landed
commit/dot for habu-builder-trust-checked, habu-fix-sig-clobber,
habu-hash-idx-checker, or habu-high-water-checker was found in this tree
(ancestors(fable,400), .dots/, .dots/archive/). Per CLAUDE.md "do not normalize
the gap with local runtime guards": this stays blocked on those deps rather than
shipping a pre-pass that either rejects legit boundaries or has to fork a parallel
"checkable subset" list (which the dot explicitly forbids). Next rung once the
deps land: build the checkable subset from the SAME BF-APPEND-* order, minus the
injected-boundary files, and grow it as builder-trust conversion shrinks the
boundary set.
