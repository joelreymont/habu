---
title: Allocate checker boot stores on first use
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-25T08:58:55.103706+02:00\""
closed-at: "2026-09-25T11:39:29.924287+02:00"
close-reason: Reviewed lazy SYMS/NORETS and owned SPA accepted with identical native generations, all 490 native suites, and Maki routing/negotiation with identical exports. Boot alone removes 851936 DATA bytes; combined Maki is 21702368 bytes, 984960 smaller, while native grows 16512 to 2906359 bytes. Evidence ~/.cache/tmp/habu-opt-round3/{boot,integrated}; no codec or former-copy reclamation claim.
---

Measured Maki reservations SYMS-BOOT (655,360), NORET-BOOT (131,072) and
SPA-BOOT (65,536) total 851,968 bytes and remain present and all zero after
shared-effect optimization. This is potential allocation elimination, not
achieved file savings or authorization for the held snapshot codec.

Design and independent review: `~/.cache/tmp/habu-opt-round2/` contains
`checker-boot-lifetime.md`, `checker-boot-lifetime-review.md` and
`spa-release-design.md`. Remove the static reservations. Lazy SYMS keeps
positive power-of-two logical capacity and guards lookup, raw growth and
persistence. Lazy NORETS preserves the empty terminator, IDs, histories and
rollback, and resets current capacity. SPA owns its mapping, retains it across
checks and trials, and releases the exact extent after growth and at quiescent
capture.

Failure modes: null read or zero-capacity mask; absent terminator; stale indexes
after a move; recycled live IDs; invalidated trial borrow; leaked or incorrectly
sized munmap; captured mmap address or nonzero count; former persisted live
copies still retained. The existing SPA fixture forges capacity and must use an
honestly sized allocation before unmapping.

Acceptance: existing growth, persistence, rollback, owner-transfer and restored
compilation E2Es, native fixpoint and full gate, Maki routing/export artifact.
Reconcile actual raw DATA, native sections and file deltas including former
persisted copies; report runtime allocation separately. No history pruning,
codec, general GC or ABI change. Serialize checker representation seams with
registry-capacity and history work; no correctness dependency on those tasks.
Owner: agent opt_checker_names, workspace `.jj-ws/opt-boot-storage`.

Implementation failure modes recorded before edits: SPA count multiplication
or doubling must not overflow; allocation failure must leave the old mapping
published; old-map release failure must clean up the unpublished replacement
and refuse without publishing it. Capacity always describes the actual mapped
extent. Full capacity must survive growth because saved trial indices may name
rows above a rewound SPN. Quiescent capture releases storage and resets pointer,
capacity and count together; NEW and trial restore only rewind counters.
Existing behavior fixtures will be adapted only where their forged capacity or
boot-pointer assumptions conflict with this ownership contract. No new unit
tests. Focused evidence: `~/.cache/tmp/habu-opt-round3/boot/`; completed native
fixpoint, full gate and Maki acceptance: `~/.cache/tmp/habu-opt-round3/integrated/`.
