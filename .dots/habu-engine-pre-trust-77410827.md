---
title: "Engine: pre-trust defer capability"
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T21:28:55.114752+02:00"
---

Capability dot minted from the stage2b BLOCKED report (habu-migrate-engine-hooks-e4f31fc6): native C-DEFER (habu2.f:2317) unconditionally runs C-CALL-TRUST-PEND (habu2.f:2341 -> C-FIND-TRUST) which LFINDs the checker word 'trust', first defined at checker.f:7685-7687 - so a bare 'defer NAME ( E )' declared ANYWHERE before that line (any checker.f cell before 7687, and all of the B5 block at checker.f:418-431) writes 'trust' to fd 2 and exits 70 at boot. Fixture (acceptance red-case): 'defer S2B-PROBE-XT ( -- )' at checker.f ~2500 -> exit 70 printing trust; positive control after 8352 -> exit 0 with checked is. This is the ORIGINAL blocker-2 of the stored-xt program - the checker-defer bridge (5d2f6d29) fixed only the stage0-mirror rejection, not the native ordering; the pre-existing LESSONS entry ('inside checker.f only post-TRUST hooks can become defers') documented it and the 2b design scout overlooked it. Work options (decide at dispatch, native + stage0 mirror parity + pinned counts): (a) PENDING-REGISTRATION - C-DEFER branches on trust/checker-defer findability; if absent, record the defer's name+effect in a pending table the checker drains when TRUST/checker-defer come alive (the same late-binding pattern the hook cells themselves use; fail-closed if the table is never drained: boot-end check dies naming undrained defers); (b) reorder trust/checker-defer registration earlier in checker.f (tfam-surface surgery, coordinate); (c) the typed pre-prefix install primitive from the original blocker-2 text. Acceptance: the fixture compiles+certifies at checker.f ~2500 native AND through the full recovery chain; existing post-7687 defers unaffected; fixpoint x2; wide-memory + recovery green; negative regression pinning whatever fail-closed shape option (a) adds. Unblocks: stage-2b pre-7687 class (BADSIG-XT, REG-SCRATCH-SNAP-XT, LOCSHOWXT, REG-EXT-PERSIST-XT, SIG-QUOT-XT) AND the entire B5 block. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, src/core/checker.f (option b), tests. Ownership: engine defer machinery.

IMPLEMENTER CONSTRAINT (from the landed clean-4 partial): die-class default-is
bodies must locals-consume their declared inputs ({: a:ptr u:n ... :} before
the die) or the fixpoint certify pass rejects with E-BUILD-CERTIFY at the die
site - boot alone does not exercise certify; the stage certify is the
authority (TF-SHA16-UNSET precedent, LBUF-EVAL locals shape).
