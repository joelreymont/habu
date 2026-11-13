---
title: "Name the compiler's shared services without native"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.450064+02:00"
closed-at: "2026-11-04T11:48:49.717855+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Renames tier-1 packages (NEMIT, NPUB, NTRAP still: emission.f:11, publish.f:20); what T0 needs from tier 1 is copied into COMP instead (Joel 2026-11-03); jerry-r1 gone."
---

Joel 2026-10-29: names drop the tier, everything now. jerry-r1: NEMIT->EMISSION, NPUB->PUBLISH, NSTR->LITERALS (src/habu/literals.f), NTRAP->TRAP, moved to src/habu; NPUBWIN->PUBLISH-WINDOW, NSTR-ABI->LITERALS-ABI; primitives native-unit-publish->unit-publish, ndict-append->dict-append.
