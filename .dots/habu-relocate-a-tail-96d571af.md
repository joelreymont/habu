---
title: Relocate a tail branch across a snapshot
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T14:01:24.302190+02:00"
---

src/compiler/native/publish.f refuses (E-NPUB-RELOC, -8621) an emission that leaves through a branch to an address OUTSIDE the JIT code region, because the snapshot relocation record cannot describe one: the engine's loader walks the recorded sites and exits CALLMAP-RC when a site does not hold a branch-with-link (src/habu/habu2.f EMIT-CALLS compares the top six bits against BL-OP-HI, $25). A tail branch to the engine's own loaded text is therefore unpublishable today, so src/compiler/native/select.f never gets to build one for such a callee and a habu word that tail-calls a sealed engine word keeps its call and its return. Closing it means widening that comparison to the imm26 PC-relative branch forms (B as well as BL) in the engine's emitted relocation pass, and widening the machine-checked model it is held against - formal/Common/Reloc.v plus the pinned BL-OP-HI rows in test/compiler/reloc-schema.f, reloc-cases.f and reloc-obligations.f, which today build every recorded site's word from that one opcode. Falsify the widening by mutation in both directions before believing it.
