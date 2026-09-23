---
title: Raise the per-definition body text capacity
status: active
priority: 3
issue-type: task
created-at: "2026-09-18T09:02:58.024196+03:00"
---

Problem: BODYBUF-CAP is 8000 bytes. The engine's captured definition body
lives at DATA+$800, boxed in by the header cells above it. VERIFY:BODY-BUF
and NCOMP:TEXT-CAP already derive their sizes from that same constant.
The named overflow refusal is implemented; the small capacity remains.
Ownership: engine layout. Claim: Alder; layout seam awaiting Hazel's review.

Current reproduction on 806f0654 / engine 274f9bea: nine 900-byte string
literals refuse rc 71, naming NINE-LITERALS, capacity 8000 and needed 8214.
The complete source is 8239 bytes. Evidence and the Habu source generator:
~/.cache/habu/body-cap/source-806f0654/.

Proposed implementation: reserve BODYBUF-CAP + 2 bytes in a dedicated DATA
band after TIER-PROV:END, align the new DATA-START to a cell, and use 64 KiB
for the shared cap. This covers the measured literal table with headroom
without moving any other existing header cell. Keep BODYBUF in the existing
protected-band table: it is protected today, despite an old pending-defer
comment calling the BODYBUF class unguarded. A static reserved band keeps
the current snapshot semantics and needs no new process-owned pointer,
mapping or capture hook.

All body-buffer address emitters must materialize its new offset rather than
use the 12-bit ADDI form. Capacity loads must also represent 65536 rather than
use the 16-bit MOVZ form. Apply the same changes to the Gforth mirror. The
hosted emitter reads its host's baked layout (habu2.f EM-LAYOUT documents the
existing boundary), so the first product is a bridge; promote only after
the new-layout generations converge. No compiler/checker source change is
proposed: their existing allocations and bounds read BODYBUF-CAP already.

Acceptance: the nine-literal program executes; the runtime regression still
admits exactly the cap and refuses cap + 1 by name; the replay fixture grows
its source capacity from the shared bound; writes into the relocated buffer
remain refused. Record actual engine size and relevant compile/startup costs.
Three private generations with gen2 == gen3, check-only bootstrap, every
owning registry row (including the bootstrap readers), then the integration
gate. Rebase implementation onto the current integrated head before editing
the engine; do not integrate this proposal as an implemented fix.

Expected files: src/habu/layout.f, habu1.f and habu2.f; bootstrap/cg/forth.fs;
the capacity/protection/replay fixtures and relevant docs. Verify-source and
native/compiler.f are read-only consumers unless measurement proves another
change is needed and its owner releases that seam.
