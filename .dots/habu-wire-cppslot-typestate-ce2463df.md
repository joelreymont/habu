---
title: Wire CPPSLOT typestate into production PIPE-LOOP
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-17T17:42:02.355604+02:00\""
---

Follow-up of habu-checker-cp-async-6ba788a5 (landed 527babc1): the typestate capability + negatives are proven, but the production compute loop still emits issue/commit/wait/bar.sync atomically inside MM-PIPE-KLOOP-WITH's runtime $KLOOP - the CPPSLOT vocabulary (cpp-pending/committed/ready over symbolic parity, COMMIT/WAIT audited transitions, checked READ/READ-STAGE) is exercised by fixtures, not by the shipping kernel surface. Work: re-express the PIPE-LOOP quotation body so its emit-time protocol steps thread cpp-slot tokens through the landed CPP-* step words (cg-matmul-emit.f), keeping every pinned config byte-identical (tile-pipe-test EMIT-PIPED==EMIT-MATMUL exact equality is the hard gate; the 20-config golden method is the template); PIPE-LOOP's mmstage mint stays in the audited core, and there is deliberately NO ISSUE mint word - the trusted cp.async issue is the mint. If byte-identity cannot survive the threading, stop and report per the decomposition dot's precedent. Constraints: exclusive ownership of lib/ptx/tile-pipe.f + cg-matmul-emit.f + cg-mma.f in lockstep; engine untouched (the capability exists); full ptx suite + byte pins + maki. Files: lib/ptx/tile-pipe.f, lib/ptx/cg-matmul-emit.f, tests. Ownership: ptx staging typestate.

Claim: agent=cppwire workspace=.jj-ws/fable-cppwire (owns lib/ptx/tile-pipe.f + cg-matmul-emit.f + cg-mma.f in lockstep)

RE-SCOPED 2026-07-17 (cppwire lane, finding commit f0e63597; claim
RELEASED): the DOUBLE-BUFFERED production path is REFUTED for typestate
threading, evidence-backed: the single emitted $KLOOP body commits the
PREFETCH slot (kt+1) and waits the CURRENT slot (kt) - different slots,
loop-carried pairing, so the single-slot pending->committed->ready
lifecycle has nothing to bind to; plus byte drift (CPPSLOT:WAIT's
adjacent wait+sync matches neither split branch) and interface walls
(untyped [ -- ] quotation consumers in lower-mm/cg-mma; RB-TILE outside
scope; a self-minted-self-consumed token would be a tautology). This is
the same runtime-carried class as blocker 4 - permanently out of an
emit-time checker's model for the double buffer. NEW SCOPE: thread the
typestate through the SINGLE-BUFFER path (MMA-PIPE-KLOOP-SINGLE), which
has a clean per-iteration commit->wait->sync->read on ONE slot AND is
the shipping BEST kernel's path (MMM-WIDE-B-M4-S1 is stages=1
single-buffer). Ownership for the re-scoped work: lib/ptx/cg-mma.f +
cg-matmul-emit.f + cpp-slot tests, byte-identity discipline unchanged
(the 20-config golden method; stop on drift). The double-buffer
refutation stands recorded in LESSONS.
