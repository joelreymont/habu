---
title: Give AIO handles a generation beside the record index
status: closed
priority: 3
issue-type: task
created-at: "2026-09-22T00:48:36.852863+03:00"
closed-at: "2026-09-22T04:55:11.557749+03:00"
close-reason: "Landed as 59f6820d: a handle is gen*MAX-OPS+idx, REC.GEN bumped by REC-FREE, OWNED-CHECK answers the index and refuses a generation mismatch with E-AIO-STATE, groups hold handle cells; aio-test cases 18 pin a stale ticket, a stale xfer over a timer, and a stale ticket over a live transfer (red on the base: the two handles were the same bare index)."
---

Problem: an AIO:ticket or AIO:xfer is a bare record index (lib/aio.f OWNED-CHECK checks the state and the owner only), so a handle kept after its record was freed and reclaimed by the same task passes OWNED-CHECK and names the new operation: AWAIT waits on the wrong operation; AWAIT of a stale ticket whose record now holds a transfer frees the record through TAKE/REC-FREE and the allocation leaks (REC.HOLD cleared without RELEASE-BYTES); AWAIT-XFER of a stale xfer whose record now holds a poll hands back a null pointer with the stale extent row. Found in the completion-ops review. Acceptance: the handle carries a generation beside the index (a counter REC-FREE bumps, folded into the cell the NEWTYPE wraps) and OWNED-CHECK refuses a mismatch with E-AIO-STATE; a regression in lib/aio-test.f awaits a ticket after its record was reused by the same task and pins E-AIO-STATE; three generations with gen2 == gen3. Files: lib/aio.f, lib/aio-test.f, docs/aio.md. Verify: lib/aio-test.f, test/run.f. Depends: none. Ownership: hazel.
