---
title: Refuse a checked tick of a trust-boundary word
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T06:16:43.204529+03:00"
---

Problem: a checked tick of an ABI-only word (no primitive, no owner row) is admitted while a call to it is refused E-CAP-TRUSTED. checker.f OWNED-TICK-REFUSED? (:17920) refuses only when PE-OWNER-ROW (:14379) finds an owner row, and BTICK-TOK (:17933) then pushes the row's quotation type. Measured on master 9d126e86 bin/hb with /private/tmp/claude-501/-Users-joel-Work-habu/45544816-68f7-4358-b8ff-9239acfd1105/scratchpad/rev-b8/probes/p-tick.f (`1 set-tier`, PRAW defined under `0 set-check`, hook restored, `: PTICK ( n -- n ) ['] PRAW execute ;`): certifies, rc 0. The same file calling `PRAW` directly: E-CAP-TRUSTED 'PRAW' is a trust-boundary primitive, rc 70. Found by the B8 review; pre-existing on its parent product. Acceptance: a checked `[']` or `'` of a word whose call is E-CAP-TRUSTED is refused with the same code, naming the word; ticks of owned or checked words unchanged; rejected-program fixture for both tick forms beside the call case. Files: src/core/checker.f (OWNED-TICK-REFUSED?, BTICK-TOK), the E-CAP-TRUSTED fixture suite. Verify: the probe exits 70 naming PRAW; checker suites; test/run.f. Depends: none. Ownership: src/core/checker.f tick path. Claim: unassigned.

Proof on 70336539 parent, before root gates:
- `test/internal-word-gate.f` added a real product ABI-only row and checked `[']` cases before the checker edit. On the matching B8 product host, the returned quotation and `execute` cases both exited 0 instead of 70 (four failed assertions); the direct call already refused E-CAP-TRUSTED.
- The row-hit path in `OWNED-TICK-REFUSED?` now consults `CALL-AUTHORITY` without requiring an owner row. The no-row owner-private case still uses `PE-OWNER-ROW`; `BTICK-TOK` still handles current and expired recovery rows. A tick forms a quotation without matching the callee's input stack.
- Private rebuilt product `hb`: `test/internal-word-gate.f` exits 0, `test: ok`. It checks the direct call, both checked tick shapes, an admitted checked-word quotation, an admitted primitive tick, and top-level interpreted `'` of the ABI-only word. The named E-CAP-TRUSTED diagnostic is pinned at the ticked word.
- Private rebuilt whitebox `hb-whitebox`: `test/checker-effect-authority.f` exits 0, `test: ok`; unsealed FRESH tick binds, same-run recovery tick remains admitted, expired recovery tick refuses, and an unrelated ABI-only tick refuses during recovery.
- The existing owner window fixture on that private whitebox exits 0 with `prim-owner: ok` and `window: 0`; its transcript includes admitted tier-0/tier-1 ticks inside owners and refusals outside.
- The full native registry, generation convergence, and independent adversarial review remain for root before landing. Keep this dot open until those gates and integration finish.
- On frozen parent `346d0940`, `1 set-tier` and `: PT ( -- ) ['] patch32 drop ;` exits 70 with the bare `habu: in pt: at '[']'` diagnostic; the added `test/internal-word-gate.f` assertion fails once. `BTICK-TOK` now pins the trusted primitive target and sets `CAPREQ` on its early refusal. One rebuilt official product exits 70 with `E-CAP-TRUSTED` naming `patch32` on that probe, and the focused internal-word gate exits 0 (`test: ok`). Before and after logs are retained under `~/.cache/tmp/dave-b8-tick-qualification/` and `~/.cache/tmp/dave-tick-pin-fix/`.
