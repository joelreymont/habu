---
title: "Rebase captured WIDs into the target's space at seed"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-15T20:02:58.506973+02:00\""
---

P1 soundness defect found by bake-chain-6 (2026-08-15): a captured AOT record travels with its CAPTURE-PROCESS wid and EM-SEED-AOT registers it against the TARGET engine's live wid space - two unrelated numbering spaces. Isolated on the parent commit: two dummy host packages shift the fixture's AWBGATE from host wid 205 to 209; target wid 209 is sealed, so the O(1) boot gate refuses (exit 84) - but a captured wid aliasing an UNSEALED target wordlist registers silently into the wrong package. Both engines' protected bitmaps byte-identical (36 wids, max 198); target WIDN 288, so every host wid below 288 aliases a real target wordlist. The leaf e98b03d4's '137 chain WIDs all land above the target's WIDN - measured' is measured, NOT enforced: host-only closure packages (aot-capture/aot-arm/aot-ident/aot-file) shift every later host wid relative to the target, which is exactly what the isolation experiment reproduced. Fix direction the code itself suggests: EMIT-AOT-PROT-RESTORE already advances WIDN past the highest restored wid in the other direction - captured record wids need the equivalent rebase into the target's space at seed time (or WIDN advanced past the highest captured wid BEFORE registration, making alias impossible by construction). Acceptance: a fixture that bakes a package whose host wid aliases (a) a sealed and (b) an unsealed target wordlist, both refused-or-rebased correctly, mutation-proven; the e98b03d4 chain bake items (d)-(g) must not ship on the accidental above-WIDN alignment - they depend on this dot or on a structural proof of the invariant. Interim boundary (ruled): test/aot-wid-build.f asserts the wid it bakes is not protected in the target and refuses BY NAME - a loud precondition, not a skip.

Claim: agent=bake-chain-7 workspace=.jj-ws/habu-bake-chain

LANDED 2026-08-15 (bake-chain-7). BASELINE, both halves reproduced
on the real build path with the interim burn suppressed: (a) the
fixture's package baked at host wid 209, sealed in the target, and
the engine refused to boot - `hb: AOT protected-WID gate reject`,
exit 84, in gate mode 2 which asks for no protection at all; (b)
burned to 211 the engine booted exit 0 and its own dictionary shows
`AWBGATE pub=211 prv=212` beside `LOWER-CERT pub=211 prv=212` -
two packages owning one wordlist, silently. Option B of this dot is
REFUTED: WIDN governs future allocation, so advancing it changes
neither baseline.
THE FIX is the rebase, wid as a window-relative coordinate:
aot-capture.f WID-SPAN latches [W0,W1) (mandatory, mirroring
PRELUDE-MARK), the audit refuses any record wid that is neither 0
nor in the window BY NAME, the window's own sealed wids move out of
the bitmap into a window-relative u32 table, and at boot
AOT-WINDOW:REBASE-WID, maps 0 to 0 and an in-window wid to
WIDN+(wid-W0), refusing anything else; WIDN then moves once, past
the whole span. Artifact VERSION 2 carries the base, the span and
the sealed table. ONE unsigned bound test does both sides (the
below-base case wraps high) - proven by the mutation that deleted
the second.
EVIDENCE: six mutations, each redding a named case (rebase emits
nothing -> the alias cases exit 84 again; refusal deleted -> both
forged windows accepted; wid 0 rebased too -> the rebuilt engine
cannot boot; WIDN advance skipped -> "the engine's next id is past
every id its records claim" fails; seal split deleted -> the boot
gate stops rejecting; capture audit deleted -> the narrowed-window
build no longer refuses). Interim boundary and its three emitted
words retired. Fixpoint 161e8f2b x2, 165367 bytes; CODELEN
129528 -> 129960 attributed region-by-region against a control
build of master that reproduced d151fbac byte for byte.
Chain census now reports widw0=288 widspan=137 pwin=125.

