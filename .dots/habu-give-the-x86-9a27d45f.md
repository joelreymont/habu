---
title: Give the x86-64 kernel the replay primitives
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T19:07:10.605130+02:00"
---

Problem: src/habu/kernel-x64.f ~2611-2616 registers replay-open, replay-close, replay-widn!, replay-record, record-wid! and replay-private with REFUSE-WID, so the checker's replay dies with 'hb: replay-open is not in the x86-64 kernel'. On master efef8267's x86 engine (joel@pulsar-ua-106) it ends these rows: live-verify-source linear check-repair-hints checker-verify-pkg-scope structure-opaque using-import verify-binding replay-binding typed-storage checker-dup-record tool-boundary-lints tool-boundary-check-repair type-linear pointer-storage compiler-native-hookless-reject. With T0 at tier 0 on x86 (C4b, jerry-t0x86) it also ends test/tier.f TEST-TRUSTED-TICK-ORDER's two include cases (include test/tick-provider-replace.f and -unit.f, exit 76, F39-41 F43-45), which master refused earlier at 0 set-tier. The arm64 twins: habu2.f DEFWRITE:REPLAY-OPEN (~4187, registered ~13818) and its siblings; prims.f ~961-984 states the contract; layout.f ~1948 the saved scope band. Acceptance: the x86 kernel implements the six primitives as arm64 does; those rows pass on the x86 peer or fail for a different, recorded cause; tier passes on x86. Files: src/habu/kernel-x64.f, src/habu/layout.f if the band needs an x86 slot. Verify: x86 cross-build, then the listed rows on joel@pulsar-ua-106. Depends: habu-install-t0-as-6492133d landed. Ownership: kernel-x64.f replay rows. Claim: jerry, lane rpB held; rpA landed.

rpB held 2026-11-03: Joel's order puts the linker on arm64 before new x86-64 lanes (DECISIONS.md "x86-64 builds"; dot habu-build-x86-64-3ed6f45d). Critic round 1 REJECTED rpB. Rewrite the INSERT so row x86-64-kernel-engine passes. The wid and the index must ride the machine stack across WINDOW-SPAN,. Hold the tier to "passes". Drop the habu1.f comment edit, and fix the cited lines. Revise the brief only when x86-64 work resumes.

rpA landed 2026-11-03 ("Give x86-64 replay-open, -close and -widn!"): the three rows have x86-64 bodies, the twins of habu2.f DEFWRITE's, and OVERLAY-GUARD, guards def-open, namespace-private, alias-record and the `wordlist` row. replay-record, record-wid! and replay-private still refuse, and they are rpB's. The code review accepted the bodies, the registers across WINDOW-SPAN,, the DPBAD split, the guard placement and the row move, and found no ordinal dependency. Its five comment findings are folded in, and the x86-64 cross-build after the fold is byte-identical to the reviewed one. Proofs ran on pulsar. Never exercised by a proof: replay-widn!, the guard in def-open, namespace-private, alias-record and native-unit-publish, and the zeroed record bytes themselves (only the counts were checked).

Also ends test/certify-does-definer.f on x86 (rc 76, `hb: replay-record is not in the x86-64 kernel`): master 04a4dffa cross-built, joel@pulsar-ua-106, 2026-11-03 (carl).

Census 2026-11-04: the claim named a workspace or agent whose work is gone or landed; reset to open, Claim: unassigned.
