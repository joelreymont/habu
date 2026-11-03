---
title: "Make a callback's C entry in the step that defines it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:54:25.237579+02:00"
---

Joel 2026-11-03, not yet settled as a design: a callback is defined for the program's life to bridge one Habu word; no request left between calls, so nothing to end; the engine's 16 ready-made stubs (layout.f CB-POOL) go; he asked why FFI-CB is a separate package. Replaces lane 2 of habu-copy-the-tier-ca6204f0 (brief /tmp/jerry-big/cbreq-brief.md, withdrawn) and closes habu-disarm-a-cb-7870abbf and habu-give-the-cb-e6876f60 when it lands. T0 takes on no N* package (DECISIONS "T0's callback request and shadow cell"). Design: Plan on Fable, then critic, then Joel's public items. Claim: agent=jerry, design at the critic (design /tmp/jerry-big/cbdef-design.md, Plan prompt /tmp/jerry-big/cbdef-plan-prompt.md), no workspace.

Design rev 2 2026-11-03 (Fable on Plan): /tmp/jerry-big/cbdef-design-r2.md, answering critic round 1 (/tmp/jerry-big/cbdef-critic-r1.md). A callback's C entry is a fixed body: COMP:body gains `callback`, the slot is its value as a constant's number is, and `CALLBACK:` defines NAME-ENTRY through a public OUTER:DEFINE-FIXED that DEF-FIXED routes to FIXED-COMP-CELL at every latch; the stub is the whole record, so its code address is the entry. Both request cells, the 16 stubs, `callback-entry`, FFI:CALLBACK-ENTRY and FFI-CB:ENTRY-ADDRESS go; INSTALL-ENTRY goes private; T0 stops calling NDEFINITION's callback group and NCBSTUB, which stay in tier 1 with no caller; x86-64's thunk becomes the helper (CALLBACK-THUNK); test/x86-64-kernel-callback.f goes; FFI-CB folds into FFI with CALLBACK- tails. Lead's note: the proposed NCOMP-DISPATCH:ORIGIN-CELLS names the NATIVE-BODY record that origin O2 deletes, so it goes if the cell lane follows O2. Critic round 2 running.
