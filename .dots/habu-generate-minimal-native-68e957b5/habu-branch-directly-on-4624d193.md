---
title: Branch directly on CASE comparison flags
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T19:54:20.881110+02:00"
blocks:
  - habu-slim-match-emitted-66941fb5
---

Measured at master 3909bbac. J-OF emits cmp; cset x9,eq; cbz x9,next for every CASE arm, materializing a boolean that is consumed only by the immediately following branch. The arm dispatcher is 28 bytes; cmp followed by a forward b.ne makes it 24 bytes, saving exactly 4 bytes and one executed instruction per arm. Controlled one- and two-arm definitions are 124 and 180 bytes and should become 120 and 172. Root cause: the current forward patch chain supports unconditional B placeholders but not conditional branches; the boolean is an encoding workaround, not a semantic value. Fix after habu-slim-match-emitted-66941fb5 adds typed B.cond forward patching: emit cmp; b.ne next; drop for CASE OF and preserve the existing patch-chain ownership. Acceptance: exact 120/172-byte fixtures; first, later, no-match/default, nested CASE, ENDOF/ENDCASE joins, malformed control, checked effects, forward range failure, AOT/snapshot relocation, bootstrap mirror, fixpoint x2, both targets, full control-flow gates, and exact ratchets pass. Files: src/habu/habu2.f J-OF, bootstrap/cg/forth.fs, CASE disassembly/runtime tests, and size gates.
