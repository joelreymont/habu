---
title: Branch directly on CASE comparison flags
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-07-19T19:54:20.881110+02:00\\\"\""
closed-at: "2026-07-19T22:41:22.712253+02:00"
close-reason: "Landed at master 3e13e4bb (commit 'Slim CASE OF dispatch: cmp + b.ne per arm'): J-OF now branches straight off the compare flags (cmp; b.ne next; drop), dropping the materialized cset boolean - 4 bytes and one executed instruction per arm, exact dot targets hit (one-arm 124 to 120, two-arm 180 to 172, byte-level dis recorded above TCASE in test/engine-suite.f). Uses the B.cond imm19 LPAT class-path landed by the MATCH slimming; the gforth bootstrap emitter is deliberately NOT mirrored because its LPAT still selects by bit 31 and would misfile a b.ne - the recovery seed's larger dispatch never reaches the final engine (native fixpoint refresh replaces it). Fixpoint 3f128c23 x2 byte-identical; full CASE matrix, AOT/snapshot relocation, and the complete battery green including an in-band stable run.f pass (exit 0). macOS CODELEN row honestly lowered 127536 to 127524; Linux row carries the -12 prediction for the next linux-arm64 fixpoint."
---

Measured at master 3909bbac. J-OF emits cmp; cset x9,eq; cbz x9,next for every CASE arm, materializing a boolean that is consumed only by the immediately following branch. The arm dispatcher is 28 bytes; cmp followed by a forward b.ne makes it 24 bytes, saving exactly 4 bytes and one executed instruction per arm. Controlled one- and two-arm definitions are 124 and 180 bytes and should become 120 and 172. Root cause: the current forward patch chain supports unconditional B placeholders but not conditional branches; the boolean is an encoding workaround, not a semantic value. Fix after habu-slim-match-emitted-66941fb5 adds typed B.cond forward patching: emit cmp; b.ne next; drop for CASE OF and preserve the existing patch-chain ownership. Acceptance: exact 120/172-byte fixtures; first, later, no-match/default, nested CASE, ENDOF/ENDCASE joins, malformed control, checked effects, forward range failure, AOT/snapshot relocation, bootstrap mirror, fixpoint x2, both targets, full control-flow gates, and exact ratchets pass. Files: src/habu/habu2.f J-OF, bootstrap/cg/forth.fs, CASE disassembly/runtime tests, and size gates.

Claim: agent=case-bcond workspace=.jj-ws/habu-branch-directly-on-4624d193
