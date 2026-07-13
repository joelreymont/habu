---
title: "Bootstrap: mirror tok-imm primitive"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T23:58:10.600022+02:00\""
---

Problem: master c6aed11f adds checker.f calls to tok-imm? and emits BTOKIMM only in src/habu/habu2.f; bootstrap/cg/forth.fs has no mirrored primitive. Exact recovery repro: HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh fails test/bootstrap-wide-memory.fs; running /tmp/nf-bin prints tok-imm? and exits 70. Fix: implement byte-identical semantic mirror in bootstrap/cg/forth.fs, register it in the mirrored primitive table, add native/recovery parity assertions and a negative recovery regression that fails when either registration is missing. Preserve LFIND clobber/frame contract and DNAME-IMM bit semantics. Verify Gforth bootstrap-wide-memory, full no-binary recovery, forced native fixpoint, full gate, Maki, PTX, typed/trust/host/filemap/dot lints. Claim: agent=root workspace=.jj-ws/type-dsl-ptr
