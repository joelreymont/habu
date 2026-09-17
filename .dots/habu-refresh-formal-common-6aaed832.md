---
title: "Refresh formal/Common/Storage.v's context comment"
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T00:56:24.702575+03:00"
---

Problem: formal/Common/Storage.v:744 says 'A live context: its generation, and the base of the mapping that backs it. IR-CTX:GENS and IR-CTX:BASES (context.f:111-112)' over Record ctx := MkCtx { cgen : nat; cbase : nat }, but IR-CTX:GENS no longer exists (the registry is HANDLES), BASES is now a declared TYPED-BUFFER (habu-convert-the-raw-d12eec95), and the line numbers point at the wrong lines (compiler conversion lane, 2026-09-18). The model asserts nothing about the storage form, so the proofs stand; the comment misleads. Acceptance: the comment names the current words and lines and states that the model treats a base as an opaque natural; make -C formal still builds; test/compiler/reloc-proof.f and the storage proof rows unchanged. Files: formal/Common/Storage.v. Verify: make -C formal; the proof gates. Depends: none. Ownership: formal model. Claim: unassigned.
