---
title: Check public module identity issuance
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T16:00:36.031458+02:00"
---

NEW-MODULE in src/compiler/ir/id.f is the public issuer of module identities.
A mutation to `TAKE-SERIAL 1000 mod 1+ dup MINT-KEY swap MINT-MODULE`
cycles serials every 1000 calls while the existing native identity tests pass.
A driver minting 1001 modules demonstrated duplicate module IDs.

Exercise the public issuance contract through native behavior tests: identities
remain unique across the demonstrated wrap boundary, and each minted key
matches its module identity. Both the period-1000 mutation and a mutation
minting key and identity from different serials must fail. The unmodified
implementation must pass. The retired theorem subsystem is not required.
