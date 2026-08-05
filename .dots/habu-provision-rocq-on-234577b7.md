---
title: Provision Rocq on the Linux gate host
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.399475+02:00"
---

docs/bootstrap.md now names Rocq >= 9.2 as a build prerequisite and the seven test/compiler/*-proof.f parity gates fail loudly without it (dot habu-probe-rocq-toolchain-239abfd5, direction: no skip logic). spark has no rocq, so its stdlib gate is honestly red until the toolchain is provisioned there (opam or distro package on aarch64 Linux). Install it, verify bin/hb --load test/compiler/ir-id-proof.f prints test: ok on spark, and record the install step beside the Mac one in docs/bootstrap.md if it differs.
