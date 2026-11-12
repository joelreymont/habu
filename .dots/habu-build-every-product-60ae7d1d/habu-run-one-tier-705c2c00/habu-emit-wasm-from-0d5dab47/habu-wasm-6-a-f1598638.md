---
title: "Wasm 6: a Wasm build links unchanged files' objects"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T21:32:06.928239+02:00"
---

Problem: a build for another machine than hb's (--target wasm; x86-64 next) compiles every file on every build: src/habu/linker.f CROSS? (:135) keeps it from writing or linking objects (:1380, :1866, :3972, :3976), because an object holds one piece of code per definition and such a build makes two, the target's and the arm64 copy later files run while they compile. Measured 2026-11-10 on a copy of Maki's viewer: Wasm 3.79 s on every build, 0 objects; arm64 8.18 s with no objects (216 written, the kernel build's 117 included) and 0.98 s unchanged, byte-equal; the checker is about half of the Wasm build's samples, tier 0 and the Wasm backend 2.4%. Settled: DECISIONS.md 'Wasm through the one linker' (an object carries both copies). The design is being written (brief .jj-ws.noindex/jerry-wasm/.out/briefs/design-warm.md) and goes to Joel before a worker starts; the steps become child dots. Acceptance: a Wasm build of an unchanged program links every file and compiles none; after an edit it compiles what an arm64 build compiles after the same edit; the module is byte-equal to a build's with no objects; CROSS? no longer keeps a build from objects; an arm64 build's objects and bytes are unchanged or the cold build is rerun once. Files: src/habu/linker.f, src/compiler/native/shadow.f, src/habu/t0.f, per the design. Verify: warm bytes equal cold for Wasm and arm64; the suite; the chain check. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s3w.
