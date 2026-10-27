---
title: Run host loaders as private words
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:28:42.943720+02:00"
---

Problem: four one-word packages exist only to load a host or target file after their main package closes: AIO-LOAD (src/lib/aio.f:1183), SECRET-LOAD (src/lib/secret.f:174), JITDUMP-LOAD (tools/jitdump-core.f:6), NATIVE-EMIT-LOAD (tools/native-emit.f:8). Ruling (Joel 2026-10-27, interview): each loader becomes a private word in the main package, its xt taken before ;package and executed after, so the *-LOAD names leave the dictionary. Acceptance: the four packages gone; rg 'AIO-LOAD|SECRET-LOAD|JITDUMP-LOAD|NATIVE-EMIT-LOAD' src tools test docs empty; host files still load on macOS. Verify: rows loading aio, secret, jitdump and native-emit. Depends: none. Ownership: carl. Claim: unassigned.
