---
title: "Move GENIO's line reader into genio-core.f"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:41:53.064152+01:00"
closed-at: "2026-11-18T17:47:31.724867+01:00"
---

Problem: src/lib/genio.f reopens package GENIO (:15) to add READ-LINE and the TCP device, so GENIO cannot load before the build seals the engine, as the parent dot needs genio-core.f to. Acceptance: genio.f is gone, GENIO:READ-LINE is in genio-core.f, the REPL reads through it, and no file reopens GENIO. Depends: d305e42c. Claim: agent=worker workspace=.jj-ws.noindex/jerry-d6.
