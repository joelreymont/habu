---
title: "Lower a TRUSTED: body that binds a wide input to locals"
status: active
priority: 1
issue-type: task
created-at: "2026-10-23T23:10:08.204356+03:00"
---

Regression from the tier-0 TRUSTED: scan stack (slnrrlyu..sqzxqknl): test/c2-memory-runtime.f passes on master, fails at slnrrlyu (rc 76 replay drift) and at sqzxqknl (rc 75 'hb: incomplete layout facts: READ-ROOT'). READ-ROOT binds a wide mut-view into {: p:ptr len:n :}. Lane: jerry, worker in .jj-ws.noindex/jerry-t0bind (on sqzxqknl). Reproducers ~/.cache/tmp/jerry-t0bind-cases/. Blocks landing of 3913bb32's stack.
