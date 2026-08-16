---
title: Gforth stage-0 path broken on this host
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T06:38:19.830044+02:00"
---

Pre-existing on CLEAN master (proven by bake-chain-12 with and without its change, identical failure): tools/bootstrap.sh's wide-memory gate and any FORTH-EXE engine die 'pick-reason at CODE-REASON' rc 70 from src/core/generated-declaration.f. The recovery host is dead on this host - a real risk (bootstrap.md names gforth as THE recovery road if bin/hb is lost). The bootstrap-mirror lint still passes structurally, so mirror drift is not the cause. Diagnose from the generated-declaration.f error outward; bisect against the last known-good stage-0 run if one is recorded.
