---
title: "Drop a refused definition's shadow rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.785778+02:00"
---

shadow-kinds (35ae4a04) measured 'stale shadow rows: 2': a section refused after it filed shadow rows leaves them in NSHADOW, and the record index they name is reused by later words. A cross-build stops on a refusal, so this bites only a refusal caught inside product text; the linker must not read a row whose record was rolled back.
