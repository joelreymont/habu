---
title: "Let AIO:STOP wait for records an ended task left"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:58:10.359197+01:00"
---

Problem: src/lib/aio.f AIO:STOP tests ANY-BUSY? at once, while an ended task's SCRUB has only staged its cancels, so a load or program that joins its tasks and stops AIO throws E-AIO-BUSY now and then; ws-test and http-test retry for it. Acceptance: AIO:STOP waits for the forgotten records to complete, the retries are gone, and E-AIO-BUSY still answers a live waiter. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-aio-stop.
