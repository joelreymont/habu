---
title: "Keep ws-test's stray file under TMPDIR"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T18:26:34.102550+02:00"
---

Problem: src/lib/net/ws-test.f STRAY$ (:679-680) is build/ws-stray.txt, relative to the working directory, so two ws-test runs in one tree throw E-FS-OPEN at WS-TEST:RUN (seen 2026-11-25 with 12 copies at once). Acceptance: the file lives under TMPDIR, and parallel runs in one tree pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ws-hang.
