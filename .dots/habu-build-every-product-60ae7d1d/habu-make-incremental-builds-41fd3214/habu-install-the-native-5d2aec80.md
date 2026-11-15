---
title: Install the native host before a build opens
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T05:19:59.730377+02:00"
closed-at: "2026-11-13T05:25:28.074493+02:00"
---

Problem: src/compiler/native/host.f NHOST:INSTALL runs on a process's first compile (definition.f:561 STAGE-UNIT -> SOURCE-BEGIN). When that compile is inside an open build, as once hb carries hb-build's files, habu2.f ROUTE-HEADER sends its xt! of INVALIDATE into the image's NATIVE-OBS-CELLS:HOST-INVALIDATE, the linker refuses it (linker.f REFUSE-CELL, E-COMP-STATE) and hb's own cell stays 0 while OWNER-INSTALLED is 1. Acceptance: the install runs in LINKER INSTALL-LOOP before IMAGE-IN, with the linker's other hooks; an engine carrying hb-build's files builds an engine. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nh.
