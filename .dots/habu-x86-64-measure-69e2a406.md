---
title: "x86-64: measure whether WRITER-MACHINE-CK still guards a foreign build"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:44.287115+02:00"
---

Problem: after R2b (qzvmqxko) tools/native-build-core.f RESTORE-HOST-DISPATCH gives back the four compiler entries after the capture, but not the window seal's other installs. WRITER-MACHINE-CK (:924) refuses an x86-64 host building for another machine without a writer loaded before the window; test/native-build-entry.f :152 pins its message. Unmeasured whether a writer loaded after an arm64 window's capture now compiles correctly on an x86-64 host. Acceptance: on joel@pulsar-ua-106, build --target for arm64 with the writer check removed; if the writer compiles and the output runs, delete the check, its message and the test branch; else state in the comment which seal install breaks it. Depends: x86-64 host, R2b landed. Claim: unassigned.
