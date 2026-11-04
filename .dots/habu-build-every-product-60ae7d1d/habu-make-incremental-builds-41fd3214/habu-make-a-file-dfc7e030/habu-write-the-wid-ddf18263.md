---
title: Write the wid cells at the x86-64 boot
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T14:54:29.943959+02:00"
closed-at: "2026-11-04T20:07:09.780729+02:00"
close-reason: "implemented, reviewed (ACCEPT), merged: candidate /tmp/jerry-pushA/hb built from 605fb2f5 (rc 0); full suite bin/hb --load test/run.f ran 521 of 521, rc 0, no red rows (/tmp/jerry-pushA/suite.log)"
---

Problem: BOOT-LAYOUT:GLOBAL-WL-CELL and NAMESPACE-WL-CELL (src/habu/layout.f, step 3a) are written 0 and DICT-WL:NAMESPACE by every arm64 boot only; shared Habu code that will read them (xref.f, outer.f, compiler/native/dict.f, core/internal-mark.f, checker.f; step 3c/3d) also runs on x86-64, where nothing writes them. Acceptance: src/habu/boot-x64.f DATA-INIT, writes both cells on every x86-64 entry; a probe on pulsar prints 0 -1; using-test, package-seal and export-package as on the parent's x86-64 engine. Files: src/habu/boot-x64.f. Verify: brief /tmp/jerry-big/wlcell-x64-brief.md proof. Depends: habu-hold-the-global-ca0d8e61. Ownership: boot-x64.f DATA-INIT,. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wlcell-x64; brief /tmp/jerry-big/wlcell-x64-brief.md.
