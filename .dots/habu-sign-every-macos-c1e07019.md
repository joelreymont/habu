---
title: Sign every macOS build with the team identity
status: active
priority: 2
issue-type: task
created-at: "2026-11-02T16:29:41.331897+02:00"
---

Lane carl-sign, commit rlxpkzpu: every macOS build signs with the team identity, and a missing certificate stops the build with CODESIGN:E-IDENTITY (Joel 2026-11-01 "Approve (Recommended)": "Missing certificate stops the build with a named error."). Lane review fold 1 (M1, M3, M4, L1, L2) accepted by the lead; rebased onto master 2e6258d4. Landing: build bin/hb on the rebased tree, full suite (/tmp/carl-sign/build.sh, log /tmp/carl-sign/build.log), land, install, record E-IDENTITY in LESSONS. Claim: agent=carl workspace=.jj-ws.noindex/carl-sign.

Status 2026-11-02: lane build rc 0; full suite 508/514, 6 red (hb-build-aot-image, build-fixpoint-fixtures, native-file-state, native-file-dynamic-load, native-file-auto, native-file-replay; /tmp/carl-sign/suite.log). Root-cause worker running against a master baseline (brief .out/carl-file/brief-sign-red.md).
