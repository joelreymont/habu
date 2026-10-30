---
title: "Record the cold load's reads as support inputs"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T23:04:05.511860+02:00"
---

The support child (native-build-image.f PROGRAM!) runs COLD-LOAD:RUN before its recorder starts, so no file the cold load reads (startup files, native-runtime.f's units, cold-load*.f, build-producer.f) has a content row in native-build-support.inputs: an edit to one reuses a support image built from old host code (warm != cold). lzmtpwoz drops build-producer.f's row (68 -> 67). Lane carl-fsup (brief-fix-supinputs.md), new commit on lzmtpwoz; lands with it before lane 11. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fsup.
