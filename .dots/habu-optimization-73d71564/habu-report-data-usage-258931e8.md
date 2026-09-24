---
title: Report DATA usage at build and name the ceiling
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T23:47:54.229362+03:00"
---

Problem: Linux DATA-SIZE is 32 MiB (src/os/linux/layout.f) and an AOT image pays dictionary DATA twice, so Tender's 'build --server' died with 'hb: data space out of range' (76) once about 25 ordinary definitions joined an image that allotted ~13 MB of buffers (aspen 2026-09-17, ~/.cache/tender/habu-gaps/literal-bytes-per-definition.md); the margin is invisible until the link refuses, and the refusal names neither the usage nor the ceiling. Acceptance: hb-build and the AOT linker print a data-usage line (bytes used of DATA-SIZE, the two dictionary DATA copies itemised) so a program sees its margin before it fails, the out-of-range refusal names the usage and the ceiling, and docs/native-applications.md explains why an image pays the dictionary DATA twice and how to keep large buffers out of the image (map at first use). Files: src/habu/aot-lib.f, tools/hb-build-report.f, src/os/linux/layout.f (the constant's comment), docs/native-applications.md, test/. Verify: a fixture near the ceiling reports its usage and the refusal names it; test/run.f. Depends: none. Ownership: AOT linker and build report. Claim: unassigned.
