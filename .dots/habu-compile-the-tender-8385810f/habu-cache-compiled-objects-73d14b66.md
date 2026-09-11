---
title: Cache compiled objects per file and link
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.282890+03:00"
---

Problem: every build recompiles every file; the toolchain rebuilds itself on every run. Joel (2026-09-11): the toolchain is built once and objects are cached until something changes. Acceptance: per-file compiled objects keyed on source digest and dependency digests, a linker that composes an executable from objects, a rebuild with nothing changed doing no compilation, a one-file change recompiling one file; measured. Files: tools/hb-build-lib.f, tools/native-build.f, the linker. Verify: build twice, the second is milliseconds; change one file, one object rebuilt. Depends: habu-build-the-compiler-c348eab0. Ownership: cedar. Claim: unassigned
