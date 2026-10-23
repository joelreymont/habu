---
title: Specify Windows x64 and ARM64 native profiles
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:29.013488+03:00"
---

Windows x64 and ARM64 compiler hosts remain required and unqualified in the
adopted portability design. Keep this parked until a Windows/COM design and an
available host lane exist. No Windows work is part of current Maki deployment.

When brought up, implement the actual PE/DLL, unwind, TLS, ABI and OS boundaries
needed by the target, then qualify native self-build and real consumers on each
host. The retired generic fragment/HostServices frameworks are not prerequisites;
use existing responsible providers unless a demonstrated boundary requires an
extension. Keep docs/xcomp.md "Target contract" stating which Windows profiles
are declarable only and which are qualified.
