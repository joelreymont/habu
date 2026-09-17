---
title: Complete shared library reviews and image lifecycle integration
status: closed
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.386256+03:00"
closed-at: "2026-09-16T14:34:48.336519+03:00"
close-reason: "superseded by habu-campaign-c5-runtime-723833a3: Residue: the shared library and image-lifecycle integration for FMATH, BUF/VEC, TASK callbacks, ZIP/XML and the UDP/serial stack still needs one owner and focused suites."
---

Owner: Cedar; peers retain owned library edits. FMATH including fractional FROUND correction and BUF/VEC consumed-state clearing are integrated with passing focused suites. TASK callback storage is typed through its pthread ABI and task-test passes. Shared concurrent lifecycle registration is fixed and reviewed in d8b044e7.

Integrate reviewed Office-support stack through faa83580 (ZIP/XML/byte edits, Unicode classes, F64-TEXT and FS:SAMEFILE). FS identity review found a small per-call foreign boundary without cached process state. Tender owns ZIP/F64 lifecycle repair: replace captured handle/function/locale constants with cleanup and reacquisition, free owned locale/handles, register before acquisition, preserve APIs. Kestrel's additive target/network stack is73983518, followed by d8b99ba6 borrowing libc symbols through RTLD_DEFAULT; owned UDP/SERIAL lifecycle callbacks await matching TASK/error-registry integration. Capture all additive error constants in the matching binary, then run focused APIs, concurrent initialization and fresh-process reuse. Generic XMODEM is included in that candidate; no new application feature scope.
