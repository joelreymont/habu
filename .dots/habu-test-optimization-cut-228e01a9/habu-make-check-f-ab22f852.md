---
title: Make check.f accept test/gate-images.f
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T16:51:16.726317+02:00\""
---

Problem: `bin/hb --load tools/check.f -- test/gate-images.f` prints 'check.f: source preverify failed before run', 'check.f: throw code 7121', rc 67 (measured 2026-09-30 on d40cc36d). 7121 is E-LAYOUT-BUFFER (src/core/layout-buffer.f:23) and E-SIZE (src/core/dynamic-storage.f:18); which one fires, and whether the file, check.f's preverify or the checker is wrong, is not known. Acceptance: root cause named with the reduced failing input; fixed at the responsible layer; check.f accepts test/gate-images.f and still refuses a source it must refuse; the case runs through the real check.f load path in the suite that owns check.f. Files: tools/check*.f, test/gate-images.f or the core file the cause names. Verify: the command above rc 0; the owning suite rc 0. Depends: none. Ownership: those files. Claim: agent=kestrel workspace=.jj-ws/r4-check.
