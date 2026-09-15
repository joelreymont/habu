---
title: Name the file when an include target cannot be opened through a link
status: open
priority: 2
issue-type: task
created-at: "2026-09-15T16:08:07.408735+03:00"
---

Problem: bin/hb --load lib/test.f in a directory whose lib is a dangling symlink exits 74 with no diagnostic, while a missing file prints 'include: cannot open <path>' before the same exit; tools/check-test-lib.f's missing-engine case hid behind this for a day. Acceptance: every failed include open names the path on stderr, whatever errno the open returned; a regression alongside the existing cannot-open case. Files: src/core/include.f (the open failure leg), test for include diagnostics. Verify: mkdir d; ln -s lib d/lib; cd d; bin/hb --load lib/test.f prints the path and exits 74. Depends: none. Ownership: include loader. Claim: unassigned.
