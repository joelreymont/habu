---
title: One libc O-NONBLOCK word
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.804090+02:00"
---

Problem: the libc-encoded O_NONBLOCK (4 on macOS, $800 on Linux) is defined four times: src/lib/fd.f:66, src/lib/process.f:104, src/lib/net/tcp4.f:176 and test/http-accept-starved.f:41, each as 'HB-TARGET-MACOS? if 4 else $800 then', and src/lib/signal.f:24 documents the split instead of removing it; found while fixing habu-fix-o-nonblock-f18448d5, 2026-10-30. Acceptance: one public definition (name Joel's, in the module that owns fcntl flags) used by every fcntl and libc-open caller; the three copies and the signal.f sentence go; FS:O-NONBLOCK (the BSD-shaped bit for the open primitive) stays separate and its comment says why the two exist. Files: src/lib/fd.f, src/lib/process.f, src/lib/net/tcp4.f, test/http-accept-starved.f, src/lib/signal.f. Verify: rg -n ': O-NONBLOCK' src test tools finds one libc definition and FS's constant; hb --load src/lib/fd-test.f, src/lib/process-test.f, src/lib/net/tcp4-test.f (or the tcp4 row test/run.f names) rc 0. Depends: habu-fix-o-nonblock-f18448d5. Ownership: src/lib/fd.f. Claim: unassigned.
