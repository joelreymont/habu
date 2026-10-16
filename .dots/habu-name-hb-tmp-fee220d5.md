---
title: Name HB_TMP when its value is too long
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:05.385724+03:00"
---

Problem: src/lib/fs-mutate.f:586 FS-MUT-HB-TMP-CHECK names HB_TMP only for a value that is not a directory. A value longer than FS-PATH-CAP makes its DIR? throw a bare E-FS-PATH in FS-PATHZ-INTO (src/lib/fs.f:330), so the process dies without saying HB_TMP was the cause, which is the failure the missing-directory fix (517b18f6) removed. Measured with an HB_TMP over 1024 bytes. Acceptance: an HB_TMP over FS-PATH-CAP bytes is refused before anything is made with one fd 2 line that names HB_TMP and says it is too long, then a throw; the existing not-a-directory line and code are unchanged. Files: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f (its HB_TMP case, FMT-HB-TMP:RUN). Verify: bin/hb --load src/lib/fs-mutate-test.f rc 0 with the new case. Depends: none. Ownership: those two files. Claim: unassigned.
